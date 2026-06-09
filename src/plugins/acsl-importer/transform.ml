(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Transformation module. *)

open Cil_types

(*-----------------------------------------------------------------------*)
(* Transformation on Cabs unrolling loop conditions, identifying loop
   bodies and adding unroll loop prammas *)

module Mark: sig
  (* returns [true] for an unprocessed loop body *)
  val is_loop_attr_markup: Cabs.statement -> bool
  val loop_attr_markup: int -> is_unrollable:bool -> Cabs.attribute list * bool
end = struct
  let loop_attr_name = "acsl!loop_processed!"

  let attr_cabs s = { Cabs.expr_loc = Cabshelper.cabslu ;
                      Cabs.expr_node = Cabs.(CONSTANT (CONST_STRING s)) }

  let loop_attr_markup =
    let loop_condition_attr = "acsl!loop_condition!unrolled" in
    let loop_processed_attr_cabs = [ attr_cabs loop_attr_name ] in
    let loop_condition_attr_cabs = attr_cabs loop_condition_attr ::
                                   loop_processed_attr_cabs in
    fun n ~is_unrollable ->
      let loop_attr_cabs = if is_unrollable
        then loop_condition_attr_cabs else loop_processed_attr_cabs
      in
      let loop_attr_cabs = (attr_cabs (Paste.loop_body_attr_name n)) ::
                           loop_attr_cabs
      in [ Paste.hidden_attr, loop_attr_cabs ], is_unrollable

  let is_block_markup processed_attr block =
    not (List.exists (fun (attr,args) ->
        ((attr = Paste.hidden_attr) &&
         List.exists (function
             | { Cabs.expr_node = CONSTANT (CONST_STRING attr) } ->
               processed_attr = attr
             | _ -> false) args)) block.Cabs.battrs)

  let is_stmt_markup processed_attr = function
    | { Cabs.stmt_node=BLOCK(block,_,_)} ->
      is_block_markup processed_attr block
    | _ -> true

  let is_loop_attr_markup = is_stmt_markup loop_attr_name
end

let has_unroll_loop li =
  List.exists Logic_ptree.(function
      | AExtended (_,_,{ext_name="unfold"}) -> true
      | _ -> false) li

let has_only_unroll_loop li =
  not (List.exists Logic_ptree.(function
      | AExtended (_,_,{ext_name="unfold"})-> false
      | _ -> true) li)

module S = struct

  let stmt stmt_node ~sref = { sref with Cabs.stmt_node }
  let nop ~loc= stmt (NOP (None,loc))

  let block_node ?(loc2=Cabshelper.cabslu) ~loc bstmts battrs  =
    Cabs.BLOCK ({ bstmts ; blabels = [] ; battrs }, loc, loc2)
  let block ?loc2 ~loc bstmts battrs = stmt (block_node ?loc2 ~loc bstmts battrs)

  (* transform "{ ... L1: /*@annot*/ S1; ... }"
          into "{ ... L1: {/*label attrib*/ /*@annot*/ S1; } ... }"
     note: S1 cannot be a declaration because it is a labeled statement
     returns None when the list is unmodified *)
  let get_processed_label ss =
    let is_annot = Cabs.(function | CODE_ANNOT _ ->true
                                  | CODE_SPEC _ -> true
                                  | _ -> false) in
    let mk_label s = function
      | Cabs.LABEL(l,_,loc) -> Cabs.LABEL(l,s,loc)
      | CASERANGE(e1,e2,_,loc) -> CASERANGE(e1,e2,s,loc)
      | CASE(e,_,loc) ->  CASE(e,s,loc)
      | DEFAULT(_,loc) -> DEFAULT(s,loc)
      | _ -> assert false
    in
    let rec get prev = function
      | ({ Cabs.stmt_node=LABEL(_,s,loc) } as sref)::ls
      | ({ stmt_node=DEFAULT(s,loc) } as sref)::ls
      | ({ stmt_node=CASE(_,s,loc) } as sref)::ls
      | ({ stmt_node=CASERANGE(_,_,s,loc) } as sref)::ls
        -> extract prev (sref,loc) [] (s::ls)
      | s::ls -> get (s::prev) ls
      | [] -> prev
    and extract prev vref fs = function
      | s::ls when is_annot s.Cabs.stmt_node -> extract prev vref (s::fs) ls
      | s::ls -> next prev vref (s::fs) ls
      | []    -> next prev vref fs []
    and next prev (sref,loc) fs ls =
      let bstmts = (List.rev fs) in
      let blk = block ~loc bstmts [] ~sref in
      let slabel = stmt (mk_label blk sref.Cabs.stmt_node) ~sref in
      get (slabel::prev) ls
    in
    let rec find = function
      | ({ Cabs.stmt_node=LABEL(_,s,loc) } as sref)::ls
      | ({ stmt_node=DEFAULT(s,loc) } as sref)::ls
      | ({ stmt_node=CASE(_,s,loc) } as sref)::ls
      | ({ stmt_node=CASERANGE(_,_,s,loc) } as sref)::ls
        -> let rec first_rev_stmts prev = function
            | s::_ when s == sref -> prev
            | s::ls -> first_rev_stmts (s::prev) ls
            | [] -> assert false in
        let prev = first_rev_stmts [] ss in
        Some (List.rev (extract prev (sref,loc) [] (s::ls)))
      | _::ls -> find ls
      | [] -> None
    in find ss
end

(** May add an UNROLL_LOOP pragma from -acsl-ulevel option *)
let dkey = Options.register_category "trace-transformations"
let transform_cabs cabs =
  (* Syntactic transformation of the source code transforming loop body:
     - add a new attribute to blocks of each loop body,
     - unrool loop conditions when -acsl-unroll-loop-conditions is set,
     - insert unroll pragmas as specified by -acsl-ulevel option.
  *)
  let unroll_loop_cond = Options.is_unroll_loop_condition_on ()
  and unroll_loop_pragma = not (Options.is_unroll_loop_pragma_on ())
  in
  let visitor = object (self)
    inherit Cabsvisit.nopCabsVisitor

    val mutable loop_cpt = 0
    val mutable fct_name = ""

    (* Adds eventual unroll loop pragmas to the current loop statement [s] *)
    method unroll_pragma_insertion_process s =
      let mk_spec cst = Logic_ptree.({ lexpr_loc = Fileloc.unknown;
                                       lexpr_node = PLconstant cst }) in
      if not unroll_loop_pragma then s
      else try begin
        let unroll_pragma_insertion_process loop_category li =
          Options.debug ~level:3 ~dkey
            "Look at %S loop #%d of function %S for insertion of UNROLL pragma@."
            loop_category loop_cpt fct_name;
          let (is_total_unrolling,nb_unrolling) =
            (* Raises [Not_found] if there is nothing to do. *)
            (* 1 - Check if there is no UNROLL_LOOP pragma for that loop *)
            if has_unroll_loop li then
              raise Not_found ;
            (* 2 - Check if there is unrolling level specified option for that
                   loop *)
            Options.find_ulevel_spec loop_category loop_cpt fct_name
            (* May raise [Not_found] *)
          in
          let unroll_specs =
            let unroll_specs =
              [ mk_spec Logic_ptree.(IntConstant (string_of_int nb_unrolling)) ]
            in
            if is_total_unrolling then
              (mk_spec Logic_ptree.(StringConstant "completely"))::unroll_specs
            else unroll_specs
          in
          let ext = Logic_ptree.{
              ext_name = "unfold";
              ext_plugin = "kernel";
              ext_content = unroll_specs
            }
          in
          li@[ Logic_ptree.(AExtended([],true,ext)) ]
        in match s.Cabs.stmt_node with
        | WHILE(li,cond,body,loc) ->
          let li = unroll_pragma_insertion_process "while" li in
          { s with stmt_node=WHILE(li,cond,body,loc) }
        | DOWHILE(li,cond,body,loc) ->
          let li = unroll_pragma_insertion_process "do-while" li in
          { s with stmt_node=DOWHILE(li,cond,body,loc) }
        | FOR(li,init,cond,inc,body,loc) ->
          let li = unroll_pragma_insertion_process "for" li in
          { s with stmt_node=FOR(li,init,cond,inc,body,loc) }
        | _ -> assert false
      end
        with Not_found -> s

    method fresh_loop_body_attr li =
      let is_unrollable = unroll_loop_cond &&
                          ((has_only_unroll_loop li) ||
                           (Options.result
                              "Loop condition of loop #%d of function %S was not unrolled since there is a loop annotation."
                              loop_cpt fct_name;
                            false))
      in
      Mark.loop_attr_markup loop_cpt ~is_unrollable

    (* Returns a transformed loop where a fresh attribute is added to
       each loop body. An attribute is also added when the loop
       condition is unrolled. *)
    method unroll_loop_process ~sref = match sref.Cabs.stmt_node with
      | WHILE(li,cond,body,loc) ->
        let battrs, is_unrollable = self#fresh_loop_body_attr li in
        let body = S.block ~loc [body] battrs ~sref in
        if is_unrollable then
          (* Transforms the "while (c) Sb;" into
             "if (c) do {/* body attribs */ Sb; } while (c);" *)
          let dowhile = S.stmt (DOWHILE(li,cond,body,loc)) ~sref
          and nop = S.nop ~loc ~sref
          in Options.debug ~level:2 ~dkey
            "Unrolling loop condition of loop #%d of function %S."
            loop_cpt fct_name;
          Cabs.IF(cond,dowhile,nop,loc)
        else WHILE(li,cond, body,loc)
      | DOWHILE(li,cond,body,loc) ->
        (* Transforms the "do Sb while (c);" into
           "do {/* body attribs */ Sb; } while (c);" *)
        let battrs, _ = self#fresh_loop_body_attr [] in
        let body = S.block ~loc [body] battrs ~sref in
        DOWHILE(li,cond,body,loc)
      | FOR(li,init,cond,inc,body,loc) ->
        let battrs, is_unrollable = self#fresh_loop_body_attr li in
        let mk_body bs = S.block ~loc (body::bs) battrs ~sref in
        if is_unrollable then
          (* Transforms the "for (s;c;e) Sb;" into
             "{ s; if (c) do {/* body attribs */ Sb; e; } while (c); }" *)
          let body = match inc with
            | { Cabs.expr_node = NOTHING } -> mk_body []
            | _ -> mk_body [ S.stmt (COMPUTATION (inc,loc)) ~sref ]
          in
          let dowhile_node = Cabs.DOWHILE(li,cond,body,loc) in
          let ifdowhile_node = match cond with
            | { expr_node = NOTHING } ->
              (* ISO C11 : 6.8.5.3.2 *)
              let cond = {cond with expr_node = Cabs.(CONSTANT (CONST_INT "1"))} in
              Cabs.DOWHILE(li,cond,body,loc)
            | _ -> let nop = S.nop ~loc ~sref in
              IF(cond,(S.stmt dowhile_node ~sref),nop,loc)
          in Options.debug ~level:2 ~dkey
            "Unrolling loop condition of loop #%d of function %S."
            loop_cpt fct_name;
          let init = match init with
            | FC_EXP { expr_node = NOTHING } -> None
            | FC_EXP init  -> Some (Cabs.COMPUTATION (init,loc))
            | FC_DECL decl -> Some (DEFINITION decl)
          in match init with
          | None -> ifdowhile_node
          | Some init_node ->
            let ifdowhile = S.stmt ifdowhile_node ~sref in
            let init = S.stmt init_node ~sref in
            S.block_node ~loc [init ; ifdowhile] []
        else
          FOR(li,init,cond,inc,(mk_body []),loc)
      | _ -> assert false

    method! vexpr _ = SkipChildren (* share the AST via stmt such as
                                      Return, IF, ... *)
    method! vinitexpr _ = SkipChildren  (* share the AST *)
    method! vtypespec _ = SkipChildren (* share the AST *)
    method! vdecltype _ = SkipChildren (* share the AST *)
    method! vname _ _ _ = SkipChildren (* share the AST *)
    method! vspec _ = SkipChildren (* share the AST via visitCilFunction *)
    method! vattr _ = SkipChildren (* share the AST via Asm stmt *)

    method! vdef def = match def with
      | FUNDEF (_,(_,(name,_,_,_)),_,_,_) -> begin
          loop_cpt <- 0 ;
          fct_name <- name ;
          DoChildren
        end
      | _ -> SkipChildren

    method! vblock block =
      match S.get_processed_label block.bstmts with
      | None -> DoChildren
      | Some bstmts ->
        ChangeDoChildrenPost ( {block with bstmts }, fun x -> x)

    method! vstmt sref =
      let change f = Cil.ChangeDoChildrenPost ([f ~sref], fun x -> x) in
      let loop_process ~sref =
        loop_cpt <- loop_cpt + 1;
        let sref = self#unroll_pragma_insertion_process sref in
        S.stmt (self#unroll_loop_process ~sref) ~sref
      in match sref.stmt_node with
      | WHILE(_,_,body,_)
      | DOWHILE(_,_,body,_)
      | FOR(_,_,_,_,body,_) when Mark.is_loop_attr_markup body
        -> change loop_process
      | _ -> DoChildren
  end
  in if unroll_loop_cond then
    (let rec get_basename name = try
         get_basename (Filename.chop_extension name)
       with Invalid_argument _ -> name
     in let cabsfile = Filepath.to_string (fst cabs)
     in let get_basename () = get_basename (Filename.basename cabsfile)
     in Options.debug ~dkey
       "Unrolling loop conditions in file: %s ..." (get_basename ()) ) ;
  if unroll_loop_pragma then begin
    let rec get_basename name =
      try
        get_basename (Filename.chop_extension name)
      with Invalid_argument _ -> name
    in let cabsfile = Filepath.to_string (fst cabs)
    in let get_basename () = get_basename (Filename.basename cabsfile)
    in Options.debug ~dkey
      "Inserting unrolling loop pragmas in file: %s ..." (get_basename ())
  end ;
  Cabsvisit.visitCabsFile (visitor:>Cabsvisit.cabsVisitor) cabs

(*--------------------------*)

let () =
  Frontc.add_syntactic_transformation
    (fun cabs ->
       if Options.is_importation_on() ||
          (Options.is_unroll_loop_condition_on ()) ||
          not (Options.is_unroll_loop_pragma_on ())
       then
         transform_cabs cabs
       else cabs)

(*-----------------------------------------------------------------------*)
(* Transformation on Cil identifying loop body that must be done before
   unrolling loops *)

let ident_attr_cil f_attr_name n =
  (Paste.hidden_attr, [ AStr (f_attr_name n) ])

let loop_ident_attr_cil = ident_attr_cil Paste.loop_number_attr_name

let ast_has_changed = ref false

class do_it = object(_self)
  inherit Visitor.frama_c_inplace

  initializer ast_has_changed := false;

  val mutable loop_cpt = 0 ;

  val mutable cfg_has_changed = false ;

  method! vfunc fundec =
    assert (not cfg_has_changed) ;
    loop_cpt <- 0 ;
    let post_goto_updater =
      (fun id ->
         if cfg_has_changed then begin
           File.must_recompute_cfg id;
           ast_has_changed:=true ;
           cfg_has_changed <- false
         end;
         id) in
    ChangeDoChildrenPost (fundec, post_goto_updater)

  method! vstmt_aux s = match s.skind with
    | Loop (_,block,_,_,_) ->
      loop_cpt <- 1 + loop_cpt ;
      block.Cil_types.battrs <-
        Ast_attributes.add (loop_ident_attr_cil loop_cpt) block.Cil_types.battrs ;
      DoChildren
    | _ -> DoChildren
end

let transform_cil file =
  if Options.is_importation_on () then
    let visitor = new do_it in
    Visitor.visitFramacFileSameGlobals (visitor:>Visitor.frama_c_visitor) file;
    if !ast_has_changed then Ast.mark_as_changed ()

let () =
  File.add_code_transformation_after_cleanup
    ~deps:[(module Options.Import:Parameter_sig.S);
           (module Options.Run: Parameter_sig.S)]
    ~before:[Unfold_loops.transform; Options.main_import]
    Options.aux_import transform_cil

(*-----------------------------------------------------------------------*)
