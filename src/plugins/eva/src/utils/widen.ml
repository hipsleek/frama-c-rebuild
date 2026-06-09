(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

module IntSet = Z.Set
module FloatSet = Datatype.Float.Set

let dkey = Widen_hints_ext.dkey

(* Note concerning all visitors and hints related to statements:
   currently, [stmt] is always [None]. Because our dataflow does not
   stabilize inner loop before the outer ones, we sometimes end up widening
   an inner variable inside an outer loop. Hence, we need to have the inner
   widening hints in the outer loops. To do so, the simplest is to avoid
   specifying statements altogether. This may be inefficient for codes that
   reuse loop indexes...
*)

let rec constFoldTermToReal = function
  | TConst (LReal r) -> Some r.r_nearest
  | TUnOp (Neg, e) ->
    Option.map (fun f -> -. f) (constFoldTermToReal e.term_node)
  | _ -> None

module Global_Static_Hints =
  State_builder.Ref
    (Widen_type)
    (struct
      let dependencies = [ Ast.self ]
      let name = "Widen.Global_Static_Hints"
      let default = Widen_type.default
    end)
let () = Ast.add_monotonic_state Global_Static_Hints.self

let update_global_hints new_hints =
  let hints = Widen_type.join (Global_Static_Hints.get ()) new_hints in
  Global_Static_Hints.set hints;

class widen_visitor init_widen_hints init_enclosing_loops = object(self)
  inherit Visitor.frama_c_inplace

  val widen_hints = init_widen_hints
  val enclosing_loops = init_enclosing_loops

  method private add_int_thresholds ?base int_thresholds =
    let new_hints = Widen_type.num_hints None(*see note*) base int_thresholds in
    if Option.fold ~none:false ~some:Base.is_global base
    then update_global_hints new_hints
    else widen_hints := Widen_type.join new_hints !widen_hints

  method private add_var_hints ~stmt hints =
    widen_hints := Widen_type.join (Widen_type.var_hints stmt hints) !widen_hints

  method! vstmt (s:stmt) =
    match s.skind with
    | Loop (_, bl, _, _, _) -> begin
        (* ZZZ: this code does not handle loops that are created using gotos. We
           could improve this by finding the relevant statements using a
           traversal of the CFG. *)
        let new_loop_info = s :: enclosing_loops in
        let visitor = new widen_visitor widen_hints new_loop_info in
        ignore (Visitor.visitFramacBlock visitor bl);
        Cil.SkipChildren (* Otherwise the inner statements are visited multiple
                            times needlessly *)
      end
    | If (exp, bl_then, bl_else, _) -> begin
        (* Look for if-goto and if-break statements. The variables of the
           condition are added to the early widening variable set for this loop.*)
        let aux_loop loop =
          let loop_stmts = Stmts_graph.get_stmt_stmts loop in
          let rec aux_block_loop bl =
            match bl with
            | {bstmts = []} -> ()
            | {bstmts = [{skind = Block bl}]} -> aux_block_loop bl
            | {bstmts = ({skind = Break _; succs = [stmt]}|
                         {skind = Goto ({contents=stmt},_)})
                        ::_} when not (Stmt.Set.mem stmt loop_stmts) ->
              (* This block goes out of [loop]. The variables of [exp] are hints*)
              let varinfos = Cil.extract_varinfos_from_exp exp in
              let var_hints =
                Varinfo.Set.fold
                  (fun vi set -> Base.Set.add (Base.of_varinfo vi) set)
                  varinfos Base.Set.empty
              in
              self#add_var_hints ~stmt:loop var_hints
            | _ -> ()
          in
          aux_block_loop bl_then;
          aux_block_loop bl_else
        in
        List.iter aux_loop enclosing_loops;
        Cil.DoChildren
      end
    | Instr (Set ((Var vi, _), exp, _) ) ->
      let rec find_candidates expr =
        match expr.enode with
        | BinOp (Mod, _, modu, _typ) -> [modu]
        | BinOp (BAnd, e1, e2, _typ) -> [e1; e2]
        | CastE (_, expr) -> find_candidates expr
        | _ -> []
      in
      let process expr =
        match Cil.constFoldToInt expr with
        | None -> ()
        | Some i ->
          let base = Base.of_varinfo vi in
          let threshold = IntSet.singleton (Z.pred i) in
          self#add_int_thresholds ~base threshold
      in
      List.iter process (find_candidates exp);
      Cil.DoChildren
    | _ -> Cil.DoChildren

  method! vexpr (e:exp) = begin
    let with_succ v = [v ; Z.succ v]
    and with_pred v = [Z.pred v ; v ]
    and with_s_p_ v = [Z.pred v; v; Z.succ v]
    and default_visit _e = Cil.DoChildren
    and unop_visit e =
      match e with
      | {enode=(CastE(_, { enode=Lval (Var varinfo, _)})
               | Lval (Var varinfo, _))} ->
        let int_thresholds = IntSet.singleton Z.zero in
        let base = Base.of_varinfo varinfo in
        self#add_int_thresholds ~base int_thresholds;
        Cil.DoChildren
      | _ -> Cil.DoChildren
    and comparison_visit add1 add2 e1 e2 =
      let add base set =
        let int_thresholds =
          List.fold_right IntSet.add set IntSet.empty
        in
        self#add_int_thresholds ~base int_thresholds
      in
      let i1, i2 = Cil.constFoldToInt e1, Cil.constFoldToInt e2 in begin
        match i1, i2, e1, e2 with
        | Some int64, _, _, {enode=(CastE(_, { enode=Lval (Var varinfo, _)})
                                   | Lval (Var varinfo, _))}->
          add (Base.of_varinfo varinfo) (add1 int64)
        | _, Some int64, {enode=(CastE(_, { enode=Lval (Var varinfo, _)})
                                | Lval (Var varinfo, _))}, _ ->
          add (Base.of_varinfo varinfo) (add2 int64)
        | _ -> ()
      end;
      Cil.DoChildren
    in
    match e.enode with
    | BinOp (Lt, e1, e2, _)
    | BinOp (Gt, e2, e1, _)
    | BinOp (Le, e2, e1, _)
    | BinOp (Ge, e1, e2, _) ->
      comparison_visit with_succ with_pred e1 e2
    | BinOp (Eq, e1, e2, _)
    | BinOp (Ne, e1, e2, _) ->
      comparison_visit with_s_p_ with_s_p_ e1 e2
    | UnOp (Neg, e, _) ->
      unop_visit e
    | Lval _ ->
      unop_visit e
    | _ -> default_visit e
  end

  (* [idx] is an expression that serves as index in an access to an array
     of size [size]. When possible, add hints for the variables in [idx] *)
  method private add_index_hints size idx =
    (* add the bounds [size-shift, size-shift-1] to the hints for [vidx] *)
    let add_hint vidx size shift =
      let bound1 = Z.sub size shift in
      let bound2 = Z.(sub bound1 one) in
      let int_thresholds = IntSet.of_list [bound1; bound2] in
      self#add_int_thresholds ~base:(Base.of_varinfo vidx) int_thresholds
    in
    (* Find inside [idx] a variable on which we will add hints. [shift] is an
       integer that indicates that we access to [idx+shift], instead of to
       [idx] directly *)
    let rec aux_idx idx shift =
      match idx.enode with
      | Lval (Var vidx, _) -> add_hint vidx size shift
      | CastE (typ, e') when Ast_types.is_integral typ ->
        (* It is safe to ignore casts: hints do not need to be sound. *)
        aux_idx e' shift
      | BinOp ((PlusA | MinusA as op), e1, e2, _) -> begin
          (* See if either [e1] or [e2] is constant. If so, find a variable in
             the other expression and add a hint for this variable, shifted. *)
          let shift' s =
            if op = PlusA then Z.add shift s else Z.sub shift s
          in
          match Cil.constFoldToInt e1 with
          | Some shift1 -> aux_idx e2 (shift' shift1)
          | None -> begin
              match Cil.constFoldToInt e2 with
              | None -> ()
              | Some shift2 -> aux_idx e1 (shift' shift2)
            end
        end
      | _ -> ()
    in
    aux_idx idx Z.zero

  (* Find an array access and infer hints for the variables involved. We visit
     the l-value ourselves. This way, we catch all accesses, including in
     sub-structures. *)
  method private find_array_accesses (host, off) =
    let rec aux_offset typ offs =
      match offs with
      | NoOffset -> ()
      | Field (fi, off) -> aux_offset fi.ftype off
      | Index (idx, off) -> begin
          match Ast_types.unroll_node typ with
          | TArray (typ_e, size) -> begin
              aux_offset typ_e off;
              try
                let size = Cil.lenOfArray64 size in
                if Z.gt size 0z then
                  self#add_index_hints size idx
              with Cil.LenOfArray _ -> ()
            end
          | _ -> ()
        end
    in
    aux_offset (Cil.typeOfLhost host) off

  method! vlval lv =
    self#find_array_accesses lv;
    Cil.DoChildren
end

(* returns the (static) bases associated to [hvars], which
   must not be [HintMem]. *)
let base_of_static_hvars hvars =
  match hvars with
  | Widen_hints_ext.HintAllVars -> None
  | Widen_hints_ext.HintVar vi -> Some (Base.of_varinfo vi)
  | Widen_hints_ext.HintMem (e, offset) ->
    (* syntactic constraints prevent this from happening *)
    Self.fatal "unsupported lhost: %a" Printer.pp_lval (Mem e, offset)

type threshold = Int_th of Z.t | Real_th of float

(* try parsing as int, then as float *)
let threshold_of_threshold_term ht =
  let global_find_init vi =
    try (Globals.Vars.find vi).init with Not_found -> None
  in
  let ht = Cil.visitCilTerm
      (new Logic_utils.simplify_const_lval global_find_init) ht
  in
  match Logic_utils.constFoldTermToInt ht with
  | Some i -> Int_th i
  | None ->
    match constFoldTermToReal ht.term_node with
    | Some f -> Real_th f
    | None ->
      Self.abort ~source:(fst ht.term_loc)
        "could not parse widening hint: %a@ \
         If it contains variables, they must be global const integers."
        Printer.pp_term ht

let thresholds_of_threshold_terms hts =
  let has_int = ref false in
  let has_float = ref false in
  List.fold_left (fun (int_acc, float_acc) ht ->
      match threshold_of_threshold_term ht with
      | Int_th i ->
        if !has_float then
          Self.abort ~source:(fst ht.term_loc)
            "widening hint mixing integers and floats: %a"
            Printer.pp_term ht;
        has_int := true;
        IntSet.add i int_acc, float_acc
      | Real_th f ->
        if !has_int then
          Self.abort ~source:(fst ht.term_loc)
            "widening hint mixing integers and floats: %a"
            Printer.pp_term ht;
        has_float := true;
        int_acc, FloatSet.add f float_acc
    ) (IntSet.empty, FloatSet.empty) hts

class hints_visitor init_widen_hints global = object(self)
  inherit Visitor.frama_c_inplace

  val widen_hints = init_widen_hints

  method private iter_static_hints ~global hints =
    let static_hints =
      List.filter
        (fun h -> not (Widen_hints_ext.is_dynamic h)) hints
    in
    List.iter
      (fun ({Widen_hints_ext.vars; loc}, wh_terms) ->
         let base = base_of_static_hvars vars in
         let int_thresholds, float_thresholds =
           thresholds_of_threshold_terms wh_terms
         in
         Self.feedback ~source:(fst loc) ~dkey
           "adding%s hint from annotation: %a, %t (for all statements)"
           (if global then " global" else "")
           (Pretty_utils.pp_opt ~none:(format_of_string "for all variables")
              Base.pretty) base
           (fun fmt ->
              if IntSet.is_empty int_thresholds then
                Format.fprintf fmt "float:%a" FloatSet.pretty float_thresholds
              else
                IntSet.pretty fmt int_thresholds);
         let new_int_hints =
           Widen_type.num_hints None (* see note above *) base int_thresholds
         in
         widen_hints := Widen_type.join new_int_hints !widen_hints;
         let new_float_hints =
           Widen_type.float_hints None (* see note above *) base float_thresholds
         in
         widen_hints := Widen_type.join new_float_hints !widen_hints
      ) static_hints

  method! vstmt s =
    let all_hints = Widen_hints_ext.get_stmt_widen_hint_terms s in
    let global_hints =
      List.filter (fun ht -> Widen_hints_ext.is_global ht = global) all_hints
    in
    self#iter_static_hints ~global global_hints;
    Cil.DoChildren
end

(* Precompute global widen hints, used for all functions *)
let compute_global_static_hints () =
  Self.debug ~dkey "computing global widen hints";
  let global_widen_hints = ref (Global_Static_Hints.get ()) in
  Globals.Functions.iter_on_fundecs (fun fd ->
      let visitor = new hints_visitor global_widen_hints true in
      ignore (Visitor.visitFramacFunction visitor fd)
    );
  Global_Static_Hints.set !global_widen_hints

let per_function_static_hints fdec =
  let widen_hints = ref Widen_type.empty in
  let visitor_pragma = new widen_visitor widen_hints [] in
  ignore (Visitor.visitFramacFunction visitor_pragma fdec);
  let visitor_local = new hints_visitor widen_hints false in
  ignore (Visitor.visitFramacFunction visitor_local fdec);
  !widen_hints

module Per_Function_Static_Hints =
  State_builder.Hashtbl
    (Cil_datatype.Fundec.Hashtbl)
    (Widen_type)
    (struct
      let name = "Widen.Per_Function_Static_Hints"
      let size = 97
      let dependencies = [ Ast.self ]
    end)
let () = Ast.add_monotonic_state Per_Function_Static_Hints.self

(* parse and precompute global and local static hints *)
let precompute_widen_hints () =
  compute_global_static_hints ();
  Globals.Functions.iter_on_fundecs
    (fun fd ->
       Per_Function_Static_Hints.replace fd (per_function_static_hints fd))

type dynamic_hint = {
  mutable bases : Base.Hptset.t
(* dynamic, used to detect when a new base needs to be added to the global
   widening hints *);
  lv : exp * offset; (* static, parsed once from the AST *)
  int_thresholds : IntSet.t; (* static, computed only once *)
  float_thresholds : FloatSet.t; (* static, computed only once *)
}

module ExpOffset = Datatype.Pair(Exp)(Offset)

module DynamicHintDatatype = Datatype.Make(struct
    include Datatype.Serializable_undefined
    type t = dynamic_hint
    let name = "Widen.DynamicHintDatatype"
    let structural_descr =
      Structural_descr.t_tuple
        [| Base.Hptset.packed_descr;
           ExpOffset.packed_descr;
           IntSet.packed_descr;
           FloatSet.packed_descr |]
    let reprs =
      List.product_map
        (fun wh fh -> { bases = Base.Hptset.empty;
                        lv = (Exp.dummy, NoOffset);
                        int_thresholds = wh;
                        float_thresholds = fh })
        IntSet.reprs
        FloatSet.reprs
    let mem_project = Datatype.never_any_project
  end)

(* use a list of hints instead of multiple entries in a hashtable
   because we need to replace one entry (e.g. one hint for which
   a base was added) but not all, so Hashtbl.replace will not work. *)
module StmtDynamicHint = Datatype.List(DynamicHintDatatype)

(** Stores a mapping from statements to parsed dynamic hint terms.
    Only stores mappings for statements with annotations, to avoid
    wasting memory.
    The dataflow iteration consults this table each time it reaches
    a statement with an annotation. It must quickly evaluate the
    bases related to the annotations, to see if there are new bases
    that should be added to the global widening hints. Therefore,
    we store, for each annotation, the set of bases computed so far,
    plus the thresholds (to avoid recomputing them).
*)
module Parsed_Dynamic_Hints =
  State_builder.Hashtbl
    (Stmt.Hashtbl)
    (StmtDynamicHint)
    (struct
      let name = "Widen.Parsed_Dynamic_Hints"
      let size = 7
      let dependencies = [ Ast.self; Self.state ]
    end)

let dynamic_bases_of_lval states e offset =
  let lv = Eva_ast.mk_lval (Mem e, offset) in
  List.fold_left (fun acc' state ->
      let location = Cvalue_queries.lval_to_loc state lv in
      Addresses.Bits.fold_bases
        (fun base acc'' -> Base.Hptset.add base acc'')
        location.Locations.addr acc'
    ) Base.Hptset.empty states

(* Find syntactically the dynamic hints on [stmt]. *)
let extract_dynamic_hints stmt =
  let source = fst (Stmt.loc stmt) in
  Self.debug ~source ~dkey
    "computing dynamic hints for statement %d" stmt.sid;
  let wh = Widen_hints_ext.get_stmt_widen_hint_terms stmt in
  let aux l (hlv, threshold_terms) =
    let open Widen_hints_ext in
    match hlv.vars with
    | HintMem (e, offset) ->
      let int_thresholds, float_thresholds =
        thresholds_of_threshold_terms threshold_terms
      in
      let bases = Base.Hptset.empty in
      { bases; lv = (e, offset); int_thresholds; float_thresholds; } :: l
    | _-> l
  in
  List.fold_left aux [] wh

let parsed_dynamic_hints = Parsed_Dynamic_Hints.memo extract_dynamic_hints

module Dynamic_Hints =
  State_builder.Ref
    (Widen_type)
    (struct
      let dependencies = [ Ast.self; Self.state ]
      let name = "Widen.Dynamic_Hints"
      let default = Widen_type.default
    end)
let () = Ast.add_monotonic_state Dynamic_Hints.self

(* The contents of this table should always be the join Dynamic_hints
   and Per_Function_Static_Hints, for the functions that have been computed.
   It must be cleared when Dynamic_Hints is changed. *)
module Per_Function_Hints =
  State_builder.Hashtbl
    (Cil_datatype.Fundec.Hashtbl)
    (Widen_type)
    (struct
      let name = "Widen.Per_Function_Hints"
      let size = 97
      let dependencies = [ Ast.self; Dynamic_Hints.self ]
    end)
let () = Ast.add_monotonic_state Per_Function_Hints.self

let extract_per_function_hints fdec =
  let global = Global_Static_Hints.get () in
  let for_fdec =
    try Per_Function_Static_Hints.find fdec
    with Not_found -> assert false
  in
  let dynamic = Dynamic_Hints.get () in
  Widen_type.join (Widen_type.join global for_fdec) dynamic

let per_function_hints = Per_Function_Hints.memo extract_per_function_hints

let dynamic_widen_hints_hook _callstack stmt states =
  if Annotations.has_code_annot stmt then
    let hs = parsed_dynamic_hints stmt in
    if hs <> [] then
      let source = fst (Stmt.loc stmt) in
      let modified, new_hints =
        List.fold_right (fun dhint (_acc_modified, acc_hints as acc) ->
            let old_bases = dhint.bases in
            let exp, offset = dhint.lv in
            let exp = Eva_ast.translate_exp exp
            and offset = Eva_ast.translate_offset offset in
            let bases = dynamic_bases_of_lval states exp offset in
            let new_bases = Base.Hptset.diff bases old_bases in
            if Base.Hptset.is_empty new_bases then
              acc
            else
              let new_hints =
                Base.Hptset.fold (fun base acc ->
                    Self.debug ~source ~dkey
                      "adding new base due to dynamic widen hint: %a, %t"
                      Base.pretty base
                      (fun fmt ->
                         if IntSet.is_empty dhint.int_thresholds
                         then FloatSet.pretty fmt dhint.float_thresholds
                         else IntSet.pretty fmt dhint.int_thresholds);
                    let int_hint_for_base =
                      Widen_type.num_hints None (Some base) dhint.int_thresholds
                    in
                    let float_hint_for_base =
                      Widen_type.float_hints None (Some base) dhint.float_thresholds
                    in
                    let acc = Widen_type.join acc int_hint_for_base in
                    Widen_type.join acc float_hint_for_base
                  ) new_bases acc_hints
              in
              dhint.bases <- Base.Hptset.union dhint.bases new_bases;
              true, new_hints
          ) hs (false, Widen_type.empty)
      in
      if modified then begin
        Per_Function_Hints.clear ();
        let hints = Widen_type.join (Dynamic_Hints.get ()) new_hints in
        Dynamic_Hints.set hints;
      end

let () = Cvalue_callbacks.register_statement_hook dynamic_widen_hints_hook

let getWidenHints (kf:kernel_function) (stmt:stmt) =
  let hints =
    match kf.fundec with
    | Declaration _ -> Widen_type.empty
    | Definition (fdec, _) -> per_function_hints fdec
  in
  Widen_type.hints_from_keys stmt hints
