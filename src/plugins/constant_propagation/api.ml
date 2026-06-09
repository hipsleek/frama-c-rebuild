(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
module FC_file = File
open Cil_datatype

exception Cannot_expand
exception Cannot_change

(* Build the term [p+i], assuming that [p] has pointer type *)
let plus_pi ~loc p i =
  if Z.(equal zero i) then
    p
  else
    Cil.mkBinOp_exn ~loc PlusPI p (Cil.kinteger64 ~loc i)

(** This visitor also performs a deep copy. *)
class propagate project fnames ~cast_intro = object(self)
  inherit Visitor.frama_c_copy project

  (* Variables which have already been declared earlier in the list of
     globals. Varinfos of the old project. *)
  val mutable known_globals = Varinfo.Set.empty

  (* Variables whose declaration must be put before the global we are visiting.
     Reset before each global. Varinfos of the _new_ project. *)
  val mutable must_add_decl = Varinfo.Set.empty

  method! vstmt_aux s=
    (* Do not propagate on 'return' statements: one invariant of the AST is
       that they must be of the form 'return v;' where 'v' is a variable *)
    match s.skind with
    | Return _ -> Cil.JustCopy
    | _ -> Cil.DoChildren

  method! vfunc fundec =
    if Cil_datatype.Fundec.Set.is_empty fnames ||
       Cil_datatype.Fundec.Set.mem fundec fnames
    then begin
      PropagationParameters.feedback
        ~level:2
        "propagated constant in function %s"
        (fundec.svar.vname);
      Cil.DoChildren
    end else Cil.JustCopy

  method private add_decl_non_source_var vi =
    PropagationParameters.debug ~level:2
      "Need to declare %a earlier" Printer.pp_varinfo vi;
    let vi' =
      Visitor.visitFramacVarDecl (self :> Visitor.frama_c_visitor) vi
    in
    must_add_decl <- Varinfo.Set.add vi' must_add_decl;
    known_globals <- Varinfo.Set.add vi known_globals;
    if Ast_types.is_fun vi.vtype then begin
      let kf = Globals.Functions.get vi in
      let new_kf = Visitor_behavior.Memo.kernel_function self#behavior kf in
      Queue.add (fun () -> Globals.Functions.register new_kf)
        self#get_filling_actions;
    end

  (* introduce a new cast from [oldt] to [newt] or do not expand [e] *)
  method private add_cast ~ignore_const_cast ~oldt ~newt e =
    (* strip the superfluous 'const' attribute (see bts #1787) on
       pointed values. *)
    let oldt, newt =
      if ignore_const_cast then
        match Ast_types.unroll oldt, Ast_types.unroll newt with
        | { tnode = TPtr typ; tattr = attrs }, { tnode = TPtr typ'; tattr =  attrs' } ->
          let drop_const ty = Ast_types.remove_attributes ["const"] ty in
          Cil_const.mk_tptr ~tattr:attrs  (drop_const typ),
          Cil_const.mk_tptr ~tattr:attrs' (drop_const typ')
        | _ -> oldt, newt
      else
        oldt, newt
    in
    let exp = Cil.mkCastT ~oldt ~newt e in
    if cast_intro then
      exp
    else match exp.enode with
      | CastE _ ->
        if exp == e (* older cast, no new cast added *) then
          exp
        else begin
          (* without [cast_intro], introducing such a cast is not
             allowed: do not expand [e] *)
          PropagationParameters.debug
            ~level:2
            "Need a cast introduction (force using -scf-allow-cast option)";
          raise Cannot_expand
        end
      | _ ->
        (* remember the change done by [mkCastT] (if any).
           note that [mkCastT] make some modifications, even if it
           does not introduce a new cast. *)
        exp

  (* Make sure that [expr] is in the original project. *)
  method private propagated expr ~ignore_const_cast =
    PropagationParameters.debug ~level:2
      "Replacing %s%a?"
      (if ignore_const_cast then "(without const* cast) " else "")
      Printer.pp_exp expr;
    try
      let loc = expr.eloc in
      let typ = Cil.typeOf expr in
      let typ_e = Ast_types.unroll typ in
      begin match typ_e.tnode with
        | TInt _ | TFloat _ | TPtr _ | TEnum _ -> ()
        | _ -> raise Cannot_expand
      end;
      let stmt = match self#current_stmt with
        | None -> raise Cannot_change
        | Some s -> s
      in
      let evaled = Eva.Results.(before stmt |> eval_exp expr |> as_cvalue) in
      let b, m = Cvalue.V.find_lonely_binding evaled in
      let can_replace vi =
        (* can replace the current expr by [vi] iff (1) it is a source var, or
           expansion of non-source var is allowed. *)
        (vi.vsource || PropagationParameters.ExpandLogicContext.get ())
        &&
        (* (2) [vi] is bound in this function *)
        (vi.vglob ||
         Option.fold
           ~some:(Kernel_function.is_formal_or_local vi) ~none:false
           self#current_kf)
      in
      let change_to = match b with
        | Base.Var(vi, _) | Base.Allocated (vi, _, _)
          when not (Base.is_weak b) &&  can_replace vi ->
          if vi.vglob && not (Varinfo.Set.mem vi known_globals) then
            self#add_decl_non_source_var vi;
          PropagationParameters.debug
            "Trying replacing %a from a pointer value {&%a + %a}"
            Printer.pp_exp expr Base.pretty b Ival.pretty m;
          let offset = Ival.project_int m in (* these are bytes *)
          let expr' =
            try
              if not (Ast_types.is_ptr typ_e) then
                raise Bit_utils.NoMatchingOffset;
              let typ_pointed = Ast_types.(unroll (direct_pointed_type typ_e)) in
              if Ast_types.is_void typ_pointed then
                raise Bit_utils.NoMatchingOffset;
              let offset = Z.mul offset 8z in
              let m = Bit_utils.MatchType typ_pointed in
              let off, _ = Bit_utils.(find_offset vi.vtype ~offset m) in
              Cil.mkAddrOrStartOf ~loc (Var vi, off)
            with Bit_utils.NoMatchingOffset ->
              (* Build [((char* )&t[idx])+rem] when vi is an array, or
                 [(char* )(&vi+idx)+rem] otherwise. Automatically simplify
                 when [idx] or [rem] is zero. *)
              let array, idx, rem =
                let array, sizeof_pointed =
                  let array = Ast_types.is_array vi.vtype in
                  let size = if array
                    then Bit_utils.osizeof_pointed vi.vtype
                    else Bit_utils.osizeof vi.vtype
                  in
                  array, Z_or_top.project size
                in
                let div,rem = Z.ediv_rem offset sizeof_pointed in
                array,div,rem
              in
              let expr' =
                if array then
                  let off_idx =
                    if Z.is_zero idx
                    then NoOffset
                    else Index (Cil.kinteger64 ~loc idx, NoOffset)
                  in
                  Cil.mkAddrOrStartOf ~loc (Var vi, off_idx)
                else
                  let start = Cil.mkAddrOrStartOf ~loc (Var vi, NoOffset) in
                  plus_pi ~loc start idx
              in
              if Z.is_zero rem then expr'
              else
                plus_pi ~loc
                  (self#add_cast
                     ~ignore_const_cast:false
                     ~oldt:(Cil.typeOf expr')
                     ~newt:Cil_const.charPtrType
                     expr')
                  rem
          in
          (* preserve typing: propagating constant could change the type
             of the expression. We have to put back the original type. *)
          self#add_cast
            ~ignore_const_cast
            ~oldt:(Cil.typeOf expr')
            ~newt:typ
            expr'

        | Base.Null ->
          let const_integer m ikind =
            try
              let v = Ival.project_int m in
              if not (Cil.fitsInInt ikind v) then
                PropagationParameters.error "Constant found by Value (%a) \
                                             does not fit inside type %a. Please report"
                  Z.pretty v
                  Printer.pp_typ typ;
              Cil.kinteger64 ~loc ~kind:ikind v
            with Ival.Not_Singleton_Int -> raise Cannot_expand
          and const_float m fkind =
            try
              let f = Ival.project_float m in
              let f =  Fval.(F.to_float (project_float f)) in
              Cil.kfloat ~loc:expr.eloc fkind f
            with Fval.Not_Singleton_Float->
              raise Cannot_expand
          in
          (match typ_e.tnode with
           | TFloat fkind -> const_float m fkind
           | TInt ikind | TEnum { ekind = ikind} ->
             const_integer m ikind
           | _ -> raise Cannot_expand)

        | Base.Var _ | Base.Allocated _
        | Base.CLogic_Var _ -> raise Cannot_change
      in
      PropagationParameters.debug "Replacing %a with %a"
        Printer.pp_exp expr Printer.pp_exp change_to;
      Some change_to
    with
    | Cannot_change -> None
    | Not_found | Cannot_expand | Cil.Not_representable
    | Abstract_interp.Error_Top as e ->
      PropagationParameters.debug "Replacement failed %s"
        (Printexc.to_string e);
      None

  method! vexpr expr =
    (* nothing is done for [expr] already being a constant *)
    match expr.enode with
    | Const (_) -> Cil.DoChildren
    | _ -> begin
        (* Start by trying to constant-propagate all of [expr]. Casts are allowed
           only if -scf-allow-cast is set *)
        match self#propagated expr ~ignore_const_cast:false with
        | Some expr' -> Cil.ChangeDoChildrenPost (expr', fun x -> x)
        | None -> begin
            (* Global constant propagation of [expr] failed. We try a special
               const-folding, AND simplify the sub-expressions in all cases *)
            match expr.enode with
            | Lval (Mem exp_mem, off) -> begin
                (* [expr] is a Mem. Try to see if we can propagate [exp_mem] into
                   something simpler, because the result will be of the form
                   [Var _, offs'], which can be simplified under a [Mem]. This time,
                   we ignore const-related casts when simplifying [exp_mem], because
                   they will disappear when the l-value is dereferenced. *)
                match self#propagated exp_mem ~ignore_const_cast:true with
                | Some exp_mem' ->
                  let lv =
                    Cil.new_exp ~loc:expr.eloc (Lval (Cil.mkMem ~addr:exp_mem' ~off))
                  in
                  Cil.ChangeDoChildrenPost (lv, fun x -> x)
                | None -> Cil.DoChildren
              end
            | _ -> Cil.DoChildren
          end
      end

  method! vvdec v =
    if v.vglob then begin
      known_globals <- Varinfo.Set.add v known_globals;
    end;
    Cil.DoChildren

  method! vglob_aux g =
    must_add_decl <- Varinfo.Set.empty;
    (* Check if [g] has already been declared earlier, due to being used in
       some earlier values. If so, we will skip [g]. We do this check now and
       not in [add_decls], because [self#vvdec] will mark g as known. *)
    let g_is_known = match g with
      | GVarDecl (vi, _) | GFunDecl (_, vi, _) -> Varinfo.Set.mem vi known_globals
      | _ -> false
    in
    let add_decls l =
      (* Do not re-add a declaration for g if it is known. *)
      let l = if g_is_known then [] else l in
      (* Add declarations for the globals that are referenced in g's propagated
         value. *)
      Varinfo.Set.fold
        (fun vi l ->
           PropagationParameters.feedback ~level:2
             "Adding declaration of global %a" Printer.pp_varinfo vi;
           let g' =
             if Ast_types.is_fun vi.vtype
             then GFunDecl(Cil.empty_funspec(), vi, vi.vdecl)
             else GVarDecl(vi, vi.vdecl)
           in
           g' ::l)
        must_add_decl l
    in
    Cil.DoChildrenPost add_decls

  method! vlhost h =
    let simplify_lhost =
      function
      | Mem e -> fst (Cil.mkMem ~addr:e ~off:NoOffset)
      | h -> h
    in
    Cil.ChangeDoChildrenPost(h, simplify_lhost)

  method! vlval (h,off) =
    match h with
    | Mem e ->
      let addr = Cil.visitCilExpr (self:>Cil.cilVisitor) e in
      let off = Cil.visitCilOffset (self:>Cil.cilVisitor) off in
      Cil.ChangeTo (Cil.mkMem ~addr ~off)
    | Var _ -> Cil.DoChildren

end

module Result_pair =
  Datatype.Pair_with_collections(Cil_datatype.Fundec.Set)(Datatype.Bool)
module Result =
  State_builder.Hashtbl
    (Datatype.Hashtbl
       (Result_pair.Hashtbl)
       (Result_pair))
    (Project.Datatype)
    (struct
      let size = 7
      let name = "Semantical constant propagation"
      let dependencies =
        [ Eva.Analysis.self;
          PropagationParameters.CastIntro.self;
          PropagationParameters.Project_name.self ]
    end)

let selection_command_line_option =
  State_selection.singleton PropagationParameters.SemanticConstFolding.self

(* add labels *)
let get fnames ~cast_intro =
  Result.memo
    (fun _ ->
       Eva.Analysis.compute ();
       let fresh_project =
         FC_file.create_project_from_visitor
           (PropagationParameters.Project_name.get ())
           (fun prj -> new propagate prj fnames ~cast_intro)
       in
       let ctx = Parameter_state.get_selection_context () in
       let ctx = State_selection.diff ctx selection_command_line_option in
       Project.copy ~selection:ctx fresh_project;
       fresh_project)
    (fnames, cast_intro)

(** Constant Propagation *)

let compute () =
  PropagationParameters.feedback "beginning constant propagation";
  let fnames = PropagationParameters.SemanticConstFold.get () in
  let cast_intro = PropagationParameters.CastIntro.get () in
  let propagated = get fnames ~cast_intro in
  if PropagationParameters.SemanticConstFolding.get () then
    FC_file.pretty_ast ~prj:propagated ();
  let project_name = Project.get_name propagated in
  PropagationParameters.feedback  "@[constant propagation done%t@]"
    (fun fmt ->
       if project_name <> PropagationParameters.Project_name.get () then
         Format.fprintf fmt ",@ result is in project@ `%s`" project_name)

let compute, self =
  let name = "Constant_Propagation.compute" in
  let deps = [ PropagationParameters.SemanticConstFold.self;
               PropagationParameters.SemanticConstFolding.self;
               Result.self ]
  in
  State_builder.apply_once name deps compute

let main () =
  let force_semantic_folding =
    PropagationParameters.SemanticConstFolding.get ()
    || not (Cil_datatype.Fundec.Set.is_empty
              (PropagationParameters.SemanticConstFold.get ()))
  in
  if force_semantic_folding then compute ()

let () =
  Boot.Main.extend main
