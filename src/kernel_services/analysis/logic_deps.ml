(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

exception NYI of string
let not_yet_implemented = ref ""

(** [compute_term_deps] is provided by Eva; computes the memory zone on which a
    term depends. *)
let compute_term_deps = ref (fun _stmt _expr -> None)


(* Register slice ACSL extensions: these are directives for the slicing plugin,
   but they are processed in this file so we also register them here. *)
let () =
  let typer typing_context loc args =
    match args with
    | [] -> Ext_terms []
    | _ -> typing_context.Logic_typing.error loc "Invalid slice directive"
  in
  Acsl_extension.register_code_annot_next_stmt
    ~plugin:"slicing" "slice_preserve_stmt" typer false;
  Acsl_extension.register_code_annot
    ~plugin:"slicing" "slice_preserve_ctrl" typer false;
  let expr_typer typing_context loc args =
    match args with
    | [] -> typing_context.Logic_typing.error loc "Invalid slice directive"
    | _ ->
      let type_term =
        let open Logic_typing in
        typing_context.type_term typing_context typing_context.pre_state
      in
      Ext_terms (List.map type_term args)
  in
  Acsl_extension.register_code_annot
    ~plugin:"slicing" "slice_preserve_expr" expr_typer false

type slice_directive = Stmt | Ctrl | Terms of term list

let slice_directive acsl_extension =
  match acsl_extension.ext_name with
  | "slice_preserve_stmt" -> Some Stmt
  | "slice_preserve_ctrl" -> Some Ctrl
  | "slice_preserve_expr" ->
    begin
      match acsl_extension.ext_kind with
      | Ext_terms terms -> Some (Terms terms)
      | _ -> assert false
    end
  | _ -> None

let is_slice_directive acsl_ext = Option.is_some (slice_directive acsl_ext)


type ctx = {
  site: ctx_site;
  before: bool option;
  kf: Kernel_function.t
}

and ctx_site =
  | FunctionContract
  | StatementContract of stmt
  | StatementAnnotation of stmt

type slices = {ctrl: Stmt.Set.t ; stmt: Stmt.Set.t}
type t = {before:bool ; ki:stmt ; zone:Memory_zone.t}
type zone_info = (t list) option
type decl = {var: Varinfo.Set.t ; lbl: Logic_label.Set.t}

let mk_ctx_func_contract ?before kf =
  { before; site=FunctionContract; kf }

let mk_ctx_stmt_contract ?before kf stmt =
  { before; site=StatementContract stmt; kf }

let mk_ctx_stmt_annot kf stmt =
  { before=Some true; site=StatementAnnotation stmt; kf }

type result = {
  slices: slices;
  locals: Varinfo.Set.t;
  labels: Logic_label.Set.t;
  zones: (Memory_zone.t * Memory_zone.t) Stmt.Map.t option;
}

let empty_slices =
  { ctrl = Stmt.Set.empty;
    stmt = Stmt.Set.empty }

let empty_results = {
  slices = empty_slices;
  locals = Varinfo.Set.empty;
  labels = Logic_label.Set.empty;
  zones = Some Stmt.Map.empty;
}

let add_top_zone not_yet_implemented_msg =
  function
  | None -> (* top zone *) None
  | Some _ ->
    not_yet_implemented := not_yet_implemented_msg;
    None

let add_zone ~before ki zone =
  function
  | None -> (* top zone *) None
  | Some other_zones ->
    let zone_true, zone_false =
      try Stmt.Map.find ki other_zones
      with Not_found -> Memory_zone.bottom, Memory_zone.bottom
    in
    let new_zone =
      if before
      then Memory_zone.join zone_true zone, zone_false
      else zone_true, Memory_zone.join zone_false zone
    in
    Some (Stmt.Map.add ki new_zone other_zones)

let get_result result =
  let zones =
    match result.zones with
    | None -> None
    | Some other_zones ->
      let zones =
        Stmt.Map.to_seq other_zones |>
        Seq.flat_map (fun (ki, (zone_true, zone_false)) ->
            List.to_seq [
              { before=false; ki; zone=zone_false } ;
              { before=true; ki; zone=zone_true }
            ]) |>
        Seq.filter (fun x ->
            not (Memory_zone.equal Memory_zone.bottom x.zone)) |>
        List.of_seq
      in
      Some zones
  in
  zones, {var = result.locals; lbl = result.labels}

let get_annot_result result =
  get_result result, result.slices

(** Logic_var utility: *)
let extract_locals logicvars =
  Logic_var.Set.fold
    (fun lv cvars -> match lv.lv_origin with
       | None -> cvars
       | Some cvar ->
         if cvar.Cil_types.vglob then cvars
         else Varinfo.Set.add cvar cvars)
    logicvars
    Varinfo.Set.empty

(** Term utility:
    Extract C local variables occurring into a [term]. *)
let extract_locals_from_term term =
  extract_locals (Cil.extract_free_logicvars_from_term term)

(** Predicate utility:
    Extract C local variables occurring into a [term]. *)
let extract_locals_from_pred pred =
  extract_locals (Cil.extract_free_logicvars_from_predicate pred)

type abs_label = | AbsLabel_here
                 | AbsLabel_pre
                 | AbsLabel_post
                 | AbsLabel_init
                 | AbsLabel_loop_entry
                 | AbsLabel_loop_current
                 | AbsLabel_stmt of stmt

let is_same_label absl l =
  match absl, l with
  | AbsLabel_stmt s1, StmtLabel s2 -> Cil_datatype.Stmt.equal s1 !s2
  | AbsLabel_here, BuiltinLabel Here -> true
  | AbsLabel_pre, BuiltinLabel Pre -> true
  | AbsLabel_post, BuiltinLabel Post -> true
  | AbsLabel_init, BuiltinLabel Init -> true
  | AbsLabel_loop_entry, BuiltinLabel LoopEntry -> true
  | AbsLabel_loop_current, BuiltinLabel LoopCurrent -> true
  | _, (StmtLabel _ | FormalLabel _ | BuiltinLabel _) -> false

let populate_zone ctx visit cil_node current_zones =
  (* interpretation from the
     - pre-state if [before=Some true]
     - post-state if [before=Some false]
     - pre-state with possible reference to the post-state if
        [before=None] of a property relative to
     - the contract of function [kf] when [side=FunctionContract]
     - the contract of the statement [ki] when [site=StatementContract stmt]
     - the annotation of the statement [ki] when [site=StatementAnnotation stmt] *)
  let vis = object(self)
    inherit Visitor.frama_c_inplace
    val mutable current_label = AbsLabel_here
    val mutable zones = current_zones
    method get_zones = zones

    method private get_ctrl_point () =
      let get_fct_entry_point () =
        (* TODO: to replace by true, None *)
        true,
        (try Some (Kernel_function.find_first_stmt ctx.kf)
         with Kernel_function.No_Statement ->
           (* raised when [kf] has no code. *)
           None)
      in
      let get_ctrl_point dft =
        let before = Option.value ~default:dft ctx.before in
        match ctx.site with
        | FunctionContract ->
          if before then get_fct_entry_point ()
          else before, None
        (* statement contract *)
        | StatementContract stmt | StatementAnnotation stmt ->  (* statement contract and code annotation *)
          before, Some stmt
      in
      let result = match current_label with
        | AbsLabel_stmt stmt -> true, Some stmt
        | AbsLabel_pre -> get_fct_entry_point ()
        | AbsLabel_here -> get_ctrl_point true
        | AbsLabel_post -> get_ctrl_point false
        | AbsLabel_init -> raise (NYI "[logic_interp] Init label")
        | AbsLabel_loop_current ->
          raise (NYI "[logic_interp] LoopCurrent label")
        | AbsLabel_loop_entry ->
          raise (NYI "[logic_interp] LoopEntry label")
      in (* TODO: the method should be able to return result directly *)
      match result with
      | current_before, Some current_stmt -> current_before, current_stmt
      | _ -> raise (NYI
                      "[logic_interp] clause related to a function contract")

    method private change_label: 'a.abs_label -> 'a -> 'a Cil.visitAction =
      fun label x ->
      let old_label = current_label in
      current_label <- label;
      ChangeDoChildrenPost
        (x,fun x -> current_label <- old_label; x)

    method private change_label_to_here: 'a.'a -> 'a Cil.visitAction =
      fun x ->
      self#change_label AbsLabel_here x

    method private change_label_to_old: 'a.'a -> 'a Cil.visitAction =
      fun x ->
      match ctx.site, ctx.before with
      (* function contract *)
      | FunctionContract, Some true ->
        failwith "The use of the label Old is forbidden inside clauses \
                  related to the pre-state of function contracts."
      | FunctionContract, None
      | FunctionContract, Some false ->
        (* refers to the pre-state of the contract. *)
        self#change_label AbsLabel_pre x
      (* statement contract *)
      | StatementContract _stmt, Some true  ->
        failwith "The use of the label Old is forbidden inside clauses \
                  related to the pre-state of statement contracts."
      | StatementContract stmt, None
      | StatementContract stmt, Some false  ->
        (* refers to the pre-state of the contract. *)
        self#change_label (AbsLabel_stmt stmt) x
      (* code annotation *)
      | StatementAnnotation _stmt, _ ->
        (* refers to the pre-state of the function contract. *)
        self#change_label AbsLabel_pre x

    method private change_label_to_post: 'a.'a -> 'a Cil.visitAction =
      fun x ->
      (* allowed when [before_opt=None] for function/statement contracts *)
      match ctx.site, ctx.before with
      (* function contract *)
      | FunctionContract, Some _ ->
        failwith "Function contract where the use of the label Post is \
                  forbidden."
      | FunctionContract, None ->
        (* refers to the post-state of the contract. *)
        self#change_label AbsLabel_post x
      (* statement contract *)
      | StatementContract _stmt, Some _  ->
        failwith "Statement contract where the use of the label Post is \
                  forbidden."
      | StatementContract _stmt,None ->
        (* refers to the pre-state of the contract. *)
        self#change_label AbsLabel_post x
      (* code annotation *)
      | StatementAnnotation _stmt, _ ->
        failwith "The use of the label Post is forbidden inside code \
                  annotations."

    method private change_label_to_pre: 'a.'a -> 'a Cil.visitAction =
      fun x ->
      match ctx.site with
      (* function contract *)
      | FunctionContract ->
        failwith "The use of the label Pre is forbidden inside function \
                  contracts."
      (* statement contract *)
      (* code annotation *)
      | StatementContract _ | StatementAnnotation _ ->
        (* refers to the pre-state of the function contract. *)
        self#change_label AbsLabel_pre x

    method private change_label_aux: 'a. _ -> 'a -> 'a Cil.visitAction =
      fun lbl x -> self#change_label lbl x

    method private change_label_to_stmt: 'a.stmt -> 'a -> 'a Cil.visitAction =
      fun stmt x ->
      match ctx.site with
      (* function contract *)
      | FunctionContract ->
        failwith "the use of C labels is forbidden inside clauses related \
                  to function contracts."
      (* statement contract *)
      (* code annotation *)
      | StatementContract _ | StatementAnnotation _ ->
        (* refers to the state at the C label of the statement [stmt]. *)
        self#change_label (AbsLabel_stmt stmt) x


    method! vpredicate_node p =
      let fail () =
        raise (NYI (Format.asprintf
                      "[logic_interp] %a" Printer.pp_predicate_node p))
      in
      match p with
      | Pat (_, BuiltinLabel Old) -> self#change_label_to_old p
      | Pat (_, BuiltinLabel Here) -> self#change_label_to_here p
      | Pat (_, BuiltinLabel Pre) -> self#change_label_to_pre p
      | Pat (_, BuiltinLabel Post) -> self#change_label_to_post p
      | Pat (_, BuiltinLabel Init) ->
        self#change_label_aux AbsLabel_init p
      | Pat (_, BuiltinLabel LoopCurrent) ->
        self#change_label_aux AbsLabel_loop_current p
      | Pat (_, BuiltinLabel LoopEntry) ->
        self#change_label_aux AbsLabel_loop_entry p
      | Pat (_, FormalLabel s) ->
        failwith ("unknown logic label" ^ s)
      | Pat (_, StmtLabel st) -> self#change_label_to_stmt !st p
      | Pfalse | Ptrue | Prel _ | Pand _ | Por _ | Pxor _ | Pimplies _
      | Piff _ | Pnot _ | Pif _ | Plet _ | Pforall _ | Pexists _
      | Papp (_, [], _) (* No label, thus cannot access memory *)
      | Paligned (_, _) (* No label, thus cannot access memory *)
      | Pseparated _ (* need only to preserve the values of each pointer *)
        -> DoChildren

      | Pinitialized (lbl, t) | Pdangling (lbl, t) ->
        (* Dependencies of [\initialized(p)] or [\dangling(p)] are the
            dependencies of [*p]. *)
        if is_same_label current_label lbl then (
          let typ = Logic_utils.type_of_pointed t.term_type in
          let tlv = Cil.mkTermMem ~addr:t ~off:TNoOffset in
          let tlv' = Logic_const.term (TLval tlv) typ in
          self#do_term_lval tlv';
          DoChildren
        )
        else fail ()

      | Pvalid_read (_lbl, _) | Pvalid (_lbl, _) ->
        (* Does not take dynamic allocation into account, but then
            Value does not either. [lbl] can be ignored because they are
            taken into account by the functions [from_...] below *)
        DoChildren

      | Pobject_pointer _ | Pvalid_function _ ->
        DoChildren

      | Papp _ | Pallocable _ | Pfreeable _ | Pfresh _
        -> fail ()

    method private do_term_lval t =
      let current_before, current_stmt = self#get_ctrl_point () in
      match !compute_term_deps current_stmt t with
      | Some zone ->
        let filter = function Base.CLogic_Var _ -> false | _ -> true in
        let zone = Memory_zone.filter_base filter zone in
        zones <- add_zone ~before:current_before current_stmt zone zones
      | None ->
        raise (NYI "[logic_interp] dependencies of a term lval")

    method! vterm t =
      match t.term_node with
      | TAddrOf _ | TLval (TMem _,_)
      | TLval(TVar {lv_origin = Some _},_) | TStartOf _  ->
        self#do_term_lval t;
        SkipChildren
      | Tat (_, BuiltinLabel Old) -> self#change_label_to_old t
      | Tat (_, BuiltinLabel Here) -> self#change_label_to_here t
      | Tat (_, BuiltinLabel Pre) -> self#change_label_to_pre t
      | Tat (_, BuiltinLabel Post) -> self#change_label_to_post t
      | Tat (_, BuiltinLabel Init) ->
        self#change_label_aux AbsLabel_init t
      | Tat (_, BuiltinLabel LoopCurrent) ->
        self#change_label_aux AbsLabel_loop_current t
      | Tat (_, BuiltinLabel LoopEntry) ->
        self#change_label_aux AbsLabel_loop_entry t
      | Tat (_, StmtLabel st) -> self#change_label_to_stmt !st t
      | Tat (_, FormalLabel s) ->
        failwith ("unknown logic label" ^ s)
      | TSizeOf _ | TSizeOfE _ | TAlignOf _ | TAlignOfE _ ->
        (* These are static constructors, there are no dependencies here *)
        SkipChildren
      | _ -> DoChildren
  end
  in
  try
    ignore (visit (vis :> Visitor.frama_c_inplace) cil_node);
    vis#get_zones
  with NYI msg ->
    add_top_zone msg (vis#get_zones)

let update_slices f results =
  { results with slices = f results.slices }

let add_ctrl_slice stmt =
  update_slices (fun x -> { x with ctrl = Stmt.Set.add stmt x.ctrl })

let add_stmt_slice stmt =
  update_slices (fun x -> { x with stmt = Stmt.Set.add stmt x.stmt })

let add_results_from_term ctx results t =
  let zones = populate_zone ctx Visitor.visitFramacTerm t results.zones in
  {
    results with
    zones;
    locals = Varinfo.Set.union (extract_locals_from_term t) results.locals;
    labels = Logic_label.Set.union (Cil.extract_labels_from_term t) results.labels
  }

let add_results_from_pred ctx results p =
  let zones = populate_zone ctx Visitor.visitFramacPredicate p results.zones in
  {
    results with
    zones;
    locals = Varinfo.Set.union (extract_locals_from_pred p) results.locals;
    labels = Logic_label.Set.union (Cil.extract_labels_from_pred p) results.labels
  }

(** Entry point to get the list of [ki] * [Memory_zone.t]
    needed to evaluate the list of [terms]
    relative to the [ctx] of interpretation. *)
let from_terms terms ctx =
  List.fold_left (add_results_from_term ctx) empty_results terms |>
  get_result

(** Entry point to get the list of [ki] * [Memory_zone.t]
    needed to evaluate the [term]
    relative to the [ctx] of interpretation. *)
let from_term term ctx =
  add_results_from_term ctx empty_results term |>
  get_result

(** Entry point to get the list of [ki] * [Memory_zone.t]
    needed to evaluate the list of [preds]
    relative to the [ctx] of interpretation. *)
let from_preds preds ctx =
  List.fold_left (add_results_from_pred ctx) empty_results preds |>
  get_result

(** Entry point to get the list of [ki] * [Memory_zone.t]
    needed to evaluate the [pred]
    relative to the [ctx] of interpretation. *)
let from_pred pred ctx =
  add_results_from_pred ctx empty_results pred |>
  get_result

(** Used by annotations entry points. *)
let get_zone_from_annot a (ki,kf) loop_body_opt results =
  let get_zone_from_term k term results =
    let ctx = mk_ctx_stmt_annot kf k in
    add_results_from_term ctx results term
  and get_zone_from_term_list k terms results =
    let ctx = mk_ctx_stmt_annot kf k in
    List.fold_left (add_results_from_term ctx) results terms
  and get_zone_from_pred k pred results =
    let ctx = mk_ctx_stmt_annot kf k in
    add_results_from_pred ctx results pred
  in
  match a.annot_content with
  | AAssert (_behav,pred) ->
    (* to preserve the interpretation of the assertion *)
    get_zone_from_pred ki pred.tp_statement results
  | AInvariant (_behav,true,pred) -> (* loop invariant *)
    (* WARNING this is obsolete *)
    (* [JS 2010/09/02] TODO: so what is the right way to do? *)
    (* to preserve the interpretation of the loop invariant *)
    get_zone_from_pred (Option.get loop_body_opt) pred.tp_statement results
  | AInvariant (_behav,false,pred) -> (* code invariant *)
    (* to preserve the interpretation of the code invariant *)
    get_zone_from_pred ki pred.tp_statement results
  | AVariant (term,_) ->
    (* to preserve the interpretation of the variant *)
    get_zone_from_term (Option.get loop_body_opt) term results
  | AAllocation (_,FreeAllocAny) -> results
  | AAllocation (_,FreeAlloc(f,a)) ->
    let get_zone results x =
      get_zone_from_term (Option.get loop_body_opt) x.it_content results
    in
    let results = List.fold_left get_zone results f in
    let results = List.fold_left get_zone results a in
    results
  | AAssigns (_, WritesAny) -> results
  | AAssigns (_, Writes l) -> (* loop assigns *)
    let get_zone results x =
      get_zone_from_term (Option.get loop_body_opt) x.it_content results
    in
    List.fold_left
      (fun results (zone,deps) ->
         let results = get_zone results zone in
         match deps with
           FromAny -> results
         | From l -> List.fold_left get_zone results l)
      results l
  | AStmtSpec _ -> (* TODO *)
    raise (NYI "[logic_interp] statement contract")
  | AExtended (_, _, acsl_extension) ->
    begin
      match slice_directive acsl_extension with
      | Some Stmt ->
        (* to preserve the effect of the statement *)
        add_stmt_slice ki results
      | Some Ctrl ->
        (* to select the reachability of the slice directive *)
        add_ctrl_slice ki results
      | Some (Terms terms) ->
        results |>
        (* to preserve the interpretation of the term *)
        get_zone_from_term_list ki terms |>
        (* to select the reachability of the directive *)
        add_ctrl_slice ki
      | None ->
        match acsl_extension.ext_kind with
        | Ext_preds preds ->
          (* to select the declaration of the variables *)
          List.fold_left
            (fun results pred -> {
                 results with
                 locals = Varinfo.Set.union (extract_locals_from_pred pred) results.locals;
                 labels = Logic_label.Set.union (Cil.extract_labels_from_pred pred) results.labels
               })
            results preds
        | Ext_terms terms ->
          (* to select the declaration of the variables *)
          List.fold_left
            (fun results term -> {
                 results with
                 locals = Varinfo.Set.union (extract_locals_from_term term) results.locals;
                 labels = Logic_label.Set.union (Cil.extract_labels_from_term term) results.labels
               })
            results terms
        | _ -> raise (NYI "[logic_interp] extension")
    end


(* Used by annotations entry points. *)
let get_from_stmt_annots code_annot_filter ((ki, _kf) as stmt) results =
  Option.fold
    ~none:results
    ~some:(fun caf ->
        let loop_body_opt = match ki.skind with
          | Loop(_, { bstmts = body :: _ }, _, _, _) -> Some body
          | _ -> None
        in
        Annotations.fold_code_annot
          (fun _ a results ->
             if caf a
             then get_zone_from_annot a stmt loop_body_opt results
             else results)
          ki results)
    code_annot_filter

(** Used by annotations entry points. *)
let from_ki_annot annot ((ki, _kf) as stmt) =
  let real_ki = match ki.skind with
      Loop(_,{bstmts = loop_entry::_},_,_,_) -> Some loop_entry
    | _ -> None
  in
  get_zone_from_annot annot stmt real_ki

(** Entry point to get the list of [ki] * [Memory_zone.t]
    needed to evaluate the code annotations related to this [stmt]. *)
let from_stmt_annot annot stmt =
  from_ki_annot annot stmt empty_results |>
  get_annot_result

(** Entry point to get the list of [ki] * [Memory_zone.t]
    needed to evaluate the code annotations related to this [stmt]. *)
let from_stmt_annots code_annot_filter stmt =
  get_from_stmt_annots code_annot_filter stmt empty_results |>
  get_annot_result

(** Entry point to get the list of [ki] * [Memory_zone.t]
    needed to evaluate the code annotations related to this [kf]. *)
let from_func_annots iter_on_kf_stmt code_annot_filter kf =
  let results = ref empty_results in
  let from_stmt_annots ki =
    results := get_from_stmt_annots code_annot_filter (ki, kf) !results
  in
  iter_on_kf_stmt from_stmt_annots kf;
  get_annot_result !results

(** To quickly build a annotation filter *)
let code_annot_filter annot ~threat ~user_assert ~slicing_annot ~loop_inv ~loop_var ~others =
  match annot.annot_content with
  | AAssert _ ->
    (match Alarms.find annot with
     | None -> user_assert
     | Some _a -> threat)
  | AVariant _ -> loop_var
  | AInvariant(_behav,true,_pred) -> loop_inv
  | AInvariant(_,false,_) -> others
  | AAllocation _ -> others
  | AAssigns _ -> others
  | AExtended (_, _, ext) when is_slice_directive ext -> slicing_annot
  | AStmtSpec _ | AExtended _ (* TODO *) -> false


exception Prune

let to_result_from_pred p =
  let visitor = object (_self)
    inherit Visitor.frama_c_inplace

    method! vterm_lhost t =
      match t with
      | TResult _ -> raise Prune
      | _ -> DoChildren

  end
  in
  (try
     ignore(Visitor.visitFramacPredicate visitor p);
     false
   with Prune ->
     true)
