(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cvalue

(* Truth values for a predicate analyzed by the value analysis *)

type predicate_status = Abstract_interp.Comp.result = True | False | Unknown

let join_predicate_status x y = match x, y with
  | True, True -> True
  | False, False -> False
  | True, False | False, True
  | Unknown, _ | _, Unknown -> Unknown

(* Type of possible errors during evaluation. See pretty-printer for details *)
type logic_evaluation_error =
  | Unsupported of string
  | UnsupportedLogicVar of logic_var
  | AstError of string
  | NoEnv of logic_label
  | NoResult
  | CAlarm

let pretty_logic_evaluation_error fmt = function
  | Unsupported s -> Format.fprintf fmt "unsupported ACSL construct: %s" s
  | UnsupportedLogicVar tv ->
    Format.fprintf fmt "unsupported logic var %s" tv.lv_name
  | AstError s -> Format.fprintf fmt "error in AST: %s; please report" s
  | NoEnv (FormalLabel s) ->
    Format.fprintf fmt "no environment to evaluate \\at(_,%s)" s
  | NoEnv (BuiltinLabel l) ->
    Format.fprintf fmt "no environment to evaluate \\at(_,%a)"
      Printer.pp_logic_builtin_label l
  | NoEnv (StmtLabel _) ->
    Format.fprintf fmt "\\at() on a C label is unsupported"
  | NoResult -> Format.fprintf fmt "meaning of \\result not specified"
  | CAlarm -> Format.fprintf fmt "alarm during evaluation"

exception LogicEvalError of logic_evaluation_error

let unsupported s = raise (LogicEvalError (Unsupported s))
let unsupported_lvar v = raise (LogicEvalError (UnsupportedLogicVar v))
let ast_error s = raise (LogicEvalError (AstError s))
let no_env lbl = raise (LogicEvalError (NoEnv lbl))
let no_result () = raise (LogicEvalError NoResult)
let c_alarm () = raise (LogicEvalError CAlarm)

(** Three modes to handle the alarms when evaluating a logical term. *)
type alarm_mode =
  | Ignore            (* Ignores all alarms. *)
  | Fail              (* Raises a LogicEvalError when an alarm is encountered. *)
  | Track of bool ref (* Tracks the possibility of an alarm in the boolean. *)

(* Process the possibility of an alarm according to the alarm_mode.
   The boolean [b] is true when an alarm is possible. *)
let track_alarms b = function
  | Ignore -> ()
  | Fail -> if b then c_alarm ()
  | Track bref -> if b then bref := true

let display_evaluation_error ~loc = function
  | CAlarm -> ()
  | pa ->
    Self.warning ~wkey:Self.wkey_acsl_unsupported ~source:(fst loc) ~once:true
      "cannot evaluate ACSL term, %a" pretty_logic_evaluation_error pa

(* Warning mode use when performing _reductions_ in the logic ( ** not **
   evaluation). "Logic alarms" are ignored, and the reduction proceeds as if
   they had not occurred. *)
let alarm_reduce_mode () =
  if Parameters.ReduceOnLogicAlarms.get () then Ignore else Fail

let find_indeterminate ~alarm_mode state loc =
  let is_invalid = not Locations.(is_valid Read loc) in
  track_alarms is_invalid alarm_mode;
  Model.find_indeterminate ~conflate_bottom:true state loc

let find_or_alarm ~alarm_mode state loc =
  let v = find_indeterminate ~alarm_mode state loc in
  let is_indeterminate = Cvalue.V_Or_Uninitialized.is_indeterminate v in
  track_alarms is_indeterminate alarm_mode;
  V_Or_Uninitialized.get_v v

(* Returns true if [loc] contains a memory location definitely invalid
   for a memory access of kind [access]. *)
let contains_invalid_loc access loc =
  let valid_loc = Locations.valid_part access loc in
  not (Locations.equal loc valid_loc)

(* -------------------------------------------------------------------------- *)
(* --- Evaluation environments.                                           --- *)
(* -------------------------------------------------------------------------- *)

(* Evaluation environments are used to evaluate predicate on \at nodes,
   the varinfo \result and logic variables introduced by quantifiers. *)

(* Labels:
   pre: pre-state of the function. Equivalent to \old in the postcondition
     (and displayed as such)
   here: current location, always the intuitive meaning. Assertions are
     evaluated before the statement.
   post: forbidden in preconditions;
     In postconditions:
      in function contracts, state of in the post-state
      in statement contracts, state after the evaluation of the statement
   old: forbidden in assertions.
        In statement contracts post, means the state before the statement
        In functions contracts post, means the pre-state
*)

(* TODO: evaluating correctly Pat with the current Value domain is tricky,
   and only works reliably for the four labels below, that are either
   invariant during the course of the program, or fully local. The
   program below shows the problem:
   if (c) x = 1; else x = 3;
   L:
   x = 1;
   \assert \at(x == 1, L);
   A naive implementation of assertions involving C labels is likely to miss
   the fact that the assertion is false after the else branch. A good
   solution is to use a dummy edge that flows from L to the assertion,
   to force its re-evaluation.
*)

module Logic_label = Cil_datatype.Logic_label
type labels_states = Cvalue.Model.t Logic_label.Map.t

let join_label_states m1 m2 =
  let aux _ s1 s2 = Some (Cvalue.Model.join s1 s2) in
  if m1 == m2 then m1
  else Logic_label.Map.union aux m1 m2

(* Environment for mathematical variables introduced by quantifiers:
   maps from their int identifiers to cvalues representing an over-approximation
   of their values (either mathematical integers or mathematical reals). *)
module LogicVarEnv = struct
  include Cil_datatype.Logic_var.Map

  let find logic_var map =
    try find logic_var map
    with Not_found -> unsupported_lvar logic_var

  let join m1 m2 =
    let aux _ v1 v2 = Some (Cvalue.V.join v1 v2) in
    if m1 == m2 then m1
    else union aux m1 m2
end

type logic_env = Cvalue.V.t LogicVarEnv.t

(* The logic can refer to the state at other points of the program
   using labels. [e_cur] indicates the current label (in changes when
   evaluating the term in a \at(label,term). [e_states] associates a
   memory state to each label. [logic_vars] binds mathematical variables
   to their possible values. [result] contains the variable
   corresponding to \result; this works even with leaf functions
   without a body. [result] is None when \result is meaningless
   (e.g. the function returns void, logic outside of a function
   contract, etc.) *)
type eval_env = {
  e_cur: logic_label;
  e_states: labels_states;
  logic_vars: logic_env;
  result: varinfo option;
}

let join_env e1 e2 = {
  e_cur = (assert (Logic_label.equal e1.e_cur e2.e_cur); e1.e_cur);
  e_states = join_label_states e1.e_states e2.e_states;
  logic_vars = LogicVarEnv.join e1.logic_vars e2.logic_vars;
  result = (assert (e1.result == e2.result); e1.result);
}

let env_state env lbl =
  try Logic_label.Map.find lbl env.e_states
  with Not_found -> no_env lbl

let env_current_state e = env_state e e.e_cur

let overwrite_state env state lbl =
  { env with e_states = Logic_label.Map.add lbl state env.e_states }

let overwrite_current_state env state = overwrite_state env state env.e_cur

let lbl_here = Logic_const.here_label

let add_logic ll state (states: labels_states): labels_states =
  Logic_label.Map.add ll state states
let add_here = add_logic Logic_const.here_label
let add_pre = add_logic Logic_const.pre_label
let add_post = add_logic Logic_const.post_label
let add_old = add_logic Logic_const.old_label
(* Init is a bit special, it is constant and always added to the initial state*)
let add_init states =
  match Cvalue_results.get_state Initial with
  | `Bottom | `Top -> states
  | `Value state -> add_logic Logic_const.init_label state states

let make_env logic_env state =
  let transfer label map =
    Logic_label.Map.add label (logic_env.Abstract_domain.states label) map
  in
  let map =
    Logic_label.Map.add lbl_here state
      (transfer Logic_const.pre_label
         (transfer Logic_const.old_label
            (transfer Logic_const.post_label
               (add_init Logic_label.Map.empty))))
  in
  { e_cur = lbl_here;
    e_states = map;
    logic_vars = LogicVarEnv.empty;
    result = logic_env.Abstract_domain.result }

let env_pre_f ~pre () = {
  e_cur = lbl_here;
  e_states = add_here pre (add_pre pre (add_init Logic_label.Map.empty));
  logic_vars = LogicVarEnv.empty;
  result = None (* Never useful in a pre *);
}

let env_post_f ?(c_labels=Logic_label.Map.empty) ~pre ~post ~result () = {
  e_cur = lbl_here;
  e_states = add_post post
      (add_here post (add_pre pre (add_old pre (add_init c_labels))));
  logic_vars = LogicVarEnv.empty;
  result = result;
}

let env_annot ?(c_labels=Logic_label.Map.empty) ~pre ~here () = {
  e_cur = lbl_here;
  e_states = add_here here (add_pre pre (add_init c_labels));
  logic_vars = LogicVarEnv.empty;
  result = None (* Never useful in a 'assert'. TODO: will be needed for stmt
                   contracts *);
}

let env_assigns ~pre = {
  e_cur = lbl_here;
  (* YYY: Post label is missing, but is too difficult in the current evaluation
          scheme, since we build it by evaluating the assigns... *)
  e_states = add_old pre
      (add_here pre (add_pre pre (add_init Logic_label.Map.empty)));
  logic_vars = LogicVarEnv.empty;
  result = None (* Treated in a special way in callers *)
}

let env_only_here state = {
  e_cur = lbl_here;
  e_states = add_here state (add_init Logic_label.Map.empty);
  logic_vars = LogicVarEnv.empty;
  result = None (* Never useful in a 'assert'. TODO: will be needed for stmt
                   contracts *);
}

let top_float =
  let neg_infinity = Fval.F.of_float neg_infinity
  and pos_infinity = Fval.F.of_float infinity in
  let fval = Fval.inject ~nan:false Fval.Real neg_infinity pos_infinity in
  Ival.inject_float fval

let bind_logic_vars env lvs =
  let bind_one (state, logic_vars) lv =
    let bind_logic_var ival =
      state, LogicVarEnv.add lv (Cvalue.V.inject_ival ival) logic_vars
    in
    match Ast_types.unroll_logic lv.lv_type with
    | Linteger -> bind_logic_var Ival.top
    | Lreal -> bind_logic_var top_float
    | Ctype ctyp when Ast_types.is_integral ctyp ->
      let base = Base.of_c_logic_var lv in
      let size = Z.of_int (Cil.bitsSizeOf ctyp) in
      let v = Cvalue.V_Or_Uninitialized.initialized V.top_int in
      let state = Model.add_base_value base ~size v ~size_v:Z.one state in
      state, logic_vars
    | _ -> unsupported_lvar lv
  in
  let state = env_current_state env in
  let state, logic_vars = List.fold_left bind_one (state, env.logic_vars) lvs in
  overwrite_current_state { env with logic_vars } state

let add_logic_var env lvar value =
  match Ast_types.unroll_logic lvar.lv_type with
  | Linteger | Lreal ->
    let logic_vars = LogicVarEnv.add lvar value env.logic_vars in
    { env with logic_vars }
  | Ctype _ ->
    let base = Base.of_c_logic_var lvar in
    let loc = Locations.of_base base in
    let state = env_current_state env in
    Model.add_binding ~exact:true state loc value
    |> overwrite_current_state env
  | _ -> unsupported_lvar lvar

let find_logic_var env lvar =
  match Ast_types.unroll_logic lvar.lv_type with
  | Linteger | Lreal -> LogicVarEnv.find lvar env.logic_vars
  | Ctype _ ->
    let base = Base.of_c_logic_var lvar in
    let loc = Locations.of_base base in
    Model.find (env_current_state env) loc
  | _ -> unsupported_lvar lvar

let copy_logic_vars ~src ~dst lvars =
  let copy_one dst lvar =
    let value = find_logic_var src lvar in
    add_logic_var dst lvar value
  in
  List.fold_left copy_one dst lvars

(* Splits values of logic variables [lvars] from [env] into singletons, only for
   logic variables with less than -ilevel possible concrete values. Returns a
   list of environments such that an evaluation in [env] can be replaced by the
   union of evaluations in each returned environment. *)
let split_logic_vars env lvars =
  let split_value cvalue =
    match Cvalue.V.project_ival cvalue |> Ival.project_small_set with
    | Some list -> Some (List.map Cvalue.V.inject_int list)
    | None | exception Cvalue.V.Not_based_on_null -> None
  in
  let split_one_var env_list lvar =
    let value = find_logic_var env lvar in
    match split_value value with
    | None -> env_list
    | Some values ->
      let aux env = List.map (add_logic_var env lvar) values in
      List.concat_map aux env_list
  in
  List.fold_left split_one_var [env] lvars

let unbind_logic_vars env lvs =
  let unbind_one (state, logic_vars) lv =
    match Ast_types.unroll_logic lv.lv_type with
    | Linteger | Lreal -> state, LogicVarEnv.remove lv logic_vars
    | Ctype _ ->
      let base = Base.of_c_logic_var lv in
      Model.remove_base base state, logic_vars
    | _ -> unsupported_lvar lv
  in
  let state = env_current_state env in
  let state, logic_vars = List.fold_left unbind_one (state, env.logic_vars) lvs in
  overwrite_current_state { env with logic_vars } state

(* -------------------------------------------------------------------------- *)
(* --- Evaluation results.                                                --- *)
(* -------------------------------------------------------------------------- *)

(* Result of the evaluation of a term as an exact location (to reduce its value
   in the current state). *)
type exact_location =
  | Location of typ * Locations.t
  (* A location represented in the cvalue state. *)
  | Logic_var of logic_var
  (* A logic variable, introduced by a quantifier, and stored in an environment
     besides the states. *)

(* Raised when a term cannot be evaluated as an exact location.*)
exception Not_an_exact_loc

type logic_deps = Memory_zone.t Logic_label.Map.t

let deps_at lbl (ld:logic_deps) =
  try Logic_label.Map.find lbl ld
  with Not_found -> Memory_zone.bottom

let add_deps lbl ldeps deps =
  let prev_deps = deps_at lbl ldeps in
  let deps = Memory_zone.join prev_deps deps in
  let ldeps : logic_deps = Logic_label.Map.add lbl deps ldeps in
  ldeps

let join_logic_deps (ld1:logic_deps) (ld2: logic_deps) : logic_deps =
  let aux _ d1 d2 = match d1, d2 with
    | None as d, None | (Some _ as d), None | None, (Some _ as d) -> d
    | Some d1, Some d2 -> Some (Memory_zone.join d1 d2)
  in
  Logic_label.Map.merge aux ld1 ld2

let empty_logic_deps =
  Logic_label.Map.add lbl_here Memory_zone.bottom Logic_label.Map.empty

(* Type holding the result of an evaluation. Currently, 'a is either
   [Cvalue.V.t] for [eval_term], and [Addresses.Bits.t] for
   [eval_tlval_as_loc], and [Ival.t] for [eval_toffset].

   [eover] is an over-approximation of the evaluation. [eunder] is an
   under-approximation, under the hypothesis that the state in which we
   evaluate is not Bottom. (Otherwise, all under-approximations would be
   Bottom themselves).
   The following two invariants should hold:
   (1) eunder \subset eover.
   (2) when evaluating something that is not a Tset, either eunder = Bottom,
      or eunder = eover, and cardinal(eover) <= 1. This is due to the fact
      that under-approximations are not propagated as an abstract domain, but
      only created from Trange or inferred from exact over-approximations.

   This type can be used for the evaluation of logical sets, in which case
   an eval_result [r] represents all *non-empty* sets [S] such that:
   [r.eunder ⊆ S ⊆ r.eover]. The value {V.bottom} does *not* represents empty
   sets, as for most predicates, P(∅) ≠ P(⊥) (if the latter has a meaning).
   The boolean [empty] indicates whether an eval_result also represents a
   possible empty set. *)
type 'a eval_result = {
  etype: Cil_types.typ;
  eunder: 'a;
  eover: 'a;
  empty: bool;
  ldeps: logic_deps;
}

let join_eval_result r1 r2 =
  let eover = Cvalue.V.join r2.eover r1.eover in
  let eunder = Cvalue.V.meet r1.eunder r2.eunder in
  let ldeps = join_logic_deps r1.ldeps r2.ldeps in
  { eover; eunder; ldeps; etype = r1.etype; empty = r1.empty || r2.empty; }

(* When computing an under-approximation, we make the hypothesis that the state
   is not Bottom. Hence, over-approximations of cardinal <= 1 are actually of
   cardinal 1, and are thus exact. *)
let under_from_over eover =
  if Cvalue.V.cardinal_zero_or_one eover
  then eover
  else Cvalue.V.bottom

let under_loc_from_over eover =
  if Addresses.Bits.cardinal_zero_or_one eover
  then eover
  else Addresses.Bits.bottom

(* Injects [cvalue] as an eval_result of type [typ] with no dependencies. *)
let inject_no_deps typ cvalue =
  { etype = typ;
    eunder = under_from_over cvalue;
    eover = cvalue;
    empty = false;
    ldeps = empty_logic_deps }

(* Integer result with no memory dependencies: constants, sizeof & alignof,
   and values of logic variables.*)
let einteger = inject_no_deps Cil_const.intType

(* Note: some reals cannot be exactly represented as floats; in which
   case we do not know their under-approximation. *)
let ereal fval = inject_no_deps Cil_const.doubleType (Cvalue.V.inject_float fval)
let efloat fval = inject_no_deps Cil_const.floatType (Cvalue.V.inject_float fval)

(* -------------------------------------------------------------------------- *)
(* --- Utility functions on the Cil AST                                 --- *)
(* -------------------------------------------------------------------------- *)

let is_rel_binop = function
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Ne -> true
  | _ -> false

let lop_to_cop op =
  match op with
  | Req -> Eq
  | Rneq -> Ne
  | Rle -> Le
  | Rge -> Ge
  | Rlt -> Lt
  | Rgt -> Gt

let rel_of_binop = function
  | Lt -> Rlt
  | Gt -> Rgt
  | Le -> Rle
  | Ge -> Rge
  | Eq -> Req
  | Ne -> Rneq
  | _ -> assert false

(* Types currently understood in the evaluation of the logic: no arrays,
   structs, logic arrays or subtle ACSL types. Sets of sets seem to
   be flattened, so the current treatment of them is correct. *)
let rec isLogicNonCompositeType t =
  match t with
  | Lvar _ | Larrow _ -> false
  | Ltype (info, _) ->
    info.lt_name = "sign" ||
    (try isLogicNonCompositeType (Logic_const.type_of_element t)
     with Failure _ -> false)
  | Lboolean | Linteger | Lreal -> true
  | Ctype t -> Ast_types.is_scalar t

let rec infer_type = function
  | Ctype t ->
    (match t.tnode with
     | TInt _ -> Cil_const.intType
     | TFloat _ -> Cil_const.doubleType
     | _ -> t)
  | Lvar _ -> Cil_const.voidPtrType (* For polymorphic empty sets *)
  | Lboolean -> Cil_const.boolType
  | Linteger -> Cil_const.intType
  | Lreal -> Cil_const.doubleType
  | Ltype _ | Larrow _ as t ->
    if Logic_const.is_plain_type t then
      unsupported (Pretty_utils.to_string Cil_datatype.Logic_type.pretty t)
    else Logic_const.plain_or_set infer_type t

(* Best effort for comparing the types currently understood by Value: ignore
   differences in integer and floating-point sizes, that are meaningless
   in the logic *)
let same_etype t1 t2 =
  match Ast_types.unroll_node t1, Ast_types.unroll_node t2 with
  | (TInt _ | TEnum _), (TInt _ | TEnum _) -> true
  | TFloat _, TFloat _ -> true
  | TPtr p1, TPtr p2 -> Cil_datatype.Typ.equal p1 p2
  | _, _ -> Cil_datatype.Typ.equal t1 t2

(* Returns the kind of floating-point represented by a logic type, or None. *)
let logic_type_fkind = function
  | Ctype typ -> begin
      match Ast_types.unroll_node typ with
      | TFloat fkind -> Some fkind
      | _ -> None
    end
  | _ -> None

let infer_binop_res_type op targ =
  match op with
  | PlusA | MinusA | Mult | Div -> targ
  | PlusPI | MinusPI ->
    assert (Ast_types.is_ptr targ); targ
  | MinusPP -> Cil_const.intType
  | Mod | Shiftlt | Shiftrt | BAnd | BXor | BOr ->
    (* can only be applied on integral arguments *)
    assert (Ast_types.is_integral targ); Cil_const.intType
  | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr ->
    Cil_const.intType (* those operators always return a boolean *)

(* Computes [*tsets], assuming that [tsets] has a pointer type. *)
let deref_tsets tsets = Cil.mkTermMem ~addr:tsets ~off:TNoOffset

(* returns true iff the logic variable is defined by the
   Frama-C standard library *)
let comes_from_fc_stdlib lvar =
  Cil.is_in_libc lvar.lv_attr ||
  match lvar.lv_origin with
  | None -> false
  | Some vi ->
    Cil.is_in_libc vi.vattr

let is_noop_cast ~src_typ ~dst_typ =
  let src_typ = Logic_const.plain_or_set
      (fun lt ->
         match Ast_types.unroll_logic lt with
         | Ctype typ -> Eval_typ.classify_as_scalar typ
         | _ -> None
      ) (Ast_types.unroll_logic src_typ)
  in
  let open Eval_typ in
  match src_typ, Eval_typ.classify_as_scalar dst_typ with
  | Some (TSInt rsrc), Some (TSInt rdst) ->
    Eval_typ.range_inclusion rsrc rdst
  | Some (TSFloat srckind), Some (TSFloat destkind) ->
    Cil.frank srckind <= Cil.frank destkind
  | Some (TSPtr _), Some (TSPtr _) -> true
  | _ -> false

(* If casting [trm] to [typ] has no effect in terms of the values contained
   in [trm], do nothing. Otherwise, raise [exn]. Adapted from [pass_cast] *)
let pass_logic_cast exn typ trm =
  match Ast_types.(unroll_logic typ, unroll_logic trm.term_type) with
  | Linteger, Ctype { tnode = (TInt _ | TEnum _) } -> () (* Always inclusion *)
  | Ctype ({ tnode = (TInt _ | TEnum _) } as typ),
    Ctype ({ tnode = (TInt _ | TEnum _) } as typeoftrm) ->
    let sztyp = Bit_utils.sizeof typ in
    let szexpr = Bit_utils.sizeof typeoftrm in
    let styp, sexpr =
      match sztyp, szexpr with
      | `Value styp, `Value sexpr -> styp, sexpr
      | _ -> raise exn
    in
    let sityp = Bit_utils.is_signed_int_enum_pointer typ in
    let sisexpr = Bit_utils.is_signed_int_enum_pointer typeoftrm in
    if (Z.geq styp sexpr && sityp = sisexpr) (* larger, same signedness *)
    || (Z.gt styp sexpr && sityp) (* strictly larger and signed *)
    then ()
    else raise exn

  | Lreal,  Ctype { tnode = (TFloat _) } -> () (* Always inclusion *)
  | Ctype { tnode = (TFloat f1) }, Ctype { tnode = (TFloat f2) } ->
    if Cil.frank f1 < Cil.frank f2
    then raise exn

  | _ -> raise exn (* Not a scalar type *)

let is_same_term_coerce t1 t2 =
  match t1.term_node, t2.term_node with
  | TCast (true,_,_), TCast (true,_,_) -> Logic_utils.is_same_term t1 t2
  | TCast (true, _,t1), _ -> Logic_utils.is_same_term t1 t2
  | _, TCast (true, _,t2) -> Logic_utils.is_same_term t1 t2
  | _ -> Logic_utils.is_same_term t1 t2

(* Constrain the ACSL range [idx] when it is used to access an array of
   [size_arr] cells, and it is a Trange in which one size is not
   specified. (E.g. t[1..] is transformed into t[1..9] when t has size 10). *)
let constraint_trange idx size_arr =
  if Kernel.SafeArrays.get () then
    match idx.term_node with
    | Trange ((None as low), up) | Trange (low, (None as up)) -> begin
        let loc = idx.term_loc in
        match Option.bind Cil.constFoldToInt size_arr with
        | None -> idx
        | Some size ->
          let low = match low with (* constrained l.h.s *)
            | Some _ -> low
            | None -> Some (Logic_const.tint ~loc Z.zero)
          in
          let up = match up with (* constrained r.h.s *)
            | Some _ -> up
            | None -> Some (Logic_const.tint ~loc (Z.pred size))
          in
          Logic_const.trange ~loc (low, up)
      end
    | _ -> idx
  else idx

(* -------------------------------------------------------------------------- *)
(* --- Inlining of defined logic functions and predicates                 --- *)
(* -------------------------------------------------------------------------- *)

type pred_fun_origin = ACSL | Libc

let known_logic_funs = [
  "strlen", Libc;
  "wcslen", Libc;
  "strchr", Libc;
  "wcschr", Libc;
  "memchr_off", Libc;
  "wmemchr_off", Libc;
  "atan2", ACSL;
  "atan2f", ACSL;
  "pow", ACSL;
  "powf", ACSL;
  "fmod", ACSL;
  "fmodf", ACSL;
  "sqrt", ACSL;
  "sqrtf", ACSL;
  "\\sign", ACSL;
  "\\min", ACSL;
  "\\max", ACSL;
  "\\abs", ACSL;
  "\\neg_float",ACSL;
  "\\add_float",ACSL;
  "\\sub_float",ACSL;
  "\\mul_float",ACSL;
  "\\div_float",ACSL;
  "\\neg_float32",ACSL;
  "\\add_float32",ACSL;
  "\\sub_float32",ACSL;
  "\\mul_float32",ACSL;
  "\\div_float32",ACSL;
  "\\neg_float64",ACSL;
  "\\add_float64",ACSL;
  "\\sub_float64",ACSL;
  "\\mul_float64",ACSL;
  "\\div_float64",ACSL;
  "\\neg_double",ACSL;
  "\\add_double",ACSL;
  "\\sub_double",ACSL;
  "\\mul_double",ACSL;
  "\\div_double",ACSL;
  "security_status",ACSL;
]
let known_predicates = [
  "\\warning", ACSL;
  "\\is_finite", ACSL;
  "\\is_infinite", ACSL;
  "\\is_plus_infinity", ACSL;
  "\\is_minus_infinity", ACSL;
  "\\is_NaN", ACSL;
  "\\eq_float", ACSL;
  "\\ne_float", ACSL;
  "\\lt_float", ACSL;
  "\\le_float", ACSL;
  "\\gt_float", ACSL;
  "\\ge_float", ACSL;
  "\\eq_float32", ACSL;
  "\\ne_float32", ACSL;
  "\\lt_float32", ACSL;
  "\\le_float32", ACSL;
  "\\gt_float32", ACSL;
  "\\ge_float32", ACSL;
  "\\eq_float64", ACSL;
  "\\ne_float64", ACSL;
  "\\lt_float64", ACSL;
  "\\le_float64", ACSL;
  "\\gt_float64", ACSL;
  "\\ge_float64", ACSL;
  "\\eq_double", ACSL;
  "\\ne_double", ACSL;
  "\\lt_double", ACSL;
  "\\le_double", ACSL;
  "\\gt_double", ACSL;
  "\\ge_double", ACSL;
  "\\subset", ACSL;
  "\\tainted", ACSL;
  "\\tainted_directly", ACSL;
  "\\tainted_indirectly", ACSL;
  "valid_read_string", Libc;
  "valid_string", Libc;
  "valid_read_wstring", Libc;
  "valid_wstring", Libc;
  "is_allocable", Libc;
]

let is_known_logic_fun_pred known lvi =
  try
    let origin = List.assoc lvi.lv_name known in
    match origin with
    | ACSL -> true
    | Libc -> comes_from_fc_stdlib lvi
  with Not_found -> false

let is_known_logic_fun = is_known_logic_fun_pred known_logic_funs
let is_known_predicate = is_known_logic_fun_pred known_predicates

let inline logic_info =
  let logic_var = logic_info.l_var_info in
  not (is_known_logic_fun logic_var || is_known_predicate logic_var)

(* -------------------------------------------------------------------------- *)
(* --- Auxiliary evaluation functions                                     --- *)
(* -------------------------------------------------------------------------- *)

(* We evaluate the ACSL sign type as integers 1 or -1. Sign values can only be
   constructed through the \sign function (handled in eval_known_logic_function)
   and the \Positive and \Negative constructors (handled in eval_term). They can
   only be compared through equality and disequality; no other operation exists
   on this type, so our interpretation remains correct. *)
let positive_cvalue = Cvalue.V.inject_int Z.one
let negative_cvalue = Cvalue.V.inject_int Z.minus_one

let is_true = function
  | `True -> true
  | `Unknown _ | `False -> false

let truth_to_status = function
  | `True -> True
  | `Unknown _ -> Unknown
  | `False -> False

let reduce_value_with_truth assume value =
  match assume value with
  | `True -> value
  | `Unknown v -> v
  | `False -> Cvalue.V.bottom

(* Check "logic alarms" when evaluating [v1 op v2]. All operators shifts are
   defined unambiguously in ACSL. *)
let check_logic_alarms ~alarm_mode typ (_v1: V.t eval_result) op v2 =
  match op with
  | Div | Mod when Ast_types.is_integral_or_pointer typ ->
    let truth = Cvalue_forward.assume_non_zero v2.eover in
    let division_by_zero = not (is_true truth) in
    track_alarms division_by_zero alarm_mode
  | Shiftlt | Shiftrt -> begin
      (* Check that [e2] is positive. [e1] can be arbitrary, we use
         the arithmetic vision of shifts *)
      try
        let i2 = Cvalue.V.project_ival_bottom v2.eover in
        let valid = Ival.is_included i2 Ival.positive_integers in
        track_alarms (not valid) alarm_mode
      with Cvalue.V.Not_based_on_null -> track_alarms true alarm_mode
    end
  | _ -> ()

(* As usual in this file, [dst_typ] may be misleading: the 'size' is
   meaningless, because [src_typ] may actually be a logic type. Thus,
   this size must not be used below. *)
let cast ~src_typ ~dst_typ v =
  let open Eval_typ in
  match classify_as_scalar dst_typ, classify_as_scalar src_typ with
  | None, _ | _, None  -> v (* unclear whether this happens. *)
  | Some dst, Some src ->
    match dst, src with
    | TSFloat fkind, (TSInt _ | TSPtr _) ->
      Cvalue.V.cast_int_to_float (Fval.kind fkind) v

    | (TSInt dst | TSPtr dst), TSFloat fkind ->
      (* This operation is not fully defined in ACSL. We raise an alarm
         in case of overflow. *)
      if is_true (Cvalue_forward.assume_not_nan ~assume_finite:true fkind v)
      then Cvalue_forward.cast_float_to_int dst v
      else c_alarm ()

    | (TSInt dst | TSPtr dst), (TSInt _ | TSPtr _) ->
      let size = Z.of_int dst.i_bits in
      let signed = dst.i_signed in
      V.cast_int_to_int ~signed ~size v

    | TSFloat fkind, TSFloat _ ->
      Cvalue.V.cast_float_to_float (Fval.kind fkind) v

(* V.cast_int_to_int is unsound when the destination type is _Bool.
   Use this function instead. *)
let cast_to_bool r =
  let contains_zero = V.contains_zero r.eover
  and contains_non_zero = V.contains_non_zero r.eover in
  let eover = V.interp_boolean ~contains_zero ~contains_non_zero in
  { eover; eunder = under_from_over eover; empty = r.empty;
    ldeps = r.ldeps; etype = Cil_const.boolType }

(* Note: "charlen" stands for either strlen or wcslen *)

(* Applies a cvalue [builtin] to the list of arguments [args_list] in the
   current state of [env]. Returns [v, alarms], where [v] is the resulting
   cvalue, which can be bottom. *)
let apply_logic_builtin builtin env args_list =
  (* the call below could in theory return Builtins.Invalid_nb_of_args,
     but logic typing constraints prevent that. *)
  builtin (env_current_state env) args_list

(* Never raises exceptions; instead, returns [-1,+oo] in case of alarms
   (most imprecise result possible for the logic strlen/wcslen predicates). *)
let eval_logic_charlen wrapper env v ldeps =
  let eover =
    let v, alarms = apply_logic_builtin wrapper env [v] in
    if alarms && not (Cvalue.V.is_bottom v)
    then Cvalue.V.inject_ival (Ival.inject_range (Some Z.minus_one) None)
    else v
  in
  let eunder = under_from_over eover in
  (* the C strlen function has type size_t, but the logic strlen function has
     type ℤ (signed) *)
  let etype = Cil_const.intType in
  { etype; ldeps; eover; empty = false; eunder }

(* Evaluates the logical predicates strchr/wcschr. *)
let eval_logic_charchr builtin env s c ldeps_s ldeps_c =
  let eover =
    let v, alarms = apply_logic_builtin builtin env [s; c] in
    if Cvalue.V.is_bottom v then v else
    if alarms then Cvalue.V.zero_or_one else
      let ctrue = Cvalue.V.contains_non_zero v
      and cfalse = Cvalue.V.contains_zero v in
      match ctrue, cfalse with
      | true, true -> Cvalue.V.zero_or_one
      | true, false -> Cvalue.V.singleton_one
      | false, true -> Cvalue.V.singleton_zero
      | false, false -> assert false (* a logic alarm would have been raised*)
  in
  let eunder = under_from_over eover in
  (* the C strchr function has type char*, but the logic strchr predicate has
     type 𝔹 *)
  let etype = Cil_const.boolType in
  let ldeps = join_logic_deps ldeps_s ldeps_c in
  { etype; ldeps; eover; empty = false; eunder }

(* Evaluates the logical functions memchr_off/wmemchr_off. *)
let eval_logic_memchr_off builtin env s c n =
  let minus_one = Cvalue.V.inject_int Z.minus_one in
  let positive = Cvalue.V.inject_ival Ival.positive_integers in
  let pred_n = Cvalue.V.add_untyped ~factor:Z.one n.eover minus_one in
  let n_pos = Cvalue.V.narrow positive pred_n in
  let eover =
    if Cvalue.V.is_bottom n_pos then minus_one else
      let args = [s.eover; c.eover; n_pos] in
      let v, alarms = apply_logic_builtin builtin env args in
      if Cvalue.V.is_bottom v then pred_n else
      if alarms then Cvalue.V.join pred_n v else
      if Cvalue.V.equal n_pos pred_n then v else
        Cvalue.V.join minus_one v
  in
  let ldeps = join_logic_deps s.ldeps (join_logic_deps c.ldeps n.ldeps) in
  { (einteger eover) with ldeps }

(* Evaluates the logical predicate is_allocable, according to the following
   logic:
   - if the size to allocate is always too large (> SIZE_MAX), allocation fails;
   - otherwise, if AllocReturnsNull is true or if the size may exceed SIZE_MAX,
     returns Unknown (to simulate non-determinism);
   - otherwise, allocation always succeeds. *)
let eval_is_allocable size =
  let size_ok = Builtins_malloc.alloc_size_ok size in
  match size_ok, Parameters.AllocReturnsNull.get () with
  | Alarmset.False, _ -> False
  | Alarmset.Unknown, _ | _, true -> Unknown
  | Alarmset.True, false -> True

(* Evaluates a [valid_read_string] or [valid_read_wstring] predicate
   using str* builtins.
   - if [bottom] is obtained, return False;
   - otherwise, if no alarms are emitted, return True;
   - otherwise, return [Unknown]. *)
let eval_valid_read_str ~wide env v =
  let wrapper =
    if wide then Builtins_string.frama_c_wcslen_wrapper
    else Builtins_string.frama_c_strlen_wrapper
  in
  let v, alarms = apply_logic_builtin wrapper env [v] in
  if Cvalue.V.is_bottom v
  then False (* bottom state => string always invalid *)
  else if alarms
  then Unknown (* alarm => string possibly invalid *)
  else True (* no alarm => string always valid for reading *)

(* Evaluates a [valid_string] or [valid_wstring] predicate.
   First, we check the constness of the arguments.
   Then, we evaluate [valid_read_string/valid_read_wstring] on non-const ones. *)
let eval_valid_str ~wide env v =
  assert (not (Cvalue.V.is_bottom v));
  (* filter const bases *)
  let v' = Cvalue.V.filter_base (fun b -> not (Base.is_read_only b)) v in
  if Cvalue.V.is_bottom v' then False (* all bases were const *)
  else
  if Cvalue.V.equal v v' then
    eval_valid_read_str ~wide env v (* all bases non-const *)
  else (* at least one base was const *)
    match eval_valid_read_str ~wide env v with
    | True -> Unknown (* weaken result *)
    | False | Unknown as r -> r

(* Do all the possible values of a location in [state] satisfy [test]?  [loc] is
   an over-approximation of the location, so the answer cannot be [False] even
   if some parts of [loc] do not satisfy [test]. Thus, this function does not
   fold the location, but instead applies [test] to the join of all values
   stored in [loc] in [state].  *)
let forall_in_over_location state loc test =
  let v = Model.find_indeterminate state loc in
  test v

(* Do all the possible values of a location in [state] satisfy [test]?  [loc] is
   an under-approximation of the location, so the answer cannot be [True], as
   the values of some other parts of the location may not satisfy [test].
   However, it is [False] as soon as some part of [loc] contradicts [test]. *)
let forall_in_under_location state loc test =
  let exception EFalse in
  let inspect_value (_, _) (value, _, _) acc =
    match test value with
    | True | Unknown -> acc
    | False -> raise EFalse
  in
  let inspect_itv base itv acc =
    match Cvalue.Model.find_base_or_default base state with
    | `Top | `Bottom -> Unknown
    | `Value offsm ->
      Cvalue.V_Offsetmap.fold_between ~entire:true itv inspect_value offsm acc
  in
  let inspect_base base intervals acc =
    Int_Intervals.fold (inspect_itv base) intervals acc
  in
  let zone = Locations.enumerate_bits loc in
  try Memory_zone.fold_i inspect_base zone Unknown
  with EFalse -> False
     | Abstract_interp.Error_Top -> Unknown

(* Evaluates an universal predicate about the values of a location evaluated to
   [r] in [state]. The predicates holds whenever all the possible values at the
   location satisfy [test]. *)
let eval_forall_predicate state r test =
  let size_bits = Eval_typ.sizeof_lval_typ r.etype in
  let make_loc addr = Locations.make addr size_bits in
  let over_loc = make_loc r.eover in
  if not Locations.(is_valid Read over_loc) then c_alarm ();
  match forall_in_over_location state over_loc test with
  | Unknown ->
    let under_loc = make_loc r.eunder in
    forall_in_under_location state under_loc test
  | True -> True
  | False -> if r.empty then Unknown else False

(* Evaluation of an \initialized predicate on a location evaluated to [r]
   in the state [state]. *)
let eval_initialized state r =
  let test = function
    | V_Or_Uninitialized.C_init_esc _
    | V_Or_Uninitialized.C_init_noesc _ -> True
    | V_Or_Uninitialized.C_uninit_esc _ -> Unknown
    | V_Or_Uninitialized.C_uninit_noesc v ->
      if Addresses.Bytes.is_bottom v then False else Unknown
  in
  eval_forall_predicate state r test

(* Evaluation of a \dangling predicate on a location evaluated to [r]
   in the state [state]. *)
let eval_dangling state r =
  let test = function
    | V_Or_Uninitialized.C_init_esc v ->
      if Addresses.Bytes.is_bottom v then True else Unknown
    | V_Or_Uninitialized.C_uninit_esc _ -> Unknown
    | V_Or_Uninitialized.C_init_noesc _
    | V_Or_Uninitialized.C_uninit_noesc _ -> False
  in
  eval_forall_predicate state r test

(* -------------------------------------------------------------------------- *)
(* --- Evaluation of terms                                                --- *)
(* -------------------------------------------------------------------------- *)

exception Reduce_to_bottom

let int_or_float_op typ int_op float_op =
  match typ.tnode with
  | TInt _ | TPtr _ | TEnum _ -> int_op
  | TFloat _fk -> float_op
  | _ -> ast_error (Format.asprintf
                      "binop on incorrect type %a" Printer.pp_typ typ)

let forward_binop_by_type typ =
  let forward_int = Cvalue_forward.forward_binop_int ~typ
  and forward_float = Cvalue_forward.forward_binop_float Fval.Real in
  int_or_float_op typ forward_int forward_float

let forward_binop typ v1 op v2 =
  match op with
  | Eva_ast.Eq | Ne | Le | Lt | Ge | Gt ->
    let comp = Eva_ast.conv_relation op in
    if Ast_types.is_ptr typ || Cvalue_forward.are_comparable comp v1 v2
    then forward_binop_by_type typ v1 op v2
    else Cvalue.V.zero_or_one
  | _ -> forward_binop_by_type typ v1 op v2

let rec eval_term ~alarm_mode env t =
  match t.term_node with
  | Tat (t, lab) ->
    ignore (env_state env lab);
    eval_term ~alarm_mode { env with e_cur = lab } t

  | TConst (Boolean true) -> einteger Cvalue.V.singleton_one
  | TConst (Boolean false) -> einteger Cvalue.V.singleton_zero
  | TConst (Integer (v, _)) -> einteger (Cvalue.V.inject_int v)
  | TConst (LEnum e) ->
    (match Cil.constFoldToInt e.eival with
     | Some v -> einteger (Cvalue.V.inject_int v)
     | _ -> ast_error "non-evaluable constant")
  | TConst (LChr c) ->
    einteger (Cvalue.V.inject_int (Cil.charConstToInt c))
  | TConst (LReal { r_nearest; r_lower ; r_upper }) -> begin
      if Fc_float.is_nan r_nearest
      then ereal Fval.nan
      else
        let r_lower = Fval.F.of_float r_lower in
        let r_upper = Fval.F.of_float r_upper in
        let f = Fval.inject Fval.Real r_lower r_upper in
        ereal f
    end

  (*  | TConst ((CStr | CWstr) Missing cases *)

  | TAddrOf tlval ->
    let r = eval_tlval ~alarm_mode env tlval in
    { etype = Cil_const.mk_tptr r.etype;
      ldeps = r.ldeps;
      eunder = Addresses.Bits.to_bytes_under r.eunder;
      eover = Addresses.Bits.to_bytes r.eover;
      empty = r.empty; }

  | TStartOf tlval ->
    let r = eval_tlval ~alarm_mode env tlval in
    { etype = Cil_const.mk_tptr (Ast_types.direct_element_type r.etype);
      ldeps = r.ldeps;
      eunder = Addresses.Bits.to_bytes_under r.eunder;
      eover = Addresses.Bits.to_bytes r.eover;
      empty = r.empty; }

  (* Special case for the constants \pi, \e, \infinity and \NaN. *)
  | TLval (TVar {lv_name = "\\pi"}, _) -> ereal Fval.pi
  | TLval (TVar {lv_name = "\\e"}, _)  -> ereal Fval.e
  | TLval (TVar {lv_name = "\\plus_infinity"}, _) ->
    efloat Fval.(pos_infinity Single)
  | TLval (TVar {lv_name = "\\minus_infinity"}, _) ->
    efloat Fval.(neg_infinity Single)
  | TLval (TVar {lv_name = "\\NaN"}, _) -> efloat Fval.nan

  (* Mathematical logic variable: uses the [logic_vars] environment. *)
  | TLval (TVar ({ lv_type = Linteger | Lreal } as logic_var), TNoOffset) ->
    let cvalue = LogicVarEnv.find logic_var env.logic_vars in
    if logic_var.lv_type = Linteger
    then einteger cvalue
    else inject_no_deps Cil_const.doubleType cvalue

  | TLval tlval ->
    let lval = eval_tlval ~alarm_mode env tlval in
    let typ = lval.etype in
    let size = Eval_typ.sizeof_lval_typ typ in
    let state = env_current_state env in
    let eover_loc = Locations.make (lval.eover) size in
    let eover = find_or_alarm ~alarm_mode state eover_loc in
    let eover = Cvalue_forward.make_volatile ~typ eover in
    let eover = Cvalue_forward.reinterpret typ eover in
    (* Skip dependencies if state is dead *)
    let deps =
      if Cvalue.Model.is_reachable state then
        add_deps env.e_cur empty_logic_deps
          (Locations.enumerate_valid_bits Read eover_loc)
      else empty_logic_deps
    in
    let eunder_loc = Locations.make (lval.eunder) size in
    let eunder =
      match Eval_op.find_under_approximation state eunder_loc with
      | Some eunder ->
        Cvalue_forward.reinterpret typ (V_Or_Uninitialized.get_v eunder)
      | None -> under_from_over eover
    in
    { etype = typ;
      ldeps = join_logic_deps deps (lval.ldeps);
      eunder; eover; empty = lval.empty; }

  (* TBinOp ((LOr | LAnd), _t1, _t2) -> TODO: a special case would be useful.
     But this requires reducing the state after having evaluated t1 by
     a term that is in fact a predicate *)
  | TBinOp (op,t1,t2) -> eval_binop ~alarm_mode env op t1 t2

  | TUnOp (op, t) ->
    let r = eval_term ~alarm_mode env t in
    let typ' = match op with
      | Neg -> r.etype
      | BNot -> r.etype (* can only be used on an integer type *)
      | LNot -> Cil_const.intType
    in
    let op = Eva_ast.translate_unop op in
    let v = Cvalue_forward.forward_unop r.etype op r.eover in
    let eover = v in
    { etype = typ';
      ldeps = r.ldeps;
      eover; eunder = under_from_over eover; empty = r.empty; }

  | Trange (opt_low, opt_high) ->
    (* The overapproximation is the range [min(low.eover)..max(high.eover)].
       The underapproximation is the range [max(low.eover)..min(high.eover)].
       Perhaps surprisingly, we do not use the under-approximations of
       otlow and othigh to compute the underapproximation. We could
       potentially compute [min(max(low.over),  min(low.under) ..
                            max(min(high.over), max(high.under)]
       However, tsets cannot be used as bounds of ranges. By invariant (2),
       eunder is either Bottom, or equal to eover, both being of cardinal
       one. In both cases, using eover is more precise. *)
    let deps = ref empty_logic_deps in
    (* Evaluates the minimum and maximum integer value of an optional term.
       According to the Ival convention, None stands for -∞ and ∞ for the
       minimum and maximum respectively. *)
    let min_max = function
      | None -> None, None
      | Some term ->
        try
          let result = eval_term ~alarm_mode env term in
          deps := join_logic_deps !deps result.ldeps;
          let ival = Cvalue.V.project_ival result.eover in
          Ival.min_int ival, Ival.max_int ival
        with
        | Cvalue.V.Not_based_on_null -> None, None
        | LogicEvalError e ->
          if e <> CAlarm then
            Self.warning ~wkey:Self.wkey_acsl_unsupported
              ~source:(fst t.term_loc) ~once:true
              "Cannot evaluate range bound %a (%a). Approximating."
              Printer.pp_term term pretty_logic_evaluation_error e;
          None, None
    in
    let min_over, min_under = min_max opt_low
    and max_under, max_over = min_max opt_high in
    let eover = Cvalue.V.inject_ival (Ival.inject_range min_over max_over) in
    (* Beware: [None] stands for positive infinity for [min_under] and for
       negative infiny for [max_under] (in which case eunder must be bottom),
       except when the bound was itself [None]. *)
    let eunder =
      if (opt_low <> None && min_under = None)
      || (opt_high <> None && max_under = None)
      then Cvalue.V.bottom
      else Cvalue.V.inject_ival (Ival.inject_range min_under max_under)
    in
    let empty = Cvalue.V.is_bottom eunder in
    { ldeps = !deps;
      etype = Cil_const.intType;
      eunder; eover; empty; }

  | TCast (false, Ctype typ, t) ->
    let r = eval_term ~alarm_mode env t in
    (* See if the cast does something. If not, we can keep eunder as is.*)
    if is_noop_cast ~src_typ:t.term_type ~dst_typ:typ
    then { r with etype = typ }
    else if Ast_types.is_bool typ
    then cast_to_bool r
    else
      let eover = cast ~src_typ:r.etype ~dst_typ:typ r.eover in
      { etype = typ; ldeps = r.ldeps; eunder = under_from_over eover; eover;
        empty = r.empty; }
  | TCast (false, _,_) -> assert false

  | TCast (true, ltyp, t) ->
    let r = eval_term ~alarm_mode env t in
    (* we must handle coercion from singleton to set, for which there is
       nothing to do, AND coercion from an integer type to a floating-point
       type, that require a conversion. *)
    (match Logic_const.plain_or_set Fun.id ltyp with
     | Linteger when Logic_utils.is_integral_type t.term_type
                  || Logic_const.is_boolean_type t.term_type -> r
     | Ctype typ when Ast_types.is_integral_or_pointer typ -> r
     | Lreal ->
       let eover =
         if Logic_utils.is_integral_type t.term_type
         then V.cast_int_to_float Fval.Real r.eover
         else V.cast_float_to_float Fval.Real r.eover
       in
       { etype = Cil_const.longDoubleType; (* hack until logic type *)
         ldeps = r.ldeps;
         eover; eunder = under_from_over eover;
         empty = r.empty }
     | ltyp ->
       if Logic_const.is_boolean_type ltyp
       && Logic_utils.is_integral_type t.term_type
       then cast_to_bool r
       else if Logic_utils.is_same_type ltyp t.term_type then
         (* coercion from singleton to set *)
         r
       else
         unsupported
           (Format.asprintf "logic coercion %a -> %a@."
              Printer.pp_logic_type t.term_type Printer.pp_logic_type ltyp)
    )

  | Tif (pcond, ttrue, tfalse) ->
    eval_tif eval_term Cvalue.V.join Cvalue.V.meet ~alarm_mode env
      pcond ttrue tfalse

  | TSizeOf _ | TSizeOfE _ | TAlignOf _ | TAlignOfE _ ->
    let e = Cil.constFoldTerm t in
    let v = match e.term_node with
      | TConst (Integer (v, _)) -> Cvalue.V.inject_int v
      | _ -> V.top_int
    in
    einteger v

  | Tunion l ->
    let eunder, eover, deps, empty = List.fold_left
        (fun (accunder, accover, accdeps, accempty) t ->
           let r = eval_term ~alarm_mode env t in
           (Cvalue.V.link accunder r.eunder,
            Cvalue.V.join accover r.eover,
            join_logic_deps accdeps r.ldeps,
            accempty || r.empty))
        (Cvalue.V.bottom, Cvalue.V.bottom, empty_logic_deps, false) l
    in
    { etype = infer_type t.term_type;
      ldeps = deps; eunder; eover; empty; }

  | Tempty_set ->
    { etype = infer_type t.term_type;
      ldeps = empty_logic_deps;
      eunder = Cvalue.V.bottom;
      eover = Cvalue.V.bottom;
      empty = true; }

  | Tnull ->
    { etype = Cil_const.voidPtrType;
      ldeps = empty_logic_deps;
      eunder = Cvalue.V.singleton_zero;
      eover = Cvalue.V.singleton_zero;
      empty = false; }

  (* TODO: the meaning of the label in \offset and \base_addr is not obvious
     at all *)
  | Toffset (_lbl, t) ->
    let r = eval_term ~alarm_mode env t in
    let add_offset _ offs acc = Ival.join offs acc in
    let offs =
      try Addresses.Bytes.fold_topset_ok add_offset r.eover Ival.bottom
      with Abstract_interp.Error_Top -> Ival.top
    in
    let eover = Cvalue.V.inject_ival offs in
    { etype = Cil_const.intType;
      ldeps = r.ldeps;
      eover;
      eunder = under_from_over eover;
      empty = r.empty; }

  | Tbase_addr (_lbl, t) ->
    let r = eval_term ~alarm_mode env t in
    let add_base b acc = V.join acc (V.inject b Ival.zero) in
    let eover =
      try Addresses.Bytes.fold_bases add_base r.eover V.bottom
      with Abstract_interp.Error_Top -> r.eover
    in
    { etype = Cil_const.charPtrType;
      ldeps = r.ldeps;
      eover;
      eunder = under_from_over eover;
      empty = r.empty; }

  | Tblock_length (_lbl, t) -> (* TODO: take label into account for locals *)
    let r = eval_term ~alarm_mode env t in
    let add_block_length b acc =
      let bl =
        (* Convert the validity frontiers into a range of bytes. The
           frontiers are always 0 or 8*k-1 (because validity is in bits and
           starts on zero), so we add 1 everywhere, then divide by eight. *)
        let convert start_bits end_bits =
          let congr_succ i = Z.(equal zero (erem (succ i) (of_int 8))) in
          let congr_or_zero i = Z.(equal zero i || congr_succ i) in
          assert (congr_or_zero start_bits || congr_or_zero end_bits);
          let start_bytes = Z.(ediv (Z.succ start_bits) (of_int 8)) in
          let end_bytes =   Z.(ediv (Z.succ end_bits)   (of_int 8)) in
          Ival.inject_range (Some start_bytes) (Some end_bytes)
        in
        match Base.validity b with
        | Base.Empty -> Ival.zero
        | Base.Invalid -> Ival.top (* we may also emit an alarm *)
        | Base.Known (_, ma) -> convert ma ma
        | Base.Unknown (mi, None, ma) -> convert mi ma
        | Base.Unknown (_, Some mi, ma) -> convert mi ma
        | Base.Variable weak_v ->
          convert weak_v.Base.min_alloc weak_v.Base.max_alloc
      in
      Ival.join acc bl
    in
    let bl =
      try Addresses.Bytes.fold_bases add_block_length r.eover Ival.bottom
      with Abstract_interp.Error_Top -> Ival.top
    in
    let eover = V.inject_ival bl in
    { etype = Cil_const.charPtrType;
      ldeps = r.ldeps;
      eover;
      eunder = under_from_over eover;
      empty = r.empty; }

  | Tapp (li, labels, args) -> begin
      if is_known_logic_fun li.l_var_info then
        eval_known_logic_function ~alarm_mode env li labels args
      else
        match Inline.inline_term ~inline ~current:env.e_cur t with
        | Some t' -> eval_term ~alarm_mode env t'
        | None ->
          let s =
            Format.asprintf "logic function %a"
              Printer.pp_logic_var li.l_var_info
          in
          unsupported s
    end

  | TDataCons (ctor_info, _) ->
    begin
      match ctor_info.ctor_name with
      | "\\Positive" -> einteger positive_cvalue
      | "\\Negative" -> einteger negative_cvalue
      | _ -> unsupported "logic inductive types"
    end

  | Tcomprehension (term, quantifiers, predicate) ->
    let env = bind_comprehension_quantifiers env quantifiers predicate in
    let r = eval_term ~alarm_mode env term in
    let pred_deps = Option.bind (predicate_deps env) predicate in
    let ldeps =
      Option.fold ~none:r.ldeps ~some:(join_logic_deps r.ldeps) pred_deps
    in
    { r with empty = true; ldeps }

  | Tlet (li, t') ->
    let env = eval_let_binding ~alarm_mode env li in
    eval_term ~alarm_mode env t'

  | Tlambda _ -> unsupported "logic functions or predicates"
  | TUpdate _ -> unsupported "functional updates"
  | Ttype _ -> unsupported "\\type operator"
  | Ttypeof _ -> unsupported "\\typeof operator"
  | Tinter _ -> unsupported "set intersection"
  | TConst (LStr _) -> unsupported "constant strings"
  | TConst (LWStr _) -> unsupported "wide constant strings"

and eval_binop ~alarm_mode env op t1 t2 =
  if not (isLogicNonCompositeType t1.term_type) then
    if Self.debug_atleast 1 then
      unsupported (Format.asprintf
                     "operation (%a) %a (%a) on non-supported type %a"
                     Printer.pp_term t1
                     Printer.pp_binop op
                     Printer.pp_term t2
                     Printer.pp_logic_type t1.term_type)
    else
      unsupported (Format.asprintf
                     "%a operation on non-supported type %a"
                     Printer.pp_binop op
                     Printer.pp_logic_type t1.term_type)
  else
    let r1 = eval_term ~alarm_mode env t1 in
    let r2 = eval_term ~alarm_mode env t2 in
    let te1 = Ast_types.unroll r1.etype in
    check_logic_alarms ~alarm_mode te1 r1 op r2;
    let typ_res = infer_binop_res_type op te1 in
    let op = Eva_ast.translate_binop op in
    let eover = forward_binop te1 r1.eover op r2.eover in
    let default _r1 _r2 = under_from_over eover in
    let add_untyped_op factor =
      int_or_float_op te1 (V.add_untyped_under ~factor) default
    in
    let eunder_op = match op with
      | PlusPI -> begin
          match Bit_utils.osizeof_pointed te1 with
          | `Top -> fun _ _ -> V.bottom
          | `Value size -> add_untyped_op size
        end
      | PlusA -> add_untyped_op (Z.one)
      | MinusA -> add_untyped_op (Z.minus_one)
      | _ -> fun _ _ -> under_from_over eover
    in
    let eunder = eunder_op r1.eunder r2.eunder in
    { etype = typ_res;
      ldeps = join_logic_deps r1.ldeps r2.ldeps;
      eunder; eover; empty = r1.empty || r2.empty; }

and eval_tif : 'a. (alarm_mode:_ -> _ -> _ -> 'a eval_result) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> alarm_mode:_ -> _ -> _ -> _ -> _ -> 'a eval_result =
  fun eval join meet ~alarm_mode env pcond ttrue tfalse ->
  match eval_predicate env pcond with
  | Unknown ->
    let vtrue = eval ~alarm_mode env ttrue in
    let vfalse = eval ~alarm_mode env tfalse in
    if not (same_etype vtrue.etype vfalse.etype) then
      Self.failure ~current:true
        "Incoherent types in conditional: %a vs. %a. \
         Please report"
        Printer.pp_typ vtrue.etype Printer.pp_typ vfalse.etype;
    let eover = join vtrue.eover vfalse.eover in
    let eunder = meet vtrue.eunder vfalse.eunder in
    { etype = vtrue.etype;
      ldeps = join_logic_deps vtrue.ldeps vfalse.ldeps;
      eunder; eover; empty = vtrue.empty || vfalse.empty; }
  | True -> eval ~alarm_mode env ttrue
  | False -> eval ~alarm_mode env tfalse

(* if you add something here, update known_logic_funs above also *)
and eval_known_logic_function ~alarm_mode env li labels args =
  let lvi = li.l_var_info in
  match lvi.lv_name, li.l_type, labels, args with
  (* strlen comes in two flavors: one that takes an array, hence no labels,
     and one that takes a char pointer and a label. Only the first one
     can be fed with logic string literals as arguments. *)
  | ("strlen" | "wcslen") as b,  _, [], [arg] ->
    begin
      match arg.term_node with
      | TConst (LStr str) | TCast(_,_,{term_node = TConst(LStr str)})->
        (* Look for the first occurrence of the '\0' character, otherwise
           return the string length. *)
        let length =
          try String.index str '\x00'
          with Not_found -> String.length str
        in
        einteger (Cvalue.V.inject_int (Z.of_int length))
      | _ ->
        let r = eval_term ~alarm_mode env arg in
        let builtin =
          if b = "strlen" then Builtins_string.frama_c_strlen_wrapper
          else Builtins_string.frama_c_wcslen_wrapper
        in
        eval_logic_charlen builtin env r.eover r.ldeps
    end
  | ("strlen" | "wcslen") as b,  _, [lbl], [arg] ->
    let r = eval_term ~alarm_mode env arg in
    let builtin =
      if b = "strlen" then Builtins_string.frama_c_strlen_wrapper
      else Builtins_string.frama_c_wcslen_wrapper
    in
    eval_logic_charlen builtin { env with e_cur = lbl } r.eover r.ldeps
  | ("memchr_off" | "wmemchr_off") as b,  _, [lbl], [arg_s; arg_c; arg_n] ->
    let s = eval_term ~alarm_mode env arg_s in
    let c = eval_term ~alarm_mode env arg_c in
    let n = eval_term ~alarm_mode env arg_n in
    let builtin =
      if b = "memchr_off"
      then Builtins_string.frama_c_memchr_off_wrapper
      else Builtins_string.frama_c_wmemchr_off_wrapper
    in
    eval_logic_memchr_off builtin { env with e_cur = lbl } s c n

  | ("strchr" | "wcschr") as b,  _, [lbl], [arg_s; arg_c] ->
    let s = eval_term ~alarm_mode env arg_s in
    let c = eval_term ~alarm_mode env arg_c in
    let builtin =
      if b = "strchr" then Builtins_string.frama_c_strchr_wrapper
      else Builtins_string.frama_c_wcschr_wrapper
    in
    eval_logic_charchr builtin
      { env with e_cur = lbl } s.eover c.eover s.ldeps c.ldeps

  | ( "atan2" | "atan2f" | "fmod" | "fmodf" | "pow" | "powf"
    | "\\add_float" | "\\sub_float" | "\\mul_float" | "\\div_float"
    | "\\add_float32" | "\\sub_float32" | "\\mul_float32" | "\\div_float32"
    | "\\add_float64" | "\\sub_float64" | "\\mul_float64" | "\\div_float64"
    | "\\add_double" | "\\sub_double" | "\\mul_double" | "\\div_double" ),
    _, _, [arg1; arg2] ->
    eval_float_builtin_arity2 ~alarm_mode env lvi.lv_name arg1 arg2

  | ( "sqrt" | "sqrtf" | "\\neg_float" | "\\neg_double" ),_,_, [arg] ->
    eval_float_builtin_arity1 ~alarm_mode env lvi.lv_name arg

  | "\\sign", _, _, [arg] ->
    begin
      let r = eval_term ~alarm_mode env arg in
      try
        let fval = Cvalue.V.project_float r.eover in
        let sign = match Fval.is_negative fval with
          | True -> negative_cvalue
          | False -> positive_cvalue
          | Unknown -> Cvalue.V.join negative_cvalue positive_cvalue
        in
        { (einteger sign) with ldeps = r.ldeps }
      with Cvalue.V.Not_based_on_null -> c_alarm ()
    end

  | ("\\min" | "\\max"), Some typ, _, t1 :: t2 :: tail_args ->
    begin
      let min = lvi.lv_name = "\\min" in
      let comp = Abstract_interp.Comp.(if min then Le else Ge) in
      let backward =
        match typ with
        | Linteger -> Cvalue.V.backward_comp_int_left comp
        | Lreal -> Cvalue.V.backward_comp_float_left_true comp Fval.Real
        | _ -> assert false
      in
      let r1 = eval_term ~alarm_mode env t1
      and r2 = eval_term ~alarm_mode env t2 in
      (* If there are 2 arguments, it is the mathematical function \min or \max.
         If there are 3, it is the ACSL extended quantifiers \min or \max. *)
      match tail_args with
      | [] -> compute_extremum backward r1 r2
      | [ { term_node = Tlambda ([lv], term) } ] ->
        (* Evaluation of [term] when the logic variable [lv] is in [cvalue]. *)
        let eval_term cvalue =
          let env = add_logic_var env lv cvalue in
          eval_term ~alarm_mode env term
        in
        eval_quantifier_extremum backward ~min:r1 ~max:r2 eval_term
      | _ -> assert false
    end

  | "\\abs", Some typ, _, [t] ->
    begin
      let r = eval_term ~alarm_mode env t in
      try
        let ival = Cvalue.V.project_ival r.eover in
        let result =
          match typ with
          | Linteger -> einteger (Cvalue.V.inject_ival (Ival.abs_int ival))
          | Lreal -> ereal Fval.(abs Real (Ival.project_float ival))
          | _ -> assert false
        in
        { result with empty = r.empty; ldeps = r.ldeps; }
      with Cvalue.V.Not_based_on_null -> c_alarm ()
    end

  (* Not supported by cvalue, evaluated by the taint domain. *)
  | "security_status", _, _, [_t] -> einteger Cvalue.V.top_int

  | _ -> assert false

and eval_float_builtin_arity2  ~alarm_mode env name arg1 arg2 =
  let fcaml = match name with
    | "atan2" ->  Fval.atan2 Fval.Double
    | "atan2f" -> Fval.atan2 Fval.Single
    | "fmod" ->   Fval.fmod  Fval.Double
    | "fmodf" ->  Fval.fmod  Fval.Single
    | "pow" ->    Fval.pow   Fval.Double
    | "powf" ->   Fval.pow   Fval.Single
    | "\\add_float"   | "\\add_float32" -> Fval.add Fval.Single
    | "\\sub_float"   | "\\sub_float32" -> Fval.sub Fval.Single
    | "\\mul_float"   | "\\mul_float32" -> Fval.mul Fval.Single
    | "\\div_float"   | "\\div_float32" -> Fval.div Fval.Single
    | "\\add_float64" | "\\add_double"  -> Fval.add Fval.Double
    | "\\sub_float64" | "\\sub_double"  -> Fval.sub Fval.Double
    | "\\mul_float64" | "\\mul_double"  -> Fval.mul Fval.Double
    | "\\div_float64" | "\\div_double"  -> Fval.div Fval.Double
    | _ -> assert false
  in
  let r1 = eval_term ~alarm_mode env arg1 in
  let r2 = eval_term ~alarm_mode env arg2 in
  let v =
    try
      let i1 = Cvalue.V.project_ival r1.eover in
      let f1 = Ival.project_float i1 in
      let i2 = Cvalue.V.project_ival r2.eover in
      let f2 = Ival.project_float i2 in
      Cvalue.V.inject_float (fcaml f1 f2)
    with Cvalue.V.Not_based_on_null ->
      Cvalue.V.topify Origin.Arith (V.join r1.eover r2.eover)
  in
  let eunder = under_from_over v in
  let ldeps = join_logic_deps r1.ldeps r2.ldeps in
  { etype = r1.etype; eunder; eover = v; ldeps; empty = r1.empty || r2.empty; }

and eval_float_builtin_arity1  ~alarm_mode env name arg =
  let fcaml = match name with
    | "sqrt" ->   Fval.sqrt  Fval.Double
    | "sqrtf" ->  Fval.sqrt  Fval.Single
    | "\\neg_float" | "\\neg_float32" | "\\neg_float64" | "\\neg_double" ->
      Fval.neg
    | _ -> assert false
  in
  let r = eval_term ~alarm_mode env arg in
  let v =
    try
      let i = Cvalue.V.project_ival r.eover in
      let f = Ival.project_float i in
      Cvalue.V.inject_float (fcaml f)
    with Cvalue.V.Not_based_on_null ->
      Cvalue.V.topify Origin.Arith r.eover
  in
  let eunder = under_from_over v in
  { etype = r.etype; eunder; eover = v; ldeps=r.ldeps; empty = r.empty; }

(* Computes the max (resp. the min) between the evaluation results [r1] and [r2]
   according to [backward_left v1 v2] that reduces [v1] by assuming it is
   greater than (resp. lower than) [v2]. *)
and compute_extremum backward_left r1 r2 =
  let r1 = { r1 with eover = backward_left r1.eover r2.eover }
  and r2 = { r2 with eover = backward_left r2.eover r1.eover } in
  join_eval_result r1 r2

(* Evaluates ACSL extended quantifiers \max or \min: computes (an approximation
   of) the max (resp. the min) of [eval_term i] when [i] ranges between [min]
   and [max], according to [backward_left v1 v2] that reduces [v1] by assuming
   it is greater (resp. lower) than [v2]. *)
and eval_quantifier_extremum backward_left ~min ~max eval_term =
  (* Returns [r] as the result, with updated [empty] and [ldeps] fields. *)
  let return r =
    let ldeps = join_logic_deps r.ldeps (join_logic_deps min.ldeps max.ldeps) in
    let empty = min.empty || max.empty || r.empty in
    { r with ldeps; empty; }
  in
  let eval_ival ival = eval_term (Cvalue.V.inject_ival ival) in
  let project r = Cvalue.V.project_ival r.eover in
  match Ival.min_and_max (project min),
        Ival.min_and_max (project max) with
  | (min, Some b), (Some e, max) when Z.leq b e ->
    (* All integers between [b] and [e] are necessarily in the range to be
       considered. If [e-b] is small enough, evaluate [eval_term i] for each [i]
       between [b] and [e]. Otherwise, evaluate [eval_term] for the bound [b]
       and [e], and for the interval [b+1..e-1]. We could be more precise by
       subdividing the interval. *)
    let r =
      if Z.equal e b
      then eval_term (Cvalue.V.inject_int b)
      else
        let fold =
          if Z.(leq (sub e b) (of_int 10))
          then Ival.fold_enum
          else Ival.fold_int_bounds
        in
        let ival = Ival.inject_range (Some b) (Some e) in
        let list = fold (fun i acc -> eval_ival i :: acc) ival [] in
        (* Reduce each result to only keep the possible extrema. *)
        let hd, tl = List.hd list, List.tl list in
        List.fold_left (compute_extremum backward_left) hd tl
    in
    (* Integers below [b] and above [e] may not be in the range to be considered,
       so we can't reduce [r] according to them. However, we must join to [r]
       the evaluation of [eval_term] for these integers, and we can reduce
       these evaluations to only keep results greater (or lower, according to
       [backward_left]) than [r]. *)
    let below = eval_ival (Ival.inject_range min (Some (Z.pred b)))
    and above = eval_ival (Ival.inject_range (Some (Z.succ e)) max) in
    let below = { below with eover = backward_left below.eover r.eover }
    and above = { above with eover = backward_left above.eover r.eover } in
    join_eval_result r (join_eval_result below above)
  | (b, _), (_, e) ->
    (* If [min] can be greater than [max], the result is unspecified. *)
    let r = eval_ival (Ival.inject_range b e) in
    return { r with eover = Cvalue.V.top; eunder = Cvalue.V.bottom }
  | exception Cvalue.V.Not_based_on_null ->
    let r = eval_term (Cvalue.V.join min.eover max.eover) in
    return { r with eover = Cvalue.V.top; eunder = Cvalue.V.bottom }

(* Introduces the logic variables [quantifiers] in [env], and reduces their
   value according to [predicate]. *)
and bind_comprehension_quantifiers env quantifiers predicate =
  let env = bind_logic_vars env quantifiers in
  match predicate with
  | None -> env
  | Some pred ->
    try
      let alarm_mode = Fail in
      let reduced_env = reduce_by_predicate ~alarm_mode env true pred in
      copy_logic_vars ~src:reduced_env ~dst:env quantifiers
    with LogicEvalError error ->
      display_evaluation_error ~loc:pred.pred_loc error;
      env

and eval_let_binding ~alarm_mode env logic_info =
  match logic_info with
  | { l_labels = []; l_tparams = []; l_profile = []; l_body = LBterm term } ->
    begin
      let var = logic_info.l_var_info in
      let env = bind_logic_vars env [var] in
      let var_term = Logic_const.tvar var in
      try reduce_left_by_relation ~alarm_mode env true var_term Req term
      with Reduce_to_bottom ->
        Self.warning ~current:true ~once:true
          "ignoring \\let binding of logic variable %a evaluating to bottom"
          Printer.pp_logic_var var;
        env
    end
  | _ -> unsupported "\\let binding"

(* -------------------------------------------------------------------------- *)
(* --- Evaluation of term lval and of terms as location                   --- *)
(* -------------------------------------------------------------------------- *)

and eval_tlhost ~alarm_mode env lv =
  match lv with
  | TVar lvar ->
    let base, typ =
      match lvar.lv_origin, Ast_types.unroll_logic lvar.lv_type with
      | Some v, _ -> Base.of_varinfo v, v.vtype
      | None, Ctype typ -> Base.of_c_logic_var lvar, typ
      | _ -> unsupported_lvar lvar
    in
    let addr = Addresses.Bits.inject base Ival.zero in
    { etype = typ;
      ldeps = empty_logic_deps;
      eover = addr;
      eunder = under_loc_from_over addr;
      empty = false; }
  | TResult typ ->
    (match env.result with
     | Some v ->
       let addr = Addresses.Bits.inject (Base.of_varinfo v) Ival.zero in
       { etype = typ;
         ldeps = empty_logic_deps;
         eunder = addr; eover = addr;
         empty = false; }
     | None -> no_result ())
  | TMem t ->
    let r = eval_term ~alarm_mode env t in
    let tres = match Ast_types.unroll_node r.etype with
      | TPtr t -> t
      | _ -> ast_error "*p where p is not a pointer"
    in
    { etype = tres;
      ldeps = r.ldeps;
      eunder = Addresses.Bits.of_bytes r.eunder;
      eover = Addresses.Bits.of_bytes r.eover;
      empty = r.empty; }

and eval_toffset ~alarm_mode env typ toffset =
  match toffset with
  | TNoOffset ->
    { etype = typ;
      ldeps = empty_logic_deps;
      eunder = Ival.zero;
      eover = Ival.zero;
      empty = false; }
  | TIndex (idx, remaining) ->
    let typ_e, size = match Ast_types.unroll_node typ with
      | TArray (t, size) -> t, size
      | _ -> ast_error "index on a non-array"
    in
    let idx = constraint_trange idx size in
    let idxs = eval_term ~alarm_mode env idx in
    let offsrem = eval_toffset ~alarm_mode env typ_e remaining in
    let size_e = Bit_utils.sizeof typ_e in
    let eover =
      let offset =
        try Cvalue.V.project_ival_bottom idxs.eover
        with Cvalue.V.Not_based_on_null -> Ival.top
      in
      let offset = Ival.scale_or_top size_e offset in
      Ival.add_int offset offsrem.eover
    in
    let eunder =
      let offset =
        try Cvalue.V.project_ival idxs.eunder
        with Cvalue.V.Not_based_on_null -> Ival.bottom
      in
      let offset = match size_e with
        | `Top -> Ival.bottom
        | `Value f ->
          (* [Ival.scale] is exact (and so can be used for under-approximation),
             except on floating-point values, which should never happen. *)
          assert (Ival.is_int offset);
          Ival.scale f offset
      in
      Ival.add_int_under offset offsrem.eunder
    in
    { etype = offsrem.etype;
      ldeps = join_logic_deps idxs.ldeps offsrem.ldeps;
      eunder; eover; empty = idxs.empty || offsrem.empty; }

  | TField (fi, remaining) ->
    let size_current default =
      try Ival.of_int (fst (Cil.fieldBitsOffset fi))
      with Cil.SizeOfError _ -> default
    in
    let attrs = Ast_types.get_attributes typ in
    let attrs = Ast_attributes.filter_qualifiers attrs in
    let typ_fi = Ast_types.add_attributes attrs fi.ftype in
    let offsrem = eval_toffset ~alarm_mode env typ_fi remaining in
    { etype = offsrem.etype;
      ldeps = offsrem.ldeps;
      eover = Ival.add_int (size_current Ival.top) offsrem.eover;
      eunder = Ival.add_int_under (size_current Ival.bottom) offsrem.eunder;
      empty = offsrem.empty; }

  | TModel _ -> unsupported "model fields"

and eval_tlval ~alarm_mode env (thost, toffs) =
  let rhost = eval_tlhost ~alarm_mode env thost in
  let roffset = eval_toffset ~alarm_mode env rhost.etype toffs in
  { etype = roffset.etype;
    ldeps = join_logic_deps rhost.ldeps roffset.ldeps;
    eunder = Addresses.Bits.shift_under roffset.eunder rhost.eunder;
    eover = Addresses.Bits.shift roffset.eover rhost.eover;
    empty = rhost.empty || roffset.empty;
  }

and eval_term_as_lval ~alarm_mode env t =
  match t.term_node with
  | TLval tlval ->
    eval_tlval ~alarm_mode env tlval
  | Tunion l ->
    let eunder, eover, deps, empty = List.fold_left
        (fun (accunder, accover, accdeps, accempty) t ->
           let r = eval_term_as_lval ~alarm_mode env t in
           Addresses.Bits.link accunder r.eunder,
           Addresses.Bits.join accover r.eover,
           join_logic_deps accdeps r.ldeps,
           accempty || r.empty
        ) (Addresses.Bits.top, Addresses.Bits.bottom, empty_logic_deps, false) l
    in
    { etype = infer_type t.term_type;
      ldeps = deps;
      eover; eunder; empty }
  | Tempty_set ->
    { etype = infer_type t.term_type;
      ldeps = empty_logic_deps;
      eunder = Addresses.Bits.bottom;
      eover = Addresses.Bits.bottom;
      empty = true }
  | Tat (t, lab) ->
    ignore (env_state env lab);
    eval_term_as_lval ~alarm_mode { env with e_cur = lab } t
  | TCast (true, _lt, t) ->
    (* Logic coerce on locations (that are pointers) can only introduce
       sets, that do not change the abstract value. *)
    eval_term_as_lval ~alarm_mode env t
  | Tif (pcond, ttrue, tfalse) ->
    eval_tif eval_term_as_lval Addresses.Bits.join Addresses.Bits.meet ~alarm_mode env
      pcond ttrue tfalse
  | Tcomprehension (term, quantifiers, predicate) ->
    let env = bind_comprehension_quantifiers env quantifiers predicate in
    let r = eval_term_as_lval ~alarm_mode env term in
    let pred_deps = Option.bind (predicate_deps env) predicate in
    let ldeps =
      Option.fold ~none:r.ldeps ~some:(join_logic_deps r.ldeps) pred_deps
    in
    { r with empty = true; ldeps }
  | Tinter _ -> unsupported "intersection of locations"
  | _ -> ast_error (Format.asprintf "non-lval term %a" Printer.pp_term t)

(* Evaluate a term as a non-empty under-approximated location, or raise
   [Not_an_exact_loc]. *)
and eval_term_as_exact_locs ~alarm_mode env t =
  match t.term_node with
  | TLval (TVar ({ lv_type = Linteger | Lreal } as logic_var), TNoOffset) ->
    Logic_var logic_var

  | TLval tlval ->
    let loc = eval_tlval ~alarm_mode env tlval in
    let typ = loc.etype in
    (* eval_term_as_exact_loc is only used for reducing values, and we must
       NOT reduce volatile locations. *)
    if Ast_types.has_qualifier "volatile" typ then raise Not_an_exact_loc;
    let loc = Locations.make loc.eunder (Eval_typ.sizeof_lval_typ typ)in
    if Locations.is_bottom loc then raise Not_an_exact_loc;
    Location (typ, loc)

  | TCast (true, Lreal, t) -> begin
      match eval_term_as_exact_locs ~alarm_mode env t with
      | Logic_var _ as x -> x
      | Location (_, locs) as r ->
        (* Real is not a supertype of non-finite floats because of NaN and
           infinities, we do not want to go in the case below. Instead,
           we check that there are no NaN/infinity, so that the subtyping
           relation indeed holds. *)
        let aux loc () =
          let state = env_current_state env in
          let v = find_or_alarm ~alarm_mode state loc in
          let v = Cvalue_forward.reinterpret Cil_const.longDoubleType v in
          let is_finite =
            match V.project_float v with
            | exception Cvalue.V.Not_based_on_null -> Unknown
            | f -> Fval.is_finite f
          in
          match is_finite with
          | True -> ()
          | False | Unknown -> raise Not_an_exact_loc
        in
        Eval_op.apply_on_all_locs aux locs ();
        r
    end

  | TCast (true, _, t) ->
    (* Otherwise it is always ok to pass through a TCast (true,_,_), as the destination
       type is always a supertype *)
    eval_term_as_exact_locs ~alarm_mode env t

  | TCast (false, ctype, t') ->
    pass_logic_cast Not_an_exact_loc ctype t';
    eval_term_as_exact_locs ~alarm_mode env t'

  | Tunion [t] ->
    eval_term_as_exact_locs ~alarm_mode env t

  | Tcomprehension (term, quantifiers, predicate) ->
    let env = bind_comprehension_quantifiers env quantifiers predicate in
    eval_term_as_exact_locs ~alarm_mode env term

  | _ -> raise Not_an_exact_loc

(* -------------------------------------------------------------------------- *)
(* --- Reduction by predicates                                            --- *)
(* -------------------------------------------------------------------------- *)

(* Apply [reduce] to the value of location [arg] if it is an exact location. *)
and reduce_exact_location ~alarm_mode env reduce loc =
  match eval_term_as_exact_locs ~alarm_mode env loc with
  | Logic_var logic_var ->
    let cvalue = LogicVarEnv.find logic_var env.logic_vars in
    let cvalue = reduce logic_var.lv_type cvalue in
    if V.is_bottom cvalue then raise Reduce_to_bottom;
    add_logic_var env logic_var cvalue
  | Location (typ_loc, locs) ->
    let aux loc env =
      let state = env_current_state env in
      let v = find_or_alarm ~alarm_mode state loc in
      let v = Cvalue_forward.reinterpret typ_loc v in
      let v' = reduce (Ctype typ_loc) v in
      if V.is_bottom v' then raise Reduce_to_bottom;
      if V.equal v' v then env else
        let state' = Cvalue.Model.reduce_previous_binding state loc v' in
        overwrite_current_state env state'
    in
    Eval_op.apply_on_all_locs aux locs env
  | exception Not_an_exact_loc
  | exception LogicEvalError _ -> env

and reduce_by_valid env positive access (tset: term) =
  let exception DoNotReduce in
  (* Auxiliary function that reduces \valid(lv+offs), where lv is atomic
     (no more tsets), and offs is a bits-expressed constant offset.
     [offs_typ] is supposed to be the type of the pointed location after [offs]
     has been applied; it can be different from [typeOf_pointed lv], for
     example if offset is a field access. *)
  let aux lv env (offs_typ, offs) =
    try
      if not (Addresses.Bits.cardinal_zero_or_one lv.eover) ||
         not (Ival.cardinal_zero_or_one offs)
      then raise DoNotReduce;
      let state = env_current_state env in
      let lv_size = Eval_typ.sizeof_lval_typ lv.etype in
      let lvloc = Locations.make lv.eover lv_size in
      (* [p] is the range that we attempt to reduce *)
      let alarm_mode = alarm_reduce_mode () in
      let p_orig = find_or_alarm ~alarm_mode state lvloc in
      let pb = Addresses.Bits.of_bytes p_orig in
      let shifted_p = Addresses.Bits.shift offs pb in
      let offs_size = Eval_typ.sizeof_lval_typ offs_typ in
      let lshifted_p = Locations.make shifted_p offs_size in
      let valid = (* reduce the shifted pointer to the wanted part *)
        if positive
        then Locations.valid_part access lshifted_p
        else Locations.invalid_part lshifted_p
      in
      let valid = valid.addr in
      if Addresses.Bits.equal shifted_p valid
      then env
      else
        (* Shift back *)
        let shift = Ival.neg_int offs in
        let pb = Addresses.Bits.shift shift valid in
        let p = Addresses.Bits.to_bytes pb in
        (* Store the result *)
        let state = Model.reduce_previous_binding state lvloc p in
        overwrite_current_state env state
    with
    | DoNotReduce | V.Not_based_on_null | Cil.SizeOfError _ | LogicEvalError _
      -> env
  in
  (*  Auxiliary function to reduce by the under-approximation of an offset.
      Since validities are contiguous, we simply reduce by the minimum and
      maximum of the under-approximation. *)
  let aux_min_max_offset f env off =
    try
      let env = match Ival.min_int off with
        | None -> env
        | Some min -> f env (Ival.inject_singleton min)
      in
      match Ival.max_int off with
      | None -> env
      | Some max -> f env (Ival.inject_singleton max)
    with Abstract_interp.Error_Bottom -> env
  in
  (* reduce [loc] so that its contents are a valid pointer to [typ] *)
  let aux_one_lval typ loc env =
    try
      let state =
        Eval_op.reduce_by_valid_loc ~positive access
          loc typ (env_current_state env)
      in
      overwrite_current_state env state
    with LogicEvalError _ -> env
  in
  (* reduce [t], which must be valid term-lval, so that its contents are
     a valid pointer to [typ]. If [typ] is not supplied, it is inferred
     from the type of [t]. *)
  let aux_lval ?typ tlval env =
    try
      let alarm_mode = alarm_reduce_mode () in
      let r = eval_tlval ~alarm_mode env tlval in
      let typ = match typ with None -> r.etype | Some t -> t in
      let loc = Locations.make r.eunder (Eval_typ.sizeof_lval_typ typ) in
      let r = Eval_op.apply_on_all_locs (aux_one_lval typ) loc env in
      r
    with LogicEvalError _ -> env
  in
  let rec do_one env t =
    match t.term_node with
    | Tunion l ->
      List.fold_left do_one env l

    | TLval tlval -> aux_lval tlval env

    | TCast (false, Ctype typ, {term_node = TLval tlval}) -> aux_lval ~typ tlval env

    | TAddrOf (TMem {term_node = TLval tlval}, offs) ->
      (try
         let alarm_mode = alarm_reduce_mode () in
         let lt = eval_tlval ~alarm_mode env tlval in
         let typ = lt.etype in
         (* Compute the offsets, that depend on the type of the lval.
            The computed list is exactly what [aux] requires *)
         let roffs =
           eval_toffset ~alarm_mode env (Ast_types.direct_pointed_type typ) offs
         in
         let aux env offs = aux lt env (roffs.etype, offs) in
         aux_min_max_offset aux env roffs.eunder
       with LogicEvalError _ -> env)

    | TBinOp ((PlusPI | MinusPI) as op, {term_node = TLval tlval}, i) ->
      (try
         let alarm_mode = alarm_reduce_mode () in
         let rtlv = eval_tlval ~alarm_mode env tlval in
         let ri = eval_term ~alarm_mode env i in
         (* Convert offsets to a simpler form if [op] is [MinusPI] *)
         let li =
           try V.project_ival ri.eunder
           with V.Not_based_on_null -> raise Exit
         in
         let li = if op = PlusPI then li else Ival.neg_int li in
         let typ_p = Ast_types.direct_pointed_type rtlv.etype in
         let sbits = Z.of_int (Cil.bitsSizeOf typ_p) in
         (* Compute the offsets expected by [aux], which are [i *
            8 * sizeof( *tlv)] *)
         let li = Ival.scale sbits li in
         (* Now reduce [tlv] by values possible for [i] *)
         let aux env offs = aux rtlv env (typ_p, offs) in
         aux_min_max_offset aux env li
       with
       | LogicEvalError _ | Exit -> env
      )
    | _ -> env
  in
  do_one env tset

(* Reduces the possible value of [arg] by assuming it points to a valid string
   (or not if [positive] is false), for reading or writing according to [access].
   This reduces the possible value of [arg] to a valid pointer (thus only
   considering the first character of the string), and filters out bases that
   cannot be a valid string because strlen returns bottom.
   This reduction could be improved by also reducing offsets according to the
   position of \0 in the pointed strings. *)
and reduce_by_valid_string ~alarm_mode env positive ~wide ~access arg =
  (* First, reduces [arg] assuming it is a valid pointer. *)
  let env = reduce_by_valid env positive access arg in
  (* Reduce the cvalue [v]:
     - if [positive] holds, remove bases which cannot be a valid string
       as the proper strlen builtin returns bottom;
     - if [positive] is false, remove bases which are a valid string,
       as the proper strlen builtin returns no alarm. *)
  let reduce _typ v =
    let wrapper =
      if wide
      then Builtins_string.frama_c_wcslen_wrapper
      else Builtins_string.frama_c_strlen_wrapper
    in
    let aux base offset acc =
      let value = Cvalue.V.inject base offset in
      let v, alarms = apply_logic_builtin wrapper env [value] in
      (* Beware of not removing const strings on the negation of \valid_string. *)
      let alarms = alarms || (access = Write && Base.is_read_only base) in
      if (positive && Cvalue.V.is_bottom v)
      || (not positive && not alarms)
      then acc
      else Cvalue.V.add base offset acc
    in
    Cvalue.V.fold_i aux v Cvalue.V.bottom
  in
  reduce_exact_location ~alarm_mode env reduce arg

(* Reduces [tl] so that [\base_addr(tl) == \base_addr(tr)] holds. *)
and reduce_left_by_base_addr_eq ~alarm_mode env tl tr =
  let right = eval_term ~alarm_mode env tr in
  let right_bases = Cvalue.V.get_bases right.eover in
  (* Avoids reducing the null base. *)
  let filter base = Base.is_null base || Base.SetLattice.mem base right_bases in
  let reduce _typ = Cvalue.V.filter_base filter in
  reduce_exact_location ~alarm_mode env reduce tl

(* reduce [tl] so that [rl rel tr] holds *)
and reduce_left_by_relation ~alarm_mode env positive tl rel tr =
  let rtl = eval_term ~alarm_mode env tr in
  let comp = Eva_utils.conv_relation rel in
  let reduce typ cvalue =
    Eval_op.backward_comp_left_from_type typ positive comp cvalue rtl.eover
  in
  reduce_exact_location ~alarm_mode env reduce tl

and reduce_by_relation ~alarm_mode env positive t1 rel t2 =
  (* special case: t1 is a term of the form "a rel' b",
     and is compared to "== 0" or "!= 0" => evaluate t1 directly;
     note: such terms may be created by other evaluation/reduction functions
     e.g. eval_predicate, reduce_by_predicate_content *)
  match t1.term_node, rel with
  | TBinOp (bop, t1', t2'), Rneq when is_rel_binop bop && Cil.isLogicZero t2 ->
    reduce_by_relation ~alarm_mode env positive t1' (rel_of_binop bop) t2'
  | TBinOp (bop, t1', t2'), Req when is_rel_binop bop && Cil.isLogicZero t2 ->
    reduce_by_relation ~alarm_mode env (not positive) t1' (rel_of_binop bop) t2'
  | _ ->
    (* Special case for \base_addr. *)
    let reduce_left env t1 rel t2 =
      match t1.term_node with
      | Tbase_addr (_lbl, t1) ->
        if rel = Req && positive || rel = Rneq && not positive
        then reduce_left_by_base_addr_eq ~alarm_mode env t1 t2
        else env
      | _ -> reduce_left_by_relation ~alarm_mode env positive t1 rel t2
    in
    let env = reduce_left env t1 rel t2 in
    let sym_rel = match rel with
      | Rgt -> Rlt | Rlt -> Rgt | Rle -> Rge | Rge -> Rle
      | Req -> Req | Rneq -> Rneq
    in
    reduce_left env t2 sym_rel t1

(* if you add something here, update [known_predicates] above also
   (and of course [eval_known_papp] below).
   May raise LogicEvalError or Not_an_exact_loc, when no reduction can be done,
   and Reduce_to_bottom, in which case the reduction leads to bottom. *)
and reduce_by_known_papp ~alarm_mode env positive li _labels args =
  (* If the term [arg] is a floating-point lvalue with an exact location,
     reduces its value in [env] by using the backward propagator on fval
     [fval_reduce]. *)
  let reduce_float fval_reduce arg =
    let reduce typ cvalue =
      match logic_type_fkind typ with
      | Some fkind -> begin
          try
            let v = Cvalue.V.project_float cvalue in
            let kind = Fval.kind fkind in
            match fval_reduce kind v with
            | `Value f -> V.inject_float f
            | `Bottom -> V.bottom
          with Cvalue.V.Not_based_on_null -> cvalue
        end
      | None -> cvalue (* Better safe than sorry, we may have e.g. an int
                          location here *)
    in
    reduce_exact_location ~alarm_mode env reduce arg
  in
  (* Reduces [f] to positive or negative infinity (according to [pos]),
     or to the complement if [positive] is false. *)
  let reduce_by_infinity ~pos prec f =
    let inf = if pos then Fval.pos_infinity prec else Fval.neg_infinity prec in
    let fval =
      if positive
      then inf
      else Fval.(join nan (join (Fval.neg inf) (top_finite prec)))
    in
    Fval.narrow fval f
  in
  match li.l_var_info.lv_name, args with
  | "\\is_finite", [arg] ->
    reduce_float (Fval.backward_is_finite ~positive) arg
  | "\\is_infinite", [arg] ->
    reduce_float (Fval.backward_is_infinite ~positive) arg
  | "\\is_plus_infinity", [arg] ->
    reduce_float (reduce_by_infinity ~pos:true) arg
  | "\\is_minus_infinity", [arg] ->
    reduce_float (reduce_by_infinity ~pos:false) arg
  | "\\is_NaN", [arg] ->
    reduce_float (fun _fkind -> Fval.backward_is_nan ~positive) arg
  | ("\\eq_float" | "\\eq_float32" | "\\eq_float64" | "\\eq_double"), [t1;t2] ->
    reduce_by_relation ~alarm_mode env positive t1 Req t2
  | ("\\ne_float" | "\\ne_float32" | "\\ne_float64" | "\\ne_double"), [t1;t2] ->
    reduce_by_relation ~alarm_mode env positive t1 Rneq t2
  | ("\\lt_float" | "\\lt_float32" | "\\lt_float64" | "\\lt_double"), [t1;t2] ->
    reduce_by_relation ~alarm_mode env positive t1 Rlt t2
  | ("\\le_float" | "\\le_float32" | "\\le_float64" | "\\le_double"), [t1;t2] ->
    reduce_by_relation ~alarm_mode env positive t1 Rle t2
  | ("\\gt_float" | "\\gt_float32" | "\\gt_float64" | "\\gt_double"), [t1;t2] ->
    reduce_by_relation ~alarm_mode env positive t1 Rgt t2
  | ("\\ge_float" | "\\ge_float32" | "\\ge_float64" | "\\ge_double"), [t1;t2] ->
    reduce_by_relation ~alarm_mode env positive t1 Rge t2
  | "\\subset", [argl;argr] when positive ->
    let vr = (eval_term ~alarm_mode env argr).eover in
    let reduce _typ vl = Cvalue.V.narrow vl vr in
    reduce_exact_location ~alarm_mode env reduce argl
  | "valid_read_string", [arg] ->
    reduce_by_valid_string ~alarm_mode env positive ~wide:false ~access:Read arg
  | "valid_string", [arg] ->
    reduce_by_valid_string ~alarm_mode env positive ~wide:false ~access:Write arg
  | "valid_read_wstring", [arg] ->
    reduce_by_valid_string ~alarm_mode env positive ~wide:true ~access:Read arg
  | "valid_wstring", [arg] ->
    reduce_by_valid_string ~alarm_mode env positive ~wide:true ~access:Write arg

  | _ -> (* Do not fail here. We can be asked to reduce on predicates that we
            can evaluate, but on which we are not able to reduce on (yet ?).*)
    env

(** Big recursive functions for predicates *)

and reduce_by_predicate ~alarm_mode env positive p =
  let loc = p.pred_loc in
  let rec reduce_by_predicate_content env positive p_content =
    match positive,p_content with
    | true,Ptrue | false,Pfalse -> env

    | true,Pfalse | false,Ptrue ->
      overwrite_current_state env Cvalue.Model.bottom

    (* desugared form of a <= b <= c <= d *)
    | true, Pand (
        {pred_content=Pand (
             {pred_content=Prel ((Rlt | Rgt | Rle | Rge | Req as op),_ta,tb) as p1},
             {pred_content=Prel (op', tb',tc) as p2})},
        {pred_content=Prel (op'',tc',_td) as p3})
      when
        op = op' && op' = op'' &&
        is_same_term_coerce tb tb' &&
        is_same_term_coerce tc tc'
      ->
      let red env p = reduce_by_predicate_content env positive p in
      let env = red env p1 in
      let env = red env p3 in
      let env = red env p2 in
      (*Not really useful in practice*)
      (*let env = red env (Prel (op, ta, tc)) in
        let env = red env (Prel (op, tb, td)) in *)
      env

    | true,Pand (p1,p2) | false,Por(p1,p2)->
      let r1 = reduce_by_predicate ~alarm_mode env positive p1 in
      reduce_by_predicate ~alarm_mode r1 positive p2

    | true,Por (p1,p2 ) | false,Pand (p1, p2) ->
      let env1 = reduce_by_predicate ~alarm_mode env positive p1 in
      let env2 = reduce_by_predicate ~alarm_mode env positive p2 in
      join_env env1 env2

    | true,Pimplies (p1,p2) ->
      let env1 = reduce_by_predicate ~alarm_mode env false p1 in
      let env2 = reduce_by_predicate ~alarm_mode env true p2 in
      join_env env1 env2

    | false,Pimplies (p1,p2) ->
      reduce_by_predicate ~alarm_mode
        (reduce_by_predicate ~alarm_mode env true p1)
        false
        p2

    | _,Pnot p -> reduce_by_predicate ~alarm_mode env (not positive) p

    | true,Piff (p1, p2) ->
      let red1 = reduce_by_predicate_content env true (Pand (p1, p2)) in
      let red2 = reduce_by_predicate_content env false (Por (p1, p2)) in
      join_env red1 red2

    | false,Piff (p1, p2) ->
      reduce_by_predicate ~alarm_mode env true
        (Logic_const.por ~loc
           (Logic_const.pand ~loc (p1, Logic_const.pnot ~loc p2),
            Logic_const.pand ~loc (Logic_const.pnot ~loc p1, p2)))

    | _,Pxor(p1,p2) ->
      reduce_by_predicate ~alarm_mode env
        (not positive) (Logic_const.piff ~loc (p1, p2))

    | _,Prel (op,t1,t2) ->
      begin
        try
          (* ugly, but eval_predicate_content does not exist yet *)
          let p = Logic_const.unnamed ~loc p_content in
          let p' = if positive then p else Logic_const.pnot ~loc p in
          (* Evaluate the predicate before reducing. In some cases, although
             evaluation results in Bottom, reduction fails to reduce the
             resulting env to Bottom, and we lose precision. *)
          match eval_predicate env p' with
          | True -> env
          | False -> overwrite_current_state env Cvalue.Model.bottom
          | Unknown -> reduce_by_relation ~alarm_mode env positive t1 op t2
        with
        | LogicEvalError _ -> env
        | Reduce_to_bottom ->
          overwrite_current_state env Cvalue.Model.bottom
          (* if the exception was obtained without an alarm emitted,
             it is correct to return the bottom state *)
      end

    | _,Pvalid (_label,tsets) ->
      (* TODO: label should not be ignored. Instead, we should clear
         variables that are not in scope at the label. *)
      reduce_by_valid env positive Write tsets
    | _,Pvalid_read (_label,tsets) ->
      reduce_by_valid env positive Read tsets
    | _,Pobject_pointer (_label, tsets) ->
      reduce_by_valid env positive Object_pointer tsets

    | _,Pvalid_function _tsets -> env (* TODO *)

    | _, Paligned (t, n) -> begin
        let align = eval_term ~alarm_mode env n in
        let to_int v =
          try Cvalue.V.project_ival v |> Ival.project_int |> Z.to_int_opt
          with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int -> None
        in
        match to_int align.eover with
        | Some align when align > 0 ->
          begin
            let assume_aligned = Cvalue_forward.assume_aligned align in
            let reduce _typ = reduce_value_with_truth assume_aligned in
            try reduce_exact_location ~alarm_mode env reduce t
            with Reduce_to_bottom -> overwrite_current_state env Model.bottom
               | LogicEvalError _ -> env
          end
        | Some _ | None -> env
      end

    | _,(Pinitialized (lbl_initialized,tsets)
        | Pdangling (lbl_initialized,tsets)) ->
      begin
        try
          let alarm_mode = alarm_reduce_mode () in
          (* See comments in the code for the evaluation of Pinitialized *)
          let star_tsets = deref_tsets tsets in
          let rlocb = eval_tlval ~alarm_mode env star_tsets in
          (* No reduction on negations of \initialized or \dangling on multiple
             locations: at least one of them is non initialized/dangling, but
             which one? Reduction would only be possible in the rare case where
             only one of the locations might be non initialized/dangling. *)
          if not (positive || Addresses.Bits.cardinal_zero_or_one rlocb.eover)
          then env
          else
            let size = Eval_typ.sizeof_lval_typ rlocb.etype in
            let state = env_state env lbl_initialized in
            let fred = match p_content with
              | Pinitialized _ -> V_Or_Uninitialized.reduce_by_initializedness
              | Pdangling _ -> V_Or_Uninitialized.reduce_by_danglingness
              | _ -> assert false
            in
            let fred = Eval_op.reduce_by_initialized_defined (fred positive) in
            let state_reduced =
              let loc = Locations.make rlocb.eunder size in
              let loc = Eval_op.make_loc_contiguous loc in
              Eval_op.apply_on_all_locs fred loc state
            in
            overwrite_state env state_reduced lbl_initialized
        with LogicEvalError _ -> env
      end

    | _,Pat (p, lbl) ->
      (try
         let env_at = { env with e_cur = lbl } in
         let env' = reduce_by_predicate ~alarm_mode env_at positive p in
         { env' with e_cur = env.e_cur }
       with LogicEvalError _ -> env)

    | true, Pforall (varl, p) | false, Pexists (varl, p) ->
      begin
        try
          (* TODO: add case analysis on the variables of the quantification
             that are constrained *)
          let env = bind_logic_vars env varl in
          let env_result = reduce_by_predicate ~alarm_mode env true p in
          unbind_logic_vars env_result varl
        with LogicEvalError _ -> env
      end

    | _, Plet (li, p') ->
      begin
        try
          let env = eval_let_binding ~alarm_mode env li in
          let env_result = reduce_by_predicate ~alarm_mode env positive p' in
          unbind_logic_vars env_result [li.l_var_info]
        with LogicEvalError _ -> env
      end

    | _,Papp (li, labels, args) -> begin
        if is_known_predicate li.l_var_info then
          try reduce_by_known_papp ~alarm_mode env positive li labels args
          with
          | Reduce_to_bottom -> overwrite_current_state env Model.bottom
          | LogicEvalError _ | Not_an_exact_loc -> env
        else
          match Inline.inline_predicate ~inline ~current:env.e_cur p with
          | None -> env
          | Some p' -> reduce_by_predicate_content env positive p'.pred_content
      end
    | _,Pif (pcond, ptrue, pfalse) ->
      begin
        let reduce = reduce_by_predicate ~alarm_mode in
        match eval_predicate env pcond with
        | Unknown ->
          let env_true = reduce env positive ptrue in
          let env_false = reduce env positive pfalse in
          join_env env_true env_false
        | True -> reduce env positive ptrue
        | False -> reduce env positive pfalse
      end
    | true, Pexists (_, _) | false, Pforall (_, _)
    | _,Pallocable (_,_) | _,Pfreeable (_,_) | _,Pfresh (_,_,_,_)
    | _, Pseparated _
      -> env
  in
  reduce_by_predicate_content env positive p.pred_content

(* -------------------------------------------------------------------------- *)
(* --- Evaluation of predicates                                           --- *)
(* -------------------------------------------------------------------------- *)

and eval_predicate env pred =
  let alarm_mode = Fail in
  let loc = pred.pred_loc in
  let rec do_eval env p =
    try do_eval_exn env p
    with LogicEvalError ee ->
      display_evaluation_error ~loc:p.pred_loc ee;
      Unknown
  and do_eval_exn env p =
    match p.pred_content with
    | Ptrue -> True
    | Pfalse -> False
    | Pand (p1,p2 ) ->
      begin match do_eval env p1 with
        | True -> do_eval env p2
        | False -> False
        | Unknown ->
          let reduced = reduce_by_predicate ~alarm_mode env true p1 in
          match do_eval reduced p2 with
          | False -> False
          | _ -> Unknown
      end
    | Por (p1,p2 ) ->
      let val_p1 = do_eval env p1 in
      (*Format.printf "Disjunction: state %a p1:%a@."
          Cvalue.Model.pretty (env_current_state env)
          Printer.pp_predicate p1; *)
      begin match val_p1 with
        | True -> True
        | False -> do_eval env p2
        | Unknown -> begin
            let reduced_state = reduce_by_predicate ~alarm_mode env false p1 in
            (* Format.printf "Disjunction: reduced to %a to eval %a@."
               Cvalue.Model.pretty (env_current_state reduced_state)
               Printer.pp_predicate p2; *)
            match do_eval reduced_state p2 with
            | True -> True
            | _ -> Unknown
          end
      end
    | Pxor (p1,p2) ->
      begin match do_eval env p1, do_eval env p2 with
        | True, True -> False
        | False, False -> False
        | True, False | False, True -> True
        | Unknown, _ | _, Unknown -> Unknown
      end
    | Piff (p1,p2 ) ->
      begin match do_eval env p1,do_eval env p2 with
        | True, True | False, False ->  True
        | Unknown, _ | _, Unknown -> Unknown
        | _ -> False
      end
    | Pat (p, lbl) ->
      ignore (env_state env lbl);
      do_eval { env with e_cur = lbl } p

    | Pvalid (_, tsets)
    | Pvalid_read (_, tsets)
    | Pobject_pointer (_, tsets) ->
      (* TODO: see same constructor in reduce_by_predicate *)
      let kind: Locations.access =
        match p.pred_content with
        | Pvalid_read _ -> Read
        | Pvalid _ -> Write
        | Pobject_pointer _ -> Object_pointer
        | _ -> assert false
      in
      let typ_pointed = Logic_typing.ctype_of_pointed tsets.term_type in
      (* Check if we are trying to write in a const l-value *)
      if kind = Write && Eva_utils.is_const_write_invalid typ_pointed
      then False
      else
        let eover, eunder, indeterminate, empty =
          match tsets.term_node with
          | TLval tlval ->
            (* Do not use [eval_term]: the evaluation would fail if the value of
               [tlval] is uninitialized or escaping. *)
            let r = eval_tlval ~alarm_mode env tlval in
            let size = Eval_typ.sizeof_lval_typ r.etype in
            let loc = Locations.make r.eover size in
            let state = env_current_state env in
            let v = find_indeterminate ~alarm_mode state loc in
            let v, indeterminate =
              Cvalue.V_Or_Uninitialized.(get_v v, is_indeterminate v)
            in
            v, Cvalue.V.bottom, indeterminate, r.empty
          | _ ->
            let result = eval_term ~alarm_mode env tsets in
            result.eover, result.eunder, false, result.empty
        in
        let status =
          (* [True] on empty sets, [False] on bottom locations otherwise. *)
          if Cvalue.V.is_bottom eover
          then if empty then True else False
          else
            let size = Eval_typ.sizeof_lval_typ typ_pointed in
            let make_loc l =
              Locations.make (Addresses.Bits.of_bytes l) size
            in
            let loc_over = make_loc eover in
            (* The predicate holds if [eover] is entirely valid. It is false if
               [eover] is entirely invalid or if [eunder] contains an invalid
               location. Unknown otherwise. *)
            if Locations.is_valid kind loc_over
            then True
            else if Locations.(is_bottom (valid_part kind loc_over))
                 || contains_invalid_loc kind (make_loc eunder)
            then False
            else Unknown
        in
        (* False status on uninitialized or escaping pointers. *)
        let status =
          if indeterminate then join_predicate_status status False else status
        in
        (* True status on empty sets. *)
        if empty then join_predicate_status status True else status

    | Pvalid_function tsets -> begin
        let v = eval_term ~alarm_mode env tsets in
        let funs, warn = Main_values.CVal.resolve_functions v.eover in
        match funs with
        | `Top -> Unknown
        | `Value funs ->
          let typ = Ast_types.direct_pointed_type v.etype in
          let funs, warn' = Eval_typ.compatible_functions typ funs in
          if warn || warn' then
            (* No function possible -> signal hard error. Otherwise, follow
               Eva's convention, which is not to stop on semi-ok functions. *)
            if funs = [] then False else Unknown
          else
            True
      end

    | Paligned (t, n) -> begin
        let align = eval_term ~alarm_mode env n in
        let to_int v =
          try Cvalue.V.project_ival v |> Ival.project_int |> Z.to_int_opt
          with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int -> None
        in
        match to_int align.eover with
        | Some align when align > 0 ->
          let pointer = eval_term ~alarm_mode env t in
          let truth = Cvalue_forward.assume_aligned align pointer.eover in
          truth_to_status truth
        | Some _ | None -> Unknown
      end

    | Pinitialized (label,tsets) | Pdangling (label,tsets) -> begin
        (* Create [*tsets] and compute its location. This is important in
           case [tsets] points to the address of a bitfield, which we
           cannot evaluate as a pointer (indexed on bytes) *)
        let star_tsets = deref_tsets tsets in
        let locb = eval_tlval ~alarm_mode env star_tsets in
        let state = env_state env label in
        match p.pred_content with
        | Pinitialized _ -> eval_initialized state locb
        | Pdangling _ -> eval_dangling state locb
        | _ -> assert false
      end

    | Prel (op,t1,t2) ->
      let r = eval_binop ~alarm_mode env (lop_to_cop op) t1 t2 in
      (* [eval_binop] uses the forward semantics of [Cvalue.V], which does not
         handle empty sets. Returns [Unknown] if empty sets are possible. *)
      if r.empty
      then Unknown
      else if V.equal V.singleton_zero r.eover
      then False
      else if V.equal V.singleton_one r.eover
      then True
      else Unknown

    | Pforall (varl, p') | Pexists (varl, p') ->
      begin
        (* If [p'] is true (or false) for all possible values of [varl],
           then so is Pforall(varl, p') and Pexists(varl, p'). *)
        let env = bind_logic_vars env varl in
        let r = do_eval env p' in
        if r <> Unknown then r else
          (* Otherwise:
             - if [p'] evaluates to [false] for at least some values of [varl],
               then Pforall (varl, p') is false.
             - if [p'] evaluates to [true] for at least some values of [varl],
               then Pexists (varl, p') is true.

             In order to find such values, we reduce the environment by assuming
             [p'] is true (for Pexists) or false (for Pforall), and then we
             reevaluate [p'] with these values. *)
          let positive =
            match p.pred_content with Pforall _ -> false | _ -> true
          in
          let reduced_env = reduce_by_predicate ~alarm_mode env positive p' in
          (* Reduce values of logical variables [varl] in [env] according to
             [reduced_env], and splits those reduced values into singletons
             if possible. *)
          let env = copy_logic_vars ~src:reduced_env ~dst:env varl in
          let env_list = split_logic_vars env varl in
          let exists t = List.exists (fun env -> do_eval env p' = t) env_list in
          match p.pred_content with
          | Pexists _ when exists True -> True
          | Pforall _ when exists False -> False
          | _ -> Unknown
      end

    | Plet (li, p') ->
      let env = eval_let_binding ~alarm_mode env li in
      do_eval env p'

    | Pnot p ->  begin match do_eval env p with
        | True -> False
        | False -> True
        | Unknown -> Unknown
      end

    | Pimplies (p1,p2) ->
      do_eval env (Logic_const.por ~loc ((Logic_const.pnot ~loc p1), p2))

    | Pseparated ltsets ->
      (try
         let to_zones tset =
           (* Create [*tset] and compute its location. This is important in
              case [tset] points to the address of a bitfield, which we
              cannot evaluate as a pointer (indexed on bytes). *)
           let star_tset = deref_tsets tset in
           let rtset = eval_tlval ~alarm_mode env star_tset in
           let size = Eval_typ.sizeof_lval_typ rtset.etype in
           let addr_over = rtset.eover in
           let addr_under = rtset.eunder in
           Locations.enumerate_bits (Locations.make addr_over size),
           Locations.enumerate_bits_under (Locations.make addr_under size)
         in
         let lz = List.map to_zones ltsets in
         let unknown = ref false in
         (* Are those two lists of locations separated? *)
         let do_two (z1, zu1) l2 =
           let combine (z2, zu2) =
             if Memory_zone.intersects z1 z2 then begin
               unknown := true;
               if Memory_zone.intersects zu1 zu2 then raise Exit;
             end
           in
           List.iter combine l2
         in
         let rec aux = function
           | [] | [_] -> ()
           | loc :: qlocs ->
             do_two loc qlocs;
             aux qlocs
         in
         aux lz;
         if !unknown then Unknown else True
       with
       | Exit -> False)

    | Papp (li, labels, args) -> begin
        if is_known_predicate li.l_var_info then
          eval_known_papp env li labels args
        else
          match Inline.inline_predicate ~inline ~current:env.e_cur p with
          | None -> Unknown
          | Some p' -> do_eval env p'
      end

    | Pif (pcond, ptrue, pfalse) ->
      begin
        match eval_predicate env pcond with
        | Unknown ->
          let env_true = reduce_by_predicate ~alarm_mode env true pcond in
          let env_false = reduce_by_predicate ~alarm_mode env false pcond in
          join_predicate_status (do_eval env_true ptrue) (do_eval env_false pfalse)
        | True -> do_eval env ptrue
        | False -> do_eval env pfalse
      end
    | Pfreeable (BuiltinLabel Here, t) ->
      let r = eval_term ~alarm_mode env t in
      Builtins_malloc.freeable r.eover
    | Pfresh _ | Pallocable _ | Pfreeable _ -> Unknown

  (* Logic predicates. Update the list known_predicates above if you
     add something here. *)
  and eval_known_papp env li _labels args =
    let open Abstract_interp in
    let unary_float unary_fun arg =
      try
        let eval_result = eval_term ~alarm_mode env arg in
        unary_fun (V.project_float eval_result.eover)
      with
      | V.Not_based_on_null -> Unknown
    in
    let fval_cmp comp arg1 arg2 =
      try
        let e1 = eval_term ~alarm_mode env arg1
        and e2 = eval_term ~alarm_mode env arg2 in
        let f1 = V.project_float e1.eover
        and f2 = V.project_float e2.eover in
        Fval.forward_comp comp f1 f2
      with
      | V.Not_based_on_null -> Unknown
    in
    match li.l_var_info.lv_name, args with
    | "\\is_finite", [arg] -> unary_float Fval.is_finite arg
    | "\\is_infinite", [arg] -> unary_float Fval.is_infinite arg
    | "\\is_plus_infinity", [arg] ->
      let pos_inf = Fval.pos_infinity Float_sig.Single in
      unary_float (fun f -> Fval.forward_comp Comp.Eq f pos_inf) arg
    | "\\is_minus_infinity", [arg] ->
      let neg_inf = Fval.neg_infinity Float_sig.Single in
      unary_float (fun f -> Fval.forward_comp Comp.Eq f neg_inf) arg
    | "\\is_NaN", [arg] -> inv_truth (unary_float Fval.is_not_nan arg)
    | ("\\eq_float" | "\\eq_float32" | "\\eq_float64" | "\\eq_double"),
      [arg1;arg2] -> fval_cmp Comp.Eq arg1 arg2
    | ("\\ne_float" | "\\ne_float32" | "\\ne_float64" | "\\ne_double"),
      [arg1;arg2] -> fval_cmp Comp.Ne arg1 arg2
    | ("\\lt_float" | "\\lt_float32" | "\\lt_float64" | "\\lt_double"),
      [arg1;arg2] -> fval_cmp Comp.Lt arg1 arg2
    | ("\\le_float" | "\\le_float32" | "\\le_float64" | "\\le_double"),
      [arg1;arg2] -> fval_cmp Comp.Le arg1 arg2
    | ("\\gt_float" | "\\gt_float32" | "\\gt_float64" | "\\gt_double"),
      [arg1;arg2] -> fval_cmp Comp.Gt arg1 arg2
    | ("\\ge_float" | "\\ge_float32" | "\\ge_float64" | "\\ge_double"),
      [arg1;arg2] -> fval_cmp Comp.Ge arg1 arg2
    | "\\warning", _ -> begin
        match args with
        | [{ term_node = TConst(LStr(str))}] ->
          Self.warning "reached \\warning(\"%s\")" str; Unknown
        | _ ->
          Self.abort
            "Wrong argument: \\warning expects a constant string"
      end
    | "\\subset", [argl;argr] ->
      let l = eval_term ~alarm_mode env argl in
      let r = eval_term ~alarm_mode env argr in
      if r.empty then
        if V.is_bottom l.eover then True
        else if not (V.is_bottom l.eunder || l.empty) then False
        else Unknown
      else if V.is_included l.eover r.eunder then
        True (* all elements of [l] are included in the guaranteed elements
                of [r] *)
      else if not (V.is_included l.eunder r.eover) ||
              not (V.intersects l.eover r.eover)
      then False (* one guaranteed element of [l] is not included in [r],
                    or [l] and [r] are disjoint, in which case there is
                    an element of [l] not in [r]. (Here, [l] is not bottom,
                    as [V.is_included bottom r.eunder] holds. *)
      else Unknown
    | ("\\tainted" | "\\tainted_directly" | "\\tainted_indirectly"), [_] ->
      Unknown
    | "valid_read_string", [arg] ->
      let r = eval_term ~alarm_mode env arg in
      eval_valid_read_str ~wide:false env r.eover
    | "valid_string", [arg] ->
      let r = eval_term ~alarm_mode env arg in
      eval_valid_str ~wide:false env r.eover
    | "valid_read_wstring", [arg] ->
      let r = eval_term ~alarm_mode env arg in
      eval_valid_read_str ~wide:true env r.eover
    | "valid_wstring", [arg] ->
      let r = eval_term ~alarm_mode env arg in
      eval_valid_str ~wide:true env r.eover
    | "is_allocable", [arg] when comes_from_fc_stdlib li.l_var_info ->
      let r = eval_term ~alarm_mode env arg in
      eval_is_allocable r.eover
    | _, _ -> assert false
  in
  do_eval env pred

(* -------------------------------------------------------------------------- *)
(* --- Dependencies of predicates                                         --- *)
(* -------------------------------------------------------------------------- *)

and eval_tsets_deps ~alarm_mode env lbl tsets =
  let star_tsets = deref_tsets tsets in
  let r = eval_tlval ~alarm_mode env star_tsets in
  let size_bits = Eval_typ.sizeof_lval_typ r.etype in
  let loc = Locations.make r.eover size_bits in
  let zone = Locations.enumerate_valid_bits Locations.Read loc in
  Logic_label.Map.add lbl zone r.ldeps

and predicate_deps env pred =
  let alarm_mode = Ignore in
  let rec do_eval env p =
    let term_deps term = (eval_term ~alarm_mode env term).ldeps in
    let tsets_deps lbl tsets = eval_tsets_deps ~alarm_mode env lbl tsets in
    let logic_info_deps li =
      match li.l_body with
      | LBnone -> empty_logic_deps
      | LBterm t -> term_deps t
      | LBpred p -> do_eval env p
      | _ -> unsupported (Format.asprintf "%a" Printer.pp_logic_info li)
    in
    match p.pred_content with
    | Ptrue | Pfalse -> empty_logic_deps

    | Pand (p1, p2) | Por (p1, p2 ) | Pxor (p1, p2) | Piff (p1, p2 )
    | Pimplies (p1, p2) ->
      join_logic_deps (do_eval env p1) (do_eval env p2)

    | Prel (_, t1, t2) ->
      join_logic_deps (term_deps t1) (term_deps t2)

    | Pif (c, p1, p2) ->
      join_logic_deps (do_eval env c)
        (join_logic_deps (do_eval env p1) (do_eval env p2))

    | Pat (p, lbl) ->
      do_eval { env with e_cur = lbl } p

    | Pvalid (_, tsets) | Pvalid_read (_, tsets)
    | Pobject_pointer (_, tsets) | Pvalid_function tsets ->
      term_deps tsets

    | Paligned (t, n) -> join_logic_deps (term_deps t) (term_deps n)

    | Pinitialized (lbl, tsets) | Pdangling (lbl, tsets) ->
      tsets_deps lbl tsets

    | Pnot p -> do_eval env p

    | Pseparated ltsets ->
      List.fold_left
        (fun acc tsets -> join_logic_deps acc (tsets_deps lbl_here tsets))
        empty_logic_deps ltsets

    | Pexists (l, p) | Pforall (l, p) ->
      let env = bind_logic_vars env l in
      (* TODO: unbind all references to l in the results? If so, clean up
         Logic_interp.do_term_lval. *)
      do_eval env p

    | Plet (li, p) ->
      let env = eval_let_binding ~alarm_mode env li in
      join_logic_deps (logic_info_deps li) (do_eval env p)

    | Papp (li, _labels, args) -> begin
        if is_known_predicate li.l_var_info then
          List.fold_left
            (fun acc arg -> join_logic_deps acc (term_deps arg))
            empty_logic_deps args
        else
          match Inline.inline_predicate ~inline ~current:env.e_cur p with
          | None -> unsupported (Format.asprintf "%a" Printer.pp_predicate p)
          | Some p' -> do_eval env p'
      end

    | Pfresh _ | Pallocable _ | Pfreeable _
      -> unsupported (Format.asprintf "%a" Printer.pp_predicate p)
  in
  try Some (do_eval env pred)
  with LogicEvalError _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Export                                                             --- *)
(* -------------------------------------------------------------------------- *)

(* Position default value for ~alarm_mode *)
let reduce_by_predicate env positive p =
  let alarm_mode = alarm_reduce_mode () in
  reduce_by_predicate ~alarm_mode env positive p

let eval_tlval_as_location ~alarm_mode env t =
  let r = eval_term_as_lval ~alarm_mode env t in
  let s = Eval_typ.sizeof_lval_typ r.etype in
  Locations.make r.eover s

(* Return a pair of (under-approximating, over-approximating) zones. *)
let eval_tlval_as_zone_under_over ~alarm_mode access env t =
  let r = eval_term_as_lval ~alarm_mode env t in
  let size = Eval_typ.sizeof_lval_typ r.etype in
  let under_loc = Locations.make r.eunder size in
  let under_zone = Locations.enumerate_valid_bits_under access under_loc in
  let over_loc = Locations.make r.eover size in
  let over_zone = Locations.enumerate_valid_bits access over_loc in
  (under_zone, over_zone)

let eval_tlval_as_zone ~alarm_mode access env t =
  let _under, over =
    eval_tlval_as_zone_under_over ~alarm_mode access env t
  in
  over

let tlval_deps env t = (eval_term_as_lval ~alarm_mode:Ignore env t).ldeps
