(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Evaluation of expressions to values. *)

open Cil_types
open Eval
open Eva_ast

(* The forward evaluation of an expression [e] gives a value to each subterm
   of [e], from its variables to the root expression [e]. It also computes the
   set of alarms which may occur in the evaluation of each subterm.
   All these intermediate results of an evaluation are stored in a cache, whose
   type is described in eval.mli. The cache is the complete result of the
   evaluation. *)

(* The forward evaluation of an expression relies on queries of the abstract
   domain, which must be able to assign a value to some expression (see
   abstract_domain.mli for more details).
   An oracle for the value of expressions is also given to the domain,
   which may use it to build its answer. This oracle is the main forward
   evaluation function itself, so the domain may initiate the evaluation
   of some new expressions.
   To avoid loops in the use of the oracle:
   - before any computation or an expression [e], Value.top is stored
     in the cache; this dummy value will be erased at the end of the
     computation, but in the meantime, any evaluation of [e] (by the oracle)
     returns top immediately.
   - fuel is used to limit the depth of the use of the oracle. The fuel level is
     decremented at each use of the oracle up to zero, where the oracle returns
     top.
     The fuel level with which an expression has been evaluated is stored in the
     cache. The recomputation of an expression may be performed with a higher
     fuel than before, if needed. *)

(* Reductions may happen in the forward evaluation when:
   – a domain returns a value more precise than the one internally computed;
   – alarms are emitted by an operation on values. In particular, locations
     are reduced to their valid part for a read or write operation.
   These reductions are propagated to the sub-expressions by a backward
   evaluation, after the forward evaluation has finished.
   The backward evaluation also propagates the reduction stemming from an if
   statement, where the condition may be reduced to zero or non-zero. *)

(* An expression is deemed volatile if it contains an access to a volatile
   location. The forward evaluation computes this syntactically, by checking
   for volatile qualifiers on sub-lvalues and intermediate types. A 'volatile'
   flag is propagated through the expression. This flag prevents the update of
   the value computed by the initial forward evaluation. *)

(* When a backward reduction has been successfully performed, the domain may
   initiate new reductions, via the reduce_further function.
   A fuel level is used in the same way as for the forward evaluation to
   avoid reduction loops. *)


(* The fuel level with which an expression has been evaluated. *)
type fuel =
  | Loop
  (* No evaluation at all: the value in the cache is a dummy value, set
     to avoid a loop in the use of the oracle. *)
  | Finite of int
  (* An evaluation with a finite level of fuel, which has been consumed. *)
  | Infty
  (* The evaluation never used all its fuel. *)

let less_fuel_than n = function
  | Loop     -> true
  | Finite f -> f >= n
  | Infty    -> true

type reduction_kind = | Neither | Forward | Backward

let update_reduction reduction b = match reduction with
  | Neither -> if b then Neither else Forward
  | x -> x

(* Some information about a forward evaluation. *)
type forward_report = {
  fuel: fuel;                (* The fuel used for the evaluation. *)
  reduction: reduction_kind; (* Whether a reduction has occur, which may be
                                propagated to the sub-terms. *)
  volatile: bool;         (* If true, the expression may contain an access
                             to a volatile location, and thus cannot be safely
                             reduced. *)
}

(* Parameters of the evaluation of the location of a left value. *)
type loc_report = {
  for_writing: bool;
  with_reduction: bool;
}

(* If a value is cached by an external source, we assume that it was
   computed with infty fuel, that possible reduction have been
   propagated backward, and that the expression cannot be volatile. *)
let extern_report = { fuel = Infty; reduction = Neither; volatile = false }

let no_fuel = -1
let root_fuel () = Parameters.OracleDepth.get ()
let backward_fuel () = Parameters.ReductionDepth.get ()

let already_precise_loc_report ~for_writing ~reduction loc_report =
  (not for_writing || loc_report.for_writing)
  && (not reduction || loc_report.with_reduction)


let rec may_be_reduced_offset = function
  | NoOffset -> false
  | Field (_, offset) -> may_be_reduced_offset offset
  | Index _ -> true

let may_be_reduced_lval lval =
  let (host, offset) = lval.node in
  match host with
  | Var _ -> may_be_reduced_offset offset
  | Mem _ -> true

let rec is_packed_struct_field offset =
  match offset with
  | NoOffset -> false
  | Index (_, offset) -> is_packed_struct_field offset
  | Field (fi, offset) ->
    Ast_attributes.contains "packed" fi.fcomp.cattr
    || is_packed_struct_field offset

let warn_pointer_comparison typ =
  match Parameters.WarnPointerComparison.get () with
  | `None -> false
  | `All -> true
  | `Pointer -> Ast_types.(is_ptr typ)

let propagate_all_pointer_comparison typ =
  not (Ast_types.is_ptr typ)
  || Parameters.UndefinedPointerComparisonPropagateAll.get ()

let comparison_kind = function
  | Eq | Ne -> Some Abstract_value.Equality
  | Le | Lt
  | Ge | Gt -> Some Abstract_value.Relation
  | _ -> None

let signed_ikind = function
  | IBool                   -> IBool
  | IChar | ISChar | IUChar -> ISChar
  | IInt | IUInt            -> IInt
  | IShort | IUShort        -> IShort
  | ILong | IULong          -> ILong
  | ILongLong | IULongLong  -> ILongLong
  | IInt128 | IUInt128      -> IInt128

let rec signed_counterpart typ =
  let typ = Ast_types.unroll typ in
  match typ.tnode with
  | TInt ik -> Cil_const.mk_tint ~tattr:typ.tattr (signed_ikind ik)
  | TEnum ei ->
    let info = { ei with ekind = signed_ikind ei.ekind } in
    Cil_const.mk_tenum ~tattr:typ.tattr info
  | TPtr _ -> signed_counterpart ((Machine.uintptr_type ()))
  | _ -> assert false

let return t = `Value t, Alarmset.none

(* Intersects [alarms] with the only possible alarms from the dereference of
   the left-value [lval] of type [typ].
   Useful if the abstract domain returns a non-closed AllBut alarmset for
   some lvalues. *)
let close_dereference_alarms lval alarms =
  let init_alarm = Alarms.Uninitialized lval
  and escap_alarm = Alarms.Dangling lval in
  let init_status = Alarmset.find init_alarm alarms
  and escap_status = Alarmset.find escap_alarm alarms in
  let closed_alarms = Alarmset.set init_alarm init_status Alarmset.none in
  let closed_alarms = Alarmset.set escap_alarm escap_status closed_alarms in
  closed_alarms

let define_value value =
  { v = `Value value; initialized = true; escaping = false }

(* [record] and [alarms] must be the value and the alarms resulting from the
   evaluation of the lvalue [lval].
   This function removes the alarms about the initialization and the
   escaping of [lval], and sets accordingly the initialized and escaping flags
   of the computed value. *)
let indeterminate_copy lval result alarms =
  let init_alarm = Alarms.Uninitialized lval
  and escap_alarm = Alarms.Dangling lval in
  let initialized = Alarmset.find init_alarm alarms = Alarmset.True
  and escaping = not (Alarmset.find escap_alarm alarms = Alarmset.True) in
  let alarms =
    if not (initialized)
    then Alarmset.set init_alarm Alarmset.True alarms
    else alarms
  in
  let alarms =
    if escaping
    then Alarmset.set escap_alarm Alarmset.True alarms
    else alarms
  in
  let reductness = Unreduced in
  let v, origin = match result with
    | `Bottom -> `Bottom, None
    | `Value (v, origin) -> `Value v, origin
  in
  let value = { v; initialized; escaping } in
  let record = { value; origin; reductness; val_alarms = alarms} in
  record, alarms


module type Value = sig
  include Abstract.Value.External
  val reduce : t -> t
end

module type Queries = sig
  include Abstract_domain.Queries
  include Datatype.S with type t = state
end

module Make
    (Context : Abstract_context.S)
    (Value : Value with type context = Context.t)
    (Loc : Abstract_location.S with type value = Value.t)
    (Domain : Queries with type context = Context.t
                       and type value = Value.t
                       and type location = Loc.location)
= struct

  type state = Domain.state
  type context = Context.t
  type value = Value.t
  type origin = Domain.origin
  type loc = Loc.location

  module ECache = Eva_ast.Exp.Map
  module LCache = Eva_ast.Lval.Map

  (* Imperative cache for the evaluation:
     all intermediate results of an evaluation are cached here.
     See [eval.mli] for more details. *)
  module Cache = struct
    type value = Value.t
    type origin = Domain.origin
    type loc = Loc.location

    (* For expression, the forward_report about the evaluation is also stored. *)
    type t =
      ((value, origin) record_val * forward_report) ECache.t
      * (loc record_loc * (forward_report * loc_report)) LCache.t

    (* Interface of Context.Valuation *)
    let empty : t = ECache.empty, LCache.empty
    let find (cache:t) exp =
      try `Value (fst (ECache.find exp (fst cache)))
      with Not_found -> `Top
    let add (cache:t) exp record =
      let s, t = cache in ECache.add exp (record, extern_report) s, t
    let fold f (cache:t) acc =
      ECache.fold (fun e (r, _) acc -> f e r acc) (fst cache) acc

    (* Functions used by the evaluator, with the boolean for backward
       reduction. *)
    let find' (cache:t) exp = ECache.find exp (fst cache)
    let add' (cache:t) exp record =
      let s, t = cache in ECache.add exp record s, t

    (* Locations of lvalue. *)
    let find_loc (cache:t) lval =
      try `Value (fst (LCache.find lval (snd cache)))
      with Not_found -> `Top

    let find_loc_def cache lval =
      match find_loc cache lval with
      | `Top -> Loc.top
      | `Value record -> record.loc

    (* Locations of lvalue. *)
    let find_loc' (cache:t) lval =
      try `Value (LCache.find lval (snd cache))
      with Not_found -> `Top
    let add_loc' (cache:t) lval record =
      let s, t = cache in s, LCache.add lval record t

    let remove (s, t) expr = ECache.remove expr s, t
    let remove_loc (s, t) lval = s, LCache.remove lval t
  end

  (* Imperative cache for the evaluator. A reference is mandatory here, because
     the cache must be also filled by the evaluations initiated by a domain
     through the oracle, but should not leak in the domain queries signature. *)
  let cache = ref Cache.empty

  (* Was the fuel entirely consumed? *)
  let fuel_consumed = ref false

  let top_entry =
    let v = `Value Value.top in
    let value = { v; initialized = false; escaping = true } in
    let val_alarms = Alarmset.all in
    let record = { value; origin = None; reductness = Dull; val_alarms } in
    let report = { fuel = Loop; reduction = Neither; volatile = false } in
    record, report

  (* Updates the abstractions stored in the cache for the expression [expr]
     with the given record, report and value. [kind] is the type of the
     reduction (forward or backward). *)
  let reduce_expr_recording kind expr (record, report) value =
    (* Avoids reduction of volatile expressions. *)
    if report.volatile then ()
    else
      let red = record.reductness in
      let reductness =
        if red = Unreduced && kind <> Neither then Reduced else red
      in
      (* TODO: allow to reduce initialized and escaping flags? *)
      let record = { record with value = define_value value; reductness } in
      let report = { report with reduction = kind } in
      cache := Cache.add' !cache expr (record, report)

  (* Updates the abstractions stored in the cache for the expression [expr]
     with the value [value]. [kind] is the type of the reduction.*)
  let reduce_expr_value kind expr value =
    let record, report = Cache.find' !cache expr in
    reduce_expr_recording kind expr (record, report) value

  let reduce_value record =
    let v = record.value.v >>-: Value.reduce in
    { record with value = {record.value with v = v} }

  (* ------------------------------------------------------------------------
                    Forward Operations, Alarms and Reductions
     ------------------------------------------------------------------------ *)

  (* Applies function [f] on the optional value in a truth. *)
  let apply_on_truth f = function
    | `Unknown v -> `Unknown (f v)
    | `TrueReduced v -> `TrueReduced (f v)
    | `True | `False | `Unreachable as x -> x

  (* Assumes that the abstract value [value] represents only values
     different from integer [i]. *)
  let assume_non_equal context typ value i =
    let forward_binop = Value.forward_binop context typ in
    (* Bottom should never happen on addition or subtraction. *)
    let add v1 v2 = Bottom.non_bottom (forward_binop PlusA v1 v2) in
    let sub v1 v2 = Bottom.non_bottom (forward_binop MinusA v1 v2) in
    let v_i = Value.inject_int typ i in
    let truth = Value.assume_non_zero (sub value v_i) in
    apply_on_truth (fun value -> add value v_i) truth

  (* Handles the result of an [assume] function from value abstractions (see
     abstract_values.mli for more details), applied to the initial [value].
     If the value could have been reduced, [reduce] is applied on the new value.
     If the status is not true, [alarm] is used to create the alarm. *)
  let process_truth ~reduce ~alarm value =
    let build_alarm status = Alarmset.singleton ~status (alarm ()) in
    function
    | `Unreachable   -> `Bottom, Alarmset.none
    | `False         -> `Bottom, build_alarm Alarmset.False
    | `Unknown v     -> reduce v; `Value v, build_alarm Alarmset.Unknown
    | `TrueReduced v -> reduce v; `Value v, Alarmset.none
    | `True          -> `Value value, Alarmset.none

  (* Does not register the possible reduction, as the initial [value] has not
     been saved yet. *)
  let interpret_truth ~alarm value truth =
    let reduce _ = () in
    process_truth ~reduce ~alarm value truth

  let reduce_argument (e, v) new_value =
    if not (Value.equal v new_value) then reduce_expr_value Forward e new_value

  (* Registers the new value if it has been reduced. *)
  let reduce_by_truth ~alarm (expr, value) truth =
    let reduce = reduce_argument (expr, value) in
    process_truth ~reduce ~alarm value truth

  (* Interprets a [truth] about a pair of values (v1, v2), and reduce both
     values accordingly. Used on the result of [Value.assume_comparable], which
     affects both arguments of the comparison. *)
  let reduce_both_by_truth ~alarm (e1, v1) (e2, v2) truth =
    let reduce (new_value1, new_value2) =
      Option.iter (fun e1 -> reduce_argument (e1, v1) new_value1) e1;
      reduce_argument (e2, v2) new_value2;
    in
    process_truth ~reduce ~alarm (v1, v2) truth

  (* Creates a disjunctive truth about the pair of values (v1, v2). *)
  let disjunctive_truth (v1, truth1) (v2, truth2) =
    match truth1, truth2 with
    | (`True | `TrueReduced _), _
    | _, (`True | `TrueReduced _) -> `True
    | `Unreachable, _ | _, `Unreachable -> `Unreachable
    | `False, `False -> `False
    | `False, `Unknown v2 -> `Unknown (v1, v2)
    | `Unknown v1, `False -> `Unknown (v1, v2)
    | `Unknown _, `Unknown _ ->
      (* [v1] (resp. [v2]) cannot be reduced as [truth2] (resp. [truth1])
         might be true for some values. *)
      `Unknown (v1, v2)

  let is_true = function
    | `True | `TrueReduced _ -> true
    | _ -> false

  (* Overflow of div is processed separately in [assume_valid_div]. *)
  let may_overflow = function
    | Shiftlt | Mult | MinusPP | MinusPI | PlusPI | PlusA | MinusA -> true
    | _ -> false

  let truncate_bound overflow_kind bound bound_kind expr value =
    let alarm () =
      (* The expression does not necessary come from the original program *)
      let cil_expr = Eva_ast.to_cil_exp expr in
      Alarms.Overflow (overflow_kind, cil_expr, bound, bound_kind)
    in
    let bound = Abstract_value.Int bound in
    let truth = Value.assume_bounded bound_kind bound value in
    interpret_truth ~alarm value truth

  let truncate_lower_bound overflow_kind expr range value =
    let min_bound = Eval_typ.range_lower_bound range in
    let bound_kind = Alarms.Lower_bound in
    truncate_bound overflow_kind min_bound bound_kind expr value

  let truncate_upper_bound overflow_kind expr range value =
    let max_bound = Eval_typ.range_upper_bound range in
    let bound_kind = Alarms.Upper_bound in
    truncate_bound overflow_kind max_bound bound_kind expr value

  let truncate_integer overflow_kind expr range value =
    truncate_lower_bound overflow_kind expr range value >>= fun value ->
    truncate_upper_bound overflow_kind expr range value

  let handle_integer_overflow context expr range value =
    let signed = range.Eval_typ.i_signed in
    let signed_overflow = signed && Kernel.SignedOverflow.get () in
    let unsigned_overflow = not signed && Kernel.UnsignedOverflow.get () in
    if signed_overflow || unsigned_overflow then
      let overflow_kind = if signed then Alarms.Signed else Alarms.Unsigned in
      truncate_integer overflow_kind expr range value
    else
      let v = Value.rewrap_integer context range value in
      if range.Eval_typ.i_signed && not (Value.equal value v) then
        Self.warning ~wkey:Self.wkey_signed_overflow
          ~current:true ~once:true "2's complement assumed for overflow" ;
      return v

  let restrict_float ?(reduce=false) ~assume_finite expr fkind value =
    let truth = Value.assume_not_nan ~assume_finite fkind value in
    let alarm () =
      let cil_expr = Eva_ast.to_cil_exp expr in
      if assume_finite
      then Alarms.Is_nan_or_infinite (cil_expr, fkind)
      else Alarms.Is_nan (cil_expr, fkind)
    in
    if reduce
    then reduce_by_truth ~alarm (expr, value) truth
    else interpret_truth ~alarm value truth

  let remove_special_float expr fk value =
    match Kernel.SpecialFloat.get () with
    | "none"       -> return value
    | "nan"        -> restrict_float ~assume_finite:false expr fk value
    | "non-finite" -> restrict_float ~assume_finite:true expr fk value
    | _            -> assert false

  (* Casts to void* are always safe, and no alignment for functions. *)
  let check_alignment target =
    Kernel.UnalignedPointer.get ()
    && not (Ast_types.is_void target)
    && not (Ast_types.is_fun target)

  let assume_aligned expr typ value =
    let target = Ast_types.direct_pointed_type typ in
    if check_alignment target then
      let target_align = Cil.bytesAlignOf target in
      if target_align > 1 then
        let truth = Value.assume_aligned target_align value in
        let alarm () =
          Alarms.Unaligned_pointer (Eva_ast.to_cil_exp expr, target)
        in
        interpret_truth ~alarm value truth
      else return value
    else return value

  let assume_pointer context expr value =
    let open Evaluated.Operators in
    let+ value =
      if Kernel.InvalidPointer.get () then
        let truth = Value.assume_pointer value in
        let alarm () = Alarms.Invalid_pointer (Eva_ast.to_cil_exp expr) in
        interpret_truth ~alarm value truth
      else return value
    in
    (* Rewrap absolute addresses of pointer values, seen as unsigned
       integers, to ensure a consistent representation of pointers. *)
    let range = Eval_typ.pointer_range () in
    Value.rewrap_integer context range value

  let handle_overflow ~may_overflow context expr typ value =
    match Eval_typ.classify_as_scalar typ with
    | Some (Eval_typ.TSInt range) ->
      (* If the operation cannot overflow, truncates the abstract value to the
         range of the type (without emitting alarms). This can regain some
         precision when the abstract operator was too imprecise.
         Otherwise, truncates or rewraps the abstract value according to
         the parameters of the analysis. *)
      if not may_overflow
      then fst (truncate_integer Alarms.Signed expr range value), Alarmset.none
      else handle_integer_overflow context expr range value
    | Some (Eval_typ.TSFloat fk) -> remove_special_float expr fk value
    | Some (Eval_typ.TSPtr _) -> assume_pointer context expr value
    | None -> return value

  (* Assumes that [res] is a valid result for the lvalue [lval] of type [typ].
     Removes NaN and infinite floats and trap representations of bool values. *)
  let assume_valid_value context lval res =
    let open Evaluated.Operators in
    let* value, origin = res in
    match Ast_types.unroll_node lval.typ with
    | TFloat fkind ->
      let expr = Eva_ast.Build.lval lval in
      let+ new_value = remove_special_float expr fkind value in
      new_value, origin
    | TInt IBool when Kernel.InvalidBool.get () ->
      let one = Abstract_value.Int Z.one in
      let truth = Value.assume_bounded Alarms.Upper_bound one value in
      let alarm () = Alarms.Invalid_bool (Eva_ast.to_cil_lval lval) in
      let+ new_value = interpret_truth ~alarm value truth in
      new_value, origin
    | TPtr _ ->
      let expr = Eva_ast.Build.lval lval in
      let* new_value = assume_pointer context expr value in
      let+ new_value = assume_aligned expr lval.typ new_value in
      new_value, origin
    | _ -> res

  (* Reduce the rhs argument of a shift so that it fits inside [size] bits. *)
  let reduce_shift_rhs typ expr value =
    let open Evaluated.Operators in
    let size = Cil.bitsSizeOf typ in
    let size_int = Abstract_value.Int (Z.of_int (size - 1)) in
    let zero_int = Abstract_value.Int Z.zero in
    let alarm () =
      Alarms.Invalid_shift (Eva_ast.to_cil_exp expr, Some size)
    in
    let truth = Value.assume_bounded Alarms.Lower_bound zero_int value in
    let* value = reduce_by_truth ~alarm (expr, value) truth in
    let truth = Value.assume_bounded Alarms.Upper_bound size_int value in
    reduce_by_truth ~alarm (expr, value) truth

  (* Reduces the right argument of a shift, and if [warn_negative] is true,
     also reduces its left argument to a positive value. *)
  let reduce_shift ~warn_negative typ (e1, v1) (e2, v2) =
    let open Evaluated.Operators in
    let* v2 = reduce_shift_rhs typ e2 v2 in
    if warn_negative && Bit_utils.is_signed_int_enum_pointer typ then
      (* Cannot shift a negative value *)
      let zero_int = Abstract_value.Int Z.zero in
      let alarm () =
        Alarms.Invalid_shift (Eva_ast.to_cil_exp e1, None)
      in
      let truth = Value.assume_bounded Alarms.Lower_bound zero_int v1 in
      let+ v1 = reduce_by_truth ~alarm (e1, v1) truth in
      v1, v2
    else return (v1, v2)

  (* Emits alarms for an index out of bound, and reduces its value. *)
  let assume_valid_index ~size ~size_expr ~index_expr value =
    let open Evaluated.Operators in
    let size_int = Abstract_value.Int (Z.pred size) in
    let zero_int = Abstract_value.Int Z.zero in
    let alarm () =
      Alarms.Index_out_of_bound (Eva_ast.to_cil_exp index_expr, None)
    in
    let truth = Value.assume_bounded Alarms.Lower_bound zero_int value in
    let* value = reduce_by_truth ~alarm (index_expr, value) truth in
    let alarm () =
      Alarms.Index_out_of_bound (Eva_ast.to_cil_exp index_expr, Some size_expr)
    in
    let truth = Value.assume_bounded Alarms.Upper_bound size_int value in
    reduce_by_truth ~alarm (index_expr, value) truth

  (* Checks the arguments of a division and creates a division by zero alarm
     if [v2] can be equal to 0, and an overflow alarm if [typ] is a signed
     integer type, [v1] can be equal to min_int AND [v2] can be equal to -1.
     Also reduces the values of [v1] or [v2] accordingly when possible. *)
  let assume_valid_div context typ (e1, v1) (e2, v2) =
    let open Evaluated.Operators in
    let truth = Value.assume_non_zero v2 in
    let alarm () = Alarms.Division_by_zero (Eva_ast.to_cil_exp e2) in
    let* v2 = reduce_by_truth ~alarm (e2, v2) truth in
    match Eval_typ.integer_range ~ptr:false typ with
    | Some range when range.i_signed ->
      let min_int = Eval_typ.range_lower_bound range in
      let max_int = Eval_typ.range_upper_bound range in
      let alarm () =
        let expr = Eva_ast.Build.div e1 e2 in
        let cil_expr = Eva_ast.to_cil_exp expr in
        Alarms.(Overflow (Signed, cil_expr, max_int, Upper_bound))
      in
      let truth1 = assume_non_equal context typ v1 min_int in
      let truth2 = assume_non_equal context typ v2 Z.minus_one in
      let truth = disjunctive_truth (v1, truth1) (v2, truth2) in
      reduce_both_by_truth ~alarm (Some e1, v1) (e2, v2) truth
    | _ -> return (v1, v2)

  let assume_valid_binop context typ (e1, v1 as arg1) op (e2, v2 as arg2) =
    if Ast_types.is_integral typ || Ast_types.is_ptr typ then
      match op with
      | Div | Mod ->
        (* The behavior of a%b is undefined if the behavior of a/b is undefined,
           according to section 6.5.5 §5 and §6 of the C standard. *)
        assume_valid_div context typ arg1 arg2
      | Shiftrt ->
        let warn_negative = Kernel.RightShiftNegative.get () in
        reduce_shift ~warn_negative typ arg1 arg2
      | Shiftlt ->
        let warn_negative = Kernel.LeftShiftNegative.get () in
        reduce_shift ~warn_negative typ arg1 arg2
      | MinusPP when Parameters.WarnPointerSubtraction.get () ->
        let kind = Abstract_value.Subtraction in
        let truth = Value.assume_comparable kind v1 v2 in
        let alarm () =
          Alarms.Differing_blocks (Eva_ast.to_cil_exp e1, Eva_ast.to_cil_exp e2)
        in
        let arg1 = Some e1, v1 in
        reduce_both_by_truth ~alarm arg1 arg2 truth
      | _ -> return (v1, v2)
    else return (v1, v2)

  (* If possible, prints only the cvalue component of an abstract value. *)
  let pretty_cvalue fmt v =
    match Value.get Main_values.CVal.key with
    | Some get -> Cvalue.V.pretty fmt (get v)
    | None -> Value.pretty fmt v

  let forward_comparison ~compute ~typ_res ~typ_arg kind (e1, v1) (e2, v2) =
    let truth = Value.assume_comparable kind v1 v2 in
    let alarm () =
      let cil_e1 = Option.map Eva_ast.to_cil_exp e1
      and cil_e2 = Eva_ast.to_cil_exp e2 in
      Alarms.Pointer_comparison (cil_e1, cil_e2)
    in
    let propagate_all = propagate_all_pointer_comparison typ_arg in
    let args, alarms =
      if warn_pointer_comparison typ_arg then
        if propagate_all
        then `Value (v1, v2), snd (interpret_truth ~alarm (v1, v2) truth)
        else reduce_both_by_truth ~alarm (e1, v1) (e2, v2) truth
      else `Value (v1, v2), Alarmset.none
    in
    let result = let* v1, v2 = args in compute v1 v2 in
    let value =
      if is_true truth || not propagate_all
      then result
      else
        let zero = Value.inject_int typ_res Z.zero in
        let one = Value.inject_int typ_res Z.one in
        let zero_or_one = Value.join zero one in
        if Ast_types.is_ptr typ_arg then
          Self.result
            ~current:true ~once:true
            ~dkey:Self.dkey_pointer_comparison
            "evaluating condition to {0; 1} instead of %a because of %s"
            (Bottom.pretty pretty_cvalue) result
            Parameters.UndefinedPointerComparisonPropagateAll.name;
        `Value zero_or_one
    in
    value, alarms

  let forward_binop context ~typ_res (e1, v1 as arg1) op arg2 =
    let open Evaluated.Operators in
    let typ_arg = Ast_types.unroll e1.typ in
    match comparison_kind op with
    | Some kind ->
      let compute v1 v2 = Value.forward_binop context typ_arg op v1 v2 in
      (* Detect zero expressions created by the evaluator *)
      let e1 = if Eva_ast.is_zero_ptr e1 then None else Some e1 in
      forward_comparison ~compute ~typ_res ~typ_arg kind (e1, v1) arg2
    | None ->
      let& v1, v2 = assume_valid_binop context typ_arg arg1 op arg2 in
      Value.forward_binop context typ_arg op v1 v2

  let forward_unop context ~typ_res unop (e, v as arg) =
    let typ_arg = Ast_types.unroll e.typ in
    if unop = LNot then
      let kind = Abstract_value.Equality in
      let compute _ v = Value.forward_unop context typ_arg unop v in
      forward_comparison ~compute ~typ_res ~typ_arg kind (None, Value.zero) arg
    else Value.forward_unop context typ_arg unop v, Alarmset.none

  (* ------------------------------------------------------------------------
                                    Casts
     ------------------------------------------------------------------------ *)

  type integer_range = Eval_typ.integer_range = { i_bits: int; i_signed: bool }

  let cast_integer overflow_kind expr ~src ~dst value =
    let open Evaluated.Operators in
    let* value =
      if Eval_typ.(Z.lt (range_lower_bound src) (range_lower_bound dst))
      then truncate_lower_bound overflow_kind expr dst value
      else return value
    in
    if Eval_typ.(Z.gt (range_upper_bound src) (range_upper_bound dst))
    then truncate_upper_bound overflow_kind expr dst value
    else return value

  (* Relaxed semantics for downcasts into signed types:
     first converts the value to the signed counterpart of the source type, and
     then downcasts it into the signed destination type. Emits only alarms for
     the second cast. *)
  let relaxed_signed_downcast context expr ~src ~dst value =
    let expr, src, value =
      if not src.i_signed then
        let signed_src = { src with i_signed = true } in
        let signed_v = Value.rewrap_integer context signed_src value in
        let signed_typ = signed_counterpart expr.typ in
        let signed_exp = Eva_ast.Build.cast signed_typ expr in
        signed_exp, signed_src, signed_v
      else expr, src, value
    in
    cast_integer Alarms.Signed_downcast expr ~src ~dst value

  (* The type of alarms emitted for a conversion depends on whether the source
     type is an integer or a pointer. As a special case, conversions from a
     pointer type to "intptr_t" or "uintptr_t" never lead to alarms. *)
  type conversion_alarm = IntDowncast | PtrDowncast | NoAlarm

  let cast_int_to_int context expr ~alarm ~src ~dst value =
    (* Regain some precision in case a transfer function was imprecise.
       This should probably be done in the transfer function, though. *)
    let value =
      if Value.(equal top_int value)
      then Value.rewrap_integer context src value
      else value
    in
    if Eval_typ.range_inclusion src dst then
      return value (* Upcast, nothing to check. *)
    else
      let overflow_kind, warn =
        match alarm with
        | NoAlarm -> Alarms.Pointer_downcast, false
        | PtrDowncast -> Alarms.Pointer_downcast, Kernel.PointerDowncast.get ()
        | IntDowncast ->
          if dst.i_signed
          then Alarms.Signed_downcast, Kernel.SignedDowncast.get ()
          else Alarms.Unsigned_downcast, Kernel.UnsignedDowncast.get ()
      in
      if warn
      then cast_integer overflow_kind expr ~src ~dst value
      else if dst.i_signed && Parameters.WarnSignedConvertedDowncast.get ()
      then relaxed_signed_downcast context expr ~src ~dst value
      else return (Value.rewrap_integer context dst value)

  (* Re-export type here *)
  type scalar_typ = Eval_typ.scalar_typ =
    | TSInt of integer_range
    | TSPtr of integer_range
    | TSFloat of fkind

  let truncate_float_bound fkind bound bound_kind expr value =
    let next_int, prev_float, is_beyond = match bound_kind with
      | Alarms.Upper_bound -> Z.succ, Fval.F.prev_float, Z.geq
      | Alarms.Lower_bound -> Z.pred, Fval.F.next_float, Z.leq
    in
    let ibound = next_int bound in
    let fbound = Z.to_float ibound in
    let fbound = Floating_point.round_if_single_precision fkind fbound in
    let float_bound =
      if is_beyond (Z.of_float fbound) ibound
      then prev_float (Fval.kind fkind) fbound
      else fbound
    in
    let alarm () =
      Alarms.Float_to_int (Eva_ast.to_cil_exp expr, bound, bound_kind)
    in
    let bound = Abstract_value.Float (float_bound, fkind) in
    let truth = Value.assume_bounded bound_kind bound value in
    reduce_by_truth ~alarm (expr, value) truth

  let truncate_float fkind dst_range expr value =
    let open Evaluated.Operators in
    let max_bound = Eval_typ.range_upper_bound dst_range in
    let bound_kind = Alarms.Upper_bound in
    let* value = truncate_float_bound fkind max_bound bound_kind expr value in
    let min_bound = Eval_typ.range_lower_bound dst_range in
    let bound_kind = Alarms.Lower_bound in
    truncate_float_bound fkind min_bound bound_kind expr value

  let forward_cast context ~dst_typ expr value =
    let open Evaluated.Operators in
    let src_typ = expr.typ in
    match Eval_typ.(classify_as_scalar src_typ, classify_as_scalar dst_typ) with
    | None, _ | _, None -> return value (* Unclear whether this happens. *)
    | Some src_type, Some dst_type ->
      let& value =
        match src_type, dst_type with
        | TSPtr src, TSInt dst ->
          let alarm =
            if Ast_types.is_intptr_t dst_typ || Ast_types.is_uintptr_t dst_typ
            then NoAlarm
            else PtrDowncast
          in
          cast_int_to_int context ~alarm ~src ~dst expr value
        | TSInt src, (TSInt dst | TSPtr dst) ->
          cast_int_to_int context ~alarm:IntDowncast ~src ~dst expr value
        | TSFloat src, (TSInt dst | TSPtr dst)  ->
          let reduce = true and assume_finite = true in
          let* value = restrict_float ~reduce ~assume_finite expr src value in
          truncate_float src dst expr value
        | (TSInt _ | TSPtr _), TSFloat _ ->
          (* Cannot overflow with 32 bits float. *)
          return value
        | TSFloat _, TSFloat _ | TSPtr _, TSPtr _ -> return value
      in
      Value.forward_cast context ~src_type ~dst_type value

  (* ------------------------------------------------------------------------
                               Forward Evaluation
     ------------------------------------------------------------------------ *)

  (* The forward evaluation environment: arguments that must be passed through
     the mutually recursive evaluation functions without being modified. *)
  type recursive_environment =
    { (* The abstract domain state in which the evaluation takes place. *)
      state: Domain.t;
      (* The abstract context in which the evaluation takes place. *)
      context: Context.t Abstract_value.enriched;
      (* Is the expression currently processed the "root" expression being
         evaluated, or is it a sub-expression? Useful for domain queries. *)
      root: bool;
      (* Maximum number of subdivisions. See {!Subdivided_evaluation} for
         more details. *)
      subdivision: int;
      (* Is the current evaluation subdivided? *)
      subdivided: bool;
      (* The remaining fuel: maximum number of nested oracle uses, decremented
         at each call to the oracle. *)
      remaining_fuel: int;
      (* The oracle which can be used by abstract domains to get a value for
         some expressions. *)
      oracle: recursive_environment -> exp -> Value.t evaluated;
    }

  (* Builds the query to the domain from the environment. *)
  let make_domain_query query env =
    let { state; oracle; root; subdivision; subdivided; } = env in
    let oracle = oracle env in
    let domain_env = Abstract_domain.{ root; subdivision; subdivided; } in
    query ~oracle domain_env state

  (* Returns the cached value and alarms for the evaluation if it exists;
     call [coop_forward_eval] and caches its result otherwise.
     Also returns a boolean indicating whether the expression is volatile.  *)
  let rec root_forward_eval env expr =
    (* Search in the cache for the result of a previous computation. *)
    try
      let record, report = Cache.find' !cache expr in
      (* If the record was computed with more fuel than [fuel], return it. *)
      if report.fuel = Loop then fuel_consumed := true;
      if less_fuel_than env.remaining_fuel report.fuel
      then (let+ v = record.value.v in v, report.volatile), record.val_alarms
      else raise Not_found
    (* If no result found, evaluate the expression. *)
    with Not_found ->
      let previous_fuel_consumed = !fuel_consumed in
      (* Fuel not consumed for this new evaluation. *)
      fuel_consumed := false;
      (* Fill the cache to avoid loops in the use of the oracle. *)
      cache := Cache.add' !cache expr top_entry;
      (* Evaluation of [expr]. *)
      let result, alarms = coop_forward_eval env expr in
      let value =
        let* record, reduction, volatile = result in
        (* Put the alarms in the record. *)
        let record = { record with val_alarms = alarms } in
        (* Inter-reduction of the value (in case of a reduced product). *)
        let record = reduce_value record in
        (* Cache the computed result with an appropriate report. *)
        let fuel = env.remaining_fuel in
        let fuel = if !fuel_consumed then Finite fuel else Infty in
        let report = {fuel; reduction; volatile} in
        cache := Cache.add' !cache expr (record, report);
        let+ v = record.value.v in v, volatile
      in
      (* Reset the flag fuel_consumed. *)
      fuel_consumed := previous_fuel_consumed || !fuel_consumed;
      value, alarms

  and forward_eval env expr = root_forward_eval env expr >>=: fst

  (* The functions below returns, along with the computed value (when it is not
     bottom):
     - the state of reduction of the current expression: Neither if it has
       not been reduced, Forward otherwise.
     - a boolean indicating whether the expression is volatile. *)

  (* Asks the abstract domain for abstractions (value and alarms) of [expr],
     and performs the narrowing with the abstractions computed by
     [internal_forward_eval].  *)
  and coop_forward_eval env expr =
    match expr.node with
    | Lval lval -> eval_lval env lval
    | BinOp _ | UnOp _ | CastE _ ->
      let domain_query = make_domain_query Domain.extract_expr env in
      let env = { env with root = false } in
      let intern_value, alarms = internal_forward_eval env expr in
      let domain_value, alarms' = domain_query expr in
      (* Intersection of alarms, as each sets of alarms are correct
         and "complete" for the evaluation of [expr]. *)
      begin match Alarmset.inter alarms alarms' with
        | `Inconsistent ->
          (* May happen for a product of states with no concretization. Such
             cases are reported to the user by transfer_stmt. *)
          `Bottom, Alarmset.none
        | `Value alarms ->
          let v =
            let* intern_value, reduction, volatile = intern_value in
            let* domain_value, origin = domain_value in
            let+ result = Value.narrow intern_value domain_value in
            let reductness =
              if Value.equal domain_value result then Unreduced
              else if Value.(equal domain_value top) then Created else Reduced
            in
            let reduction =
              update_reduction reduction (Value.equal intern_value result)
            and value = define_value result in
            (* The proper alarms will be set in the record by forward_eval. *)
            {value; origin; reductness; val_alarms = Alarmset.all},
            reduction, volatile
          in
          v, alarms
      end
    | _ ->
      let open Evaluated.Operators in
      let+ value, reduction, volatile = internal_forward_eval env expr in
      let value = define_value value and origin = None in
      let reductness = Dull and val_alarms = Alarmset.all in
      { value; origin; reductness; val_alarms }, reduction, volatile

  (* Recursive descent in the sub-expressions. *)
  and internal_forward_eval env expr =
    let open Evaluated.Operators in
    let compute_reduction (v, a) volatile =
      let+ v = (v, a) in
      let reduction = if Alarmset.is_empty a then Neither else Forward in
      v, reduction, volatile
    in
    match expr.node with
    | Const constant -> internal_forward_eval_constant env expr constant
    | Lval _lval -> assert false
    | AddrOf lval | StartOf lval ->
      let* loc, _ = lval_to_loc env ~for_writing:false ~reduction:false lval in
      let* value = Loc.to_value loc, Alarmset.none in
      let v = assume_pointer env.context expr value in
      let v =
        (* A variable is always aligned with its type. However, in case of a
           packed struct, a field might be unaligned with its own type. *)
        if is_packed_struct_field (snd lval.node)
        then let* v in assume_aligned expr expr.typ v
        else v
      in
      compute_reduction v false

    | UnOp (op, e, typ_res) ->
      let* v, volatile = root_forward_eval env e in
      let* v = forward_unop env.context ~typ_res op (e, v) in
      let may_overflow = op = Neg in
      let v = handle_overflow ~may_overflow env.context expr typ_res v in
      compute_reduction v volatile

    | BinOp (op, e1, e2, typ_res) ->
      let* v1, volatile1 = root_forward_eval env e1 in
      let* v2, volatile2 = root_forward_eval env e2 in
      let* v = forward_binop env.context ~typ_res (e1, v1) op (e2, v2) in
      let may_overflow = may_overflow op in
      let v = handle_overflow ~may_overflow env.context expr typ_res v in
      compute_reduction v (volatile1 || volatile2)

    | CastE (dst_typ, e) ->
      let* value, volatile = root_forward_eval env e in
      let v = forward_cast env.context ~dst_typ e value in
      let v = match Ast_types.unroll_node dst_typ with
        | TFloat fkind -> let* v in remove_special_float expr fkind v
        | TPtr _ ->
          let* v in
          let* v = assume_pointer env.context expr v in
          assume_aligned e dst_typ v
        | _ -> v
      in
      compute_reduction v volatile

  and internal_forward_eval_constant env expr constant =
    let open Evaluated.Operators in
    let+ value =
      match constant with
      | CEnum (_, e) ->
        forward_eval env e
      | CReal (_f, fkind, _fstring) ->
        let value = Value.constant env.context expr constant in
        remove_special_float expr fkind value
      (* Integer constants never overflow, because the front-end chooses a
         suitable type. *)
      | _ -> return (Value.constant env.context expr constant)
    in
    value, Neither, false


  (* ------------------------------------------------------------------------
                                Lvalue evaluation
     ------------------------------------------------------------------------ *)

  (* Calls the internal evaluation of an lvalue to a location, and stores the
     result in the cache. If the result is already in the cache, the computation
     is avoided, unless if it may reduce the cache.
     If [reduction] is false, don't reduce the location and the offset by their
     valid parts, and don't emit alarms about their validity.
     If the location is not bottom, the function also returns the typ of the
     lvalue, and a boolean indicating that the lvalue contains a sub-expression
     with volatile qualifier (in its host or offset). *)
  and lval_to_loc env ~for_writing ~reduction lval =
    let compute () =
      let res, alarms = reduced_lval_to_loc env ~for_writing ~reduction lval in
      let res =
        let+ loc, red, volatile = res in
        let fuel = env.remaining_fuel in
        let record = { loc; loc_alarms = alarms }
        and report = { fuel = Finite fuel; reduction = red; volatile }
        and loc_report = { for_writing; with_reduction = reduction } in
        cache := Cache.add_loc' !cache lval (record, (report, loc_report));
        (loc, volatile)
      in
      res, alarms
    in
    let already_precise = already_precise_loc_report ~for_writing ~reduction in
    let not_enough_fuel = less_fuel_than env.remaining_fuel in
    match Cache.find_loc' !cache lval with
    | `Value (record, (report, loc_report)) ->
      if already_precise loc_report && not_enough_fuel report.fuel
      then `Value (record.loc, report.volatile), record.loc_alarms
      else compute ()
    | `Top -> compute ()

  (* If [reduction] is false, don't reduce the location and the offset by their
     valid parts, and don't emit alarms about their validity. *)
  and reduced_lval_to_loc env ~for_writing ~reduction lval =
    let open Evaluated.Operators in
    let lval_to_loc = internal_lval_to_loc env ~for_writing ~reduction in
    let* loc, volatile = lval_to_loc lval in
    if reduction then
      let bitfield = Eva_ast.is_bitfield lval in
      let truth = Loc.assume_valid_location ~for_writing ~bitfield loc in
      let access = Alarms.(if for_writing then For_writing else For_reading) in
      let alarm () = Alarms.Memory_access (Eva_ast.to_cil_lval lval, access) in
      let+ valid_loc = interpret_truth ~alarm loc truth in
      let reduction = if Loc.equal_loc valid_loc loc then Neither else Forward in
      valid_loc, reduction, volatile
    else `Value (loc, Neither, volatile), Alarmset.none

  (* Internal evaluation of a lvalue to an abstract location.
     Combination of the evaluation of the right part of an lval (an host) with
     an offset, to obtain a location *)
  and internal_lval_to_loc env ~for_writing ~reduction lval =
    let open Evaluated.Operators in
    let host, offset = lval.node in
    let basetyp = Eva_ast.type_of_lhost host in
    let reduce_valid_index = reduction in
    let evaluated = eval_offset env ~reduce_valid_index basetyp offset in
    let* (offs, offset_volatile) = evaluated in
    if for_writing && Eva_utils.is_const_write_invalid lval.typ then
      let cil_lval = Eva_ast.to_cil_lval lval in
      let alarm = Alarms.(Memory_access (cil_lval, For_writing)) in
      `Bottom, Alarmset.singleton ~status:Alarmset.False alarm
    else
      let+ loc, host_volatile = eval_host env lval.typ offs host in
      loc, offset_volatile || host_volatile

  (* Host evaluation. Also returns a boolean which is true if the host
     contains a volatile sub-expression. *)
  and eval_host env typ_offset offs = function
    | Var host ->
      let loc = Loc.forward_variable typ_offset host offs in
      (let+ loc in loc, false), Alarmset.none
    | Mem x ->
      let open Evaluated.Operators in
      let& loc_lv, volatile = root_forward_eval env x in
      let open Bottom.Operators in
      let+ loc = Loc.forward_pointer typ_offset loc_lv offs in
      loc, volatile

  (* Offset evaluation. Also returns a boolean which is true if the offset
     contains a volatile sub-expression. *)
  and eval_offset env ~reduce_valid_index typ = function
    | NoOffset -> return (Loc.no_offset, false)
    | Index (index_expr, remaining) ->
      let open Evaluated.Operators in
      let typ_pointed, array_size =
        match Ast_types.unroll typ with
        | { tnode = TArray (t, size) } -> t, size
        | t -> Self.fatal ~current:true "Got type '%a'" Printer.pp_typ t
      in
      let eval = eval_offset env ~reduce_valid_index typ_pointed remaining in
      let* roffset, remaining_volatile = eval in
      let* index, volatile = root_forward_eval env index_expr in
      let valid_index =
        if not (Kernel.SafeArrays.get ()) || not reduce_valid_index
        then `Value index, Alarmset.none
        else
          try
            (* If possible, reduce the index value by the array size. *)
            let size = Cil.lenOfArray64 array_size in
            (* Handle the special GCCism of zero-sized arrays: Frama-C
               pretends their size is unknown, exactly like GCC. *)
            if Z.is_zero size
            then `Value index, Alarmset.none
            else
              let size_expr = Option.get array_size in (* array_size exists *)
              assume_valid_index ~size ~size_expr ~index_expr index
          with Cil.LenOfArray _ -> `Value index, Alarmset.none (* unknown array size *)
      in
      let+ index = valid_index in
      Loc.forward_index typ_pointed index roffset,
      remaining_volatile || volatile
    | Field (fi, remaining) ->
      let open Evaluated.Operators in
      let attrs = Ast_types.get_attributes typ in
      let attrs = Ast_attributes.filter_qualifiers attrs in
      let typ_fi = Ast_types.add_attributes attrs fi.ftype in
      let evaluated = eval_offset env ~reduce_valid_index typ_fi remaining in
      let+ r, volatile = evaluated in
      let off = Loc.forward_field typ fi r in
      off, volatile

  and eval_lval ?(indeterminate=false) env lval =
    let open Evaluated.Operators in
    let domain_query = make_domain_query Domain.extract_lval env in
    let env = { env with root = false } in
    (* Computes the location of [lval]. *)
    let evaluated = lval_to_loc env ~for_writing:false ~reduction:true lval in
    let* loc, volatile_expr = evaluated in
    (* the lvalue is volatile:
       - if it has qualifier volatile (lval_to_loc propagates qualifiers
         in the proper way through offsets)
       - if it contains a sub-expression which is volatile (volatile_expr)
    *)
    let volatile = volatile_expr || Ast_types.has_qualifier "volatile" lval.typ in
    let cil_lval = Eva_ast.to_cil_lval lval in
    (* Find the value of the location, if not bottom. *)
    let v, alarms = domain_query lval loc in
    let alarms = close_dereference_alarms cil_lval alarms in
    if indeterminate then
      let record, alarms = indeterminate_copy cil_lval v alarms in
      `Value (record, Neither, volatile), alarms
    else
      let v, alarms = assume_valid_value env.context lval (v, alarms) in
      let+ value, origin = v, alarms in
      let value = define_value value in
      let reductness, reduction =
        if Alarmset.is_empty alarms then Unreduced, Neither else Reduced, Forward
      in
      (* The proper alarms will be set in the record by forward_eval. *)
      { value; origin; reductness; val_alarms = Alarmset.all },
      reduction, volatile

  (* ------------------------------------------------------------------------
                       Subdivided Forward Evaluation
     ------------------------------------------------------------------------ *)

  (* These two modules could be implemented as mutually recursive, to avoid
     the reference for the oracle given to the domains. *)
  module Forward_Evaluation = struct
    type environment = recursive_environment
    let evaluate ~subdivided environment valuation expr =
      let open Evaluated.Operators in
      cache := valuation;
      let root = not subdivided && environment.root in
      let subdivided = subdivided || environment.subdivided in
      let environment = { environment with root ; subdivided } in
      let+ value, _ = root_forward_eval environment expr in
      !cache, value
  end

  module Subdivided_Evaluation =
    Subdivided_evaluation.Make (Value) (Loc) (Cache) (Forward_Evaluation)

  let oracle env =
    let remaining_fuel = pred env.remaining_fuel in
    if remaining_fuel > 0 then
      fun expr ->
        let valuation = !cache in
        let env = { env with remaining_fuel } in
        let subdivnb = env.subdivision in
        let evaluate = Subdivided_Evaluation.evaluate env valuation in
        let eval, alarms = evaluate ~subdivnb expr in
        (* Always reset the reference to the cached valuation after a subdivided
           evaluation (as the multiple evaluations modify the cache). *)
        match eval with
        | `Bottom ->
          (* The evaluation of [expr] requested by a domain led to Bottom,
             but the evaluation of the primary expression may continue. *)
          cache := valuation;
          `Bottom, alarms
        | `Value (new_valuation, value) ->
          (* If the evaluation of [expr] produced any alarms, the resulting
             values may have been reduced accordingly, but these alarms will not
             be emitted (as [expr] might be unrelated to the current statement).
             In this case, do not record the evaluation results in the cache:
             it would be correct as a cache for further evaluations, but not as
             a result for the domains, as they could use the reduced values
             while the related alarms have not been emitted. *)
          let valuation =
            if Alarmset.is_empty alarms then new_valuation else valuation
          in
          cache := valuation;
          `Value value, alarms
    else fun _ -> fuel_consumed := true; `Value Value.top, Alarmset.all

  (* Context from state [state]. *)
  let get_context state =
    let+ from_domains = Domain.build_context state in
    Abstract_value.{ from_domains }

  (* Environment for the forward evaluation of a root expression in state [state]
     with maximal precision. *)
  let root_environment ?subdivnb state =
    let+ context = get_context state in
    let subdivided = false and root = true in
    let remaining_fuel = root_fuel () in
    (* By default, use the number of subdivision defined by the global option
       -eva-subdivide-non-linear. *)
    let default () =  Parameters.SubdivideNonLinear.get () in
    let subdivision = match subdivnb with None -> default () | Some n -> n in
    { state; context; root; subdivision; subdivided; remaining_fuel; oracle }

  (* Environment for a fast forward evaluation with minimal precision:
     no subdivisions, no calls to the oracle, and the expression is not
     considered as a "root" expression. *)
  let fast_eval_environment state =
    let+ context = get_context state in
    let remaining_fuel = no_fuel and root = false in
    let subdivision = 0 and subdivided = false in
    { state; context; root; subdivision; subdivided; remaining_fuel; oracle }

  let subdivided_forward_eval valuation ?subdivnb state expr =
    let open Evaluated.Operators in
    let* env = root_environment ?subdivnb state, Alarmset.none in
    let subdivnb = env.subdivision in
    Subdivided_Evaluation.evaluate env valuation ~subdivnb expr

  (* ------------------------------------------------------------------------
                           Backward Evaluation
     ------------------------------------------------------------------------ *)

  (* Find the value of a previously evaluated expression. *)
  let find_val expr =
    match Cache.find !cache expr with
    | `Value record -> record.value.v
    | `Top -> assert false (* [expr] must have been evaluated already. *)

  (* Find the record computed for an lvalue.
     Return None if no reduction can be performed. *)
  let find_loc_for_reduction lval =
    if may_be_reduced_lval lval then
      let record, report =
        match Cache.find_loc' !cache lval with
        | `Value all -> all
        | `Top -> assert false
      in
      if (snd report).with_reduction then Some (record, report) else None
    else None

  (* Evaluate an expression before any reduction, if needed. Also return the
     report indicating if a forward reduction during the forward evaluation may
     be propagated backward. *)
  let evaluate_for_reduction state expr =
    try `Value (Cache.find' !cache expr)
    with Not_found ->
      let* env = fast_eval_environment state in
      let+ _ = forward_eval env expr |> fst in
      try Cache.find' !cache expr
      with Not_found -> assert false

  (* The backward propagation at a step is relevant only if:
     - the new value (if any) is more precise than the old one.
       Then the latter is reduced by the former, and the reduction kind is set
       to [Backward].
     - or the old value has been reduced during the forward evaluation.
       Then, [report.reduced] is [Forward], and must be set to [Neither] as
       the reduction is propagated but the value of the current expression is
       unchanged. *)
  let backward_reduction old_value latter_reduction value =
    let forward = latter_reduction = Forward in
    let neither () = Some (old_value, Neither) in
    let propagate_forward_reduction () = if forward then neither () else None in
    match value with
    | None -> `Value (propagate_forward_reduction ())
    | Some new_value ->
      let+ value = Value.narrow old_value new_value in
      if Value.is_included old_value value
      then propagate_forward_reduction ()
      else Some (value, Backward)

  (* [backward_eval state expr value] reduces the the expression [expr] and
     its subterms in the cache, according to the state [state]:
     - the reductions performed during the forward evaluation (due to alarms or
       abstract domains) are propagated backward to the subexpressions;
     - if [value = Some v], then [expr] is assumed to evaluate to [v] (and is
       reduced accordingly). *)
  let rec backward_eval fuel context state expr value =
    (* Evaluate the expression if needed. *)
    let* record, report = evaluate_for_reduction state expr in
    (* Reduction of [expr] by [value]. Also performs further reductions
       requested by the domains. Returns Bottom if one of these reductions
       leads to bottom. *)
    let reduce kind value =
      let continue = `Value () in
      (* Avoids reduction of volatile expressions. *)
      if not report.volatile then
        let value = Value.reduce value in
        reduce_expr_recording kind expr (record, report) value ;
        (* If enough fuel, asks the domain for more reductions. *)
        if fuel > 0 then
          (* The reductions requested by the domains. *)
          let reductions_list = Domain.reduce_further state expr value in
          (* Reduces [expr] to value [v]. *)
          let reduce acc (expr, value) =
            (* If a previous reduction has returned bottom, return bottom. *)
            let* () = acc in
            backward_eval (pred fuel) context state expr (Some value)
          in
          List.fold_left reduce continue reductions_list
        else continue
      else continue
    in
    let* old_value = record.value.v in
    (* Determines the need of a backward reduction. *)
    let* reduced = backward_reduction old_value report.reduction value in
    match reduced with
    | None ->
      (* If no reduction to be propagated, just visit the subterms. *)
      recursive_descent fuel context state expr
    | Some (value, kind) ->
      (* Otherwise, backward propagation to the subterms. *)
      match expr.node with
      | Lval lval ->
        begin
          (* For a lvalue, we try to reduce its location according to the value;
             this operation may lead to a more precise value for this lvalue,
             which is then reduced accordingly. *)
          let* reduced_loc = backward_loc state lval value in
          match reduced_loc with
          | None ->
            let* () = reduce kind value in
            recursive_descent_lval fuel context state lval
          | Some (loc, new_value) ->
            let included = Value.is_included old_value new_value in
            let kind = if included then Neither else Backward in
            let* () = reduce kind new_value in
            internal_backward_lval fuel context state loc lval
        end
      | _ ->
        let* () = reduce kind value in
        internal_backward fuel context state expr value

  (* Backward propagate the reduction [expr] = [value] to the subterms of the
     compound expression [expr]. *)
  and internal_backward fuel context state expr value =
    match expr.node with
    | Lval _lv -> assert false
    | UnOp (LNot, e, _) ->
      let cond = Eva_ast.normalize_condition e false in
      (* TODO: should we compute the meet with the result of the call to
         Value.backward_unop? *)
      backward_eval fuel context state cond (Some value)
    | UnOp (op, e, _typ) ->
      let typ_arg = Ast_types.unroll e.typ in
      let* arg = find_val e in
      let* v = Value.backward_unop context ~typ_arg op ~arg ~res:value in
      backward_eval fuel context state e v
    | BinOp (binop, e1, e2, typ) ->
      let resulting_type = Ast_types.unroll typ in
      let input_type = e1.typ in
      let* left = find_val e1
      and* right = find_val e2 in
      let backward = Value.backward_binop context ~input_type ~resulting_type in
      let* v1, v2 = backward binop ~left ~right ~result:value in
      let* () = backward_eval fuel context state e1 v1 in
      backward_eval fuel context state e2 v2
    | CastE (typ, e) ->
      begin
        let dst_typ = Ast_types.unroll typ in
        let src_typ = Ast_types.unroll e.typ in
        let* src_val = find_val e in
        let backward = Value.backward_cast context ~src_typ ~dst_typ in
        let* v = backward ~src_val ~dst_val:value in
        backward_eval fuel context state e v
      end
    | _ -> `Value ()

  and recursive_descent fuel context state expr =
    match expr.node with
    | Lval lval -> backward_lval fuel context state lval
    | UnOp (_, e, _)
    | CastE (_, e) -> backward_eval fuel context state e None
    | BinOp (_binop, e1, e2, _typ) ->
      let* () = backward_eval fuel context state e1 None in
      backward_eval fuel context state e2 None
    | _ -> `Value ()

  and recursive_descent_lval fuel context state lval =
    let (host, offset) = lval.node in
    let* () = recursive_descent_host fuel context state host in
    recursive_descent_offset fuel context state offset

  and recursive_descent_host fuel context state = function
    | Var _ -> `Value ()
    | Mem expr -> backward_eval fuel context state expr None

  and recursive_descent_offset fuel context state = function
    | NoOffset               -> `Value ()
    | Field (_, remaining)   ->
      recursive_descent_offset fuel context state remaining
    | Index (exp, remaining) ->
      let* _ = backward_eval fuel context state exp None in
      recursive_descent_offset fuel context state remaining

  (* Even if the value of an lvalue has not been reduced, its memory location
     could have been, and this can be propagated backward. Otherwise, continue
     the recursive descent. *)
  and backward_lval fuel context state lval =
    match find_loc_for_reduction lval with
    | None -> recursive_descent_lval fuel context state lval
    | Some (record, report) ->
      if (fst report).reduction = Forward
      then internal_backward_lval fuel context state record.loc lval
      else recursive_descent_lval fuel context state lval

  (* [backward_loc state lval value] tries to reduce the memory location of the
     lvalue [lval] according to its value [value] in the state [state]. *)
  and backward_loc state lval value =
    match find_loc_for_reduction lval with
    | None -> `Value None
    | Some (record, report) ->
      let* loc, new_value =
        Domain.backward_location state lval record.loc value
      in
      let+ value = Value.narrow new_value value in
      let b = not (Loc.equal_loc record.loc loc) in
      (* Avoids useless reductions and reductions of volatile expressions. *)
      if b && not (fst report).volatile then
        let record = { record with loc } in
        let report = { (fst report) with reduction = Backward }, snd report in
        cache := Cache.add_loc' !cache lval (record, report);
      else ();
      if b || (fst report).reduction = Forward
      then Some (loc, value)
      else None

  and internal_backward_lval fuel context state location lval =
    match lval.node with
    | Var host, offset ->
      let* loc_offset = Loc.backward_variable host location in
      backward_offset fuel context state host.vtype offset loc_offset
    | Mem expr, offset ->
      match offset with
      | NoOffset ->
        let* loc_value = Loc.to_value location in
        backward_eval fuel context state expr (Some loc_value)
      | _ ->
        let reduce_valid_index = true in
        let typ_lval = Ast_types.direct_pointed_type expr.typ in
        let* env = fast_eval_environment state in
        let eval = eval_offset env ~reduce_valid_index typ_lval offset in
        let* loc_offset, _ = fst eval in
        let* value = find_val expr in
        let pointer = Loc.backward_pointer value loc_offset location in
        let* pointer_value, loc_offset = pointer in
        let* () = backward_eval fuel context state expr (Some pointer_value) in
        backward_offset fuel context state typ_lval offset loc_offset

  and backward_offset fuel context state typ offset loc_offset =
    match offset with
    | NoOffset -> `Value ()
    | Field (field, remaining)  ->
      let* rem = Loc.backward_field typ field loc_offset in
      backward_offset fuel context state field.ftype remaining rem
    | Index (exp, remaining) ->
      let* v = find_val exp in
      let typ_pointed = Ast_types.direct_element_type typ in
      let* env = fast_eval_environment state in
      let* rem, _ =
        eval_offset env ~reduce_valid_index:true typ_pointed remaining |> fst
      in
      let* v', rem' =
        Loc.backward_index ~index:v ~remaining:rem typ_pointed loc_offset
      in
      let reduced_v = if Value.is_included v v' then None else Some v' in
      let* () = backward_eval fuel context state exp reduced_v in
      backward_offset fuel context state typ_pointed remaining rem'


  (* ------------------------------------------------------------------------
                       Second Pass of Forward Evaluation
     ------------------------------------------------------------------------ *)

  exception Not_Exact_Reduction

  (** Second forward evaluation after a backward propagation for the condition
      of an if statement.
      Allows to forward propagate the backward reductions. Uses the internal
      forward functions to actually perform the computation instead of relying
      on the cache. However, the internal evaluation uses the cache for the sub-
      expressions, so this evaluation is still bottom-up, and update the cache
      progressively.
      Stops the descent as soon as there is no backward propagation to recover
      for an expression. However, more backward reduction could have been done
      below for other reasons (due to alarms or domains).
      Raises Not_Exact_Reduction if at any point, the forward evaluation leads
      to a less precise value than the one stored after the backward evaluation.
      This means that the backward propagation has not been precise enough. *)
  let rec second_forward_eval state expr =
    let find e = try Cache.find' !cache e with Not_found -> assert false in
    let record, report = find expr in
    if report.reduction == Backward then
      let* value = record.value.v in
      let* () = recursive_descent state expr in
      let new_value =
        match expr.node with
        | Lval lval -> second_eval_lval state lval value
        | _ ->
          let* env = fast_eval_environment state in
          let+ v, _, _ = fst (internal_forward_eval env expr) in v
      in
      let* evaled = new_value in
      let evaled = Value.reduce evaled in
      let+ new_value = Value.narrow value evaled in
      if Value.is_included evaled value then
        let kind = if Value.equal value new_value then Neither else Forward in
        reduce_expr_value kind expr new_value
      else raise Not_Exact_Reduction
    else `Value ()

  and second_eval_lval state lval value =
    if may_be_reduced_lval lval then
      let record, report =
        match Cache.find_loc' !cache lval with
        | `Value all -> all
        | `Top -> assert false
      in
      let* env = fast_eval_environment state in
      let* () =
        if (fst report).reduction = Backward then
          let for_writing = false and reduction = true in
          let+ loc, _, _ =
            reduced_lval_to_loc ~for_writing ~reduction env lval |> fst
          in
          (* TODO: Loc.narrow *)
          let record = { record with loc } in
          let in_record loc = Loc.equal_loc record.loc loc in
          let reduction = if in_record loc then Neither else Forward in
          let report = { (fst report) with reduction }, snd report in
          cache := Cache.add_loc' !cache lval (record, report);
        else `Value ()
      in
      let* record, _, _ = eval_lval env lval |> fst in
      record.value.v
    else `Value value

  and recursive_descent state expr =
    match expr.node with
    | Lval lval -> recursive_descent_lval state lval
    | UnOp (_, e, _)
    | CastE (_, e) -> second_forward_eval state e
    | BinOp (_, e1, e2, _) ->
      let* () = second_forward_eval state e1 in
      second_forward_eval state e2
    | _ -> `Value ()

  and recursive_descent_lval state lval =
    let (host, offset) = lval.node in
    let* () = recursive_descent_host state host in
    recursive_descent_offset state offset

  and recursive_descent_host state = function
    | Var _ -> `Value ()
    | Mem expr -> second_forward_eval state expr

  and recursive_descent_offset state = function
    | NoOffset               -> `Value ()
    | Field (_, remaining)   -> recursive_descent_offset state remaining
    | Index (exp, remaining) ->
      let* () = second_forward_eval state exp in
      recursive_descent_offset state remaining

  (* ------------------------------------------------------------------------
                              Generic Interface
     ------------------------------------------------------------------------ *)

  module Valuation = Cache

  let to_domain_valuation valuation =
    let find = Valuation.find valuation in
    let fold f acc = Valuation.fold f valuation acc in
    let find_loc = Valuation.find_loc valuation in
    let find_loc_def = Valuation.find_loc_def valuation in
    Abstract_domain.{ find ; fold ; find_loc ; find_loc_def }

  let evaluate ?(valuation=Cache.empty) ?(reduction=true) ?subdivnb state expr =
    let eval, alarms = subdivided_forward_eval valuation ?subdivnb state expr in
    let result =
      if reduction && not (Alarmset.is_empty alarms) then
        let open Bottom.Operators in
        let* valuation, value = eval in
        cache := valuation;
        let fuel = backward_fuel () in
        let* context = get_context state in
        let+ () = backward_eval fuel context state expr None in
        !cache, value
      else eval
    in
    result, alarms

  let copy_lvalue ?(valuation=Cache.empty) ?subdivnb state lval =
    let open Evaluated.Operators in
    let expr = Eva_ast.Build.lval lval in
    let* env = root_environment ?subdivnb state, Alarmset.none in
    try
      let record, report = Cache.find' valuation expr in
      if less_fuel_than env.remaining_fuel report.fuel
      then `Value (valuation, record.value), record.val_alarms
      else raise Not_found
    with Not_found ->
      cache := valuation;
      let+ record, _, volatile = eval_lval env ~indeterminate:true lval in
      let record = reduce_value record in
      (* Cache the computed result with an appropriate report. *)
      let fuel = Finite (root_fuel ()) in
      let report = { fuel; reduction = Neither; volatile } in
      let valuation = Cache.add' !cache expr (record, report) in
      valuation, record.value

  (* When evaluating an lvalue, we use the subdivided evaluation for the
     expressions included in the lvalue. *)
  let rec evaluate_offsets valuation ?subdivnb state = function
    | NoOffset             -> `Value valuation, Alarmset.none
    | Field (_, offset)    -> evaluate_offsets valuation ?subdivnb state offset
    | Index (expr, offset) ->
      let open Evaluated.Operators in
      let* valuation, _ =
        subdivided_forward_eval valuation ?subdivnb state expr
      in
      evaluate_offsets valuation ?subdivnb state offset

  let evaluate_host valuation ?subdivnb state = function
    | Var _ -> `Value valuation, Alarmset.none
    | Mem e -> subdivided_forward_eval valuation ?subdivnb state e >>=: fst

  let lvaluate ?(valuation=Cache.empty) ?subdivnb ~for_writing state lval =
    let open Evaluated.Operators in
    (* If [for_writing] is true, the location of [lval] is reduced by removing
       const bases. Use [for_writing:false] if const bases can be written
       through a mutable field or an initializing function. *)
    let mutable_or_init = Eva_ast.(is_mutable lval || is_initialized lval) in
    let for_writing = for_writing && not mutable_or_init in
    let (host, offset) = lval.node in
    let* valuation = evaluate_host valuation ?subdivnb state host in
    let* valuation = evaluate_offsets valuation ?subdivnb state offset in
    cache := valuation;
    let* env = root_environment ?subdivnb state, Alarmset.none in
    let& _ = lval_to_loc env ~for_writing ~reduction:true lval in
    let open Bottom.Operators in
    let+ () = backward_lval (backward_fuel ()) env.context state lval in
    match Cache.find_loc !cache lval with
    | `Value record -> !cache, record.loc
    | `Top -> assert false

  let reduce ?valuation:(valuation=Cache.empty) state expr positive =
    let open Evaluated.Operators in
    (* Generate [e == 0] *)
    let expr = Eva_ast.normalize_condition expr (not positive) in
    cache := valuation;
    (* Currently, no subdivisions are performed during the forward evaluation
       in this function, which is used to evaluate the conditions of if(…)
       statements in the analysis. *)
    let* env = root_environment ~subdivnb:0 state, Alarmset.none in
    let& _, volatile = root_forward_eval env expr in
    let open Bottom.Operators in
    (* Reduce by [(e == 0) == 0] *)
    let fuel = backward_fuel () in
    let* () = backward_eval fuel env.context state expr (Some Value.zero) in
    try let+ () = second_forward_eval state expr in !cache
    with Not_Exact_Reduction ->
      (* Avoids reduce_by_enumeration on volatile expressions. *)
      if not volatile then
        let* env = fast_eval_environment state in
        Subdivided_Evaluation.reduce_by_enumeration env !cache expr false
      else `Value !cache

  let assume ?valuation:(valuation=Cache.empty) state expr value =
    cache := valuation;
    let fuel = backward_fuel () in
    let* context = get_context state in
    let+ () = backward_eval fuel context state expr (Some value) in
    !cache


  (* ------------------------------------------------------------------------
                                      Misc
     ------------------------------------------------------------------------ *)

  (* For pointer calls, we retro-propagate which function is being called
     in the abstract state. This may be useful:
     - inside the call for languages with OO (think 'self')
     - everywhere, because we may remove invalid values for the pointer
     - after if enough slevel is available, as states obtained in
       different functions are not merged by default. *)
  let backward_function_pointer valuation state expr kf =
    (* Builds the expression [exp_f != &f], and assumes it is false. *)
    let vi_f = Kernel_function.get_vi kf in
    let expr = Eva_ast.Build.(ne expr (var_addr vi_f)) in
    fst (reduce ~valuation state expr false)

  let eval_function ?subdivnb func ?args state =
    match func with
    | Var vinfo ->
      `Value [Globals.Functions.get vinfo, Valuation.empty], Alarmset.none
    | Mem v ->
      match evaluate ?subdivnb state v with
      | `Bottom, alarms -> `Bottom, alarms
      | `Value (valuation, value), alarms ->
        match Value.resolve_functions value with
        | `Top, _ -> `Top, Alarmset.all
        | `Value kfs, alarm ->
          let open Bottom.Operators in
          let args_types = Option.map (List.map (fun e -> e.typ)) args in
          let ftyp = Eva_ast.type_of_lhost func in
          let compatible_funcs = Eval_typ.compatible_functions ftyp in
          let kfs, alarm' = compatible_funcs ?args:args_types kfs in
          let reduce = backward_function_pointer valuation state v in
          let reduce kf = let+ value = reduce kf in (kf, value) in
          let reduced_kfs = Bottom.list_filter_map reduce kfs in
          let status =
            if reduced_kfs = [] then Alarmset.False
            else if alarm || alarm' then Alarmset.Unknown
            else Alarmset.True
          in
          let cil_v = Eva_ast.to_cil_exp v in
          let cil_args = Option.map (List.map Eva_ast.to_cil_exp) args in
          let alarm = Alarms.Function_pointer (cil_v, cil_args) in
          `Value reduced_kfs, Alarmset.set alarm status alarms
end
