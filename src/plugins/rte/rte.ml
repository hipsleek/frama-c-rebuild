(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type 'a alarm_gen =
  remove_trivial:bool ->
  on_alarm:(invalid:bool -> Alarms.alarm -> unit) ->
  'a -> unit

type bound_kind = Alarms.bound_kind = Lower_bound | Upper_bound

(* Tries to evaluate expr as a constant value (Int64.t).
   Uses Cil constant folding (e.g. for (-0x7ffffff -1) => Some (-2147483648)) on
   32 bits *)
let get_expr_val expr = Cil.constFoldToInt expr

(* Creates [0 <= e] and [e < size] assertions *)
let valid_index ~remove_trivial ~on_alarm e size =
  let alarm bk =
    let b = match bk with
      | Lower_bound -> None
      | Upper_bound -> Some size
    in
    (* Do not create upper-bound check on GNU zero-length arrays *)
    if not (bk == Upper_bound && Cil.isZero size) then begin
      on_alarm ~invalid:false (Alarms.Index_out_of_bound(e, b))
    end
  in
  if remove_trivial then begin
    (* See if the two assertions do not trivially hold. In this
       case, do not return then *)
    let v_e = get_expr_val e in
    let v_size = get_expr_val size in
    let neg_ok =
      Option.fold ~none:false ~some:(Z.leq Z.zero) v_e
      || Cil.isUnsignedInteger (Cil.typeOf e)
    in
    if not neg_ok then alarm Lower_bound;
    let pos_ok = match v_e, v_size with
      | Some v_e, Some v_size -> Z.lt v_e v_size
      | None, _ | _, None -> false
    in
    if not pos_ok then alarm Upper_bound
  end else begin
    alarm Lower_bound;
    alarm Upper_bound;
  end


(* returns the assertion associated with an lvalue:
   returns non empty assertions only on pointer dereferencing and array access.
   The validity assertions are emitted using [valid] if
   [~read_only] is false, or with [valid_read] otherwise *)
let lval_assertion ~read_only ~remove_trivial ~on_alarm lv =
  (* For accesses to known arrays we generate an assertions that constrains
     the index. This is simpler than the [\valid] assertion *)
  let rec check_array_access default off typ in_struct =
    match off with
    | NoOffset ->
      if default then
        on_alarm ~invalid:false (Alarms.Memory_access(lv, read_only))
    | Field (fi, off) ->
      (* Mark that we went through a struct field, then recurse *)
      check_array_access default off fi.ftype true
    | Index (e, off) ->
      match Ast_types.unroll_node typ with
      | TArray (bt, Some size) ->
        if Kernel.SafeArrays.get () || not in_struct then begin
          (* Generate an assertion for this access, then go deeper in
             case other accesses exist *)
          valid_index ~remove_trivial ~on_alarm e size;
          check_array_access default off bt in_struct
        end else
          (* Access to an array embedded in a struct with option
             [-unsafe-arrays]. Honor the option and generate only
             the default [\valid] assertion *)
          check_array_access true off bt in_struct
      | TArray (bt, None) -> check_array_access true off bt in_struct
      | _ -> assert false
  in
  match lv with
  | Var vi , off -> check_array_access false off vi.vtype false
  | (Mem _ as lh), off ->
    if not (Ast_types.is_fun (Cil.typeOfLval lv)) then
      check_array_access true off (Cil.typeOfLhost lh) false

(* assertion for lvalue initialization *)
let lval_initialized_assertion ~remove_trivial:_ ~on_alarm lv =
  let typ = Cil.typeOfLval lv in
  match lv with
  | Var vi, NoOffset ->
    (* Note: here [lv] has structure/union type or fundamental type.
       We exclude structures and unions. And for fundamental types:
       - globals (initialized and then only written with initialized values)
       - formals (checked at function call)
       - temporary variables (initialized during AST normalization)
    *)
    if not (vi.vglob || vi.vformal || vi.vtemp)
    && not (Ast_types.is_struct_or_union typ)
    then
      on_alarm ~invalid:false (Alarms.Uninitialized lv)
  | _ ->
    if not Ast_types.(is_fun typ || is_struct_or_union typ) then
      on_alarm ~invalid:false (Alarms.Uninitialized lv)

(* assertion for unary minus signed overflow *)
let uminus_assertion ~remove_trivial ~on_alarm exp =
  (* - expr overflows if exp is TYPE_MIN *)
  let t = Ast_types.unroll (Cil.typeOf exp) in
  let size = Cil.bitsSizeOf t in
  let min_ty = Cil.min_signed_number size in
  (* alarm is bound <= exp, hence bound must be MIN_INT+1 *)
  let bound = Z.succ min_ty in
  let alarm ?(invalid=false) () =
    let a = Alarms.Overflow(Alarms.Signed, exp, bound, Lower_bound) in
    on_alarm ~invalid a
  in
  if remove_trivial then begin
    match get_expr_val exp with
    | None -> alarm ()
    | Some a64 ->
      (* constant operand *)
      if Z.equal a64 min_ty then
        alarm ~invalid:true ()
  end
  else alarm ()

(* assertions for multiplication/addition/subtraction overflows *)
let mult_sub_add_assertion ~signed ~remove_trivial ~on_alarm (exp,op,lexp,rexp) =
  (* signed multiplication/addition/subtraction:
     the expression overflows iff its integer value
     is strictly more than [max_ty] or strictly less than [min_ty] *)
  let t = Ast_types.unroll (Cil.typeOf exp) in
  let size = Cil.bitsSizeOf t in
  let min_ty, max_ty =
    if signed then Cil.min_signed_number size, Cil.max_signed_number size
    else Z.zero, Cil.max_unsigned_number size
  in
  let alarm ?(invalid=false) bk =
    let bound = match bk with
      | Upper_bound -> max_ty
      | Lower_bound -> min_ty
    in
    let signed = if signed then Alarms.Signed else Alarms.Unsigned in
    on_alarm ~invalid (Alarms.Overflow (signed, exp, bound, bk));
  in
  let alarms () =
    alarm Lower_bound;
    alarm Upper_bound;
  in
  if remove_trivial then begin
    match get_expr_val lexp, get_expr_val rexp, op with
    | Some l, Some r, _ -> (* both operands are constant *)
      let warn r =
        let warn bk = alarm ~invalid:true bk in
        if Z.gt r max_ty then warn Upper_bound
        else if Z.lt r min_ty then warn Lower_bound
      in
      (match op with
       | MinusA -> warn (Z.sub l r)
       | PlusA -> warn (Z.add l r)
       | Mult -> warn (Z.mul l r)
       | _ -> assert false)

    | _, Some v , PlusA | Some v, _, PlusA ->
      if Z.(gt v zero) then alarm Upper_bound
      else if Z.(lt v zero) then alarm Lower_bound (* signed only *)

    | _, Some r , MinusA ->
      if Z.(gt r zero) then alarm Lower_bound
      else if Z.(lt r zero) then alarm Upper_bound (* signed only *)

    | Some l, None , MinusA ->
      if signed then begin
        (* The possible range for [-r] is [-max_int .. -min_int] i.e.
           [min_int+1..max_int+1]; we need to check [l] w.r.t [-1]. *)
        if Z.(gt l minus_one) then alarm Upper_bound
        else if Z.(lt l minus_one) then alarm Lower_bound
      end
      else begin
        (* Only negative overflows are possible, since r is positive. (TODO:
           nothing can happen on [max_int]. *)
        alarm Lower_bound
      end

    | Some v, None, Mult | None, Some v, Mult
      when Z.is_zero v || Z.is_one v -> ()

    | None, None, _ | Some _, None, _ | None, Some _, _ -> alarms ()
  end
  else alarms ()

(* assertions for division and modulo (divisor is 0) *)
let divmod_assertion ~remove_trivial ~on_alarm divisor =
  (* division or modulo: overflow occurs when divisor is equal to zero *)
  let alarm ?(invalid=false) () =
    on_alarm ~invalid (Alarms.Division_by_zero divisor);
  in
  if remove_trivial then begin
    match get_expr_val divisor with
    | None -> (* divisor is not a constant *)
      alarm ();
    | Some v64 ->
      if Z.is_zero v64 then
        (* divide by 0 *)
        alarm ~invalid:true ()
        (* else divide by constant which is not 0: nothing to assert *)
  end
  else alarm ()

(* assertion for signed division overflow *)
let signed_div_assertion ~remove_trivial ~on_alarm (exp, lexp, rexp) =
  (* Signed division: overflow occurs when dividend is equal to the
     the minimum (negative) value for the signed integer type,
     and divisor is equal to -1. Under the hypothesis (cf Value) that
     integers are represented in two's complement.
  *)
  let t = Ast_types.unroll (Cil.typeOf rexp) in
  let size = Cil.bitsSizeOf t in
  (* check dividend_expr / divisor_expr : if constants ... *)
  (* compute smallest representable "size bits" (signed) integer *)
  let max_ty = Cil.max_signed_number size in
  let alarm ?(invalid=false) () =
    let a = Alarms.Overflow(Alarms.Signed, exp, max_ty, Alarms.Upper_bound) in
    on_alarm ~invalid a;
  in
  if remove_trivial then begin
    let min = Cil.min_signed_number size in
    match get_expr_val lexp, get_expr_val rexp with
    | Some e1, _ when not (Z.equal e1 min) ->
      (* dividend is constant, with an unproblematic value *)
      ()
    | _, Some e2 when not (Z.equal e2 Z.minus_one) ->
      (* divisor is constant, with an unproblematic value *)
      ()
    | Some _, Some _ ->
      (* invalid constant division *)
      alarm ~invalid:true ()
    | None, Some _ | Some _, None | None, None ->
      (* at least one is not constant: cannot conclude *)
      alarm ()
  end
  else alarm ()

(* Assertions for the left and right operands of left and right shift. *)
let shift_assertion ~remove_trivial ~on_alarm (exp, upper_bound) =
  let alarm ?(invalid=false) () =
    let a = Alarms.Invalid_shift(exp, upper_bound) in
    on_alarm ~invalid a ;
  in
  if remove_trivial then begin
    match get_expr_val exp with
    | None -> alarm ()
    | Some c64 ->
      (* operand is constant:
         check it is nonnegative and strictly less than the upper bound (if
         any) *)
      let upper_bound_ok = match upper_bound with
        | None -> true
        | Some u -> Z.lt c64 (Z.of_int u)
      in
      if not (Z.geq c64 Z.zero && upper_bound_ok) then
        alarm ~invalid:true ()
  end
  else alarm ()

(* The right operand of shifts should be nonnegative and strictly less than the
   width of the promoted left operand. *)
let shift_width_assertion ~remove_trivial ~on_alarm (exp, typ) =
  let size = Cil.bitsSizeOf typ in
  shift_assertion ~remove_trivial ~on_alarm (exp, Some size)

(* The left operand of signed shifts should be nonnegative:
   implementation defined for right shift, undefined behavior for left shift. *)
let shift_negative_assertion ~remove_trivial ~on_alarm exp =
  shift_assertion ~remove_trivial ~on_alarm (exp, None)

(* Assertion for left and right shift overflow: the result should be
   representable in the result type.  *)
let shift_overflow_assertion ~signed ~remove_trivial ~on_alarm (exp, op, lexp, rexp) =
  let t = Ast_types.unroll (Cil.typeOf exp) in
  let size = Cil.bitsSizeOf t in
  if size <> Cil.bitsSizeOf (Cil.typeOf lexp) then
    (* size of result type should be size of left (promoted) operand *)
    Options.warning ~current:true ~once:true
      "problem with bitsSize of %a: not treated" Printer.pp_exp exp;
  if op = Shiftlt then
    (* compute greatest representable "size bits" (signed) integer *)
    let maxValResult =
      if signed
      then Cil.max_signed_number size
      else Cil.max_unsigned_number size
    in
    let overflow_alarm ?(invalid=false) () =
      let signed = if signed then Alarms.Signed else Alarms.Unsigned in
      let a = Alarms.Overflow (signed, exp, maxValResult, Alarms.Upper_bound) in
      on_alarm ~invalid a;
    in
    if remove_trivial then begin
      match get_expr_val lexp, get_expr_val rexp with
      | None,_ | _, None ->
        overflow_alarm ()
      | Some lval64, Some rval64 ->
        (* both operands are constant: check result is representable in
           result type *)
        if Z.(rval64 >= zero && (shift_left_z lval64 rval64) >= maxValResult)
        then
          overflow_alarm ~invalid:true ()
    end
    else overflow_alarm ()

(* Assertion for downcasts. *)
let downcast_assertion ~remove_trivial ~on_alarm (dst_type, exp) =
  let src_type = Cil.typeOf exp in
  let src_signed = Cil.isSignedInteger src_type in
  let dst_signed = Cil.isSignedInteger dst_type in
  let src_size = Cil.bitsSizeOf src_type in
  let dst_size = Cil.bitsSizeOfBitfield dst_type in
  if (dst_size < src_size || dst_size == src_size && dst_signed <> src_signed)
  && not Ast_types.(is_ptr src_type && (is_intptr_t dst_type || is_uintptr_t dst_type))
  then
    let dst_min, dst_max =
      if dst_signed
      then Cil.min_signed_number dst_size, Cil.max_signed_number dst_size
      else Z.zero, Cil.max_unsigned_number dst_size
    in
    let overflow_kind =
      if Ast_types.is_ptr src_type
      then Alarms.Pointer_downcast
      else if dst_signed
      then Alarms.Signed_downcast
      else Alarms.Unsigned_downcast
    in
    let alarm ?(invalid=false) bound bound_kind =
      let a = Alarms.Overflow (overflow_kind, exp, bound, bound_kind) in
      on_alarm ~invalid a;
    in
    let alarms () =
      alarm dst_max Upper_bound;
      (* unsigned values cannot overflow in the negative *)
      if src_signed then alarm dst_min Lower_bound;
    in
    match remove_trivial, get_expr_val exp with
    | true, Some a64 ->
      let invalid = true in
      if Z.lt a64 dst_min then alarm ~invalid dst_min  Lower_bound
      else if Z.gt a64 dst_max then alarm ~invalid dst_max Upper_bound
    | _ -> alarms ()

(* assertion for casting a floating-point value to an integer *)
let float_to_int_assertion ~remove_trivial ~on_alarm (ty, exp) =
  let e_typ = Ast_types.unroll (Cil.typeOf exp) in
  match e_typ.tnode, ty.tnode with
  | TFloat _, TInt ikind ->
    let signed = Cil.isSigned ikind in
    let size = Cil.bitsSizeOfBitfield ty in
    let largest = Cil.max_unsigned_number size in
    let max_ty = if signed then Cil.max_signed_number size else largest in
    let min_ty = if signed then Cil.min_signed_number size else Z.zero in
    let bound = function Lower_bound -> min_ty | Upper_bound -> max_ty in
    let build_alarm b = Alarms.Float_to_int (exp, bound b, b) in
    let alarm ?(invalid = false) b = on_alarm ~invalid (build_alarm b) in
    let number =
      match exp.enode with
      | Const (CReal (f, fk, _)) -> Some (f, fk)
      | UnOp (Neg, { enode = Const (CReal (f, fk, _)) }, _) -> Some (-. f, fk)
      | _ -> None
    in
    begin match remove_trivial, number with
      | false, _ | true, None -> alarm Upper_bound ; alarm Lower_bound
      | true, Some (f, _) ->
        match Floating_point.truncate_to_integer f with
        | Underflow -> alarm Lower_bound
        | Overflow  -> alarm Upper_bound
        | Integer i when Z.lt i min_ty -> alarm ~invalid:true Lower_bound
        | Integer i when Z.gt i max_ty -> alarm ~invalid:true Upper_bound
        | Integer _ -> ()
    end
  | _ -> ()

(* assertion for checking only finite float are used *)
let finite_float_assertion ~remove_trivial:_ ~on_alarm (fkind, exp) =
  let invalid = false in
  match Kernel.SpecialFloat.get () with
  | "none"       -> ()
  | "nan"        -> on_alarm ~invalid (Alarms.Is_nan (exp, fkind))
  | "non-finite" -> on_alarm ~invalid (Alarms.Is_nan_or_infinite (exp, fkind))
  | _            -> assert false

(* assertion for a pointer call [( *e )(args)]. *)
let pointer_call ~remove_trivial:_ ~on_alarm (e, args) =
  on_alarm ~invalid:false (Alarms.Function_pointer (e, Some args))

let rec is_safe_offset = function
  | NoOffset -> true
  | Field(fi,o) -> fi.fcomp.cstruct && not fi.faddrof && is_safe_offset o
  | Index(_,o) -> is_safe_offset o

let is_safe_pointer_value = function
  | Lval (Var vi, offset) ->
    (* Reading a pointer variable must emit an alarm if an invalid pointer value
       could have been written without previous alarm, through:
       - an union type, in which case [offset] is not NoOffset;
       - an untyped write, in which case the address of [vi] is taken. *)
    not vi.vaddrof && is_safe_offset offset
  | AddrOf (_, NoOffset) | StartOf (_, NoOffset) -> true
  | CastE (_typ, e) ->
    (* 0 can always be converted into a NULL pointer. *)
    let v = get_expr_val e in
    Option.fold ~none:false ~some:Z.(equal zero) v
  | _ -> false

let pointer_value ~remove_trivial ~on_alarm expr =
  if not (remove_trivial && is_safe_pointer_value expr.enode)
  then on_alarm ~invalid:false (Alarms.Invalid_pointer expr)

type verdict = Yes | No | Maybe

let trivially_aligned (expr: Cil_types.exp) target =
  if Ast_types.is_void target
  || Ast_types.is_fun target
  then
    (* - From an alignment point of view, casting to void* is always OK
         (except for function pointers, but anyway, the problem is not
         alignment)
       - Alignment does not make sense for functions *)
    Yes
  else
    (* we can safely compute this now *)
    let t_align = Cil.bytesAlignOf target in
    let expr = Cil.stripCasts expr in
    let orig_t = Cil.typeOf expr in
    if Ast_types.is_void_ptr orig_t || Ast_types.is_fun_ptr orig_t
    then Maybe
    else
    if Ast_types.is_integral orig_t
    then match Cil.constFoldToInt expr with
      | None -> Maybe
      | Some value when Z.(zero = (value mod of_int t_align)) -> Yes
      | _ -> No
    else
      match expr.enode with
      | Lval (Var vi, NoOffset) when not vi.vglob && not vi.vaddrof ->
        (* This optimization can be generalized if we check strict aliasing *)
        if t_align <= Cil.bytesAlignOf @@ Ast_types.direct_pointed_type orig_t
        then Yes
        else Maybe

      | AddrOf (Var vi, NoOffset) | StartOf (Var vi, NoOffset) ->
        if 0 = Cil.bytesAlignOfVarinfo vi mod t_align
        then Yes
        else Maybe

      | _ -> (* probably more cases to optimize here *)
        Maybe

let pointer_alignment ~remove_trivial ~on_alarm (expr, t) =
  assert (Ast_types.is_ptr t) ;
  let pointed_to = Ast_types.direct_pointed_type t in
  let expr = Cil.stripCasts expr in
  match trivially_aligned expr pointed_to with
  | Yes ->
    if not remove_trivial
    then on_alarm ~invalid:false (Alarms.Unaligned_pointer (expr, pointed_to))
  | No ->
    on_alarm ~invalid:true (Alarms.Unaligned_pointer (expr, pointed_to))
  | Maybe ->
    on_alarm ~invalid:false (Alarms.Unaligned_pointer (expr, pointed_to))

let bool_value ~remove_trivial ~on_alarm lv =
  match remove_trivial, lv with
  | true, (Var vi, NoOffset)
    (* This optimization can be generalized if we check strict aliasing *)
    when (* consider as trivial accesses to ...  *)
      (not vi.vglob) && (* local variable or formal parameter when ... *)
      (not vi.vaddrof)  (* their address is not taken *)
    -> ()
  | _ -> on_alarm ~invalid:false (Alarms.Invalid_bool lv)
