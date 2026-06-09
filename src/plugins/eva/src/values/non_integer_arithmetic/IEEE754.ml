(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast
open Field



include IEEE754_sig
type 'f format = 'f Typed_float.format

module Make (Model : Modeling) = struct

  include Model

  type context = Context.t

  type 'f representation =
    { exact    : exact
    ; absolute : absolute
    ; relative : relative
    ; format   : 'f format
    }

  (* When computing backward operations, the Eva engine provides an abstract
     value representing the result of the operation. For logical expressions,
     the only provided value is the integer 0, meaning in C that the
     expression is false. However, the abstraction built here only handles
     floating point. Thus, to be able to handle backward operations on
     logical expressions, the constructor False is provided to represent
     the integer 0 in case it encodes that a logical expression is false. *)
  type value =
    | Top   : value
    | False : value
    | Repr  : 'f representation -> value

  let pretty fmt = function
    | False -> Format.fprintf fmt "{ 0 }"
    | Top -> Format.pp_print_string fmt Utf8_logic.top
    | Repr r ->
      Format.fprintf fmt "@[<v0>" ;
      Format.fprintf fmt "Real     : @[<h>%a@]@ " Exact.pretty    r.exact    ;
      Format.fprintf fmt "Absolute : @[<h>%a@]@ " Absolute.pretty r.absolute ;
      Format.fprintf fmt "Relative : @[<h>%a@]@ " Relative.pretty r.relative ;
      Format.fprintf fmt "Format : %a" Typed_float.pretty_format r.format ;
      Format.fprintf fmt "@]"

  let hash = function
    | False -> 0
    | Top -> 1
    | Repr r ->
      let format   = Hashtbl.hash  r.format   in
      let exact    = Exact.hash    r.exact    in
      let absolute = Absolute.hash r.absolute in
      let relative = Relative.hash r.relative in
      2 + Hashtbl.hash (format, exact, absolute, relative)

  let copy = function
    | False -> False
    | Top -> Top
    | Repr r ->
      let exact    = Exact.copy    r.exact    in
      let absolute = Absolute.copy r.absolute in
      let relative = Relative.copy r.relative in
      let format   = r.format in
      Repr { exact ; absolute ; relative ; format }

  let compare l r =
    match l, r with
    | False, False | Top, Top -> 0
    | False, _ | _, Top -> -1
    | _, False | Top, _ ->  1
    | Repr l, Repr r ->
      let conclude_they_are_equal = 0 in
      let ( let= ) c f = if c = 0 then f `Eq else c in
      let= `Eq = Typed_float.compare_format l.format r.format in
      let= `Eq = Exact.compare    l.exact    r.exact    in
      let= `Eq = Absolute.compare l.absolute r.absolute in
      let= `Eq = Relative.compare l.relative r.relative in
      conclude_they_are_equal

  let structural_descr =
    let open Structural_descr in
    let exact    = Exact.packed_descr    in
    let absolute = Absolute.packed_descr in
    let relative = Relative.packed_descr in
    let format = t_sum [| [||] ; [||] |] |> pack in
    let repr = t_record [| exact ; absolute ; relative ; format |] in
    t_sum [| [||] ; [||] ; [| pack repr |] |]

  module Type = struct
    type t = value
    let name = name
    let reprs = [ False ; Top ]
    let rehash = Datatype.identity
    let mem_project = Datatype.never_any_project
    let structural_descr = structural_descr
    let pretty = pretty
    let hash = hash
    let copy = copy
    let compare = compare
    let equal = Datatype.from_compare
  end

  include Datatype.Make (Type)
  let context = Abstract_context.Leaf (module Context)
  let key : t Abstract_value.key =
    let key_name = Format.sprintf "IEEE754(%s)" name in
    Structure.Key_Value.create_key key_name

  let resolve context computation =
    let context = Abstract_value.(context.from_domains) in
    Computation.resolve context computation

  let map  = Computation.map
  let lift = Computation.return

  let exact    repr = map (fun r -> r.exact   ) repr
  let absolute repr = map (fun r -> r.absolute) repr
  let relative repr = map (fun r -> r.relative) repr
  let format   repr = map (fun r -> r.format  ) repr
  let approx   repr = Exact.(exact repr + absolute repr)

  let format_of_fkind = function
    | Cil_types.FFloat      -> Format Single
    | Cil_types.FFloat32    -> Format Float32
    | Cil_types.FFloat64    -> Format Float64
    | Cil_types.FDouble     -> Format Double
    | Cil_types.FLongDouble ->
      Kernel.warning ~wkey:Kernel.wkey_long_double
        "%s does not support the long double format.\
         It will instead use the double format."
        name ;
      Format Double



  let reduce_absolute exact absolute relative =
    let open Computation.Operators in
    if do_reduce_absolute_with_relative () then
      if not Relative.(equal relative zero) then
        let+ computed = recompute_absolute ~exact ~relative in
        let reduced = Absolute.narrow absolute computed in
        Eval.Bottom.non_bottom reduced
      else Computation.return Absolute.zero
    else Computation.return absolute

  let reduce_relative exact absolute relative =
    let open Computation.Operators in
    if do_reduce_relative_with_absolute () then
      if not Absolute.(equal absolute zero) then
        let+ computed = recompute_relative ~exact ~absolute in
        let reduced = Relative.narrow relative computed in
        Eval.Bottom.non_bottom reduced
      else Computation.return Relative.zero
    else Computation.return relative

  let make exact absolute' relative' format =
    let open Computation.Operators in
    if not Exact.(equal exact top) then
      let* absolute = reduce_absolute exact absolute' relative' in
      let+ relative = reduce_relative exact absolute' relative' in
      Repr { exact ; absolute ; relative ; format }
    else Computation.return Top



  (* Returns the unit in the last place of the given format if the input
     interval [ lower ; upper ] contains normalized numbers.
     Returns zero otherwise. *)
  let machine_epsilon format lower upper =
    let epsilon = Typed_float.unit_in_the_last_place_of ~format in
    let epsilon = Scalar.of_float (Typed_float.to_float epsilon) in
    let in_pos_range = Scalar.(epsilon <= upper) in
    let in_neg_range = Scalar.(lower <= neg epsilon) in
    if in_pos_range || in_neg_range then epsilon else Scalar.zero

  (* Returns the smallest denormalized number in the given format if the input
     interval [ lower ; upper ] contains denormalized numbers.
     Returns zero otherwise. *)
  let machine_delta format lower upper =
    let epsilon = Typed_float.unit_in_the_last_place_of ~format in
    let epsilon = Scalar.of_float (Typed_float.to_float epsilon) in
    let delta = Typed_float.smallest_denormal_float_of ~format in
    let delta = Scalar.of_float (Typed_float.to_float delta) in
    let in_neg_range = Scalar.(lower <= neg epsilon && neg epsilon <= upper) in
    let in_pos_range = Scalar.(lower <= epsilon && epsilon <= upper) in
    if in_pos_range || in_neg_range then delta else Scalar.zero

  let abs_exact_bounds approx =
    let { lower ; upper } = Exact.bounds approx in
    let lower' = Scalar.abs lower in
    let upper' = Scalar.abs upper in
    if Scalar.(lower <= zero && zero <= upper)
    then Scalar.{ lower = zero ; upper = max lower' upper' }
    else Scalar.{ lower = min lower' upper' ; upper = max lower' upper' }

  (* Returns upper bounds of the elementary rounding errors introduced by a
     correctly rounded expression in a given format based on its absolute
     floating point bounds.

     Denote r = [l ; u] the lower (resp upper) bound of the absolute value
     of the given floating point computation. Denote ε either the unit in the
     last place if r contains normalized numbers or zero otherwise. Denote δ
     either the smallest denormalized if r contains denormalized numbers or
     zero otherwise.

     The absolute elementary rounding error bound is max (2 ^ ⌊log₂ u⌋ × ε, δ)
     The relative elementary rounding error bound is max (λ × ε, δ / l) where
     λ is equal to (2 ^ ⌊log₂ l⌋) / l if l and u share the same exponent and
     one otherwise. *)
  let elementary expr format approx =
    let open Computation.Operators in
    let max_float = Typed_float.largest_finite_float_of ~format in
    let max_float = Typed_float.to_float max_float |> Scalar.of_float in
    let { lower ; upper } = abs_exact_bounds approx in
    if Scalar.(upper <= max_float) then
      let machine_epsilon = machine_epsilon format lower upper in
      let machine_delta = machine_delta format lower upper in
      let epsilon = Scalar.(pow2 (log2 upper).lower * machine_epsilon) in
      let absolute = Scalar.max epsilon machine_delta in
      let* absolute = new_absolute_elementary_error expr absolute in
      if Scalar.(zero < lower) then
        let same_exponent = Scalar.log2 lower = Scalar.log2 upper in
        let significant n = Scalar.(pow2 (log2 n).lower / n) in
        let ufp = if same_exponent then significant lower else Scalar.one in
        let epsilon = Scalar.(ufp * machine_epsilon) in
        let delta = Scalar.(if machine_delta = zero then zero else one) in
        let relative = Scalar.max epsilon delta in
        let+ relative = new_relative_elementary_error expr relative in
        absolute, relative
      else Computation.return (absolute, Relative.top)
    else Computation.return (Absolute.top, Relative.top)

  let elementary expr format approx =
    let open Computation.Operators in
    let elementary = elementary expr format approx in
    let absolute = let+ elementary in fst elementary in
    let relative = let+ elementary in snd elementary in
    absolute, relative

  let add_elementary expr exact approx absolute relative format =
    let open Computation.Operators in
    let e_absolute, e_relative = elementary expr format approx in
    let* absolute = Absolute.(absolute + e_absolute) in
    let* relative = Relative.(relative * (lift one + e_relative) - lift one) in
    make exact absolute relative format

  let no_elementary exact absolute relative format =
    let open Computation.Operators in
    let* absolute and* relative = Relative.(relative - lift one) in
    make exact absolute relative format



  let contains_pos_inf r =
    let upper = Exact.upper r in
    Scalar.(upper = pos_inf)

  let contains_neg_inf r =
    let lower = Exact.lower r in
    Scalar.(lower = neg_inf)

  let contains_an_inf r =
    contains_pos_inf r || contains_neg_inf r

  let contains_zero r =
    let { lower ; upper } = Exact.bounds r in
    Scalar.(lower <= zero && zero <= upper)

  let is_zero r =
    let { lower ; upper } = Exact.bounds r in
    Scalar.(lower = zero && upper = zero)

  let is_power_of_two r =
    let { lower ; upper } = Exact.bounds r in
    let is_power_of_two f = Scalar.(pow2 (log2 (abs f)).lower = abs f) in
    Scalar.(lower = upper && (lower = zero || is_power_of_two lower))

  module NaN = struct

    let ( + ) l r =
      let open Computation.Operators in
      let+ l = exact l and+ r = exact r in
      (contains_pos_inf l && contains_neg_inf r) ||
      (contains_neg_inf l && contains_pos_inf r)

    let ( - ) l r =
      let open Computation.Operators in
      let+ l = exact l and+ r = exact r in
      (contains_pos_inf l && contains_pos_inf r) ||
      (contains_neg_inf l && contains_neg_inf r)

    let ( * ) l r =
      let open Computation.Operators in
      let+ l = exact l and+ r = exact r in
      (contains_an_inf l && contains_zero r) ||
      (contains_zero l && contains_an_inf r)

    let ( / ) l r =
      let open Computation.Operators in
      let+ l = exact l and+ r = exact r in
      (contains_an_inf l && contains_an_inf r) ||
      (contains_zero l && contains_zero r)

  end



  type 'f element = 'f representation computation
  type unary = Unary : 'f element -> unary

  let ( let@ ) computation step =
    let open Computation.Operators in
    let* value = computation in
    match value with
    | Top -> lift Top
    | False -> lift Top
    | Repr r -> step (Unary (lift r))

  let neg computation =
    let open Computation.Operators in
    let@ Unary  x = computation in
    let* exact    = Exact.neg    (exact    x) in
    let* absolute = Absolute.neg (absolute x) in
    let* relative = relative x and* format = format x in
    make exact absolute relative format

  let sqrt computation =
    let open Computation.Operators in
    let@ Unary x = computation in
    let* lower = Exact.(exact x |> map lower) in
    if Scalar.(lower >= zero) then
      let* format  = format x in
      let* result  = Exact.sqrt (exact x) in
      let* expr    = let+ x = exact x in Sqrt x in
      let* approx  = Exact.sqrt (approx x) in
      let relative = Relative.(sqrt (lift one + relative x)) in
      if do_reduce_absolute_with_relative () then
        let* relative' = Relative.(relative - lift one) in
        let absolute = recompute_absolute ~exact:result ~relative:relative' in
        add_elementary expr result approx absolute relative format
      else
        let imprecise = Absolute.(absolute x / exact x) in
        let imprecise = Absolute.(sqrt (lift one + imprecise) - lift one) in
        let absolute  = Absolute.(exact x * imprecise) in
        add_elementary expr result approx absolute relative format
    else Computation.return Top



  type binary = Binary : 'f binary_elements -> binary
  and 'f binary_elements = 'f format * 'f element * 'f element

  let ( let@ ) (left, right) step =
    let open Computation.Operators in
    let* left and* right in
    match left, right with
    | Top, _ | _, Top -> lift Top
    | False, _ | _, False -> lift Top
    | Repr left, Repr right ->
      match Typed_float.same_format left.format right.format with
      | Yes format -> step (Binary (format, lift left, lift right))
      | No -> lift Top

  let a_x_plus_b_y_over_x_plus_y ~a ~x ~b ~y result =
    if contains_zero result then Relative.(lift top)
    else a_x_plus_b_y_over_x_plus_y ~a ~x ~b ~y

  (* Check if the computation [l - r] is exactly computed. It relies on the
     Sterbenz lemma, stating that if [r / 2 ≤ l ≤ 2r] then the computation
     is exact. Moreover, this condition is equivalent to [l / 2 ≤ r ≤ 2l] in
     the concrete. Just to be sure, we check both conditions in the abstract.
     Note that the computation is also exact if either l or r are equal to
     zero, which is also checked. *)
  let is_linear_exact l r =
    let open Computation.Operators in
    let* l = Exact.(map bounds l) in
    let+ r = Exact.(map bounds r) in
    Scalar.(r.upper / two <= l.lower && l.upper <= r.lower * two) ||
    Scalar.(l.upper / two <= r.lower && r.upper <= l.lower * two) ||
    Scalar.(l.lower = zero && l.upper = zero) ||
    Scalar.(r.lower = zero && r.upper = zero)

  let is_addition_exact l r =
    is_linear_exact (exact l) (Exact.neg (exact r))

  let is_subtraction_exact l r =
    is_linear_exact (exact l) (exact r)

  let ( + ) l r =
    let open Computation.Operators in
    let@ Binary (format, l, r) = l, r in
    let* result_is_nan = NaN.(l + r) in
    if not result_is_nan then
      let* result = Exact.(exact l + exact r) in
      let* a = Relative.(lift one + relative l) and* x = exact l in
      let* b = Relative.(lift one + relative r) and* y = exact r in
      let relative = a_x_plus_b_y_over_x_plus_y ~a ~x ~b ~y result in
      let absolute = Absolute.(absolute l + absolute r) in
      let* exactly_computed = is_addition_exact l r in
      if not exactly_computed then
        let* expr = let+ l = exact l and+ r = exact r in Add (l, r) in
        let* approx = Exact.(approx l + approx r) in
        add_elementary expr result approx absolute relative format
      else no_elementary result absolute relative format
    else Computation.return Top

  let ( - ) l r =
    let open Computation.Operators in
    let@ Binary (format, l, r) = l, r in
    let* result_is_nan = NaN.(l - r) in
    if not result_is_nan then
      let* result = Exact.(exact l - exact r) in
      let* a = Relative.(lift one + relative l) in
      let* b = Relative.(lift one + relative r) in
      let* x = exact l and* y = Exact.neg (exact r) in
      let relative = a_x_plus_b_y_over_x_plus_y ~a ~x ~b ~y result in
      let absolute = Absolute.(absolute l - absolute r) in
      let* exactly_computed = is_subtraction_exact l r in
      if not exactly_computed then
        let* expr = let+ l = exact l and+ r = exact r in Sub (l, r) in
        let* approx = Exact.(approx l - approx r) in
        add_elementary expr result approx absolute relative format
      else no_elementary result absolute relative format
    else Computation.return Top

  let ( * ) l r =
    let open Computation.Operators in
    let@ Binary (format, l, r) = l, r in
    let* result_is_nan = NaN.(l * r) in
    if not result_is_nan then
      let* result = Exact.(exact l * exact r) in
      let from_l = Absolute.(exact l * absolute r) in
      let from_r = Absolute.(exact r * absolute l) in
      let from_errors = Absolute.(absolute l * absolute r) in
      let absolute = Absolute.(from_l + from_r + from_errors) in
      let relative = Relative.((lift one + relative l) * (lift one + relative r)) in
      let* l_is_a_power_of_two = map is_power_of_two (exact l) in
      let* r_is_a_power_of_two = map is_power_of_two (exact r) in
      if not (l_is_a_power_of_two || r_is_a_power_of_two) then
        let* expr = let+ l = exact l and+ r = exact r in Mul (l, r) in
        let* approx = Exact.(approx l * approx r) in
        add_elementary expr result approx absolute relative format
      else no_elementary result absolute relative format
    else Computation.return Top

  let ( / ) l r =
    let open Computation.Operators in
    let@ Binary (format, l, r) = l, r in
    let* result_is_nan = NaN.(l / r) in
    if not result_is_nan then
      let* result = Exact.(exact l / exact r) in
      let denominator = Absolute.(exact r + absolute r) in
      let compute exact relative = recompute_absolute ~exact ~relative in
      let compute e r = let* e and* r in compute e r in
      let reduced () = compute (exact l) (relative r) in
      let direct  () = Absolute.(exact l * absolute r / exact r) in
      let do_reduce () = do_reduce_absolute_with_relative () in
      let from_divisor = if do_reduce () then reduced () else direct () in
      let numerator = Absolute.(absolute l - from_divisor) in
      let absolute = Absolute.(numerator / denominator) in
      let shifted_relative x = Relative.(lift one + relative x) in
      let relative = Relative.(shifted_relative l / shifted_relative r) in
      let* r_is_power_of_two = map is_power_of_two (exact r) in
      let* l_is_zero = map is_zero (exact l) in
      if not (r_is_power_of_two || l_is_zero) then
        let* expr = let+ l = exact l and+ r = exact r in Div (l, r) in
        let* approx = Exact.(approx l / approx r) in
        add_elementary expr result approx absolute relative format
      else no_elementary result absolute relative format
    else Computation.return Top



  let top  = Top
  let zero = False
  let top_int = top
  let inject_int _ _ = top

  let zero_float format =
    let exact    = Exact.zero    in
    let absolute = Absolute.zero in
    let relative = Relative.zero in
    Repr { exact ; absolute ; relative ; format }

  let assume_non_zero v = `Unknown v
  let assume_bounded _ _ v = `Unknown v
  let assume_not_nan ~assume_finite:_ _ v = `Unknown v
  let assume_pointer v = `Unknown v
  let assume_aligned _ v = `Unknown v
  let assume_comparable _ l r = `Unknown (l, r)
  let rewrap_integer _ _ _ = top
  let resolve_functions _ = `Top, true
  let replace_base _ v = v
  let pretty_typ _ = pretty



  let is_included l r =
    match l, r with
    | (Top | False | Repr _), Top | False, False -> true
    | Top, (False | Repr _) | Repr _, False | False, Repr _ -> false
    | Repr l, Repr r ->
      match Typed_float.same_format l.format r.format with
      | No -> false
      | Yes _ ->
        Exact.is_included    l.exact    r.exact    &&
        Absolute.is_included l.absolute r.absolute &&
        Relative.is_included l.relative r.relative

  let join l r =
    match l, r with
    | (False | Repr _ | Top), Top | Top, (False | Repr _) -> Top
    | False, Repr _ | Repr _, False -> Top
    | False, False -> False
    | Repr l, Repr r ->
      match Typed_float.same_format l.format r.format with
      | No -> Top
      | Yes format ->
        let exact = Exact.join    l.exact    r.exact    in
        let abs   = Absolute.join l.absolute r.absolute in
        let rel   = Relative.join l.relative r.relative in
        Repr { exact ; absolute = abs ; relative = rel ; format }

  let narrow l r =
    let open Eval.Bottom.Operators in
    match l, r with
    | Top, result | result, Top -> `Value result
    | False, False -> `Value False
    | False, Repr _ | Repr _, False ->
      (* Trying to compute the narrow between False and a valid representation.
         In the abstraction lattice view, it should be Bottom, as False is a
         boolean, not a floating point number. But in C, both the boolean
         false and the floating point number 0.0 share the same exact bitfield
         representation and are thus strictly equal. The abstraction would be
         technically incorrect with respect to the C semantic, even if it
         would make no sens on the abstraction side. Good luck with that. *)
      Self.fatal
        "Trying to compute the narrow between False and a valid \
         floating-point representation, which are not compatible."
    | Repr l, Repr r ->
      match Typed_float.same_format l.format r.format with
      | No -> `Bottom
      | Yes format ->
        let* exact = Exact.narrow    l.exact    r.exact    in
        let* abs   = Absolute.narrow l.absolute r.absolute in
        let+ rel   = Relative.narrow l.relative r.relative in
        Repr { exact ; absolute = abs ; relative = rel ; format }



  let represents ~exact ~in_format =
    let approx = Scalar.represents ~scalar:exact ~in_format in
    let absolute = Scalar.(approx - exact) in
    let relative = Scalar.(if exact = zero then zero else absolute / exact) in
    let exact = Exact.singleton exact in
    let absolute = Absolute.singleton absolute in
    let relative = Relative.singleton relative in
    Repr { exact ; absolute ; relative ; format = in_format }

  let constant _ _ = function
    | CInt64 _ | CTopInt _ | CChr _ | CEnum _ -> Top
    | CReal (f, fkind, None) ->
      let Format format = format_of_fkind fkind in
      let exact = Scalar.of_float f |> Exact.singleton in
      let absolute = Absolute.zero in
      let relative = Relative.zero in
      Self.debug ~level:2
        "No exact representation for constant %f. \
         Assuming its floating point representation \
         is exact in format %a."
        f Typed_float.pretty_format format ;
      Repr { exact ; absolute ; relative ; format }
    | CReal (_, fkind, Some str) ->
      let Format format = format_of_fkind fkind in
      let exact = Scalar.of_string str in
      represents ~exact ~in_format:format

  let forward_unop context _ unop value =
    match unop with
    | Neg -> `Value (lift value |> neg |> resolve context)
    | BNot | LNot -> `Value top

  let forward_binop context _ binop l r =
    match binop with
    | PlusA  -> `Value (lift l + lift r |> resolve context)
    | MinusA -> `Value (lift l - lift r |> resolve context)
    | Mult   -> `Value (lift l * lift r |> resolve context)
    | Div    -> `Value (lift l / lift r |> resolve context)
    | PlusPI | MinusPI | MinusPP -> `Value top
    | Mod | Shiftlt | Shiftrt -> `Value top
    | Lt | Gt | Le | Ge | Eq | Ne -> `Value top
    | BAnd | BXor | BOr | LAnd | LOr -> `Value top

  let is_an_exact_constant_in_format ~format r =
    let exact = Exact.bounds r.exact in
    let abs = Absolute.bounds r.absolute in
    let rel = Relative.bounds r.relative in
    let limit = Typed_float.largest_finite_float_of ~format in
    let limit = Typed_float.to_float limit |> Scalar.of_float in
    let is_const b = Scalar.(b.lower = b.upper) in
    let in_format b = Scalar.(neg limit <= b.lower && b.upper <= limit) in
    if is_const exact && in_format exact && is_const abs && is_const rel
    then Some (exact.lower)
    else None

  let perform_imprecise_cast ~dest r =
    let open Computation.Operators in
    let expr = Cast r.exact and r = lift r in
    let* exact = exact r and* approx = approx r in
    let relative = Relative.(relative r + lift one) in
    let absolute = absolute r in
    add_elementary expr exact approx absolute relative dest

  let forward_cast context ~src_type ~dst_type value =
    let open Eval_typ in
    match value, src_type, dst_type with
    | Top, TSFloat _, TSFloat _ -> `Value Top
    | False, _, TSFloat destination ->
      let Format format = format_of_fkind destination in
      `Value (zero_float format)
    | _, (TSInt _ | TSPtr _), (TSInt _ | TSFloat _ | TSPtr _) -> `Value Top
    | _, TSFloat _, (TSInt _ | TSPtr _) -> `Value Top
    | Repr r, TSFloat _, TSFloat destination ->
      let Format dest = format_of_fkind destination in
      let default = Repr { r with format = dest } in
      match r.format, dest with
      | (Single | Float32), (Single | Float32 | Double | Float64) ->
        `Value default
      | (Double | Float64), (Double | Float64) -> `Value default
      | (Double | Float64), (Single | Float32) ->
        match is_an_exact_constant_in_format ~format:Single r with
        | None -> `Value (perform_imprecise_cast ~dest r |> resolve context)
        | Some r -> `Value (represents ~exact:r ~in_format:Single)



  type backward = Backward : 'a representation * 'b representation -> backward

  let ( let$ ) (Backward (left, right)) f =
    match Typed_float.same_format left.format right.format with
    | Yes format -> f (Format format)
    | No -> `Bottom

  let backward_binop context ~input_type:_ ~resulting_type:_ op ~left ~right ~result =
    let open Eval.Bottom.Operators in
    match op, left, right, result with
    | Ne, _, _, False ->
      let+ reduced = narrow left right in
      Some reduced, Some reduced
    | (Ge | Gt), Repr l, Repr r, False ->
      let$ Format format = Backward (l, r) in
      let* reduced_l = Exact.backward_left_less_than    ~left:l.exact ~right:r.exact in
      let+ reduced_r = Exact.backward_left_greater_than ~left:r.exact ~right:l.exact in
      let l = make reduced_l l.absolute l.relative format |> resolve context in
      let r = make reduced_r r.absolute r.relative format |> resolve context in
      Some l, Some r
    | (Le | Lt), Repr l, Repr r, False ->
      let$ Format format = Backward (l, r) in
      let* reduced_l = Exact.backward_left_greater_than ~left:l.exact ~right:r.exact in
      let+ reduced_r = Exact.backward_left_less_than    ~left:r.exact ~right:l.exact in
      let l = make reduced_l l.absolute l.relative format |> resolve context in
      let r = make reduced_r r.absolute r.relative format |> resolve context in
      Some l, Some r
    | _, _, _, _ -> `Value (None, None)

  let backward_cast _ ~src_typ:_ ~dst_typ:_ ~src_val:_ ~dst_val:_ =
    `Value None

  let backward_unop _ ~typ_arg:_ _ ~arg:_ ~res:_ =
    `Value None



  let make exact absolute relative format =
    Repr { exact ; absolute ; relative ; format }

  let exact = function
    | Top    -> Exact.top
    | False  -> Exact.zero
    | Repr r -> r.exact

  let absolute = function
    | Top    -> Absolute.top
    | False  -> Absolute.zero
    | Repr r -> r.absolute

  let relative = function
    | Top    -> Relative.top
    | False  -> Relative.zero
    | Repr r -> r.relative

  let format = function
    | Top | False -> `Top
    | Repr r -> `Value (Format r.format)

end
