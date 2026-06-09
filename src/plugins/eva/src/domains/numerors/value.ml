(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type ('context, 'value) builtin =
  'context -> 'value list -> 'value Eval.or_bottom

module Make (Model : IEEE754.Modeling) = struct

  include IEEE754.Make (Model)

  let track_variable vi =
    Ast_types.is_float Cil_types.(vi.vtype)

  let of_scalars fkind l u =
    let Format format = format_of_fkind fkind in
    let lower = Exact.singleton l in
    let upper = Exact.singleton u in
    let exact = Exact.join lower upper in
    let absolute = Absolute.zero in
    let relative = Relative.zero in
    make exact absolute relative format

  let set_errors_to_zero value =
    match format value with
    | `Top -> value
    | `Value (Format format) ->
      let exact    = exact value in
      let absolute = Absolute.zero in
      let relative = Relative.zero in
      make exact absolute relative format

  let sqrt context = function
    | [ v ] -> `Value Computation.(return v |> sqrt |> resolve context)
    | _ -> `Value top

  let between _context = function
    | [ l ; u ] -> `Value (join l u |> set_errors_to_zero)
    | _ -> `Value top

  let builtins =
    [ ("Frama_C_double_interval", between)
    ; ("sqrt", sqrt)
    ]



  module Hints = Set.Make (Scalar)

  let convert_hints float_hints =
    let fold f = Hints.add (Scalar.of_float f) in
    Datatype.Float.Set.fold fold float_hints Hints.empty

  let compute_error_hints_multipliers format =
    let open Scalar in
    let ulp = Typed_float.unit_in_the_last_place_of ~format in
    let ulp = Model.Scalar.of_float (Typed_float.to_float ulp) in
    let hints = ref Hints.(empty |> add zero |> add ulp |> add one) in
    let to_scalar z = Z.to_string z |> of_string in
    for i = 0 to 6 do
      let exponent = Z.pow 2z i |> Z.to_int in
      let hint = Z.pow 2z exponent |> to_scalar in
      hints := Hints.add (ulp * hint) !hints
    done ;
    let positive = !hints in
    let negative = Hints.map neg !hints in
    Hints.union positive negative

  let simple = compute_error_hints_multipliers Single
  let double = compute_error_hints_multipliers Double
  let float32 = compute_error_hints_multipliers Float32
  let float64 = compute_error_hints_multipliers Float64

  let error_hints_multiplier : type f. f Typed_float.format -> Hints.t =
    function Single  -> simple  | Double  -> double
           | Float32 -> float32 | Float64 -> float64



  module type Abstraction = IEEE754.Abstraction
    with module Scalar = Scalar
     and module Computation = Computation

  type 't abstraction = (module Abstraction with type t = 't)

  let widen_bound ~get ~choose ~optimize before after =
    let before = get before and after = get after in
    if not Scalar.(before = after)
    then optimize (choose before after)
    else after

  let lower (type t) (abstraction : t abstraction) hints before after =
    let module A = (val abstraction) in
    let choose x y = Model.Scalar.min x y in
    let decide lower t = Model.Scalar.(t <= lower) in
    let topify v = Option.value ~default:Model.Scalar.neg_inf v in
    let optimize lower = Hints.find_last_opt (decide lower) hints |> topify in
    widen_bound ~get:A.lower ~choose ~optimize before after |> A.singleton

  let upper (type t) (abstraction : t abstraction) hints before after =
    let module A = (val abstraction) in
    let choose x y = Model.Scalar.max x y in
    let decide upper t = Model.Scalar.(upper <= t) in
    let topify v = Option.value ~default:Model.Scalar.pos_inf v in
    let optimize upper = Hints.find_first_opt (decide upper) hints |> topify in
    widen_bound ~get:A.upper ~choose ~optimize before after |> A.singleton

  let widen_abst (type t) (abst : t abstraction) project hints before after =
    let lower = lower abst hints (project before) (project after) in
    let upper = upper abst hints (project before) (project after) in
    let module A = (val abst) in A.join lower upper

  let widen_or_top hints before after =
    let open Eval.Top.Operators in
    let widen abst proj hints = widen_abst abst proj hints before after in
    let+ Format f = format before and+ Format f' = format after in
    match Typed_float.same_format f f' with
    | No -> top
    | Yes format when Exact.is_included (exact after) (exact before) ->
      let hints = error_hints_multiplier format in
      let absolute = widen (module Absolute) absolute hints in
      let relative = widen (module Relative) relative hints in
      make (exact before) absolute relative format
    | Yes format ->
      let hints = convert_hints hints in
      let absolute = Absolute.zero in
      let relative = Relative.zero in
      let exact = widen (module Exact) exact hints in
      join after (make exact absolute relative format)

  let widen (_, hints) before after =
    widen_or_top hints before after |>
    Eval.Top.value ~top:top

end
