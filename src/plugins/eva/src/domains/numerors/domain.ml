(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)



(* Reduced product interaction modes. *)
type interaction_modes =
  | Only_Reduce_Absolute_Using_Relative
  | Only_Reduce_Relative_Using_Absolute
  | Complete_Reduced_Product
  | No_Reduced_Product

(* Retrieving the interaction mode from parameters. *)
let get_interaction_mode () =
  match Parameters.NumerorsInteraction.get () with
  | "relative" -> Only_Reduce_Relative_Using_Absolute
  | "absolute" -> Only_Reduce_Absolute_Using_Relative
  | "both" -> Complete_Reduced_Product
  | "none" -> No_Reduced_Product
  | _ -> assert false

(* Identity effect with unit context. *)
module Identity = struct
  include Identity
  type context = unit
  let resolve () x = x
end

(* Interval abstraction over rationals. *)
module Abstraction = Field_interval.Make (Rational) (Identity)



(* Numerors model based on rational intervals and without context. *)
module Model = struct

  module Scalar = Rational
  type scalar = Rational.t

  module Context = Unit_context
  module Computation = Identity
  type 'a computation = 'a Computation.t

  module Additive       = Abstraction
  module Multiplicative = Abstraction

  module Exact    = Abstraction
  module Absolute = Abstraction
  module Relative = Abstraction

  type exact    = Abstraction.t
  type absolute = Abstraction.t
  type relative = Abstraction.t

  let name = "Numerors.Value"

  let between lower upper =
    let lower = Abstraction.singleton lower in
    let upper = Abstraction.singleton upper in
    Abstraction.join lower upper

  let new_absolute_elementary_error _ bound =
    let upper = Scalar.abs bound in
    let lower = Scalar.neg upper in
    between lower upper

  let new_relative_elementary_error _ bound =
    let upper = Scalar.abs bound in
    let lower = Scalar.neg upper in
    between lower upper

  let do_reduce_absolute_with_relative () =
    match get_interaction_mode () with
    | Only_Reduce_Absolute_Using_Relative | Complete_Reduced_Product -> true
    | Only_Reduce_Relative_Using_Absolute | No_Reduced_Product -> false

  let do_reduce_relative_with_absolute () =
    match get_interaction_mode () with
    | Only_Reduce_Relative_Using_Absolute | Complete_Reduced_Product -> true
    | Only_Reduce_Absolute_Using_Relative | No_Reduced_Product -> false

  let recompute_absolute ~(exact : exact) ~(relative : relative) =
    Abstraction.(exact * relative)

  let recompute_relative ~(exact : exact) ~(absolute : absolute) =
    Abstraction.(absolute / exact)

  let a_x_plus_b_y_over_x_plus_y ~a ~x ~b ~y =
    let bounds s = let b = Abstraction.bounds s in [ b.lower ; b.upper ] in
    let permutations l r = List.(map (fun l -> map (fun r -> l, r) r) l |> flatten) in
    let compute ((a, x), (b, y)) = Scalar.((a * x + b * y) / (x + y)) in
    let ax_permutations = permutations (bounds a) (bounds x) in
    let by_permutations = permutations (bounds b) (bounds y) in
    let values = List.map compute (permutations ax_permutations by_permutations) in
    let lower = List.fold_left Scalar.min Scalar.pos_inf values in
    let upper = List.fold_left Scalar.max Scalar.neg_inf values in
    between lower upper

end



(* Instantiate the value using the model. *)
module Value = struct
  include Value.Make (Model)
  let contextualize (name, builtin) = (name, builtin ())
  let builtins = List.map contextualize builtins
end



(* Reduced product with Cvalues. *)
module Reduce_Cast (Abstract : Abstractions.S) : Abstractions.S = struct

  include Abstract

  let project_ival cvalue =
    try Cvalue.V.project_ival cvalue
    with Cvalue.V.Not_based_on_null -> Ival.top

  let cast get_cvalue set_numerors context ~src_type ~dst_type value =
    let open Eval.Bottom.Operators in
    let+ result = Val.forward_cast context ~src_type ~dst_type value in
    match src_type, dst_type with
    | Eval_typ.(TSInt _, TSFloat fkind) ->
      let ival = get_cvalue value |> project_ival in
      let lower, upper = Ival.min_and_max ival in
      let to_neg_inf v = Option.value v ~default:Rational.neg_inf in
      let to_pos_inf v = Option.value v ~default:Rational.pos_inf in
      let lower = Option.(map Q.of_bigint lower |> to_neg_inf) in
      let upper = Option.(map Q.of_bigint upper |> to_pos_inf) in
      let numerors = Value.of_scalars fkind lower upper in
      set_numerors numerors result
    | _ -> result

  module Val = struct
    include Val
    let forward_cast =
      match get Main_values.CVal.key, mem Value.key with
      | None, _ | _, false -> forward_cast
      | Some get_cvalue, true -> cast get_cvalue (set Value.key)
  end

end



(* Public description of the Numerors abstract domain. *)
let descr =
  "Infers ranges for the absolute and relative errors \
   in floating-point computations."

(* Registration of the Numerors abstract domain and its reduced product. *)
let registered =
  let name = "numerors" in
  let module Name = (struct let name = name end) in
  let module Domain = Simple_memory.Make_Domain (Name) (Value) in
  Abstractions.Hooks.register (fun (module A) -> (module Reduce_Cast (A))) ;
  Abstractions.Domain.register ~name ~descr ~experimental:true ~priority:5
    (module Domain)
