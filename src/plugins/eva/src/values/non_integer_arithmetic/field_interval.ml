(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Name = sig val name : string end

type 'k bounds = 'k Field.bounds = { lower : 'k ; upper : 'k }

module Make (K : Field.S) (Computation : IEEE754.Computation) = struct

  module Scalar = K
  type scalar = K.t

  module Computation = Computation
  type 'a computation = 'a Computation.t

  type subset = scalar bounds

  let singleton k = { lower = k ; upper = k }
  let zero = singleton K.zero
  let one  = singleton K.one
  let top  = { lower = K.neg_inf ; upper = K.pos_inf }

  let between l u =
    if Scalar.(is_valid l && is_valid u) then
      if Scalar.(l <= u) then { lower = l ; upper = u }
      else Self.fatal "Lower bound is greater than upper bound."
    else top

  let pretty fmt { lower ; upper } =
    Format.fprintf fmt "@[[%a .. %a]@]" K.pretty lower K.pretty upper

  let compare l r =
    let c = K.compare l.lower r.lower in
    if c = 0 then K.compare l.upper r.upper else c

  let structural_descr =
    let scalar = K.packed_descr in
    Structural_descr.t_record [| scalar ; scalar |]

  include Datatype.Make (struct
      type t = subset
      let name = "Field.Interval(" ^ K.datatype_name ^ ")"
      let reprs = [ zero ; one ; top ]
      let structural_descr = structural_descr
      let mem_project = Datatype.never_any_project
      let rehash = Datatype.identity
      let copy b = { lower = K.copy b.lower ; upper = K.copy b.upper }
      let hash b = Hashtbl.hash(K.hash b.lower, K.hash b.upper)
      let pretty = pretty
      let compare = compare
      let equal = Datatype.from_compare
    end)

  let is_included l r = K.(r.lower <= l.lower && l.upper <= r.upper)
  let join l r = between (K.min l.lower r.lower) (K.max l.upper r.upper)
  let narrow l r =
    if K.(l.upper < r.lower || r.upper < l.lower) then `Bottom
    else `Value (between (K.max l.lower r.lower) (K.min l.upper r.upper))

  let lower  b = b.lower
  let upper  b = b.upper
  let bounds b = b

  let neg b =
    let open Computation.Operators in
    let+ b in between (K.neg b.upper) (K.neg b.lower)

  let sqrt b =
    let open Computation.Operators in
    let+ b in join (K.sqrt b.lower) (K.sqrt b.upper)

  let ( + ) l r =
    let open Computation.Operators in
    let+ l and+ r in between K.(l.lower + r.lower) K.(l.upper + r.upper)

  let ( - ) l r =
    let open Computation.Operators in
    let+ l and+ r in between K.(l.lower - r.upper) K.(l.upper - r.lower)

  let ( * ) l r =
    let open Computation.Operators in
    let* { lower = l  ; upper = u  } = l in
    let+ { lower = l' ; upper = u' } = r in
    let a, b, c, d = K.(l * l', l * u', u * l', u * u') in
    between K.(min (min a b) (min c d)) K.(max (max a b) (max c d))

  let ( / ) l r =
    let open Computation.Operators in
    let* { lower = l  ; upper = u  } = l in
    let+ { lower = l' ; upper = u' } = r in
    if K.(zero < l' || u' < zero) then
      let a, b, c, d = K.(l / l', l / u', u / l', u / u') in
      between K.(min (min a b) (min c d)) K.(max (max a b) (max c d))
    else top

  let backward_left_less_than ~left ~right =
    let lower = left.lower and upper = K.min left.upper right.upper in
    if not K.(lower <= upper) then `Bottom else `Value { lower ; upper }

  let backward_left_greater_than ~left ~right =
    let upper = left.upper and lower = K.max left.lower right.lower in
    if not K.(lower <= upper) then `Bottom else `Value { lower ; upper }

end
