(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Axiom = sig
  type 'a interior and 'a exterior
  val swap : 'a exterior interior -> 'a interior exterior
end

(* Using [Based_on_map] here, as the natural way to write the [bind] is through
   [map] and [flatten]. *)
module Make
    (Interior : Monad.S)
    (Exterior : Monad.S)
    (X : Axiom
     with type 'a interior = 'a Interior.t
      and type 'a exterior = 'a Exterior.t)
  =
  Monad.Make_based_on_map (struct
    type 'a t = 'a Interior.t Exterior.t
    let return  x = Exterior.return (Interior.return x)
    let map   f m = Exterior.map (Interior.map f) m
    let flatten m =
      Exterior.map X.swap m |> Exterior.flatten |> Exterior.map Interior.flatten
  end)

(* As for the previous functor and for the exact same reason, we use
   [Based_on_map_with_product]. *)
module Make_with_product
    (Interior : Monad.S_with_product)
    (Exterior : Monad.S_with_product)
    (X : Axiom
     with type 'a interior = 'a Interior.t
      and type 'a exterior = 'a Exterior.t)
  =
  Monad.Make_based_on_map_with_product (struct
    type 'a t = 'a Interior.t Exterior.t
    let return  x = Exterior.return (Interior.return x)
    let map   f m = Exterior.map (Interior.map f) m
    let flatten m =
      Exterior.map X.swap m |> Exterior.flatten |> Exterior.map Interior.flatten
    let product l r =
      Exterior.product l r |> Exterior.map (fun (l, r) -> Interior.product l r)
  end)
