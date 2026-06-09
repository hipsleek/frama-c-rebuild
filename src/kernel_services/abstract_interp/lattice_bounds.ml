(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)



(** Types definitions *)

type 'a or_bottom = [ `Value of 'a | `Bottom ]
type 'a or_top = [ `Value of 'a | `Top ]
type 'a or_top_bottom = [ `Value of 'a | `Bottom | `Top ]


(* Internal representation of OCaml types above, used for safe unmarshalling.
   Polymorphic variants with parameters are boxed with an extra header word
   to store the value, as compared to normal variants. Polymorphic variants
   with no parameters are unboxed.
   Cf https://ocaml.org/docs/memory-representation *)
let make_structural_descr x =
  Structural_descr.t_sum [| [| Datatype.Int.packed_descr; x |] |]



(** Common functions *)

module Common = struct

  (* Pretty-printing *)

  let pretty_top = Unicode.pp_top
  let pretty_bottom = Unicode.pp_bottom

  let pretty pretty_value fmt = function
    | `Bottom -> pretty_bottom fmt
    | `Top -> pretty_top fmt
    | `Value v -> pretty_value fmt v

  (* Datatype *)

  let hash hash_value = function
    | `Bottom  -> 7
    | `Top -> 13
    | `Value v -> hash_value v

  let equal equal_value x y =
    match x, y with
    | `Bottom, `Bottom -> true
    | `Top, `Top -> true
    | `Value vx, `Value vy -> equal_value vx vy
    | (`Value _ | `Bottom | `Top), (`Value _ | `Bottom | `Top) -> false

  let compare compare_value a b = match a, b with
    | `Bottom, `Bottom -> 0
    | `Bottom, (`Top | `Value _) -> -1
    | (`Top | `Value _), `Bottom -> 1
    | `Top, `Top -> 0
    | `Top, `Value _ -> -1
    | `Value _, `Top -> 1
    | `Value vx, `Value vy -> compare_value vx vy

  (* Tests *)

  let is_bottom = function
    | `Bottom -> true
    | `Value _ | `Top -> false

  let is_top = function
    | `Top -> true
    | `Value _ | `Bottom -> false

  let is_included is_included x y =
    match x, y with
    | `Bottom, #or_top_bottom | #or_top_bottom, `Top -> true
    | #or_top_bottom, `Bottom | `Top, #or_top_bottom -> false
    | `Value vx, `Value vy -> is_included vx vy

  (* Iterator *)

  let iter f = function
    | `Bottom | `Top -> ()
    | `Value v -> f v

  (* Conversion *)

  let to_option = function
    | `Bottom | `Top -> None
    | `Value x -> Some x

end



module Bottom = struct

  include Common

  (** Access *)

  let non_bottom = function
    | `Value v -> v
    | `Bottom  -> assert false

  let value ~bottom = function
    | `Value v -> v
    | `Bottom -> bottom

  (* Lattice operators *)

  let join join x y =
    match x, y with
    | `Value vx, `Value vy -> `Value (join vx vy)
    | `Bottom, (`Value _ as v)
    | (`Value _ as v), `Bottom
    | (`Bottom as v), `Bottom -> v

  let narrow narrow x y =
    match x, y with
    | `Value vx, `Value vy -> narrow vx vy
    | `Bottom, `Value _
    | `Value _, `Bottom
    | `Bottom, `Bottom -> `Bottom

  let join_list f = Stdlib.List.fold_left (join f) `Bottom

  (* Combination *)

  let zip x y =
    match x, y with
    | `Bottom, _ | _, `Bottom -> `Bottom
    | `Value x, `Value y -> `Value (x,y)

  (* Monadic operations *)

  include Monad.Make_based_on_bind_with_product (struct
      type 'a t = 'a or_bottom
      let return x = `Value x
      let bind f = function `Bottom -> `Bottom | `Value x -> f x
      let product l r = zip l r
    end)

  (** Conversion *)

  let of_option = function
    | None -> `Bottom
    | Some v -> `Value v

  (** List operations *)

  let of_list ~join = function
    | [] -> `Bottom
    | x :: list -> `Value (Stdlib.List.fold_left join x list)

  let to_list = function
    | `Bottom  -> []
    | `Value v -> [v]

  let add_to_list elt list =
    match elt with
    | `Bottom    -> list
    | `Value elt -> elt :: list

  let list_values l =
    Stdlib.List.fold_left (fun l elt -> add_to_list elt l) [] l

  let list_filter_map f l =
    let f' x = to_option (f x) in
    Stdlib.List.filter_map f' l

  (** Datatype construction *)

  let counter = ref 0

  module Make_Datatype (X: Datatype.S) =
    Datatype.Make_with_collections
      (struct
        include Datatype.Serializable_undefined
        type t = X.t or_bottom
        let () = incr counter
        let name = X.datatype_name ^ "+bottom(" ^ string_of_int !counter ^ ")"
        let reprs = `Bottom :: Stdlib.List.map (fun v -> `Value v) X.reprs
        let structural_descr = make_structural_descr X.packed_descr
        let hash = Common.hash X.hash
        let equal = (Common.equal X.equal :> t -> t -> bool)
        let compare = Common.compare X.compare
        let rehash = Datatype.identity
        let copy = map X.copy
        let pretty = Common.pretty X.pretty
        let mem_project = Datatype.never_any_project
      end)

  (* Bound lattice *)

  module Bound_Lattice (Lattice: Lattice_type.Join_Semi_Lattice) = struct
    include Make_Datatype (Lattice)
    let bottom = `Bottom
    let join = join Lattice.join
    let is_included = is_included Lattice.is_included
  end

end



module Top = struct

  include Common

  (** Access *)

  let non_top = function
    | `Value v -> v
    | `Top  -> assert false

  let value ~top = function
    | `Value v -> v
    | `Top -> top

  (** Conversion. *)

  let of_option = function
    | None -> `Top
    | Some x -> `Value x

  (** Lattice *)

  let join join_value x y =
    match x, y with
    | `Top, _ | _, `Top -> `Top
    | `Value vx, `Value vy -> join_value vx vy

  let narrow narrow_value x y =
    match x, y with
    | `Top, v | v, `Top -> v
    | `Value vx, `Value vy -> `Value (narrow_value vx vy)

  (** Combination *)

  let zip x y =
    match x, y with
    | `Top, _ | _, `Top -> `Top
    | `Value x, `Value y -> `Value (x,y)

  (** Monadic operators *)

  include Monad.Make_based_on_bind_with_product (struct
      type 'a t = 'a or_top
      let return x = `Value x
      let bind f = function `Top -> `Top | `Value x -> f x
      let product l r = zip l r
    end)

  (** Datatype construction *)

  let counter = ref 0

  module Make_Datatype (X: Datatype.S) =
    Datatype.Make_with_collections
      (struct
        include Datatype.Serializable_undefined
        type t = X.t or_top
        let () = incr counter
        let name = X.datatype_name ^ "+top(" ^ string_of_int !counter ^ ")"
        let reprs = `Top :: Stdlib.List.map (fun v -> `Value v) X.reprs
        let structural_descr = make_structural_descr X.packed_descr
        let hash = Common.hash X.hash
        let equal = (Common.equal X.equal :> t -> t -> bool)
        let compare = Common.compare X.compare
        let rehash = Datatype.identity
        let copy = map X.copy
        let pretty = Common.pretty X.pretty
        let mem_project = Datatype.never_any_project
      end)

  (* Bound lattice *)

  module Bound_Lattice (Lattice: Lattice_type.Join_Semi_Lattice) = struct
    include Make_Datatype (Lattice)
    let top = `Top
    let join = join (fun a b -> `Value (Lattice.join a b))
    let is_included = is_included Lattice.is_included
  end
end



module TopBottom = struct

  type 'a t = 'a or_top_bottom
  include Common

  (** Combination *)

  let zip x y =
    match x, y with
    | `Bottom, #t | #t, `Bottom -> `Bottom
    | `Top, #t | #t, `Top -> `Top
    | `Value x, `Value y -> `Value (x,y)

  (** Monadic operators. We have to redefines every operators to ensure
      subtyping properties. *)

  let return x = `Value x
  let product l r = zip l r

  let bind f = function
    | `Bottom -> `Bottom
    | `Top -> `Top
    | `Value x -> f x

  let map f = function
    | `Bottom -> `Bottom
    | `Top -> `Top
    | `Value x -> `Value (f x)

  let flatten = function
    | `Bottom | `Value `Bottom -> `Bottom
    | `Top | `Value `Top -> `Top
    | `Value `Value x -> `Value x

  module Operators = struct
    let ( >>-  ) (m : [< 'a t]) (f : 'a -> ([> 'b t] as 'c)) : 'c = bind f m
    let ( let* ) (m : [< 'a t]) (f : 'a -> ([> 'b t] as 'c)) : 'c = bind f m
    let ( and* ) (l : [< 'a t]) (r : [< 'b t]) : [> ('a * 'b) t] = product l r
    let ( >>-: ) (m : [< 'a t]) (f : 'a -> 'b) : [> 'b t] = map f m
    let ( let+ ) (m : [< 'a t]) (f : 'a -> 'b) : [> 'b t] = map f m
    let ( and+ ) (l : [< 'a t]) (r : [< 'b t]) : [> ('a * 'b) t] = product l r
  end

  (* Lattice operators *)

  let join join_value x y = match x, y with
    | `Top, _ | _, `Top -> `Top
    | `Bottom, x | x, `Bottom -> x
    | `Value vx, `Value vy -> (join_value vx vy :> 'a t)

  let narrow narrow_value x y = match x, y with
    | `Top, v | v, `Top -> v
    | `Bottom, _ | _, `Bottom -> `Bottom
    | `Value vx, `Value vy -> (narrow_value vx vy :> 'a t)

  (** Datatype construction *)

  let counter = ref 0

  module Make_Datatype (X: Datatype.S) =
    Datatype.Make_with_collections
      (struct
        include Datatype.Serializable_undefined
        type t = X.t or_top_bottom
        let () = incr counter
        let name = X.datatype_name ^ "+top_bottom(" ^ string_of_int !counter ^ ")"
        let reprs = `Bottom :: `Top :: (Stdlib.List.map (fun v -> `Value v) X.reprs)
        let structural_descr = make_structural_descr X.packed_descr
        let hash = Common.hash X.hash
        let equal = (Common.equal X.equal :> t -> t -> bool)
        let compare = Common.compare X.compare
        let rehash = Datatype.identity
        let copy = map X.copy
        let pretty = Common.pretty X.pretty
        let mem_project = Datatype.never_any_project
      end)

end
