(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Basic signature with all monadic functions *)
module type Basic = sig
  type 'a t
  val return : 'a -> 'a t
  val flatten : 'a t t -> 'a t
  val map  : ('a -> 'b  ) -> 'a t -> 'b t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

(* Complete signature *)
module type S = sig
  include Basic
  module Operators : sig
    val ( >>-  ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>-: ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

(* Complete signature with a product *)
module type S_with_product = sig
  include Basic
  val product : 'a t -> 'b t -> ('a * 'b) t
  module Operators : sig
    val ( >>-  ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    val ( >>-: ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end


(* Minimal signature based on bind *)
module type Based_on_bind = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

(* Minimal signature based on bind with product *)
module type Based_on_bind_with_product = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
end

(* Minimal definition based on map *)
module type Based_on_map = sig
  type 'a t
  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val flatten : 'a t t -> 'a t
end

(* Minimal signature based on map with product *)
module type Based_on_map_with_product = sig
  type 'a t
  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val flatten : 'a t t -> 'a t
  val product : 'a t -> 'b t -> ('a * 'b) t
end


(* Basic based on bind signature *)
module Basic_based_on_bind (M : Based_on_bind) = struct
  type 'a t = 'a M.t
  let return x = M.return x
  let bind f m = M.bind f m
  let flatten m = bind (fun x -> x) m
  let map f m = bind (fun x -> return (f x)) m
end

(* Basic based on map signature *)
module Basic_based_on_map (M : Based_on_map) = struct
  type 'a t = 'a M.t
  let return x = M.return x
  let map f m = M.map f m
  let flatten m = M.flatten m
  let bind f m = flatten (map f m)
end

(* Make operators from extended signatures *)
module Make_operators (M : Basic) = struct
  let ( >>-  ) m f = M.bind f m
  let ( let* ) m f = M.bind f m
  let ( >>-: ) m f = M.map  f m
  let ( let+ ) m f = M.map  f m
end



(* Extend a basic monad based on bind minimal monad *)
module Make_based_on_bind (M : Based_on_bind) = struct
  module Basic = Basic_based_on_bind (M)
  module Operators = Make_operators (Basic)
  include Basic
end

(* Extend a basic monad based on map minimal monad *)
module Make_based_on_map (M : Based_on_map) = struct
  module Basic = Basic_based_on_map (M)
  module Operators = Make_operators (Basic)
  include Basic
end

(* Extend a basic monad based on bind monad with a product *)
module Make_based_on_bind_with_product (M : Based_on_bind_with_product) = struct
  include Make_based_on_bind (M)
  let product = M.product
  module Operators = struct
    include Operators
    let ( and* ) l r = product l r
    let ( and+ ) l r = product l r
  end
end

(* Extend a basic monad based on map monad with a product *)
module Make_based_on_map_with_product (M : Based_on_map_with_product) = struct
  include Make_based_on_map (M)
  let product = M.product
  module Operators = struct
    include Operators
    let ( and* ) l r = product l r
    let ( and+ ) l r = product l r
  end
end


(* Monadic iterators signature *)
module type Iterators = sig
  type 'a iterable
  type 'a monad
  val fold : ('a -> 'b -> 'a monad) -> 'a -> 'b iterable -> 'a monad
  val map  : ('a -> 'b monad) -> 'a iterable -> 'b iterable monad
  val iter : ('a -> unit monad) -> 'a iterable -> unit monad
end
