(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val (<?>) : int -> int lazy_t -> int



module Result : sig
  include Monad.S

  val ok : 'a -> 'a t
  val warning : 'a -> ('r, Format.formatter, unit, 'a t) format4 -> 'r
  val error : ('r, Format.formatter, unit, 'a t) format4 -> 'r

  val log : error : 'a -> 'a t -> 'a
  val value : 'a t -> 'a
end



type trilean = True | False | Unknown

module Trilean : sig
  include Datatype.S_with_collections with type t = trilean
  val top : t
  val is_included : t -> t -> bool
  val intersects : t -> t -> bool
  val join : t -> t -> t
  val narrow : t -> t -> t
  val maybe_true  : t -> bool
  val maybe_false : t -> bool
  val of_bool : bool -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  val not : t -> t
end



module Value : sig
  include module type of (Cvalue.V)
  val zero : t
  val of_int : int -> t
  val to_int_list : t -> int list Result.t
  val extract_singleton : t -> int option
  val extract_fun : t -> Cil_types.kernel_function list Result.t
end
