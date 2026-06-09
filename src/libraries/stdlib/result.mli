(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Adding let binding operators to the Result module.
    @see <https://v2.ocaml.org/manual/bindingops.html>
    This module does not use the generic monad interface (cf. {!Monad}) because
    of the error type, which would require another layer of functors. *)
include module type of Stdlib.Result

(** [zip r1 r2] regroups values in a pair [Ok (v1, v2)] if both arguments are
    [Ok v1] and [Ok v2] and propagate errors in other cases. If both [r1]
    and [r2] are errors we keep the first one, in this case [r1]. *)
val zip : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result

module Operators : sig
  val ( >>-  ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val ( >>-: ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
  val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
  val ( and* ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
  val ( and+ ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
end

(** [value_or_else ~error r] is equivalent to [fold ~ok:Fun.id ~error r]. It is
    similar to {!value} but uses a function to compute the default value.
    @since 33.0-Arsenic
*)
val value_or_else : error:('e -> 'a) -> ('a, 'e) result -> 'a

