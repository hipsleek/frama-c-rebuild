(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Encoding of the Peano arithmetic in OCaml type system. A value of type
    [n nat] contains [n] at the value and the type level, allowing to express
    properties on objects sizes and accesses for example. It is used by the
    module {!Linear} to represent vectors and matrices dimensions. *)

type zero = |
type 'n succ = |
type 'n nat
type positive_or_null = PositiveOrNull : 'n nat -> positive_or_null
type strictly_positive = StrictlyPositive : 'n succ nat -> strictly_positive

val zero : zero nat
val one  : zero succ nat
val succ : 'n nat -> 'n succ nat
val prev : 'n succ nat -> 'n nat

(** The call [to_int n] returns an integer equal to n. This function complexity
    is O(1). *)
val to_int : 'n nat -> int

(** Returns a positive or null natural. If the given parameter is strictly
    negative then [None] is returned. This function complexity is O(1). *)
val of_int : int -> positive_or_null option

(** Returns a strictly positive natural. If the given parameter is less or equal
    than zero, then [None] is returned. This function complexity is O(1). *)
val of_strictly_positive_int : int -> strictly_positive option
