(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Nat

(** Encoding of finite set in OCaml type system. *)

(** The type [n finite] encodes all finite sets of cardinal [n]. It is used by
    the module {!Linear} to represent accesses to vectors and matrices
    coefficients, statically ensuring that no out of bounds access can be
    performed. *)
type 'n finite

(** The first element of any finite subset. The type encodes that for a finite
    subset to have an element, its cardinal must be at least one. *)
val first  : 'n succ finite

(** [last n] returns a value encoding the last element of any
    finite subset of cardinal [n]. *)
val last   : 'n succ nat -> 'n succ finite

(** The call [next f] returns a value encoding the element right after [f] in
    a finite subset. The type encodes the relations between the cardinal of
    the finite subset containing [f] and the cardinal of the one containing
    its successor. *)
val next   : 'n finite -> 'n succ finite

(** If [f] is an element of any finite subset of cardinal [n], it is also an
    element of any finite subset of cardinal [n + 1]. The call [weaken f]
    allows to prove that fact to the type system. *)
val weaken : 'n finite -> 'n succ finite

(** If [f] is an element of any finite subset of cardinal [n + 1], it may
    also be an element of any finite subset of cardinal [n]. The call
    [strengthen n f] allows to prove that fact to the type system. [None]
    is returned if and only if [f] is the last element of its subset. *)
val strengthen : 'n nat -> 'n succ finite -> 'n finite option

(** The call [of_int limit n] returns a finite value representing the nth
    element of a finite set of cardinal limit. If n is not in the bounds, [None]
    is returned. This function complexity is O(1). *)
val of_int : 'n succ nat -> int -> 'n succ finite option

(** The call [to_int n] returns an integer equal to n. This function complexity
    is O(1). *)
val to_int : 'n finite -> int

(** The call [fold f ?start ?stop size acc] folds over each elements between
    [start] and [stop] of a finite set of cardinal [size], computing [f]
    and accumulating its results at each step, starting with [acc].
    The default values of start and stop are respectively [Finite.first]
    and [Finite.last size], i.e by default, [fold] will go through all
    elements of a finite set of cardinal [size].
    The function complexity is O(n). *)
val fold : ('n finite -> 'a -> 'a) ->
  ?start: 'n finite ->
  ?stop: 'n finite ->
  'n nat -> 'a -> 'a

(** The call [iter f ?start ?stop limit] iterates over each elements between
    [start] and [stop] of a finite set of cardinal [size]. As for [fold], the
    default values of [start] and [stop] are respectively [Finite.first] and
    [Finite.last size]. *)
val iter : ('n finite -> unit) ->
  ?start: 'n finite ->
  ?stop: 'n finite ->
  'n nat -> unit

(** The call [for_all f ?start ?stop limit] returns true if and only if [f i]
    is true for all elements [i] between [start] and [stop] of a finite set
    of cardinal [size]. As for [fold], the default values of [start] and [stop]
    are respectively [Finite.first] and [Finite.last size]. If [size] is zero
    or [start] is strictly greater than [stop], the call returns true. *)
val for_all : ('n finite -> bool) ->
  ?start: 'n finite ->
  ?stop: 'n finite ->
  'n nat -> bool

(** {2 Relational operators.} *)

val ( =  ) : 'n finite -> 'n finite -> bool
val ( != ) : 'n finite -> 'n finite -> bool
val ( <  ) : 'n finite -> 'n finite -> bool
val ( <= ) : 'n finite -> 'n finite -> bool
val ( >  ) : 'n finite -> 'n finite -> bool
val ( >= ) : 'n finite -> 'n finite -> bool
