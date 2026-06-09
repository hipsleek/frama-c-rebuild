(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Variable Analysis                                                  --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types

(** By lattice order of usage *)
type access =
  | NoAccess (** Never used *)
  | ByRef   (** Only used as ["*x"],   equals to [load(shift(load(&x),0))] *)
  | ByArray (** Only used as ["x[_]"], equals to [load(shift(load(&x),_))] *)
  | ByValue (** Only used as ["x"],    equals to [load(&x)] *)
  | ByAddr  (** Widely used, potentially up to ["&x"] *)

val get : ?kf:kernel_function -> ?init:bool -> varinfo -> access

val iter: ?kf:kernel_function -> ?init:bool -> (varinfo -> access -> unit) -> unit

val is_nullable : varinfo -> bool
(** [is_nullable vi] returns true
    iff [vi] is a formal and has an attribute 'nullable' *)

val has_nullable : unit -> bool
(** [has_nullable ()] return true
    iff there exists a variable that satisfies [is_nullable] *)

val print : varinfo -> access -> Format.formatter -> unit
val dump : unit -> unit
val compute : unit -> unit
val is_computed : unit -> bool
