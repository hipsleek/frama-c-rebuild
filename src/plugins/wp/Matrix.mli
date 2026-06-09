(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Array Dimensions                                                   --- *)
(* -------------------------------------------------------------------------- *)

type t (** Matrix dimensions.
           Encodes the number of dimensions and their kind *)

val of_dims : int option list -> t
val compare : t -> t -> int
val pretty : Format.formatter -> t -> unit
val pp_suffix_id : Format.formatter -> t -> unit

val merge : int option list -> int option list -> int option list option

open Lang.F

type env = {
  size_var : var list ; (** size variables *)
  size_val : term list ; (** size values *)
  index_var : var list ; (** index variables *)
  index_val : term list ; (** index values *)
  index_range : pred list ; (** indices are in range of size variables *)
  index_offset : term list ; (** polynomial of indices *)
  length : term option ; (** number of cells (None is infinite) *)
}

val cc_tau : tau -> t -> tau
(** Type of matrix *)

val cc_env : t -> env
(** Dimension environment *)

val cc_dims : int option list -> term list
(** Value of size variables *)

(* -------------------------------------------------------------------------- *)
