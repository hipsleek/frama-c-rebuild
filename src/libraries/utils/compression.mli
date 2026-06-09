(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** File compression. *)

include module type of Gzip

(** Implementation of {!Stdlib.input_value} for a {!Gzip.in_channel}. *)
val input_value : in_channel -> 'a

(** Implementation of {!Stdlib.unsafe_really_input} for a {!Gzip.in_channel}. *)
val unsafe_really_input : in_channel -> bytes -> int -> int -> unit

(** Implementation of {!Stdlib.output_value} for a {!Gzip.out_channel}. *)
val output_value : out_channel -> 'a -> unit
