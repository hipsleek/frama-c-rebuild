(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extension of OCaml's {!Stdlib.Int} module.
    @since 32.0-Germanium
*)

include module type of Stdlib.Int

(** Compute the greatest common divisor of two ints. *)
val gcd : int -> int -> int
