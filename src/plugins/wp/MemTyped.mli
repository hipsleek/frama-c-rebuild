(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Typed Memory Model                                                 --- *)
(* -------------------------------------------------------------------------- *)

type pointer = NoCast | Fits | Unsafe
val pointer : pointer Context.value

include Memory.Model
