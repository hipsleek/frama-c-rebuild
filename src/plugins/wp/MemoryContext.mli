(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type validity = Valid | Nullable
type param = NotUsed | ByAddr | ByValue | ByShift | ByRef
           | InContext of validity | InArray of validity

val pp_param: Format.formatter -> param -> unit

type partition

val empty: partition
val set: varinfo -> param -> partition -> partition

val compute: string -> (kernel_function -> partition) -> unit

val add_behavior:
  kernel_function -> string -> (kernel_function -> partition) -> unit
val warn:
  kernel_function -> string -> (kernel_function -> partition) -> unit
