(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Print a warning message when an undefined behavior may occurs in an
   unspecified sequence, i.e. two writes or a write and a read (not used
   for determining the value to write, Cf. C99 6.5§2). We compute an
   over-approximation here but under the assumption that
   it is not possible to access two distinct fields by overflowing
   an index, i.e. s.f[i] is always distinct from s.g[j]
*)
val check_sequences: Cil_types.file -> unit
