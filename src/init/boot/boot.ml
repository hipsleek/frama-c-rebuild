(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Frama-C Entry Point (last linked module).
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)


let () = Frama_c_kernel.Boot.boot ()
(* Implicit exit 0 if we haven't exited yet *)
