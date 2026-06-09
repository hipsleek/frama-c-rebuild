(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module/library is used to avoid shadowing [Z] from Zarith in our own
    [Z] module.
    @since 33.0-Arsenic
*)

[@@@alert fc_internal_z "Do not use this module unless your are z.ml(i) or ppx_z_literals lib."]

include module type of Z with type t = Z.t
