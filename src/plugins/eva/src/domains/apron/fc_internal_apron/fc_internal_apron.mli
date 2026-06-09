(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module/library is used to avoid polluting the namespace with generic
    names from Apron library
    @since 33.0-Arsenic
*)

include module type of Apron with type 'a Manager.t = 'a Apron.Manager.t
module Box : module type of Box
module Oct : module type of Oct
module Polka : module type of Polka
