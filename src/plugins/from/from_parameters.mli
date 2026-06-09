(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

(** Option -deps *)
module ForceDeps: Parameter_sig.Bool

(** Option -calldeps.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module ForceCallDeps: Parameter_sig.Bool

(** Option -show-indirect-deps *)
module ShowIndirectDeps: Parameter_sig.Bool

(** Option -from-verify-assigns. *)
module VerifyAssigns: Parameter_sig.Bool
