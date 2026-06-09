(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* ************************************************************************* *)
(** {2 Security parameters} *)
(* ************************************************************************* *)

include Plugin.S

module Slicing: Parameter_sig.Bool
(** Perform the security slicing pre-analysis. *)
