(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Server Plugin & Options *)

include Plugin.General_services

(** Generate documentation *)
module Doc : Parameter_sig.Filepath

(** Idle waiting time (in ms) *)
module Polling : Parameter_sig.Int

(** Monitor logs *)
module AutoLog : Parameter_sig.Bool

val wpage : warn_category
(** Inconsistent page warning *)

val wkind : warn_category
(** Inconsistent category warning *)

val wname : warn_category
(** Invalid name warning *)

val has_relative_filepath: unit -> bool

val dkey_protocol : category

(**************************************************************************)
