(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** generates the report (either final or [draft] according to the flag) *)
val gen_report: draft:bool -> unit -> unit

(** Filled when Eva is loaded. *)
module Eva_info: sig
  val loaded: bool ref
  val computed: (unit -> bool) ref
  val coverage_md_gen: (unit -> Markdown.elements) ref
  val domains_md_gen: (unit -> (Markdown.text * Markdown.text) list) ref
end
