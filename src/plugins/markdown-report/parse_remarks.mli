(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Parse skeleton files to add manually written comments to various parts
    of the report. *)

(** [get_remarks f] retrieves the elements associated to various sections
    of the report, referenced by their anchor. *)
val get_remarks: Filepath.t -> Markdown.element list Datatype.String.Map.t
