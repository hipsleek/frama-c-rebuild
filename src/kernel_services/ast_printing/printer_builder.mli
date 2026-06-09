(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Build a dynamic printer that bind all pretty-printers to the
    object obtained by (P()) *)

module Make_pp
    (_: sig val printer: unit -> Printer_api.extensible_printer_type end):
  Printer_api.S_pp

(** Build a full pretty-printer from a pretty-printing class.
    @since Fluorine-20130401 *)

module Make
    (_: sig class printer: unit -> Printer_api.extensible_printer_type end):
  Printer_api.S
