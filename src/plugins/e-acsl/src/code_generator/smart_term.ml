(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let tsizeof ?(smart = true) ?(loc = Fileloc.unknown) typ =
  if smart then
    try Logic_const.tint ~loc @@ Z.of_int @@ Cil.bytesSizeOf typ
    with Cil.SizeOfError _ -> Logic_const.term ~loc (TSizeOf typ) Linteger
  else Logic_const.term ~loc (TSizeOf typ) Linteger

let talignof ?(smart = true) ?(loc = Fileloc.unknown) typ =
  if smart then
    try Logic_const.tint ~loc @@ Z.of_int @@ Cil.bytesAlignOf typ
    with Cil.SizeOfError _ -> Logic_const.term ~loc (TAlignOf typ) Linteger
  else Logic_const.term ~loc (TAlignOf typ) Linteger

let copy ?(smart = true) t =
  if smart then
    match t.term_node with
    | TSizeOf typ -> tsizeof ~loc:t.term_loc typ
    | TAlignOf typ -> talignof ~loc:t.term_loc typ
    | _ -> Misc.Id_term.deep_copy t
  else Misc.Id_term.deep_copy t
