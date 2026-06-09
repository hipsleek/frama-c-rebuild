(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let strings
  : varinfo Datatype.String.Hashtbl.t
  = Datatype.String.Hashtbl.create 16

let reset () = Datatype.String.Hashtbl.clear strings

let is_empty () = Datatype.String.Hashtbl.length strings = 0

let add = Datatype.String.Hashtbl.add strings
let find = Datatype.String.Hashtbl.find strings
let fold f = Datatype.String.Hashtbl.fold_sorted f strings
