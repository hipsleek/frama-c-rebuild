(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

open Memory

val add_spec :
  map:map ->
  ?called:stmt ->
  kf:kernel_function ->
  ?ki:kinstr ->
  ?formals:domain Cil_datatype.Varinfo.Map.t ->
  result:node option ->
  spec -> unit

val add_code_annot :
  map:map ->
  kf:kernel_function ->
  stmt:stmt ->
  result:node option ->
  code_annotation -> unit
