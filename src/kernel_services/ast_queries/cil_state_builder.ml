(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open State_builder
open Cil_datatype

module Stmt_set_ref = Set_ref(Stmt.Set)
module Kinstr_hashtbl = Hashtbl(Kinstr.Hashtbl)
module Stmt_hashtbl = Hashtbl(Stmt.Hashtbl)
module Varinfo_hashtbl = Hashtbl(Varinfo.Hashtbl)
module Exp_hashtbl = Hashtbl(Exp.Hashtbl)
module Kernel_function_hashtbl = Hashtbl(Kf.Hashtbl)
module Lval_hashtbl = Hashtbl(Lval.Hashtbl)

(*
module Code_annotation_hashtbl =
  State_builder.Hashtbl(Cil_datatype.Code_Annotation)
 *)
