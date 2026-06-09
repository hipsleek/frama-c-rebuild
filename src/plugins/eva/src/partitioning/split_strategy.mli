(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type split_strategy =
  | NoSplit
  | SplitAuto
  | SplitEqList of Z.t list
  | FullSplit

include Parameter_sig.Value_datatype with type t = split_strategy
