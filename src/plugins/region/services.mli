(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server
open Request

val package : Package.package

module Node : Data.S with type t = Memory.node
module Range : Output with type t = Memory.range
module Region : Output with type t = Memory.region
