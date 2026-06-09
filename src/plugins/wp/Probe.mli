(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type probe = private {
  id : int;
  name : string ;
  stmt : stmt option ;
  loc : location ;
}

val annotations : stmt -> (string * term) list
val create : loc:location -> ?stmt:stmt -> name:string -> unit -> probe

include Datatype.S_with_collections with type t = probe

(**************************************************************************)
