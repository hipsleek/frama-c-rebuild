(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype
open Memory

type env = {
  map : map ;
  result : node option ; (* where returned value is stored *)
  formals : domain Varinfo.Map.t ;
  context : Access.clause ;
}

val call : map -> logic_info -> domain list -> domain
val cons : map -> logic_ctor_info -> domain list -> domain

val add_addr_lval : loc:location -> env -> ?garbage:bool -> term_lval -> typ * node
val add_term_lval : loc:location -> env -> term_lval -> domain

val add_term : env -> term -> domain
val add_predicate : env -> predicate -> unit
val add_path : env -> Spec.region -> Spec.path -> node
val add_region : env -> Spec.region -> unit
