(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type node

type root = Root of {
    ip : Property.t ;
    named : string ;
    typ : typ ; ptr : term ; inf : term ; sup : term ;
    flags : Attr.flags ;
  }

type cvar = Cvar of {
    cvar : varinfo ;
    label : string ;
    cells : int ;
  }

type range = Range of {
    label : string ;
    offset : int ;
    length : int ;
    cells : int ;
    data : node ;
  }

type region = {
  node: node ;
  parents: node list ;
  cresult: bool ;
  cvars: cvar list ;
  roots: root list ;
  labels: string list ;
  types: typ list ;
  typed : typ option ;
  fields: Fields.domain ;
  flags : Attr.flags ;
  reads: Access.acs list ;
  writes: Access.acs list ;
  inits: Access.acs list ;
  shifts: Access.acs list ;
  sizeof: int ;
  singleton : bool ;
  ranges: range list ;
  pointed: node option ;
}

type domain = node Domain.t
type context = node Domain.context

type map

val pp_node : Format.formatter -> node -> unit
val pp_root : Format.formatter -> root -> unit
val pp_region : Format.formatter -> region -> unit

(** Initially unlocked. *)
val create : unit -> map

(** Lock the map. No more access nor merge can be added into the map. *)
val lock : map -> unit

(** The underlying map must have been lock to get unique identifiers. *)
val id : node -> int

(** Provided the map is locked and the id exists. *)
val of_id : map -> int -> node

val equal : node -> node -> bool
val find : node -> node
val find_all : node list -> node list

val size : node -> int
val parents : node -> node list
val cvars : node -> varinfo list
val labels : node -> string list
val region : node -> region
val regions : map -> region list
val iter : map -> (node -> unit) -> unit

val fresh : map -> node
val add_cvar : map -> ?garbage:bool -> Cil_types.varinfo -> node
val add_lvar : map -> Cil_types.logic_var -> domain
val add_logic : map -> Cil_types.logic_info -> domain
val add_result : map -> node
val add_label : map -> string -> node
val add_root : map -> node -> root -> unit
val add_field : node -> fieldinfo -> node
val add_field_range : node -> fieldinfo -> fieldinfo -> node
val add_index : node -> typ -> node
val add_points_to : node -> node -> unit
val add_value : node -> typ -> node option

val add_read : node -> Access.acs -> unit
val add_write : node -> Access.acs -> unit
val add_shift : node -> Access.acs -> typ -> unit
val add_init : node -> Access.acs -> typ -> unit

val domain_of_typ : map -> typ -> domain
val domain_of_ltyp : map -> ?ctxt:context -> logic_type -> domain

val merge : node -> node -> unit
val merge_all : node list -> unit

val pure : domain
val merge_domain : domain -> domain -> domain
val merge_points_to : domain -> node option

val cvar : map -> varinfo -> node
val lvar : map -> logic_var -> domain
val logic : map -> logic_info -> domain
val field : node -> fieldinfo -> node
val index : node -> typ -> node
val lval : map -> lval -> node
val exp : map -> exp -> node option
val result : map -> node option
val garbage : map -> varinfo -> bool

val ranges : node -> range list
val points_to : node -> node option
val pointed_by : node -> node list

val footprint : node -> node list

val included : node -> node -> bool
val separated : node -> node -> bool
val singleton : node -> bool

val reads : node -> typ list
val writes : node -> typ list
val shifts : node -> typ list
val inits : node -> typ list
val types : node -> typ list
val typed : node -> typ option
val flags : node -> Attr.flags

(**/**)
val body : (map -> logic_info -> domain -> unit) ref
[@@alert internal]
(**/***)
