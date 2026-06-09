(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {1 Interface for the Region plug-in}

    Each function is assigned a region map. Areas in the map represents l-values
    or, more generally, class of nodes. Regions are classes equivalences of
    nodes that represents a collection of addresses (at some program point).

    Regions can be subdivided into sub-regions. Hence, given two regions, either
    one is included into the other, or they are separated. Hence, given two
    l-values, if their associated regions are separated, then they can {i not}
        be aliased.

    Nodes are elementary elements of a region map. Variables maps to nodes, and
    one can move to one node to another by struct or union field or array
    element. Two disting nodes might belong to the same region. However, it is
    possible to normalize nodes and obtain a unique identifier for all nodes in
    a region.
*)

open Cil_types

(** {2 Memory Maps and Nodes} *)

type map
type node
val map : kernel_function -> map

(** Unique id of normalized node.
    This can be considered the unique identifier of the region equivalence
    class. *)
val id : node -> int
val of_id : map -> int -> node

val pretty : Format.formatter -> node -> unit

(** {2 Region Properties}

    All functions in this section provide normalized nodes
    and shall never raise exception. *)

val points_to : node -> node option
val pointed_by : node -> node list

val size : node -> int
val parents : node -> node list
val cvars : node -> varinfo list
val labels: node -> string list
val reads : node -> typ list
val writes : node -> typ list
val shifts : node -> typ list

val typed : node -> typ option
(** Full-sized cells with unique type access *)

val iter : map -> (node -> unit) -> unit

(** {2 Alias Analysis} *)

(** [equal a b] checks if nodes [a] and [b] are in the same region. *)
val equal : node -> node -> bool

(** [compare a b] compares regions [a] and [b] by their unique id. *)
val compare : node -> node -> int

(** [include a b] checks if region [a] is a sub-region of [b] in map [m]. *)
val included : node -> node -> bool

(** [separated a b] checks if region [a] and region [b] are disjoint.
    Disjoints regions [a] and [b] have the following properties:
    - [a] is {i not} a sub-region of [b];
    - [b] is {i not} a sub-region of [a];
    - two l-values respectively localized in [a] and [b]
      can {i never} be aliased.
*)
val separated : node -> node -> bool


(** [singleton a] returns [true] when node [a] is guaranteed to have only
    one single address in its equivalence class. *)
val singleton : node -> bool

(** [lval m lv] is region where the address of [l] lives in.
    The returned region is normalized.
    @raises Not_found if the l-value is not localized in the map *)
val lval : map -> lval -> node

(** [exp m e] is the domain of values that can computed by expression [e].
    The domain is [Some r] is [e] has a pointer type and any pointer computed by
    [e] lives in region [r]. The domain is [None] if [e] has a non-pointer
    scalar or compound type.
    @raises Not_found if the l-value is not localized in the map
*)
val exp : map -> exp -> node option

(** {2 Low-level Navigation through Memory Maps}

    For optimized access, all the functions in this section return
    unnormalized nodes and may raise [Not_found] for not localized routes. *)

(** Unormalized.
    @raises Not_found *)
val cvar : map -> varinfo -> node

(** Unormalized.
    @raises Not_found *)
val field : node -> fieldinfo -> node

(** Unormalized.
    @raises Not_found *)
val index : node -> typ -> node

(** Normalized list of leaf nodes. *)
val footprint : node -> node list
