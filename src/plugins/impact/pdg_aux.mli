(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

open Pdg_types

(** Useful functions that are not directly accessible through the other
    Pdg modules. *)


(** Refinement of a PDG node: we add an indication of which zone is really
    impacted *)
type node = PdgTypes.Node.t * Memory_zone.t

val pretty_node: node Pretty_utils.formatter


(** Sets of pairs [Node.t * Memory_zone.t], with a special semantics for zones:
    [add n z (add n z' empty)] results in [(n, Memory_zone.join z z')] instead
    of a set with two different elements. All operations see only  instance
    of a node, with the join of all possible zones. Conversely, a node should
    not be present with an empty zone. *)
module NS: sig
  include Datatype.S

  val empty: t
  val is_empty: t -> bool
  val pretty: t Pretty_utils.formatter

  val add': node -> t -> t

  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t

  val remove: PdgTypes.Node.t -> t -> t

  val mem: PdgTypes.Node.t -> t -> bool
  val mem': node -> t -> bool
  val intersects: t -> t -> bool
  val for_all': (node -> bool) -> t -> bool

  val iter': (node -> unit) -> t -> unit
  val fold: (node -> 'a -> 'a) -> t -> 'a -> 'a
  val filter': (node -> bool) -> t -> t
end


(** Abstract view of a call frontier. An element [n, S] of the list
    is such that [n] is impacted if one of the nodes of [S] is impacted. *)
type call_interface = (PdgTypes.Node.t * NS.t) list


(** [all_call_input_nodes caller callee call_stmt] find all the nodes
    above [call_stmt] in the pdg of [caller] that define the inputs
    of [callee]. Each input node in [callee] is returned with the set
    of nodes that define it in [caller].  *)
val all_call_input_nodes:
  caller:Pdg.Api.t ->  callee:kernel_function * Pdg.Api.t -> stmt ->
  call_interface

(** [all_call_out_nodes ~callee ~caller stmt] find all the nodes of [callee]
    that define the Call/Out nodes of [caller] for the call to [callee]
    that occurs at [stmt]. Each such out node is returned, with the set
    of nodes that define it in [callee] *)
val all_call_out_nodes :
  callee:Pdg.Api.t ->  caller:Pdg.Api.t -> stmt -> call_interface
