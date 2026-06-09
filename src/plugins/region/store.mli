(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type NodeData =
sig
  type 'a t
  val get_id : 'a t -> int
  val set_id : 'a t -> int -> unit
end

module Make (D : NodeData) :
sig
  type node
  type store

  type data = node D.t
  val create : unit -> store

  val store : node -> store
  val fresh : store -> data -> node
  (** Returns a fresh node with the associated data. *)

  val get : node -> data
  val set : node -> data -> unit
  val any : node -> node -> node
  val merge : (data -> data -> data) -> node -> node -> node
  (** Merge the two nodes in the same equivalence class. *)

  val find : node -> node
  (** Returns an equivalent, normalized node *)

  val find_all : node list -> node list
  (** Returns a set of (unique, normalized) nodes *)

  val find_all2 : node list -> node list -> node list
  (** Returns the set of (unique, normalized) nodes from the two lists. *)

  val eq : node -> node -> bool

  val noid : int
  (** Default identifier for [D.t] *)

  val lock : node -> bool
  (** Assigns a unique identifier to the node by [D.set_id].
      Returns [true] if the node has been already locked.
      The underlying store is now locked and no fresh nodes can be created
      nor nodes can not more be merged. *)

  val is_locked : store -> bool

  val id : node -> int
  (** Get the unique identifier of the (locked) node. *)

  val of_id : store -> int -> node
  (** Retrieves the (locked) node associated with the given id. *)

  val pretty : Format.formatter -> node -> unit
  (** Prints '#HHHH' for non-locked nodes or 'Rhhhh' for locked nodes.
      For non-locked nodes, 'HHHH' is the raw rref of the node, for
      locked nodes, 'hhhh' is the unique identifier of the node. *)

  type marks
  val marks : unit -> marks
  val marked : marks -> node -> bool

  (** Returns [true] if the node is already marked and finally mark it. *)
  val test_and_mark : marks -> node -> bool

  (** Returns [true] if the node is already marked and finally mark it.
      The callback is only invoked when the node was {i not} marked. *)
  val once : (node -> unit) -> node -> bool

end
