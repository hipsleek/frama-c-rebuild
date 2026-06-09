(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_memory.Types
open Mt_types
open Mt_shared_vars_types

type thread = Thread.t

(** Live threads/taken mutexes at a given point of execution *)

type context = {
  started_threads : ThreadPresence.t;
  locked_mutexes : MutexPresence.t;
}

module Context: sig

  type t = context
  val pretty: t Pretty_utils.formatter

  val empty: t

end


type var_access_kind =
  | NotReallySharedVar
  (** Accesses that have been computed as possibly concurrent by the
      naive analysis, but that are in fact non-concurrent *)
  | SharedVarNonConcurrentAccess
  (** Accesses to a variable that is accessed concurrently, but this
      particular access is non-concurrent (the competing threads
      are not yet or no longer running) *)
  | ConcurrentAccess
  (** Really concurrent access *)


type cfg_concur = {
  concur_accesses: SetZoneAccess.t;
  (** Var accesses at this statement *)

  var_access_kind: var_access_kind;
  (** Does this node contains a concurrent access? We do not distinguish
      the information by zone accessed, as this only used to display the cfg,
      not to compute information. This information is *not* correctly computed
      when the cfg is created, and must be updated later using XXX *)
}


module CfgConcur: sig
  type t = cfg_concur

  val default: t

  val combine: t -> t -> t
  val add_access: rw * Memory_zone.t -> t -> t

  (** See {!CfgNode.must_be_in_cfg} below *)
  val must_be_in_cfg: keep:var_access_kind -> t -> bool
  val has_concur_accesses: t -> bool
end


type node_value_state = {
  state_before: state;
  state_after: state;
}



module NodeValueState: sig
  type t = node_value_state

  val dummy: t

  val threads_presence:
    [> `NotStarted | `Prior | `Started | `MaybeStarted]
    -> Thread.t -> state -> (presence_flag, string) Result.t

  val mutex_presence: Mutex.t -> state -> (presence_flag, string) Result.t

end


type node = {
  cfgn_id : int;
  mutable cfgn_stack: Callstack.t;
  mutable cfgn_var_access: cfg_concur;
  mutable cfgn_kind : node_kind;
  mutable cfgn_preds: node list;
  mutable cfgn_value_state: node_value_state;
  mutable cfgn_context: context;
}
and node_kind =
  | NMT of stmt * Mt_types.events_set * node
  | NInstr of stmt * node
  | NCall of stmt * (Kernel_function.t list * node list)
  | NWholeCall of
      Kernel_function.t * stmt list * Mt_types.events_set * node
  | NWhile of stmt * node
  | NIf of stmt * node * node
  | NSwitch of stmt * exp * node list
  | NJump of jump_type * node
  | NStart of Kernel_function.t * node
  | NEOP
  | NDead
and jump_type =
  | JBreak of stmt
  | JContinue of stmt
  | JGoto of stmt
  | JReturn of stmt
  | JExit of stmt
  | JBlock of stmt
and cfg = node (** Alias for nodes. A cfg is represented by its start node,
                   but it is useful to distinguish between the two in signatures *)


(** Definitions for multithread cfg *)
module CfgNode : sig
  include Datatype.S_with_collections with type t = node

  (** Node with cfgn_kind set to NDead. We always reuse this node,
      as it is never displayed (and it has no successor, thus causing
      no problem in the dataflow analysis) *)
  val dead: t

  (** Fresh node generator. The initial content of the node is
      unspecified. The nodes are guaranteed to be different from
      [dummy] and [dead] *)
  val new_node: Callstack.t -> t


  val node_kind_stmt: node_kind -> stmt list
  val node_stmt: t -> stmt list
  val node_first_loc: t -> Filepos.t option

  val node_kind_succs : node_kind -> t list
  val node_succs: t -> t list


  (** Is there a variable access at this node *)
  val has_concur_accesses: t -> bool

  (** Should we keep a node in the cfg considering the concurrent
      accesses it performs. We keep all accesses with a level equal or
      above [keep].  Note that in the current cfg construction model,
      nodes are labelled with [ConcurrentAccess] or
      [SharedVarNonConcurrentAccess] very late.  Thus,
      [must_be_in_cfg] must not be called with [keep] not equal to
      [NotReallySharedVar] before that. *)
  val must_be_in_cfg: keep:var_access_kind -> t -> bool

  val pretty: t Pretty_utils.formatter
  val pretty_with_stmts: t Pretty_utils.formatter

  val pretty_stmts : t Pretty_utils.formatter

  val pretty_kind: node_kind Pretty_utils.formatter

  val pretty_kind_debug: node_kind Pretty_utils.formatter

  val pretty_kinds_node_list: node list Pretty_utils.formatter


  (** Iteration on all the nodes of an automata reachable from the
      node passed as argument. The order of visit is unspecified. Each node
      is visited exactly once. The function [f_before] is called
      before the children of the node are visited (except in case
      of cycles), while [f_after] is called only after all the
      children have been visited
  *)
  val iter: ?f_before:(t -> unit) -> ?f_after:(t -> unit) -> t -> unit

  (** Same iterator as above, except that the function also receives as
      argument the path between the initial node and the visited node
      (starting from the visited node) *)
  val iter_with_prevs:
    ?f_before:(prevs:t list -> t -> unit) ->
    ?f_after:(prevs:t list -> t -> unit) ->
    t -> unit

end


module NodeIdAccess : Datatype.S with type t = rw * node * thread

module SetNodeIdAccess: sig
  include Lattice_type.Lattice_Set with type O.elt = NodeIdAccess.t

  val pretty_aux:
    NodeIdAccess.t Pretty_utils.formatter -> t Pretty_utils.formatter
end

module AccessesByZoneNode:
  Lmap_bitwise.Location_map_bitwise with type v = SetNodeIdAccess.t
