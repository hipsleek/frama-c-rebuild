(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Pdg_types

type t = PdgTypes.Pdg.t
(** Program Dependence Graph type *)

type t_nodes_and_undef =
  ((PdgTypes.Node.t * Memory_zone.t option) list * Memory_zone.t option)
(** type for the return value of many [find_xxx] functions when the
    answer can be a list of [(node, z_part)] and an [undef zone].
    For each node, [z_part] can specify which part of the node
    is used in terms of zone ([None] means all). *)

(**************************************************************************)

exception Bottom
(** Raised by most function when the PDG is Bottom because we can hardly do
    nothing with it. It happens when the function is unreachable because we
    have no information about it. *)

exception Top
(** Raised by most function when the PDG is Top because we can hardly do
    nothing with it. It happens when we didn't manage to compute it, for
    instance for a variadic function. *)

(**************************************************************************)

val self : State.t
(** PDG dependency state *)

(**************************************************************************)

(** {3 Getters} *)

val get : Cil_types.kernel_function -> t
(** Get the PDG of a function. Build it if it doesn't exist yet. *)

val node_key : PdgTypes.Node.t -> PdgIndex.Key.t

val from_same_fun : t -> t -> bool

(**************************************************************************)

(** {3 Finding PDG nodes} *)

val find_decl_var_node : t -> Cil_types.varinfo -> PdgTypes.Node.t
(** Get the node corresponding the declaration of a local variable or a
    formal parameter.
    @raise Not_found if the variable is not declared in this function.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_ret_output_node : t -> PdgTypes.Node.t
(** Get the node corresponding return stmt.
    @raise Not_found if the output state in unreachable
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_output_nodes : t -> PdgIndex.Signature.out_key -> t_nodes_and_undef
(** Get the nodes corresponding to a call output key in the called pdg.
    @raise Not_found if the output state in unreachable
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_input_node : t -> int -> PdgTypes.Node.t
(** Get the node corresponding to a given input (parameter).
    @raise Not_found if the number is not an input number.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_all_inputs_nodes : t -> PdgTypes.Node.t list
(** Get the nodes corresponding to all inputs.
    {!node_key} can be used to know their numbers.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_stmt_node : t -> Cil_types.stmt -> PdgTypes.Node.t
(** Get the node corresponding to the statement.
    It shouldn't be a call statement.
    See also {!find_simple_stmt_nodes} or {!find_call_stmts}.
    @raise Not_found if the given statement is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top.
    @raise PdgIndex.CallStatement if the given stmt is a function
    call. *)


val find_simple_stmt_nodes : t -> Cil_types.stmt -> PdgTypes.Node.t list
(** Get the nodes corresponding to the statement.
    It is usually composed of only one node (see {!find_stmt_node}),
    except for call statement.
    Be careful that for block statements, it only returns a node
    corresponding to the elementary stmt
                       (see {!find_stmt_and_blocks_nodes} for more)
    @raise Not_found if the given statement is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_label_node : t -> Cil_types.stmt -> Cil_types.label -> PdgTypes.Node.t
(** Get the node corresponding to the label.
    @raise Not_found if the given label is not in the PDG.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_stmt_and_blocks_nodes : t -> Cil_types.stmt -> PdgTypes.Node.t list
(** Get the nodes corresponding to the statement like
    {!find_simple_stmt_nodes} but also add the nodes of the enclosed
    statements if [stmt] contains blocks.
    @raise Not_found if the given statement is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_top_input_node : t -> PdgTypes.Node.t
(** @raise Not_found if there is no top input in the PDG.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_entry_point_node : t -> PdgTypes.Node.t
(** Find the node that represent the entry point of the function, i.e. the
    higher level block.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_location_nodes_at_stmt : t -> Cil_types.stmt -> before:bool ->
  Memory_zone.t -> t_nodes_and_undef
(** Find the nodes that define the value of the location at the given
    program point. Also return a zone that might be undefined at that point.
    @raise Not_found if the given statement is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_location_nodes_at_end : t -> Memory_zone.t -> t_nodes_and_undef
(** Same than {!find_location_nodes_at_stmt} for the program point located
    at the end of the function.
    @raise Not_found if the output state is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_location_nodes_at_begin : t -> Memory_zone.t -> t_nodes_and_undef
(** Same than {!find_location_nodes_at_stmt} for the program point located
    at the beginning of the function.
    Notice that it can only find formal argument nodes.
    The remaining zone (implicit input) is returned as undef.
    @raise Not_found if the output state is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_call_stmts : Cil_types.kernel_function ->
  caller:Cil_types.kernel_function -> Cil_types.stmt list
(** Find the call statements to the function (can maybe be somewhere
    else).
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_call_ctrl_node : t ->  Cil_types.stmt -> PdgTypes.Node.t
(** @raise Not_found if the call is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_call_input_node : t ->  Cil_types.stmt -> int -> PdgTypes.Node.t
(** @raise Not_found if the call is unreachable or has no such input.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_call_output_node : t ->  Cil_types.stmt -> PdgTypes.Node.t
(** @raise Not_found if the call is unreachable or has no output node.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val find_code_annot_nodes : t -> Cil_types.stmt ->
  Cil_types.code_annotation ->
  PdgTypes.Node.t list * PdgTypes.Node.t list * (t_nodes_and_undef option)
(** The result is composed of three parts :
    - the first part of the result are the control dependencies nodes
      of the annotation,
    - the second part is the list of declaration nodes of the variables
      used in the annotation;
    - the third part is similar to [find_location_nodes_at_stmt] result
      but  for all the locations needed by the annotation.
      When the third part  is globally [None],
      it means that we were not able to compute this information.
      @raise Not_found if the statement is unreachable.
      @raise Bottom if given PDG is bottom.
      @raise Top if the given pdg is top. *)

val find_fun_precond_nodes : t -> Cil_types.predicate ->
  PdgTypes.Node.t list * (t_nodes_and_undef option)
(** Similar to [find_code_annot_nodes] (no control dependencies nodes) *)

val find_fun_postcond_nodes : t -> Cil_types.predicate ->
  PdgTypes.Node.t list * (t_nodes_and_undef option)
(** Similar to [find_fun_precond_nodes] *)

val find_fun_variant_nodes : t -> Cil_types.term ->
  PdgTypes.Node.t list * (t_nodes_and_undef option)
(** Similar to [find_fun_precond_nodes] *)

(**************************************************************************)

(** {3 Propagation}
    See also [Pdg.mli] for more function that cannot be here because
    they use polymorphic types. **)

val find_call_out_nodes_to_select :
  t -> PdgTypes.NodeSet.t -> t ->  Cil_types.stmt -> PdgTypes.Node.t list
(** [find_call_out_nodes_to_select pdg_called called_selected_nodes
    pdg_caller call_stmt]
    @return the call outputs nodes [out] such that
    [find_output_nodes pdg_called out_key]
    intersects [called_selected_nodes]. *)

val find_in_nodes_to_select_for_this_call :
  t -> PdgTypes.NodeSet.t -> Cil_types.stmt -> t -> PdgTypes.Node.t list
(** [find_in_nodes_to_select_for_this_call
    pdg_caller caller_selected_nodes call_stmt pdg_called]
    @return the called input nodes such that the corresponding nodes
    in the caller intersect [caller_selected_nodes]
    @raise Not_found if the statement is unreachable.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

(**************************************************************************)

(** {3 Dependencies} *)

val direct_dpds :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** Get the nodes to which the given node directly depend on.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val direct_ctrl_dpds :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** Similar to {!direct_dpds}, but for control dependencies only.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val direct_data_dpds :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** Similar to {!direct_dpds}, but for data dependencies only.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val direct_addr_dpds :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** Similar to {!direct_dpds}, but for address dependencies only.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val all_dpds :  t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
(** Transitive closure of {!direct_dpds} for all the given nodes.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val all_data_dpds :  t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
(** Gives the data dependencies of the given nodes, and recursively, all
    the dependencies of those nodes (regardless to their kind).
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val all_ctrl_dpds :  t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
(** Similar to {!all_data_dpds} for control dependencies.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val all_addr_dpds :  t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
(** Similar to {!all_data_dpds} for address dependencies.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val direct_uses :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** build a list of all the nodes that have direct dependencies on the
    given node.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val direct_ctrl_uses :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** Similar to {!direct_uses}, but for control dependencies only.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val direct_data_uses :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** Similar to {!direct_uses}, but for data dependencies only.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val direct_addr_uses :  t -> PdgTypes.Node.t -> PdgTypes.Node.t list
(** Similar to {!direct_uses}, but for address dependencies only.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val all_uses :  t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
(** build a list of all the nodes that have dependencies (even indirect) on
    the given nodes.
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val custom_related_nodes : (PdgTypes.Node.t -> PdgTypes.Node.t list) ->
  PdgTypes.Node.t list -> PdgTypes.Node.t list
(** [custom_related_nodes get_dpds node_list] build a list, starting from
    the node in [node_list], and recursively add the nodes given by the
    function [get_dpds].  For this function to work well, it is important
    that [get_dpds n] returns a subset of the nodes directly related to
    [n], ie a subset of [direct_uses] U [direct_dpds].
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

val iter_nodes : (PdgTypes.Node.t -> unit) -> t -> unit
(** apply a given function to all the PDG nodes
    @raise Bottom if given PDG is bottom.
    @raise Top if the given pdg is top. *)

(**************************************************************************)

(** {3 Printers} *)

val extract : t -> string -> unit
(** Pretty print pdg into a dot file. *)

val pretty_node : bool -> Format.formatter -> PdgTypes.Node.t -> unit
(** Pretty print information on a node : with [short=true], only the id
      of the node is printed.. *)

val pretty_key : Format.formatter -> PdgIndex.Key.t -> unit
(** Pretty print information on a node key *)

val pretty : ?bw:bool -> Format.formatter -> t -> unit
(** For debugging... Pretty print pdg information.
    Print codependencies rather than dependencies if [bw=true]. *)

(**************************************************************************)

module Marks : module type of Marks

(**************************************************************************)
