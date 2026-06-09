(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Subgraph from a given vertex *)
module Make
    (G: sig
       (** Graph datastructure *)
       include Graph.Sig.G
       val create: ?size:int -> unit -> t
       val add_edge_e: t -> E.t -> unit
     end)
    (_: Datatype.S with type t = G.t(* Graph datatype *))
    (_: sig
       (** additional information *)
       val self: State.t
       val name: string
       (** name of the state *)

       val get: unit -> G.t
       val vertex: Kernel_function.t -> G.V.t
     end) :
sig
  val get: unit -> G.t
  val self: State.t
end
