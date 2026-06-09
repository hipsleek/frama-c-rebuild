(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* -------------------------------------------------------------------------- *)
(* --- Annotation Generator                                               --- *)
(* -------------------------------------------------------------------------- *)

(** Generates a predicate characterizing the domain of the l-value. *)
val export_value :
  loc:location -> ?name:string list -> lval -> Results.request -> predicate

(**
   Generates a collection of predicates for each l-value that is read by the
   instruction or the branching condition of the statement. Other kinds of
   statements, like loops, blocks and exceptions are not visited.

   More precisely, for set and call instructions: the written l-values from
   left-hand-side are not visited, but their inner l-values are visited; any
   l-value from the right-hand-side of the instruction is also visited.
*)
val export_stmt :
  ?callstack:Callstack.t -> ?name:string list -> stmt -> predicate list

(** Emitter used for generating domain assertions. *)
val emitter : Emitter.t

(**
   Creates a visitor that can be used to generate new annotations for all
   visited instructions. The generated assertions are associated with the local
   {!emitter}. They are all assigned a valid status by {!Analysis.emitter}.
*)
val generator : unit -> Visitor.frama_c_inplace

(**
   Creates a visitor that can be used to remove all generated annotations from
   {!emitter}. This will also remove their associated status.
*)
val cleaner : unit -> Visitor.frama_c_inplace

(* -------------------------------------------------------------------------- *)
