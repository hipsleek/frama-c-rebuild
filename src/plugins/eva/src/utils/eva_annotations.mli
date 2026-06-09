(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Register special annotations to locally guide the Eva analysis:

    - slevel annotations: "slevel default", "slevel merge" and "slevel i"
    - loop unroll annotations: "loop unroll term"
    - value partitioning annotations: "split term" and "merge term"
    - subdivision annotations: "subdivide i"
*)

(* Note: widen hints annotations are still registered in !{widen_hints_ext.ml}. *)

(** Annotations tweaking the behavior of the -eva-slevel parameter. *)
type slevel_annotation =
  | SlevelMerge        (** Join all states separated by slevel. *)
  | SlevelDefault      (** Use the limit defined by -eva-slevel. *)
  | SlevelLocal of int (** Use the given limit instead of -eva-slevel. *)
  | SlevelFull         (** Remove the limit of number of separated states. *)

(** Loop unroll annotations. *)
type unroll_annotation =
  | UnrollAmount of Cil_types.term (** Unroll the n first iterations. *)
  | UnrollFull (** Unroll amount defined by -eva-default-loop-unroll. *)
  | UnrollAuto of int (** Use the automatic loop unrolling with the given limit,
                          as if -eva-auto-loop-unroll N was locally set. *)

type split_kind = Static | Dynamic

(** Trace partitioning splits. *)
type split_term =
  | Term of Cil_types.term (* Split according an ACSL term. *)
  | Predicate of Cil_types.predicate (* Split according an ACSL predicate. *)
  | ConditionalCases (* Split according to a conditional statement. *)

(** Split/merge annotations for value partitioning.  *)
type flow_annotation =
  | FlowSplit of split_term * split_kind
  (** Split states according to a term. *)
  | FlowMerge of split_term
  (** Merge states separated by a previous split. *)

type allocation_kind = By_stack | Fresh | Fresh_weak | Imprecise

type array_segmentation =
  Cil_types.varinfo * Cil_types.offset * Cil_types.exp list

type domain_scope =
  string (* domain *) *
  Cil_types.varinfo list (* variables that must be tracked by the domain *)

val get_slevel_annot : Cil_types.stmt -> slevel_annotation option
val get_unroll_annot : Cil_types.stmt -> unroll_annotation list
val get_flow_annot : Cil_types.stmt -> flow_annotation list
val get_subdivision_annot : Cil_types.stmt -> int list
val get_allocation: Cil_types.stmt -> allocation_kind

val add_slevel_annot : emitter:Emitter.t ->
  Cil_types.stmt -> slevel_annotation -> unit
val add_unroll_annot : emitter:Emitter.t ->
  Cil_types.stmt -> unroll_annotation -> unit
val add_flow_annot : emitter:Emitter.t ->
  Cil_types.stmt -> flow_annotation -> unit
val add_subdivision_annot : emitter:Emitter.t ->
  Cil_types.stmt -> int -> unit
val add_array_segmentation : emitter:Emitter.t ->
  Cil_types.stmt -> array_segmentation -> unit
val add_domain_scope : emitter:Emitter.t ->
  Cil_types.stmt -> domain_scope -> unit

val read_array_segmentation : Cil_types.acsl_extension -> array_segmentation
val read_domain_scope : Cil_types.acsl_extension -> domain_scope
