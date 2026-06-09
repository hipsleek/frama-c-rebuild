(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** In the definitions below, setting argument [libc] to [true] will
    include functions/variables from the C stdlib in the metrics. *)

val compute_syntactic: libc:bool -> Kernel_function.t ->
  Cil_datatype.Varinfo.Set.t
(** List of functions that can be syntactically reached from the function *)

val compute_semantic: libc:bool -> Cil_datatype.Varinfo.Set.t
(** Functions analyzed by the value analysis *)

type coverage_metrics = {
  syntactic: Cil_datatype.Varinfo.Set.t; (** syntactically reachable functions *)
  semantic: Cil_datatype.Varinfo.Set.t; (** semantically reachable functions *)
  initializers: (Cil_types.varinfo * Cil_types.init_or_str) list;  (** initializers *)
}

val percent_coverage : libc:bool -> coverage_metrics -> float

val compute : libc:bool -> coverage_metrics
(** Computes both syntactic and semantic coverage information. *)

(** Computes the semantic coverage by function. *)
val compute_coverage_by_fun: unit -> unit

(** Returns the coverage for a given function. Raises [Not_found] if it has
    not been computed for the function. *)
val get_coverage: Kernel_function.t -> int * int * float

(* Returns [true] if the coverage by function has been computed. *)
val is_computed_by_fun: unit -> bool

val clear_coverage_by_fun: unit -> unit

(** Pretty-printer for syntactic coverage metrics. *)
class syntactic_printer : libc:bool -> Cil_datatype.Varinfo.Set.t -> object
    method pp_reached_from_function: Format.formatter -> Kernel_function.t -> unit
    (** Pretty-print the functions that can be syntactically reached from the
        parameter *)
  end

(** Pretty-printer for semantic coverage metrics. Includes syntactic coverage
    metrics. *)
class semantic_printer : libc:bool -> coverage_metrics -> object
    inherit syntactic_printer
    method pp_unreached_calls: Format.formatter -> unit
    (** Pretty-print semantically unreachable functions that are called by
        semantically reachable functions. *)

    method pp_value_coverage: Format.formatter -> unit
    (** Pretty-print value coverage information, including functions
        syntactically and semantically reachable from the entry point,
        as well as coverage percentage. *)

    method pp_stmts_reached_by_function: Format.formatter -> unit
  end
