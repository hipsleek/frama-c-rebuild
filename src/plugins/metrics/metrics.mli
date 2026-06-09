(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Metrics plugin. *)

(** See {!Metrics_coverage}. *)
module Metrics_coverage : sig
  val compute_syntactic:
    libc:bool -> Kernel_function.t -> Cil_datatype.Varinfo.Set.t

  (**/**)
  val compute_semantic:
    libc:bool -> Cil_datatype.Varinfo.Set.t
end

(** See {!Metrics_base}. *)
module Metrics_base : sig
  module OptionKf :
    Datatype.S_with_collections with type t = Kernel_function.t option
  module BasicMetrics : sig
    type t = {
      cfile_name : Filepath.t;
      cfunc : Kernel_function.t option;
      cslocs: int;
      cifs: int;
      cloops: int;
      ccalls: int;
      cgotos: int;
      cassigns: int;
      cexits: int;
      cfuncs: int;
      cptrs: int;
      cdecision_points: int;
      cglob_vars: int;
      ccyclo: int;
    }
  end
end

(** See {!Metrics_cilast}. *)
module Metrics_cilast : sig
  val get_metrics_map: libc:bool ->
    (Metrics_base.BasicMetrics.t Metrics_base.OptionKf.Map.t)
      Filepath.Map.t
end
