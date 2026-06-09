(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Interface of {!Eva_perf} exported in Eva.ml. *)
module type API = sig

  (** Statistic about the analysis time of a function or a callstack. *)
  type stat = {
    nb_calls: int;
    (** How many times the given function or callstack has been analyzed. *)
    self_duration: float;
    (** Time spent analyzing the function or callstack itself. *)
    total_duration: float;
    (** Total time, including the analysis of other functions called. *)
    called: Kernel_function.Hptset.t;
    (** Set of functions called from this function or callstack. *)
  }

  type 'a by_fun = (Cil_types.kernel_function * 'a) list

  (** Returns a list of the functions with the longest total analysis time,
      sorted by decreasing analysis time. Each function [f] is associated to
      its stat and the unsorted list of stats of all function calls from [f]. *)
  val compute_stat_by_fun: unit -> (stat * stat by_fun) by_fun

  (** Statistics about each analyzed callstack. *)
  module StatByCallstack : sig
    type callstack = Cil_types.kernel_function list

    (** Get the current analysis statistic for a callstack. *)
    val get: callstack -> stat

    (** Iterate on the statistic of every analyzed callstack. *)
    val iter: (callstack -> stat -> unit) -> unit

    (** Set a hook on statistics computation *)
    val add_hook_on_change:
      ((callstack, stat) State_builder.hashtbl_event -> unit) -> unit

    (** Sub-signature of [State_builder.Hashtbl] required by the server
        to build synchronized arrays. *)

    type key = Cil_types.kernel_function list
    type data = stat
    module Datatype: Datatype.S
    val add_hook_on_update: (Datatype.t -> unit) -> unit
  end

end
