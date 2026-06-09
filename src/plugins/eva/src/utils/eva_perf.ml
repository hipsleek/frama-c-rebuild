(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Period between two consecutive displays, in seconds. *)
let display_interval = 60.0

(* Do not show functions that execute for less than that percent of
   the total running time. The value is 1/60, i.e. it only displays
   functions that execute for longer than 3s after it has run for
   3 minutes. *)
let threshold = 1.667 /. 100.

(* OCaml time is not always increasing, so we use max to fix this. *)
let duration a b = max (b -. a) 0.0

(* -------------------------------------------------------------------------- *)
(*                                 Flamegraph                                 *)
(* -------------------------------------------------------------------------- *)

(* Ref to the formatter to be written if option [-eva-flamegraph] is set. *)
let flamegraph_output = ref None

(* Sets the reference above according to -eva-flamegraph. *)
let initialize_flamegraph () =
  if not (Parameters.Flamegraph.is_empty ()) then
    try
      let file = Parameters.Flamegraph.get () in
      let out_channel = open_out (Filepath.to_string_abs file) in
      let formatter = Format.formatter_of_out_channel out_channel in
      flamegraph_output := Some formatter;
    with e ->
      Self.error "cannot open flamegraph file: %s" (Printexc.to_string e);
      flamegraph_output := None (* to be on the safe side *)

(* Adds [duration] for the callstack [kf_list] into the flamegraph file. *)
let update_flamegraph kf_list duration =
  let print fmt =
    let pp_callstack = Pretty_utils.pp_list ~sep:";" Kernel_function.pretty in
    Format.fprintf fmt "%a %.3f\n%!"
      pp_callstack (List.rev kf_list) (duration *. 1000.)
  in
  Option.iter print !flamegraph_output

(* -------------------------------------------------------------------------- *)
(*                      Save execution time by callstack                      *)
(* -------------------------------------------------------------------------- *)

(* Statistic about the analysis of a function or a callstack. *)
type stat = {
  nb_calls: int; (* How many times the function has been analyzed. *)
  self_duration: float; (* Time spent analyzing the function itself. *)
  total_duration: float; (* Total time, including functions called. *)
  called: Kernel_function.Hptset.t; (* Set of functions called. *)
}

let empty_stat =
  { nb_calls = 0; self_duration = 0.; total_duration = 0.;
    called = Kernel_function.Hptset.empty; }

module Stat = Datatype.Make (struct
    include Datatype.Serializable_undefined
    type t = stat
    let name = "Eva_perf.Stat"
    let reprs = [ empty_stat ]
  end)

module KfList = Datatype.List_with_collections (Kernel_function)

module StatByCallstack = struct
  module Info = struct
    let name = "Eva_perf.StatByCallstack"
    let dependencies = [ Self.state ]
    let size = 32;
  end

  include State_builder.Hashtbl (KfList.Hashtbl) (Stat) (Info)

  type callstack = KfList.t

  let get kf_list =
    try find kf_list
    with Not_found -> empty_stat

  let add_total stat total =
    { stat with nb_calls = stat.nb_calls + 1;
                total_duration = stat.total_duration +. total; }

  let add_called stat called =
    { stat with called = Kernel_function.Hptset.add called stat.called }

  let add ?called ?total ~self kf_list =
    let stat = get kf_list in
    let stat = { stat with self_duration = stat.self_duration +. self } in
    let stat = Option.fold total ~none:stat ~some:(add_total stat) in
    let stat = Option.fold called ~none:stat ~some:(add_called stat) in
    replace kf_list stat
end

(* Reference to the time at which the analysis of each function from the current
   callstack started. The most recent function is the head of the list.
   Set when starting the analysis of a function, used when leaving the function
   to compute the total time the analysis has taken. *)
let stack_timestamp = ref []

(* Set and used whenever the current analyzed function changes, to compute the
   time spent analyzing each function. *)
let last_timestamp = ref 0.

let duration_since_last_timestamp current_time =
  let duration = duration !last_timestamp current_time in
  last_timestamp := current_time;
  duration

(* Called at the start of the analysis of a callstack. *)
let register_start callstack current_time =
  let kf = Callstack.top_kf callstack in
  let kf_list = List.map fst !stack_timestamp in
  stack_timestamp := (kf, current_time) :: !stack_timestamp;
  match kf_list with
  | [] ->
    last_timestamp := current_time;
    initialize_flamegraph ()
  | kf_list ->
    let last_duration = duration_since_last_timestamp current_time in
    StatByCallstack.add ~called:kf ~self:last_duration kf_list;
    update_flamegraph kf_list last_duration

(* Called at the end of the analysis of a callstack. *)
let register_stop _callstack current_time =
  let kf_list = List.map fst !stack_timestamp in
  match !stack_timestamp with
  | [] -> assert false
  | (_kf, start) :: tl ->
    stack_timestamp := tl;
    let last_duration = duration_since_last_timestamp current_time in
    let total = duration start current_time in
    StatByCallstack.add ~total ~self:last_duration kf_list;
    update_flamegraph kf_list last_duration

(* -------------------------------------------------------------------------- *)
(*                    Compute execution time by function                      *)
(* -------------------------------------------------------------------------- *)

(* Returns the total duration of the analysis. *)
let analysis_duration current_time =
  match !stack_timestamp with
  | [] ->
    (* No analysis ongoing: use the duration of the main function. *)
    let main, _ = Globals.entry_point () in
    let stat = StatByCallstack.get [main] in
    stat.total_duration
  | list ->
    (* Analysis in progress: diff between analysis start and current time. *)
    let analysis_start = List.rev list |> List.hd |> snd in
    duration analysis_start current_time

(* Updates the total duration of [stat] by the current executing time if the
   analysis of [kf] is ongoing. *)
let complete_duration kf stat current_time =
  let eq_kf t = Kernel_function.equal kf (fst t) in
  match List.find_opt eq_kf !stack_timestamp with
  | None -> stat
  | Some (_kf, since) ->
    let total_duration = stat.total_duration +. (duration since current_time) in
    { stat with total_duration }

(* Filters and sorts the [list] of pairs (kf, stat) according to total time. *)
let filter_and_sort ~total_duration ~current_time list =
  let limit = total_duration *. threshold in
  let complete (kf, stat) = kf, complete_duration kf stat current_time in
  let list = List.map complete list in
  let filter (_kf, stat) = stat.total_duration > limit in
  let filtered, others = List.partition filter list in
  let cmp s1 s2 = Float.compare s2.total_duration s1.total_duration in
  List.fast_sort (fun (_, s1) (_, s2) -> cmp s1 s2) filtered, others

let merge_stat s1 s2 =
  { nb_calls = s1.nb_calls + s2.nb_calls;
    self_duration = s1.self_duration +. s2.self_duration;
    total_duration = s1.total_duration +. s2.total_duration;
    called = Kernel_function.Hptset.union s1.called s2.called; }

(* Use recorded stats in StatByCallstack to compute stat by functions,
   regardless of the callstack. Returns a list of the functions with the
   longest total analysis time. Each function is associated to its stat
   and those of all called functions. *)
let compute_stat_by_fun ~total_duration ~current_time =
  let module KfHashtbl = Kernel_function.Hashtbl in
  let stat_by_fun = KfHashtbl.create 60 in
  let stat_by_caller = KfHashtbl.create 60 in
  let find_calls caller =
    KfHashtbl.memo stat_by_caller caller (fun _ -> KfHashtbl.create 32)
  in
  let default = empty_stat in
  let find_stat hashtbl kf = KfHashtbl.find_default ~default hashtbl kf in
  let add_stat hashtbl kf new_stat =
    let old_stat = find_stat hashtbl kf in
    let stat = merge_stat old_stat new_stat in
    KfHashtbl.replace hashtbl kf stat
  in
  let process kf_list stat =
    match kf_list with
    | [] -> ()
    | [ kf ] -> add_stat stat_by_fun kf stat
    | kf :: caller :: _ ->
      add_stat stat_by_fun kf stat;
      add_stat (find_calls caller) kf stat;
  in
  StatByCallstack.iter process;
  let list = KfHashtbl.to_seq stat_by_fun |> List.of_seq in
  let filtered_sorted, _ = filter_and_sort ~total_duration ~current_time list in
  let get_calls kf = find_calls kf |> KfHashtbl.to_seq |> List.of_seq in
  List.map (fun (kf, stat) -> kf, (stat, get_calls kf)) filtered_sorted

(* -------------------------------------------------------------------------- *)
(*                         Print execution feedback                           *)
(* -------------------------------------------------------------------------- *)

(* Prints info on the analysis time of function [kf] given by [stat].
   [calls] is the list of called functions with their own statistics. *)
let print_function fmt current_time (kf, (stat, calls)) =
  let total_duration = stat.total_duration in
  Format.fprintf fmt "* %a: executed: %dx total: %.3fs@,"
    Kernel_function.pretty kf stat.nb_calls total_duration;
  let print_called_functions fmt =
    let sorted, others = filter_and_sort ~total_duration ~current_time calls in
    let nb_others = List.length others in
    let others_duration =
      List.fold_left (fun acc (_, stat) -> acc +. stat.total_duration) 0. others
    in
    let percent duration = 100. *. duration /. total_duration in
    let pp_duration fmt d = Format.fprintf fmt "%.3fs (%.1f%%)" d (percent d) in
    let print_called (kf, stat) =
      Format.fprintf fmt "| %a %dx %a@ "
        Kernel_function.pretty kf stat.nb_calls pp_duration stat.total_duration;
    in
    List.iter print_called sorted;
    if nb_others > 0 then
      Format.fprintf fmt "| %d others: %a@ " nb_others pp_duration others_duration;
    Format.fprintf fmt "| self: %a" pp_duration stat.self_duration
  in
  Format.fprintf fmt "  @[<hov>%t@]@," print_called_functions

(* Prints info on the functions with the longest analysis time. *)
let print_flat ~total_duration ~current_time fmt =
  let list = compute_stat_by_fun ~total_duration ~current_time in
  List.iter (print_function fmt current_time) list

(* Prints execution time as a tree from the main function. *)
let print_tree ~total_duration ~current_time fmt =
  let rec print indent previous_callstack (kf, stat) =
    for _i = 0 to indent-1 do Format.fprintf fmt "| " done;
    Format.fprintf fmt "* %a: executed: %dx total: %.3fs@,"
      Kernel_function.pretty kf stat.nb_calls stat.total_duration;
    let callstack = kf :: previous_callstack in
    let called = Kernel_function.Hptset.elements stat.called in
    let find_stat kf = kf, StatByCallstack.get (kf :: callstack) in
    let calls = List.map find_stat called in
    let sorted, _ = filter_and_sort ~total_duration ~current_time calls in
    List.iter (print (indent+1) callstack) sorted
  in
  let main, _ = Globals.entry_point () in
  let stat = StatByCallstack.get [main] in
  print 0 [] (main, complete_duration main stat current_time)

let show_perf current_time fmt =
  let total_duration = analysis_duration current_time in
  Format.fprintf fmt
    "@[<v> ######## Eva execution feedback ######## \
     @,Long running functions:\
     @,================================================================@,";
  print_flat ~total_duration ~current_time fmt;
  Format.fprintf fmt
    "@,Execution time per callstack:\
     @,================================================================@,";
  print_tree ~total_duration ~current_time fmt;
  Format.fprintf fmt "@,@]"

(* -------------------------------------------------------------------------- *)
(*                            Exported functions                              *)
(* -------------------------------------------------------------------------- *)

let start callstack =
  let current_time = Sys.time () in
  register_start callstack current_time

let last_time_displayed = ref 0.0

let stop callstack =
  let current_time = Sys.time () in
  register_stop callstack current_time;
  (* Export the analysis duration stat, only at the end of the analysis. *)
  if Callstack.is_empty callstack then begin
    let d = analysis_duration current_time in
    Statistics.(set analysis_duration () d)
  end;
  (* Print perfs *)
  if Parameters.ShowPerf.get ()
  && (duration !last_time_displayed current_time) > display_interval
  then begin
    last_time_displayed := current_time;
    Self.feedback "%t" (show_perf current_time)
  end

let display fmt =
  if Parameters.ShowPerf.get ()
  then show_perf (Sys.time ()) fmt

let reset () =
  StatByCallstack.clear ();
  flamegraph_output := None;
  stack_timestamp := [];
  last_timestamp := 0.;
  last_time_displayed := 0.


type 'a by_fun = (Cil_types.kernel_function * 'a) list

let compute_stat_by_fun () : (stat * stat by_fun) by_fun =
  let current_time = Sys.time () in
  let total_duration = analysis_duration current_time in
  compute_stat_by_fun ~total_duration ~current_time
