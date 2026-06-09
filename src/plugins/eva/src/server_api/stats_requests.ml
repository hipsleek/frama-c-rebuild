(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Server requests about general statistics on the Eva analysis. *)

open Server
open Cil_types

let package =
  let title = "Statistics about Eva analysis" in
  Package.package ~plugin:"eva" ~name:"stats" ~title ()


(* ----- Analysis statistics ------------------------------------------------ *)

module AlarmCategory = struct
  open Server.Data

  module Tags =
  struct
    let dictionary = Enum.dictionary ()

    (* Give a normal representation of the category *)
    let repr =
      let e = List.hd Cil_datatype.Exp.reprs in
      let lv = List.hd Cil_datatype.Lval.reprs in
      let typ = List.hd Cil_datatype.Typ.reprs in
      function
      | Summary.Division_by_zero -> Alarms.Division_by_zero e
      | Memory_access -> Memory_access (lv, For_reading)
      | Index_out_of_bound-> Index_out_of_bound (e, None)
      | Unaligned_pointer -> Unaligned_pointer (e, typ)
      | Invalid_shift -> Invalid_shift (e, None)
      | Overflow -> Overflow (Signed, e, Z.one, Lower_bound)
      | Uninitialized -> Uninitialized lv
      | Dangling -> Dangling lv
      | Nan_or_infinite -> Is_nan_or_infinite (e, FFloat)
      | Float_to_int -> Float_to_int (e, Z.one, Lower_bound)
      | Other -> assert false

    let register alarm_category =
      let name, descr = match alarm_category with
        | Summary.Other -> "other", "Any other alarm"
        | alarm_category ->
          let alarm = repr alarm_category in
          Alarms.(get_short_name alarm, get_description alarm)
      in
      Enum.tag dictionary
        ~name
        ~label:(Markdown.plain name)
        ~descr:(Markdown.plain descr)

    let division_by_zero = register Division_by_zero
    let memory_access = register Memory_access
    let index_out_of_bound = register Index_out_of_bound
    let unaligned_pointer = register Unaligned_pointer
    let invalid_shift = register Invalid_shift
    let overflow = register Overflow
    let uninitialized = register Uninitialized
    let dangling = register Dangling
    let nan_or_infinite = register Nan_or_infinite
    let float_to_int = register Float_to_int
    let other = register Other

    let () = Enum.set_lookup dictionary
        begin function
          | Summary.Division_by_zero -> division_by_zero
          | Memory_access -> memory_access
          | Index_out_of_bound -> index_out_of_bound
          | Unaligned_pointer -> unaligned_pointer
          | Invalid_shift -> invalid_shift
          | Overflow -> overflow
          | Uninitialized -> uninitialized
          | Dangling -> dangling
          | Nan_or_infinite -> nan_or_infinite
          | Float_to_int -> float_to_int
          | Other -> other
        end
  end

  let name = "alarmCategory"
  let descr = Markdown.plain
      "The alarms are counted after being grouped by these categories"
  let data = Request.dictionary ~package ~name ~descr Tags.dictionary

  include (val data : S with type t = Summary.alarm_category)
end

module Coverage =
struct
  open Summary
  type t = coverage
  let jtype = Package.(
      Jrecord [
        "reachable",Jnumber ;
        "dead",Jnumber ;
      ])
  let to_json x = `Assoc [
      "reachable", `Int x.reachable ;
      "dead", `Int x.dead ;
    ]
end

module Events =
struct
  open Summary
  let jtype = Package.(
      Jrecord [
        "errors",Jnumber ;
        "warnings",Jnumber ;
      ])
  let to_json x = `Assoc [
      "errors", `Int x.errors ;
      "warnings", `Int x.warnings ;
    ]
end

module Statuses =
struct
  open Summary
  type t = statuses
  let jtype =
    Data.declare ~package
      ~name:"statusesEntry"
      ~descr:(Markdown.plain "Statuses count.")
      Package.(Jrecord [
          "valid",Jnumber ;
          "unknown",Jnumber ;
          "invalid",Jnumber ;
        ])
  let to_json x = `Assoc [
      "valid", `Int x.valid ;
      "unknown", `Int x.unknown ;
      "invalid", `Int x.invalid ;
    ]
end

module AlarmEntry =
struct
  let jtype =
    Data.declare ~package
      ~name:"alarmEntry"
      ~descr:(Markdown.plain "Alarm count for each alarm category.")
      Package.(Jrecord [
          "category", AlarmCategory.jtype ;
          "count", Jnumber ])
  let to_json (a,c) =  `Assoc [
      "category", AlarmCategory.to_json a ;
      "count", `Int c ]
end

module Alarms =
struct
  type t = (AlarmCategory.t * int) list
  let jtype = Package.Jarray AlarmEntry.jtype
  let to_json x = `List (List.map AlarmEntry.to_json x)
end

module Statistics = struct
  open Summary
  type t = program_stats
  let jtype =
    Data.declare ~package
      ~name:"programStatsType"
      ~descr:(Markdown.plain "Statistics about an Eva analysis.")
      Package.(Jrecord [
          "progFunCoverage",Coverage.jtype ;
          "progStmtCoverage",Coverage.jtype ;
          "progAlarms", Alarms.jtype ;
          "evaEvents",Events.jtype ;
          "kernelEvents",Events.jtype ;
          "alarmsStatuses",Statuses.jtype ;
          "assertionsStatuses",Statuses.jtype ;
          "precondsStatuses",Statuses.jtype ])
  let to_json x = `Assoc [
      "progFunCoverage", Coverage.to_json x.prog_fun_coverage ;
      "progStmtCoverage", Coverage.to_json x.prog_stmt_coverage ;
      "progAlarms", Alarms.to_json x.prog_alarms ;
      "evaEvents", Events.to_json x.eva_events ;
      "kernelEvents", Events.to_json x.kernel_events ;
      "alarmsStatuses", Statuses.to_json x.alarms_statuses ;
      "assertionsStatuses", Statuses.to_json x.assertions_statuses ;
      "precondsStatuses", Statuses.to_json x.preconds_statuses ]
end

let _computed_signal =
  States.register_value ~package
    ~name:"programStats"
    ~descr:(Markdown.plain
              "Statistics about the last Eva analysis for the whole program")
    ~output:(module Statistics)
    ~get:Summary.compute_stats
    ~add_hook:Analysis_requests.register_computation_hook
    ()

let _functionStats =
  let open Summary in
  let model = States.model () in

  States.column model ~name:"fctName"
    ~descr:(Markdown.plain "Function name")
    ~data:(module Data.Jalpha)
    ~get:(fun (kf,_) -> Kernel_function.get_name kf);

  States.column model ~name:"coverage"
    ~descr:(Markdown.plain "Coverage of the Eva analysis")
    ~data:(module Coverage)
    ~get:(fun (_kf,stats) -> stats.fun_coverage);

  States.column model ~name:"alarmCount"
    ~descr:(Markdown.plain "Alarms raised by the Eva analysis by category")
    ~data:(module Alarms)
    ~get:(fun (_kf,stats) -> stats.fun_alarm_count);

  States.column model ~name:"alarmStatuses"
    ~descr:(Markdown.plain "Alarms statuses emitted by the Eva analysis")
    ~data:(module Statuses)
    ~get:(fun (_kf,stats) -> stats.fun_alarm_statuses);

  States.register_framac_array
    ~package
    ~name:"functionStats"
    ~descr:(Markdown.plain
              "Statistics about the last Eva analysis for each function")
    ~key:(fun kf -> Kernel_ast.Decl.index (SFunction kf))
    ~keyType:(Kernel_ast.Decl.jtype)
    model (module FunctionStats)


(* ----- Flamegraph and execution times ------------------------------------- *)

let callstack_to_string kf_list =
  let pp_list = Pretty_utils.pp_list ~sep:":" Kernel_function.pretty in
  Format.asprintf "%a" pp_list (List.rev kf_list)

let _evaFlamegraph =
  let model = States.model () in

  States.column model ~name:"stackNames"
    ~descr:(Markdown.plain "Callstack as functions name list, starting from main")
    ~data:(module Data.Jlist (Data.Jstring))
    ~get:(fun (cs, _) -> List.rev_map Kernel_function.get_name cs);

  States.column model ~name:"nbCalls"
    ~descr:(Markdown.plain "Number of times the callstack has been analyzed")
    ~data:(module Data.Jint)
    ~get:(fun (_cs, stat) -> stat.Eva_perf.nb_calls);

  States.column model ~name:"selfTime"
    ~descr:(Markdown.plain "Computation time for the callstack itself")
    ~data:(module Data.Jfloat)
    ~get:(fun (_cs, stat) -> stat.Eva_perf.self_duration);

  States.column model ~name:"totalTime"
    ~descr:(Markdown.plain "Total computation time, including functions called")
    ~data:(module Data.Jfloat)
    ~get:(fun (_cs, stat) -> stat.Eva_perf.total_duration);

  States.column model ~name:"kfDecl"
    ~descr:(Markdown.plain "Declaration of the top function")
    ~data:(module Kernel_ast.Decl)
    ~get:(fun (cs, _) -> Printer_tag.SFunction (List.hd cs));

  States.register_framac_array
    ~package
    ~name:"flamegraph"
    ~descr:(Markdown.plain "Data for flamegraph: execution times by callstack")
    ~key:callstack_to_string
    model (module Eva_perf.StatByCallstack)
