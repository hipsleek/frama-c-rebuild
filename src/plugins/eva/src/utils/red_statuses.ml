(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type alarm_or_property = Alarm of Alarms.t | Prop of Property.t

(* Datatype for [alarm_or_property]. *)
module AlarmOrProp = Datatype.Make_with_collections(struct
    include Datatype.Serializable_undefined

    type t = alarm_or_property

    let reprs = [Alarm (List.hd Alarms.reprs); Prop (List.hd Property.reprs)]

    let name = "Value.Red_statuses.AlarmOrProp"

    let pretty fmt = function
      | Alarm a -> Alarms.pretty fmt a
      | Prop p -> Property.pretty fmt p

    let compare v1 v2 = match v1, v2 with
      | Alarm a1, Alarm a2 -> Alarms.compare a1 a2
      | Prop p1, Prop p2 -> Property.compare p1 p2
      | Alarm _, Prop _ -> -1
      | Prop _, Alarm _ -> 1

    let equal = Datatype.from_compare

    let hash = function
      | Alarm a -> 3 + Alarms.hash a
      | Prop p -> 175 + Property.hash p
  end)

module CallstackOption = Datatype.Option_with_collections (Callstack)
module Callstacks = CallstackOption.Set

(* For each alarm or predicate, stores the set of callstacks for which it was
   evaluated to False. *)
module RedStatuses = AlarmOrProp.Map.Make (Callstacks)

(* Stores the set of red statuses at each program point. *)
module RedStatusesTable =
  Cil_state_builder.Kinstr_hashtbl
    (RedStatuses)
    (struct
      let name = "Value.Red_statuses.RedStatusesTable"
      let size = 16
      let dependencies = [ Self.state ]
    end)

module Hook = Hook.Build (AlarmOrProp)

let register_hook f =
  Hook.extend f

let add_red_ap kinstr ap =
  let current_map =
    try RedStatusesTable.find kinstr
    with Not_found -> AlarmOrProp.Map.empty
  in
  let callstacks =
    try AlarmOrProp.Map.find ap current_map
    with Not_found -> Callstacks.empty
  in
  let new_callstacks = Callstacks.add (Current_callstack.get ()) callstacks in
  let new_map = AlarmOrProp.Map.add ap new_callstacks current_map in
  RedStatusesTable.replace kinstr new_map;
  Hook.apply ap

let add_red_alarm ki a = add_red_ap ki (Alarm a)

let add_red_property ki (ip: Property.identified_property) =
  add_red_ap ki (Prop ip);
  (* If [ip] is an instance of a precondition [ip'], also add [ip'] itself. *)
  match ip with
  | IPPropertyInstance {ii_ip = IPPredicate {ip_kind = PKRequires _} as ip'} ->
    add_red_ap Kglobal (Prop ip')
  | _ -> ()

let is_red ip =
  let kinstr = Property.get_kinstr ip in
  let ap =
    match ip with
    | Property.IPCodeAnnot ica ->
      begin
        match Alarms.find ica.ica_ca with
        | Some a -> Alarm a
        | None -> Prop ip
      end
    | _ -> Prop ip
  in
  try
    let map = RedStatusesTable.find kinstr in
    let callstacks = AlarmOrProp.Map.find ap map in
    not (Callstacks.is_empty callstacks)
  with Not_found -> false

let get_all () =
  let gather kinstr map acc =
    AlarmOrProp.Map.fold
      (fun ap callstacks acc ->
         let number = Callstacks.cardinal callstacks in
         (kinstr, ap, number) :: acc)
      map acc
  in
  RedStatusesTable.fold gather []

(* Information to print on the csv file for each property with a red status. *)
type information = {
  loc: location; (* Source-code location of the property. *)
  kf: kernel_function; (* Function including the property. *)
  alarm: bool;  (* Is the property an Eva alarm or another logic property? *)
  kind: string; (* Name of the property. *)
  text: string; (* Node of the property. *)
  status: Property_status.Feedback.t; (* Final status of the property. *)
  contexts: int; (* Number of contexts in which the property had a red status. *)
}

let kinstr_to_stmt = function
  | Kglobal ->
    let kf = fst (Globals.entry_point ()) in
    kf, Kernel_function.find_first_stmt kf
  | Kstmt stmt -> Kernel_function.find_englobing_kf stmt, stmt

let kf_of_property ip =
  match Property.get_kf ip with
  | Some kf -> kf
  | None -> fst (Globals.entry_point ())

let loc_of_property kf ip =
  let loc = Property.location ip in
  if Fileloc.(equal loc unknown)
  then Kernel_function.get_location kf
  else loc

(* For properties that we want to skip *)
exception Skip

let compute_information (kinstr, alarm_or_prop, contexts) =
  let kf, property, alarm =
    match alarm_or_prop with
    | Alarm alarm ->
      let kf, stmt = kinstr_to_stmt kinstr in
      let loc = Cil_datatype.Stmt.loc stmt in
      let code_annot, _ = Alarms.to_annot kinstr ~loc alarm in
      let property = Property.ip_of_code_annot_single kf stmt code_annot in
      kf, property, true
    | Prop ip -> kf_of_property ip, ip, false
  in
  let kind, text = match Description.property_kind_and_node property with
    | None -> raise Skip
    | Some (kind, text) -> kind, text
  in
  let loc = loc_of_property kf property in
  let status = Property_status.Feedback.get property in
  { loc; kf; alarm; kind; text; status; contexts }

let print_information fmt { loc; kf; alarm; kind; text; status; contexts } =
  let pos = fst loc in
  let path = Filepos.path pos in
  let dir = Filepath.(dirname path |> to_string_rel) in
  let file = Filepath.basename path in
  let lnum = Filepos.line pos in
  let kf = Kernel_function.get_name kf in
  let alarm = if alarm then "Alarm" else "Property" in
  let status = Description.status_feedback status in
  Format.fprintf fmt "@[<h>%s\t%s\t%d\t%s\t%s\t%s\t%i\t%s\t%s@]@,"
    dir file lnum kf alarm kind contexts status text

let output file =
  Self.feedback "Listing red statuses in file %a"
    Filepath.pretty file;
  let channel = open_out (Filepath.to_string_abs file) in
  let fmt = Format.formatter_of_out_channel channel in
  Format.pp_set_margin fmt 1000000;
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt
    "@[directory\tfile\tline\tfunction\tkind\tname\t#contexts\tstatus\tproperty@]@,";
  let list = get_all () in
  let compute e acc = try compute_information e :: acc with Skip -> acc in
  let infos = List.fold_right compute list [] in
  List.iter (print_information fmt) infos;
  Format.fprintf fmt "@]%!"

let report () =
  Parameters.ReportRedStatuses.(if not (is_empty ()) then output (get ()))
