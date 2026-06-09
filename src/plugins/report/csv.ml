(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Split the location into 'dir,file,line number,char number' in this order *)
let split_pos pos =
  let file = Filepos.path pos in
  let dir = Filepath.(dirname file |> to_string) in
  let file = Filepath.basename file in
  dir, file, Filepos.line pos, Filepos.input_column pos

(* For properties that we want to skip *)
exception Skip

let kf_of_property ip =
  match Property.get_kf ip with
  | Some kf -> kf
  | None -> fst (Globals.entry_point ())

let to_string ip =
  let status = Description.status_feedback (Property_status.Feedback.get ip) in
  let loc = Property.location ip in
  match Description.property_kind_and_node ip with
  | None -> raise Skip
  | Some (kind, txt) ->
    let kf = kf_of_property ip in
    let loc =
      if not (Fileloc.is_known loc) then
        Kernel_function.get_location kf
      else loc
    in
    let loc = split_pos (fst loc) in
    (loc, Kernel_function.get_name kf, kind, status, txt)

(* Compute the lines to export as a .csv, then sorts them *)
let lines () =
  let do_one_ip ip l =
    if Scan.report_ip ip then
      try to_string ip :: l
      with Skip -> l
    else l
  in
  let l = Property_status.fold do_one_ip [] in
  (* This [sort] removes fully identical lines, including identical alarms
     emitted on statements copied through loop unrolling. This is the desired
     semantics for now. However, since we compare entire locations, textually
     identical lines that refer to different expressions are kept separate *)
  List.sort_uniq Stdlib.compare l

let output file =
  let open Filesystem.Operators in
  let$ fmt = Filesystem.with_formatter_exn file in
  Format.pp_set_margin fmt 1000000;
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt
    "@[directory\tfile\tline\tfunction\tproperty kind\tstatus\tproperty@]@ ";
  let pp ((dir, file, lnum, _), kf, kind, status, txt) =
    Format.fprintf fmt "@[<h>%s\t%s\t%d\t%s\t%s\t%s\t%s@]@ "
      dir file lnum kf kind status txt;
  in
  List.iter pp (lines ());
  Format.fprintf fmt "@]%!"


(** Registration of non-free options *)

let print_csv =
  Dynamic.register
    ~plugin:"Report"
    "print_csv"
    (Datatype.func Filepath.ty Datatype.unit)
    output

let print_csv_once () =
  let file = Report_parameters.CSVFile.get () in
  Report_parameters.feedback "Dumping properties in '%a'"
    Filepath.pretty file;
  print_csv file

let print_csv, _ =
  State_builder.apply_once
    "Report.print_csv_once"
    [ Report_parameters.PrintProperties.self;
      Report_parameters.Specialized.self;
      Report_parameters.CSVFile.self;
      Property_status.self ]
    print_csv_once

let main () =
  if not (Report_parameters.CSVFile.is_empty ()) then print_csv ()

let () = Boot.Main.extend main
