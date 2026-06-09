(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Plug-in Implementation                                             --- *)
(* -------------------------------------------------------------------------- *)

let print () =
  Report_parameters.feedback "Computing properties status..." ;
  Log.print_on_output (fun fmt -> Scan.iter (Dump.create fmt))

let print =
  Dynamic.register
    ~plugin:"Report"
    "print"
    (Datatype.func Datatype.unit Datatype.unit)
    print

let print, _ =
  State_builder.apply_once
    "Report.print_once"
    [ Report_parameters.Print.self;
      Report_parameters.PrintProperties.self;
      Report_parameters.Specialized.self;
      Property_status.self ]
    print

let main () = if Report_parameters.Print.get () then print ()

let () =
  Boot.Main.extend main;
