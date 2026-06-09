(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Options = Reduc_options

let command_line () =
  let annoth = match Options.GenAnnot.get () with
    | "all" -> Collect.AnnotAll
    | "inout" -> Collect.AnnotInout
    | _ -> Options.fatal "Not a valid annotation heuristic"
  in
  annoth

let main () =
  if (Options.Reduc.get ()) then begin
    let annoth = command_line () in
    let env = Alarms.fold Collect.get_relevant (Collect.empty_env annoth) in
    Hyp.generate_hypotheses env;
    ()
  end

let () = Boot.Main.extend main
