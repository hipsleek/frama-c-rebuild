(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let main () =
  match Mdr_params.Generate.get () with
  | "none" -> ()
  | "md" -> Md_gen.gen_report ~draft:false ()
  | "draft" -> Md_gen.gen_report ~draft:true ()
  | "sarif" -> Sarif_gen.generate ()
  | s ->
    Mdr_params.fatal "Unexpected value for option %s: %s"
      Mdr_params.Generate.option_name s

let () =
  Boot.Main.extend main
