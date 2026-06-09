(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let disable_other_analyzers () =
  if Options.Run.get () then
    let selection =
      State_selection.diff
        (Parameter_state.get_selection ())
        (State_selection.union
           (State_selection.of_list
              (Kernel.CodeOutput.self :: Options.states))
           (* The command-line options that govern the creation of the AST
              must be preserved *)
           (State_selection.with_codependencies Ast.self))
    in
    Project.clear ~selection ()

let () = Cmdline.run_after_configuring_stage disable_other_analyzers

let force_run () =
  if not (Dictionary.is_computed ()) then begin
    let old_printer = Printer.current_printer () in
    Obfuscate.obfuscate ();
    if Options.Dictionary.is_default () then Log.print_delayed Dictionary.pretty
    else begin
      let file = Options.Dictionary.get () in
      try
        let cout = open_out file in
        let fmt = Format.formatter_of_out_channel cout in
        Dictionary.pretty fmt
      with Sys_error _ as exn ->
        Options.error
          "@[cannot generate the dictionary into file `%s':@ %s@]"
          file
          (Printexc.to_string exn)
    end;
    File.pretty_ast ();
    Printer.set_printer old_printer
  end

let force_run =
  Dynamic.register
    ~plugin:"Obfuscator"
    "force_run"
    (Datatype.func Datatype.unit Datatype.unit)
    force_run

let run () =
  if Options.Run.get () then begin
    force_run ();
    Cmdline.bail_out () (* stop Frama-C as specified by the -help message
                           and by the discussion in Gitlab issue #491 *)
  end

let () = Boot.Main.extend run
