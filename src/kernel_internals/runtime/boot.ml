(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Main = Hook.Make ()

let toplevel = ref (fun f -> f ())
let set_toplevel run = toplevel := run

let play_analysis () =
  if Kernel.TypeCheck.get () then begin
    if Kernel.Files.get () <> [] || Kernel.TypeCheck.is_set () then begin
      Ast.compute ();
      (* Printing files before anything else (in debug mode only) *)
      if Kernel.debug_atleast 1 && Kernel.is_debug_key_enabled Kernel.dkey_ast
      then File.pretty_ast ()
    end
  end;
  try
    Main.apply ();
    (* Printing code, if required, have to be done at end. Else, we would need
       to provide additional -then options to correctly sequence printing, which
       would be impractical.
       Furthermore, we cannot easily migrate this to Cmdline without making
       exception handling more complex. *)
    if Kernel.PrintCode.get () then File.pretty_ast ();
    Log.treat_deferred_error ();
    (* Easier to handle option -set-project-as-default at the last moment:
       no need to worry about nested [Project.on] *)
    Project.set_keep_current (Kernel.Set_project_as_default.get ());
    (* unset Kernel.Set_project_as_default, but only if it set.
       This avoids disturbing the "set by user" flag. *)
    if Kernel.Set_project_as_default.get () then
      Kernel.Set_project_as_default.off ()
  with Globals.No_such_entry_point msg ->
    Kernel.abort "%s" msg

let boot () =
  (* Main: let's go! *)
  Sys.catch_break true;
  let f () =
    ignore (Project.create "default");
    Cmdline.parse_and_boot
      ~get_toplevel:(fun () -> !toplevel)
      ~play_analysis
  in
  Cmdline.catch_toplevel_run
    ~f
    ~at_normal_exit:Cmdline.run_normal_exit_hook
    ~on_error:Cmdline.run_error_exit_hook
