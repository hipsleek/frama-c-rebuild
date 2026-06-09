(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let compute () =
  (* compute RTE annotations, whether Enabled is set or not *)
  Ast.compute () ;
  let include_function kf =
    let fsel = Options.FunctionSelection.get () in
    Kernel_function.Set.is_empty fsel
    || Kernel_function.Set.mem kf fsel
  in
  Globals.Functions.iter
    (fun kf -> if include_function kf then Visit.annotate kf)

let main () =
  (* reset "rte generated" properties for all functions *)
  if Options.Enabled.get () then begin
    Options.feedback ~dkey:Options.dkey_annot ~level:2
      "generating annotations";
    compute ();
    Options.feedback ~dkey:Options.dkey_annot ~level:2
      "annotations computed"
  end

let () = Boot.Main.extend main
