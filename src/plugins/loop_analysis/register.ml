(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let analyze kf =
  if Kernel_function.is_definition kf
  then
    if Cil_datatype.Stmt.Set.is_empty (Loop.get_non_naturals kf)
    then Loop_analysis.analyze kf
    else
      Options.warning "Could not analyze function %a;@ \
                       it contains a non-natural loop"
        Kernel_function.pretty kf
;;

let main () =
  if Options.Run.get() then begin
    Globals.Functions.iter analyze;
    Loop_analysis.display_results ();
  end
;;

Boot.Main.extend main;;
