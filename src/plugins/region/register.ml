(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Region Analysis Main Entry Point                                   --- *)
(* -------------------------------------------------------------------------- *)

let () =
  Boot.Main.extend
    begin fun () ->
      if Options.Enabled.get () then
        begin
          Ast.compute () ;
          Globals.Functions.iter Guards.annotate ;
          Options.Enabled.set false ;
        end
    end

(* -------------------------------------------------------------------------- *)
