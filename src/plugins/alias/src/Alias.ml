(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


module Analysis = Analysis

module API = API

let main () =
  if Options.Enabled.get() then
    begin
      Analysis.compute ();
      Options.debug "Analysis complete";
    end

let () =
  Boot.Main.extend main
