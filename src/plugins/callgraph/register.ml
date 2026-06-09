(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let main () =
  if not (Options.Filename.is_empty ()) then
    if Options.Services.get () then begin
      if not (Services.is_computed ()) then Services.dump ()
    end else
    if not (Cg.is_computed ()) then Cg.dump ()

let () = Boot.Main.extend main
