(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* State to store the association between a replaced function and the original
   function. *)
module Replacements =
  Cil_state_builder.Varinfo_hashtbl
    (Cil_datatype.Varinfo)
    (struct
      let size = 17
      let name = "replacements"
      let dependencies =
        [ Kernel.VariadicTranslation.self; Kernel.VariadicStrict.self ]
    end)

let add new_vi old_vi =
  File.never_remove_global old_vi.Cil_types.vname;
  Replacements.add new_vi old_vi

let find new_vi =
  Replacements.find new_vi

let mem new_vi =
  Replacements.mem new_vi
