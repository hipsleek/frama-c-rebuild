(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let category = File.register_code_transformation_category "instantiate"

let () =
  let perform file =
    if Options.Enabled.get () then
      Transform.transform file
  in
  File.add_code_transformation_after_cleanup category perform
