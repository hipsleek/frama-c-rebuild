(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Reduction"
      let shortname = "reduc"
      let help = "Generate ACSL annotations from Eva information"
    end)

module Reduc =
  Bool
    (struct
      let option_name = "-reduc"
      let help = "Use reduc"
      let default = false
    end)

module GenAnnot =
  String
    (struct
      let option_name = "-reduc-gen-annot"
      let arg_name = "gen-annot-heuristic"
      let help = "Heuristic to generate annotations from Eva"
      let default = "inout"
    end)
let () = GenAnnot.set_possible_values ["inout"; "all"]
