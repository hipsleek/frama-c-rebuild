(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Plugin Registration                                                --- *)
(* -------------------------------------------------------------------------- *)

include Plugin.Register
    (struct
      let name = "Region Analysis"
      let help = "Memory Region Analysis (experimental)"
      let shortname = "region"
    end)

module Enabled = Action
    (struct
      let option_name = "-region"
      let help = "Annotate all functions wrt regions"
    end)

let () = Parameter_customize.set_negative_option_name "-region-check"
let () = Parameter_customize.set_negative_option_help "Generate ACSL 'check' annotations"
module Assert = False
    (struct
      let option_name = "-region-assert"
      let help = "Generate ACSL 'assert' annotations instead of checks"
    end)

module Logic = False
    (struct
      let option_name = "-region-logic"
      let help = "Also generate guards for logic"
    end)
