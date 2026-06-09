(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Loop"
      let shortname = "loop"
      let help = "Find maximum number of iterations in loops"
    end)

module Run = False
    (struct
      let option_name = "-loop"
      let help = "Launch loop analysis"
    end)
