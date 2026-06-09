(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Occurrence"
      let shortname = "occurrence"
      let help = "automatically computes where variables are used"
    end)

module Print =
  False
    (struct
      let option_name = "-occurrence"
      let help = "print results of occurrence analysis"
    end)
