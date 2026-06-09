(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include
  Plugin.Register
    (struct
      let name = "Studia"
      let shortname = "studia"
      let help = "Tools for Eva case studies"
    end)
