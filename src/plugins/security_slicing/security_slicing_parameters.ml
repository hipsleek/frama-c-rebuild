(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Security-slicing"
      let shortname = "security-slicing"
      let help = "security slicing (experimental, undocumented)"
    end)

module Slicing =
  False
    (struct
      let option_name = "-security-slicing"
      let help = "perform the security slicing analysis"
    end)
