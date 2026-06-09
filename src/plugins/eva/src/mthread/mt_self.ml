(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let () = Plugin.is_share_visible ()

include Plugin.Register
    (struct
      let name = "Mthread"
      let shortname = "mt"
      let help = "Experimental tools for multi-threaded programs"
    end)
