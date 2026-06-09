(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Sparecode"
      let shortname = "sparecode"
      let help = "code cleaner"
    end)

module Analysis =
  False(struct
    let option_name = "-sparecode"
    let help = "perform a spare code analysis"
  end)
let () = Analysis.add_aliases ["-sparecode-analysis"]

module Annot =
  True(struct
    let option_name = "-sparecode-annot"
    let help = "select more things to keep every reachable annotation"
  end)

module GlobDecl =
  False(struct
    let option_name = "-sparecode-rm-unused-globals"
    let help = ("only remove unused global types and variables "^
                "(automatically done by -sparecode-analysis)")
  end)
