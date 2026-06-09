(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Impact"
      let shortname = "impact"
      let help = "impact analysis"
    end)

module Annot =
  Kernel_function_set
    (struct
      let option_name = "-impact-annot"
      let arg_name = "f1, ..., fn"
      let help = "use the impact annotations in the code of functions f1,...,fn"
    end)
let () = Annot.add_aliases ~visible:false ~deprecated:true ["-impact-pragma"]

module Print =
  False
    (struct
      let option_name = "-impact-print"
      let help = "print the impacted stmt"
    end)

module Reason =
  False
    (struct
      let option_name = "-impact-graph"
      let help = "build a graph that explains why a statement is in the set \
                  of impacted nodes"
    end)

module Slicing =
  False
    (struct
      let option_name = "-impact-slicing"
      let help = "slice from the impacted stmt"
    end)

module Skip =
  String_set
    (struct
      let arg_name = "v1,...,vn"
      let help = "consider that those variables are not impacted"
      let option_name = "-impact-skip"
    end)

let () = Parameter_customize.set_negative_option_name "-impact-not-in-callers"
module Upward =
  True
    (struct
      let  option_name = "-impact-in-callers"
      let help = "compute compute impact in callers as well as in callees"
    end)

let is_on () = not (Annot.is_empty ())
