(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Pdg"
      let shortname = "pdg"
      let help = "Program Dependence Graph"
    end)

let output = add_group "Output"

module BuildAll =
  False
    (struct
      let option_name = "-pdg"
      let help = "build the dependence graph of each function"
    end)

module BuildFct =
  Kernel_function_set
    (struct
      let option_name = "-fct-pdg"
      let arg_name = ""
      let help = "build the dependence graph for the specified function"
    end)

let () = Parameter_customize.set_group output
module PrintBw =
  False(struct
    let option_name = "-codpds"
    let help = "show co-dependencies rather than dependencies"
  end)

let () = Parameter_customize.set_group output
module DotBasename =
  Empty_string
    (struct
      let option_name = "-pdg-dot"
      let arg_name = "basename"
      let help = "put the PDG of function <f> in basename.f.dot"
    end)
