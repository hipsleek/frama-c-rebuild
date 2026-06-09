(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let name = "Instantiate"
let shortname = "instantiate"

include Plugin.Register
    (struct
      let name = name
      let shortname = shortname
      let help = "Overrides standard library functions"
    end)

module Enabled = False
    (struct
      let option_name = "-" ^ shortname
      let help = ""
    end)

let () = Parameter_customize.argument_may_be_fundecl()
module Kfs =
  Kernel_function_set
    (struct
      let option_name = "-" ^ shortname ^ "-fct"
      let arg_name = "f,..."
      let help = "Override stdlib functions only into the specified functions (defaults to all)."
    end)

module NewInstantiator (I: sig val function_name: string end) = True
    (struct
      let option_name = "-" ^ shortname ^ "-" ^ I.function_name
      let help = "Activate replacement for function '" ^ I.function_name ^ "'"
    end)

let emitter = Emitter.create shortname [Emitter.Funspec] ~correctness:[] ~tuning:[]
