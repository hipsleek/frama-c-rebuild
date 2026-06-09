(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Obfuscator"
      let shortname = "obfuscator"
      let help = "obfuscator for confidential code"
    end)

module Run =
  False
    (struct
      let option_name = "-obfuscate"
      let help = "print an obfuscated version of the input files and exit.\n\
                  Disable any other Frama-C analysis."
    end)

module Dictionary =
  Empty_string
    (struct
      let option_name = "-obfuscator-dictionary"
      let arg_name = "f"
      let help = "generate the dictionary into file <f> (on stdout by default)"
    end)

module String_literal =
  Empty_string
    (struct
      let option_name = "-obfuscator-string-dictionary"
      let arg_name = "f"
      let help = "generate the dictionary of literal strings into file <f> \
                  (in the same place than the code by default)"
    end)

let states = [ Run.self; Dictionary.self; String_literal.self ]
