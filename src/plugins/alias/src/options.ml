(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Plugin Registration                                                --- *)
(* -------------------------------------------------------------------------- *)

include Plugin.Register
    (struct
      let name = "Alias Analysis"
      let help = "Lightweight May-Alias Analysis (experimental)"
      let shortname = "alias"
    end)

module Enabled = False
    (struct
      let option_name = "-alias"
      let help = "Enable May-Alias Analyzer"
    end)

module ShowFunctionTable = False
    (struct
      let option_name = "-alias-show-function-table"
      let help = "display summary for each function after the analysis"
    end)

module ShowStmtTable = False
    (struct
      let option_name = "-alias-show-stmt-table"
      let help = "display abstract state for each statement after the analysis"
    end)


module DebugTable = False
    (struct
      let option_name = "-alias-debug-table"
      let help = "switch to debug mode when printing statement or function tables (with options -alias-show-stmt-table and -alias-show-function-table)"
    end)

module Dot_output =
  Empty_string
    (struct
      let option_name = "-alias-dot-output"
      let arg_name = "f"
      let help = "output final abstract state as dot file <f>"
    end)

module Warn = struct
  let no_return_stmt = register_warn_category "no-return"
  let undefined_function = register_warn_category "undefined:fn"
  let unsupported_address = register_warn_category "unsupported:addr"
  let unsupported_asm = register_warn_category "unsupported:asm"
  let unsupported_function = register_warn_category "unsupported:fn"
  let unsafe_cast = register_warn_category "unsafe-cast"
  let incoherent = register_warn_category "incoherent"
end

module DebugKeys = struct
  let show_libc_vars =
    let help = "include variables stemming from the C library in output" in
    register_category ~help "printer:show-libc-vars"
  let show_internal_state =
    let help = "print internal representation of abstract state" in
    register_category ~help "debug:internal-state"
end
