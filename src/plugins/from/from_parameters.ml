(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "From analysis"
      let shortname = "from"
      let help = "functional dependencies"
    end)

module ForceDeps =
  False
    (struct
      let option_name = "-deps"
      let help = "force dependencies display"
    end)

module ForceCallDeps =
  False
    (struct
      let option_name = "-calldeps"
      let help = "force callsite-wise dependencies"
    end)

module ShowIndirectDeps =
  False
    (struct
      let option_name = "-show-indirect-deps"
      let help = "experimental"
    end)

module VerifyAssigns =
  False
    (struct
      let option_name = "-from-verify-assigns"
      let help = "verification of assigns/from clauses for functions with \
                  bodies. Implies -calldeps"
    end)
let () =
  VerifyAssigns.add_set_hook
    (fun _ new_ ->
       if new_ then ForceCallDeps.set true)
