(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Pdg_types

type t = PdgTypes.Pdg.t

(**************************************************************************)

let compute = Build.compute_pdg

module Tbl =
  Kernel_function.Make_Table
    (PdgTypes.Pdg)
    (struct
      let name = "Pdg.State"
      let dependencies = [] (* postponed because From.self may not exist yet *)
      let size = 17
    end)

let self = Tbl.self
let get = Tbl.memo compute

(**************************************************************************)

let pretty ?(bw=false) fmt pdg =
  let kf = PdgTypes.Pdg.get_kf pdg in
  Format.fprintf fmt "@[RESULT for %s:@]@\n@[ %a@]"
    (Kernel_function.get_name kf) (PdgTypes.Pdg.pretty_bw ~bw) pdg

let pretty_node short =
  if short then PdgTypes.Node.pretty
  else PdgTypes.Node.pretty_node

let print_dot pdg filename =
  PdgTypes.Pdg.build_dot filename pdg;
  Pdg_parameters.feedback "dot file generated in %s" filename

let pretty_key = PdgIndex.Key.pretty

(**************************************************************************)
