(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type offsm_or_top = O of Cvalue.V_Offsetmap.t | Top

val cast :
  old_size: Z.t -> new_size: Z.t -> signed: bool ->
  Cvalue.V_Offsetmap.t -> Cvalue.V_Offsetmap.t

module Offsm : Abstract_value.Leaf with type t = offsm_or_top
