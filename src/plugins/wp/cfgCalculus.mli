(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* -------------------------------------------------------------------------- *)
(* --- WP Calculus Driver from Interpreted Automata                       --- *)
(* -------------------------------------------------------------------------- *)

type mode = {
  kf : kernel_function ; (* Selected function *)
  bhv : funbehavior ; (* Selected behavior *)
  infos : CfgInfos.t ; (* Associated infos *)
}

type props = [ `All | `Names of string list | `PropId of Property.t ]

module Make(W : Mcfg.S) :
sig
  exception NonNaturalLoop of location
  val compute : mode:mode -> props:props -> W.t_prop
end

(* -------------------------------------------------------------------------- *)
