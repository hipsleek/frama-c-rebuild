(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open LogicUsage

(* -------------------------------------------------------------------------- *)
(* --- VC Generator                                                       --- *)
(* -------------------------------------------------------------------------- *)

module type VCgen =
sig
  include Mcfg.S
  val register_lemma : logic_lemma -> unit
  val compile_lemma : logic_lemma -> Wpo.t
  val compile_wp : Wpo.index -> t_prop -> Wpo.t Bag.t
end

val vcgen : Factory.setup -> Factory.driver -> (module VCgen)

(* -------------------------------------------------------------------------- *)
