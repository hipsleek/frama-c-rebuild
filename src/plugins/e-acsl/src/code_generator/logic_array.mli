(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val comparison_to_exp: loc:location -> kernel_function -> Env.t ->
  name:string -> binop -> exp -> exp -> exp * Env.t
(** [comparison_to_exp ~loc kf env ~name bop e1 e2] generate the C code
    equivalent to [e1 bop e2].
    Requires that [bop] is either [Ne] or [Eq] and that [e1] and [e2] are
    arrays. *)


(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

module Translate_rtes : sig
  val exp_ref:
    (?filter:(code_annotation -> bool) -> kernel_function -> Env.t -> exp ->
     Env.t) ref
end

module Translate_utils : sig
  val comparison_to_exp_ref:
    (loc:location -> kernel_function -> Env.t -> Analyses_types.number_ty ->
     binop -> exp -> exp -> ?name:string -> term option -> exp * Env.t) ref
end
