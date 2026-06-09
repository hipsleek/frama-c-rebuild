(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** A unified signature for building terms and expressions.
    Not to be confused with [Analyses_types.pred_or_term], which
    simply is the sum of both types, while here separate modules are provided
    for predicates and terms. *)
module type S = sig
  type t

  val mk_false : ?loc:location -> logic_type option -> t
  val mk_true : ?loc:location -> logic_type option -> t
  val mk_logic_body : t -> logic_body
  val mk_let : ?loc:location -> logic_info -> t -> t
  val mk_if : ?loc:location -> predicate -> t -> t -> t
  val mk_at : logic_label -> t -> t

  val visit : Visitor.frama_c_visitor -> t -> t
  val pretty : Format.formatter -> t -> unit
end

module Predicate : S with type t = predicate
module Term : S with type t = term
