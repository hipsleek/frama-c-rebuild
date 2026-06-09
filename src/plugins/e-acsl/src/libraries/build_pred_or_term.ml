(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

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

module Predicate : S with type t = predicate = struct
  type t = predicate

  let mk_false ?loc:_ = function
    | None -> Logic_const.pfalse
    | Some _ -> Options.fatal "cannot specify a type for building a predicate"

  let mk_true ?loc:_ = function
    | None -> Logic_const.ptrue
    | Some _ -> Options.fatal "cannot specify a type for building a predicate"

  let mk_logic_body pred = LBpred pred

  let mk_let ?loc li = Logic_const.plet ?loc li

  let mk_if ?loc p_cond t_true t_false =
    match (t_true.pred_content, t_false.pred_content) with
    (* cond ? \true : \false  ≡  cond *)
    | Ptrue, Pfalse -> p_cond
    (* cond ? \true : f  ≡  cond || f *)
    | Ptrue, _ -> Logic_const.por ?loc (p_cond, t_false)
    (* cond ? t : \false  ≡  cond && t *)
    | _, Pfalse -> Logic_const.pand ?loc (p_cond, t_true)
    | _ ->
      Logic_const.pif (p_cond, t_true, t_false)

  let mk_at labels p = {p with pred_content = Pat (p, labels)}

  let visit = Visitor.visitFramacPredicate

  let pretty = Printer.pp_predicate
end

module Term : S with type t = term = struct
  type t = term

  let mk_false ?loc = function
    | None -> Options.fatal "must specify a type for building a term"
    | Some l_type ->
      let default = Logic_const.tinteger ?loc 0 in
      Logic_const.term ?loc (TCast (true, l_type, default)) l_type

  let mk_true ?loc = function
    | None -> Options.fatal "must specify a type for building a term"
    | Some l_type ->
      let default = Logic_const.tinteger ?loc 1 in
      Logic_const.term ?loc (TCast (true, l_type, default)) l_type

  let mk_logic_body term = LBterm term

  let mk_let ?loc li t = Logic_const.term ?loc (Tlet (li, t)) t.term_type

  let mk_if ?loc p_cond t_true t_false =
    Logic_const.term ?loc (Tif (p_cond, t_true, t_false)) t_true.term_type

  let mk_at labels p = {p with term_node = Tat (p, labels)}

  let visit = Visitor.visitFramacTerm

  let pretty = Printer.pp_term
end
