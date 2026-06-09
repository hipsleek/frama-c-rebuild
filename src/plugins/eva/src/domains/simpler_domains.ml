(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Simplified interfaces for abstract domains. Complete abstract domains can be
    built from these interfaces through the functors in {!Domain_builder}.  More
    documentation can be found on the complete interface of abstract domains,
    in {!Abstract_domain}. *)

open Cil_types
open Eval

(** Both the formal argument of a called function and the concrete argument at a
    call site. *)
type simple_argument = {
  formal: varinfo;
  concrete: exp;
}

(** Simple information about a function call. *)
type simple_call = {
  kf: kernel_function;                (* The called function. *)
  arguments: simple_argument list;    (* The list of arguments of the call. *)
  rest: exp list;                     (* Extra arguments. *)
  return: varinfo option;             (* Fake varinfo where the result of the
                                         call is stored. *)
}

(** Simplest interface for an abstract domain. No exchange of information with
    the other abstractions of Eva. *)
module type Minimal = sig
  type t
  val name: string
  val compare: t -> t -> int
  val hash: t -> int

  (** Lattice structure. *)

  val top: t
  val is_included: t -> t -> bool
  val join: t -> t -> t
  val widen: kernel_function -> stmt -> t -> t -> t

  (** Transfer functions. *)

  val assign: pos:Position.t -> lval -> exp -> t -> t or_bottom
  val assume: pos:Position.t -> exp -> bool -> t -> t or_bottom
  val start_call: pos:Position.local -> simple_call -> t -> t
  val finalize_call: pos:Position.local -> simple_call -> pre:t -> post:t -> t or_bottom

  (** Initialization of variables. *)

  val empty: unit -> t
  val initialize_variable:
    lval -> initialized:bool -> Abstract_domain.init_value -> t -> t

  val enter_scope: Abstract_domain.variable_kind -> varinfo list -> t -> t
  val leave_scope: kernel_function -> varinfo list -> t -> t

  (** Pretty printers. *)

  val pretty: Format.formatter -> t -> unit
end

(** The simplest interface of domains, equipped with a frama-c datatype. *)
module type Minimal_with_datatype = sig
  include Minimal
  include Datatype.S with type t := t
end


(** A simpler functional interface for valuations. *)
type cvalue_valuation = {
  find: exp -> Cvalue.V.t flagged_value or_top;
  find_loc: lval -> Precise_locs.precise_location or_top
}

type precise_loc = Precise_locs.precise_location
type cvalue = Cvalue.V.t

(** A simple interface allowing the abstract domain to use the value and
    location abstractions computed by the other domains. Only the {!Cvalue.V}
    and the the {!Precise_locs} abstractions are available in this interface, on
    the transfer functions for assignment, assumption and at the call sites. On
    the other hand, the abstract domain cannot assist the computation of these
    value and location abstractions. The communication is thus unidirectional,
    from other domains to these simpler domains. *)
module type Simple_Cvalue = sig
  include Datatype.S

  (** Domain name *)
  val name: string

  (** Lattice structure. *)

  val top: t
  val is_included: t -> t -> bool
  val join: t -> t -> t
  val widen: kernel_function -> stmt -> t -> t -> t

  (** Query functions. *)

  val extract_expr: t -> exp -> cvalue or_bottom
  val extract_lval: t -> lval -> precise_loc -> cvalue or_bottom

  (** Transfer functions. *)

  val assign:
    pos:Position.t -> Precise_locs.precise_location left_value -> exp ->
    (precise_loc, cvalue) assigned -> cvalue_valuation -> t -> t or_bottom

  val assume: pos:Position.t -> exp -> bool -> cvalue_valuation -> t -> t or_bottom

  val start_call:
    pos:Position.local -> (precise_loc, cvalue) call -> cvalue_valuation ->
    t -> t

  val finalize_call:
    pos:Position.local -> (precise_loc, cvalue) call ->  pre:t -> post:t ->
    t or_bottom

  (** Initialization of variables. *)

  val empty: unit -> t
  val initialize_variable:
    lval -> initialized:bool -> Abstract_domain.init_value -> t -> t

  val enter_scope: Abstract_domain.variable_kind -> varinfo list -> t -> t
  val leave_scope: kernel_function -> varinfo list -> t -> t
end
