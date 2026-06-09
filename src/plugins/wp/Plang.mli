(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang
open Lang.F

(** Lang Pretty-Printer *)

type scope = Qed.Engine.scope
module Env : Qed.Engine.Env with type term := term

type pool
val pool : unit -> pool
val alloc_e : pool -> (var -> unit) -> term -> unit
val alloc_p : pool -> (var -> unit) -> pred -> unit
val alloc_xs : pool -> (var -> unit) -> Vars.t -> unit
val alloc_domain : pool -> Vars.t
val sanitizer : string -> string

type iformat = [ `Hex | `Dec | `Bin ]
type rformat = [ `Ratio | `Float | `Double ]

class engine :
  object
    inherit [Z.t,ADT.t,Field.t,Fun.t,tau,var,term,Env.t] Qed.Engine.engine
    method get_iformat : iformat
    method set_iformat : iformat -> unit
    method get_rformat : rformat
    method set_rformat : rformat -> unit
    method marks : Env.t * Lang.F.marks
    method pp_pred : Format.formatter -> pred -> unit
    method lookup : term -> scope
    (**/**)
    inherit Lang.idprinting
    method sanitize : string -> string
    method op_spaced : string -> bool
    (**/**)
  end
