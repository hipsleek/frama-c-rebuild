(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Conditions
module F = Lang.F
module Env = Plang.Env

type 'a printer = Format.formatter -> 'a -> unit

(* -------------------------------------------------------------------------- *)
(* --- Autofocus                                                          --- *)
(* -------------------------------------------------------------------------- *)

type v_fold = [ `Auto | `Visible | `Hidden ]
type v_term = [ v_fold | `Shared | `Name of string ]

type part = Term | Goal | Step of step

class autofocus :
  object
    method clear : unit
    method reset : unit
    method env : Env.t
    method set_term : F.term -> v_term -> unit
    method get_term : F.term -> v_term
    method set_target : F.term -> unit
    method clear_target : unit
    method focus : extend:bool -> F.term -> unit
    method unfocus : F.term -> unit
    method unfocus_last : unit
    method is_selected : F.term -> bool
    method is_focused : F.term -> bool
    method is_visible : F.term -> bool
    method is_targeted : F.term -> bool
    method set_autofocus : bool -> unit
    method get_autofocus : bool
    method is_autofocused : bool
    method set_step : step -> v_fold -> unit
    method get_step : step -> v_fold
    method is_visible_step : step -> bool
    method locate : F.term -> Tactical.selection
    method set_sequent : sequent -> bool
  end

(* -------------------------------------------------------------------------- *)
(* --- Term Engine                                                        --- *)
(* -------------------------------------------------------------------------- *)

class type term_wrapper =
  object
    method wrap : F.term printer -> F.term printer
  end

class type term_selection =
  object
    method is_focused : F.term -> bool
    method is_visible : F.term -> bool
    method is_targeted : F.term -> bool
  end

class plang :
  terms:#term_wrapper ->
  focus:#term_wrapper ->
  target:#term_wrapper ->
  autofocus:#term_selection ->
  object
    inherit Pcond.state
    method set_target : F.term -> unit
    method clear_target : unit
  end

(* -------------------------------------------------------------------------- *)
(* --- Condition Engine                                                   --- *)
(* -------------------------------------------------------------------------- *)

class type part_marker =
  object
    method wrap : part printer -> part printer
    method mark : 'a. part -> 'a printer -> 'a printer
  end

class type step_selection =
  object
    method is_visible : F.term -> bool
    method is_visible_step : step -> bool
  end

class pcond :
  parts:#part_marker ->
  target:#part_marker ->
  autofocus:#step_selection ->
  plang:#Pcond.state ->
  object
    inherit Pcond.seqengine
    method visible : step -> bool
    method set_target : part -> unit
  end

(* -------------------------------------------------------------------------- *)
(* --- Sequent Engine                                                     --- *)
(* -------------------------------------------------------------------------- *)

type target = part * F.term option
type focus = [ `Transient | `Select | `Focus | `Extend | `Reset ]

class pseq :
  autofocus:#autofocus ->
  plang:#plang ->
  pcond:#pcond ->
  object
    method reset : unit

    method update_ce_models : Wpo.t -> unit
    method get_ce_mode : bool
    method set_ce_mode : bool -> unit
    method get_focus_mode : bool
    method set_focus_mode : bool -> unit
    method get_state_mode : bool
    method set_state_mode : bool -> unit
    method set_unmangled : bool -> unit

    method get_iformat : Plang.iformat
    method set_iformat : Plang.iformat -> unit

    method get_rformat : Plang.rformat
    method set_rformat : Plang.rformat -> unit

    method selected : unit
    method target : target
    method unselect : target
    method restore : focus:focus -> target -> unit
    method resolve : target -> Tactical.selection
    method on_selection : (unit -> unit) -> unit

    method sequent : Conditions.sequent
    method selection : Tactical.selection
    method set_selection : Tactical.selection -> unit
    method selection_to_target : Tactical.selection -> target
    method highlight : Tactical.selection -> unit

    method pp_term : F.term printer
    method pp_pred : F.pred printer
    method pp_selection : Tactical.selection printer

    method pp_sequent : Conditions.sequent printer
    method pp_goal : Wpo.t printer
    method pp_step : Conditions.step printer
  end

(* -------------------------------------------------------------------------- *)
