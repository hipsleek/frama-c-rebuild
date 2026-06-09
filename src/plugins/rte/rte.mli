(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type 'a alarm_gen =
  remove_trivial:bool ->
  on_alarm:(invalid:bool -> Alarms.alarm -> unit) ->
  'a -> unit
(** ['a alarm_gen] is an abstraction over the process of generating a certain
    kind of RTEs over something of type ['a].
    The [on_alarm] argument receives all corresponding alarms, with
    optionally a status indicating that the alarm is red. *)

val lval_assertion: read_only: Alarms.access_kind -> lval alarm_gen
val lval_initialized_assertion: lval alarm_gen
val divmod_assertion: exp alarm_gen
val signed_div_assertion: (exp * exp * exp) alarm_gen
val shift_width_assertion: (exp * typ) alarm_gen
val shift_negative_assertion: exp alarm_gen
val shift_overflow_assertion: signed:bool -> (exp * binop * exp * exp) alarm_gen
val mult_sub_add_assertion: signed:bool -> (exp * binop * exp * exp) alarm_gen
val uminus_assertion: exp alarm_gen
val downcast_assertion: (typ * exp) alarm_gen
val float_to_int_assertion: (typ * exp) alarm_gen
val finite_float_assertion: (fkind * exp) alarm_gen
val pointer_call: (exp * exp list) alarm_gen
val pointer_value: exp alarm_gen
val pointer_alignment: (exp * typ) alarm_gen
val bool_value: lval alarm_gen
