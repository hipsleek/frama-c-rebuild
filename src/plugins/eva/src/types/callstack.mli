(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A call is identified by the function called and the call statement *)
type call = Cil_types.kernel_function * Cil_types.stmt

module Call : Datatype.S with type t = call

(** Eva callstacks. *)
type callstack = {
  thread: int;
  (* An identifier of the thread's callstack. *)
  entry_point: Cil_types.kernel_function;
  (** The first function function of the callstack. *)
  stack: call list;
  (** A call stack is a list of calls. The head is the latest call. *)
}

include Datatype.S_with_collections with type t = callstack

(** Prints a callstack without displaying call sites. *)
val pretty_short : Format.formatter -> t -> unit

(** Prints a hash of the callstack. *)
val pretty_hash : Format.formatter -> t -> unit

(** [compare_lex] compares callstack lexicographically, slightly slower
    than [compare] but in a more natural order, giving more importance
    to the function at bottom of the callstack - the first functions called. *)
val compare_lex : t -> t -> int

(** [is_empty cs] returns true if the callstack is empty, i.e., [top_callsite]
    would return [Kglobal]. *)
val is_empty : t -> bool

(*** {2 Stack manipulation} *)

(*** Constructor *)
val init : thread:int -> entry_point:Cil_types.kernel_function -> t

(** Adds a new call to the top of the callstack. *)
val push : Cil_types.kernel_function -> Cil_types.stmt -> t -> t

(** Removes the topmost call from the callstack. *)
val pop : t -> t option

(** Removes the topmost call from the callstack and returns it. *)
val pop_call : t -> Cil_types.kernel_function * (Cil_types.stmt * t) option

val top : t -> (Cil_types.kernel_function * Cil_types.stmt) option
val top_kf : t -> Cil_types.kernel_function
val top_callsite : t -> Cil_types.kinstr
val top_call : t -> Cil_types.kernel_function * Cil_types.kinstr

(** Returns the function that called the topmost function of the callstack and
    the top callsite. *)
val top_caller : t -> (Cil_types.stmt * Cil_types.kernel_function) option

(** {2 Conversion} *)

(** Gives the list of kf in the callstack from the entry point to the top of the
    callstack (i.e. reverse order of the call stack). *)
val to_kf_list : t -> Cil_types.kernel_function list

(** Gives the list of call statements from the bottom to the top of the
    callstack (i.e. reverse order of the call stack). *)
val to_stmt_list : t -> Cil_types.stmt list

(** Gives the list of call from the bottom to the top of the callstack
    (i.e. reverse order of the call stack). *)
val to_call_list : t -> (Cil_types.kernel_function * Cil_types.kinstr) list

(** {2 Iteration} *)

(** [iter f cs] calls [f] on [cs] and all the callstacks obtained by successful
    calls to {!pop} until the result of the call is [None]. *)
val iter : (t -> unit) -> t -> unit
