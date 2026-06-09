(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module should not be used outside of the Project library.
    @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Type declaration} *)
(* ************************************************************************** *)

type t = private
  { pid: int;
    mutable name: string }
(** @since Carbon-20101201
    @before 33.0-Arsenic Had a mutable field [unique_name]
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

type project = t
(** @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Constructor} *)
(* ************************************************************************** *)

val dummy: t
(** @since Carbon-20101201 *)

(** @since Carbon-20101201 *)
module Make_setter () : sig

  val make: string -> t
  (** @since Carbon-20101201 *)

  val set_name: t -> string -> unit
  (** @since Carbon-20101201 *)

end

val get_project_debug_name: t -> string
(** @return a project name appended with its id.
    @since 32.0-Germanium *)
