(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Slicing module types. *)

exception Slicing_Internal_Error of string
exception ChangeCallErr of string
exception PtrCallExpr
exception CantRemoveCalledFf
exception WrongSlicingLevel

(** raised when someone tries to build more than one slice for the entry point.
*)
exception OnlyOneEntryPointSlice

(** raised when one tries to select something in a function where we are not
    able to compute the Pdg. *)
exception NoPdg

(** {2 Public types} *)

(** These types are the only one that should be used by the API functions.
    Public type definitions should be hidden to the outside world.
*)

(** contains global things that has been computed so far
    for the slicing project.
    This includes :
    - the slices of the functions,
    - and the queue of actions to be applied.
*)
type sl_project   = SlicingInternals.project

(** Type of the selections
    (we store the varinfo because we cannot use the kernel_function in this file)
*)
type sl_select = Cil_types.varinfo * SlicingInternals.fct_user_crit

module Fct_user_crit : Datatype.S with type t = SlicingInternals.fct_user_crit

(** Function slice *)
type sl_fct_slice = SlicingInternals.fct_slice

(** Marks : used to put 'colors' in the result *)
type sl_mark = SlicingInternals.pdg_mark

module Sl_project : Datatype.S with type t = sl_project

module Sl_select : Datatype.S with type t = sl_select

module Sl_fct_slice : Datatype.S with type t = SlicingInternals.fct_slice

val dyn_sl_fct_slice : Sl_fct_slice.t Type.t

module Sl_mark : Datatype.S_with_collections with
  type t = SlicingInternals.pdg_mark

val dyn_sl_mark : Sl_mark.t Type.t
