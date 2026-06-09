(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(** Alpha conversion. *)

(** This is the type of the elements that are recorded by the alpha
    conversion functions in order to be able to undo changes to the tables
    they modify. Useful for implementing
    scoping *)
type 'a undoAlphaElement

(** This is the type of the elements of the alpha renaming table. These
    elements can carry some data associated with each occurrence of the name. *)
type 'a alphaTableData

(** type for alpha conversion table. We split the lookup in two to avoid
    creating accidental collisions when converting x_0 into x_0_0 if the
    original code contains both. *)
type 'a alphaTable =
  (string, (string, 'a alphaTableData ref) Hashtbl.t) Hashtbl.t

(** Create a new name based on a given name. The new name is formed from a
    prefix (obtained from the given name by stripping a suffix consisting of _
    followed by only digits), followed by a special separator and then by a
    positive integer suffix. The first argument is a table mapping name
    prefixes to some data that specifies what suffixes have been used and how
    to create the new one. This function updates the table with the new
    largest suffix generated. The "undolist" argument, when present, will be
    used by the function to record information that can be used by
    {!Alpha.undoAlphaChanges} to undo those changes. Note that the undo
    information will be in reverse order in which the action occurred. Returns
    the new name and, if different from the lookupname, the location of the
    previous occurrence. This function knows about the location implicitly
    from the [(Current_loc.get ())]. *)
val newAlphaName: alphaTable: 'a alphaTable ->
  undolist: 'a undoAlphaElement list ref option ->
  lookupname:string -> data:'a -> string * 'a


(** Register a name with an alpha conversion table to ensure that when later
    we call newAlphaName we do not end up generating this one *)
val registerAlphaName: alphaTable: 'a alphaTable ->
  lookupname:string -> data:'a -> unit

(** Split the name in preparation for newAlphaName. Returns a pair
    [(prefix, infix)] where [prefix] is the index in the outer table, while
    infix is the index in the inner table.
*)
val getAlphaPrefix: lookupname:string -> string * string

(** Undo the changes to a table *)
val undoAlphaChanges: alphaTable:'a alphaTable ->
  undolist:'a undoAlphaElement list -> unit
