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

(** Smart constructors for some CIL data types *)
open Cil_types

(**/**)
(* Reference to [Ast_types.add_attributes] to avoid circular dependencies
   Ideally we want to move everything related to types in [Ast_types], but it
   also requires moving a few things from [Logic_const] to [Ast_types]. We do
   not use [Extlib.mk_fun] here because [mk_typ] is called later at toplevel
   to build types, none of which are arrays, so we do not need to push
   attributes anyway.
*)
val add_attributes_ref : (?push_qualifiers:bool -> attributes -> typ -> typ) ref
[@@alert add_attributes_ref "Only use this if your name is Ast_types."]
(**/**)

(** Create a typ record, [tattr] defaults to empty list. [push_qualifiers] is
    passed to {!Ast_types.add_attributes} and defaults to [true].
    @since 31.0-Gallium
*)
val mk_typ : ?push_qualifiers:bool -> ?tattr:attributes -> typ_node -> typ

(** Create a typ record [TVoid], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tvoid : ?tattr:attributes -> unit  -> typ

(** Create a typ record [TInt ik], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tint : ?tattr:attributes -> ikind -> typ

(** Create a typ record [TFloat fk], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tfloat : ?tattr:attributes -> fkind -> typ

(** Create a typ record [TPtr t], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tptr : ?tattr:attributes -> typ -> typ

(** Create a typ record [TArray (t, len)], [tattr] defaults to empty list.
    [push_qualifiers] is passed to {!Ast_types.add_attributes} and defaults to
    [true], arrays are not supposed to be qualified.
    @since 31.0-Gallium
*)
val mk_tarray : ?push_qualifiers:bool -> ?tattr:attributes -> typ ->
  exp option -> typ

(** Create a typ record [TFun (rt, args, is_va)], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tfun : ?tattr:attributes -> typ ->
  (string * typ * attributes) list option -> bool -> typ

(** Create a typ record [TNamed ti], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tnamed : ?tattr:attributes -> typeinfo -> typ

(** Create a typ record [TComp ci], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tcomp : ?tattr:attributes -> compinfo -> typ

(** Create a typ record [TEnum ei], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tenum : ?tattr:attributes -> enuminfo -> typ

(** Create a typ record [TBuiltin_va_list], [tattr] defaults to empty list.
    @since 31.0-Gallium
*)
val mk_tbuiltin : ?tattr:attributes -> unit -> typ

(** void *)
val voidType: typ

(** bool
    @since 30.0-Zinc *)
val boolType: typ

(** int
    @since 30.0-Zinc *)
val intType: typ

(** unsigned
    @since 30.0-Zinc *)
val uintType: typ

(** short
    @since 30.0-Zinc *)
val shortType : typ

(** unsigned short
    @since 30.0-Zinc *)
val ushortType : typ

(** long
    @since 30.0-Zinc *)
val longType: typ

(** long long
    @since 30.0-Zinc *)
val longLongType: typ

(** unsigned long
    @since 30.0-Zinc *)
val ulongType: typ

(** unsigned long long
    @since 30.0-Zinc *)
val ulongLongType: typ

(** __int128 (GCC extension)
    @since 33.0-Arsenic *)
val int128Type: typ

(** unsigned __int128 (GCC extension)
    @since 33.0-Arsenic *)
val uint128Type: typ

(** char
    @since 30.0-Zinc *)
val charType: typ

(** signed char
    @since 30.0-Zinc *)
val scharType: typ

(** unsigned char
    @since 30.0-Zinc *)
val ucharType: typ

(** char *
    @since 30.0-Zinc *)
val charPtrType: typ

(** signed char *
    @since 30.0-Zinc *)
val scharPtrType: typ

(** unsigned char *
    @since 30.0-Zinc *)
val ucharPtrType: typ

(** char const *
    @since 30.0-Zinc *)
val charConstPtrType: typ

(** void *
    @since 30.0-Zinc *)
val voidPtrType: typ

(** void const *
    @since 30.0-Zinc *)
val voidConstPtrType: typ

(** int *
    @since 30.0-Zinc *)
val intPtrType: typ

(** unsigned int *
    @since 30.0-Zinc *)
val uintPtrType: typ

(** float
    @since 30.0-Zinc *)
val floatType: typ

(** double
    @since 30.0-Zinc *)
val doubleType: typ

(** long double
    @since 30.0-Zinc *)
val longDoubleType: typ

(** _Float32
    @since 33.0-Arsenic *)
val float32Type : typ

(** _Float64
    @since 33.0-Arsenic *)
val float64Type : typ

(** set the vid to a fresh number. *)
val set_vid: varinfo -> unit

(** returns a copy of the varinfo with a fresh vid.
    If the varinfo has an associated logic var, a copy of the logic var
    is made as well.
*)
val copy_with_new_vid: varinfo -> varinfo

(** [change_varinfo_name vi name] changes the name of [vi] to [name]. Takes
    care of renaming the associated logic_var if any.
    @since Oxygen-20120901
*)
val change_varinfo_name: varinfo -> string -> unit

(** Generate a new ID for variables. This will be different from any variable ID
    that is generated by {!Cil.makeLocalVar} and friends.
    Must not be used for setting vid: use {!set_vid} instead.
    @before 33.0-Arsenic Was called [new_raw_id].
*)
val new_raw_vid: unit -> int

(** Generate a new ID for statements. This will be different from any statement
    ID that is generated by {!Cfg.computeFileCFG} and friends.
    @since 33.0-Arsenic
*)
val new_raw_sid: unit -> int

(** Generate a new ID for expressions. This will be different from any
    expression ID that is generated by {!Cil.new_exp} and friends.
    @since 33.0-Arsenic
*)
val new_raw_eid: unit -> int

(** Creates a (potentially recursive) composite type. The arguments are:
    (1) a boolean indicating whether it is a struct or a union, (2) the name
    (always non-empty), (3) a function that when given a representation of the
    structure type constructs the type of the fields recursive type (the first
    argument is only useful when some fields need to refer to the type of the
    structure itself), and (4) an optional list of attributes to be associated
    with the composite type, "None" means that the struct is incomplete.
    @since 23.0-Vanadium the 4th parameter is a function that returns an option.
*)
val mkCompInfo: bool ->      (* whether it is a struct or a union *)
  string -> (* name of the composite type; cannot be empty *)
  ?norig:string -> (* original name of the composite type, empty when anonymous *)
  (compinfo ->
   (string * typ * int option * exp option * attributes * location) list option) ->
  (* a function that when given a forward
     representation of the structure type constructs the type of
     the fields. The function can ignore this argument if not
     constructing a recursive type.  *)
  attributes -> compinfo

(** Makes a shallow copy of a {!Cil_types.compinfo} changing the name. It also
    copies the fields, and makes sure that the copied field points back to the
    copied compinfo.
    If [fresh] is [true] (the default), it will also give a fresh id to the
    copy.
*)
val copyCompInfo: ?fresh:bool -> compinfo -> string -> compinfo


(** Create a fresh logical variable giving its name, type and origin.
    @since Fluorine-20130401
*)
val make_logic_var_kind : string -> logic_var_kind -> logic_type -> logic_var

(** Create a new global logic variable
    @since Fluorine-20130401 *)
val make_logic_var_global: string -> logic_type -> logic_var

(** Create a new formal logic variable
    @since Fluorine-20130401 *)
val make_logic_var_formal: string -> logic_type -> logic_var

(** Create a new quantified logic variable
    @since Fluorine-20130401 *)
val make_logic_var_quant: string -> logic_type -> logic_var

(** Create a new local logic variable
    @since Fluorine-20130401 *)
val make_logic_var_local: string -> logic_type -> logic_var

(** Create a fresh logical (global) variable giving its name and type. *)
val make_logic_info : string -> logic_info

(** Create a new local logic variable given its name.
    @since Fluorine-20130401
*)
val make_logic_info_local : string -> logic_info

(** Create a logic type info given its name.
    @since 30.0-Zinc
*)
val make_logic_type : string -> logic_type_info

module Vid: sig
  val next: unit -> int
  [@@deprecated "Use Cil_const.new_raw_vid instead."]
  [@@migrate { repl = Cil_const.new_raw_vid } ]
end

module Sid: sig
  val next: unit -> int
  [@@deprecated "Use Cil_const.new_raw_sid instead."]
  [@@migrate { repl = Cil_const.new_raw_sid } ]
end

module Eid: sig
  val next: unit -> int
  [@@deprecated "Use Cil_const.new_raw_eid instead."]
  [@@migrate { repl = Cil_const.new_raw_eid } ]
end

val new_raw_id: unit -> int
(** Generate a new ID. This will be different than any variable ID
    that is generated by {!Cil.makeLocalVar} and friends.
    Must not be used for setting vid: use {!set_vid} instead. *)
[@@deprecated "Use Cil_const.new_raw_vid instead."]
[@@migrate { repl = Rel.new_raw_vid } ]
