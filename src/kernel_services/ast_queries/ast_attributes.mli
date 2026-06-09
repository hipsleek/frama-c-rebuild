(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This file contains attribute related types/functions/values.
    @since 31.0-Gallium
    @before 31.0-Gallium Most of these functions were in {!Cil}
*)

open Cil_types

(* **************************** *)
(** {2 Attributes lists/values} *)
(* **************************** *)

(** [const], [volatile], [restrict] and [ghost] attributes. *)
val qualifier_attributes : string list

(** Name of the attribute that is automatically inserted (with an [AINT size]
    argument when querying the type of a field that is a bitfield.
*)
val bitfield_attribute_name : string

(** Name of the attribute that is inserted when generating a name for a varinfo
    representing an anonymous function parameter.
*)
val anonymous_attribute_name : string

(** Attribute identifying anonymous function parameters. *)
val anonymous_attribute : attribute

(** Internal attributes of Frama-C.  *)
val fc_internal_attributes : string list

(** Attribute for globals in Frama-C's libc (extern or not) that are internal to
    Frama-C. *)
val fc_stdlib_internal : string

(** Qualifiers and internal attributes to remove when doing a C cast. *)
val cast_irrelevant_attributes : string list

(** Qualifiers and internal attributes to remove when doing a C cast. *)
val spare_attributes_for_c_cast : string list

(** Qualifiers and internal attributes to remove when doing a C cast. *)
val spare_attributes_for_logic_cast : string list

(** A block marked with this attribute is known to be a ghost else. *)
val frama_c_ghost_else : string

(** A varinfo marked with this attribute is known to be a ghost formal. *)
val frama_c_ghost_formal : string

(** A formal marked with this attribute is known to be a pointer to an
    object being initialized by the current function, which can thus assign
    any sub-object regardless of const status.
*)
val frama_c_init_obj : string

(** A field struct marked with this attribute is known to be mutable, i.e.
    it can be modified even on a const object.
*)
val frama_c_mutable : string

(** A block marked with this attribute is known to be inlined, i.e.
    it replaces a call to an inline function.
*)
val frama_c_inlined : string

(** Name of the attribute inserted by the elaboration to prevent user blocks
    from disappearing. It can be removed whenever block contracts have been
    processed.
*)
val frama_c_keep_block : string

(** Name of the attribute used to store the function that should be called
    when the corresponding variable exits its scope.
*)
val frama_c_destructor : string

(** Name of the attribute used to indicate that a given static variable has a
    local syntactic scope (despite a global lifetime).
*)
val fc_local_static : string

(** Internal attribute use in Frama-C's libc, see share/libc/feature.h. *)
val fc_stdlib : string

(** Attribute added when generating variadic functions from Frama-C's libc. *)
val fc_stdlib_generated : string

(** Attribute added by Frama-C's parser. *)
val fc_oldstyleproto : string

(** Attribute of globals that represent a (wide)string literal.
    @since 32.0-Germanium *)
val fc_literal: string

(** Attribute added by cabs2cil on functions calls encountered before any
    declaration/definition.
*)
val fc_missingproto : string

(* ***************************** *)
(** {2 Attributes manipulations} *)
(* ***************************** *)

(** Return the name of an attribute. *)
val get_name : attribute -> string

(** Add an attribute. Maintains the attributes in sorted order of the second
    argument. The attribute is not added if it is already there.
*)
val add : attribute -> attributes -> attributes

(** Add a list of attributes. Maintains the attributes in sorted order. The
    second argument must be sorted, but not necessarily the first.
*)
val add_list : attributes -> attributes -> attributes

(** Remove all attributes with the given name. Maintains the attributes in
    sorted order.
*)
val drop : string -> attributes -> attributes

(** Remove all attributes with names appearing in the string list.
    Maintains the attributes in sorted order.
*)
val drop_list : string list -> attributes -> attributes

(** [replace_params name params al] will {!drop} all the attributes named [name]
    in [al] and {!add} a new attribute [(name, params)] in the list.
*)
val replace_params : string -> attrparam list -> attributes -> attributes

(** True if the named attribute appears in the attribute list. The list of
    attributes must be sorted.
*)
val contains : string -> attributes -> bool

(** Return the list of parameters associated to an attribute. The list is empty
    if there is no such attribute or it has no parameters at all.
*)
val find_params : string -> attributes -> attrparam list

(** Retain attributes with the given name. *)
val filter : string -> attributes -> attributes

(* **************************************** *)
(** {2 Attributes classes and registration} *)
(* **************************************** *)

type attribute_class =
  (** Attribute of a name. If argument is [true] and we are on MSVC then
      the attribute is printed using __declspec as part of the storage
      specifier.
  *)
  | AttrName of bool

  (** Attribute of a function type. If argument is [true] and we are on
      MSVC then the attribute is printed just before the function name.
  *)
  | AttrFunType of bool

  (** Attribute of a type. *)
  | AttrType

  (** Attribute of a statement or a block. *)
  | AttrStmt

  (** Attribute with unknown class. It is assigned a default class by
      {!get_class} and can lead to attributes being ignored by {!partition}.
  *)
  | AttrUnknown

(** Registered information about an attribute. *)
type attribute_info = {
  attr_class : attribute_class; (** Class of the attribute. *)
  attr_ignore: bool; (** Ignore the attribute when comparing types. *)
  attr_print : bool; (** Print the attribute when printing the AST. *)
}

(** Table containing all registered attributes. *)
val known_table : (string, attribute_info) Hashtbl.t

(** Add a new attribute with a specified class, if it should be printed
    (default is [true]) and ignore when comparing types (default if [true] for
    [AttrUnknown] class and [false] otherwise).
*)
val register : ?print:bool -> ?ignore:bool -> attribute_class ->
  string -> unit

(** Same as {!register} but with [print] set to [false]. *)
val register_noprint : ?ignore:bool -> attribute_class -> string -> unit

(** Call {!register} on a list of attributes with the same class and print
    status.
*)
val register_list : ?print:bool -> ?ignore:bool -> attribute_class ->
  string list -> unit

(** Remove an attribute previously registered. *)
val remove : string -> unit

(** [is_known attrname] returns [true] if the attribute named [attrname] is
    known (registered) by Frama-C.
*)
val is_known : string -> bool

(** [find_known attrname] returns [Some attrinfo] if the attribute named
    [attrname] is known (registered) by Frama-C, [None] otherwise.
*)
val find_known : string -> attribute_info option

(** Return the class of an attribute. The class `default' is returned for
    unknown and ignored attributes.
*)
val get_class : default:attribute_class -> string -> attribute_class

(** [should_print attrname] return the field [attr_print] of the attribute
    named [attrname] if it is known (registered) by Frama-C, and return [true]
    otherwise.
*)
val should_print : string -> bool

(** [should_ignore attrname] return the field [attr_ignore] of the attribute
    named [attrname] if it is known (registered) by Frama-C, and return [false]
    otherwise.
*)
val should_ignore : string -> bool

(** Partition the attributes into classes: name, function type and type.
    Statement attributes are removed with a warning, Unknown attributes are
    returned in the `default` attribute class. If this class is [AttrUnknown],
    again, they are removed like [AttrStmt] without warning.
*)
val partition : default:attribute_class -> attributes ->
  attributes * (* AttrName *)
  attributes * (* AttrFunType *)
  attributes   (* AttrType *)

(* **************************************** *)
(** {2 Utility functions} *)
(* **************************************** *)

(** Retain attributes corresponding to type qualifiers (6.7.3) *)
val filter_qualifiers : attributes -> attributes

(** Given some attributes on an array type, split them into those that belong
    to the type of the elements of the array (currently, qualifiers such as
    const and volatile), and those that must remain on the array, in that
    order.
*)
val split_array_attributes : attributes -> attributes * attributes

(** Separate out the storage-modifier name attributes *)
val split_storage_modifiers : attributes -> attributes * attributes

(** Find the name of the replaced macro for extern globals in Frama-C's libc
    that are replacing an existing macro. For instance [stdout] for
    [__fc_stdout]. *)
val find_fc_stdlib_extern_replacement : attributes -> string option
