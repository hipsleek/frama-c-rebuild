(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This file contains types related types/functions/values.
    @since 31.0-Gallium
    @before 31.0-Gallium Most of these functions were in {!Cil}
*)

open Cil_types

(* ************************************************************************* *)
(** {2 Type Attributes} *)
(* ************************************************************************* *)

(** Returns all the attributes contained in a type. This requires a traversal
    of the type structure, in case of composite, enumeration and named types *)
val get_attributes : typ -> attributes

(** Add some attributes to a type. [push_qualifiers] determines if type
    qualifiers are pushed to the elements type. It defaults to [true] and
    should not be set to [false] unless you known what you are doing. In
    Frama-C this is useful for formals (see C11 6.7.6.3#7), so
    [push_qualifiers] is turned off when typing array formals before they are
    changed into pointers.

    @before 31.0-Gallium In Cil [push_qualifiers] was not present, which caused a
    bug in cabs2cil. Also [combine] was present and allowed to chose the
    function used to combine attributes, now it only uses
    {!Ast_attributes.add_list}.
*)
val add_attributes : ?push_qualifiers:bool -> attribute list -> typ -> typ

(** Does the type have the given attribute. Does
    not recurse through pointer types, nor inside function prototypes.
*)
val has_attribute : string -> typ -> bool

(** Does the type have the given qualifier. Handles the case of arrays, for
    which the qualifiers are actually carried by the type of the elements.
    It is always correct to call this function instead of {!has_attribute}.
    For l-values, both functions return the same results, as l-values cannot
    have array type.
*)
val has_qualifier : string -> typ -> bool

(** [has_attribute_memory_block attr t] is [true] iff at least one component of
    an object of type [t] has attribute [attr]. In other words, it searches
    for [attr] under aggregates, but not under pointers.
*)
val has_attribute_memory_block : string -> typ -> bool

(** Remove all attributes with the given names from a type. Note that this
    does not remove attributes from typedef and tag definitions, just from
    their uses (unfolding the type definition when needed).
    It only removes attributes of topmost type, i.e. does not
    recurse under pointers, arrays, ...
*)
val remove_attributes : string list -> typ -> typ

(** Same as {!remove_attributes}, but remove any existing attribute from
    the type.
*)
val remove_all_attributes : typ -> typ

(** Same as {!remove_attributes}, but recursively removes the given
    attributes from inner types as well. Mainly useful to check whether
    two types are equal modulo some attributes. See also
    {!Cil.typeDeepDropAllAttributes}, which will strip every single attribute
    from a type.
*)
val remove_attributes_deep : string list -> typ -> typ

(** Remove all attributes relative to const, volatile and restrict attributes. *)
val remove_qualifiers : typ -> typ

(** Remove also qualifiers under Ptr and Arrays. *)
val remove_qualifiers_deep : typ -> typ

(** Remove all attributes relative to const, volatile and restrict attributes
    when building a C cast
*)
val remove_attributes_for_c_cast : typ -> typ

(** Remove all attributes relative to const, volatile and restrict attributes
    when building a logic cast
*)
val remove_attributes_for_logic_type : typ -> typ

(* ************************************************************************* *)
(** {2 Utils functions} *)
(* ************************************************************************* *)

(** Unroll a type until it exposes a non [TNamed]. Will collect all attributes
    appearing in [TNamed] and add them to the final type using
    {!add_attributes}.
*)
val unroll : typ -> typ

(** Same than {!unroll} but discard the final type attributes and only
    return its node. *)
val unroll_node : typ -> typ_node

(** Unroll typedefs, discarding all intermediate attribute. To be used only
    when one is interested in the shape of the type *)
val unroll_skel : typ -> typ_node

(** Unroll all the TNamed in a type (even under type constructors such as
    [TPtr], [TFun] or [TArray]. Does not unroll the types of fields in [TComp]
    types. Will collect all attributes *)
val unroll_deep : typ -> typ

(** Same than {!unroll_deep} but discard the final type attributes and only
    return its node. *)
val unroll_deep_node : typ -> typ_node

(** Expands logic type definitions. If the [unroll_typedef] flag is set to
    [true] (this is the default), C typedef will be expanded as well using
    {!Logic_const.unroll_ltdef}.
*)
val unroll_logic : ?unroll_typedef:bool -> logic_type -> logic_type

(* ************************************************************************* *)
(** {2 Const Attribute} *)
(* ************************************************************************* *)

(** Check for ["const"] qualifier from the type of an l-value using
    {!has_attribute_memory_block}.
*)
val is_const : typ -> bool

(* ************************************************************************* *)
(** {2 Volatile Attribute} *)
(* ************************************************************************* *)

(** Check for ["volatile"] qualifier from the type of an l-value using
    {!has_attribute_memory_block}.
*)
val is_volatile : typ -> bool

(* ************************************************************************* *)
(** {2 Ghost Attribute} *)
(* ************************************************************************* *)

(** Add the ghost attribute to a type (does nothing if the type is already
    ghost).
*)
val add_ghost : typ -> typ

(** Check for ["ghost"] qualifier from the type of an l-value (do not follow
    pointer)
*)
val is_ghost : typ -> bool

(** Check if the received type is well-formed according to \ghost semantics, that is
    once the type is not ghost anymore, \ghost cannot appear again.
*)
val is_wellformed_ghost : typ -> bool

(* ************************************************************************* *)
(** {2 Type checkers} *)
(* ************************************************************************* *)

(** is the given type "void"? *)
val is_void : typ -> bool

(** is the given type "void *"? *)
val is_void_ptr : typ -> bool

(** True if the argument is [_Bool]. *)
val is_bool : typ -> bool

(** True if the argument is a plain character type (but neither [signed char]
    nor [unsigned char]).
*)
val is_char : typ -> bool

(** True if the argument is a character type (i.e. plain, signed or unsigned). *)
val is_any_char : typ -> bool

(** True if the argument is a pointer to a plain character type (but neither
    [signed char] nor [unsigned char]).
*)
val is_char_ptr : typ -> bool

(** True if the argument is a pointer to a character type (i.e. plain, signed or
    unsigned).
*)
val is_any_char_ptr : typ -> bool

(** True if the argument is a pointer to a constant character type, e.g. a
    string literal.
*)
val is_char_const_ptr : typ -> bool

(** True if the argument is a short type (i.e. signed or unsigned). *)
val is_short : typ -> bool

(** True if the argument is an integral type (i.e. integer or enum). *)
val is_integral : typ -> bool

(** True if the argument is [intptr_t] (but _not_ its underlying integer type). *)
val is_intptr_t : typ -> bool

(** True if the argument is [uintptr_t] (but _not_ its underlying integer type). *)
val is_uintptr_t : typ -> bool

(** True if the argument is a floating point type. *)
val is_float : typ -> bool

(** True if the argument is a long double type. *)
val is_long_double : typ -> bool

(** True if the argument is an arithmetic type (i.e. integer, enum or floating
    point.
*)
val is_arithmetic : typ -> bool

(** True if the argument is a pointer type. *)
val is_ptr : typ -> bool

(** True if the argument is an integral or pointer type. *)
val is_integral_or_pointer : typ -> bool

(** True if the argument is an array type. *)
val is_array : typ -> bool

(** True if the argument is an array type without size. *)
val is_unsized_array : typ -> bool

(** True if the argument is a sized array type. *)
val is_sized_array : typ -> bool

(** True if the argument is an array of a character type (i.e. plain, signed or
    unsigned).
*)
val is_char_array : typ -> bool

(** True if the argument is an array of a character type (i.e. plain, signed or
    unsigned).
*)
val is_any_char_array : typ -> bool

(** True if the argument is an array of wchar_t. Can only be used after
    Machdep has been set.
    @since 32.0-Germanium
*)
val is_wchar_array : typ -> bool

(** True if the argument is a function type. *)
val is_fun : typ -> bool

(** True if the argument is a variadic function type.
    @since 33.0-Arsenic
*)
val is_variadic : typ -> bool

(** True if the argument is a function pointer type. *)
val is_fun_ptr : typ -> bool

(** True if the argument is a pointer {i or} a function type.
    @since 33.0-Arsenic
*)
val is_fun_or_ptr : typ -> bool

(** True if the argument is a scalar type (i.e. integral, enum, floating point
    or pointer.
*)
val is_scalar : typ -> bool

(** True if the argument is an object type (i.e. not a function type). *)
val is_object : typ -> bool

(** True if the argument is an object pointer type.
    @since 33.0-Arsenic
*)
val is_object_ptr : typ -> bool

(** True if the argument is a struct. *)
val is_struct : typ -> bool

(** True if the argument is a type that directly (modulo name) contains a
    bitfield.
    @since 32.0-Germanium
*)
val has_bitfield : typ -> bool

(** True if the argument is a union type. *)
val is_union : typ -> bool

(** True if the argument is a struct or union type. *)
val is_struct_or_union : typ -> bool

(** Check if a type is a transparent union, and return the first field. *)
val is_transparent_union : typ -> fieldinfo option

(** True if the argument denotes the type of [...] in a variadic function. *)
val is_variadic_list : typ -> bool

(* ************************************************************************* *)
(** {2 Type access} *)
(* ************************************************************************* *)

(** Returns the type of the array elements of the given type.
    @raise AbortFatal it is not an array type.
*)
val direct_element_type : typ -> typ

(** Returns the elements type using {!direct_element_type}, but if the resulting
    type is an array, recursively call {!element_type}.
*)
val element_type : typ -> typ

(** Returns the type directly pointed by the given type.
    @raise AbortFatal it is not a pointer type.
*)
val direct_pointed_type : typ -> typ

(** Returns the pointed type using {!direct_pointed_type}, but if the resulting
    type is an array, returns the element type instead using {!element_type}
*)
val pointed_type : typ -> typ

(** Returns the type of the array elements of the given type, and the size
    of the array, if any.
    @raise AbortFatal it is not an array type.
    @before 31.0-Gallium In Cil this function applied {!Cil.constFoldToInt} on
    array's size and returned a [Z.t option].
*)
val array_elem_type_and_size : typ -> typ * exp option


(* ************************************************************************* *)
(** {2 Logic Type checkers} *)
(* ************************************************************************* *)

(** Check for ["volatile"] qualifier from a logic type using {!is_volatile}. *)
val is_logic_volatile : logic_type -> bool

(** True if the argument is the type for reified C types. *)
val is_logic_typetag : logic_type -> bool

(** True if the argument is a boolean type, either integral C type or
    mathematical boolean one.
*)
val is_logic_boolean : logic_type -> bool

(** True if the argument is [_Bool] or [boolean]. *)
val is_logic_pure_boolean : logic_type -> bool

(** True if the argument is an integral type (i.e. integer or enum), either C or
    mathematical one.
*)
val is_logic_integral : logic_type -> bool

(** True if the argument is a floating point type. *)
val is_logic_float : logic_type -> bool

(** True if the argument is the logic 'real' type. *)
val is_logic_real : logic_type -> bool

(** True if the argument is a C floating point type or logic 'real' type. *)
val is_logic_real_or_float : logic_type -> bool

(** True if the argument is a logic arithmetic type (i.e. integer, enum or
    floating point, either C or mathematical one.
*)
val is_logic_arithmetic : logic_type -> bool

(** True if the argument is a pointer type. Expands the logic type
    definition if necessary.
    @since 33.0-Arsenic
*)
val is_logic_ptr : logic_type -> bool

(** True if the argument is the logic function type. Expands the logic type
    definition if necessary.
*)
val is_logic_fun : logic_type -> bool

(** True if the argument is the logic function pointer type. Expands the logic
    type definition if necessary.
*)
val is_logic_fun_ptr : logic_type -> bool

(** True if the argument is a pointer {i or} function type.
    Expands the logic type definition if necessary.
    @since 33.0-Arsenic
*)
val is_logic_fun_or_ptr : logic_type -> bool
