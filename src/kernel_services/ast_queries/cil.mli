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

(** CIL main API.

    CIL original API documentation is available as
    an html version at http://manju.cs.berkeley.edu/cil.

    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

open Cil_types
open Cil_datatype

(* ************************************************************************* *)
(** {2 Values for manipulating globals} *)
(* ************************************************************************* *)

(** Make an empty function from an existing global varinfo.
    @since Nitrogen-20111001 *)
val emptyFunctionFromVI: varinfo -> fundec

(** Make an empty function *)
val emptyFunction: string -> fundec

(** Update the formals of a [fundec] and make sure that the function type
    has the same information. Will copy the name as well into the type. *)
val setFormals: fundec -> varinfo list -> unit

(** Takes as input a function type (or a typename on it) and return its
    return type. *)
val getReturnType: typ -> typ

(** Change the return type of the function passed as 1st argument to be
    the type passed as 2nd argument. *)
val setReturnTypeVI: varinfo -> typ -> unit
val setReturnType: fundec -> typ -> unit

(** Set the types of arguments and results as given by the function type
    passed as the second argument. Will not copy the names from the function
    type to the formals *)
val setFunctionType: fundec -> typ -> unit

(** Set the type of the function and make formal arguments for them *)
val setFunctionTypeMakeFormals: fundec -> typ -> unit

(** Update the smaxid after you have populated with locals and formals
    (unless you constructed those using {!Cil.makeLocalVar} or
    {!Cil.makeTempVar}. *)
val setMaxId: fundec -> unit

val selfFormalsDecl: State.t
(** state of the table associating formals to each prototype. *)

val makeFormalsVarDecl: ?ghost:bool -> (string * typ * attributes) -> varinfo
(** creates a new varinfo for the parameter of a prototype.
    By default, this formal variable is not ghost.
*)

(** Update the formals of a function declaration from its identifier and its
    type. For a function definition, use {!Cil.setFormals}.
    Do nothing if the type is not a function type or if the list of
    argument is empty.
*)
val setFormalsDecl: varinfo -> typ -> unit

(** remove a binding from the table.
    @since Oxygen-20120901 *)
val removeFormalsDecl: varinfo -> unit

(** replace formals of a function declaration with the given
    list of varinfo.
*)
val unsafeSetFormalsDecl: varinfo -> varinfo list -> unit

(** iterate the given function on declared prototypes.
    @since Oxygen-20120901
*)
val iterFormalsDecl: (varinfo -> varinfo list -> unit) -> unit

(** Get the formals of a function declaration registered with
    {!Cil.setFormalsDecl}.
    @raise Not_found if the function is not registered (this is in particular
    the case for prototypes with an empty list of arguments.
    See {!Cil.setFormalsDecl})
*)
val getFormalsDecl: varinfo -> varinfo list

(** Iterate over all globals, including the global initializer *)
val iterGlobals: file -> (global -> unit) -> unit

(** Fold over all globals, including the global initializer *)
val foldGlobals: file -> ('a -> global -> 'a) -> 'a -> 'a

(** Map over all globals, including the global initializer and change things
    in place *)
val mapGlobals: file -> (global -> global) -> unit

(** Find a function or function prototype with the given name in the file.
    If it does not exist, create a prototype with the given type, and return
    the new varinfo.  This is useful when you need to call a libc function
    whose prototype may or may not already exist in the file.

    Because the new prototype is added to the start of the file, you shouldn't
    refer to any struct or union types in the function type.*)
val findOrCreateFunc: file -> string -> typ -> varinfo

(** creates an expression with a fresh id *)
val new_exp: loc:location -> exp_node -> exp

(** performs a deep copy of an expression (especially, avoid eid sharing).
    @since Nitrogen-20111001
*)
val copy_exp: exp -> exp

(** creates an expression with a dummy id.
    Use with caution, {i i.e.} not on expressions that may be put in the AST.
*)
val dummy_exp: exp_node -> exp

(** Return [true] on case and default labels, [false] otherwise. *)
val is_case_label: label -> bool


(** CIL keeps the types at the beginning of the file and the variables at the
    end of the file. This function will take a global and add it to the
    corresponding stack. Its operation is actually more complicated because if
    the global declares a type that contains references to variables (e.g. in
    sizeof in an array length) then it will also add declarations for the
    variables to the types stack *)
val pushGlobal: global -> types: global list ref
  -> variables: global list ref -> unit

(** An empty statement. Used in pretty printing *)
val invalidStmt: stmt

(** Returns a location that ranges over the two locations in arguments. *)
val range_loc: location -> location -> location

(* ************************************************************************* *)
(** {2 Values for manipulating initializers} *)
(* ************************************************************************* *)

(** Make a initializer for zero-ing a data type *)
val makeZeroInit: loc:location -> typ -> init

(** Fold over the list of initializers in a Compound (not also the nested
    ones). [doinit] is called on every present initializer, even if it is of
    compound type. The parameters of [doinit] are: the offset in the compound
    (this is [Field(f,NoOffset)] or [Index(i,NoOffset)]), the initializer
    value, expected type of the initializer value, accumulator. In the case of
    arrays there might be missing zero-initializers at the end of the list.
    These are scanned only if [implicit] is true. This is much like
    [List.fold_left] except we also pass the type of the initializer.

    This is a good way to use it to scan even nested initializers :
    {[
      let rec myInit (lv: lval) (i: init) (acc: 'a) : 'a =
        match i with
        | SingleInit e -> (* ... do something with [lv] and [e] and [acc] ... *)
        | CompoundInit (ct, initl) ->
          foldLeftCompound ~implicit:false
            ~doinit:(fun off' i' _typ acc' ->
                myInit (addOffsetLval off' lv) i' acc')
            ~ct
            ~initl
            ~acc
    ]}
*)
val foldLeftCompound:
  implicit:bool ->
  doinit: (offset -> init -> typ -> 'a -> 'a) ->
  ct: typ ->
  initl: (offset * init) list ->
  acc: 'a -> 'a

(* ************************************************************************* *)
(** {2 Values for manipulating types} *)
(* ************************************************************************* *)

(** Any signed integer type of size 16 bits.
    It is equivalent to the ISO C int16_t type but without using the
    corresponding header.
    Must only be called if such type exists in the current architecture.
    @since 23.0-Vanadium
*)
val int16_t: unit -> typ

(** Any signed integer type of size 32 bits.
    It is equivalent to the ISO C int32_t type but without using the
    corresponding header.
    Must only be called if such type exists in the current architecture.
    @since 23.0-Vanadium
*)
val int32_t: unit -> typ

(** Any signed integer type of size 64 bits.
    It is equivalent to the ISO C int64_t type but without using the
    corresponding header.
    Must only be called if such type exists in the current architecture.
    @since 23.0-Vanadium
*)
val int64_t: unit -> typ

(** Any signed integer type of size 128 bits.
    It is equivalent to the (non-ISO C) GCC __int128 type.
    Must only be called if such type exists in the current architecture.
    @since 33.0-Arsenic
*)
val int128_t: unit -> typ

(** Any unsigned integer type of size 16 bits.
    It is equivalent to the ISO C uint16_t type but without using the
    corresponding header.
    Must only be called if such type exists in the current architecture.
    @since Nitrogen-20111001
*)
val uint16_t: unit -> typ

(** Any unsigned integer type of size 32 bits.
    It is equivalent to the ISO C uint32_t type but without using the
    corresponding header.
    Must only be called if such type exists in the current architecture.
    @since Nitrogen-20111001
*)
val uint32_t: unit -> typ

(** Any unsigned integer type of size 64 bits.
    It is equivalent to the ISO C uint64_t type but without using the
    corresponding header.
    Must only be called if such type exists in the current architecture.
    @since Nitrogen-20111001
*)
val uint64_t: unit -> typ

(** Any unsigned integer type of size 128 bits.
    It is equivalent to the (non-ISO C) GCC unsigned __int128 type.
    Must only be called if such type exists in the current architecture.
    @since 33.0-Arsenic
*)
val uint128_t: unit -> typ

(** @return true if and only if the given type is a signed integer type. *)
val isSignedInteger: typ -> bool

(** @return true if and only if the given type is an unsigned integer type.
    @since Oxygen-20120901 *)
val isUnsignedInteger: typ -> bool

(** This is a constant used as the name of an unnamed bitfield. These fields
    do not participate in initialization and their name is not printed. *)
val missingFieldName: string

(** Get the full name of a comp, including the 'struct' or 'union' prefix *)
val compFullName: compinfo -> string

(** Returns true if this is a complete type. This means that sizeof(t) makes
    sense. Incomplete types are not yet defined structures and empty arrays.
    @param allowZeroSizeArrays indicates whether arrays of
    size 0 (a gcc extension) are considered as complete. Default value
    depends on the current machdep.
    @param last_field indicates if it is the last element of a struct
    or not part of an aggregate. Useful for arrays without size in
    the square brackets. Unless you know what you are doing, this parameter
    should always left untouched. Default value: [false].
    @before 32.0-Germanium [last_field] was not exposed and always [false] when
    using this function
*)
val isCompleteType: ?allowZeroSizeArrays:bool -> ?last_field:bool -> typ -> bool

(** Performs lvalue-conversion on the type and returns the converted type,
    or Error if the type is incomplete and not an array type.

    @since 27.0-Cobalt
*)
val lvalue_conversion: typ -> (typ, string) result

(** [true] iff the given type is variably modified, i.e., an array
    with variable size (VLA), or a composite type containing such an array.
*)
val is_variably_modified_type: typ -> bool

(** [true] iff the given type is a [struct] whose last field is a flexible
    array member. When in gcc mode, a zero-sized array is identified with a
    FAM for this purpose.

    @since 18.0-Argon
*)
val has_flexible_array_member: typ -> bool
(** [true] iff the given type has flexible array member, in GCC/MSVC mode, this
    function mode recursively searches in the type of the last field.

    @before 24.0-Chromium this function didn't take in account the GCC/MSVC mode
*)

(** returns the type of the result of an arithmetic operator applied to
    values of the corresponding input types.
    @since Nitrogen-20111001 (moved from Cabs2cil)
*)
val arithmeticConversion : typ -> typ -> typ

(** performs the usual integral promotions mentioned in C reference manual.
    @since Nitrogen-20111001 (moved from Cabs2cil)
*)
val integralPromotion : typ -> typ

(** Obtain the argument list ([] if None).
    @since 20.0-Calcium Beware that it contains the ghost arguments. *)
val argsToList:
  (string * typ * attributes) list option -> (string * typ * attributes) list

(** Obtain the argument lists (non-ghost, ghosts) ([], [] if None)
    @since 20.0-Calcium *)
val argsToPairOfLists:
  (string * typ * attributes) list option ->
  (string * typ * attributes) list * (string * typ * attributes) list

(** possible causes for raising {!Cil.LenOfArray} *)
type incorrect_array_length = Not_constant | Not_integer | Negative | Too_big

val pp_incorrect_array_length:
  Format.formatter -> incorrect_array_length -> unit

(** Raised when {!Cil.lenOfArray} fails either because the length is [None],
    because it is a non-constant expression, or because it overflows an int.
*)
exception LenOfArray of incorrect_array_length


(** Call to compute the array length as present in the array type, to an
    integer. Raises {!Cil.LenOfArray} if not able to compute the length, such
    as when there is no length or the length is not a constant. *)
val lenOfArray: exp option -> int
val lenOfArray64: exp option -> Z.t

(** Return a named fieldinfo in compinfo, or raise Not_found *)
val getCompField: compinfo -> string -> fieldinfo

(** Return the compinfo of the typ, or raise Not_found *)
val getCompType: typ -> compinfo

(** A datatype to be used in conjunction with [existsType] *)
type existsAction =
    ExistsTrue                          (** We have found it *)
  | ExistsFalse                         (** Stop processing this branch *)
  | ExistsMaybe                         (** This node is not what we are
                                            looking for but maybe its
                                            successors are *)

(** Scans a type by applying the function on all elements.
    When the function returns ExistsTrue, the scan stops with
    true. When the function returns ExistsFalse then the current branch is not
    scanned anymore. Care is taken to
    apply the function only once on each composite type, thus avoiding
    circularity. When the function returns ExistsMaybe then the types that
    construct the current type are scanned (e.g. the base type for TPtr and
    TArray, the type of fields for a TComp, etc). *)
val existsType: (typ -> existsAction) -> typ -> bool


(** Given a function type split it into return type,
    arguments, is_vararg and attributes. An error is raised if the type is not
    a function type *)
val splitFunctionType:
  typ -> typ * (string * typ * attributes) list option * bool * attributes

(** Same as {!Cil.splitFunctionType} but takes a varinfo. Prints a nicer
    error message if the varinfo is not for a function *)
val splitFunctionTypeVI:
  varinfo ->
  typ * (string * typ * attributes) list option * bool * attributes


exception Cannot_combine of string

(** Used in {!combineTypes} and {!combineTypesGen} to indicate what we want to
    combine.

    @since 28.0-Nickel
*)
type combineWhat =
  | CombineFundef of bool
  (** The new definition is for a function definition. The old is for a
      prototype. arg is [true] for an old-style declaration.
  *)
  | CombineFunarg of bool
  (** Comparing a function argument type with an old prototype argument. arg is
      [true] for an old-style declaration, which triggers some ad hoc treatment
      in GCC mode.
  *)
  | CombineFunret
  (** Comparing the return of a function with that from an old prototype *)
  | CombineOther

(** [combineAttributes what olda a] combines the attributes in [olda] and [a]
    according to [what]:
    - if [what == CombineFunarg], then override old attributes;
      this is used to ensure that attributes from formal argument types in a
      function definition are not mixed with attributes from arguments in other
      (compatible, but with different qualifiers) declarations;
    - else, perform the union of old and new attributes.

    @since 28.0-Nickel
*)
val combineAttributes : combineWhat -> attribute list -> attributes -> attributes

(** [combineFunction] contains information on how enum, struct/union and typedef
    are to be handled when combining with {!combineTypes} and
    {!combineTypesGen}. In practice, the first argument of each field is a
    recursive definition.

    @since 28.0-Nickel
    @before 29.0-Copper [strictReturnTypes] was not named and [strictInteger]
    not present in {!typ_combine}.
*)
type combineFunction =
  {
    typ_combine : combineFunction ->
      strictInteger:bool -> strictReturnTypes:bool ->
      combineWhat -> typ -> typ -> typ;

    enum_combine : combineFunction ->
      enuminfo -> enuminfo -> enuminfo;

    comp_combine : combineFunction ->
      compinfo -> compinfo -> compinfo;

    name_combine : combineFunction -> combineWhat ->
      typeinfo -> typeinfo -> typeinfo;
  }

(** [combineTypesGen ~strictInteger ~strictReturnTypes combF combW oldt newt]
    Combine [oldt] and [newt] accordingly to [combF], [combW] indicates what
    we are combining.

    Warning : this is not commutative. Indeed, excluding enum, struct/union and
    typedef which depend on [combF], the resulting type is as close as possible
    to [newt].

    If [strictInteger] is [true], same size/sign integers with different types
    will not be combined. Emits a warning if it is [false] and the compatibility
    is machine-dependent.

    If [strictReturnTypes] is [false], anything will be considered compatible
    with void if [combW] is [CombineFunret] (i.e. comparing function return
    types).

    [~emitwith] is used to emit warnings.

    @raise Cannot_combine with an explanation when the type cannot be combined.

    @since 28.0-Nickel
    @before 29.0-Copper [strictInteger:true] and [strictReturnTypes:false] were
    optional
*)
val combineTypesGen : ?emitwith:(Log.event -> unit) -> combineFunction ->
  strictInteger:bool -> strictReturnTypes:bool ->
  combineWhat -> typ -> typ -> typ

(** Specialized version of {!combineTypesGen}, we suppose here that if two
    global symbols are equal, then they are the same object. [strictInteger] is
    [true] by default, [strictReturnTypes] is [false] by default.

    @since 28.0-Nickel
    @before 29.0-Copper [strictInteger] was not present and left with its
    default value in combineTypesGen.
*)
val combineTypes : ?strictInteger:bool -> ?strictReturnTypes:bool ->
  combineWhat -> typ -> typ -> typ

(** How type qualifiers must be checked when checking for types compatibility
    with {!areCompatibleTypes} and {!compatibleTypes}.

    @since 28.0-Nickel
*)
type qualifier_check_context =
  | Identical (** Identical qualifiers. *)
  | IdenticalToplevel
  (** Ignore at toplevel, use Identical when going under a pointer. *)
  | Covariant
  (** First type can have const-qualifications the second doesn't have. *)
  | CovariantToplevel
  (** Accepts everything for current type, use Covariant when going under a
      pointer. *)
  | Contravariant
  (** Second type can have const-qualifications the first doesn't have. *)
  | ContravariantToplevel
  (** Accepts everything for current type, use Contravariant when going under
      a pointer. *)

(** [areCompatibleTypes] returns [true] if two types are compatible.
    [context] indicates how check the compatibility of qualifiers.
    Other arguments are the same than [combineTypes].

    @since 28.0-Nickel
*)
val areCompatibleTypes :
  ?strictReturnTypes:bool -> ?context:qualifier_check_context -> typ -> typ -> bool

(** Same as [areCompatibleTypes old newt] but combine [oldt] and [newt].
    [context] does not impact the qualifiers of the result.

    @raise Cannot_combine if [oldt] and [newt] are not compatible.

    @since 28.0-Nickel
*)
val compatibleTypes :
  ?strictReturnTypes:bool -> ?context:qualifier_check_context -> typ -> typ -> typ

(*********************************************************)
(**  LVALUES *)

(** Make a varinfo. Use this (rarely) to make a raw varinfo. Use other
    functions to make locals ({!Cil.makeLocalVar} or {!Cil.makeFormalVar} or
    {!Cil.makeTempVar}) and globals ({!Cil.makeGlobalVar}). Note that this
    function will assign a new identifier.
    The [temp] argument defaults to [false], and corresponds to the
    [vtemp] field in type {!Cil_types.varinfo}.
    The [source] argument defaults to [true], and corresponds to the field
    [vsource] .
    The [referenced] argument defaults to [false], and corresponds to the field
    [vreferenced] .
    The [ghost] argument defaults to [false], and corresponds to the field
    [vghost] .
    The [alignas] argument defaults to the alignment of the provided type.
    The [loc] argument defaults to [Fileloc.unknown], and corresponds to the field
    [vdecl] .
    The first unnamed argument specifies whether the varinfo is for a global and
    the second is for formals.
*)
val makeVarinfo:
  ?source:bool -> ?temp:bool -> ?referenced:bool -> ?ghost:bool ->
  ?alignas:exp -> ?loc:Fileloc.t -> bool -> bool
  -> string -> typ -> varinfo

(** Make a formal variable for a function declaration. Insert it in both the
    sformals and the type of the function. You can optionally specify where to
    insert this one. If where = "^" then it is inserted first. If where = "$"
    then it is inserted last. Otherwise where must be the name of a formal
    after which to insert this. By default it is inserted at the end.

    The [ghost] parameter indicates if the variable should be inserted in the
    list of formals or ghost formals. By default, it takes the ghost status of
    the function where the formal is inserted. Note that:

    - specifying ghost to false if the function is ghost leads to an error
    - when [where] is specified, its status must be the same as the formal to
      insert (else, it cannot be found in the list of ghost or non ghost formals)
*)
val makeFormalVar: fundec -> ?ghost:bool -> ?where:string -> ?loc:Fileloc.t -> string -> typ -> varinfo

(** Make a local variable and add it to a function's slocals and to the given
    block (only if insert = true, which is the default).
    Make sure you know what you are doing if you set [insert=false].
    [temp] is passed to {!Cil.makeVarinfo}.
    The variable is attached to the toplevel block if [scope] is not specified.
    If the name passed as argument already exists within the function,
    a fresh name will be generated for the varinfo.
*)
val makeLocalVar:
  fundec -> ?scope:block -> ?temp:bool -> ?referenced:bool -> ?insert:bool ->
  ?ghost:bool -> ?loc:Fileloc.t -> string -> typ -> varinfo

(** if needed, rename the given varinfo so that its [vname] does not
    clash with the one of a local or formal variable of the given function.

    @since Chlorine-20180501
*)
val refresh_local_name: fundec -> varinfo -> unit

(** Make a temporary variable and add it to a function's slocals. The name of
    the temporary variable will be generated based on the given name hint so
    that to avoid conflicts with other locals.
    Optionally, you can give the variable a description of its contents and
    its location.
    Temporary variables are always considered as generated variables.
    If [insert] is true (the default), the variable will be inserted
    among other locals of the function. The value for [insert] should
    only be changed if you are completely sure this is not useful.

*)
val makeTempVar: fundec -> ?insert:bool -> ?ghost:bool -> ?name:string ->
  ?descr:string -> ?descrpure:bool -> ?loc:Fileloc.t -> typ -> varinfo

(** Make a global variable. Your responsibility to make sure that the name
    is unique. [source] defaults to [true]. [temp] defaults to [false].
*)
val makeGlobalVar: ?source:bool -> ?temp:bool -> ?referenced:bool ->
  ?ghost:bool -> ?alignas:exp -> ?loc:Fileloc.t -> string -> typ -> varinfo

(** Make a shallow copy of a [varinfo] and assign a new identifier.
    If the original varinfo has an associated logic var, it is copied too and
    associated to the copied varinfo
*)
val copyVarinfo: varinfo -> string -> varinfo

(** Changes the type of a varinfo and of its associated logic var if any.
    @since Neon-20140301 *)
val update_var_type: varinfo -> typ -> unit

(** Is an lvalue a bitfield? *)
val isBitfield: lval -> bool

(** Returns the last offset in the chain. *)
val lastOffset: offset -> offset

(** Add an offset at the end of an lvalue. Make sure the type of the lvalue
    and the offset are compatible. *)
val addOffsetLval: offset -> lval -> lval

(** [addOffset o1 o2] adds [o1] to the end of [o2]. *)
val addOffset:     offset -> offset -> offset

(** Remove ONE offset from the end of an lvalue. Returns the lvalue with the
    trimmed offset and the final offset. If the final offset is [NoOffset]
    then the original [lval] did not have an offset. *)
val removeOffsetLval: lval -> lval * offset

(** Remove ONE offset from the end of an offset sequence. Returns the
    trimmed offset and the final offset. If the final offset is [NoOffset]
    then the original [lval] did not have an offset. *)
val removeOffset:   offset -> offset * offset

(** Compute the type of an lvalue *)
val typeOfLval: lval -> typ

(** Compute the type of an lhost (with no offset) *)
val typeOfLhost: lhost -> typ

(** Equivalent to [typeOfLval] for terms. *)
val typeOfTermLval: term_lval -> logic_type

(** Compute the type of an offset from a base type *)
val typeOffset: typ -> offset -> typ

(** Equivalent to [typeOffset] for terms. *)
val typeTermOffset: logic_type -> term_offset -> logic_type

(** Compute the type of an initializer *)
val typeOfInit: init -> typ

(** indicates whether the given lval is a modifiable lvalue in the sense
    of the C standard 6.3.2.1§1. *)
val is_modifiable_lval: lval -> bool

(* ************************************************************************* *)
(** {2 Values for manipulating expressions} *)
(* ************************************************************************* *)

(* Construct integer constants *)

(** 0 *)
val zero: loc:Fileloc.t -> exp

(** 1 *)
val one: loc:Fileloc.t -> exp

(** -1 *)
val mone: loc:Fileloc.t -> exp

(** Construct an integer of a given kind without literal representation.
    Truncate the integer if [kind] is given, and the integer does not fit
    inside the type. The integer can have an optional literal representation
    [repr].
    @raise Not_representable if no ikind is provided and the integer is not
    representable. *)
val kinteger64: loc:location -> ?repr:string -> ?kind:ikind -> Z.t -> exp

(** Construct an integer of a given kind. Converts the integer to int64 and
    then uses kinteger64. This might truncate the value if you use a kind
    that cannot represent the given integer. This can only happen for one of
    the Char or Short kinds *)
val kinteger: loc:location -> ikind -> int -> exp

(** Construct an integer of kind IInt. You can use this always since the
    OCaml integers are 31 bits and are guaranteed to fit in an IInt *)
val integer: loc:location -> int -> exp

(** Constructs a floating point constant.
    @since Oxygen-20120901
*)
val kfloat: loc:location -> fkind -> float -> exp

(** True if the given expression is a (possibly cast'ed)
    character or an integer constant *)
val isInteger: exp -> Z.t option

(** True if the expression is a compile-time constant.
    [is_varinfo_cst] indicates whether a variable should
    be considered as having a constant content. Defaults to
    [false].

    @before 28.0-Nickel [is_varinfo_cst] does not exist
*)
val isConstant: ?is_varinfo_cst:(varinfo -> bool) -> exp -> bool

(** True if the expression is a compile-time integer constant

    @before 28.0-Nickel [is_varinfo_cst] does not exist
*)
val isIntegerConstant: ?is_varinfo_cst:(varinfo -> bool) -> exp -> bool

(** True if the given offset contains only field names or constant indices.

    @before 28.0-Nickel [is_varinfo_cst] does not exist
*)
val isConstantOffset: ?is_varinfo_cst:(varinfo -> bool) -> offset -> bool

(** True if the given expression is a (possibly cast'ed) integer or character
    constant with value zero *)
val isZero: exp -> bool

(** True if the given expression is a null pointer, i.e. [0], [(void * )0],
    which are the two null pointer constants in the standard, or the cast of
    a null pointer (constant or not) into a pointer type.

    @since 28.0-Nickel
*)
val is_nullptr: exp -> bool

(** True if the term is the constant 0 *)
val isLogicZero: term -> bool

(** True if the given term is [\null] or a constant null pointer*)
val isLogicNull: term -> bool

(** [no_op_coerce typ term] is [true] iff converting [term] to [typ] does
    not modify its value.

    @since 19.0-Potassium
*)
val no_op_coerce: logic_type -> term -> bool

(** gives the value of a wide char literal. *)
val reduce_multichar: typ -> int64 list -> int64

(** gives the value of a char literal. *)
val interpret_character_constant: int64 list -> constant * typ

(** Given the character c in a (CChr c), sign-extend it to 32 bits.
    (This is the official way of interpreting character constants, according to
    ISO C 6.4.4.4.10, which says that character constants are chars cast to ints)
    Returns CInt64(sign-extended c, IInt, None) *)
val charConstToInt: char -> Z.t
val charConstToIntConstant: char -> constant

(** Do constant folding on an expression. If the first argument is [true] then
    will also compute compiler-dependent expressions such as sizeof.
    See also {!Cil.constFoldVisitor}, which will run constFold on all
    expressions in a given AST node. *)
val constFold: bool -> exp -> exp

(** Do constant folding on the given expression, just as [constFold] would. The
    resulting integer value, if the const-folding was complete, is returned.
    The [machdep] optional parameter, which is set to [true] by default,
    forces the simplification of architecture-dependent expressions. *)
val constFoldToInt: ?machdep:bool -> exp -> Z.t option

(** Do constant folding on an term at toplevel only.
    This uses compiler-dependent information and will
    remove all sizeof and alignof. *)
val constFoldTermNodeAtTop:  term_node -> term_node

(** Do constant folding on an term.
    @before 29.0-Copper takes a boolean [machdep] to decide if we actually do
            the fold or not. *)
val constFoldTerm: term -> term

(** Do constant folding on a {!Cil_types.offset}. If the second argument is true
    then will also compute compiler-dependent expressions such as [sizeof]. *)
val constFoldOffset: bool -> offset -> offset

(** Do constant folding on a binary operation. The bulk of the work done by
    [constFold] is done here. If the second argument is true then
    will also compute compiler-dependent expressions such as [sizeof]. *)
val constFoldBinOp: loc:location -> bool -> binop -> exp -> exp -> typ -> exp

(** Convert an expression [e] to a boolean expression [e != 0] if [e] is not
    already a boolean.
    @since 33.0-Arsenic
*)
val expression_to_bool: exp -> exp

(** [true] if the two constant are equal.
    @since Nitrogen-20111001
*)
val compareConstant: constant -> constant -> bool


(** [true] if two kinds have the same size independently of the machine.*)
val sameSizeInt : ?machdep:bool -> ikind -> ikind -> bool

(** [true] if the result of two expressions are two equal integers. *)
val same_int64 : ?machdep:bool -> exp -> exp -> bool

(** Increment an expression. Can be arithmetic or pointer type *)
val increm: exp -> int -> exp

(** Increment an expression. Can be arithmetic or pointer type *)
val increm64: exp -> Z.t -> exp

(** Makes an lvalue out of a given variable *)
val var: varinfo -> lval

(** Creates an expr representing the variable.
    @since Nitrogen-20111001
*)
val evar: ?loc:location -> varinfo -> exp

(** Make an AddrOf. Given an lvalue of type T will give back an expression of
    type ptr(T). It optimizes somewhat expressions like "& v" and "& v[0]"  *)
val mkAddrOf: loc:location -> lval -> exp

(** Creates an expression corresponding to "&v".
    @since Oxygen-20120901 *)
val mkAddrOfVi: varinfo -> exp

(** Like mkAddrOf except if the type of lval is an array then it uses
    StartOf. This is the right operation for getting a pointer to the start
    of the storage denoted by lval. *)
val mkAddrOrStartOf: loc:location -> lval -> exp

(** Make a Mem, while optimizing AddrOf. The type of the addr must be
    TPtr(t) and the type of the resulting lval is t. Note that in CIL the
    implicit conversion between an array and the pointer to the first
    element does not apply. You must do the conversion yourself using
    StartOf *)
val mkMem: addr:exp -> off:offset -> lval

(** Makes a binary operation and performs constant folding if [?constfold] is
    [true] (defaults to [false]). Inserts casts as needed. Use appropriate binop
    ([PlusPI] & friends).

    For pointer comparisons we do the following:
    - If both types are equal, do dothing
    - If both types are compatible, cast the second expression to the first type
    - If both types are object pointers, cast to [void*]
    - Else cast to [uintptr_t]

    The [Result.Error] contains an optional [location] to target a specific
    operand and an error message.

    @before 33.0-Arsenic the function could raise [AbortFatal] instead of using
    result type. It still can raise an exception via sub-function calls. The
    parameter [?constfold] was not present and we always applied constant
    folding.
*)
val mkBinOp: ?constfold:bool -> loc:location -> binop -> exp -> exp ->
  (exp, (location option * string)) result

(** Same as {!mkBinOp} but handles [Error] by throwing an exception with the
    given message and current location.
    @raise Abortfatal if {!mkBinOp} fails
    @since 33.0-Arsenic
*)
val mkBinOp_exn: ?constfold:bool -> loc:location -> binop -> exp -> exp -> exp

(** Same as {!mkBinOp_exn}
    @before 33.0-Arsenic Performed a systematic cast (unless one of the
    arguments was [0]) of pointers into [uintptr_t] during comparisons,
    making such operation defined even if the pointers do not share
    the same base. This was the behavior of {!mkBinOp} prior to the
    introduction of this function.
    @since Chlorine-20180501
*)
val mkBinOp_safe_ptr_cmp: loc:location -> binop -> exp ->
  exp -> exp
[@@deprecated "Use mkBinOp_exn instead, which is now safe to use."]
[@@migrate { repl = Rel.mkBinOp_exn }]

(** Equivalent to [mkMem] for terms. *)
val mkTermMem: addr:term -> off:term_offset -> term_lval


(** [true] if both types are not equivalent.
    if [force] is [true], returns [true] whenever both types are not equal
    (modulo typedefs). If [force] is [false] (the default), other equivalences
    are considered, in particular between an enum and its representative
    integer type.
*)
val need_cast: ?force:bool -> typ -> typ -> bool

(** [typeForInsertedCast expr original_type destination_type]
    returns the type into which [expr], which has type [original_type] and
    whose type must be converted into [destination_type], must be casted.

    By default, returns [destination_type].

    This applies only to implicit casts. Casts already present
    in the source code are exempt from this hook.

    @since 28.0-Nickel
*)
val typeForInsertedCast: (exp -> typ -> typ -> typ) ref

(** [checkCast context fromsource nullptr_cast oldt newt] emits a warning
    or an error if the cast from [oldt] to [newt] is invalid (does nothing
    otherwise).
    [nullptr_cast] is [true] iff the expression being casted is a null pointer.
    Default is false.
    [fromsource] is [false] (default) if the cast is not present in the source
    code.
    Check [areCompatibleTypes] documentation for [context].

    Suspicious cases that only emit a warning:
    - Implicit cast from a pointer to an integer.
    - Cast from a pointer to a function type to another pointer to a function
      type when the function types are not compatible.
    - Cast from an array to a pointer/array when types are not compatible.
    - Cast, in both directions, between pointer to an object type and pointer
      to a function type.

    @since 28.0-Nickel
*)
val checkCast:
  ?context:qualifier_check_context ->
  ?nullptr_cast:bool ->
  ?fromsource:bool ->
  typ -> typ -> unit


(** Generic version of {!Cil.mkCastT}.
    Construct a cast when having the old type of the expression.
    [fromsource] is [false] (default) if the cast is not present in the source
    code.
    If [check] is [true] (default), we check that the cast is valid,
    emitting an error or warning if the cast is invalid.
    If the new type is the same as the old type, then no cast is added,
    unless [force] is [true] (default is [false]).
    Cast from [oldt] to [newt], returning the new type and the new expression.

    @since 28.0-Nickel
*)
val mkCastTGen: ?check:bool -> ?context:qualifier_check_context ->
  ?fromsource:bool -> ?force:bool -> oldt:typ -> newt:typ -> exp -> typ * exp

(** Construct a cast when having the old type of the expression. If the new
    type is the same as the old type, then no cast is added, unless [force]
    is [true] (default is [false]).
    Emit an error or warning if [check] is true and the cast is invalid.
    @before 23.0-Vanadium different order of arguments.
    @before 28.0-Nickel no [check] argument, it was always [false].
*)
val mkCastT: ?check:bool -> ?force:bool -> oldt:typ -> newt:typ -> exp -> exp

(** Like {!Cil.mkCastT}, but uses [typeOf] to get [oldt].
    @before 23.0-Vanadium different order of arguments.
    @before 28.0-Nickel no [check] argument, it was always [false].
*)
val mkCast: ?check:bool -> ?force:bool -> newt:typ -> exp -> exp

(** Equivalent to [stripCasts] for terms. *)
val stripTermCasts: term -> term

(** Removes casts from this expression, but ignores casts within
    other expression constructs.  So we delete the (A) and (B) casts from
    "(A)(B)(x + (C)y)", but leave the (C) cast. *)
val stripCasts: exp -> exp

val typeOf: exp -> typ
(** Compute the type of an expression. *)

val typeOf_string_literal: ?loc:location -> string -> typ
(** Returns the type (a char array of fixed length) of a string literal
    @since 32.0-Germanium
*)

val typeOf_wstring_literal: ?loc:location -> int64 list -> typ
(** Return the type (a wchar_t array of fixed length) of a wide string literal
    @since 32.0-Germanium
*)

val is_fully_arithmetic: typ -> bool
(** Returns [true] whenever the type contains only arithmetic types *)

(** Convert a string representing a C integer literal to an Z.
    Handles the prefixes 0x and 0 and the suffixes L, U, UL, LL, ULL. *)
val parseInt: string -> Z.t

(** Like [parseInt], but returns [Error message] in case of failure, instead of
    aborting Frama-C.
    @since 24.0-Chromium *)
val parseIntRes: string -> (Z.t, string) result

(** Like [parseInt], but converts to an expression. *)
val parseIntExp: loc:location -> string -> exp

(** Like [parseIntExp], but returns [Error message] in case of failure, instead
    of aborting Frama-C.
    @since 24.0-Chromium *)
val parseIntExpRes: loc:location -> string -> (exp, string) result

(** Like [parseInt], but converts to a logic term. *)
val parseIntLogic: loc:location -> string -> term

val appears_in_expr: varinfo -> exp -> bool
(** @return true if the given variable appears in the expression. *)

(**********************************************)
(** {3 Values for manipulating statements} *)
(**********************************************)

(** Construct a statement, given its kind. Initialize the [sid] field to -1
    if [valid_sid] is false (the default),
    or to a valid sid if [valid_sid] is true,
    and [labels], [succs] and [preds] to the empty list *)
val mkStmt: ?ghost:bool -> ?valid_sid:bool -> ?sattr:attributes -> stmtkind ->
  stmt

(* make the [new_stmtkind] changing the CFG relatively to [ref_stmt] *)
val mkStmtCfg: before:bool -> new_stmtkind:stmtkind -> ref_stmt:stmt -> stmt

(** Construct a block with no attributes, given a list of statements *)
val mkBlock: stmt list -> block

(** Construct a non-scoping block, i.e. a block that is not used to determine
    the end of scope of local variables. Hence, the blocals of such a block
    must always be empty.

    @since Phosphorus-20170501-beta1
*)
val mkBlockNonScoping: stmt list -> block

(** Construct a block with no attributes, given a list of statements and
    wrap it into the Cfg. *)
val mkStmtCfgBlock: stmt list -> stmt

(** Construct a statement consisting of just one instruction
    See {!Cil.mkStmt} for the signification of the optional args.
*)
val mkStmtOneInstr: ?ghost:bool -> ?valid_sid:bool -> ?sattr:attributes ->
  instr -> stmt

(** Returns an empty statement (of kind [Instr]). See [mkStmt] for [ghost] and
    [valid_sid] arguments.
*)
val mkEmptyStmt: ?ghost:bool -> ?valid_sid:bool -> ?sattr:attributes ->
  ?loc:location -> unit -> stmt

(** Create an instruction equivalent to a pure expression. The new instruction
    corresponds to the initialization of a new fresh variable, i.e.
    [int tmp = exp]. The scope of this fresh variable
    is determined by the block given in argument, that is the instruction
    must be placed directly (modulo non-scoping blocks) inside this block.
*)
val mkPureExprInstr:
  fundec:fundec -> scope:block -> ?loc:location -> exp -> instr

(** Create an instruction as above, enclosed in a block
    of a single ([Instr]) statement, which will be the scope of the fresh
    variable holding the value of the expression.

    See {!Cil.mkStmt} for information about [ghost] and [valid_sid], and
    {!Cil.mkPureExprInstr} for information about [loc].
*)
val mkPureExpr:
  ?ghost:bool -> ?valid_sid:bool -> fundec:fundec ->
  ?loc:location -> exp -> stmt

(** Make a loop. Can contain Break or Continue.
    The kind of loop (while, for, dowhile) is given by [sattr]
    (none by default). Use {!Cil.mkWhile} for a While loop.
    @before 23.0-Vanadium no unit argument, and default type was While
            (for while loops, there is now {!Cil.mkWhile}).
*)
val mkLoop: ?sattr:attributes -> guard:exp -> body:stmt list -> unit ->
  stmt list

(** Make a for loop for(i=start; i<past; i += incr) \{ ... \}. The body
    can contain Break but not Continue. Can be used with i a pointer
    or an integer. Start and done must have the same type but incr
    must be an integer
    @before 23.0-Vanadium did not have unit argument.
*)
val mkForIncr: ?sattr:attributes -> iter:varinfo -> first:exp -> stopat:exp ->
  incr:exp -> body:stmt list -> unit -> stmt list

(** Make a for loop for(start; guard; next) \{ ... \}. The body can
    contain Break but not Continue !!!
    @before 23.0-Vanadium did not have unit argument.
*)
val mkFor: ?sattr:attributes -> start:stmt list -> guard:exp -> next: stmt list ->
  body: stmt list -> unit -> stmt list

(** Make a while loop.
    @since 23.0-Vanadium
*)
val mkWhile: ?sattr:attributes -> guard:exp -> body:stmt list -> unit ->
  stmt list

(** Make a do ... while loop.
    @since 23.0-Vanadium
*)
val mkDoWhile: ?sattr:attributes -> body:stmt list -> guard:exp -> unit ->
  stmt list

(** creates a block with empty attributes from an unspecified sequence. *)
val block_from_unspecified_sequence:
  (stmt * lval list * lval list * lval list * stmt ref list) list -> block

(** [treat_constructor_as_func action v f args kind loc] calls [action] with
    the parameters corresponding to the call to [f], of kind [kind],
    initializing [v] with arguments [args].
    @since Phosphorus-20170501-beta1
    @before 32.0-Germanium [action] took an expression instead of an lhost.
*)
val treat_constructor_as_func:
  (lval option -> lhost -> exp list -> location -> 'a) ->
  varinfo -> varinfo -> exp list -> constructor_kind -> location -> 'a

(** [find_def_stmt b v] returns the [Local_init] instruction within [b] that
    initializes [v]. [v] must have its [vdefined] field set to true, and be
    among [b.blocals].
    @raise Fatal error if [v] is not a local variable of [b] with an
    initializer.
    @since Phosphorus-20170501-beta1
*)
val find_def_stmt: block -> varinfo -> stmt

(** returns [true] iff the given non-scoping block contains local init
    statements (thus of locals belonging to an outer block), either directly or
    within a non-scoping block or undefined sequence.labels

    @since Phosphorus-20170501-beta1
*)
val has_extern_local_init: block -> bool

(** returns [true] iff the given block is a ghost else block.

    @since 21.0-Scandium
*)
val is_ghost_else: block -> bool

val instr_falls_through : instr -> bool
(** returns [false] if the given instruction is a call to a function with a
    ["noreturn"] attribute, and [true] otherwise.

    @since 30.0-Zinc
*)

(* ************************************************************************* *)
(** {2 Values for manipulating attributes} *)
(* ************************************************************************* *)

(** [true] if the underlying left-value of the given expression is allowed to be
    assigned to thanks to a [frama_c_init_obj] attribute. *)
val is_initialized: exp -> bool

(** [true] if the given lval is allowed to be assigned to thanks to
    a [frama_c_init_obj] or a [frama_c_mutable] attribute.
*)
val is_mutable_or_initialized: lval -> bool

(** [true] if the given varinfo is a ghost formal variable.

    @since 20.0-Calcium
*)
val isGhostFormalVarinfo: varinfo -> bool

(** [true] if the given formal declaration corresponds to a ghost formal variable.

    @since 20.0-Calcium
*)
val isGhostFormalVarDecl: (string * typ * attributes) -> bool

(** Remove any attribute appearing somewhere in the fully expanded
    version of the type.
    @since Oxygen-20120901
*)
val typeDeepDropAllAttributes: typ -> typ

(** Convert an expression into an attrparam, if possible. Otherwise raise
    NotAnAttrParam with the offending subexpression *)
val expToAttrParam: exp -> attrparam


(** Return the attributes of the global annotation, if any.
    @since 20.0-Calcium
*)
val global_annotation_attributes: global_annotation -> attributes

(** Return the attributes of the global, if any.
    @since 20.0-Calcium
*)
val global_attributes: global -> attributes

(**
   Whether the given attributes contain libc indicators.
   @since 23.0-Vanadium
*)
val is_in_libc: attributes -> bool

(**
   Whether the given global contains libc indicators.
   @since 23.0-Vanadium
*)
val global_is_in_libc: global -> bool

exception NotAnAttrParam of exp

(* ************************************************************************* *)
(** {2 Const Attribute} *)
(* ************************************************************************* *)

(** [true] iff the given variable is a const global variable with non extern
    storage.

    @since 25.0-Manganese
*)
val isGlobalInitConst: varinfo -> bool

(* ************************************************************************* *)
(** {2 Volatile Attribute} *)
(* ************************************************************************* *)

val isVolatileLval : lval -> bool
(** Check if the l-value has a volatile part
    @since Sulfur-20171101 *)

val isVolatileTermLval : term_lval -> bool
(** Check if the l-value has a volatile part
    @since Sulfur-20171101 *)

(* ************************************************************************* *)
(** {2 The visitor} *)
(* ************************************************************************* *)

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
type 'a visitAction =
  | SkipChildren (** Do not visit the children. Return the node as it is.
                     @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | DoChildren (** Continue with the children of this node. Rebuild the node on
                   return if any of the children changes (use == test).
                   @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | DoChildrenPost of ('a -> 'a)
  (** visit the children, and apply the given function to the result.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | JustCopy (** visit the children, but only to make the necessary copies
                 (only useful for copy visitor).
                 @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | JustCopyPost of ('a -> 'a)
  (** same as JustCopy + applies the given function to the result.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>*)
  | ChangeTo of 'a  (** Replace the expression with the given one.
                        @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | ChangeToPost of 'a * ('a -> 'a)
  (** applies the expression to the function and gives back the result.
      Useful to insert some actions in an inheritance chain.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a)
  (** First consider that the entire exp is replaced by the first parameter. Then
      continue with the children. On return rebuild the node if any of the
      children has changed and then apply the function on the node.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val mk_behavior :
  ?name:string ->
  ?assumes:identified_predicate list ->
  ?requires:identified_predicate list ->
  ?post_cond:(termination_kind * identified_predicate) list ->
  ?assigns:assigns ->
  ?allocation:allocation ->
  ?extended:acsl_extension list ->
  unit ->
  behavior
(** returns a dummy behavior with the default name [Cil.default_behavior_name].
    invariant: [b_assumes] must always be
    empty for behavior named [Cil.default_behavior_name]

    @since Carbon-20101201
*)

val default_behavior_name: string
(** @since Carbon-20101201  *)

val is_default_behavior: behavior -> bool
val find_default_behavior: funspec -> funbehavior option
(** @since Carbon-20101201  *)

val find_default_requires: behavior list -> identified_predicate list
(** @since Carbon-20101201  *)

(* ************************************************************************* *)
(** {2 Visitor mechanism} *)
(* ************************************************************************* *)

(** {3 Visitor class} *)

(** A visitor interface for traversing CIL trees. Create instantiations of
    this type by specializing the class {!nopCilVisitor}. Each of the
    specialized visiting functions can also call the [queueInstr] to specify
    that some instructions should be inserted before the current statement.
    Use syntax like [self#queueInstr] to call a method
    associated with the current object.

    {b Important Note for Frama-C Users:} Unless you really know what you are
    doing, you should probably inherit from the
    {!Visitor.generic_frama_c_visitor} instead of {!genericCilVisitor} or
    {!nopCilVisitor}

    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
class type cilVisitor = object
  method behavior: Visitor_behavior.t
  (** the kind of behavior expected for the behavior.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method project: Project.t option
  (** Project the visitor operates on. Non-nil for copy visitor.
      @since Oxygen-20120901 *)

  method plain_copy_visitor: cilVisitor
  (** a visitor who only does copies of the nodes according to [behavior] *)

  method vfile: file -> file visitAction
  (** visit a whole file.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vvdec: varinfo -> varinfo visitAction
  (** Invoked for each variable declaration. The children to be traversed
      are those corresponding to the type and attributes of the variable.
      Note that variable declarations are [GVar], [GVarDecl], [GFun] and
      [GFunDecl] globals, the formals of functions prototypes, and the
      formals and locals of function definitions. This means that the list
      of formals of a function may be traversed multiple times if there exists
      both a declaration and a definition, or multiple declarations.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vvrbl: varinfo -> varinfo visitAction
  (** Invoked on each variable use. Here only the [SkipChildren] and
      [ChangeTo] actions make sense since there are no subtrees. Note that
      the type and attributes of the variable are not traversed for a
      variable use.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vexpr: exp -> exp visitAction
  (** Invoked on each expression occurrence. The subtrees are the
      subexpressions, the types (for a [Cast] or [SizeOf] expression) or the
      variable use.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlval: lval -> lval visitAction
  (** Invoked on each lvalue occurrence *)

  method vlhost: lhost -> lhost visitAction
  (** Invoked on each lhost occurrence.
      @since 32.0-Germanium
  *)

  method voffs: offset -> offset visitAction
  (** Invoked on each offset occurrence that is *not* as part of an
      initializer list specification, i.e. in an lval or recursively inside an
      offset.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vinitoffs: offset -> offset visitAction
  (** Invoked on each offset appearing in the list of a
      CompoundInit initializer.  *)

  method vinst: instr -> instr list visitAction
  (** Invoked on each instruction occurrence. The [ChangeTo] action can
      replace this instruction with a list of instructions *)

  method vstmt: stmt -> stmt visitAction
  (** Control-flow statement. The default [DoChildren] action does not create a
      new statement when the components change. Instead it updates the contents
      of the original statement. This is done to preserve the sharing with
      [Goto] and [Case] statements that point to the original statement. If you
      use the [ChangeTo] action then you should take care of preserving that
      sharing yourself.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vblock: block -> block visitAction
  (** Block. *)

  method vfunc: fundec -> fundec visitAction
  (** Function definition. Replaced in place. *)

  method vglob: global -> global list visitAction
  (** Global (vars, types, etc.)
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vinit: varinfo -> offset -> init -> init visitAction
  (** Initializers. Pass the global where this occurs, and the offset *)

  method vstr_literal: varinfo -> str_literal -> str_literal visitAction
  (** (Wide-)string literal
      @since 32.0-Germanium
  *)

  method vinit_or_str: varinfo -> init_or_str -> init_or_str visitAction
  (** Global Initializers (either normal one or (wide) string)
      @since 32.0-Germanium
  *)

  method vlocal_init: varinfo -> local_init -> local_init visitAction
  (** local initializer. pass the variable under initialization. *)

  method vtype: typ -> typ visitAction
  (** Use of some type. For typedef, struct, union and enum, the visit is
      done once at the global defining the type. Thus, children of
      [TComp], [TEnum] and [TNamed] are not visited again. *)

  method vcompinfo: compinfo -> compinfo visitAction
  (** declaration of a struct/union *)

  method venuminfo: enuminfo -> enuminfo visitAction
  (** declaration of an enumeration *)

  method vfieldinfo: fieldinfo -> fieldinfo visitAction
  (** visit the declaration of a field of a structure or union *)

  method venumitem: enumitem -> enumitem visitAction
  (** visit the declaration of an enumeration item *)

  method vattr: attribute -> attributes visitAction
  (** Attribute. Each attribute can be replaced by a list *)

  method vattrparam: attrparam -> attrparam visitAction
  (** Attribute parameters. *)

  method queueInstr: instr list -> unit
  (** Add here instructions while visiting to queue them to precede the
      current statement being processed. Use this method only
      when you are visiting an expression that is inside a function body, or a
      statement, because otherwise there will no place for the visitor to place
      your instructions. *)

  (** Gets the queue of instructions and resets the queue. This is done
      automatically for you when you visit statements. *)
  method unqueueInstr: unit -> instr list

  method current_stmt: stmt option
  (** link to the current statement being visited.

      {b NB:} for copy visitor, the stmt is the original one (use
      [get_stmt] to obtain the corresponding copy) *)

  method current_kinstr: kinstr
  (** [Kstmt stmt] when visiting statement stmt, [Kglobal] when called outside
      of a statement.
      @since Carbon-20101201
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method push_stmt : stmt -> unit
  method pop_stmt : stmt -> unit

  method current_func: fundec option
  (** link to the current function being visited.

      {b NB:} for copy visitors, the fundec is the original one. *)

  method set_current_func: fundec -> unit
  method reset_current_func: unit -> unit

  method vlogic_type: logic_type -> logic_type visitAction
  method vmodel_info: model_info -> model_info visitAction
  method videntified_term: identified_term -> identified_term visitAction
  method vterm: term -> term visitAction
  method vterm_node: term_node -> term_node visitAction
  method vterm_lval: term_lval -> term_lval visitAction
  method vterm_lhost: term_lhost -> term_lhost visitAction
  method vterm_offset: term_offset -> term_offset visitAction
  method vlogic_label: logic_label -> logic_label visitAction
  method vlogic_info_decl: logic_info -> logic_info visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlogic_info_use: logic_info -> logic_info visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlogic_type_info_decl: logic_type_info -> logic_type_info visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlogic_type_info_use: logic_type_info -> logic_type_info visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlogic_type_def: logic_type_def -> logic_type_def visitAction
  method vlogic_ctor_info_decl: logic_ctor_info -> logic_ctor_info visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlogic_ctor_info_use: logic_ctor_info -> logic_ctor_info visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlogic_var_decl: logic_var -> logic_var visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vlogic_var_use: logic_var -> logic_var visitAction
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method vquantifiers: quantifiers -> quantifiers visitAction

  method videntified_predicate:
    identified_predicate -> identified_predicate visitAction

  method vpredicate_node: predicate_node -> predicate_node visitAction
  method vpredicate: predicate -> predicate visitAction
  method vbehavior: funbehavior -> funbehavior visitAction
  method vspec: funspec -> funspec visitAction
  method vassigns: assigns -> assigns visitAction

  method vfrees:
    identified_term list -> identified_term list visitAction
  (**   @since Oxygen-20120901 *)

  method vallocates:
    identified_term list -> identified_term list visitAction
  (**   @since Oxygen-20120901 *)

  method vallocation: allocation -> allocation visitAction
  (**   @since Oxygen-20120901 *)

  method vdeps: deps -> deps visitAction
  method vfrom: from -> from visitAction
  method vcode_annot: code_annotation -> code_annotation visitAction
  method vannotation: global_annotation -> global_annotation visitAction

  method fill_global_tables: unit
  (** fill the global environment tables at the end of a full copy in a
      new project.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  method get_filling_actions: (unit -> unit) Queue.t
  (** get the queue of actions to be performed at the end of a full copy.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

end

(**/**)
class internal_genericCilVisitor:
  fundec option ref -> Visitor_behavior.t -> (unit->unit) Queue.t -> cilVisitor
(**/**)

(** generic visitor, parameterized by its copying behavior.
    Traverses the CIL tree without modifying anything *)
class genericCilVisitor: Visitor_behavior.t -> cilVisitor

(** Default in place visitor doing nothing and operating on current project. *)
class nopCilVisitor: cilVisitor

(** {3 Generic visit functions} *)

(** [doVisit vis deepCopyVisitor copy action children node]
    visits a [node]
    (or its copy according to the result of [copy]) and if needed
    its [children]. {b Do not use it if you don't understand Cil visitor
    mechanism}
    @param vis the visitor performing the needed transformations. The open
    type allows for extensions to Cil to be visited by the same mechanisms.
    @param deepCopyVisitor a generator for a visitor of the same type
    of the current one that performs a deep copy of the AST.
    Needed when the visitAction is [SkipChildren] or [ChangeTo] and [vis]
    is a copy visitor (we need to finish the copy anyway)
    @param copy function that may return a copy of the actual node.
    @param action the visiting function for the current node
    @param children what to do on the children of the current node
    @param node the current node
*)
val doVisit:
  'visitor -> 'visitor ->
  ('a -> 'a) ->
  ('a -> 'a visitAction) ->
  ('visitor -> 'a -> 'a) -> 'a -> 'a

(** same as above, but can return a list of nodes *)
val doVisitList:
  'visitor -> 'visitor ->
  ('a -> 'a) ->
  ('a -> 'a list visitAction) ->
  ('visitor -> 'a -> 'a) -> 'a -> 'a list

(* other cil constructs *)

(** {3 Visitor's entry points} *)

(** Visit a file. This will re-cons all globals TWICE (so that it is
    tail-recursive). Use {!Cil.visitCilFileSameGlobals} if your visitor will
    not change the list of globals.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
val visitCilFileCopy: cilVisitor -> file -> file

(** Same thing, but the result is ignored. The given visitor must thus be
    an inplace visitor. Nothing is done if the visitor is a copy visitor.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
val visitCilFile: cilVisitor -> file -> unit

(** A visitor for the whole file that does not *physically* change the
    globals (but maybe changes things inside the globals through
    side-effects). Use this function instead of {!Cil.visitCilFile}
    whenever appropriate because it is more efficient for long files.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
val visitCilFileSameGlobals: cilVisitor -> file -> unit

(** Same as {!Cil.visitCilFileSameGlobals}, but only visits function definitions
    (i.e. behaves as if all globals but [GFun] return [SkipChildren]).
    @since 25.0-Manganese
*)
val visitCilFileFunctions: cilVisitor -> file -> unit

(** Visit a global *)
val visitCilGlobal: cilVisitor -> global -> global list

(** Visit a function definition *)
val visitCilFunction: cilVisitor -> fundec -> fundec

(* Visit an expression *)
val visitCilExpr: cilVisitor -> exp -> exp

val visitCilEnumInfo: cilVisitor -> enuminfo -> enuminfo

(** Visit an lvalue *)
val visitCilLval: cilVisitor -> lval -> lval

(** Visit an lhost.
    @since 32.0-Germanium
*)
val visitCilLhost: cilVisitor -> lhost -> lhost

(** Visit an lvalue or recursive offset *)
val visitCilOffset: cilVisitor -> offset -> offset

(** Visit an initializer offset *)
val visitCilInitOffset: cilVisitor -> offset -> offset

(** Visit a local initializer (with the local being initialized). *)
val visitCilLocal_init: cilVisitor -> varinfo -> local_init -> local_init

(** Visit an instruction *)
val visitCilInstr: cilVisitor -> instr -> instr list

(** Visit a statement *)
val visitCilStmt: cilVisitor -> stmt -> stmt

(** Visit a block *)
val visitCilBlock: cilVisitor -> block -> block

(** Mark the given block as candidate to be flattened into its parent block,
    after returning from its visit. This is not systematic, as the environment
    might prevent it (e.g. if the preceding statement is a statement contract
    or if there are labels involved).
    Use that whenever you're creating a block in order to hold multiple
    statements as a result of visiting a single statement. If the block
    contains local variables, it will not be marked as transient, since
    removing it will change the scope of those variables.

    @raise Fatal error if the given block attempts to declare local variables
    and contain definitions of local variables that are not part of the block.

    @since Phosphorus-20170501-beta1
*)
val transient_block: block -> block

(** tells whether the block has been marked as transient

    @since Phosphorus-20170501-beta1.
*)
val is_transient_block: block -> bool

(** [flatten_transient_sub_blocks b] flattens all direct sub-blocks of [b]
    that have been marked as cleanable, whenever possible

    @since Phosphorus-20170501-beta1
*)
val flatten_transient_sub_blocks: block -> block

(**/**)

(** Internal usage only. *)

(** Indicates that the potentially transient block given as argument
    must in fact be preserved after the visit. The resulting block will
    be marked as non-scoping.

    @since Phosphorus-20170501-beta1.
*)
val block_of_transient: block -> block

(**/**)

(** Visit a type *)
val visitCilType: cilVisitor -> typ -> typ

(** Visit a variable declaration *)
val visitCilVarDecl: cilVisitor -> varinfo -> varinfo

(** Visit an initializer, pass also the global to which this belongs and the
    offset. *)
val visitCilInit: cilVisitor -> varinfo -> offset -> init -> init

(** Visit a (wide-)string literal. The [varinfo] is its representative
    (see {!Globals.Vars.get_string_literal} for more information)
    @since 32.0-Germanium
*)
val visitCilStr_literal: cilVisitor -> varinfo -> str_literal -> str_literal

(** Visit a normal initializer or a string literal.
    @since 32.0-Germanium
*)
val visitCilInit_or_str: cilVisitor -> varinfo -> init_or_str -> init_or_str

(** Visit a list of attributes *)
val visitCilAttributes: cilVisitor -> attributes -> attributes

val visitCilAnnotation: cilVisitor -> global_annotation -> global_annotation

val visitCilCodeAnnotation: cilVisitor -> code_annotation -> code_annotation

val visitCilDeps: cilVisitor -> deps -> deps

val visitCilFrom: cilVisitor -> from -> from

val visitCilAssigns: cilVisitor -> assigns -> assigns

(** @since Oxygen-20120901 *)
val visitCilFrees:
  cilVisitor -> identified_term list -> identified_term list

(** @since Oxygen-20120901 *)
val visitCilAllocates:
  cilVisitor -> identified_term list -> identified_term list

(** @since Oxygen-20120901 *)
val visitCilAllocation: cilVisitor -> allocation -> allocation

val visitCilFunspec: cilVisitor -> funspec -> funspec

val visitCilBehavior: cilVisitor -> funbehavior -> funbehavior
val visitCilBehaviors: cilVisitor -> funbehavior list -> funbehavior list

(** visit an extended clause of a behavior.
    @since Nitrogen-20111001
*)
val visitCilExtended: cilVisitor -> acsl_extension -> acsl_extension

val visitCilModelInfo: cilVisitor -> model_info -> model_info

val visitCilLogicType: cilVisitor -> logic_type -> logic_type

val visitCilIdPredicate:
  cilVisitor -> identified_predicate -> identified_predicate

val visitCilPredicateNode: cilVisitor -> predicate_node -> predicate_node

val visitCilPredicate: cilVisitor -> predicate -> predicate

val visitCilPredicates:
  cilVisitor -> identified_predicate list -> identified_predicate list

val visitCilTerm: cilVisitor -> term -> term

(** visit identified_term.
    @since Oxygen-20120901
*)
val visitCilIdTerm: cilVisitor -> identified_term -> identified_term

(** visit term_lval.
    @since Nitrogen-20111001
*)
val visitCilTermLval: cilVisitor -> term_lval -> term_lval

val visitCilTermLhost: cilVisitor -> term_lhost -> term_lhost

val visitCilTermOffset: cilVisitor -> term_offset -> term_offset

val visitCilLogicInfo: cilVisitor -> logic_info -> logic_info

val visitCilLogicVarUse: cilVisitor -> logic_var -> logic_var

val visitCilLogicVarDecl: cilVisitor -> logic_var -> logic_var

(** {3 Visiting children of a node} *)

val childrenBehavior: cilVisitor -> funbehavior -> funbehavior

(* And some generic visitors. The above are built with these *)

(* ************************************************************************* *)
(** {2 Utility functions} *)
(* ************************************************************************* *)

val is_skip: stmtkind -> bool

(** A visitor that does constant folding. Pass as argument whether you want
    machine specific simplifications to be done, or not. *)
val constFoldVisitor: bool -> cilVisitor

(* ************************************************************************* *)
(** {2 Debugging support} *)
(* ************************************************************************* *)

(** Pretty-print [(Current_loc.get ())] *)
val pp_thisloc: Format.formatter -> unit

(** @return a dummy specification *)
val empty_funspec : unit -> funspec

(** @return true if the given spec is empty. *)
val is_empty_funspec: funspec -> bool

(** @return true if the given behavior is empty. *)
val is_empty_behavior: funbehavior -> bool

(* ************************************************************************* *)
(** {2 Renaming} *)

(** See also the {!Alpha} module for other renaming operations. *)
(* ************************************************************************* *)

(** Assign unique names to local variables. This might be necessary after you
    transformed the code and added or renamed some new variables. Names are not
    used by CIL internally, but once you print the file out the compiler
    downstream might be confused. You might have added a new global that happens
    to have the same name as a local in some function. Rename the local to
    ensure that there would never be confusion. Or, viceversa, you might have
    added a local with a name that conflicts with a global *)
val uniqueVarNames: file -> unit

(* ************************************************************************* *)
(** {2 Optimization Passes} *)
(* ************************************************************************* *)

(** A peephole optimizer that processes two adjacent statements and possibly
    replaces them both. If some replacement happens and [aggressive] is true,
    then the new statements are themselves subject to optimization.  Each
    statement in the list is optimized independently. *)
val peepHole2:
  aggressive:bool -> (stmt * stmt -> stmt list option) -> stmt list -> stmt list

(** Similar to [peepHole2] except that the optimization window consists of
    one statement, not two *)
val peepHole1: (instr -> instr list option) -> stmt list -> unit

(* ************************************************************************* *)
(** {2 Machine dependency} *)
(* ************************************************************************* *)

(** Raised when one of the SizeOf/AlignOf functions cannot compute the size of a
    type. This can happen because the type contains array-length expressions
    that we don't know how to compute or because it is a type whose size is not
    defined (e.g. TFun or an undefined compinfo). The string is an explanation
    of the error *)
exception SizeOfError of string * typ

(** Give the unsigned kind corresponding to any integer kind *)
val unsignedVersionOf : ikind -> ikind

(** The signed integer kind for a given size (unsigned if second argument
    is true). Raises Not_found if no such kind exists *)
val intKindForSize : int -> bool -> ikind

(** The float kind for a given size. Raises Not_found
    if no such kind exists *)
val floatKindForSize : int-> fkind

(** The size of a type, in bits. Trailing padding is added for structs and
    arrays. Raises {!Cil.SizeOfError} when it cannot compute the size. This
    function is architecture dependent, so you should only call this after you
    call {!Machine.init}. Remember that on GCC sizeof(void) is 1! *)
val bitsSizeOf: typ -> int

(** The size of a type, in bytes. Raises {!Cil.SizeOfError} when it cannot
    compute the size. *)
val bytesSizeOf: typ -> int

(** Returns the number of bytes (resp. bits) to represent the given integer
    kind depending on the current machdep. *)
val bytesSizeOfInt: ikind -> int
val bitsSizeOfInt: ikind -> int

(** Returns the signedness of the given integer kind depending
    on the current machdep. *)
val isSigned: ikind -> bool

(** Returns the size of the given type, in bits. If this is the type of
    an lvalue which is a bitfield, the size of the bitfield is returned. *)
val bitsSizeOfBitfield: typ -> int

val selfTypSize: State.t
(** Cache for sizeof *)

(** Returns a unique number representing the integer
    conversion rank. *)
val rank: ikind -> int

(** [intTypeIncluded i1 i2] returns [true] iff the range of values
    representable in [i1] is included in the one of [i2] *)
val intTypeIncluded: ikind -> ikind -> bool

(** Returns a unique number representing the floating-point conversion rank.
    @since Oxygen-20120901 *)
val frank: fkind -> int

(** Represents an integer as for a given kind.
    Returns a flag saying whether the value was changed
    during truncation (because it was too large to fit in k). *)
val truncateInteger64: ikind -> Z.t -> Z.t * bool

(** Returns the maximal value representable in a signed integer type of the
    given size (in bits)
*)
val max_signed_number: int -> Z.t

(** Returns the smallest value representable in a signed integer type of the
    given size (in bits)
*)
val min_signed_number: int -> Z.t

(** Returns the maximal value representable in a unsigned integer type of the
    given size (in bits)
*)
val max_unsigned_number: int -> Z.t

(** True if the integer fits within the kind's range *)
val fitsInInt: ikind -> Z.t -> bool

(** True if the float is finite for the kind's range *)
val isFiniteFloat: fkind -> float -> bool

(** True if the real constant is an exact float for the given type *)
val isExactFloat: fkind -> logic_real -> bool

exception Not_representable
(** raised by {!intKindForValue}. *)

(** @return the smallest kind that will hold the integer's value.
    The kind will be unsigned if the 2nd argument is true.
    @raise Not_representable if the bigint is not representable.
*)
val intKindForValue: Z.t -> bool -> ikind

(** The size of a type, in bytes. Returns a constant expression or a "sizeof"
    expression if it cannot compute the size. This function is architecture
    dependent, so you should only call this after you call {!Machine.init}.  *)
val sizeOf: loc:location -> typ -> exp

(** The minimum alignment (in bytes) for a type. This function is
    architecture dependent, so you should only call this after you call
    {!Machine.init}.
    @raise {!SizeOfError} when it cannot compute the alignment. *)
val bytesAlignOf: typ -> int

(** The alignment of the variable according to:
    - its _Alignas specifier, or if unspecified,
    - its type.
      @raise {!SizeOfError} when it cannot compute the alignment.
      @since 32.0-Germanium
*)
val bytesAlignOfVarinfo: varinfo -> int

(** The alignment of the variable according to:
    - its _Alignas specifier, or if unspecified,
    - its type.
      @raise {!SizeOfError} when it cannot compute the alignment.
      @since 32.0-Germanium
*)
val bytesAlignOfField: fieldinfo -> int

(** [intOfAttrparam a] tries to const-fold [a] into a numeric value.
    Returns [Some n] if it succeeds, [None] otherwise.
    @since Silicium-20161101 *)
val intOfAttrparam: attrparam -> int option

(** Give a type of a base and an offset, returns the number of bits from the
    base address and the width (also expressed in bits) for the subobject
    denoted by the offset. Raises {!Cil.SizeOfError} when it cannot compute
    the size. This function is architecture dependent, so you should only call
    this after you call {!Machine.init}. *)
val bitsOffset: typ -> offset -> int * int

(** Give a field, returns the number of bits from the structure or union
    containing the field and the width (also expressed in bits) for the subobject
    denoted by the field. Raises {!Cil.SizeOfError} when it cannot compute
    the size. This function is architecture dependent, so you should only call
    this after you call {!Machine.init}. *)
val fieldBitsOffset: fieldinfo -> int * int

val create_string_literal: ?loc:Cil_types.location -> string -> varinfo
(** creates a new varinfo for holding a string literal

    **NB** this function only creates the variable, but does not insert its
    definition in an AST. Use {!Globals.Vars.add_string_literal} to properly
    create a new global.

    @since 32.0-Germanium
*)

val create_wstring_literal: ?loc:Cil_types.location -> int64 list -> varinfo
(** creates a new varinfo for holding a string literal

    **NB** this function only creates the variable, but does not insert its
    definition in an AST. Use {!Globals.Vars.add_string_literal} to properly
    create a new global.

    @since 32.0-Germanium
*)


(* ************************************************************************* *)
(** {2 Misc} *)
(* ************************************************************************* *)

(** if the list has 2 elements or more, it will return a block with
    [bscoping=false]
*)
val stmt_of_instr_list : ?loc:location -> instr list -> stmtkind

(** Convert a C variable into the corresponding logic variable.
    The returned logic variable is unique for a given C variable. *)
val cvar_to_lvar : varinfo -> logic_var

(** Convert a C variable into a logic term.
    @since 24.0-Chromium *)
val cvar_to_term: loc:location -> varinfo -> term

(** Make a temporary variable to use in annotations *)
val make_temp_logic_var: logic_type -> logic_var

(** The constant logic term zero.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
val lzero : ?loc:location -> unit -> term

(** The constant logic term 1. *)
val lone : ?loc:location -> unit -> term

(** The constant logic term -1. *)
val lmone : ?loc:location -> unit -> term

(** The given constant logic term *)
val lconstant : ?loc:location -> Z.t -> term

(** Bind all free variables with an universal quantifier *)
val close_predicate : predicate -> predicate

(** extract [varinfo] elements from an [exp] *)
val extract_varinfos_from_exp : exp -> Varinfo.Set.t

(** extract [varinfo] elements from an [lval] *)
val extract_varinfos_from_lval : lval -> Varinfo.Set.t

(** extract [logic_var] elements from a [term] *)
val extract_free_logicvars_from_term : term -> Logic_var.Set.t

(** extract [logic_var] elements from a [predicate] *)
val extract_free_logicvars_from_predicate :
  predicate -> Logic_var.Set.t

(** extract from a [predicate] the [logic_info] elements that are applied
    (using [Tapp] or [Papp]). *)
val extract_applied_logic_infos_from_predicate : predicate -> Logic_info.Set.t

(** extract [logic_label] elements from a [code_annotation] *)
val extract_labels_from_annot:
  code_annotation -> Logic_label.Set.t

(** extract [logic_label] elements from a [term] *)
val extract_labels_from_term: term -> Logic_label.Set.t

(** extract [logic_label] elements from a [pred] *)
val extract_labels_from_pred:
  predicate -> Logic_label.Set.t

(** extract [stmt] elements from [logic_label] elements *)
val extract_stmts_from_labels: Logic_label.Set.t -> Stmt.Set.t

(** creates a visitor that will replace in place uses of var in the first
    list by their counterpart in the second list.
    @raise Invalid_argument if the lists have different lengths. *)
val create_alpha_renaming: varinfo list -> varinfo list -> cilVisitor

(** Provided [s] is a switch, [separate_switch_succs s] returns the
    subset of [s.succs] that correspond to the Case labels of [s], and a
    "default statement" that either corresponds to the Default label, or to the
    syntactic successor of [s] if there is no default label. Note that this "default
    statement" can thus appear in the returned list. *)
val separate_switch_succs: stmt -> stmt list * stmt

(** Provided [s] is a if, [separate_if_succs s] splits the successors
    of s according to the truth value of the condition. The first
    element of the pair is the successor statement if the condition is
    true, and the second if the condition is false. *)
val separate_if_succs: stmt -> stmt * stmt

(**/**)

val switch_case_state_self: State.t

val pp_typ_ref: (Format.formatter -> typ -> unit) ref
val pp_binop_ref: (Format.formatter -> binop -> unit) ref
val pp_global_ref: (Format.formatter -> global -> unit) ref
val pp_exp_ref: (Format.formatter -> exp -> unit) ref
val pp_lval_ref: (Format.formatter -> lval -> unit) ref
val pp_ikind_ref: (Format.formatter -> ikind -> unit) ref
val pp_attribute_ref: (Format.formatter -> attribute -> unit) ref
val pp_attributes_ref: (Format.formatter -> attributes -> unit) ref
val pp_term_ref: (Format.formatter -> term -> unit) ref
val pp_logic_type_ref: (Format.formatter -> logic_type -> unit) ref
val pp_identified_term_ref: (Format.formatter -> identified_term -> unit) ref
val pp_from_ref: (Format.formatter -> from -> unit) ref
val pp_behavior_ref: (Format.formatter -> behavior -> unit) ref
val pp_block_ref: (Format.formatter -> block -> unit) ref
val pp_varinfo_ref: (Format.formatter -> varinfo -> unit) ref

val set_extension_handler:
  visit:(plugin:string -> string -> cilVisitor -> acsl_extension_kind ->
         acsl_extension_kind visitAction) ->
  unit
(** Used to setup a reference related to the handling of ACSL extensions.
    @since 21.0-Scandium
    @before 30.0-Zinc This function did not take a [plugin:string] parameter
*)
