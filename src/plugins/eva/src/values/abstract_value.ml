(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Abstract numeric values of the analysis. *)

open Cil_types
open Eval

(** Type for the truth value of an assertion in a value abstraction. The two
    last tags should be used only for a product of value abstractions. *)
type 'v truth =
  [ `False              (* The assertion is always false for the value
                           abstraction, leading to bottom and a red alarm. *)
  | `Unknown of 'v      (* The assertion may be true or false for different
                           concretization of the abstraction. The value is
                           reduced by assuming the assertion. *)
  | `True               (* The assertion is always true for the value
                           abstraction (that cannot be reduced). *)
  | `TrueReduced of 'v  (* The assertion is always true according to a component
                           of a product of values, but other components have
                           been reduced by assuming the assertion. *)
  | `Unreachable ]      (* A product of values was incompatible and has led to
                           inconsistent truth value for the assertion. *)

type bound_kind = Alarms.bound_kind = Lower_bound | Upper_bound
type bound = Int of Z.t | Float of float * fkind

type pointer_comparison = Equality | Relation | Subtraction

(** Enriched context.
    This record could easily be extended to contain more information about the
    context in which an evaluation takes place, if the need arises. *)
type 'a enriched = { from_domains : 'a }

(** Signature of abstract numerical values. *)
module type S = sig
  include Datatype.S

  (** A numerical value abstraction can optionally require some context from
      abstract domains. Most transfer functions take the context as argument;
      it is provided by the abstract state in which operations are performed.
      See {!Abstract_context} for more details about contexts.
      For values that don't need context, this type can be defined as unit
      and the context argument can be safely ignored. *)
  type context

  val pretty_typ: typ option -> t Pretty_utils.formatter
  (** Pretty the abstract value assuming it has the type given as argument. *)

  (** {3 Lattice Structure} *)

  val top : t
  val is_included : t -> t -> bool
  val join : t -> t -> t
  val narrow : t -> t -> t or_bottom

  (** {3 Constructors } *)

  (** Abstract value representing integer 0. Used to reduce the value of
      conditional expression when interpreting if-then-else statement. *)
  val zero: t

  (** [inject_int typ i] returns an abstract value representing integer [i]
      for the C type [typ]. *)
  val inject_int : typ -> Z.t -> t

  (** Abstract value representing all integers; it can be equal to [top]. *)
  val top_int : t

  (** {3 Alarms } *)

  (** These functions are used to create the alarms that report undesirable
      behaviors, when a value abstraction does not meet the prerequisites of an
      operation. Thereafter, the value is assumed to meet them to continue the
      analysis.
      See the documentation of the [truth] type for more details. *)

  (* Assumes that the integer value represents only non zero values. *)
  val assume_non_zero: t -> t truth

  (* [assume_bounded Lower_bound b v] assumes that the value [v] represents
     only values greater or equal to the lower bound [b].
     [assume_bounded Upper_bound b v] assumes that the value [v] represents
     only values lower or equal to the greater bound [b].
     Depending on the bound, [v] is an integer or a floating-point value. *)
  val assume_bounded: bound_kind -> bound -> t -> t truth

  (* Assumes that the floating-point value does not represent NaN.
     If [assume_finite] is true, assumes that the value represents only finite
     floating-point values. *)
  val assume_not_nan: assume_finite:bool -> fkind -> t -> t truth

  (** Assumes that the abstract value only represents well-formed pointer values:
      pointers either to an element of an array object or one past the last
      element of an array object. (A pointer to an object that is not an element
      of an array is viewed as a pointer to the first element of an array of
      length one with the type of the object as its element type.)
      The NULL pointer is always a valid pointer value. Function pointers are
      also considered as valid pointer values for now. *)
  val assume_pointer: t -> t truth

  (** [assume_aligned modu v] assumes that the abstract value [v] represents
      only integer or pointer values aligned to [modu]. *)
  val assume_aligned: int -> t -> t truth

  (* [assume_comparable cmp v1 v2] assumes that the integer or pointer values
     [v1] and [v2] are comparable for [cmp]. Integers are always comparable.
     If one value is a pointer, then both values should be pointers, and:
     and according to [cmp]:
     - if [cmp] is Equality:
       either one pointer is NULL, or both pointers are valid (pointing into
       their object), or both pointers are nearly valid (pointing into or just
       beyond their object) and point to the same object.
     - if [cmp] is Relation:
       both pointers should point into or just beyond the same object;
     - if [cmp] is Subtraction:
       both pointers should point to the same object, without any restriction on
       their offsets. *)
  val assume_comparable: pointer_comparison -> t -> t -> (t * t) truth

  (** {3 Forward Operations } *)

  (** Embeds C constants into value abstractions: returns an abstract value
      for the given constant. The constant cannot be an enumeration constant. *)
  val constant : context enriched -> Eva_ast.exp -> Eva_ast.constant -> t

  (** [forward_unop typ unop v] evaluates the value [unop v], resulting from the
      application of the unary operator [unop] to the value [v].  [typ] is the
      type of [v]. *)
  val forward_unop : context enriched -> typ -> Eva_ast.unop -> t -> t or_bottom

  (** [forward_binop typ binop v1 v2] evaluates the value [v1 binop v2],
      resulting from the application of the binary operator [binop] to the
      values [v1] and [v2]. [typ] is the type of [v1]. *)
  val forward_binop :
    context enriched -> typ -> Eva_ast.binop -> t -> t -> t or_bottom

  (** [rewrap_integer irange t] wraps around the abstract value [t] to fit the
      integer range [irange], assuming 2's complement. Also used on absolute
      addresses for pointer values, seen as unsigned integers. *)
  val rewrap_integer: context enriched -> Eval_typ.integer_range -> t -> t

  (** Abstract evaluation of casts operators from [src_type] to [dst_type]. *)
  val forward_cast :
    context enriched ->
    src_type: Eval_typ.scalar_typ -> dst_type: Eval_typ.scalar_typ ->
    t -> t or_bottom

  (** {3 Backward Operations } *)

  (** For an unary forward operation F, the inverse backward operator B tries to
      reduce the argument values of the operation, given its result.

      It must satisfy:
        if [B arg res] = v
        then ∀ a ⊆ arg such that [F a] ⊆ res, a ⊆ v

      i.e. [B arg res] returns a value [v] larger than all subvalues of [arg]
      whose result through F is included in [res].

      If [F arg] ∈ [res] is impossible, then [v] should be bottom.
      If the value [arg] cannot be reduced, then [v] should be None.

      Any n-ary operator may be considered as a unary operator on a vector
      of values, the inclusion being lifted pointwise.
  *)

  (** Backward evaluation of the binary operation [left binop right = result];
      tries to reduce the argument [left] and [right] according to [result].
      [input_type] is the type of [left], [resulting_type] the type of [result]. *)
  val backward_binop :
    context enriched -> input_type:typ -> resulting_type:typ ->
    Eva_ast.binop -> left:t -> right:t -> result:t ->
    (t option * t option) or_bottom

  (** Backward evaluation of the unary operation [unop arg = res];
      tries to reduce the argument [arg] according to [res].
      [typ_arg] is the type of [arg]. *)
  val backward_unop :
    context enriched -> typ_arg:typ -> Eva_ast.unop ->
    arg:t -> res:t -> t option or_bottom

  (** Backward evaluation of the cast of the value [src_val] of type [src_typ]
      into the value [dst_val] of type [dst_typ]. Tries to reduce [scr_val]
      according to [dst_val]. *)
  val backward_cast:
    context enriched ->
    src_typ: typ ->
    dst_typ: typ ->
    src_val: t ->
    dst_val: t ->
    t option or_bottom

  (** {3 Misc } *)

  val resolve_functions : t -> Kernel_function.t list or_top * bool
  (** [resolve_functions v] returns the list of functions that may be pointed to
      by the abstract value [v] (representing a function pointer). The returned
      boolean must be [true] if some of the values represented by [v] do not
      correspond to functions. It is always safe to return [`Top, true]. *)

  (** For pointer values, [replace_base substitution value] replaces the bases
      pointed to by [value] according to [substitution]. For arithmetic values,
      this function returns the [value] unchanged.  *)
  val replace_base: Base.substitution -> t -> t
end

type 'v key = 'v Structure.Key_Value.key

(** Signature for a leaf module of abstract values. *)
module type Leaf = sig
  include S

  (** The key identifies the module and the type [t] of abstract values. *)
  val key: t key

  (** The abstract context on which this value depends. *)
  val context : context Abstract_context.dependencies
end

(** Eva abstractions are divided between values, locations and domains.
    Locations and domains depend on values, and use this type to declare such
    dependencies. In the standard case, a domain depends on a single value
    module [V] and uses [Leaf (module V)] to declare this dependency. *)
type 'v dependencies =
  | Leaf: (module Leaf with type t = 'v) -> 'v dependencies
  | Node: 'l dependencies * 'r dependencies -> ('l * 'r) dependencies
