(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Hash-consed expressions and lvalues. *)

type unhashconsed_exprs = private E of Eva_ast.exp | LV of Eva_ast.lval
(** lvalues are never stored under a constructor [E], only [LV] *)

(** Raised when the replacement of an lvalue in an expression is impossible. *)
exception NonExchangeable

(** Reason of the replacement of an lvalue [lval]: [Modified] means that the
    value of [lval] has been modified (in which case &lval is unchanged), and
    [Deleted] means that [lval] is no longer in scope (in which case &lval
    raises the NonExchangeable error). *)
type kill_type = Modified | Deleted

module E: Datatype.S with type t = unhashconsed_exprs

(** Datatype + utilities functions for hashconsed exprsessions. *)
module HCE: sig
  include Datatype.S_with_collections
  val self: State.t

  val id: t -> int

  (** Conversions between type [t] and Cil lvalues and expressions. *)

  val of_lval: Eva_ast.lval -> t
  val of_exp: Eva_ast.exp -> t

  val get: t -> unhashconsed_exprs
  val to_exp: t -> Eva_ast.exp
  val to_lval: t -> Eva_ast.lval option
  val is_lval: t -> bool
  val type_of: t -> Cil_types.typ

  (** Replaces all occurrences of the lvalue [late] by the expression [heir].
      @raise NonExchangeable if the replacement is not feasible. *)
  val replace: kill_type -> late:Eva_ast.lval -> heir:Eva_ast.exp -> t -> t
end


(** Hashconsed sets of symbolic expressions. *)
module HCESet: Hptset.S with type elt = HCE.t
                         and type 'a map = 'a Hptmap.Shape(HCE).t

(* Sets of lvalues that appear in an expression. The [addr] field gathers the
   lvalues [lv] appearing as addresses &lv, while the [read] field gathers the
   lvalues whose value is read during the evaluation of the expression.  *)
type lvalues = {
  read : HCESet.t;
  addr : HCESet.t;
}

(* Empty sets of lvalues. *)
val empty_lvalues: lvalues

(** [syntactic_lvalues e] returns the set of lvalues that appear in the
    expression [e]. This is used by the equality domain: the expression [e] will
    be removed from an equality if a lvalue from [syntactic_lvalues e] is
    removed. This function only computes the first lvalues of the expression,
    and does not go through the lvalues (for the expression t[i]+1, only the
    lvalue t[i] is returned). *)
val syntactic_lvalues: Eva_ast.exp -> lvalues


(** Maps from symbolic expressions to their memory dependencies, expressed as a
    {!Memory_zone.t}. *)
module HCEToZone: sig
  include Hptmap_sig.S with type key = HCE.t
                        and type v = Memory_zone.t
                        and type 'a map = 'a Hptmap.Shape(HCE).map

  val is_included: t -> t -> bool
  val union: t -> t -> t
  val inter: t -> t -> t
  val merge: into:t -> t -> t
end


(** Maps from {!Base.t} to set of {!HCE.t}. *)
module BaseToHCESet: sig
  include Hptmap_sig.S with type key = Base.t
                        and type v = HCESet.t
                        and type 'a map = 'a Hptmap.Shape(Base.Base).map

  val union: t -> t -> t
  val inter: t -> t -> t

  val find_default: Base.t -> t -> HCESet.t
  (** returns the empty set when the key is not bound *)
end
