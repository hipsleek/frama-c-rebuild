(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Common Types and Signatures *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Lang.F
open Interpreted_automata
open Sigma

(* -------------------------------------------------------------------------- *)
(** {1 General Definitions} *)
(* -------------------------------------------------------------------------- *)

type 'a sequence = { pre : 'a ; post : 'a }

type 'a binder = { bind: 'b 'c. 'a -> ('b -> 'c) -> 'b -> 'c }

(** Oriented equality or arbitrary relation *)
type equation =
  | Set of term * term (** [Set(a,b)] is [a := b]. *)
  | Assert of pred

(** Access conditions *)
type acs =
  | RW (** Read-Write Access *)
  | RD (** Read-Only Access *)
  | OBJ (** Valid Object Pointer *)

(** Abstract location or concrete value *)
type 'a value =
  | Val of term
  | Loc of 'a

(** Contiguous set of locations *)
type 'a rloc =
  | Rloc of c_object * 'a
  | Rrange of 'a * c_object * term option * term option

(** Structured set of locations *)
type 'a sloc =
  | Sloc of 'a
  | Sarray of 'a * c_object * int (** full sized range (optimized assigns) *)
  | Srange of 'a * c_object * term option * term option
  | Sdescr of var list * 'a * pred

(** Typed set of locations *)
type 'a region = (c_object * 'a sloc) list

(** Logical values, locations, or sets of *)
type 'a logic =
  | Vexp of term
  | Vloc of 'a
  | Vset of Vset.set
  | Lset of 'a sloc list

(** Scope management for locals and formals *)
type scope = Enter | Leave

(** Container for the returned value of a function *)
type 'a result =
  | R_loc of 'a
  | R_var of var

(** Polarity of predicate compilation *)
type polarity = [ `Positive | `Negative | `NoPolarity ]

(* -------------------------------------------------------------------------- *)
(** {1 Reversing Models}

    It is sometimes possible to reverse memory models abstractions
    into ACSL left-values via the definitions below. *)
(* -------------------------------------------------------------------------- *)

(** Reversed ACSL left-value *)
type s_lval = s_host * s_offset list

and s_host =
  | Mvar of varinfo (** Variable *)
  | Mmem of term    (** Pointed value *)
  | Mval of s_lval  (** Pointed value of another abstract left-value *)

and s_offset = Mfield of fieldinfo | Mindex of term

(** Reversed abstract value *)
type mval =
  | Maddr of s_lval (** The value is the address of an l-value in current memory *)
  | Mlval of s_lval (** The value is the value of an l-value in current memory *)
  | Minit of s_lval (** The value is the init state of an l-value in current memory *)
  | Mchunk of Chunk.t (** The value is an abstract memory chunk (description) *)

(** Reversed update *)
type update = Mstore of s_lval * term
(** An update of the ACSL left-value with the given value *)

(* -------------------------------------------------------------------------- *)
(** {1 Memory Models} *)
(* -------------------------------------------------------------------------- *)

(** Memory Models. *)
module type Model =
sig

  (** {2 Model Definition} *)

  val configure : unit -> WpContext.rollback
  (** Initializers to be run before using the model.
      Typically push {!Context} values and returns a function to rollback. *)

  val configure_ia: automaton -> vertex binder
  (** Given an automaton, return a vertex's binder.
      Currently used by the automata compiler to bind current vertex.
      See {!StmtSemantics}. *)

  val datatype : string
  (** For projectification. Must be unique among models. *)

  val hypotheses : MemoryContext.partition -> MemoryContext.partition
  (** Computes the memory model partitioning of the memory locations.
      This function typically adds new elements to the partition received
      in input (that can be empty). *)

  type loc
  (** Representation of the memory location in the model. *)

  type segment = loc rloc

  (** {2 Reversing the Model} *)

  (** Try to interpret a term as an in-memory operation
      located at this program point. Only best-effort
      shall be performed, otherwise return [Mvalue].

      Recognized [Cil] patterns:
      - [Mvar x,[Mindex 0]] is rendered as [*x] when [x] has a pointer type
      - [Mmem p,[Mfield f;...]] is rendered as [p->f...] like in Cil
      - [Mmem p,[Mindex k;...]] is rendered as [p[k]...] to catch Cil [Mem(AddPI(p,k)),...] *)
  val lookup : state -> term -> mval

  (** Try to interpret a sequence of states into updates.

      The result shall be exhaustive with respect to values that are printed as [Memory.mval]
      values at [post] label {i via} the [lookup] function.
      Otherwise, those values would not be pretty-printed to the user. *)
  val updates : state sequence -> Vars.t -> update Bag.t

  val pretty : Format.formatter -> loc -> unit
  (** pretty printing of memory location *)

  (** {2 Memory Model API} *)

  val vars : loc -> Vars.t
  (** Return the logic variables from which the given location depend on. *)

  val occurs : var -> loc -> bool
  (** Test if a location depend on a given logic variable *)

  val null : loc
  (** Return the location of the null pointer *)

  val cvar : varinfo -> loc
  (** Return the location of a C variable. *)

  val pointer_loc : term -> loc
  (** Interpret an address value (a pointer) as an abstract location.
      Might fail on memory models not supporting pointers. *)

  val pointer_val : loc -> term
  (** Return the address value (a pointer) of an abstract location.
      Might fail on memory models not capable of representing pointers. *)

  val field : loc -> fieldinfo -> loc
  (** Return the memory location obtained by field access from a given
      memory location. *)

  val shift : loc -> c_object -> term -> loc
  (** Return the memory location obtained by array access at an index
      represented by the given {!term}. The element of the array are of
      the given {!c_object} type. *)

  val base_addr : loc -> loc
  (** Return the memory location of the base address of a given memory
      location. *)

  val base_offset : loc -> term
  (** Return the offset of the location, in bytes, from its base_addr. *)

  val block_length : sigma -> c_object -> loc -> term
  (**  Returns the length (in bytes) of the allocated block containing
       the given location. *)

  val cast : c_object sequence -> loc -> loc
  (** Cast a memory location into another memory location.
      For [cast ty loc] the cast is done from [ty.pre] to [ty.post].
      Might fail on memory models not supporting pointer casts. *)

  val loc_of_int : c_object -> term -> loc
  (** Cast a term representing an absolute memory address (to some c_object)
      given as an integer, into an abstract memory location. *)

  val int_of_loc : c_int -> loc -> term
  (** Cast a memory location into its absolute memory address,
      given as an integer with the given C-type. *)

  val domain : c_object -> loc -> domain
  (** Compute the set of chunks that hold the value of an object with
      the given C-type. It is safe to return an over-approximation of the
      chunks involved. *)

  val is_well_formed : sigma -> pred
  (** Provides the constraint corresponding to the kind of data stored by all
      chunks in sigma. *)

  val load : sigma -> c_object -> loc -> loc value
  (** Return the value of the object of the given type at the given location in
      the given memory state. *)

  val load_init : sigma -> c_object -> loc -> term
  (** Return the initialization status at the given location in the given
      memory state. *)

  val copied : sigma sequence -> c_object -> loc -> loc -> equation list
  (**
     Return a set of equations that express a copy between two memory state.

     [copied sigma ty loc1 loc2] returns a set of formula expressing that the
     content for an object [ty] is the same in [sigma.pre] at [loc1] and in
     [sigma.post] at [loc2].
  *)

  val copied_init : sigma sequence -> c_object -> loc -> loc -> equation list
  (**
     Return a set of equations that express a copy of an initialized state
     between two memory state.

     [copied sigma ty loc1 loc2] returns a set of formula expressing that the
     initialization status for an object [ty] is the same in [sigma.pre] at
     [loc1] and in [sigma.post] at [loc2].
  *)

  val stored : sigma sequence -> c_object -> loc -> term -> equation list
  (**
     Return a set of formula that express a modification between two memory
     state.

     [stored sigma ty loc t] returns a set of formula expressing that
     [sigma.pre] and [sigma.post] are identical except for an object [ty] at
     location [loc] which is represented by [t] in [sigma.post].
  *)

  val stored_init : sigma sequence -> c_object -> loc -> term -> equation list
  (**
     Return a set of formula that express a modification of the initialization
     status between two memory state.

     [stored_init sigma ty loc t] returns a set of formula expressing that
     [sigma.pre] and [sigma.post] are identical except for an object [ty] at
     location [loc] which has a new init represented by [t] in [sigma.post].
  *)

  val assigned : sigma sequence -> c_object -> loc sloc -> equation list
  (**
     Return a set of formula that express that two memory state are the same
     except at the given set of memory location.

     This function can over-approximate the set of given memory location (e.g
     it can return [true] as if the all set of memory location was given).
  *)

  val is_null : loc -> pred
  (** Return the formula that check if a given location is null *)

  val loc_eq : loc -> loc -> pred
  val loc_lt : loc -> loc -> pred
  val loc_neq : loc -> loc -> pred
  val loc_leq : loc -> loc -> pred
  (** Memory location comparisons *)

  val loc_diff : c_object -> loc -> loc -> term
  (** Compute the length in bytes between two memory locations *)

  val valid : sigma -> acs -> segment -> pred
  (** Return the formula that tests if a memory state is valid
      (according to {!acs}) in the given memory state at the given
      segment.
  *)

  val frame : sigma -> pred list
  (** Assert the memory is a proper heap state preceding the function
      entry point. *)

  val alloc : sigma -> varinfo list -> sigma
  (** Allocates new chunk for the validity of variables. *)

  val initialized : sigma -> segment -> pred
  (** Return the formula that tests if a memory state is initialized
      (according to {!acs}) in the given memory state at the given
      segment.
  *)

  val invalid : sigma -> segment -> pred
  (** Returns the formula that tests if the entire memory is invalid
      for write access. *)

  val scope : sigma sequence -> scope -> varinfo list -> pred list
  (** Manage the scope of variables.  Returns the updated memory model
      and hypotheses modeling the new validity-scope of the variables. *)

  val global : sigma -> term -> pred
  (** Given a pointer value [p], assumes this pointer [p] (when valid)
      is allocated outside the function frame under analysis. This means
      separated from the formals and locals of the function. *)

  val included : segment -> segment -> pred
  (** Return the formula that tests if two segment are included *)

  val separated : segment -> segment -> pred
  (** Return the formula that tests if two segment are separated *)

end

(* -------------------------------------------------------------------------- *)
(** {1 C and ACSL Compilers} *)
(* -------------------------------------------------------------------------- *)

(** Compiler for C expressions *)
module type CodeSemantics =
sig

  module M : Model (** The underlying memory model *)

  type loc = M.loc
  type nonrec value = loc value
  type nonrec result = loc result

  val pp_value : Format.formatter -> value -> unit

  val cval : value -> term
  (** Evaluate an abstract value. May fail because of [M.pointer_val]. *)

  val cloc : value -> loc
  (** Interpret a value as a location. May fail because of [M.pointer_loc]. *)

  val cast : typ -> typ -> value -> value
  (** Applies a pointer cast or a conversion.

      [cast tr te ve] transforms a value [ve] with type [te] into a value
      with type [tr]. *)

  val equal_typ : typ -> value -> value -> pred
  (** Computes the value of [(a==b)] provided both [a] and [b] are values
      with the given type. *)

  val not_equal_typ : typ -> value -> value -> pred
  (** Computes the value of [(a==b)] provided both [a] and [b] are values
      with the given type. *)

  val equal_obj : c_object -> value -> value -> pred
  (** Same as [equal_typ] with an object type. *)

  val not_equal_obj : c_object -> value -> value -> pred
  (** Same as [not_equal_typ] with an object type. *)

  val exp : sigma -> exp -> value
  (** Evaluate the expression on the given memory state. *)

  val cond : sigma -> exp -> pred
  (** Evaluate the conditional expression on the given memory state. *)

  val lval : sigma -> lval -> loc
  (** Evaluate the left-value on the given memory state. *)

  val instance_of : loc -> kernel_function -> pred
  (** Check whether a function pointer is (an instance of)
      some kernel function. Currently, the meaning
      of "{i being an instance of}" is simply equality. *)

  val loc_of_exp : sigma -> exp -> loc
  (** Compile an expression as a location.
      May (also) fail because of [M.pointer_val]. *)

  val val_of_exp : sigma -> exp -> term
  (** Compile an expression as a term.
      May (also) fail because of [M.pointer_loc]. *)

  val result : sigma -> typ -> result -> term
  (** Value of an abstract result container. *)

  val return : sigma -> typ -> exp -> term
  (** Return an expression with a given type.
      Short cut for compiling the expression, cast into the desired type,
      and finally converted to a term. *)

  val is_zero : sigma -> c_object -> loc -> pred
  (** Express that the object (of specified type) at the given location
      is filled with zeroes. *)

  (**
     Express that all objects in a range of locations have a given value.

     More precisely, [is_exp_range sigma loc ty a b v] express that
     value at [( ty* )loc + k] equals [v], forall [a <= k <= b].
     Value [v=None] stands for zero.
  *)
  val is_exp_range :
    sigma -> loc -> c_object -> term -> term ->
    value option ->
    pred

  val unchanged : sigma -> sigma -> varinfo -> pred
  (** Express that a given variable has the same value in two memory states. *)

  type warned_hyp = Warning.Set.t * (pred * pred)

  val init : sigma:sigma -> varinfo -> init_or_str option -> warned_hyp list
  (** Express that some variable has some initial value at the
      given memory state. The first predicate states the value,
      the second, the initialization status.

      Note: we DO NOT merge values and initialization status
      hypotheses as the factorization performed by Qed can make
      predicates too hard to simplify later.

      Remark: [None] initializer are interpreted as zeroes. This is consistent
      with the [init option] associated with global variables in CIL,
      for which the default initializer are zeroes. This function is called
      for global initializers and local initializers ([Cil.Local_init]).
      It is not called for local variables without initializers as they do not
      have a [Cil.init option].
  *)

end

(** Compiler for ACSL expressions *)
module type LogicSemantics =
sig

  module M : Model (** Underlying memory model *)

  type loc = M.loc
  type nonrec value  = M.loc value
  type nonrec logic  = M.loc logic
  type nonrec region = M.loc region
  type nonrec result = M.loc result

  (** {2 Frames}

      Frames are compilation environment for ACSL. A frame typically
      manages the current function, formal parameters, the memory environments
      at different labels and the [\result] and [\exit_status] values.

      The frame also holds the {i gamma} environment responsible for
      accumulating typing constraints, and the {i pool} for generating
      fresh logic variables.

      Notice that a [frame] is not responsible for holding the environment
      at label [Here], since this is managed by a specific compilation
      environment, see {!env} below.
  *)

  type frame
  val pp_frame : Format.formatter -> frame -> unit

  (** Get the current frame, or raise a fatal error if none. *)
  val get_frame : unit -> frame

  (** Execute the given closure with the specified current frame.
      The [Lang.gamma] and [Lang.pool] contexts are also set accordingly. *)
  val in_frame : frame -> ('a -> 'b) -> 'a -> 'b

  (** Get the memory environment at the given label.
      A fresh environment is created lazily if required.
      The label must {i not} be [Here]. *)
  val mem_at_frame : frame -> Clabels.c_label -> sigma

  (** Update a frame with a specific environment for the given label. *)
  val set_at_frame : frame -> Clabels.c_label -> sigma -> unit

  (** Check if a frame already has a specific envioronement for the given label. *)
  val has_at_frame : frame -> Clabels.c_label -> bool

  (** Same as [mem_at_frame] but for the current frame. *)
  val mem_frame : Clabels.c_label -> sigma

  (** Full featured constructor for frames, with fresh pool and gamma. *)
  val mk_frame :
    ?kf:Cil_types.kernel_function ->
    ?result:result ->
    ?status:Lang.F.var ->
    ?formals:value Cil_datatype.Varinfo.Map.t ->
    ?labels:sigma Clabels.LabelMap.t ->
    ?descr:string ->
    unit -> frame

  (** Make a local frame reusing the {i current} pool and gamma. *)
  val local : descr:string -> frame

  (** Make a fresh frame with the given function. *)
  val frame : kernel_function -> frame

  type call (** Internal call data. *)

  (** Create call data from the callee point of view,
      deriving data (gamma and pools) from the current frame.
      If [result] is specified, the called function will stored its result
      at the provided location in the current frame (the callee). *)
  val call : ?result:M.loc -> kernel_function -> value list -> call

  (** Derive a frame from the call data suitable for compiling the
      called function contracts in the provided pre-state. *)
  val call_pre   : sigma -> call -> sigma -> frame

  (** Derive a frame from the call data suitable for compiling the
      called function contracts in the provided pre-state and post-state. *)
  val call_post  : sigma -> call -> sigma sequence -> frame

  (** Result type of the current function in the current frame. *)
  val return : unit -> typ

  (** Result location of the current function in the current frame. *)
  val result : unit -> result

  (** Exit status for the current frame. *)
  val status : unit -> var

  (** Returns the current gamma environment from the current frame. *)
  val guards : frame -> pred list

  (** {2 Compilation Environment} *)

  type env
  (**
     Compilation environment for terms and predicates. Manages
     the {i current} memory state and the memory state at [Here].

     Remark: don't confuse the {i current} memory state with the
     memory state {i at label} [Here]. The current memory state is the one
     we have at hand when compiling a term or a predicate. Hence, inside
     [\at(e,L)] the current memory state when compiling [e] is the one at [L].
  *)

  (** Create a new environment.

      Current and [Here] memory points are initialized to [~here], if
      provided.

      The logic variables stand for
      formal parameters of ACSL logic function and ACSL predicates. *)
  val mk_env :
    ?here:sigma ->
    ?lvars:logic_var list ->
    unit -> env

  (** The {i current} memory state. Must be properly initialized
      with a specific {!move} before. *)
  val current : env -> sigma

  (** Move the compilation environment to the specified [Here] memory state.
      This memory state becomes also the new {i current} one. *)
  val move_at : env -> sigma -> env

  (** Returns the memory state at the requested label.
      Uses the local environment for [Here] and the current frame
      otherwise. *)
  val mem_at : env -> Clabels.c_label -> sigma

  (** Returns a new environment where the current memory state is
      moved to to the corresponding label. Suitable for compiling [e] inside
      [\at(e,L)] ACSL construct. *)
  val env_at : env -> Clabels.c_label -> env

  (** {2 Compilers} *)

  (** Compile a term l-value into a (typed) abstract location *)
  val lval : env -> Cil_types.term_lval -> Cil_types.typ * M.loc

  (** Compile a term expression. *)
  val term : env -> Cil_types.term -> term

  (** Compile a predicate. The polarity is used to generate a weaker or
      stronger predicate in case of unsupported feature from WP or the
      underlying memory model. *)
  val pred : polarity -> env -> Cil_types.predicate -> pred

  (** Compile a predicate call. *)
  val call_pred:
    env -> Cil_types.logic_info -> Cil_types.logic_label list -> term list ->
    pred

  (** Compile a term representing a set of memory locations into an abstract
      region.  *)
  val region : env -> Cil_types.term -> region

  (** Computes the region assigned by a list of froms. *)
  val assigned_of_lval :
    env -> Cil_types.lval -> region

  (** Computes the region assigned by a list of froms. *)
  val assigned_of_froms :
    env -> from list -> region

  (** Computes the region assigned by an assigns clause.
      [None] means everything is assigned. *)
  val assigned_of_assigns :
    env -> assigns -> region option

  (** Same as [term] above but reject any set of locations. *)
  val val_of_term : env -> Cil_types.term -> term

  (** Same as [term] above but expects a single loc or a single
      pointer value. *)
  val loc_of_term : env -> Cil_types.term -> loc

  (** Compile a lemma definition. *)
  val lemma : LogicUsage.logic_lemma -> Definitions.dlemma

  (** {2 Regions} *)

  (** Qed variables appearing in a region expression. *)
  val vars : region -> Vars.t

  (** Member of vars. *)
  val occurs : var -> region -> bool

  (** Check assigns inclusion.
      Compute a formula that checks whether written locations are either
      invalid (at the given memory location) or included in some assignable
      region. When [~unfold:n && n <> 0], compound memory locations are expanded
      field-by-field and arrays, cell-by-cell (by quantification). Up to [n]
      levels are unfolded, -1 means unlimited. *)
  val check_assigns :
    unfold:int -> sigma -> written:region -> assignable:region -> pred

end

(** Compiler for Performing Assigns *)
module type LogicAssigns = sig

  module M : Model
  module L : LogicSemantics with module M = M
  open M

  (** Memory footprint of a region. *)
  val domain : loc region -> Heap.set

  (** Relates two memory states corresponding to an assigns clause
      with the specified set of locations. *)
  val apply_assigns : sigma sequence -> loc region -> pred list

end

(** All Compilers Together *)
module type Compiler = sig
  module M : Model
  module C : CodeSemantics with module M = M
  module L : LogicSemantics with module M = M
  module A : LogicAssigns with module M = M and module L = L
end
