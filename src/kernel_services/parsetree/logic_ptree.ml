(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(* Logic parse trees *)

type location = Cil_types.location
[@@deriving show { with_path = false } ]

(** comparison operators. *)
type relation = Lt | Gt | Le | Ge | Eq | Neq
[@@deriving show { with_path = false } ]

(** arithmetic and logic binary operators. *)
type binop = Badd | Bsub | Bmul | Bdiv | Bmod | Bbw_and | Bbw_or | Bbw_xor |
             Blshift | Brshift
[@@deriving show { with_path = false } ]

(** unary operators. *)
type unop = Uminus | Ustar | Uamp | Ubw_not
[@@deriving show { with_path = false } ]

(** logic constants. *)
type constant =
    IntConstant of string (** integer constant *)
  | FloatConstant of string (** real constant *)
  | StringConstant of string (** string constant *)
  | WStringConstant of string (** wide string constant *)
[@@deriving show { with_path = false } ]

(** size of logic array.
    @before 31.0-Gallium was a sum type with only integer constants and variables
*)
type array_size = lexpr option
(* This attribute is common for all types in the mutually recursive group of
   types so we only need to mention it once. *)
[@@deriving show { with_path = false } ]

(** logic types. *)
and logic_type =
  | LTvoid (** C void *)
  | LTboolean (** booleans *)
  | LTinteger (** mathematical integers. *)
  | LTreal (** mathematical real. *)
  | LTint of Cil_types.ikind (** C integral type.*)
  | LTfloat of Cil_types.fkind (** C floating-point type *)
  | LTarray of logic_type * array_size (** C array *)
  | LTpointer of logic_type (** C pointer *)
  | LTenum of string (** C enum *)
  | LTstruct of string (** C struct *)
  | LTunion of string (** C union *)
  | LTnamed of string * logic_type list (** declared logic type. *)
  | LTarrow of logic_type list * logic_type
  | LTattribute of logic_type * Cil_types.attribute (* Only const and volatile can appear here *)

(** quantifier-bound variables *)
and quantifiers = (logic_type * string) list

(** logical expression. The distinction between locations, terms and
    predicate is done during typing.
*)
and lexpr = {
  lexpr_node : lexpr_node; (** kind of expression. *)
  lexpr_loc : location (** position in the source code. *)
}

(* PL is for Parsed Logic *)
(** construct inside a functional update. *)
and path_elt =
  | PLpathField of string
  | PLpathIndex of lexpr

and update_term =
  | PLupdateTerm of lexpr
  | PLupdateCont of ((path_elt list) * update_term) list

(** Kind of expression *)
and lexpr_node =
  (* both terms and predicates *)
  | PLvar of string (** a variable *)
  | PLapp of string * string list * lexpr list (** an application. *)
  (* terms *)
  | PLlambda of quantifiers * lexpr (** a lambda abstraction. *)
  | PLlet of string * lexpr * lexpr (** local binding. *)
  | PLconstant of constant (** a constant. *)
  | PLunop of unop * lexpr (** unary operator. *)
  | PLbinop of lexpr * binop * lexpr (** binary operator. *)
  | PLdot of lexpr * string (** field access ([a.x]) *)
  | PLarrow of lexpr * string (** field access ([a->x])*)
  | PLarrget of lexpr * lexpr (** array access. *)
  | PLold of lexpr (** expression refers to pre-state of a function. *)
  | PLat of lexpr * string (** expression refers to a given program point. *)
  | PLresult (** value returned by a function. *)
  | PLnull (** null pointer. *)
  | PLcast of logic_type * lexpr (** cast. *)
  | PLrange of lexpr option * lexpr option (** interval of integers. *)
  | PLalignof of logic_type (** alignof a type. *)
  | PLsizeof of logic_type (** sizeof a type. *)
  | PLsizeofE of lexpr (** sizeof the type of an expression. *)
  | PLupdate of lexpr * (path_elt list) * update_term
  (** functional update of the field of a structure. *)
  | PLinitIndex of (lexpr * lexpr) list (** array constructor. *)
  | PLinitField of (string * lexpr) list (** struct/union constructor. *)
  | PLtypeof of lexpr (** type tag for an expression. *)
  | PLtype of logic_type (** type tag for a C type. *)
  (* predicates *)
  | PLfalse (** false (either as a term or a predicate. *)
  | PLtrue (** true (either as a term or a predicate. *)
  | PLrel of lexpr * relation * lexpr (** comparison operator. *)
  | PLand of lexpr * lexpr (** conjunction. *)
  | PLor of lexpr * lexpr (** disjunction. *)
  | PLxor of lexpr * lexpr (** logical xor. *)
  | PLimplies of lexpr * lexpr (** implication. *)
  | PLiff of lexpr * lexpr (** equivalence. *)
  | PLnot of lexpr (** negation. *)
  | PLif of lexpr * lexpr * lexpr (** conditional operator. *)
  | PLforall of quantifiers * lexpr (** universal quantification. *)
  | PLexists of quantifiers * lexpr (** existential quantification. *)
  | PLbase_addr of string option * lexpr (** base address of a pointer. *)
  | PLoffset of string option * lexpr (** base address of a pointer. *)
  | PLblock_length of string option * lexpr (** length of the block pointed to by an
                                                expression. *)
  | PLvalid of string option * lexpr (** pointer is valid. *)
  | PLvalid_read of string option * lexpr (** pointer is valid for reading. *)
  | PLobject_pointer of string option * lexpr (** object pointer can be created. *)
  | PLvalid_function of lexpr (** function pointer is compatible with pointed type. *)
  | PLallocable of string option * lexpr (** pointer is valid for malloc. *)
  | PLfreeable of string option * lexpr (** pointer is valid for free. *)
  | PLinitialized of string option * lexpr (** pointer is guaranteed to be
                                               initialized *)
  | PLdangling of string option * lexpr (** pointer is guaranteed to be
                                            dangling  *)
  | PLaligned of lexpr * lexpr (** \aligned(p, v) p is aligned mod v *)
  | PLfresh of (string * string) option * lexpr * lexpr (** expression points to a newly allocated block. *)
  | PLseparated of lexpr list
  (** separation predicate. *)
  | PLnamed of string * lexpr (** named expression. *)
  (* tsets *)
  | PLcomprehension of lexpr * quantifiers * lexpr option
  (** set of expression defined in comprehension
      ([{ e | integer i; P(i)}])*)
  | PLset of lexpr list
  (** sets of elements. *)
  | PLunion of lexpr list
  (** union of sets. *)
  | PLinter of lexpr list
  (** intersection of sets. *)
  | PLempty
  (** empty set. *)
  | PLlist of lexpr list
  (** list of elements. *)
  | PLrepeat of lexpr * lexpr
  (** repeat a list of elements a number of times. *)

type toplevel_predicate =
  { tp_kind: Cil_types.predicate_kind; tp_statement: lexpr }
[@@deriving show { with_path = false } ]

(** ACSL extension.
    @before 30.0-Zinc Was of type [string * lexpr list].
*)
type extension = {
  ext_name: string;
  ext_plugin: string;
  ext_content: lexpr list;
} [@@deriving show { with_path = false } ]

(** type invariant. *)
type type_annot =  {
  inv_name: string;
  this_type : logic_type;
  this_name: string; (** name of its argument. *)
  inv: lexpr
} [@@deriving show { with_path = false } ]

(** model field. *)
type model_annot =  {
  model_for_type: logic_type;
  model_type : logic_type;
  model_name: string; (** name of the model field. *)
} [@@deriving show { with_path = false } ]

(** Concrete type definition. *)
type typedef =
  | TDsum of (string * logic_type list) list
  (** sum type, list of constructors *)
  | TDsyn of logic_type (** synonym of an existing type *)
[@@deriving show { with_path = false } ]

type loader = {
  loader_name: string;
  loader_plugin: string;
} [@@deriving show { with_path = false } ]
(** loader type used by module importers.
    @since 30.0-Zinc
*)

(** global declarations. *)
type decl = {
  decl_node : decl_node; (** kind of declaration. *)
  decl_loc : location (** position in the source code. *)
}
(* This attribute is common for all types in the mutually recursive group of
   types so we only need to mention it once. *)
[@@deriving show { with_path = false } ]

and decl_node =
  | LDlogic_def of
      string * string list * string list *
      logic_type * (logic_type * string) list * lexpr
  (** [LDlogic_def(name,labels,type_params,
                   return_type, parameters, definition)]
      represents the definition of a logic function [name] whose
      return type is [return_type] and arguments are [parameters].
      Its label arguments are [labels]. Polymorphic functions have
      their type parameters in [type_params]. [definition] is the
      body of the defined function.*)
  | LDlogic_reads of
      string * string list * string list *
      logic_type * (logic_type * string) list * lexpr list option
  (** [LDlogic_reads(name,labels,type_params,
      return_type, parameters, reads_tsets)] represents the declaration
      of logic function.  It has the same
      arguments as [LDlogic_def], except that the definition is
      abstracted to a set of read accesses in [read_tsets].
  *)
  | LDtype of string * string list * typedef option
  (** new logic type and its parameters, optionally followed by
      its definition. *)
  | LDpredicate_reads of
      string * string list * string list *
      (logic_type * string) list * lexpr list option
  (** [LDpredicate_reads(name,labels,type_params,
                        parameters, reads_tsets)]
      represents the declaration of a new predicate. It is similar to
      [LDlogic_reads] except that it has no [return_type].
  *)
  | LDpredicate_def of string * string list * string list *
                       (logic_type * string) list * lexpr
  (** [LDpredicate_def(name,labels,type_params, parameters, def)]
      represents the definition of a new predicate. It is similar to
      [LDlogic_def] except that it has no [return_type].

  *)
  | LDinductive_def of string * string list * string list *
                       (logic_type * string) list * (string * string list * string list * lexpr) list
  (** [LDinductive_def(name,labels,type_params, parameters, indcases)]
      represents an inductive definition of a new predicate.
  *)
  | LDlemma of string * string list * string list * toplevel_predicate
  (** LDlemma(name,labels,type_params,property) represents axioms and
      lemmas. Axioms and admit lemmas are fusionned.
      [labels] is the list of label arguments and
      [type_params] the list of type parameters.
      Last, [property] is the statement of the lemma.
  *)
  | LDaxiomatic of string * decl list
  (** [LDaxiomatic(id,decls)]
      represents a block of axiomatic definitions.*)
  | LDmodule of string * decl list
  (** [LDmodule(id,decls)]
      represents a module of axiomatic definitions.*)
  | LDimport of {
      import_loader: loader option;
      module_name: string;
      module_alias: string option;
    }
  (** [LDimport(loader,module,alias)]
      imports symbols from module using the specified loader,
      with optional alias.*)
  | LDinvariant of string * lexpr (** global invariant. *)
  | LDtype_annot of type_annot    (** type invariant. *)
  | LDmodel_annot of model_annot    (** model field. *)
  | LDvolatile of lexpr list * (string option * string option)
  (** volatile clause read/write. *)
  | LDextended of global_extension (** extended global annotation. *)


(** dependencies of an assigned location. *)
and deps =
  | From of lexpr list (** tsets. Empty list means \nothing. *)
  | FromAny (** Nothing specified. Any location can be involved. *)

and from = (lexpr * deps)

(** zone assigned with its dependencies. *)
and assigns =
  | WritesAny (** Nothing specified. Anything can be written. *)
  | Writes of from list
  (** list of locations that can be written. Empty list means \nothing. *)

(** allocates and frees.
    @since Oxygen-20120901  *)
and allocation =
  | FreeAlloc of lexpr list * lexpr list (** tsets. Empty list means \nothing. *)
  | FreeAllocAny (** Nothing specified. Semantics depends on where it
                     is written. *)

(** variant of a loop or a recursive function. *)
and variant = lexpr * string option


(** Global ACSL extension. *)
and global_extension =
  | Ext_lexpr of extension
  (** @before 30.0-Zinc Was of type [string * lexpr list]. *)

  | Ext_extension of {
      gext_name:string;
      gext_plugin:string;
      gext_kind: string;
      gext_content: extended_decl list;
    }
  (** @before 30.0-Zinc Was of type [string * string * extended_decl list]. *)

and extended_decl = {
  extended_node : global_extension;
  extended_loc : location
}

(** Behavior in a specification. This type shares the name of its constructors
    with {!Cil_types.behavior}. *)
type behavior = {
  mutable b_name : string; (** name of the behavior. *)
  mutable b_requires : toplevel_predicate list; (** require clauses. *)
  mutable b_assumes : lexpr list; (** assume clauses. *)
  mutable b_post_cond :
    (Cil_types.termination_kind * toplevel_predicate) list;
  (** post-condition. *)
  mutable b_assigns : assigns; (** assignments. *)
  mutable b_allocation : allocation; (** frees, allocates. *)
  mutable b_extended : extension list (** extensions *)
} [@@deriving show { with_path = false } ]

(** Function or statement contract. This type shares the name of its
    constructors with {!type:Cil_types.spec}. *)
type spec = {
  mutable spec_behavior : behavior list;
  (** behaviors *)

  mutable spec_variant : variant option;
  (** variant for recursive functions. *)

  mutable spec_terminates: lexpr option;
  (** termination condition. *)

  mutable spec_complete_behaviors: string list list;
  (** list of complete behaviors.
      It is possible to have more than one set of complete behaviors *)

  mutable spec_disjoint_behaviors: string list list;
  (** list of disjoint behaviors.
      It is possible to have more than one set of disjoint behaviors *)
} [@@deriving show { with_path = false } ]

(** all annotations that can be found in the code. This type shares the name of
    its constructors with {!Cil_types.code_annotation_node}. *)
type code_annot =
  | AAssert of string list * toplevel_predicate
  (** assertion to be checked. The list of strings is the list of
      behaviors to which this assertion applies. *)

  | AStmtSpec of string list * spec
  (** statement contract
      (potentially restricted to some enclosing behaviors). *)

  | AInvariant of string list * bool * toplevel_predicate
  (** loop/code invariant. The list of strings is the list of behaviors to which
      this invariant applies.  The boolean flag is true for normal loop
      invariants and false for invariant-as-assertions. *)

  | AVariant of variant
  (** loop variant. Note that there can be at most one variant associated to a
      given statement *)

  | AAssigns of string list * assigns
  (** loop assigns.  (see [b_assigns] in the behaviors for other assigns).  At
      most one clause associated to a given (statement, behavior) couple.  *)

  | AAllocation of string list * allocation
  (** loop allocation clause.  (see [b_allocation] in the behaviors for other
      allocation clauses).
      At most one clause associated to a given (statement, behavior) couple.
      @since Oxygen-20120901 when [b_allocation] has been added.  *)

  | AExtended of string list * bool * extension
  (** extension in a code or loop (when boolean flag is true) annotation.
      @since Silicon-20161101
  *)
[@@deriving show { with_path = false } ]

(** all kind of annotations*)
type annot =
  | Adecl of decl list (** global annotation. *)
  | Aspec  (* the real spec is parsed afterwards.
              See cparser.mly (grammar rules involving SPEC) for
              more details.
           *) (** function specification. *)
  | Acode_annot of location * code_annot (** code annotation. *)
  | Aloop_annot of location * code_annot list (** loop annotation. *)
[@@deriving show { with_path = false } ]

(** ACSL extension for external spec file **)
type ext_decl =
  | Ext_decl of decl            (* decl contains a location *)
  | Ext_macro of bool * string * lexpr (* lexpr contains a location *)
  | Ext_include of bool * string * location
[@@deriving show { with_path = false } ]

type ext_function =
  | Ext_spec of spec * location (* function spec *)
  | Ext_stmt of string list * annot * location (* loop/code annotation. *)
  | Ext_glob of ext_decl
[@@deriving show { with_path = false } ]

type ext_module = string option * ext_decl list * ((string * location) option * ext_function list) list
[@@deriving show { with_path = false } ]

type ext_spec = ext_module list
[@@deriving show { with_path = false } ]
