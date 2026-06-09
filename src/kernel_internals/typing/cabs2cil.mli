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

(** Registers a new hook that will be applied each time a side-effect free
    expression whose result is unused is dropped. The string is the name
    of the current function.
*)
val register_ignore_pure_exp_hook: (string -> Cil_types.exp -> unit) -> unit

(** new hook called when an implicit prototype is generated.
    @since Carbon-20101201
*)
val register_implicit_prototype_hook: (Cil_types.varinfo -> unit) -> unit

(** new hook called when a definition has a compatible but not
    strictly identical prototype than its declaration
    The hook takes as argument the old and new varinfo. Note that only the
    old varinfo is kept in the AST, and that its type will be modified in
    place just after to reflect the merge of the prototypes.
    @since Carbon-20101201
*)
val register_different_decl_hook:
  (Cil_types.varinfo -> Cil_types.varinfo -> unit) -> unit

val register_new_global_hook: (Cil_types.varinfo -> bool -> unit) -> unit
(** Hook called when a new global is created. The varinfo [vi] is the one
    corresponding to the global, while the boolean is [true] iff [vi] was
    already existing (it is [false] if this is the first declaration/definition
    of [vi] in the file).
    @since Silicon-20161101
*)

(** new hook called when encountering a definition of a local function. The hook
    take as argument the varinfo of the local function.
    @since Carbon-20101201
*)
val register_local_func_hook: (Cil_types.varinfo -> unit) -> unit

(** new hook called when side-effects are dropped.
    The first argument is the original expression, the second one
    the (side-effect free) normalized expression.
*)
val register_ignore_side_effect_hook:
  (Cabs.expression -> Cil_types.exp -> unit) -> unit

(** new hook called when an expression with side-effect is evaluated
    conditionally (RHS of && or ||, 2nd and 3rd term of ?:). Note that in case
    of nested conditionals, only the innermost expression with side-effects
    will trigger the hook (for instance, in [(x && (y||z++))],
    we have a warning on [z++], not on [y||z++], and similarly, on
    [(x && (y++||z))], we only have a warning on [y++]).
    - First expression is the englobing expression
    - Second expression is the expression with side effects.
*)
val register_conditional_side_effect_hook:
  (Cabs.expression -> Cabs.expression -> unit) -> unit

(** new hook that will be called when processing a for loop.
    Arguments are the four elements of the for clause
    (init, test, increment, body)
    @since Oxygen-20120901
*)
val register_for_loop_all_hook:
  (Cabs.for_clause ->
   Cabs.expression -> Cabs.expression -> Cabs.statement -> unit) -> unit

(** new hook that will be called when processing a for loop. Argument is
    the initializer of the for loop.
    @since Oxygen-20120901
*)
val register_for_loop_init_hook: (Cabs.for_clause -> unit) -> unit

(** new hook that will be called when processing a for loop. Argument is
    the test of the loop.
    @since Oxygen-20120901
*)
val register_for_loop_test_hook: (Cabs.expression -> unit) -> unit

(** new hook that will called when processing a for loop. Argument is the
    body of the loop.
    @since Oxygen-20120901
*)
val register_for_loop_body_hook: (Cabs.statement -> unit) -> unit

(** new hook that will be called when processing a for loop. Argument is
    the increment part of the loop.
    @since Oxygen-20120901
*)
val register_for_loop_incr_hook: (Cabs.expression -> unit) -> unit

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
val convFile: Cabs.file -> Cil_types.file

(** A hook into the code that creates temporary local vars.  By default this
    is the identity function, but you can overwrite it if you need to change the
    types of cabs2cil-introduced temp variables. *)
val typeForInsertedVar: (Cil_types.typ -> Cil_types.typ) ref

(** [fresh_global prefix] creates a variable name not clashing with any other
    globals and starting with [prefix] *)
val fresh_global : string -> string

val anonCompFieldName : string

val find_field_offset:
  (Cil_types.fieldinfo -> bool) -> Cil_types.fieldinfo list -> Cil_types.offset
(** returns the offset (can be more than one field in case of unnamed members)
    corresponding to the first field matching the condition.
    @raise Not_found if no such field exists.
*)

(** returns the type of the result of a logic operator applied to values of
    the corresponding input types.
*)
val logicConditionalConversion: Cil_types.typ -> Cil_types.typ -> Cil_types.typ

(** local information needed to typecheck expressions and statements *)
type local_env = private
  { authorized_reads: Cil_datatype.Lval.Set.t;
    (** sets of lvalues that can be read regardless of a potential
        write access between sequence points. Mainly for tmp variables
        introduced by the normalization.
    *)
    known_behaviors: string list;
    (** list of known behaviors at current point. *)
    is_ghost: bool;
    (** whether we're analyzing ghost code or not *)
    is_paren: bool;
    (** is the current expr a child of A.PAREN *)
    inner_paren: bool;
    (** used internally for normalizations of unop and binop. *)
  }

(** an empty local environment. *)
val empty_local_env: local_env

(** same as [empty_local_env], but sets the ghost status to the value of its
    argument
*)
val ghost_local_env: bool -> local_env

(** Applies [mkAddrOf] after marking variable whose address is taken. *)
val mkAddrOfAndMark : Cil_types.location -> Cil_types.lval -> Cil_types.exp

(** Raise Failure *)
val integral_cast: Cil_types.typ -> Cil_types.term -> Cil_types.term

(** Given a call [lv = f()], if [tf] is the return type of [f] and [tlv]
    the type of [lv], [allow_return_collapse ~tlv ~tf] returns false
    if a temporary must be introduced to hold the result of [f], and
    true otherwise.

    Currently, implicit cast between pointers or cast from an scalar type
    or a strictly bigger one are accepted without cast. This is subject
    to change without notice.

    @since Oxygen-20120901
*)
val allow_return_collapse: tlv:Cil_types.typ -> tf:Cil_types.typ -> bool

val stmtFallsThrough: Cil_types.stmt -> bool
(** returns [true] if the given statement can fall through the next
    syntactical one.

    @since Phosphorus-20170501-beta1 exported
*)

(**/**)

val fieldsToInit: Cil_types.compinfo -> string option -> Cil_types.offset list

(**
   Returns a mapping (pos_start, pos_end, funcname) for each function
   declaration and definition, spanning the entire function, including
   its specification (if it has one).

   Currently, for function declarations, the location ends at the function
   name, before the formal parameter list. For definitions, it spans until the
   closing brace.

   Note that the list is _not_ sorted, and must be further processed
   for efficient data retrieval.

   @since 25.0-Manganese
*)
val func_locs : unit -> (Filepos.t * Filepos.t * string) list

(** Deprecated  *)

(** Check that [s] starts with the prefix [p]. *)
val prefix : string -> string -> bool
[@@deprecated "Use String.starts_with instead."]
[@@migrate { repl = (fun prefix s -> String.starts_with ~prefix s) } ]
