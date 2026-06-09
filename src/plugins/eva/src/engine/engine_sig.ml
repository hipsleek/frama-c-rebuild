(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eva_ast_types
open Eval

(** Kind of function that is analyzed: a body, a specification, a builtin, an
    internal Frama_C_ (not builtin) function, or nothing. Note that if several
    functions may be called, preceding values have priority. *)
type call_kind = [ `Body | `Spec | `Builtin | `Internal | `Bottom ]

(** Results of the analysis of a function call:
    - the list of computed abstract states at the return statement of the called
      function, associated with their partition key;
    - whether the results can safely be stored in the memexec cache;
    - the kind of analyzed function. *)
type 'state call_result = {
  states: (Partition.key * 'state) list;
  cacheable: cacheable;
  kind: call_kind;
}


(** Helper module to register read and written memory zones to [Inout_access],
    built by [Transfer_inout.Make]. *)
module type Transfer_inout = sig
  type location
  type value
  type valuation

  (** [register_logc_assign pos clause location] registers to [Inout_access]
      the read and written memory zones at [pos] for the logic assign [clause]
      to the [location]. The memory accessed by the logic assign is returned. *)
  val register_logic_assign :
    Position.t -> location Eval.logic_assign -> location ->
    Inout_access.t

  (** [register_assign_lval pos valuation lval exp] registers to [Inout_access]
      the read and written memory zones at [pos] for the assignment from [exp]
      to [lval] with a given [valuation]. The memory accessed by the assignment
      is returned. *)
  val register_assign_lval :
    Position.t -> valuation ->
    Eva_ast.lval -> Eva_ast.exp ->
    Inout_access.t

  (** [register_assign_var pos valuation vi exp] registers to [Inout_access]
      the read and written memory zones at [pos] for the assignment from [exp]
      to [vi] with a given [valuation]. The memory accessed by the assignment is
      returned. *)
  val register_assign_var :
    Position.t -> valuation ->
    Eva_ast.varinfo -> Eva_ast.exp ->
    Inout_access.t

  (** [register_read_exp pos valuation exp] registers to [Inout_access] the
      read memory zones at [pos] for reading the expression [exp] with a given
      [valuation]. The memory accessed by the read is returned. *)
  val register_read_exp :
    Position.t -> valuation ->
    Eva_ast.exp ->
    Inout_access.t

  (** [register_call_args pos valuation call] registers to [Inout_access] the
      read and written memory zones at [pos] for the arguments of the given
      [call] with a given [valuation]. The memory accessed by the call arguments
      is returned. *)
  val register_call_args :
    Position.t -> valuation ->
    (location, value) Eval.call ->
    Inout_access.t
end


(** Interpretation of statements, built by functor [Transfer_stmt.Make]. *)
module type Transfer_stmt = sig
  type state

  val assign: pos:Position.t -> state -> lval -> exp -> state or_bottom

  val assume: pos:Position.t -> state -> exp -> bool -> state or_bottom

  val call:
    pos:Position.local ->
    lval option -> lhost -> exp list -> state -> state call_result

  val return : pos:Position.local -> exp option -> state -> state or_bottom

  val check_unspecified_sequence:
    pos:Position.t ->
    state ->
    (* TODO *)
    (stmt * lval list * lval list * lval list * stmt ref list) list ->
    unit or_bottom

  val enter_scope: Kernel_function.t -> varinfo list -> state -> state

  val leave_scope: Kernel_function.t -> varinfo list -> state -> state
end


(** Interpretation of logic assertions, built by functor [Transfer_logic.Make]. *)
module type Transfer_logic = sig
  type state

  val create: state -> kernel_function -> Active_behaviors.t
  val create_from_spec: state -> spec -> Active_behaviors.t

  val check_fct_preconditions_for_behaviors:
    kinstr -> kernel_function -> behavior list -> Alarmset.status ->
    state list -> state list

  val check_fct_preconditions:
    kinstr -> kernel_function -> Active_behaviors.t ->
    state -> state list

  val check_fct_postconditions_for_behaviors:
    kernel_function -> behavior list -> Alarmset.status ->
    pre_state:state -> result:varinfo option ->
    state list -> state list

  val check_fct_postconditions:
    kernel_function -> Active_behaviors.t -> termination_kind ->
    pre_state:state -> result:varinfo option ->
    state -> state list

  val evaluate_assumes_of_behavior: state -> behavior -> Alarmset.status

  val interp_annot:
    record:bool ->
    kernel_function -> Active_behaviors.t -> stmt -> code_annotation ->
    initial_state:state -> state list -> state list
end


(** Interpretation of function specification,
    built by functor [Transfer_specification.Make]. *)
module type Transfer_specification = sig
  type value
  type location
  type state

  val treat_statement_assigns: pos:Position.t -> assigns -> state -> state

  val compute_using_specification:
    warn:bool -> (location, value) call -> spec ->
    state -> (Partition.key * state) list
end


(** Initialization of variables, built by functor [Initialization.Make]. *)
module type Initialization = sig
  type state

  (** Compute the initial state for an analysis, but also bind the formal
      parameters of the function given as argument.
      @param cvalue_state if given, replace the computed initial cvalue state
      with this one.
      @param arguments if given, use these arguments values instead of
      generating ad hoc values. *)
  val initial_state_with_formals :
    ?cvalue_state: Cvalue.Model.t ->
    ?arguments: Cvalue.V.t list ->
    lib_entry:bool ->
    Cil_types.kernel_function -> state or_bottom

  (** Initializes a local variable in the current state. *)
  val initialize_local_variable:
    pos:Position.t -> varinfo -> init -> state -> state or_bottom
end


(** Analysis of a function body by iteration over its interpreted automata,
    built by the functor [Iterator.Make]. *)
module type Iterator = sig
  type state

  val compute:
    save_results:bool -> Callstack.t ->
    state -> (Partition.key * state) list * Eval.cacheable
end


(** Complete analysis of functions,
    built by the functor [Compute_functions.Make]. *)
module type Compute =
sig
  type state
  type loc
  type value

  (** Analysis of a program from the given main function and initial state.
      Returns the abstract state inferred at the return of the main function. *)
  val compute_main_call:
    thread:Thread.t -> kernel_function -> state -> state or_bottom

  (** Analysis of a function call during the Eva analysis. This function is
      called by [Transfer_stmt] when interpreting a call statement.
      [compute_call stmt call recursion state] analyzes the call [call] at
      statement [stmt] in the input abstract state [state].
      If [recursion] is not [None], the call is a recursive call. *)
  val compute_call:
    (loc, value) call -> recursion option -> state -> state call_result
end

module type Interferences =
sig
  type state

  type add_result =
    | Updated
    | NoChanges

  (** [reset ()] resets the current interferences state. Must be called
      between two analyses. *)
  val reset : unit -> unit

  (** Add the last Eva analysis results to the given interferences abstract
      representation. *)
  val add_last_analysis :
    Thread.t -> Position.Local.Set.t -> Base.Hptset.t -> add_result

  (** [inject_init_state th kf state] injects current interferences to the
      initial state of the analysis for thread [th] starting at the entry point
      [kf]. If enabled, the Mthread domain helps filtering applicable
      interferences. This function is the identity if the Mthread domain can
      infer that no other thread can interfere with the current thread. *)
  val inject_init_state : Thread.t -> kernel_function -> state -> state

  (** [inject_after_change th access state] injects current interferences to the
      given [state] of the analysis for thread [th] that has just been changed
      by a transfer function with the given [access]es. If enabled, the
      Mthread domain helps filtering applicable interferences. This function
      is the identity if the Mthread domain can infer that no shared memory
      has been read or written during the last transfer function. *)
  val inject_after_change : pos:Position.t ->  Inout_access.t -> state -> state
end


module type S = sig
  (** The four abstractions: values, locations, states and evaluation context,
      plus the evaluation engine for these abstractions. *)
  include Engine_abstractions_sig.S

  module Transfer_inout : Transfer_inout
    with type location = Loc.location
     and type value = Val.t
     and type valuation = Eval.Valuation.t

  module Transfer_stmt : Transfer_stmt with type state = Dom.t

  module Transfer_logic : Transfer_logic with type state = Dom.t

  module Transfer_specification : Transfer_specification
    with type state = Dom.t
     and type value = Val.t
     and type location = Loc.location

  module Initialization : Initialization with type state = Dom.t

  module Iterator : Iterator with type state = Dom.t

  module Compute : Compute
    with type state = Dom.t
     and type value = Val.t
     and type loc = Loc.location

  module Interferences : Interferences
    with type state = Dom.t
end

(** Access to analysis results, built by [Analysis] and used by [Results],
    which defines the final and complete API to access Eva results. *)
module type Results = sig
  type state
  type value
  type location

  (** {2 Access to abstract states inferred by the analysis} *)

  (** Returns the abstract state inferred at a control point:
      - for the given [callstack] if provided;
      - for any callstack otherwise, i.e. the join of states inferred for
        each possible callstacks. *)
  val get_state :
    ?callstack:Callstack.t -> Domain_store.control_point -> state or_top_bottom

  (** Returns the abstract state inferred at a given control point for each
      possible callstack analyzed. *)
  val get_state_by_callstack:
    Domain_store.control_point -> (Callstack.t * state) list or_top_bottom

  (** Returns all callstacks analyzed at a control point. *)
  val callstacks: Domain_store.control_point -> Callstack.t list or_top

  (** {2 Shortcuts for the evaluation in an abstract state} *)

  (** Evaluates the value of an expression in the given state. *)
  val eval_expr : state -> exp -> value evaluated

  (** Evaluates the value of an lvalue in the given state, with possible
      indeterminateness: non-initialization or escaping addresses. *)
  val copy_lvalue: state -> lval -> value flagged_value evaluated

  (** Evaluates the location of an lvalue in the given state, for a read
      access (invalid location for a read access are ignored). *)
  val eval_lval_to_loc: state -> lval -> location evaluated

  (** Evaluates the function argument of a [Call] constructor. *)
  val eval_function:
    state -> ?args:exp list -> lhost ->
    kernel_function list or_top_bottom * Alarmset.t

  (** [assume_cond ~pos state expr b] reduces the given abstract state
      by assuming [exp] evaluates to:
      - a non-zero value if [b] is true;
      - zero if [b] is false. *)
  val assume_cond : pos:Position.t -> state -> exp -> bool -> state or_bottom
end

module type S_with_results = sig
  include S
  include Results with type state := Dom.state
                   and type value := Val.t
                   and type location := Loc.location
end
