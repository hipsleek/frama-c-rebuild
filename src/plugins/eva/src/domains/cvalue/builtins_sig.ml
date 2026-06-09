(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Interface of {!Builtins} exported in Eva.ml. *)
module type API = sig

  exception Invalid_nb_of_args of int
  exception Outside_builtin_possibilities

  (* Signature of a builtin: type of the result, and type of the arguments. *)
  type builtin_type = unit -> Eva_ast.typ * Eva_ast.typ list

  (** Can the results of a builtin be cached? See {!Eval} for more details.*)
  type cacheable = Eval.cacheable = Cacheable | NoCache | NoCacheCallers

  type full_result = {
    c_values: (Cvalue.V.t option * Cvalue.Model.t) list;
    (** A list of results, consisting of:
        - the value returned (ie. what is after the 'return' C keyword)
        - the memory state after the function has been executed. *)
    c_clobbered: Base.SetLattice.t;
    (** An over-approximation of the bases in which addresses of local variables
        might have been written *)
    c_assigns: (Assigns.t * Memory_zone.t) option;
    (** If not None:
        - the assigns of the function, i.e. the dependencies of the result
          and of each zone written to.
        - and its sure outputs, i.e. an under-approximation of written zones. *)
    cacheable: cacheable;
    (** Can this result of the function call be cached with memexec? *)
  }

  (** The result of a builtin can be given in different forms. *)
  type call_result =
    | States of Cvalue.Model.t list
    (** A disjunctive list of post-states at the end of the C function.
        Can only be used if these results can be cached and reused for other
        calls with the same entry state, and if the C function:
        - does not write the address of any local variables;
        - does not read other memory locations than the call arguments;
        - does not write other locations than the result. *)
    | Result of Cvalue.V.t list
    (** A disjunctive list of resulting values. The specification is used to
        compute the post-state, in which the result is replaced by the values
        computed by the builtin. Can only be used in the same condition than
        [States] above. *)
    | Full of full_result
    (** See [full_result] type. *)

  (** Type of a cvalue builtin, whose arguments are:
      - the memory state at the beginning of the function call;
      - the list of arguments of the function call. *)
  type builtin = Cvalue.Model.t -> (Eva_ast.exp * Cvalue.V.t) list -> call_result

  (** [register_builtin name ?replace ?typ f] registers the function [f]
      as a builtin to be used instead of the C function of name [name].
      If [replace] is provided, the builtin is also used instead of the C function
      of name [replace], unless option -eva-builtin-auto is disabled.
      If [typ] is provided, consistency between the expected [typ] and the type of
      the C function is checked before using the builtin. *)
  val register_builtin:
    string -> ?replace:string -> ?typ:builtin_type -> builtin -> unit

  (** [unregister_builtin name] unregister a builtin previously registered with
      {!register_builtin_name} with name [name]. If [replace] was provided,
      the replaced function must also be unregistered with another call. If the
      no builtin with this name was previously registered, this function should
      have no effect. *)
  val unregister_builtin: string -> unit

  (** Has a builtin been registered with the given name? *)
  val is_builtin: string -> bool
end
