(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Projects management.

    A project groups together all the internal states of Frama-C. An internal
    state is roughly the result of a computation which depends of an AST. It is
    possible to have many projects at the same time. For registering a new
    state in the Frama-C projects, apply the functor {!State_builder.Register}.

    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************* *)
(** {2 Types for project} *)
(* ************************************************************************* *)

include Datatype.S_no_copy with type t = Project_skeleton.t
module Datatype: Datatype.S_with_collections with type t = Project_skeleton.t

(* re-exporting record fields *)
type project = Project_skeleton.t =
  private
  { pid : int;
    mutable name : string }

(** Type of a project. *)


(* ************************************************************************* *)
(** {2 Initialization} *)
(* ************************************************************************* *)

(** Initialize the project library. This function MUST be called at least once
    during program initialization. The [seed] must be unique to a given
    program (version included). [source] will be used for messages output with
    the format ["[source:project] ..."].
    @since 33.0-Arsenic
*)
val init: seed:string -> source:string -> unit

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

(** If set to [true], prints debug information about projects.
    @since 33.0-Arsenic
*)
val set_debug: bool -> unit

(** If set to [true], prints feedbacks about projects.
    @since 33.0-Arsenic
*)
val set_feedback: bool -> unit

(** Decide how warnings should be handled:
    - <= 0 -> ignored
    - 1 -> feedbacks
    - 2 -> warnings (default)
    - >= 3 -> errors ([Failure])

    @since 33.0-Arsenic
*)
val set_warn_level: int -> unit

val compress_saved_session: bool ref
(** This is used to decide if projects should be compressed when saved with
    {!save} and {!save_all} without the [?compress] parameter. Defaults to
    [true].
    @since 33.0-Arsenic *)

val set_keep_current: bool -> unit
(** [set_keep_current b] keeps the current project forever (even after the end
    of the current {!on}) iff [b] is [true].
    @since Aluminium-20160501 *)

(* ************************************************************************* *)
(** {2 Operations on all projects} *)
(* ************************************************************************* *)

val create: string -> t
(** Create a new project with the given name and attach it after the existing
    projects (so the current project, if existing, is unchanged).
    The given name may be already used by another project.
    If there is no other project, then the new one is the current one. *)

val register_create_hook: (t -> unit) -> unit
(** [register_create_hook f] adds a hook on function [create]: each time a
    new project [p] is created, [f p] is applied.

    The order in which hooks are applied is the same than the order in which
    hooks are registered. *)

exception NoProject
(** May be raised by [current]. *)

val current: unit -> t
(** The current project.
    @raise NoProject if there is no project.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val is_current: t -> bool
(** Check whether the given project is the current one or not. *)

val last_project_created_by_copy: unit -> int option
(** @since 33.0-Arsenic *)

val iter_on_projects: (t -> unit) -> unit
(** iteration on project starting with the current one. *)

val fold_on_projects: ('a -> t -> 'a) -> 'a -> 'a
(** folding on project starting with the current one.
    @since Boron-20100401 *)

val find_all: string -> t list
(** Find all projects with the given name. The list is ordered from most
    recently used (i.e. used as current) to least recently used.
    {!Project.pick_most_recently_created} can be used to extract the most
    recently created project from that list. *)

val pick_most_recently_created: t list -> t
(** @return the project most recently created from the given list of projects.
    @raise Failure if the list is empty. *)

val clear_all: unit -> unit
(** Clear all the projects: all the internal states of all the projects are
    now empty (wrt the action registered with
    {!register_todo_after_global_clear} and {!register_todo_after_clear}. *)

(* ************************************************************************* *)
(** {2 Operations on one project}

    Most operations have one additional selection as argument. If it
    is specified, the operation is only applied on the states of the
    given selection on the given project. Beware that the project may
    become inconsistent if your selection is incorrect. *)
(* ************************************************************************* *)

val get_pid: t -> int
(** Project ID. Recommended way of identifying a project.
    @since 32.0-Germanium *)

val get_name: t -> string
(** Project name. Two projects may have the same name. *)

val get_debug_name: t -> string
(** @return a project name appended with its id.
    @since 32.0-Germanium *)

val get_current_pid: unit -> int
(** The current project {!pid}.
    @raise NoProject if there is no project.
    @since 33.0-Arsenic *)

val pid_to_name: int -> string
(** Return a project name based from its {!pid}.
    @raise Unknown_project if no project has this unique pid.
    @since 33.0-Arsenic *)

val name_to_pid: string -> int option
(** Return the project {!pid} based on its name. If several projects are found
    with the same name, the most recent one will be picked.
    @since 33.0-Arsenic *)

val set_name: t -> string -> unit
(** Set the name of the given project.
    @since Boron-20100401 *)

val register_after_set_name_hook: (t * string -> unit) -> unit
(** [register_after_set_name_hook f] adds a hook on function {!set_name}.
    The project given as argument to [f] is the modified project, while the
    string is the old name for this project. *)

exception Unknown_project

val from_pid: int -> t
(** Return a project based on {!pid}.
    @raise Unknown_project if no project has this unique pid.
    @since 32.0-Germanium *)

val set_current: ?on:bool -> ?selection:State_selection.t -> t -> unit
(** Set the current project with the given one.
    The flag [on] is not for casual users.
    @raise Invalid_argument if the given project does not exist anymore.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val register_after_set_current_hook: user_only:bool -> (t -> unit) -> unit
(** [register_after_set_current_hook f] adds a hook on function
    {!set_current}. The project given as argument to [f] is the old current
    project.
    - If [user_only] is [true], then each time {!set_current} is directly
      called by an user of this library, [f ()] is applied.
    - If [user_only] is [false], then each time {!set_current} is applied
      (even indirectly through {!Project.on}), [f ()] is applied.
      The order in which each hook is applied is unspecified. *)

val on: ?selection:State_selection.t -> t -> ('a -> 'b) -> 'a -> 'b
(** [on p f x] sets the current project to [p], computes [f x] then
    restores the current project. You should use this function if you use a
    project different of [current ()].
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val on_from_pid: ?selection:State_selection.t -> int -> ('a -> 'b) -> 'a -> 'b
(** Same than {!on} but find the project using its {!pid}.
    @raise Unknown_project if no project with this [pid] is found
    @since 33.0-Arsenic *)

(**/**)
val set_current_as_last_created: unit -> unit
(**/**)

val copy: ?selection:State_selection.t -> ?src:t -> t -> unit
(** Copy a project into another one. Default project for [src] is [current
    ()]. Replace the destination by [src].
    For each state to copy, the function [copy] given at state registration
    time must be fully implemented.
*)

val create_by_copy:
  ?selection:State_selection.t -> ?src:t -> last:bool -> string -> t
(** Return a new project with the given name by copying some states from the
    project [src]. All the other states are initialized with their default
    values.
    Use the save/load mechanism for copying. Thus it does not require that
    the copy function of the copied state is implemented. All the hooks
    applied when loading a project are applied (see {!load}).
    If [last], then remember that the returned project is the last created
    one.
    @raise Failure If for some reasons we cannot create a temporary file in
    '/tmp' (lack of disk space, permissions, etc).
*)

val create_by_copy_hook: (t -> t -> unit) -> unit
(** Register a hook to call at the end of {!create_by_copy}. The first
    argument of the registered function is the copy source while the
    second one is the created project. *)

val clear: ?selection:State_selection.t -> ?project:t -> unit -> unit
(** Clear the given project. Default project is [current ()]. All the
    internal states of the given project are now empty (wrt the action
    registered with {!register_todo_before_clear}).
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val register_todo_before_clear: (t -> unit) -> unit
(** Register an action performed just before clearing a project.
    @since Boron-20100401 *)

val register_todo_after_clear: (t -> unit) -> unit
(** Register an action performed just after clearing a project.
    @since Boron-20100401 *)

exception Cannot_remove of string
(** Raised by [remove] *)

val remove: ?project:t -> unit -> unit
(** Default project is [current ()]. If the current project is removed, then
    the new current project is the previous current project if it still
    exists (and so on).
    @raise Cannot_remove if there is only one project. *)

val register_before_remove_hook: (t -> unit) -> unit
(** [register_before_remove_hook f] adds a hook called just before removing
    a project.
    @since Beryllium-20090902 *)

(* ************************************************************************* *)
(** {3 Inputs/Outputs} *)
(* ************************************************************************* *)

exception IOError of string
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val save:
  ?compress:bool -> ?selection:State_selection.t -> ?project:t ->
  Filepath.t -> unit
(** Save a given project in a file. Default project is [current ()]. [?compress]
    defaults to {!compress_saved_session}.
    @raise IOError if the project cannot be saved.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val load: ?selection:State_selection.t -> ?name:string -> Filepath.t -> t
(** Load a file into a new project given by its name.
    More precisely, [load only except name file]:
    {ol
    {- creates a new project;}
    {- performs all the registered [before_load] actions;}
    {- loads the (specified) states of the project according to its
    description; and}
    {- performs all the registered [after_load] actions.}
    }
    @raise IOError if the project cannot be loaded
    @return the new project containing the loaded data.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val save_all:
  ?compress:bool -> ?selection:State_selection.t -> Filepath.t -> unit
(** Save all the projects in a file. [?compress] defaults to
    {!compress_saved_session}.
    @raise IOError a project cannot be saved. *)

val load_all: ?selection:State_selection.t -> Filepath.t -> unit
(** First remove all the existing project, then load all the projects from a
    file. For each project to load, the specification is the same than
    {!Project.load}. Furthermore, after loading, all the hooks registered by
    [register_after_set_current_hook] are applied.
    @raise IOError if a project cannot be loaded. *)

val register_before_load_hook: (Project_skeleton.t -> unit) -> unit
(** [register_before_load_hook f] adds a hook called just before loading
    **each project** (more precisely, the project exists and but is empty
    while the hook is applied): if [n] projects are on disk, the same hook
    will be called [n] times (one call by project).

    Besides, for each project, the order in which the hooks are applied is
    the same than the order in which hooks are registered. *)

val register_after_load_hook: (Project_skeleton.t -> unit) -> unit
(** [register_after_load_hook f] adds a hook called just after loading
    **each project**: if [n] projects are on disk, the same hook will be
    called [n] times (one call by project).

    Besides, for each project, the order in which the hooks are applied is
    the same than the order in which hooks are registered. *)

val register_after_global_load_hook: (unit -> unit) -> unit
(** [register_after_load_hook f] adds a hook called just after loading
    **all projects**. [f] must not set the current project.
    @since Boron-20100401 *)

(* ************************************************************************* *)
(** {3 Handling the selection} *)
(* ************************************************************************* *)

val get_current_selection: unit -> State_selection.t
(** If an operation on a project is ongoing, then [get_current_selection ()]
    returns the selection which is applied on.
    The behaviour is unspecified if this function is called when no operation
    depending on a selection is ongoing. *)

(* ************************************************************************* *)
(** {2 Projects are comparable values} *)
(* ************************************************************************* *)

val compare: t -> t -> int
val equal: t -> t -> bool
val hash: t -> int
