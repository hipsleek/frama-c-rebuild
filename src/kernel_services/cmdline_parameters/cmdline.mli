(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Command line parsing.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************** *)
(** {2 Stage configurations}
    (* ************************************************************************** *)

    Frama-C uses several stages for parsing its command line.
    Each of them may be customized. *)

type stage =
  | Early       (** Initial stage for very specific almost hard-coded
                    options. Do not use it.
                    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | Extending   (** Before loading plug-ins. Run only once.
                    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | Extended    (** The stage where plug-ins are loaded.
                    It is also the first stage each time the Frama-C main
                    loop is run (e.g. after each "-then").
                    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | Exiting     (** Run once when exiting Frama-C.
                    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | Loading     (** After {!Extended}, the stage where a previous Frama-C
                    internal states is restored (e.g. the one specified by
                    -load or by running the journal).
                    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  | Configuring (** The stage where all the parameters which were not already
                    set may be modified to take into account cmdline options.
                    Just after this stage, Frama-C will run the plug-in mains.
                    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
(** The different stages, from the first to be executed to the last one.
    @since Beryllium-20090601-beta1 *)

val run_after_early_stage: (unit -> unit) -> unit
(** Register an action to be executed at the end of the early stage.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Beryllium-20090901 *)

val run_during_extending_stage: (unit -> unit) -> unit
(** Register an action to be executed during the extending stage.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Beryllium-20090901 *)

val run_after_extended_stage: (unit -> unit) -> unit
(** Register an action to be executed at the end of the extended stage.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Beryllium-20090901 *)

type exit
(** @since Beryllium-20090901 *)

val nop : exit
(** @since Beryllium-20090901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

exception Exit
(** @since Beryllium-20090901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val run_after_exiting_stage: (unit -> exit) -> unit
(** Register an action to be executed at the end of the exiting stage.
    The guarded action must finish by [exit n].
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Beryllium-20090601-beta1 *)

val run_after_loading_stage: (unit -> unit) -> unit
(** Register an action to be executed at the end of the loading stage.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Beryllium-20090601-beta1 *)

val is_going_to_load: unit -> unit
(** To be call if one action is going to run after the loading stage.
    It is not necessary to call this function if the running action is set by
    an option put on the command line.
    @since Beryllium-20090601-beta1
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val run_after_configuring_stage: (unit -> unit) -> unit
(** Register an action to be executed at the end of the configuring stage.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Beryllium-20090601-beta1 *)

val run_after_setting_files: (string list -> unit) -> unit
(** Register an action to be executed just after setting the files put on the
    command line. The argument of the function is the list of files.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Carbon-20101201 *)

val at_normal_exit: (unit -> unit) -> unit
(** Register a hook executed whenever Frama-C exits without error (the exit
    code is 0).
    @since Boron-20100401 *)

val at_error_exit: (exn -> unit) -> unit
(** Register a hook executed whenever Frama-C exits with error (the exit
    code is greater than 0). The argument of the hook is the exception at the
    origin of the error.
    @since Boron-20100401
*)

(** Group of command line options.
    @since Beryllium-20090901 *)
module Group : sig
  (** @since Beryllium-20090901 *)
  type t

  val default: t
  (** @since Beryllium-20090901 *)

  val name: t -> string
  (** @since Beryllium-20090901 *)

  (**/**)
  (** Kernel internals *)

  val add: ?memo:bool -> plugin:string -> string -> t * bool
  (** Add a new group of options to the given plugin.
      If [memo] is [true], just return the already registered group if any.
      If [memo] is [false], cannot add twice a group with the same name.
      @return the group corresponding to the given name. Also return [true]
      iff the group has just been created.
      @since Beryllium-20090901 *)
  (**/**)

end

(**/**)

(* ************************************************************************** *)
(* ************************************************************************** *)
(** From here: functions required by Kernel Internals only!
    You should not use them! *)
(* ************************************************************************** *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(** {2 Handle Hooks} *)
(* ************************************************************************** *)

val protect: exn -> string
(** Messages for exceptions raised by Frama-C
    @since Boron-20100401 *)

val catch_at_toplevel: exn -> bool
(** @return true iff the given exception is caught by the Frama-C toplevel.
    @since Boron-20100401 *)

val catch_toplevel_run:
  f:(unit -> unit) ->
  at_normal_exit:(unit -> unit) ->
  on_error:(exn -> unit) ->
  unit
(** Run [f]. When done, either call [at_normal_exit] if running [f] was ok;
    or call [on_error] (and exits) in other cases.
*)

val run_normal_exit_hook: unit -> unit
(** Run all the hooks registered by {!at_normal_exit}.
    @since Boron-20100401 *)

val run_error_exit_hook: exn -> unit
(** Run all the hooks registered by {!at_normal_exit}.
    @since Boron-20100401
*)

val error_occurred: exn -> unit
(** Remember that an error occurred.
    So {!run_error_exit_hook} will be called when Frama-C will exit.
    @since Boron-20100401
*)

val bail_out: unit -> 'a
(** Stop Frama-C with exit 0.
    @since Boron-20100401 *)

(* ************************************************************************** *)
(** {2 Special functions}
    (* ************************************************************************** *)

    These functions should not be used by a standard plug-in developer. *)

val parse_and_boot:
  get_toplevel:(unit -> (unit -> unit) -> unit) ->
  play_analysis:(unit -> unit) -> unit
(** Not for casual users.
    [parse_and_boot get_toplevel play] performs the
    parsing of the command line, then play the analysis with the good
    toplevel provided by [get_toplevel].
    @since Beryllium-20090901
*)

val nb_given_options: unit -> int
(** Number of options provided by the user on the command line.
    Should not be called before the end of the command line parsing.
    @since Beryllium-20090601-beta1 *)

val use_cmdline_files: (Filepath.t list -> unit) -> unit
(** What to do with the list of files put on the command lines.
    @since Beryllium-20090601-beta1 *)

val help: unit -> exit
(** Display the help of Frama-C
    @since Beryllium-20090601-beta1 *)

val list_plugins: unit -> exit
(** Display the list of installed plug-ins
    @since Magnesium-20151001 *)

val explain_cmdline : unit -> exit

val plugin_help: string -> exit
(** Display the help of the given plug-in (given by its shortname).
    @since Beryllium-20090601-beta1 *)

val print_option_help:
  Format.formatter -> plugin:string -> group:Group.t -> string -> unit
(** Pretty print the help of the option (given by its plug-in, its group and its
    name) in the provided formatter.
    @since Oxygen-20120901 *)

val add_plugin: ?short:string -> string -> help:string -> unit
(** [add_plugin ~short name ~help] adds a new plug-in recognized by the
    command line of Frama-C. If the shortname is not specified, then the name
    is used as the shortname. By convention, if the name and the shortname
    are equal to "", then the register "plug-in" is the Frama-C kernel
    itself.
    @raise Invalid_argument if the same shortname is registered twice
    @since Beryllium-20090601-beta1 *)

(** @since Beryllium-20090601-beta1 *)
type option_setting =
  | Unit of (unit -> unit)
  | Int of (int -> unit)
  | Float of (float -> unit)
  | String of (string -> unit)

val add_option:
  string ->
  plugin:string ->
  group:Group.t ->
  stage ->
  ?argname:string ->
  help:string ->
  visible:bool ->
  safe:bool ->
  ext_help:(unit,Format.formatter,unit) format ->
  option_setting ->
  unit
(** [add_option name ~plugin stage ~argname ~help setting]
    adds a new option of the given [name] recognized by the command line of
    Frama-C. If the [name] is the empty string, nothing is done.
    [plugin] is the shortname of the plug-in.
    [argname] is the name of the argument which can be used of the
    description [help]. Both of them are used by the help of the
    registered option. If [help] is [None], then the option is not shown
    in the help.
    @since Beryllium-20090601-beta1
*)

val add_option_without_action:
  string ->
  plugin:string ->
  group:Group.t ->
  ?argname:string ->
  help:string ->
  visible:bool ->
  ext_help:(unit,Format.formatter,unit) format ->
  unit ->
  unit
(** Equivalent to [add_option] without option setting.
    Thus do not add the option to any stage of the command line...
    Thus should not be used by casual users ;-).
    @since Carbon-20101201 *)

val add_aliases:
  string ->
  plugin:string ->
  group:Group.t ->
  ?visible: bool ->
  ?deprecated: bool ->
  stage ->
  string list ->
  unit
(** [add_aliases orig plugin group aliases] adds a list of aliases to the given
    option name [orig].
    If [visible] is set to false, the aliases do not appear in help messages.
    If [deprecated] is set to true, the use of the aliases emits a warning.
    @raise Invalid_argument if an alias name is the empty string
    @since Carbon-20110201
    @before 22.0-Titanium no [visible] and [deprecated] arguments. *)

val replace_option_setting:
  string -> plugin:string -> group:Group.t -> option_setting -> unit
(** Replace the previously registered option setting.
    @since Sodium-20150201 *)

val replace_option_help:
  string -> plugin:string -> group:Group.t -> string -> unit
(** Replace the previously registered option help.
    @since 21.0-Scandium *)

(* ************************************************************************** *)
(** {2 Special parameters}
    (* ************************************************************************** *)

    Frama-c parameters depending on the command line argument and set at the
    very beginning of the Frama-C initialization.

    They should not be used directly by a standard plug-in developer. *)

(** @since Fluorine-20130401 *)
module type Level = sig
  val value_if_set: int option ref
  val get: unit -> int
  val set: int -> unit
end
[@@deprecated "use Log.Level instead."]

module Debug_level: Log.Level
(** @since Fluorine-20130401 *)

module Verbose_level: Log.Level
(** @since Fluorine-20130401 *)

module Kernel_debug_level: Log.Level
(** @since Fluorine-20130401 *)
[@@deprecated "Use Kernel_log.Debug_level instead."]

module Kernel_verbose_level: Log.Level
(** @since Fluorine-20130401 *)
[@@deprecated "Use Kernel_log.Verbose_level instead."]

val kernel_debug_atleast_ref: (int -> bool) ref
(** @since Boron-20100401 *)
[@@deprecated "Use Kernel_log.kernel_debug_atleast_ref instead." ]
[@@migrate { repl = Kernel_log.kernel_debug_atleast_ref }]

val kernel_verbose_atleast_ref: (int -> bool) ref
(** @since Boron-20100401 *)
[@@deprecated "Use Kernel_log.kernel_verbose_atleast_ref instead." ]
[@@migrate { repl = Kernel_log.kernel_verbose_atleast_ref }]

module Kernel_log: Log.Messages
[@@deprecated "Use Kernel_log module instead." ]
[@@migrate { repl = Kernel_log }]
(** @since Neon-20140301 *)

val quiet: bool
(** Must not be used for something else that initializing values
    @since Beryllium-20090601-beta1 *)

val deterministic: bool
(** Indicates that the plugins should strive to be as deterministic as
    possible in their outputs. Higher memory consumption or analysis time
    are acceptable, as reproducibility is more important.
    @since Aluminium-20160501 *)

val tty: bool
(** Indicates whether the standard output should be considered as a tty.
    Defaults to [Unix.isattty Unix.stdout && Ansi_escape.is_supported ()].
    @since 32.0-Germanium *)

val permissive: bool
(** Downgrades some command-line errors to warnings, such as
    unknown option names and invalid values for some options
    (e.g. non-existent function names).

    @since 22.0-Titanium *)

val compress_saved_session: bool
(** Indicates whether the saved sessions should be compressed or not.

    @since 31.0-Gallium *)
[@@deprecated "Use Project.compress_saved_session instead."]
[@@migrate { repl = Project.compress_saved_session } ]

val last_project_created_by_copy: (unit -> int option) ref
[@@deprecated "Use Project.last_project_created_by_copy instead."]
[@@migrate { repl = Project.last_project_created_by_copy } ]

type project_functions = {
  current: unit -> int;
  on_from_pid: 'a. int -> (unit -> 'a) -> 'a;
  pid_to_name: int -> string;
  name_to_pid: string -> int;
}

val project_functions : project_functions ref
[@@deprecated "Use Project functions instead. 'current' was renamed 'get_current_pid'."]

val load_all_plugins: (unit -> unit) ref

val add_loading_failures: string -> unit
(** Add a package to the list of ocamlfind packages that have failed to be
    loaded.
    @since Silicon-20161101 *)

(**/**)
