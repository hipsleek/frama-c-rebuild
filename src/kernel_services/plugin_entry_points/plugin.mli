(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Plugin registration and general services.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)

(** Special signature for Kernel services, whose messages are handled in
    an ad'hoc manner. Should not be of any use for a standard plug-in,
    who would rather rely on {!Plugin.S} below.
    @since Chlorine-20180501
*)
module type S_no_log = sig

  val add_group: ?memo:bool -> string -> Cmdline.Group.t
  (** Create a new group inside the plug-in.
      The given string must be different of all the other group names of this
      plug-in if [memo] is [false].
      If [memo] is [true] the function will either create a fresh group or
      return an existing group of the same name in the same plugin.
      [memo] defaults to [false]
      @since Beryllium-20090901 *)

  module Verbose: Parameter_sig.Int
  module Debug: Parameter_sig.Int

  module Message_category: Parameter_sig.String
  module Warn_category: Parameter_sig.String

  (** Handle the specific `lib' directory of the plug-in.
      @since 33.0-Arsenic
  *)
  module Lib: Parameter_sig.Site_root

  (** Handle the specific `share' directory of the plug-in.
      @since Oxygen-20120901
      @before 30.0-Zinc more modes were allowed
  *)
  module Share: Parameter_sig.Site_root

  (** Handle the specific `session' directory of the plug-in.
      @since Neon-20140301
      @before 30.0-Zinc Session was a Specific_dir.
  *)
  module Session: Parameter_sig.User_dir_opt

  (** Handle the specific `cache' directory of the plug-in.
      @since 30.0-Zinc
  *)
  module Cache_dir (): Parameter_sig.User_dir_opt

  (** Handle the specific `config' directory of the plug-in.
      @since Neon-20140301
      @before 30.0-Zinc this was not a functor and one could expect the
              directory to exist
  *)
  module Config_dir (): Parameter_sig.User_dir_opt

  (** Handle the specific `state' directory of the plug-in.
      @since 30.0-Zinc
  *)
  module State_dir (): Parameter_sig.User_dir_opt

  val help: Cmdline.Group.t
  (** The group containing option -*-help.
      @since Boron-20100401 *)

  val messages: Cmdline.Group.t
  (** The group containing options -*-debug and -*-verbose.
      @since Boron-20100401 *)

  val grp_debug: Cmdline.Group.t
  (** Group containing debug options.
      @since 32.0-Germanium *)

  val add_plugin_output_aliases:
    ?visible:bool -> ?deprecated:bool -> string list -> unit
    (** Adds aliases to the options -plugin-help, -plugin-verbose, -plugin-log,
        -plugin-msg-key, and -plugin-warn-key.
        [add_plugin_output_aliases [alias]] adds the aliases -alias-help,
        -alias-verbose, etc.
        @since 18.0-Argon
        @before 22.0-Titanium no [visible] and [deprecated] arguments.
    *)
end

(** Provided plug-general services for plug-ins.
    @since Beryllium-20090601-beta1
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module type S = sig
  include Log.Messages
  include S_no_log
end

type plugin = private
  { p_name: string;
    p_shortname: string;
    p_help: string;
    p_parameters: (string, Typed_parameter.t list) Hashtbl.t }
(** @since Beryllium-20090901
    @before 22.0-Titanium only "iterable" parameters were included;
            now all parameters are.
*)

module type General_services = sig
  include S
  include Parameter_sig.Builder
end

(**/**)

val register_kernel: unit -> unit
(** Begin to register parameters of the kernel. Not for casual users.
      @since Beryllium-20090601-beta1 *)

(**/**)

(** Functors for registering a new plug-in. It provides access to several
    services.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Register
    (_: sig
       val name: string
       (** Name of the module. Arbitrary non-empty string. *)

       val shortname: string
       (** Prefix for plugin options. No space allowed. *)

       val help: string
       (** description of the module. Free-form text. *)
     end):
  General_services

val is_share_visible: unit -> unit
(** Make visible to the end-user the -<plug-in>-share option.
    To be called just before applying {!Register} to create plug-in services.
    @since Oxygen-20120901 *)

val is_session_visible: unit -> unit
(** Make visible to the end-user the -<plug-in>-session option.
    To be called just before applying {!Register} to create plug-in services.
    @since Neon-20140301 *)

val plugin_subpath: string -> unit
(** Use the given string as the sub-directory in which the plugin files will
    be installed (ie. [share/frama-c/plugin_subpath]...). Relevant for
    directories [Share], [Session] and [Config] above.
    @since Neon-20140301 *)

val set_default_verbose_level: int -> unit
(** Set the default level of the -<plug-in>-verbose parameter.
    To be called just before applying {!Register} to create plug-in services.
    @since 33.0-Arsenic *)

(* ************************************************************************* *)
(** {2 Handling plugins} *)
(* ************************************************************************* *)

val get_from_shortname: string -> plugin
(** Get a plug-in from its shortname.
    @since Oxygen-20120901  *)

val get_from_name: string -> plugin
(** Get a plug-in from its name.
    @since Oxygen-20120901 *)

val is_present: string -> bool
(** Whether a plug-in already exists.
    Plugins are identified by their short name.
    @since Magnesium-20151001 *)

val iter_on_plugins: (plugin -> unit) -> unit
(** Iterate on each registered plug-in.
    @since Beryllium-20090901 *)

val fold_on_plugins: (plugin -> 'a -> 'a) -> 'a -> 'a
(** Fold [f] on each registered plug-in.
    @since 22.0-Titanium *)

(**/**)
(* ************************************************************************* *)
(** {2 Internal kernel stuff} *)
(* ************************************************************************* *)

val session_is_set_ref: (unit -> bool) ref
val session_ref: (unit -> Filepath.t) ref

val cache_is_set_ref: (unit -> bool) ref
val cache_ref: (unit -> Filepath.t) ref

val config_is_set_ref: (unit -> bool) ref
val config_ref: (unit -> Filepath.t) ref

val state_is_set_ref: (unit -> bool) ref
val state_ref: (unit -> Filepath.t) ref
