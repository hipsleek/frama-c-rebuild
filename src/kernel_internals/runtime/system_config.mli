(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Information about the environment *)

module Version : sig
  val id: string
  (** Frama-C version identifier. *)

  val codename: string
  (** Frama-C version codename. *)

  val id_and_codename: string
  (** Frama-C version and codename. *)

  val major: int
  (** Frama-C major version number. *)

  val minor: int
  (** Frama-C minor version number. *)
end

(** Unless you are working in the kernel of Frama-C, you should not use this. *)
module Share : sig
  val dirs: Filepath.t list
  (** Directories where architecture-independent files are located, in order of
      priority.
  *)

  val main: Filepath.t
  (** Last directory of {!dirs} (the directory of frama-c installation) *)

  val path: string
  (** The colon-separated concatenation of {!dirs}. *)

  val libc: Filepath.t
  (** Directory where Frama-C libc headers are. *)
end

(** Unless you are working in the kernel of Frama-C, you should not use this. *)
module Lib : sig
  val dirs: Filepath.t list
  (** Directories where library and executable files are located, in order of
      priority. *)

  val path: string
  (** The colon-separated concatenation of {!dirs}. *)

  val main: Filepath.t
  (** Last directory of libdirs (the directory of frama-c installation) *)
end

(** Unless you are working in the kernel of Frama-C, you should not use this. *)
module Plugins : sig
  val dirs: Filepath.t list
  (** Directories where the Frama-C dynamic plug-ins are located. *)

  val path: string
  (** The colon-separated concatenation of {!dirs}. *)

  val load: string -> unit
  (** Load given plug-in name *)

  val load_all: unit -> unit
  (** Load all plug-ins. *)
end

(** Unless you are working in the kernel of Frama-C, you should not use this. *)
module Preprocessor : sig
  val command: string
  (** Name of the default command to call the preprocessor.
      If the CPP environment variable is set, use it;
      else use the built-in default from autoconf. Usually this is
      "gcc -C -E -I."
  *)

  val is_default: bool
  (** whether the preprocessor command is the one defined at configure time
      or the result of taking a CPP environment variable, in case it differs
  *)

  val is_gnu_like: bool
  (** whether the default preprocessor accepts the same options as gcc
      (i.e. is either gcc or clang). When this is the case, the default
      command line for preprocessing contains more options.
  *)

  val keep_comments: bool
  (** [true] if the default preprocessor selected during compilation is
      able to keep comments (hence ACSL annotations) in its output.
  *)

  val supported_arch_options: string list
  (** architecture-related options (e.g. -m32) known to be supported by
      the default preprocessor. Used to match preprocessor commands to
      selected machdeps.
  *)
end

(** Default user directories
    Unless you are working in the kernel of Frama-C, you should not use this. *)
module User_dirs : sig
  val cache: unit -> Filepath.t
  (** Where Frama-C should read/write cached files. *)

  val config: unit -> Filepath.t
  (** Where Frama-C should read/write config files. *)

  val state: unit -> Filepath.t
  (** Where Frama-C should read/write state files *)
end

val is_gui: unit -> bool
(** Whether we are running in GUI mode. If you only need to know this, it is
    fine to use it, but if you have an actual dependency on the Frama-C server,
    prefer using the Server API.
    @before 33.0-Arsenic this was a boolean value and not a function
*)

(** / **)

val set_gui : bool -> unit
[@@alert
  system_config_set_gui
    "set_gui is for internal use only for the Server plugin."]
