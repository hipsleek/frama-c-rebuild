(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Logging Services for Frama-C Kernel and Plugins.
    @since Beryllium-20090601-beta1 *)

type kind = Result | Feedback | Debug | Warning | Error | Failure
(** @since Beryllium-20090601-beta1 *)

type event = {
  evt_kind : kind ;
  evt_plugin : string ;
  evt_category : string option ; (** message or warning category *)
  evt_source : Filepos.t option ;
  evt_message : Rich_text.t ;
}
(** @since Beryllium-20090601-beta1 *)

module Event :
sig
  type t = event
  (** Pretty prints the event header and message. This function outputs the
      semantic tags if any in the event message.
      @param truncate if set, the output will be truncated if the message size
      is bigger than the given integer.  *)
  val pretty : ?truncate:Rich_text.truncation -> Format.formatter -> t -> unit

  (** Extract the message as a string. The output will be truncated
      if the message is too long. *)
  val message : t -> string
end

type 'a pretty_printer =
  ?current:bool -> ?source:Filepos.t ->
  ?emitwith:(event -> unit) -> ?echo:bool -> ?once:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,Format.formatter,unit) format -> 'a
(**
    Generic type for the various logging channels which are not aborting
    Frama-C. The first line will be prefixed (plugin name, location, message
    kind, etc.), consider skipping the first line (by adding a new line) if you
    want to keep the message alignment on multi-lines messages.
   - When [current] is [false] (default for most of the channels),
     no location is output. When it is [true], the last registered location
     is used as current (see {!Current_loc}).
   - [source] is the location to be output. If nil, [current] is used to
     determine if a location should be output
   - [emitwith] function which is called each time an event is processed
   - [echo] is [true] if the event should be output somewhere in addition
     to [stdout]
   - [append] adds some actions performed on the formatter after the event
     has been processed.
     @since Beryllium-20090601-beta1 *)

type ('a,'b) pretty_aborter =
  ?current:bool -> ?source:Filepos.t -> ?echo:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,Format.formatter,unit,'b) format4 -> 'a
(** Same as {!Log.pretty_printer} except that channels having this type
    denote a fatal error aborting Frama-C.
    @since Beryllium-20090601-beta1
*)

(* -------------------------------------------------------------------------- *)
(** {2 Exception Registry}
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Beryllium-20090601-beta1 *)
(* -------------------------------------------------------------------------- *)

exception AbortError of string
(** User error that prevents a plugin to terminate. Argument is the name
    of the plugin.
    @since Beryllium-20090601-beta1 *)

exception AbortFatal of string
(** Internal error that prevents a plugin to terminate. Argument is the
    name of the plugin.
    @since Beryllium-20090601-beta1 *)

exception FeatureRequest of Filepos.t option * string * string
(** Raised by [not_yet_implemented].
    You may catch [FeatureRequest(s,p,r)] to support degenerated behavior.
    The (optional) source location is s, the responsible plugin is 'p'
    and the feature request is 'r'.
    @before 23.0-Vanadium there was no source location
*)

(* -------------------------------------------------------------------------- *)
(** {2 Option_signature.Interface}
    @since Beryllium-20090601-beta1 *)
(* -------------------------------------------------------------------------- *)

type ontty = [
  | `Message   (** Normal message (default) *)
  | `Feedback  (** Temporary visible on console, normal message otherwise *)
  | `Transient (** Temporary visible, only on console *)
  | `Silent    (** Not visible on console *)
]

(** status of a warning category
    @since Chlorine-20180501
*)
type warn_status =
  | Winactive (** nothing is emitted. *)
  | Wfeedback_once (** combines feedback and once. *)
  | Wfeedback (** emit a feedback message. *)
  | Wonce (** emit a warning message, but only the first time the category
              is encountered. *)
  | Wactive (** emit a warning message. *)
  | Werror_once (** combines once and error. *)
  | Werror
  (** emit a message. Execution continues, but exit status will not be 0 *)
  | Wabort (** emit a message and abort execution *)

(**
   @since Beryllium-20090601-beta1
   @since 32.0-Germanium
   All formatters now interpret semantic tags for ANSI styling, as
   defined in {!Ansi_escape} module.
   @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)
module type Messages =
sig

  type category
  (** category for debugging/verbose messages. Must be registered before
      any use.
      Each column in the string defines a sub-category, e.g.
      a:b:c defines a subcategory c of b, which is itself a subcategory of a.
      Enabling a category (via -plugin-msg-category) will enable all its
      subcategories.
      @since Fluorine-20130401
  *)

  type warn_category
  (** Same as above, but for warnings
      @since Chlorine-20180501
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
  *)

  val verbose_atleast : int -> bool
  (** @since Beryllium-20090601-beta1 *)

  val debug_atleast : int -> bool
  (** @since Beryllium-20090601-beta1 *)

  val printf : ?level:int -> ?dkey:category ->
    ?current:bool -> ?source:Filepos.t ->
    ?append:(Format.formatter -> unit) ->
    ?header:(Format.formatter -> unit) ->
    ('a,Format.formatter,unit) format -> 'a
  (** Outputs the formatted message on [stdout]. Levels and
      key-categories are taken into account like event messages.
      The header formatted message is emitted as a regular [result]
      message. *)

  val result : ?level:int -> ?dkey:category -> 'a pretty_printer
  (** Results of analysis. Default level is 1.
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val has_tty : unit -> bool
  (** Returns [true] is this Log's channel is in console mode *)

  val feedback : ?ontty:ontty -> ?level:int -> ?dkey:category -> 'a pretty_printer
  (** Progress and feedback. Level is tested against the verbosity level.
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val debug : ?level:int -> ?dkey:category -> 'a pretty_printer
  (** Debugging information dedicated to Plugin developers.
      Default level is 1. The debugging key is used in message headers.
      See also [set_debug_keys] and [set_debug_keyset].
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val warning : ?wkey:warn_category -> 'a pretty_printer
  (** Hypothesis and restrictions.
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val error   : 'a pretty_printer
  (** user error: syntax/typing error, bad expected input, etc.
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val abort   : ('a,'b) pretty_aborter
  (** user error stopping the plugin.
      @raise AbortError with the channel name.
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val failure : 'a pretty_printer
  (** internal error of the plug-in.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val fatal   : ('a,'b) pretty_aborter
  (** internal error of the plug-in.
      @raise AbortFatal with the channel name.
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val verify : bool -> ('a,bool) pretty_aborter
  (** If the first argument is [true], return [true] and do nothing else,
      otherwise, send the message on the {i fatal} channel and return
      [false].

      The intended usage is: [assert (verify e "Bla...") ;].
      @since Beryllium-20090601-beta1
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val not_yet_implemented : ?current:bool -> ?source:Filepos.t ->
    ('a,Format.formatter,unit,'b) format4 -> 'a
  (** raises [FeatureRequest] but {i does not} send any message.
      If the exception is not caught, Frama-C displays a feature-request
      message to the user.
      @since Beryllium-20090901
      @before 23.0-Vanadium there was no [current] and [source] arguments.
  *)

  val deprecated: string -> now:string -> ('a -> 'b) -> ('a -> 'b)
  (** [deprecated s ~now f] indicates that the use of [f] of name [s] is now
      deprecated. It should be replaced by [now].
      @return the given function itself
      @since Lithium-20081201 in Extlib
      @since Beryllium-20090902 *)

  val with_result  : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_result f fmt] calls [f] in the same condition as {!logwith}.
      @since Beryllium-20090601-beta1
  *)

  val with_warning : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_warning f fmt] calls [f] in the same condition as {!logwith}.
      @since Beryllium-20090601-beta1
  *)

  val with_error   : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_error f fmt] calls [f] in the same condition as {!logwith}.
      @since Beryllium-20090601-beta1
  *)

  val with_failure : (event option -> 'b) -> ('a,'b) pretty_aborter
  (** [with_failure f fmt] calls [f] in the same condition as {!logwith}.
      @since Beryllium-20090601-beta1
  *)

  val log : ?kind:kind -> ?verbose:int -> ?debug:int -> 'a pretty_printer
  (** Generic log routine. The default kind is [Result]. Use cases (with
      [n,m > 0]):
      - [log ~verbose:n]: emit the message only when verbosity level is
        at least [n].
      - [log ~debug:n]: emit the message only when debugging level is
        at least [n].
      - [log ~verbose:n ~debug:m]: any debugging or verbosity level is
        sufficient.
        @since Beryllium-20090901
        @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val logwith : (event option -> 'b) -> ?wkey:warn_category ->
    ?emitwith:(event -> unit) -> ?once:bool -> ('a,'b) pretty_aborter
  (** Recommended generic log routine using [warn_category] instead of [kind].
      [logwith continuation ?wkey fmt] similar to [warning ?wkey fmt]
      and then calling the [continuation].
      The optional continuation argument refers to the corresponding event.
      [None] is used iff no message is logged.
      In case the [wkey] is considered as a [Failure], the continution is not called.
      This kind of message denotes a fatal error aborting Frama-C.
      Notice that the [~emitwith] action is called iff a message is logged.
      @since 18.0-Argon
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
  *)

  val register : kind -> (event -> unit) -> unit
  (** Local registry for listeners. *)

  val register_tag_handlers : (string -> string) * (string -> string) -> unit

  (** {3 Category management} *)

  val register_category: ?help:string -> ?default:bool -> string -> category
  (** register a new debugging/verbose category.
      Note: to enable a category's messages by default, pass [~default:true]
      or add it (e.g. via [add_debug_keys]) after registration.
      @since Fluorine-20130401
      @before 30.0-Zinc [?help] parameter was not present
      @before 32.0-Germanium [?default] parameter was not present
  *)

  val pp_category: Format.formatter -> category -> unit
  (** pretty-prints a category.
      @since Chlorine-20180501
  *)

  val pp_all_categories: unit -> unit
  (** pretty-prints all categories.
      @since 30.0-Zinc
  *)

  val dkey_name: category -> string
  (** returns the category name as a string.
      @since 18.0-Argon
  *)

  val get_category_help: category -> string
  (** returns the category help as a string.
      @since 33.0-Arsenic
  *)

  val is_registered_category: string -> bool
  (** true iff the string corresponds to a registered category
      @since Chlorine-20180501
  *)

  val get_category: string -> category option
  (** returns the corresponding registered category or [None] if no
      such category exists.
      @since Fluorine-20130401
  *)

  val get_all_categories: unit -> category list
  (** returns all registered categories. *)

  val add_debug_keys : category -> unit
  (** [add_debug_keys s] enables the emission of messages for the categories
      corresponding to [s], including potential subcategories (e.g. [a]
      and [a:b] for string [a:b]).
      The string must have been registered beforehand.
      @since Fluorine-20130401 use categories instead of plain string
  *)

  val del_debug_keys: category -> unit
  (** [add_debug_keys s] disables the emission of messages for the categories
      corresponding to [s], including potential subcategories (e.g. [a]
      and [a:b] for string [a:b]).
      The string must have been registered beforehand.
      @since Fluorine-20130401
  *)

  val get_debug_keys: unit -> category list
  (** Returns currently active keys
      @since Fluorine-20130401
  *)

  val is_debug_key_enabled: category -> bool
  (** Returns [true] if the given category is currently active
      @since Fluorine-20130401
  *)

  val register_warn_category:
    ?help:string -> ?default:warn_status -> string -> warn_category
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
      @before 30.0-Zinc [?help] parameter was not present
      @before 32.0-Germanium [?default] parameter was not present
  *)

  val is_warn_category: string -> bool

  val pp_warn_category: Format.formatter -> warn_category -> unit

  val pp_all_warn_categories_status: unit -> unit

  val wkey_name: warn_category -> string
  (** returns the warning category name as a string.
      @since 18.0-Argon
  *)

  val get_warn_category: string -> warn_category option

  val get_all_warn_categories: unit -> warn_category list

  val get_all_warn_categories_status: unit -> (warn_category * warn_status) list

  val set_warn_status: warn_category -> warn_status -> unit
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val get_warn_status: warn_category -> warn_status

end

(** Split an event category into its constituents.
    @since 18.0-Argon *)
val evt_category : event -> string list

(** Split a category specification into its constituents.
    ["*"] is considered as empty, and [""] categories are skipped.
    @since 18.0-Argon *)
val split_category : string -> string list

(** Sub-category checks.
    [is_subcategory a b] checks whether [a] is a sub-category of [b].
    Indeed, it checks whether [b] is a prefix of [a], that is,
    that [a] equals [b] or refines [b] with (a list of) sub-category(ies).
    @since 18.0-Argon *)
val is_subcategory : string list -> string list -> bool


(** Possible outcomes when parsing categories: printing the category help
    message, or returning the list of category. [bool] specify if the category
    should be added or removed.
    @since 33.0-Arsenic
*)
type category_action = Category_help | Change_category of (bool * string) list

(** Parse the given string to find categories to be added or removed via
    {!add_debug_keys} or {!del_debug_keys}.
    @since 33.0-Arsenic
*)
val parse_category: string -> category_action

(** Possible outcomes when parsing warning categories: printing the warning help
    message, returning the list of warning and their status, or a parsing error.
    @since 33.0-Arsenic
*)
type warning_action =
  | Warning_help
  | Set_status of (string * warn_status) list
  | Parsing_error of string

(** Parse the given string to find warning categories and their status which can
    be set via {!set_warn_status}
    @since 33.0-Arsenic
*)
val parse_warning: string -> warning_action

(** @before 33.0-Arsenic Was in {!Cmdline} *)
module type Level = sig
  val value_if_set: int option ref
  val get: unit -> int
  val set: int -> unit
end

(** @since 33.0-Arsenic *)
module Make_level(_ : sig val default : int end) : Level

(** Each plugin has its own channel to output messages.
    This functor should not be directly applied by plug-in developer.
    They should apply {!Plugin.Register} instead.
    @since Beryllium-20090601-beta1 *)
module Register
    (_ : sig
       val channel : string
       val label : string
       val verbose_atleast : int -> bool
       val debug_atleast : int -> bool
     end)
  : Messages

(* -------------------------------------------------------------------------- *)
(** {2 Echo and Notification} *)
(* -------------------------------------------------------------------------- *)

val set_echo : ?plugin:string -> ?kind:kind list -> bool -> unit
(** Turns echo on or off. Applies to all channel unless specified,
    and all kind of messages unless specified.
    @since Beryllium-20090601-beta1
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val add_listener : ?plugin:string -> ?kind:kind list -> (event -> unit) -> unit
(** Register a hook that is called each time an event is
    emitted. Applies to all channel unless specified,
    and all kind of messages unless specified.

    Warning: when executing the listener, all listeners will be
    temporarily deactivated in order to avoid infinite recursion.

    @since Beryllium-20090601-beta1
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val echo : event -> unit
(** Display an event of the terminal, unless echo has been turned off.
    @since Beryllium-20090601-beta1 *)

val notify : event -> unit
(** Send an event over the associated listeners.
    @since Beryllium-20090601-beta1 *)

(* -------------------------------------------------------------------------- *)
(** {2 Channel interface}
    This is the {i low-level} interface to logging services.
    Not to be used by casual users.
*)
(* -------------------------------------------------------------------------- *)

type channel
(** @since Beryllium-20090601-beta1 *)

val new_channel : string -> channel
(** @since Beryllium-20090901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val log_channel : channel -> ?kind:kind -> 'a pretty_printer
(** logging function to user-created channel.
    @since Beryllium-20090901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val kernel_channel_name: string
(** the reserved channel name used by the Frama-C kernel.
    @since Beryllium-20090601-beta1 *)
[@@deprecated "Use Kernel_log.kernel_channel_name instead."]
[@@migrate { repl = Kernel_log.kernel_channel_name } ]


val kernel_label_name: string
(** the reserved label name used by the Frama-C kernel.
    @since Beryllium-20090601-beta1 *)
[@@deprecated "Use Kernel_log.kernel_label_name instead."]
[@@migrate { repl = Kernel_log.kernel_label_name } ]

val source : file:Filepath.t -> line:int -> Filepos.t
(** @since Chlorine-20180501 *)

val get_current_source : unit -> Filepos.t

(* -------------------------------------------------------------------------- *)
(** {2 Terminal interface}
    This is the {i low-level} interface to logging services.
    Not to be used by casual users. *)
(* -------------------------------------------------------------------------- *)

val clean : unit -> unit
(** Flushes the last transient message if necessary. *)

val set_formatter : ?isatty:bool -> Format.formatter -> unit
(** Set the formatter for log outputs. This formatter is {!Format.std_formatter}
    by default and can be changed if the log output must be redirected.
    @since Beryllium-20090901
    @before 32.0-Germanium was [set_output] and took formatter output functions
    as arguments
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val reset_stdout : isatty:bool -> unit -> unit
(** Reset the log formatter to [Format.std_formatter].
    @since 32.0-Germanium *)

val print_on_output : (Format.formatter -> unit) -> unit
(** Direct printing on output.
    Message echo is delayed until the output is finished.
    Then, the output is flushed and all pending message are echoed.
    Notification of listeners is not delayed, however.

    Can not be recursively invoked.
    @since Beryllium-20090901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val print_delayed : (Format.formatter -> unit) -> unit
(** Direct printing on output.  Same as {!print_on_output}, except
    that message echo is not delayed until text material is actually
    written. This gives an chance for formatters to emit messages
    before actual pretty printing.

    Can not be recursively invoked.
    @since Beryllium-20090901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(**/**)

val set_current_source : (unit -> Filepos.t) -> unit
(** Forward reference to the function returning the current location,
    used when [~current:true] is set on printers. Currently set
    in {!Cil}. Not for the casual user. *)

val check_not_yet: (event -> bool) ref
(** Checks whether a message been emitted already, in which case it is
    not reprinted. Currently set in {!module-type:Messages}. Not for the casual
    user.
*)

val cmdline_error_occurred: (exn -> unit) ref
[@@deprecated "Use Cmdline.error_occurred directly instead"]

val cmdline_at_error_exit: ((exn -> unit) -> unit) ref
[@@deprecated "Use Cmdline.at_error_exit directly instead"]

val treat_deferred_error: unit -> unit
(** call this function when it is a good time to raise an exception following
    a delayed error or failure. Currently done:
    - after each command-line stage.
    - after each analysis step (as separated by -then and its derivatives),
      including the last one.
*)
