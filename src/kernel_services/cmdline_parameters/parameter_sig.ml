(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Signatures for command line options. *)

(* ************************************************************************** *)
(** {2 Input signatures}

    One of these signatures is required to implement a new command line
    option. *)
(* ************************************************************************** *)

(** Minimal signature to implement for each parameter corresponding to an
    option on the command line argument. *)
module type Input = sig
  val option_name: string
  (** The name of the option *)

  val help: string
  (** A description for this option (e.g. used by -help).
      If [help = ""], then it has the special meaning "undocumented" *)
end

(** Minimal signature to implement for each parameter corresponding to an
    option on the command line argument which requires an argument. *)
module type Input_with_arg = sig
  include Input
  val arg_name: string
  (** A standard name for the argument which may be used in the description.
      If empty, a generic arg_name is generated. *)
end

(** Minimal signature for collections of custom datatype *)
module type Input_collection = sig
  include Input_with_arg
  val dependencies: State.t list
end

(** Signature required to build custom collection parameters in which elements
    are convertible to string.
    @since Sodium-20150201 *)
module type Value_datatype = sig
  include Datatype.S

  val of_string: string -> t
  (** @raise Cannot_build if there is no element corresponding to the given
      string. *)

  val to_string: t -> string
end

(** Signature requires to build custom collection parameters in which elements
    are convertible to string.
    @since Sodium-20150201 *)
module type Value_datatype_with_collections = sig
  include Datatype.S_with_collections

  val of_string: string -> t list
  (** Returns the list of elements corresponding to the given string.
      Should return a list of one element for most uses, except if one string
      can represent several elements.
      @raise Cannot_build if the string is invalid.
      @before 31.0-Gallium This function returned only one element, and function
      [of_singleton_string] was used to return a set of elements. *)

  val to_string: t -> string
end

(* ************************************************************************** *)
(** {2 Output signatures}

    Signatures corresponding to a command line option of a specific type. *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(** {3 Generic signatures} *)
(* ************************************************************************** *)

(** Generic signature of a parameter, without [parameter]. *)
module type S_no_parameter = sig

  type t
  (** Type of the parameter (an int, a string, etc). It is concrete for each
      module implementing this signature. *)

  val set: t -> unit
  (** Set the option. *)

  val add_set_hook: (t -> t -> unit) -> unit
  (** Add a hook to be called after the function {!set} is called.
      The first parameter of the hook is the old value of the parameter while
      the second one is the new value. *)

  val add_update_hook: (t -> t -> unit) -> unit
  (** Add a hook to be called when the value of the parameter changes (by
      calling {!set} or indirectly by the project library. The first parameter
      of the hook is the old value of the parameter while the second one is the
      new value. Note that it is **not** specified if the hook is applied just
      before or just after the effective change.
      @since Nitrogen-20111001 *)

  val get: unit -> t
  (** Option value (not necessarily set on the current command line). *)

  val clear: unit -> unit
  (** Set the option to its default value, that is the value if [set] was
      never called. *)

  val is_default: unit -> bool
  (** Is the option equal to its default value? *)

  val get_default: unit -> t
  (** Get the default value for the option.
      @since 24.0-Chromium *)

  val option_name: string
  (** Name of the option on the command-line
      @since Carbon-20110201  *)

  val print_help: Format.formatter -> unit
  (** Print the help of the parameter in the given formatter as it would be
      printed on the command line by -<plugin>-help. For invisible parameters,
      the string corresponds to the one returned if it would be not invisible.
      @since Oxygen-20120901 *)

  include State_builder.S

  val equal: t -> t -> bool

  val add_aliases: ?visible: bool -> ?deprecated:bool -> string list -> unit
  (** Add some aliases for this option. That is other option names which have
      exactly the same semantics that the initial option.
      If [visible] is set to false, the aliases do not appear in help messages.
      If [deprecated] is set to true, the use of the aliases emits a warning.
      @raise Invalid_argument if one of the strings is empty
      @before 22.0-Titanium no [visible] and [deprecated] arguments. *)

  (**/**)
  val is_set: unit -> bool
  (** Is the function {!set} has already been called since the last call to
      function {!clear}? This function is for special uses and should mostly
      never be used. *)

  val unsafe_set: t -> unit
  (** Set but without clearing the dependencies.*)
  (**/**)

end

(** Generic signature of a parameter. *)
module type S = sig
  include S_no_parameter
  val parameter: Typed_parameter.t
  (** @since Nitrogen-20111001 *)
end

(* ************************************************************************** *)
(** {3 Signatures for simple datatypes} *)
(* ************************************************************************** *)

(** Signature for a boolean parameter.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module type Bool = sig

  include S with type t = bool

  val on: unit -> unit
  (** Set the boolean to [true]. *)

  val off: unit -> unit
  (** Set the boolean to [false]. *)

end

(** Signature for an integer parameter.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module type Int = sig

  include S with type t = int

  val incr: unit -> unit
  (** Increment the integer. *)

  val set_range: min:int -> max:int -> unit
  (** Set what is the possible range of values for this parameter.
      @since Beryllium-20090901 *)

  val get_range: unit -> int * int
  (** What is the possible range of values for this parameter.
      @since Beryllium-20090901 *)

end

(** Signature for a floating-point parameter.
    @since 31.0-Gallium *)
module type Float = sig

  include S with type t = float

  val set_range: min:float -> max:float -> unit
  (** Set what is the possible range of values for this parameter. *)

  val get_range: unit -> float * float
  (** What is the possible range of values for this parameter. *)

end

(** Signature for a string parameter. *)
module type String = sig

  include S with type t = string

  val set_possible_values: string list -> unit
  (** Set what are the acceptable values for this parameter.
      If the given list is empty, then all values are acceptable.
      @since Beryllium-20090901 *)

  val get_possible_values: unit -> string list
  (** What are the acceptable values for this parameter.
      If the returned list is empty, then all values are acceptable.
      @since Beryllium-20090901 *)

  val get_function_name: unit -> string
  (** returns the given argument only if it is a valid function name
      (see {!Parameter_customize.get_c_ified_functions} for more information),
      and abort otherwise.

      Requires that the AST has been computed. Default getter when
      {!Parameter_customize.argument_is_function_name} has been called.
      @since Sodium-20150201
  *)

  val get_plain_string: unit -> string
  (** always return the argument, even if the argument is not a function name.
      @since Sodium-20150201
  *)
end

(** Signature for a custom parameter. *)
module type Custom = sig
  include S

  val set_possible_values: string list -> unit
  (** Set what are the acceptable values for this parameter.
      If the given list is empty, then all values are acceptable.
      @since 29.0-Copper *)

  val get_possible_values: unit -> string list
  (** What are the acceptable values for this parameter.
      If the returned list is empty, then all values are acceptable.
      @since 29.0-Copper *)
end

(* ************************************************************************** *)
(** {3 Custom signatures} *)
(* ************************************************************************** *)

(** signature for normalized pathnames. *)
module type Filepath = sig
  include S with type t = Filepath.t

  (**
     Whether the Filepath is empty.

     @since 23.0-Vanadium
  *)
  val is_empty: unit -> bool
end

(** Dune site directories (share, lib, ...) and subdirectories.
    They are connected to a root module (see {!Site_root}), that may not be
    unique, and these are considered as installed files (although the user might
    provide another location).

    @since 30.0-Zinc
*)
module type Site_dir = sig
  val get_dir: string -> Filepath.t
  (** [get_dir name] tries to find the directory named [name] in the
      site. The function aborts if: [name] cannot be found or is a file instead
      of a directory, otherwise it returns the path.

      Be careful! This function finds the first directory that exists in the
      site path. Thus, by extending this path, we get *only* the subdirs and
      files in this directory, not in all directories of same name.
      {!Builder.Make_site_dir} can be used to get directories that will always
      perform the resolution.
  *)

  val get_file: string -> Filepath.t
  (** [get_file name] tries to find the file named [name] in the
      site. The function aborts if: [name] cannot be found or is a directory
      instead of a file, otherwise it returns the path.
  *)
end

(** Dune site roots (share, lib, ...).

    @since 30.0-Zinc
*)
module type Site_root = sig
  include S with type t = Filepath.t

  val set: Filepath.t -> unit
  (** Sets the <dune-site-dir> directory (without creating it). *)

  val get: unit -> Filepath.t
  (** @return the <dune-site-dir> directory (without creating it). *)

  val is_set: unit -> bool
  (** @return whether the <dune-site-dir> has been set. *)

  include Site_dir
end

(** User directories (session, config, state, ...).
    We do not expect these directories/files to exist. Several roots are
    provided in {!Plugin}, namely {!Plugin.Session}, {!Plugin.Cache_dir},
    {!Plugin.Config_dir} and {!Plugin.State_dir}.

    @since 30.0-Zinc
*)
module type User_dir = sig
  val get_dir: ?create_path:bool -> string -> Filepath.t
  (** [get_dir ~create_path name] tries to get the directory [name].
      The function aborts if:
      - a file named [name] exists,
      - creating a the directory fails.

      Otherwise returns the path, and creates it if [create_path] is true
      (it defaults to false). Subdirectories modules can be created with
      {!Builder.Make_user_dir} and {!Builder.Make_user_dir_opt}.
  *)

  val get_file: ?create_path:bool -> string -> Filepath.t
  (** [get_file ~create_path name] tries to get the file [name].
      The function aborts if:
      - a directory named [name] exists,
      - creating the path to the file fails.

      Otherwise returns the path, and creates the directories that lead to the
      file if [create_path] is true (it defaults to false). The file is *not*
      created by the function.
  *)
end

(** Basically {!User_dir} but with an option to override the original path.

    @since 30.0-Zinc
*)
module type User_dir_opt = sig
  include User_dir
  include S with type t = Filepath.t

  val set: Filepath.t -> unit
  (** Sets the <user-dir> directory (without creating it). *)

  val get: unit -> Filepath.t
  (** @return the <user-dir> directory (without creating it). *)

  val is_set: unit -> bool
  (** @return whether the <user-dir> has been set. *)
end



(* ************************************************************************** *)
(** {3 Collections} *)
(* ************************************************************************** *)

(** Signature for a category over a collection.
    @since Sodium-20150201 *)
module type Collection_category = sig
  (** Element in the category *)
  type elt
  type t = elt Parameter_category.t

  val none: t
  (** The category '\@none' *)

  val default: unit -> t
  (** The '\@default' category. By default, it is {!none}. *)

  val all: unit -> t
  (** The '\@all' category. If this category has not been created, it is
      {!none}, which means 'ignored'.
      @since Silicon-20161101 *)

  val set_default: t -> unit
  (** Modify the '\@default' category. *)

  val add: string -> State.t list -> elt Parameter_category.accessor -> t
  (** Adds a new category for this collection with the given name, accessor and
      dependencies. *)

  val enable_all: State.t list -> elt Parameter_category.accessor -> t
  (** The category '\@all' is enabled in positive occurrences, with the given
      interpretation. In negative occurrences, it is always enabled and '-\@all'
      means 'empty'. *)

  val enable_all_as: t -> unit
  (** The category '\@all' is equivalent to the given category. *)

end

(** Common signature to all collections.
    @since Sodium-20150201 *)
module type Collection = sig

  include S
  (** A collection is a standard command line parameter. *)

  type elt
  (** Element in the collection. *)

  val is_empty: unit -> bool
  (** Is the collection empty? *)

  val iter: (elt -> unit) -> unit
  (** Iterate over all the elements of the collection. *)

  val fold: (elt -> 'a -> 'a) -> 'a -> 'a
  (** Fold over all the elements of the collection. *)

  val add: elt -> unit
  (** Add an element to the collection *)

  module As_string: String
  (** A collection is a standard string parameter *)

  module Category: Collection_category with type elt = elt
  (** Categories for this collection. *)
end

(** Signature for sets as command line parameters.
    @since Sodium-20150201 *)
module type Set = sig

  include Collection
  (** A set is a collection. *)

  (** {3 Additional accessors to the set.} *)

  val mem: elt -> bool
  (** Does the given element belong to the set? *)

  val exists: (elt -> bool) -> bool
  (** Is there some element satisfying the given predicate? *)

end

module type String_set =
  Set with type elt = string and type t = Datatype.String.Set.t

(** Set of defined kernel functions. If you want to also include pure
    prototype, use {!Parameter_customize.argument_may_be_fundecl}.
    @since Sodium-20150201
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module type Kernel_function_set =
  Set with type elt = Cil_types.kernel_function
       and type t = Cil_datatype.Kf.Set.t

(** @since Sodium-20150201 *)
module type Fundec_set =
  Set with type elt = Cil_types.fundec
       and type t = Cil_datatype.Fundec.Set.t

(** Signature for lists as command line parameters.
    @since Sodium-20150201 *)
module type List =  sig

  include Collection
  (** A list is a collection. *)

  (** {3 Additional accessors to the list.} *)

  val append_before: t -> unit
  (** append a list in front of the current state
      @since Neon-20140301 *)

  val append_after: t -> unit
  (** append a list at the end of the current state
      @since Neon-20140301 *)

end

module type String_list = List with type elt = string and type t = string list

module type Filepath_list =
  List with type elt = Filepath.t and type t = Filepath.t list

(** Signature for maps as command line parameters.
    @since Sodium-20150201 *)
module type Map = sig

  (** Type of keys of the map. *)
  type key

  (** Type of the values associated to the keys. *)
  type value

  include Collection with type elt = key * value
  (** A map is a collection in which elements are pairs [(key, value)], but some
      values may be missing. *)

  (** {3 Additional accessors to the map.} *)

  val find: key -> value
  (** Search a given key in the map.
      @raise Not_found if there is no such key in the map. *)

  val mem: key -> bool

end

(** Signature for multiple maps as command line parameters. Almost similar to
    {!Map}.
    @since Sodium-20150201 *)
module type Multiple_map = sig
  type key
  type value
  include Collection with type elt = key * value list
  val find: key -> value list
  val mem: key -> bool
end

module type Filepath_map =
  Map with type key = Filepath.t

(* ************************************************************************** *)
(** {2 All the different kinds of command line options as functors} *)
(* ************************************************************************** *)

(** Signatures containing the different functors which may be used to generate
    new command line options.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module type Builder = sig

  module Bool(_:sig include Input val default: bool end): Bool
  module Action(_: Input) : Bool

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module False(_: Input) : Bool

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module True(_: Input) : Bool

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module Int(_: sig include Input_with_arg val default: int end): Int

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module Zero(_: Input_with_arg): Int

  (** Parameter with an optional decimal point converted to an Ocaml float
      @since 31.0-Gallium *)
  module Float(_: sig include Input_with_arg val default: float end): Float

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module String(_: sig include Input_with_arg val default: string end): String

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module Empty_string(_: Input_with_arg): String

  module Filepath(_: sig
      include Input_with_arg
      val existence: Filepath.existence
      val file_kind: string
      (** used in error message if the file does not exist where it should
          and vice-versa. *)
    end): Filepath

  (** Builds a {!Site_dir} from an existing one. The first parameter is the
      parent directory. The second gives the name of the directory to create.

      @since 30.0-Zinc
  *)
  module Make_site_dir
      (_: Site_dir)
      (_: sig val name: string end)
    : Site_dir


  (** Builds a {!User_dir} from an existing one. The first parameter is the
      parent directory. The second gives the name of the directory to create.

      @since 30.0-Zinc
  *)
  module Make_user_dir
      (_: User_dir)
      (_: sig val name: string end)
    : User_dir

  (** Builds a {!User_dir_opt} from an existing {!User_dir}. The first parameter
      is the parent directory. The second gives the name of the directory to
      create (also used to create the option name), a possible environment
      variable name and the help message for the option.

      @since 30.0-Zinc
  *)
  module Make_user_dir_opt
      (_: User_dir)
      (_: sig
         include Input_with_arg
         val env: string option
         (** Can be used to provide an environment variable that can be used
             instead of the option. The option has higher priority.
         *)

         val dirname: string
         (** The name of the directory *)
       end): User_dir_opt

  (** Allow using custom types as parameters.
      @since 29.0-Copper *)
  module Custom
      (V: Value_datatype)
      (_: sig
         include Input_with_arg
         val default: V.t
       end) : Custom with type t = V.t

  (** A fixed set of possible values, represented by a type [t], intended to be
      a variant with only a finite number of possible constructions.
      Note that [t] must be comparable with structural equality
      @since 29.0-Copper
      @before 31.0-Gallium it required [all_values : t list] and
      [to_string : t -> string] *)
  module Enum(X : sig
      include Input
      type t
      val default: t
      val values: (t * string) list
    end) : S with type t = X.t

  exception Cannot_build of string
  module Make_set
      (E: Value_datatype_with_collections)
      (_: sig include Input_collection val default: E.Set.t end):
    Set with type elt = E.t and type t = E.Set.t

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module String_set(_: Input_with_arg): String_set

  module Filled_string_set
      (_: sig
         include Input_with_arg
         val default: Datatype.String.Set.t
       end): String_set

  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module Kernel_function_set(_: Input_with_arg): Kernel_function_set
  module Fundec_set(_: Input_with_arg): Fundec_set

  module Make_list
      (E:
       sig
         include Value_datatype
         val of_string: string -> t list
       end)
      (_: sig include Input_collection val default: E.t list end):
    List with type elt = E.t and type t = E.t list

  module String_list(_: Input_with_arg): String_list

  module Filepath_list
      (_: sig
         include Input_with_arg
         val existence: Fclib.Filepath.existence
         val file_kind: string
         (** see [Filepath] module. *)
       end): Filepath_list

  module Value_int: Value_datatype with type t = int
  module Value_string: Value_datatype with type t = string

  module Filepath_map
      (V: Value_datatype)
      (_: sig
         include Input_with_arg
         val default: V.t Fclib.Filepath.Map.t
         val existence: Fclib.Filepath.existence
         val file_kind: string
       end):
    Map
    with type key = Fclib.Filepath.t
     and type value = V.t
     and type t = V.t Fclib.Filepath.Map.t

  (** Parameter is a map where multibindings are **not** allowed. *)
  module Make_map
      (K: Value_datatype_with_collections)
      (V: Value_datatype)
      (_: sig include Input_collection val default: V.t K.Map.t end):
    Map
    with type key = K.t and type value = V.t and type t = V.t K.Map.t

  module String_map
      (V: Value_datatype)
      (_: sig include Input_with_arg val default: V.t Datatype.String.Map.t end):
    Map
    with type key = string
     and type value = V.t
     and type t = V.t Datatype.String.Map.t

  (** As for Kernel_function_set, by default keys can only be defined functions.
      Use {!Parameter_customize.argument_may_be_fundecl} to also include
      pure prototypes. *)
  module Kernel_function_map
      (V: Value_datatype)
      (_: sig include Input_with_arg val default: V.t Cil_datatype.Kf.Map.t end):
    Map
    with type key = Cil_types.kernel_function
     and type value = V.t
     and type t = V.t Cil_datatype.Kf.Map.t

  (** Parameter is a map where multibindings are allowed. *)
  module Make_multiple_map
      (K: Value_datatype_with_collections)
      (V: Value_datatype)
      (_: sig include Input_collection val default: V.t list K.Map.t end):
    Multiple_map
    with type key = K.t and type value = V.t and type t = V.t list K.Map.t

  module String_multiple_map
      (V: Value_datatype)
      (_: sig
         include Input_with_arg
         val default: V.t list Datatype.String.Map.t
       end):
    Multiple_map
    with type key = string
     and type value = V.t
     and type t = V.t list Datatype.String.Map.t

  (** As for Kernel_function_set, by default keys can only be defined functions.
      Use {!Parameter_customize.argument_may_be_fundecl} to also include
      pure prototypes. *)
  module Kernel_function_multiple_map
      (V: Value_datatype)
      (_: sig
         include Input_with_arg
         val default: V.t list Cil_datatype.Kf.Map.t
       end):
    Multiple_map
    with type key = Cil_types.kernel_function
     and type value = V.t
     and type t = V.t list Cil_datatype.Kf.Map.t

  val parameters: unit -> Typed_parameter.t list

end
