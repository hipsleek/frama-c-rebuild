(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Operations on globals.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

open Cil_types


(** Globals variables.
    The AST should be computed before using this module
    (cf. {! Ast.compute}). *)
module Vars: sig

  (** {2 Getters} *)

  val find: varinfo -> initinfo

  (** Retrieve the (wide)string literal associated to a given varinfo.
      Fatal error if the variable does not have the corresponding attribute
      (see {!Ast_info.is_string_literal})
      @since 32.0-Germanium
  *)
  val get_string_literal: varinfo -> str_literal

  (** Creates a new global holding a (wide)string literal and insert it
      into the Ast, marking it as grown.
      @return the newly created varinfo
      @since 32.0-Germanium
  *)
  val add_string_literal: loc:location -> str_literal -> varinfo

  val find_from_astinfo: string -> syntactic_scope -> varinfo
  (** Finds a variable from its [vname] according to its localisation (which
      might be a local). If you wish to search for a symbol according to its
      original name in the source code and the syntactic scope in which
      it should appear, use {! Globals.Syntactic_search} instead.
      @raise Not_found if no such variable exists.
  *)

  val get_astinfo: varinfo -> string * syntactic_scope
  (** Linear in the number of locals and formals of the program. *)

  (** {2 Iterators} *)

  val iter: (varinfo -> initinfo -> unit) -> unit
  val fold: (varinfo -> initinfo -> 'a -> 'a) -> 'a -> 'a

  (** The next four iterators iter on all global variables present in
      the AST, following the order in which they are declared/defined. The only
      exception is for variables that are both declared and defined. In
      this case, the declarations are skipped altogether. *)
  val iter_in_file_order: (varinfo -> initinfo -> unit) -> unit
  val fold_in_file_order: (varinfo -> initinfo -> 'a -> 'a) -> 'a -> 'a
  (** @since Fluorine-20130401 *)

  val iter_in_file_rev_order: (varinfo -> initinfo -> unit) -> unit
  (** @since Neon-20140301 *)

  val fold_in_file_rev_order: (varinfo -> initinfo -> 'a -> 'a) -> 'a -> 'a
  (** @since Neon-20140301 *)

  (** {2 Setters}

      Functions of this section should not be called by casual users. *)

  exception AlreadyExists of varinfo * initinfo
  val add: varinfo -> initinfo -> unit
  (** @raise AlreadyExists if the given varinfo is already registered. *)

  val remove: varinfo -> unit
  (** Removes the given varinfo, which must have already been removed from the
      AST. Warning: this is very dangerous.
      @since 18.0-Argon
  *)

  val add_decl: varinfo -> unit
  (** @raise AlreadyExists if the given varinfo is already registered. *)

  val self: State.t

end

(* ************************************************************************* *)
(** Functions.
    The AST should be computed before using this module
    (cf. {! Ast.compute}). *)
module Functions: sig

  val self: State.t

  (** {2 Getters} *)

  val mem: varinfo -> bool
  (** Returns [true] is this variable is associated to some kernel function *)

  val get: varinfo -> kernel_function
  (** @raise Not_found if the given varinfo has no associated kernel function
      and is not a built-in.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  val get_params: kernel_function -> varinfo list
  val get_vi: kernel_function -> varinfo

  (** {2 Membership} *)

  val mem_name: string -> bool
  (** @return [true] iff there is a function with such a name
      @since 22.0-Titanium *)

  val mem_def_name: string -> bool
  (** @return [true] iff there is a function definition with such a name
      @since 22.0-Titanium *)

  val mem_decl_name: string -> bool
  (** @return [true] iff there is a function declaration with such a name
      @since 22.0-Titanium *)

  (** {2 Searching} *)

  val find_by_name : string -> kernel_function
  (** @raise Not_found if there is no function of this name. *)

  val find_all_by_orig_name : ?cmp:(kernel_function -> kernel_function -> int) ->
    string -> kernel_function list
  (**
     [find_all_by_orig_name ?cmp name] returns the list of functions whose original
     name is [name], sorted according to [cmp]. If [cmp] is [None],
     the resulting order is unspecified.

     @since 23.0-Vanadium
  *)

  val find_def_by_name : string -> kernel_function
  (** @raise Not_found if there is no function definition of this name. *)

  val find_decl_by_name : string -> kernel_function
  (** @raise Not_found if there is no function declaration of this name.
      @since Aluminium-20160501 *)

  (** {2 Iterators} *)

  val iter: (kernel_function -> unit) -> unit
  val fold: (kernel_function -> 'a -> 'a) -> 'a -> 'a
  val iter_on_fundecs: (fundec -> unit) -> unit

  (** {2 Setters}

      Functions of this section should not be called by casual users. *)

  val add: cil_function -> unit
  (**TODO: remove this function and replace all calls by: *)

  val remove: varinfo -> unit
  (** Removes the given varinfo, which must have already been removed from the
      AST. Warning: this is very dangerous.
      @since 18.0-Argon
  *)

  val replace_by_declaration: funspec -> varinfo -> location -> unit
  (** Note: if the varinfo is already registered and bound to a definition,
      the definition will be erased only if [vdefined] is false. Otherwise,
      you're trying to register a declaration for a varinfo that is supposed
      to be defined, which does not look very good. *)

  val replace_by_definition: funspec -> fundec -> location -> unit
  (**TODO: do not take a funspec as argument *)

  val register: kernel_function -> unit
end

(* ************************************************************************* *)
(** Globals associated to filename. *)
module FileIndex : sig

  val self: State.t
  (** The state kind corresponding to the table of global C symbols.
      @since Boron-20100401 *)

  (** {2 Getters} *)

  val get_symbols : Filepath.t -> global list
  (** All global C symbols of the given module.
      @since Boron-20100401 *)

  val get_files: unit -> Filepath.t list
  (** Get the files list containing all [global] C symbols. *)

  (** {2 Searching among all [global] C symbols} *)

  val get_globals : Filepath.t -> (varinfo * initinfo) list
  (** Global variables of the given module for the kernel user interface *)

  val get_global_annotations: Filepath.t -> global_annotation list
  (** Global annotations of the given module for the kernel user interface
      @since Nitrogen-20111001 *)

  val get_functions :
    ?declarations:bool -> Filepath.t -> kernel_function list
  (** Global functions of the given module for the kernel user interface.
      If [declarations] is true, functions declared in a module but defined
      in another module are only reported in the latter (default is false).
  *)

  val kernel_function_of_local_var_or_param_varinfo :
    varinfo -> (kernel_function * bool)
  (** kernel_function where the local variable or formal parameter is
      declared. The boolean result is true for a formal parameter.
      @raise Not_found if the varinfo is a global one. *)

  val remove_global_annotations: global_annotation -> unit
  (** @since Oxygen-20120901 *)

end

module Syntactic_search: sig
  val self: State.t

  val find_in_scope: ?strict:bool -> string -> syntactic_scope -> varinfo option
  (** [find_in_scope orig_name scope] finds a variable from its [orig_name],
      according to the syntactic [scope] in which it should be searched.
      @param strict indicates whether the symbol should be searched only in
        the exact [scope] that it is given. It defaults to [false], meaning
        that the search will look for enclosing scopes, according to C lookup
        rules. More precisely, non-strict lookup will proceed according to the
        following order: [Block_scope] will move across all enclosing blocks
        up to the function body (or all locals will be searched directly in
        case of [Whole_function]). Then, [Formal] will be considered, followed
        by [Translation_unit] (for the file where the function is defined),
        then [Program].
      @return [None] if there are no variables [orig_name] in [scope].
      @return [Some vi] otherwise, with [vi] the [varinfo] associated to
        [orig_name] in [scope] (or an enclosing scope when [strict] is false).
      @since Chlorine-20180501
      @before 27.0-Cobalt [strict] parameter did not exist, symbol was always
        searched according to C lookup rules.
  *)
end

(* ************************************************************************* *)
(** {2 Types} *)
(* ************************************************************************* *)

(** Types, or type-related information. *)
module Types : sig

  (** The two functions below are suitable for use in functor
      {!Logic_typing.Make} *)

  val mem_enum_tag: string -> bool
  (** @return [true] iff there is an enum constant with the given name in the
      AST.
      @since 22.0-Titanium *)

  val find_enum_tag: string -> exp * typ
  (** Find an enum constant from its name in the AST.
      @raise Not_found when no such constant exists. *)

  val mem_type: Logic_typing.type_namespace -> string -> bool
  (** @return [true] iff there is a type with the given name in the given
      namespace in the AST.
      @since 22.0-Titanium *)

  val find_type: Logic_typing.type_namespace -> string -> typ
  (** Find a type from its name in the AST.
      @raise Not_found when no such type  exists. *)

  val iter_types: (string -> typ -> Logic_typing.type_namespace -> unit) -> unit
  (** Iteration on named types (typedefs, structs, unions, enums). The first
      argument is the name of type. *)

  val global: Logic_typing.type_namespace -> string -> global
  (** Find the global that defines the corresponding type.
      @raise Not_found if no such type has been defined or declared.

      @since Magnesium-20151001 *)
end

(* ************************************************************************* *)
(** {2 Entry point} *)
(* ************************************************************************* *)

exception No_such_entry_point of string
(** May be raised by [entry_point] below. *)

val entry_point : unit -> kernel_function * bool
(** @return the current function entry point and a boolean indicating if it
    is a library entry point.
    @raise No_such_entry_point if the current entrypoint name does not
    exist. This exception is automatically handled by the Frama-C kernel. Thus
    you don't have to catch it yourself, except if you do a specific work. *)

val set_entry_point : string -> bool -> unit
(** [set_entry_point name lib] sets [Kernel.MainFunction] to [name] and
    [Kernel.LibEntry] to [lib].
    Moreover, clear the results of all the analysis which depend on
    [Kernel.MainFunction] or [Kernel.LibEntry].
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************* *)
(** {2 Comments} *)
(* ************************************************************************* *)

val get_comments_global: global -> string list
(** Gets a list of comments associated to the given global. This function
    is useful only when -keep-comments is on.

    A comment is associated to a global if it occurs after
    the declaration/definition of the preceding one in the file, before the end
    of the current declaration/definition and does not occur in the
    definition of a function. Note that this function is experimental and
    may fail to associate comments properly. Use directly
    {! Cabshelper.Comments.get} to retrieve comments in a given region.
    (see {!Globals.get_comments_stmt} for retrieving comments associated to
    a statement).

    @since Nitrogen-20111001
*)

val get_comments_stmt: stmt -> string list
(** Gets a list of comments associated to the given statement. This function
    is useful only when -keep-comments is on.

    A comment is associated to a statement if it occurs after
    the preceding statement and before the current statement ends (except for
    the last statement in a block, to which statements occurring before the end
    of the block are associated). Note that this function is experimental and
    may fail to associate comments properly. Use directly
    {! Cabshelper.Comments.get} to retrieve comments in a given region.

    @since Nitrogen-20111001
*)

(* ************************************************************************* *)
(** {2 Getters} *)
(* ************************************************************************* *)

val get_annotation_name: global_annotation -> string option
(** Returns the name of the global annotation, when it exists
    (e.g. for axiomatics, predicates, etc), or [None] if no such name exists
    (e.g. for volatile blocks).

    @since 31.0-Gallium
*)

val get_name: global -> string option
(** Returns the name of the global, when it exists (e.g. for variables,
    functions, types, etc), or [None] if no such name exists
    (e.g. for assembly statements).

    @since 31.0-Gallium
*)

(* **/** *)
(* Forward reference to functions defined in Kernel_function. Do not
   use outside of this module.
*)
val get_statics: (kernel_function -> varinfo list) ref
val find_first_stmt: (kernel_function -> stmt) ref
val find_enclosing_block: (stmt -> block) ref
val find_all_enclosing_blocks: (stmt -> block list) ref
val find_englobing_kf: (stmt -> kernel_function) ref
