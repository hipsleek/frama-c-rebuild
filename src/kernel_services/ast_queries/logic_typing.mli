(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(** Logic typing and logic environment. *)

open Cil_types

(** Relation operators conversion
    @since Nitrogen-20111001
*)
val type_rel: Logic_ptree.relation -> Cil_types.relation

(** Arithmetic binop conversion. Addition and Subtraction are always
    considered as being used on integers. It is the responsibility of the
    user to introduce PlusPI, MinusPI and MinusPP where needed.
    @since Nitrogen-20111001
*)
val type_binop: Logic_ptree.binop -> Cil_types.binop

val unescape: string -> string
val wcharlist_of_string: string -> int64 list

val ctype_of_pointed: logic_type -> typ
val ctype_of_array_elem: logic_type -> typ

val arithmetic_conversion:
  Cil_types.logic_type -> Cil_types.logic_type -> Cil_types.logic_type

exception Typing_error of location * string

(** @param explicit defaults to false, when true it means that the coercion must
           be explicitly introduced (when possible) e.g. we really want the
           wrapping behavior in the term [(int) (INT_MAX+1)].
    @raise Typing_error when the cast is illegal.
    @since 32.0-Germanium
*)
val mk_cast:
  ?explicit:bool -> Cil_types.term -> Cil_types.logic_type -> Cil_types.term

(** Local logic environment *)
module Lenv : sig
  type t
  val empty : unit -> t
  (** Note that empty still contains the label Init since it is always
      available.

      @before 31.0-Gallium Init was not available in the environment.
  *)

  val add_var: string -> Cil_types.logic_var -> t -> t
  val add_type_var: string -> Cil_types.logic_type -> t -> t
  val add_logic_info: string -> Cil_types.logic_info -> t -> t
  val add_logic_label: string -> Cil_types.logic_label -> t -> t

  val find_var: string -> t-> Cil_types.logic_var
  val find_type_var: string -> t -> Cil_types.logic_type
  val find_logic_info: string -> t -> Cil_types.logic_info
  val find_logic_label: string -> t -> Cil_types.logic_label
end

type type_namespace = Typedef | Struct | Union | Enum
(** The different namespaces a C type can belong to, used when we are searching
    a type by its name. *)

module Type_namespace: Datatype.S with type t = type_namespace

type logic_infos =
  | Ctor of logic_ctor_info
  | Lfun of logic_info list

(** Functions that can be called when type-checking an extension of ACSL.

    @before 30.0-Zinc The following fields were present:

    {[
      remove_logic_function : string -> unit;
      remove_logic_info: logic_info -> unit;
      remove_logic_type: string -> unit;
      remove_logic_ctor: string -> unit;
      add_logic_function: logic_info -> unit;
      add_logic_type: string -> logic_type_info -> unit;
      add_logic_ctor: string -> logic_ctor_info -> unit;
      find_all_logic_functions: string -> logic_info list;
      find_logic_type: string -> logic_type_info;
      find_logic_ctor: string -> logic_ctor_info;
    ]}

    You shall now use directly functions from {!Logic_env} and {!Logic_utils}.

    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)
type typing_context = {
  is_loop: unit -> bool;
  anonCompFieldName : string;
  conditionalConversion : typ -> typ -> typ;
  find_macro : string -> Logic_ptree.lexpr;
  find_var : ?label:string -> string -> logic_var;
  (** the label argument is a C label (obeying the restrictions
      of which label can be present in a \at). If present, the scope for
      searching local C variables is the one of the statement with
      the corresponding label. *)
  find_enum_tag : string -> exp * typ;
  find_comp_field: compinfo -> string -> offset;
  find_type : type_namespace -> string -> typ;
  find_label : string -> stmt ref;
  pre_state:Lenv.t;
  post_state:termination_kind list -> Lenv.t;
  assigns_env: Lenv.t;
  silent: bool;
  logic_type:
    typing_context -> location -> Lenv.t ->
    Logic_ptree.logic_type -> Cil_types.logic_type ;
  type_predicate:
    typing_context -> Lenv.t -> Logic_ptree.lexpr -> predicate;
  (** typechecks a predicate. Note that the first argument is itself a
      [typing_context], which allows for open recursion. Namely, it is
      possible for the extension to change the type-checking functions for
      the sub-nodes of the parsed tree, and not only for the toplevel [lexpr].
  *)
  type_term:
    typing_context -> Lenv.t -> Logic_ptree.lexpr -> term;
  type_assigns:
    typing_context ->
    accept_formal:bool ->
    Lenv.t -> Logic_ptree.assigns -> assigns;
  error: 'a 'b. location -> ('a,Format.formatter,unit,'b) format4 -> 'a;
  on_error: 'a 'b. ('a -> 'b) -> ((location * string) -> unit) -> 'a -> 'b
  (** [on_error f rollback x] will attempt to evaluate [f x]. If this triggers
      an error while in [-kernel-warn-key annot-error] mode, [rollback
      (loc,cause)] will be executed (where [loc] is the location of the error
      and [cause] a text message indicating the issue) and the exception will
      be re-raised.

      @since Chlorine-20180501
      @before 25.0-Manganese [rollback] didn't take [loc] and [cause] as argument
  *)
}

(** Functions that can be called when importing external modules into ACSL.
    See {!Acsl_extension.register_module_importer} for details.
    @since 30.0-Zinc
*)
type module_builder = {
  add_logic_type : location -> logic_type_info -> unit ;
  add_logic_function : location -> logic_info -> unit ;
}

module type S =
sig

  (** @since Nitrogen-20111001 *)
  val type_of_field:
    location -> string -> logic_type -> (term_offset * logic_type)

  (**
     @param explicit true if the cast is present in original source.
            defaults to false
     @since Nitrogen-20111001
  *)
  val mk_cast:
    ?explicit:bool -> Cil_types.term -> Cil_types.logic_type -> Cil_types.term

  (** [conditional_conversion loc rel t1 t2]
      tries to find a common type between two terms, either as part of
      a comparison or a conditional.
      comparisons can notably introduce logic coercions to Real, potentially
      with a warning if [acsl-float-compare] is active.

      @param loc the location of the comparison. Can be used in error/warning msg
      @param rel the relation, or [None] for a conditional
      @param t1 first term
      @param t2 second term

      @since 27.0-Cobalt
  *)
  val conditional_conversion:
    Cil_types.location -> Logic_ptree.relation option ->
    Cil_types.term -> Cil_types.term -> Cil_types.logic_type

  (** type-checks a term. *)
  val term : Lenv.t -> Logic_ptree.lexpr -> term

  val predicate : Lenv.t -> Logic_ptree.lexpr -> predicate

  (** [code_annot loc behaviors rt annot] type-checks an in-code annotation.
      @param loc current location
      @param behaviors list of existing behaviors
      @param rt return type of current function
      @param annot the annotation
  *)
  val code_annot :
    Cil_types.location -> string list ->
    Cil_types.logic_type -> Logic_ptree.code_annot -> code_annotation

  val type_annot :
    location -> Logic_ptree.type_annot -> logic_info

  val model_annot :
    location -> Logic_ptree.model_annot -> model_info

  (** Some logic declaration might not introduce new global annotations
      (eg. already imported external modules).
      @before 30.0-Zinc always return a global annotation *)
  val annot : Logic_ptree.decl -> global_annotation option

  (** [funspec behaviors f prms typ spec] type-checks a function contract.
      @param behaviors list of existing behaviors (outside of the current
      spec, e.g. in the spec of the corresponding declaration when type-checking
      the spec of a definition)
      @param f the function
      @param prms its parameters
      @param its type
      @param spec the spec to typecheck
  *)
  val funspec :
    string list ->
    varinfo -> (varinfo list) option -> typ -> Logic_ptree.spec -> funspec

end

module Make
    (_ :
     sig
       val is_loop: unit -> bool
       (** whether the annotation we want to type is contained in a loop.
           Only useful when creating objects of type [code_annotation]. *)

       val anonCompFieldName : string
       val conditionalConversion : typ -> typ -> typ
       val find_macro : string -> Logic_ptree.lexpr
       val find_var : ?label:string -> string -> logic_var
       (** see corresponding field in {!Logic_typing.typing_context}. *)

       val find_enum_tag : string -> exp * typ
       val find_type : type_namespace -> string -> typ
       val find_comp_field: compinfo -> string -> offset
       val find_label : string -> stmt ref

       (** What to do when we have a term of type Integer in a context
           expecting a C integral type.
           @raise Failure to reject such conversion
           @since Nitrogen-20111001
       *)
       val integral_cast: Cil_types.typ -> Cil_types.term -> Cil_types.term

       (** raises an error at the given location and with the given message.
           @since Magnesium-20151001 *)
       val error: location -> ('a,Format.formatter,unit, 'b) format4 -> 'a

       (** see {!Logic_typing.typing_context}. *)
       val on_error: ('a -> 'b) -> ((location * string) -> unit) -> 'a -> 'b

     end) : S


(** append the Old and Post labels in the environment *)
val append_old_and_post_labels: Lenv.t -> Lenv.t

(** appends the Here label in the environment *)
val append_here_label: Lenv.t -> Lenv.t

(** appends the "Pre" label in the environment *)
val append_pre_label: Lenv.t -> Lenv.t

(** returns the builtin label corresponding to the given name if it exists
    @since 29.0-Copper
*)
val builtin_label: string -> logic_builtin_label option

(** adds a given variable in local environment. *)
val add_var: string -> logic_var -> Lenv.t -> Lenv.t

(** add [\result] in the environment. *)
val add_result: Lenv.t -> logic_type -> Lenv.t

(** enter a given post-state. *)
val enter_post_state: Lenv.t -> termination_kind -> Lenv.t

(** enter a given post-state and put [\result] in the env.
    NB: if the kind of the post-state is neither [Normal] nor [Returns],
    this is not a normal ACSL environment. Use with caution.
*)
val post_state_env: termination_kind -> logic_type -> Lenv.t


(** {2 Internal use} *)

val set_extension_handler:
  is_extension:(plugin:string -> string -> bool) ->
  typer: (plugin:string -> string -> typing_context -> location ->
          Logic_ptree.lexpr list -> (bool * acsl_extension_kind)) ->
  typer_block:(plugin:string -> string -> typing_context -> location ->
               string * Logic_ptree.extended_decl list ->
               bool * Cil_types.acsl_extension_kind) ->
  importer: (plugin:string -> string -> module_builder -> location ->
             string list -> unit) ->
  unit
(** Used to setup references related to the handling of ACSL extensions.
    @since 21.0-Scandium
    @before 30.0-Zinc functions did not take a [plugin:string] parameter and
    the function [importer] did not exists.
*)
[@@alert acsl_extension_handler
    "This function can only be called by Acsl_extension"]

(** Type the given extension.
    @before 30.0-Zinc the function took one less argument, [plugin], which is
    now used to avoid ambiguity if plugins use the same name for an extension
*)
val get_typer :
  plugin:string ->
  string ->
  typing_context:typing_context ->
  loc:location ->
  Logic_ptree.lexpr list -> bool * Cil_types.acsl_extension_kind

(** Type the given extension block.
    @before 30.0-Zinc the function took one less argument, [plugin], which is
    now used to avoid ambiguity if plugins use the same name for an extension
*)
val get_typer_block:
  plugin:string ->
  string ->
  typing_context:typing_context ->
  loc:Logic_ptree.location ->
  string * Logic_ptree.extended_decl list ->
  bool * Cil_types.acsl_extension_kind

(** Load the given module importer extension.
    @since 30.0-Zinc
*)
val get_importer:
  plugin:string ->
  string ->
  builder:module_builder ->
  loc:Logic_ptree.location ->
  string list -> unit

(** / **)
