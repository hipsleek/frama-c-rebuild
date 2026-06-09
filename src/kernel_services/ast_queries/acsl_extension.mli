(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** ACSL extensions registration module
    @since 21.0-Scandium
*)

open Cil_types
open Logic_typing
open Logic_ptree

(** Untyped ACSL extensions transformers *)
type extension_preprocessor =
  lexpr list -> lexpr list

(** Transformers from untyped to typed ACSL extension *)
type extension_typer =
  typing_context -> location -> lexpr list -> acsl_extension_kind

(** Visitor functions for ACSL extensions *)
type extension_visitor =
  Cil.cilVisitor -> acsl_extension_kind -> acsl_extension_kind Cil.visitAction

type extension_preprocessor_block =
  string * extended_decl list -> string * extended_decl list

type extension_typer_block =
  typing_context -> location -> string * extended_decl list -> acsl_extension_kind

type extension_module_importer =
  module_builder -> location -> string list -> unit

(** Pretty printers for ACSL extensions *)
type extension_printer =
  Printer_api.extensible_printer_type -> Format.formatter ->
  acsl_extension_kind -> unit

(** type of functions that compare two extensions (with the same keyword)
    to decide if they're identical or not. See {!Ast_diff} for more information.
    @since 29.0-Copper
*)
type extension_same =
  acsl_extension_kind -> acsl_extension_kind -> Ast_diff.is_same_env -> bool

(** type of functions that register new ACSL extensions to be used in place of
    various kinds of ACSL annotations.

    The labelled parameter [plugin] is used to specify which plugin registers
    this new extension. It can be used, together with the syntax
    [\plugin::name _] in ACSL annotations (instead of just [name _]), to do some
    verifications and get better warnings/errors messages.

    The optional [preprocessor] is a function to be applied by the parser on the
    untyped content of the extension before parsing the rest of the processed
    file. By default, this function is the identity.

    The [typer] is applied when transforming the untyped AST to Cil. It receives
    the typing context of the annotation that can be used to type the received
    logic expressions (see [Logic_typing.typing_context]), and the location of
    the annotation.

    The optional [visitor] allows changing the way the ACSL extension is visited.
    By default, the behavior is [Cil.DoChildren], which ends up visiting
    each identified predicate/term in the list and leave the id as is.

    The optional [printer] allows changing the way the ACSL extension is pretty
    printed. By default, it prints the name of the extension followed by the
    formatted predicates, terms or identifier according to the
    [Cil_types.acsl_extension_kind].

    The optional [short_printer] allows changing the [Printer.pp_short_extended]
    behavior for the ACSL extension. By default, it just prints the [name]. It
    is for example used for the filetree in the GUI.

    The optional [is_same_ext] parameter allows checking whether two versions
    of the extended annotation are identical or not during a run of
    {!Ast_diff.compare_ast}. By default, [Ast_diff] will compare the lists
    of terms/predicates.

    The [status] indicates whether the extension can be assigned a property
    status or not.

    Here is a basic example:
    {[
      let count = ref 0
      let foo_typer ctxt loc = function
        | [] ->
          let id = !count in incr count;
          Ext_id id
        | [p] ->
          Ext_preds [ctxt.type_predicate ctxt (ctxt.post_state [Normal]) p]
        | _ ->
          typing_context.error loc "expecting a predicate after keyword FOO"

      let () =
        Acsl_extension.register_behavior ~plugin:"myplugin" "FOO" foo_typer false
    ]}
    @before 29.0-Copper parameters [plugin] and [is_same_ext] were not present
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)
type register_extension =
  plugin:string -> string ->
  ?preprocessor:extension_preprocessor -> extension_typer ->
  ?visitor:extension_visitor ->
  ?printer:extension_printer -> ?short_printer:extension_printer ->
  ?is_same_ext:extension_same -> bool ->
  unit

(** same as {!register_extension}, but for extensions that parse an axiomatic
    block, resulting in a {!Cil_types.Ext_annot}. *)
type register_extension_block =
  plugin:string -> string ->
  ?preprocessor:extension_preprocessor_block -> extension_typer_block ->
  ?visitor:extension_visitor ->
  ?printer:extension_printer -> ?short_printer:extension_printer ->
  ?is_same_ext:extension_same -> bool -> unit

(** Register a behavior extension, i.e. new clauses of contracts *)
val register_behavior: register_extension

(** Register extension for global annotation. *)
val register_global: register_extension

(** Registers extension for global block annotation. *)
val register_global_block: register_extension_block

(** Registers extension for code annotation to be evaluated at _current_
    program point. *)
val register_code_annot: register_extension

(** Registers extension for code annotation to be evaluated for the _next_
    statement. *)
val register_code_annot_next_stmt: register_extension

(** Registers extension for loop annotation. *)
val register_code_annot_next_loop: register_extension

(** Registers extension both for code and loop annotations. *)
val register_code_annot_next_both: register_extension

(**
   Module importer extensions allow extending the import clause with external
   loaders. For instance, consider the following declaration:
   {[
     //@ import \myplugin::A: foo::bar;
   ]}

   This import clause will invoke an external module importer named ["A"]
   provided it has been properly registered by plugin [myplugin].

   A module importer extension is a function that receives a [module_builder]
   parameter to be populated with contents of the module. The module name is
   provided as list (See {!Logic_utils.longident} for details).

   New type and function symbols shall be created with `Cil.make_xxx` functions.
   The registered symbols {i will} be automatically prefixed with the name of
   the imported module if necessary.

   The register module importer function might be invoked several times,
   typically when a given module is imported from several files. Although
   external module importers might use memoization internally, the provided
   module builder shall be populated on every call.
*)
val register_module_importer :
  plugin:string -> string -> extension_module_importer -> unit
