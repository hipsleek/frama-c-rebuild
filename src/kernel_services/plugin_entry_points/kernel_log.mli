(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This modules creates Kernel Logs manually instead of using the one created
    via the {!Plugin} modules. It is required for {!Cmdline} which cannot depend
    on {!Plugin}. It also includes all Kernel debug and warning keys. Unless
    you cannot depend on Frama-C's kernel, always prefer using {!Kernel}
    instead of {!Kernel_log}.
    @since 33.0-Arsenic
*)

module Debug_level: Log.Level

module Verbose_level: Log.Level

val kernel_debug_atleast_ref: (int -> bool) ref
[@@alert kernel_log "Use only in Plugin."]

val kernel_verbose_atleast_ref: (int -> bool) ref
[@@alert kernel_log "Use only in Plugin."]

val kernel_channel_name: string
(** the reserved channel name used by the Frama-C kernel. *)

val kernel_label_name: string
(** the reserved label name used by the Frama-C kernel. *)

include Log.Messages

(* ************************************************************************* *)
(** {2 Message and warning categories} *)
(* ************************************************************************* *)

(** Fclib dkeys *)

val dkey_hptmap: category

val dkey_task: category
(** @before 33.0-Arsenic Was in Task library *)

val dkey_project: category

(** Fclib wkeys *)

val wkey_project: warn_category

(* Kernel dkeys *)

val dkey_acsl_extension: category

val dkey_alpha: category

val dkey_alpha_undo: category

val dkey_approximation: category

val dkey_asm_contracts: category

val dkey_ast: category

val dkey_attrs: category
(** Display debug information related to attributes in Frama-C. *)

val dkey_builtins: category

val dkey_check: category

val dkey_cil_builder: category

val dkey_cmdline: category

val dkey_comments: category

val dkey_compilation_db: category

val dkey_constfold: category

val dkey_dataflow: category

val dkey_dataflow_scc: category

val dkey_dominators: category

val dkey_dyncalls: category

val dkey_dynlink: category

val dkey_emitter: category

val dkey_emitter_clear: category

val dkey_exn_flow: category

val dkey_file_annot: category

val dkey_file_print_one: category

val dkey_file_transform: category

val dkey_file_source: category
(** Messages related to operations on files during preprocessing/parsing. *)

val dkey_filter: category

val dkey_globals: category

val dkey_inline: category

val dkey_kf_blocks: category

val dkey_linker: category

val dkey_linker_find: category

val dkey_loops: category

val dkey_mopsa_db: category

val dkey_mopsa_db_verbose: category

val dkey_pp: category

val dkey_pp_logic: category

val dkey_pretty_source: category

val dkey_print_attrs: category

val dkey_print_bitfields: category

val dkey_print_builtins: category

val dkey_print_c_types: category

val dkey_print_field_offsets: category

val dkey_print_imported_modules: category

val dkey_print_logic_coercions: category

val dkey_print_logic_types: category

val dkey_print_sid: category

val dkey_print_unspecified: category

val dkey_print_vid: category

val dkey_print_debug: category

val dkey_printer_too_early: category

val dkey_prop_status: category

val dkey_prop_status_emit: category

val dkey_prop_status_graph: category

val dkey_prop_status_merge: category

val dkey_prop_status_reg: category

val dkey_referenced: category

val dkey_rmtmps: category

val dkey_typing_cast: category

val dkey_typing_chunk: category

val dkey_typing_global: category

val dkey_typing_init: category

val dkey_typing_pragma: category

val dkey_ulevel: category

val dkey_variadic: category

val dkey_visitor: category

(* Kernel wkeys *)

val wkey_acsl_extension: warn_category

val wkey_acsl_float_compare: warn_category

val wkey_alignof_bitfield: warn_category
(** @since 32.0-Germanium *)

val wkey_annot_error: warn_category
(** error in annotation. If only a warning, annotation will just be ignored. *)

val wkey_asm: warn_category
(** Warnings related to assembly code. *)

val wkey_attrs: warn_category
(** Warning related to the handling of attributes in Frama-C. *)

val wkey_audit: warn_category
(** Warning related to options '-audit-*'. *)

val wkey_c11: warn_category
(** Warnings related to usage of C11-specific constructions. *)

val wkey_cert_exp_10: warn_category

val wkey_cert_exp_46: warn_category

val wkey_cert_msc_37: warn_category

val wkey_cert_msc_38: warn_category

val wkey_check_volatile: warn_category

val wkey_cmdline: warn_category
(** Command-line related warning, e.g. for invalid options given by the user *)

val wkey_conditional_feature: warn_category
(** parsing feature that is only supported in specific modes (e.g. C11, gcc, ...). *)

val wkey_decimal_float: warn_category

val wkey_drop_unused: warn_category

val wkey_extension_unknown: warn_category
(** Warning related to the use of an unregistered ACSL extension.
    @since 29.0-Copper
*)

val wkey_file_not_found: warn_category
(** Warnings related to missing files during preprocessing/parsing. *)

val wkey_format: warn_category

val wkey_ghost_already_ghost: warn_category
(** ghost element is qualified with \ghost while this is already the case by default *)

val wkey_ghost_bad_use: warn_category
(** error in ghost code *)

val wkey_implicit_conv_void_ptr: warn_category

val wkey_implicit_function_declaration: warn_category

val wkey_implicit_int: warn_category

val wkey_incompatible_pointer_types: warn_category

val wkey_incompatible_types_call: warn_category

val wkey_inconsistent_specifier: warn_category

val wkey_initializer_overrides: warn_category

val wkey_inline: warn_category

val wkey_int_conversion: warn_category

val wkey_jcdb: warn_category

val wkey_large_array: warn_category

val wkey_libc: warn_category

val wkey_libc_framac: warn_category

val wkey_line_directive: warn_category
(** Warnings related to unknown line directives. *)

val wkey_linker_weak: warn_category

val wkey_long_double : warn_category
(** Warning emitted by plugins that do not support the long double format. *)

val wkey_merge_conversion: warn_category

val wkey_missing_spec: warn_category

val wkey_mopsa_db: warn_category

val wkey_mopsa_db_missing_library: warn_category

val wkey_mopsa_db_non_c: warn_category

val wkey_multi_from: warn_category

val wkey_no_proto: warn_category

val wkey_parser_unsupported: warn_category
(** Warning related to unsupported parsing-related features. *)

val wkey_parser_unsupported_attributes: warn_category
(** Warning related to unsupported attributes during parsing. *)

val wkey_parser_unsupported_pragma: warn_category
(** Warning related to unsupported _Pragma's during parsing. *)

val wkey_plugin_not_loaded: warn_category
(** Warning related to not loaded plugins.
    @since 29.0-Copper
*)

val wkey_prototype: warn_category

val wkey_transient: warn_category

val wkey_typing: warn_category

val wkey_unknown_attribute: warn_category
(** Warning emitted when an unknown attribute is encountered during parsing. *)

val wkey_unnamed_typedef: warn_category
(** Warning related to "unnamed typedef that does not introduce a struct or
    enumeration type".
*)

val wkey_variadic_format_nonliteral: warn_category
