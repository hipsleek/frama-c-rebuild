(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


let kernel_channel_name = "kernel"
let kernel_label_name = "kernel"

module Debug_level = Log.Make_level(struct let default = 0 end)
module Verbose_level = Log.Make_level(struct let default = 1 end)

let kernel_debug_atleast_ref   = ref (fun n -> Debug_level.get () >= n)
let kernel_verbose_atleast_ref = ref (fun n -> Verbose_level.get () >= n)

include Log.Register
    (struct
      let channel = kernel_channel_name
      let label = kernel_label_name
      let debug_atleast level = !kernel_debug_atleast_ref level
      let verbose_atleast level = !kernel_verbose_atleast_ref level
    end)

(* Fclib dkeys and wkeys *)

let dkey_hptmap =
  let help = "prints debug information about Hptmaps" in
  register_category ~help "hptmap"

let dkey_task =
  let help = "prints debug information about task executions" in
  register_category ~help "task"

let wkey_project = register_warn_category "project"

(* Kernel dkeys *)

let dkey_acsl_extension =
  register_category "acsl-extension"
    ~help:"print a message when registering a new ACSL extension"

let dkey_alpha =
  register_category ~help:"alpha conversion module (parsing)" "alpha"

let dkey_alpha_undo =
  register_category ~help:"undoing alpha conversion (parsing)" "alpha:undo"

let dkey_approximation =
  register_category "approximation" ~default:true
    ~help:"messages emitted when imprecise approximations are performed"

let dkey_asm_contracts =
  register_category ~help:"inline assembly contracts" "asm:contracts"

let dkey_ast =
  register_category ~help:"prints the AST just after Ast.compute" "ast"

let dkey_attrs =
  register_category
    ~help:"displays some info related to the handling of attributes"
    "attrs"

let dkey_builtins =
  register_category ~help:"Cil builtins" "builtins"

let dkey_check = register_category "check"

let dkey_cil_builder =
  register_category
    ~help:"displays information about cil_builder internal state"
    "cil-builder"

let dkey_cmdline = register_category "cmdline"

let dkey_comments = register_category "parser:comments"

let dkey_compilation_db = register_category "pp:compilation-db"

let dkey_constfold = register_category "constfold"

let dkey_dataflow = register_category "dataflow"

let dkey_dataflow_scc = register_category "dataflow:scc"

let dkey_dominators = register_category "dominators"

let dkey_dyncalls = register_category "dyncalls"

let dkey_dynlink = register_category "dynlink"

let dkey_emitter = register_category "emitter"

let dkey_emitter_clear = register_category "emitter:clear"

let dkey_exn_flow = register_category "exn_flow"

let dkey_file_annot = register_category "file:annotation"

let dkey_file_print_one = register_category "file:print-one"

let dkey_file_transform = register_category "file:transformation"

let dkey_file_source = register_category "file:source"

let dkey_filter = register_category "filter"

let dkey_globals = register_category "globals"

let dkey_inline = register_category "inline"

let dkey_kf_blocks = register_category "kf:blocks"

let dkey_linker = register_category "linker"

let dkey_linker_find = register_category "linker:find"

let dkey_loops = register_category "natural-loops"

let dkey_mopsa_db =
  register_category "mopsa-db"
    ~help:"messages related to -mopsa-db and related options"

let dkey_mopsa_db_verbose =
  register_category "mopsa-db:verbose"
    ~help:"highly verbose messages related to mopsa-db options"

let dkey_pp = register_category "pp"

let dkey_pp_logic = register_category "pp:logic"

let dkey_pretty_source = register_category "pretty-source"

let dkey_print_attrs = register_category "printer:attrs"

let dkey_print_bitfields = register_category "printer:bitfields"

let dkey_print_builtins = register_category "printer:builtins"

let dkey_print_c_types =
  register_category "printer:types:c"
    ~help:"annotate each Cil expression with its type"

let dkey_print_field_offsets = register_category "printer:field-offsets"

let dkey_print_imported_modules = register_category "printer:imported-modules"

let dkey_print_logic_coercions = register_category "printer:logic-coercions"

let dkey_print_logic_types =
  register_category "printer:types:logic"
    ~help:"annotate each logic term with its type"

let dkey_print_sid = register_category "printer:sid"

let dkey_print_unspecified = register_category "printer:unspecified"

let dkey_print_vid = register_category "printer:vid"

let dkey_print_debug =
  register_category "printer:debug"
    ~help:"print internal representation of AST nodes"

let dkey_printer_too_early =
  register_category "printer:too-early"
    ~help:"raise fatal error when Printer is used in early parsing stages, \
           where Cil_printer would be more adequate"

let dkey_project = register_category "project"

let dkey_prop_status = register_category "prop-status"

let dkey_prop_status_emit = register_category "prop-status:emit"

let dkey_prop_status_graph = register_category "prop-status:graph"

let dkey_prop_status_merge = register_category "prop-status:merge"

let dkey_prop_status_reg = register_category "prop-status:register"

let dkey_referenced = register_category "parser:referenced"

let dkey_rmtmps = register_category "parser:rmtmps"

let dkey_typing_cast = register_category "typing:cast"

let dkey_typing_chunk = register_category "typing:chunk"

let dkey_typing_global = register_category "typing:global"

let dkey_typing_init = register_category "typing:initializer"

let dkey_typing_pragma = register_category "typing:pragma"

let dkey_ulevel = register_category "ulevel"

let dkey_variadic = register_category "variadic"

let dkey_visitor = register_category "visitor"

(* Kernel wkeys *)

let wkey_acsl_extension = register_warn_category "acsl-extension"

let wkey_acsl_float_compare = register_warn_category "acsl-float-compare"
let () = set_warn_status wkey_acsl_float_compare Log.Winactive

let wkey_alignof_bitfield =
  register_warn_category "typing:alignof-bitfield"
    ~help:"warning related to use of alignof on bitfield storage"

let wkey_annot_error = register_warn_category "annot-error"
let () = set_warn_status wkey_annot_error Log.Wabort

let wkey_asm = register_warn_category "asm:clobber"

let wkey_attrs =
  register_warn_category
    ~help:"Warnings related to the handling of attributes in Frama-C"
    "attrs"

let wkey_audit = register_warn_category "audit"
let () = set_warn_status wkey_audit Log.Werror

let wkey_c11 = register_warn_category "c11"
let () = set_warn_status wkey_c11 Log.Winactive

let wkey_cert_exp_10 = register_warn_category "CERT:EXP:10"
let () = set_warn_status wkey_cert_exp_10 Log.Winactive

let wkey_cert_exp_46 = register_warn_category "CERT:EXP:46"

let wkey_cert_msc_37 = register_warn_category "CERT:MSC:37"

let wkey_cert_msc_38 = register_warn_category "CERT:MSC:38"
let () = set_warn_status wkey_cert_msc_38 Log.Werror

let wkey_check_volatile = register_warn_category "check:volatile"

let wkey_cmdline = register_warn_category "cmdline"

let wkey_conditional_feature =
  register_warn_category "parser:conditional-feature"
    ~default:Log.Wabort
    ~help:"parsing feature only supported in specific modes: \
           C11, a GCC-based machdep, etc"

let wkey_decimal_float = register_warn_category "parser:decimal-float"
let () = set_warn_status wkey_decimal_float Log.Wonce

let wkey_drop_unused = register_warn_category "linker:drop-conflicting-unused"

let wkey_extension_unknown = register_warn_category "extension-unknown"
let () = set_warn_status wkey_extension_unknown Log.Werror

let wkey_file_not_found = register_warn_category "file:not-found"
let () = set_warn_status wkey_file_not_found Log.Wfeedback

let wkey_format = register_warn_category "libc:format"

let wkey_ghost_already_ghost = register_warn_category "ghost:already-ghost"
let () = set_warn_status wkey_ghost_already_ghost Log.Wfeedback

let wkey_ghost_bad_use = register_warn_category "ghost:bad-use"
let () = set_warn_status wkey_ghost_bad_use Log.Werror

let wkey_implicit_conv_void_ptr =
  register_warn_category "typing:implicit-conv-void-ptr"

let wkey_implicit_function_declaration = register_warn_category
    "typing:implicit-function-declaration"

let wkey_implicit_int = register_warn_category "typing:implicit-int"
let () = set_warn_status wkey_implicit_int Log.Werror

let wkey_incompatible_pointer_types =
  register_warn_category "typing:incompatible-pointer-types"

let wkey_incompatible_types_call =
  register_warn_category "typing:incompatible-types-call"

let wkey_inconsistent_specifier =
  register_warn_category "typing:inconsistent-specifier"

let wkey_initializer_overrides =
  register_warn_category "typing:initializer-overrides"

let wkey_inline = register_warn_category "inline"

let wkey_int_conversion =
  register_warn_category "typing:int-conversion"

let wkey_jcdb = register_warn_category "pp:compilation-db"
let () = set_warn_status wkey_jcdb Log.Wonce

let wkey_large_array = register_warn_category "too-large-array"
let () = set_warn_status wkey_large_array Log.Werror

let wkey_libc = register_warn_category "libc"

let wkey_libc_framac = register_warn_category "libc:frama-c"

let wkey_line_directive = register_warn_category "pp:line-directive"

let wkey_linker_weak = register_warn_category "linker:weak"

let wkey_long_double = register_warn_category "typing:long-double-unsupported"
let () = set_warn_status wkey_long_double Log.Wonce

let wkey_merge_conversion =
  register_warn_category "typing:merge-conversion"

let wkey_missing_spec = register_warn_category "annot:missing-spec"

let wkey_mopsa_db =
  register_warn_category "mopsa-db"
    ~help:"warnings related to option -mopsa-db"

let wkey_mopsa_db_missing_library =
  register_warn_category "mopsa-db:missing-library"
    ~default:Log.Wabort
    ~help:"warnings related to missing libraries in mopsa-db files"

let wkey_mopsa_db_non_c =
  register_warn_category "mopsa-db:non-c-source"
    ~help:"warnings related non-C sources present in a mopsa-db file"

let wkey_multi_from = register_warn_category "annot:multi-from"

let wkey_no_proto = register_warn_category "typing:no-proto"

let wkey_parser_unsupported = register_warn_category "parser:unsupported"

let wkey_parser_unsupported_attributes = register_warn_category "parser:unsupported:attributes"

let wkey_parser_unsupported_pragma = register_warn_category "parser:unsupported:pragma"

let wkey_plugin_not_loaded = register_warn_category "plugin-not-loaded"
let () = set_warn_status wkey_plugin_not_loaded Log.Wactive

let wkey_prototype = register_warn_category "prototype"

let wkey_transient = register_warn_category "transient-block"
let () = set_warn_status wkey_transient Log.Winactive

let wkey_typing = register_warn_category "typing:variadic"

let wkey_unknown_attribute =
  register_warn_category
    ~help:"Warnings emitted when encountering an unknown attribute"
    "attrs:unknown"

let wkey_unnamed_typedef = register_warn_category "parser:unnamed-typedef"

let wkey_variadic_format_nonliteral =
  register_warn_category "typing:variadic:format:nonliteral"
    ~help:("warns about scanf/printf with non-literal strings; " ^
           "similar to GCC's [-Wformat-nonliteral]")
