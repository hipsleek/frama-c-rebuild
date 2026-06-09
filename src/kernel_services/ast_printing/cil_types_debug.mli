(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val pp_list : 'a Pretty_utils.formatter -> 'a list Pretty_utils.formatter
[@@deprecated "Use standard printers instead."]

val pp_option : 'a Pretty_utils.formatter -> 'a option Pretty_utils.formatter
[@@deprecated "Use standard printers instead."]

val pp_ref : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a ref -> unit
[@@deprecated "Use standard printers instead."]

val pp_pair :
  'a Pretty_utils.formatter ->
  'b Pretty_utils.formatter -> ('a * 'b) Pretty_utils.formatter
[@@deprecated "Use standard printers instead."]

val pp_tuple3 :
  ?pre:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  ?sep:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?suf:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  (Format.formatter -> 'm -> unit) ->
  (Format.formatter -> 'n -> unit) ->
  (Format.formatter -> 'o -> unit) ->
  Format.formatter -> 'm * 'n * 'o -> unit
[@@deprecated "Use standard printers instead."]

val pp_tuple4 :
  ?pre:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  ?sep:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?suf:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  (Format.formatter -> 'm -> unit) ->
  (Format.formatter -> 'n -> unit) ->
  (Format.formatter -> 'o -> unit) ->
  (Format.formatter -> 'p -> unit) ->
  Format.formatter -> 'm * 'n * 'o * 'p -> unit
[@@deprecated "Use standard printers instead."]

val pp_tuple5 :
  ?pre:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  ?sep:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?suf:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  (Format.formatter -> 'm -> unit) ->
  (Format.formatter -> 'n -> unit) ->
  (Format.formatter -> 'o -> unit) ->
  (Format.formatter -> 'p -> unit) ->
  (Format.formatter -> 'q -> unit) ->
  Format.formatter -> 'm * 'n * 'o * 'p * 'q -> unit
[@@deprecated "Use standard printers instead."]

val pp_integer : Format.formatter -> Z.t -> unit
[@@deprecated "Use Z.pp instead."]
[@@migrate { repl = Z.pp } ]

val pp_int64 : Format.formatter -> int64 -> unit
[@@deprecated "Use Int64.to_string and print as string instead."]
[@@migrate { repl = fun fmt i -> Format.pp_print_string fmt (Int64.to_string i)} ]

val pp_string : Format.formatter -> string -> unit
[@@deprecated "Use Format.pp_print_string instead."]
[@@migrate { repl = Format.pp_print_string } ]

val pp_bool : Format.formatter -> bool -> unit
[@@deprecated "Use Format.pp_print_bool instead."]
[@@migrate { repl = Format.pp_print_bool } ]

val pp_int : Format.formatter -> int -> unit
[@@deprecated "Use Format.pp_print_int instead."]
[@@migrate { repl = Format.pp_print_int } ]

val pp_char : Format.formatter -> char -> unit
[@@deprecated "Use Format.pp_print_char instead."]
[@@migrate { repl = Format.pp_print_char } ]

val pp_float : Format.formatter -> float -> unit
[@@deprecated "Use Format.pp_print_float instead."]
[@@migrate { repl = Format.pp_print_float } ]

val pp_variant : Cil_types.variant Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_variant instead."]
[@@migrate { repl = Cil_types.pp_variant } ]

val pp_allocation : Format.formatter -> Cil_types.allocation -> unit
[@@deprecated "Use Cil_types.pp_allocation instead."]
[@@migrate { repl = Cil_types.pp_allocation } ]

val pp_deps : Format.formatter -> Cil_types.deps -> unit
[@@deprecated "Use Cil_types.pp_deps instead."]
[@@migrate { repl = Cil_types.pp_deps } ]

val pp_from : (Cil_types.identified_term * Cil_types.deps) Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_from instead."]
[@@migrate { repl = Cil_types.pp_from } ]

val pp_assigns : Format.formatter -> Cil_types.assigns -> unit
[@@deprecated "Use Cil_types.pp_assigns instead."]
[@@migrate { repl = Cil_types.pp_assigns } ]

val pp_file : Format.formatter -> Cil_types.file -> unit
[@@deprecated "Use Cil_types.pp_file instead."]
[@@migrate { repl = Cil_types.pp_file } ]

val pp_global : Format.formatter -> Cil_types.global -> unit
[@@deprecated "Use Cil_types.pp_global instead."]
[@@migrate { repl = Cil_types.pp_global } ]

val pp_typ_node : Cil_types.typ_node Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_typ_node instead."]
[@@migrate { repl = Cil_types.pp_typ_node } ]

val pp_typ : Cil_types.typ Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_typ instead."]
[@@migrate { repl = Cil_types.pp_typ } ]

val pp_ikind : Format.formatter -> Cil_types.ikind -> unit
[@@deprecated "Use Cil_types.pp_ikind instead."]
[@@migrate { repl = Cil_types.pp_ikind } ]

val pp_fkind : Format.formatter -> Cil_types.fkind -> unit
[@@deprecated "Use Cil_types.pp_fkind instead."]
[@@migrate { repl = Cil_types.pp_fkind } ]

val pp_attribute : Cil_types.attribute Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_attribute instead."]
[@@migrate { repl = Cil_types.pp_attribute } ]

val pp_attributes : Format.formatter -> Cil_types.attributes -> unit
[@@deprecated "Use Cil_types.pp_attributes instead."]
[@@migrate { repl = Cil_types.pp_attributes } ]

val pp_attrparam : Cil_types.attrparam Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_attrparam instead."]
[@@migrate { repl = Cil_types.pp_attrparam } ]

val pp_compinfo : Format.formatter -> Cil_types.compinfo -> unit
[@@deprecated "Use Cil_types.pp_compinfo instead."]
[@@migrate { repl = Cil_types.pp_compinfo } ]

val pp_fieldinfo : Format.formatter -> Cil_types.fieldinfo -> unit
[@@deprecated "Use Cil_types.pp_fieldinfo instead."]
[@@migrate { repl = Cil_types.pp_fieldinfo } ]

val pp_enuminfo : Format.formatter -> Cil_types.enuminfo -> unit
[@@deprecated "Use Cil_types.pp_enuminfo instead."]
[@@migrate { repl = Cil_types.pp_enuminfo } ]

val pp_enumitem : Format.formatter -> Cil_types.enumitem -> unit
[@@deprecated "Use Cil_types.pp_enumitem instead."]
[@@migrate { repl = Cil_types.pp_enumitem } ]

val pp_typeinfo : Format.formatter -> Cil_types.typeinfo -> unit
[@@deprecated "Use Cil_types.pp_typeinfo instead."]
[@@migrate { repl = Cil_types.pp_typeinfo } ]

val pp_varinfo : Cil_types.varinfo Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_varinfo instead."]
[@@migrate { repl = Cil_types.pp_varinfo } ]

val pp_storage : Format.formatter -> Cil_types.storage -> unit
[@@deprecated "Use Cil_types.pp_storage instead."]
[@@migrate { repl = Cil_types.pp_storage } ]

val pp_exp : Cil_types.exp Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_exp instead."]
[@@migrate { repl = Cil_types.pp_exp } ]

val pp_exp_node : Format.formatter -> Cil_types.exp_node -> unit
[@@deprecated "Use Cil_types.pp_exp_node instead."]
[@@migrate { repl = Cil_types.pp_exp_node } ]

val pp_constant : Format.formatter -> Cil_types.constant -> unit
[@@deprecated "Use Cil_types.pp_constant instead."]
[@@migrate { repl = Cil_types.pp_constant } ]

val pp_unop : Format.formatter -> Cil_types.unop -> unit
[@@deprecated "Use Cil_types.pp_unop instead."]
[@@migrate { repl = Cil_types.pp_unop } ]

val pp_binop : Format.formatter -> Cil_types.binop -> unit
[@@deprecated "Use Cil_types.pp_binop instead."]
[@@migrate { repl = Cil_types.pp_binop } ]

val pp_lval : Cil_types.lval Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_lval instead."]
[@@migrate { repl = Cil_types.pp_lval } ]

val pp_lhost : Cil_types.lhost Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_lhost instead."]
[@@migrate { repl = Cil_types.pp_lhost } ]

val pp_offset : Cil_types.offset Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_offset instead."]
[@@migrate { repl = Cil_types.pp_offset } ]

val pp_init : Cil_types.init Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_init instead."]
[@@migrate { repl = Cil_types.pp_init } ]

val pp_initinfo : Format.formatter -> Cil_types.initinfo -> unit
[@@deprecated "Use Cil_types.pp_initinfo instead."]
[@@migrate { repl = Cil_types.pp_initinfo } ]

val pp_fundec : Format.formatter -> Cil_types.fundec -> unit
[@@deprecated "Use Cil_types.pp_fundec instead."]
[@@migrate { repl = Cil_types.pp_fundec } ]

val pp_block : Cil_types.block Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_block instead."]
[@@migrate { repl = Cil_types.pp_block } ]

val pp_stmt : Cil_types.stmt Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_stmt instead."]
[@@migrate { repl = Cil_types.pp_stmt } ]

val pp_label : Format.formatter -> Cil_types.label -> unit
[@@deprecated "Use Cil_types.pp_label instead."]
[@@migrate { repl = Cil_types.pp_label } ]

val pp_stmtkind : Format.formatter -> Cil_types.stmtkind -> unit
[@@deprecated "Use Cil_types.pp_stmtkind instead."]
[@@migrate { repl = Cil_types.pp_stmtkind } ]

val pp_catch_binder : Cil_types.catch_binder Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_catch_binder instead."]
[@@migrate { repl = Cil_types.pp_catch_binder } ]

val pp_instr : Cil_types.instr Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_instr instead."]
[@@migrate { repl = Cil_types.pp_instr } ]

val pp_extended_asm : Cil_types.extended_asm Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_extended_asm instead."]
[@@migrate { repl = Cil_types.pp_extended_asm } ]

val pp_filepath_position : Format.formatter -> Filepos.t -> unit
[@@deprecated "Use Filepath.pp instead."]
[@@migrate { repl = Filepath.pp } ]

val pp_lexing_position : Format.formatter -> Lexing.position -> unit
[@@deprecated "Use standard printers instead."]

val pp_location : Format.formatter -> Cil_types.location -> unit
[@@deprecated "Use Cil_types.pp_location instead."]
[@@migrate { repl = Cil_types.pp_location } ]

val pp_logic_constant : Format.formatter -> Cil_types.logic_constant -> unit
[@@deprecated "Use Cil_types.pp_logic_constant instead."]
[@@migrate { repl = Cil_types.pp_logic_constant } ]

val pp_logic_real : Format.formatter -> Cil_types.logic_real -> unit
[@@deprecated "Use Cil_types.pp_logic_real instead."]
[@@migrate { repl = Cil_types.pp_logic_real } ]

val pp_logic_type : Cil_types.logic_type Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_logic_type instead."]
[@@migrate { repl = Cil_types.pp_logic_type } ]

val pp_identified_term : Cil_types.identified_term Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_identified_term instead."]
[@@migrate { repl = Cil_types.pp_identified_term } ]

val pp_logic_label : Cil_types.logic_label Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_logic_label instead."]
[@@migrate { repl = Cil_types.pp_logic_label } ]

val pp_logic_builtin_label : Cil_types.logic_builtin_label Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_logic_builtin_label instead."]
[@@migrate { repl = Cil_types.pp_logic_builtin_label } ]

val pp_term : Cil_types.term Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_term instead."]
[@@migrate { repl = Cil_types.pp_term } ]

val pp_term_node : Format.formatter -> Cil_types.term_node -> unit
[@@deprecated "Use Cil_types.pp_term_node instead."]
[@@migrate { repl = Cil_types.pp_term_node } ]

val pp_term_lval : Format.formatter -> Cil_types.term_lval -> unit
[@@deprecated "Use Cil_types.pp_term_lval instead."]
[@@migrate { repl = Cil_types.pp_term_lval } ]

val pp_term_lhost : Cil_types.term_lhost Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_term_lhost instead."]
[@@migrate { repl = Cil_types.pp_term_lhost } ]

val pp_model_info : Format.formatter -> Cil_types.model_info -> unit
[@@deprecated "Use Cil_types.pp_model_info instead."]
[@@migrate { repl = Cil_types.pp_model_info } ]

val pp_term_offset : Cil_types.term_offset Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_term_offset instead."]
[@@migrate { repl = Cil_types.pp_term_offset } ]

val pp_logic_info : Format.formatter -> Cil_types.logic_info -> unit
[@@deprecated "Use Cil_types.pp_logic_info instead."]
[@@migrate { repl = Cil_types.pp_logic_info } ]

val pp_builtin_logic_info : Format.formatter -> Cil_types.builtin_logic_info -> unit
[@@deprecated "Use Cil_types.pp_builtin_logic_info instead."]
[@@migrate { repl = Cil_types.pp_builtin_logic_info } ]

val pp_logic_body : Format.formatter -> Cil_types.logic_body -> unit
[@@deprecated "Use Cil_types.pp_logic_body instead."]
[@@migrate { repl = Cil_types.pp_logic_body } ]

val pp_logic_type_info : Format.formatter -> Cil_types.logic_type_info -> unit
[@@deprecated "Use Cil_types.pp_logic_type_info instead."]
[@@migrate { repl = Cil_types.pp_logic_type_info } ]

val pp_logic_type_def : Format.formatter -> Cil_types.logic_type_def -> unit
[@@deprecated "Use Cil_types.pp_logic_type_def instead."]
[@@migrate { repl = Cil_types.pp_logic_type_def } ]

val pp_logic_var_kind : Format.formatter -> Cil_types.logic_var_kind -> unit
[@@deprecated "Use Cil_types.pp_logic_var_kind instead."]
[@@migrate { repl = Cil_types.pp_logic_var_kind } ]

val pp_logic_var : Cil_types.logic_var Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_logic_var instead."]
[@@migrate { repl = Cil_types.pp_logic_var } ]

val pp_logic_ctor_info : Cil_types.logic_ctor_info Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_logic_ctor_info instead."]
[@@migrate { repl = Cil_types.pp_logic_ctor_info } ]

val pp_quantifiers : Format.formatter -> Cil_types.quantifiers -> unit
[@@deprecated "Use Cil_types.pp_quantifiers instead."]
[@@migrate { repl = Cil_types.pp_quantifiers } ]

val pp_relation : Format.formatter -> Cil_types.relation -> unit
[@@deprecated "Use Cil_types.pp_relation instead."]
[@@migrate { repl = Cil_types.pp_relation } ]

val pp_predicate_node : Format.formatter -> Cil_types.predicate_node -> unit
[@@deprecated "Use Cil_types.pp_predicate_node instead."]
[@@migrate { repl = Cil_types.pp_predicate_node } ]

val pp_identified_predicate : Format.formatter -> Cil_types.identified_predicate -> unit
[@@deprecated "Use Cil_types.pp_identified_predicate instead."]
[@@migrate { repl = Cil_types.pp_identified_predicate } ]

val pp_toplevel_predicate : Format.formatter -> Cil_types.toplevel_predicate -> unit
[@@deprecated "Use Cil_types.pp_toplevel_predicate instead."]
[@@migrate { repl = Cil_types.pp_toplevel_predicate } ]

val pp_predicate : Cil_types.predicate Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_predicate instead."]
[@@migrate { repl = Cil_types.pp_predicate } ]

val pp_spec : Format.formatter -> Cil_types.spec -> unit
[@@deprecated "Use Cil_types.pp_spec instead."]
[@@migrate { repl = Cil_types.pp_spec } ]

val pp_acsl_extension : Format.formatter -> Cil_types.acsl_extension -> unit
[@@deprecated "Use Cil_types.pp_acsl_extension instead."]
[@@migrate { repl = Cil_types.pp_acsl_extension } ]

val pp_acsl_extension_kind : Format.formatter ->  Cil_types.acsl_extension_kind -> unit
[@@deprecated "Use Cil_types.pp_acsl_extension_kind instead."]
[@@migrate { repl = Cil_types.pp_acsl_extension_kind } ]

val pp_behavior : Format.formatter -> Cil_types.behavior -> unit
[@@deprecated "Use Cil_types.pp_behavior instead."]
[@@migrate { repl = Cil_types.pp_behavior } ]

val pp_termination_kind : Format.formatter -> Cil_types.termination_kind -> unit
[@@deprecated "Use Cil_types.pp_termination_kind instead."]
[@@migrate { repl = Cil_types.pp_termination_kind } ]

val pp_code_annotation_node : Format.formatter -> Cil_types.code_annotation_node -> unit
[@@deprecated "Use Cil_types.pp_code_annotation_node instead."]
[@@migrate { repl = Cil_types.pp_code_annotation_node } ]

val pp_funspec : Format.formatter -> Cil_types.funspec -> unit
[@@deprecated "Use Cil_types.pp_funspec instead."]
[@@migrate { repl = Cil_types.pp_funspec } ]

val pp_code_annotation : Cil_types.code_annotation Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_code_annotation instead."]
[@@migrate { repl = Cil_types.pp_code_annotation } ]

val pp_funbehavior : Format.formatter -> Cil_types.funbehavior -> unit
[@@deprecated "Use Cil_types.pp_funbehavior instead."]
[@@migrate { repl = Cil_types.pp_funbehavior } ]

val pp_global_annotation : Cil_types.global_annotation Pretty_utils.formatter
[@@deprecated "Use Cil_types.pp_global_annotation instead."]
[@@migrate { repl = Cil_types.pp_global_annotation } ]

val pp_kinstr : Format.formatter -> Cil_types.kinstr -> unit
[@@deprecated "Use Cil_types.pp_kinstr instead."]
[@@migrate { repl = Cil_types.pp_kinstr } ]

val pp_cil_function : Format.formatter -> Cil_types.cil_function -> unit
[@@deprecated "Use Cil_types.pp_cil_function instead."]
[@@migrate { repl = Cil_types.pp_cil_function } ]

val pp_kernel_function : Format.formatter -> Cil_types.kernel_function -> unit
[@@deprecated "Use Cil_types.pp_kernel_function instead."]
[@@migrate { repl = Cil_types.pp_kernel_function } ]

val pp_syntactic_scope : Format.formatter -> Cil_types.syntactic_scope -> unit
[@@deprecated "Use Cil_types.pp_syntactic_scope instead."]
[@@migrate { repl = Cil_types.pp_syntactic_scope } ]

