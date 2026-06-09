(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


open Cabs
open Format

val pp_cabsloc : formatter -> cabsloc -> unit
[@@deprecated "Use Cabs.pp_cabsloc instead."]
[@@migrate { repl = Cabs.pp_cabsloc } ]

val pp_storage : formatter -> storage -> unit
[@@deprecated "Use Cabs.pp_storage instead."]
[@@migrate { repl = Cabs.pp_storage } ]

val pp_cvspec : formatter -> cvspec -> unit
[@@deprecated "Use Cabs.pp_cvspec instead."]
[@@migrate { repl = Cabs.pp_cvspec } ]

val pp_const : formatter -> constant -> unit
[@@deprecated "Use Cabs.pp_constant instead."]
[@@migrate { repl = Cabs.pp_constant } ]

val pp_labels : formatter -> string list -> unit
[@@deprecated "Use standard printers instead."]

val pp_typeSpecifier : formatter -> typeSpecifier -> unit
[@@deprecated "Use Cabs.pp_typeSpecifier instead."]
[@@migrate { repl = Cabs.pp_typeSpecifier } ]

val pp_spec_elem : formatter -> spec_elem -> unit
[@@deprecated "Use Cabs.pp_spec_elem instead."]
[@@migrate { repl = Cabs.pp_spec_elem } ]

val pp_spec : formatter -> specifier -> unit
[@@deprecated "Use Cabs.pp_specifier instead."]
[@@migrate { repl = Cabs.pp_specifier } ]

val pp_decl_type : formatter -> decl_type -> unit
[@@deprecated "Use Cabs.pp_decl_type instead."]
[@@migrate { repl = Cabs.pp_decl_type } ]

val pp_name_group : formatter -> name_group -> unit
[@@deprecated "Use Cabs.pp_name_group instead."]
[@@migrate { repl = Cabs.pp_name_group } ]

val pp_field_group : formatter -> field_group -> unit
[@@deprecated "Use Cabs.pp_field_group instead."]
[@@migrate { repl = Cabs.pp_field_group } ]

val pp_field_groups : formatter -> field_group list -> unit
[@@deprecated "Use standard printers instead."]

val pp_init_name_group : formatter -> init_name_group -> unit
[@@deprecated "Use Cabs.pp_init_name_group instead."]
[@@migrate { repl = Cabs.pp_init_name_group } ]

val pp_name : formatter -> name -> unit
[@@deprecated "Use Cabs.pp_name instead."]
[@@migrate { repl = Cabs.pp_name } ]

val pp_init_name : formatter -> init_name -> unit
[@@deprecated "Use Cabs.pp_init_name instead."]
[@@migrate { repl = Cabs.pp_init_name } ]

val pp_single_name : formatter -> single_name -> unit
[@@deprecated "Use Cabs.pp_single_name instead."]
[@@migrate { repl = Cabs.pp_single_name } ]

val pp_enum_item : formatter -> enum_item -> unit
[@@deprecated "Use Cabs.pp_enum_item instead."]
[@@migrate { repl = Cabs.pp_enum_item } ]

val pp_def : formatter -> definition -> unit
[@@deprecated "Use Cabs.pp_definition instead."]
[@@migrate { repl = Cabs.pp_definition } ]

val pp_block : formatter -> block -> unit
[@@deprecated "Use Cabs.pp_block instead."]
[@@migrate { repl = Cabs.pp_block } ]

val pp_raw_stmt : formatter -> raw_statement -> unit
[@@deprecated "Use Cabs.pp_raw_statement instead."]
[@@migrate { repl = Cabs.pp_raw_statement } ]

val pp_stmt : formatter -> statement -> unit
[@@deprecated "Use Cabs.pp_statement instead."]
[@@migrate { repl = Cabs.pp_statement } ]

val pp_for_clause : formatter -> for_clause -> unit
[@@deprecated "Use Cabs.pp_for_clause instead."]
[@@migrate { repl = Cabs.pp_for_clause } ]

val pp_bin_op : formatter -> binary_operator -> unit
[@@deprecated "Use Cabs.pp_binary_operator instead."]
[@@migrate { repl = Cabs.pp_binary_operator } ]

val pp_un_op : formatter -> unary_operator -> unit
[@@deprecated "Use Cabs.pp_unary_operator instead."]
[@@migrate { repl = Cabs.pp_unary_operator } ]

val pp_exp : formatter -> expression -> unit
[@@deprecated "Use Cabs.pp_expression instead."]
[@@migrate { repl = Cabs.pp_expression } ]

val pp_exp_node : formatter -> cabsexp -> unit
[@@deprecated "Use Cabs.pp_cabsexp instead."]
[@@migrate { repl = Cabs.pp_cabsexp } ]

val pp_init_exp : formatter -> init_expression -> unit
[@@deprecated "Use Cabs.pp_init_expression instead."]
[@@migrate { repl = Cabs.pp_init_expression } ]

val pp_initwhat : formatter -> initwhat -> unit
[@@deprecated "Use Cabs.pp_initwhat instead."]
[@@migrate { repl = Cabs.pp_initwhat } ]

val pp_attr : formatter -> attribute -> unit
[@@deprecated "Use Cabs.pp_attr instead."]
[@@migrate { repl = Cabs.pp_attr } ]

val pp_attrs : formatter -> attribute list -> unit
[@@deprecated "Use standard printers instead."]

val pp_file : formatter -> file -> unit
[@@deprecated "Use Cabs.pp_file instead."]
[@@migrate { repl = Cabs.pp_file } ]
