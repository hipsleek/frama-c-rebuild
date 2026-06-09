(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(** Printers for the Cabs AST *)
val version : string

val printLn : bool ref
val printLnComment : bool ref
val printCounters : bool ref
val printComments : bool ref

val get_operator : Cabs.expression -> (string * int)

val print_specifiers : Format.formatter -> Cabs.specifier -> unit
val print_type_spec : Format.formatter -> Cabs.typeSpecifier -> unit
val print_struct_name_attr :
  string -> Format.formatter -> (string * Cabs.attribute list) -> unit
val print_decl : string -> Format.formatter -> Cabs.decl_type -> unit
val print_fields : Format.formatter -> Cabs.field_group list -> unit
val print_enum_items : Format.formatter -> Cabs.enum_item list -> unit
val print_onlytype : Format.formatter -> Cabs.specifier * Cabs.decl_type -> unit
val print_name : Format.formatter -> Cabs.name -> unit
val print_init_name : Format.formatter -> Cabs.init_name -> unit
val print_name_group : Format.formatter -> Cabs.name_group -> unit
val print_field_group : Format.formatter -> Cabs.field_group -> unit
val print_field : Format.formatter -> Cabs.name * Cabs.expression option -> unit
val print_init_name_group : Format.formatter -> Cabs.init_name_group -> unit
val print_single_name : Format.formatter -> Cabs.single_name -> unit
val print_params : Format.formatter -> (Cabs.single_name list * bool) -> unit
val print_init_expression : Format.formatter -> Cabs.init_expression -> unit
val print_expression : Format.formatter -> Cabs.expression -> unit
val print_expression_level : int -> Format.formatter -> Cabs.expression -> unit
val print_statement : Format.formatter -> Cabs.statement -> unit
val print_block : Format.formatter -> Cabs.block -> unit
val print_attribute : Format.formatter -> Cabs.attribute -> unit
val print_attributes : Format.formatter -> Cabs.attribute list -> unit
val print_defs : Format.formatter -> (bool*Cabs.definition) list -> unit
val print_def : Format.formatter -> Cabs.definition -> unit

val printFile : Format.formatter -> Cabs.file -> unit
