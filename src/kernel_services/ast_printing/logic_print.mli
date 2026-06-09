(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(** Pretty-printing of a parsed logic tree. *)

open Logic_ptree

val print_constant: Format.formatter -> constant -> unit

(** First arguments prints the name of identifier declared with the
    corresponding type (None for pure type. C syntax makes impossible to
    separate printing the type and the identifier in a declaration...
*)
val print_logic_type:
  (Format.formatter -> unit) option -> Format.formatter -> logic_type -> unit

val print_quantifiers: Format.formatter -> quantifiers -> unit

val print_lexpr: Format.formatter -> lexpr -> unit

val print_type_annot: Format.formatter -> type_annot -> unit

val print_typedef: Format.formatter -> typedef -> unit

val print_decl: Format.formatter -> decl -> unit

val print_spec: Format.formatter -> spec -> unit

val print_code_annot: Format.formatter -> code_annot -> unit

val print_assigns: Format.formatter -> assigns -> unit

val print_variant: Format.formatter -> variant -> unit
