(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types

val pp_lval : Format.formatter -> lval -> unit
val pp_lhost : Format.formatter -> lhost -> unit
val pp_offset : Format.formatter -> offset -> unit
val pp_exp : Format.formatter -> exp -> unit
val pp_constant : Format.formatter -> constant -> unit
val pp_unop : Format.formatter -> unop -> unit
val pp_binop : Format.formatter -> binop -> unit
