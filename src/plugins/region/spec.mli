(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type path =
  | Alias of location * term_lval
  | Field of location * term_lval * fieldinfo * fieldinfo
  | Range of location * term * typ * term * term

type region = {
  named : string ;
  paths : path list ;
  flags : Attr.flags ;
}

val pp_named : Format.formatter -> string -> unit
val pp_path : Format.formatter -> path -> unit
val pp_region : Format.formatter -> region -> unit
val pp_regions : Format.formatter -> region list -> unit

val of_extid : int -> region list
val of_extension : acsl_extension -> region list
val of_code_annot : code_annotation -> region list
val of_behavior : behavior -> region list
