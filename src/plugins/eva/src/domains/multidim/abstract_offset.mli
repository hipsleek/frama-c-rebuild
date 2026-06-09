(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds

type t =
  | NoOffset of Cil_types.typ
  | Index of Eva_ast.exp option * Int_val.t * Cil_types.typ * t
  | Field of Cil_types.fieldinfo * t

val pretty : Format.formatter -> t -> unit

val of_var_address : Cil_types.varinfo -> t
val of_eva_offset : (Eva_ast.exp -> Int_val.t) ->
  Cil_types.typ -> Eva_ast.offset -> t or_top
val of_ival : base_typ:Cil_types.typ -> typ:Cil_types.typ -> Ival.t -> t or_top
val of_term_offset : Cil_types.typ -> Cil_types.term_offset -> t or_top

val is_singleton : t -> bool
val references : t -> Cil_datatype.Varinfo.Set.t (* variables referenced in the offset *)

val append : t -> t -> t (* Does not check that the appended offset fits *)
val join : t -> t -> t or_top
val add_index : (Eva_ast.exp -> Int_val.t) -> t -> Eva_ast.exp -> t or_top
