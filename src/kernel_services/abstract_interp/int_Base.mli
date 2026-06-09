(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

[@@@deprecated "Use Z_or_top instead"]

type i = Z_or_top.t
[@@deprecated "Use Z_or_top.t instead"]

type t = Z_or_top.t
[@@deprecated "Use Z_or_top.t instead"]
val ty: Z_or_top.t Type.t
[@@deprecated "Use Z_or_top.ty instead"]
[@@migrate { repl = Z_or_top.ty }]
val datatype_name : string
[@@deprecated "Use Z_or_top.datatype_name instead"]
[@@migrate { repl = Z_or_top.datatype_name }]
val datatype_descr : Z_or_top.t Descr.t
[@@deprecated "Use Z_or_top.datatype_descr instead"]
[@@migrate { repl = Z_or_top.datatype_descr }]
val packed_descr : Structural_descr.pack
[@@deprecated "Use Z_or_top.packed_descr instead"]
[@@migrate { repl = Z_or_top.packed_descr }]
val reprs : Z_or_top.t list
[@@deprecated "Use Z_or_top.reprs instead"]
[@@migrate { repl = Z_or_top.reprs }]
val equal : Z_or_top.t -> Z_or_top.t -> bool
[@@deprecated "Use Z_or_top.equal instead"]
[@@migrate { repl = Z_or_top.equal }]
val compare : Z_or_top.t -> Z_or_top.t -> int
[@@deprecated "Use Z_or_top.compare instead"]
[@@migrate { repl = Z_or_top.compare }]
val hash : Z_or_top.t -> int
[@@deprecated "Use Z_or_top.hash instead"]
[@@migrate { repl = Z_or_top.hash }]
val pretty : Format.formatter -> Z_or_top.t -> unit
[@@deprecated "Use Z_or_top.pretty instead"]
[@@migrate { repl = Z_or_top.pretty }]
val mem_project : (Project_skeleton.t -> bool) -> Z_or_top.t -> bool
[@@deprecated "Use Z_or_top.mem_project instead"]
[@@migrate { repl = Z_or_top.mem_project }]
val copy : Z_or_top.t -> Z_or_top.t
[@@deprecated "Use Z_or_top.copy instead"]
[@@migrate { repl = Z_or_top.copy }]

val zero: Z_or_top.t
[@@deprecated "Use `Value Z.zero instead"]
[@@migrate { repl = `Value Z.zero }]

val one: Z_or_top.t
[@@deprecated "Use `Value Z.one instead"]
[@@migrate { repl = `Value Z.one }]

val minus_one: Z_or_top.t
[@@deprecated "Use `Value Z.minus_one instead"]
[@@migrate { repl = `Value Z.minus_one }]

val top: Z_or_top.t
[@@deprecated "Use Z_or_top.top instead"]
[@@migrate { repl = Z_or_top.top }]

val neg: Z_or_top.t -> Z_or_top.t
[@@deprecated "Lattice_bounds.Top.map Z.neg instead"]
[@@migrate { repl = (fun z -> Lattice_bounds.Top.map Z.neg z)}]

val is_zero: Z_or_top.t -> bool
[@@deprecated "Use Z_or_top.is_zero instead"]
[@@migrate { repl = Z_or_top.is_zero }]

val is_top: Z_or_top.t -> bool
[@@deprecated "Use Z_or_top.is_top instead"]
[@@migrate { repl = Z_or_top.is_top }]

val inject: Z.t -> Z_or_top.t
[@@deprecated "Use `Value z instead"]
[@@migrate { repl = (fun z -> `Value z)}]

val project: Z_or_top.t -> Z.t
[@@deprecated "Use Z_or_top.project instead"]
[@@migrate { repl = Z_or_top.project }]
(** @raise Error_Top if the argument is {!Top}. *)

val cardinal_zero_or_one: Z_or_top.t -> bool
[@@deprecated "Use not Z_or_top.is_top instead"]
[@@migrate { repl = (fun z -> not (Z_or_top.is_top z))}]
