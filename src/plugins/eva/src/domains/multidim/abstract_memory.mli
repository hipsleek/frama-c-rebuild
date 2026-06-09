(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds

(* Memory initialization *)
type initialization =
  | SurelyInitialized
  | MaybeUninitialized

(* Abstraction of an unstructured bit in the memory *)
type bit =
  | Uninitialized (* Uninitialized everywhere *)
  | Zero of initialization (* Zero or uninitialized everywhere *)
  | Any of Base.SetLattice.t * initialization
  (* Undetermined anywhere, and can contain bits
     of pointers. If the base set is empty,
     the bit can only come from numerical values. *)

module Bit :
sig
  type t = bit

  val uninitialized : t
  val zero : t
  val numerical : t
  val top : t

  val is_any : t -> bool
  val initialization : t -> initialization

  val pretty : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val is_included : t -> t -> bool
  val join : t -> t -> t
end

(* Size type for memory abstraction *)
type size = Z.t

(* Oracles for memory abstraction *)
type side = Left | Right
type oracle = Eva_ast.exp -> Int_val.t
type bioracle = side -> oracle

(* Early stage of memory abstraction building *)
module type ProtoMemory =
sig
  type t
  type value

  val pretty : Format.formatter -> t -> unit
  val pretty_root : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val of_raw : bit -> t
  val raw : t -> bit
  val of_value : Cil_types.typ -> value -> t
  val to_value : Cil_types.typ -> t -> value
  val to_singleton_int : t -> Z.t option
  val weak_erase : bit -> t -> t
  val is_included : t -> t -> bool
  val unify : oracle:bioracle ->
    (size:size -> value -> value -> value) -> t -> t -> t
  val join : oracle:bioracle -> t -> t -> t
  val smash : oracle:oracle -> t -> t -> t
  val read : oracle:oracle -> (Cil_types.typ -> t -> 'a) -> ('a -> 'a -> 'a) ->
    Abstract_offset.t -> t -> 'a
  val update : oracle:oracle ->
    (weak:bool -> Cil_types.typ -> t -> t or_bottom) ->
    weak:bool -> Abstract_offset.t -> t -> t or_bottom
  val incr_bound : oracle:oracle -> Cil_types.varinfo -> Z.t option ->
    t -> t
  val add_segmentation_bounds : oracle:oracle -> typ:Cil_types.typ ->
    Eva_ast.exp list -> t -> t
end
