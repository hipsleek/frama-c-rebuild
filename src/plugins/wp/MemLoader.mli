(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)

(** Compound Loader *)

open Cil_types
open Definitions
open Ctypes
open Lang.F
open Memory
open Sigma

val cluster : unit -> cluster

(** Loader Model for Atomic Values *)
module type Model =
sig

  val name : string

  type loc
  val pretty : Format.formatter -> loc -> unit
  val sizeof : c_object -> term
  val field : loc -> fieldinfo -> loc
  val shift : loc -> c_object -> term -> loc

  (** Conversion among loc, t_pointer terms and t_addr terms *)

  val to_region_pointer : loc -> int * term
  val of_region_pointer : int -> c_object -> term -> loc

  val value_footprint: c_object -> loc -> domain
  val init_footprint: c_object -> loc -> domain

  val last : sigma -> c_object -> loc -> term

  val fresh : loc -> var list * loc
  val separated : loc -> term -> loc -> term -> pred

  val eqmem : Chunk.t -> term -> term -> loc -> term -> pred
  val memcpy : Chunk.t -> term -> loc -> term -> loc -> term -> term

  val load_int : sigma -> c_int -> loc -> term
  val load_float : sigma -> c_float -> loc -> term
  val load_pointer : sigma -> typ -> loc -> loc
  val load_init_atom : sigma -> c_object -> loc -> term

  val store_int : sigma -> c_int -> loc -> term -> Chunk.t * term
  val store_float : sigma -> c_float -> loc -> term -> Chunk.t * term
  val store_pointer : sigma -> typ -> loc -> term -> Chunk.t * term
  val store_init_atom : sigma -> c_object -> loc -> term -> Chunk.t * term

end

(** Generates Loader for Compound Values *)
module Make (M : Model) :
sig

  val domain : c_object -> M.loc -> domain

  val load : sigma -> c_object -> M.loc -> M.loc Memory.value
  val load_init : sigma -> c_object -> M.loc -> term
  val load_value : sigma -> c_object -> M.loc -> term

  val stored : sigma sequence -> c_object -> M.loc -> term -> equation list
  val stored_init : sigma sequence -> c_object -> M.loc -> term -> equation list

  val copied : sigma sequence -> c_object -> M.loc -> M.loc -> equation list
  val copied_init : sigma sequence -> c_object -> M.loc -> M.loc -> equation list

  val assigned : sigma sequence -> c_object -> M.loc sloc -> equation list

  val initialized : sigma -> M.loc rloc -> pred

end

(* -------------------------------------------------------------------------- *)
