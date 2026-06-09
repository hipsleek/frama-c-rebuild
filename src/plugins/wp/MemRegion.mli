(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ctypes
open Memory

type prim = | Int of c_int | Float of c_float | Ptr
type kind = Single of prim | Many of prim | Garbled
val pp_kind : Format.formatter -> kind -> unit

(* -------------------------------------------------------------------------- *)
(* --- Region Memory Model                                                --- *)
(* -------------------------------------------------------------------------- *)

module type RegionProxy =
sig
  type region
  val id : region -> int
  val of_id : int -> region option
  val pretty : Format.formatter -> region -> unit
  val kind : region -> kind
  val name : region -> string option
  val cvar : varinfo -> region option
  val field : region -> fieldinfo -> region option
  val shift : region -> c_object -> region option
  val points_to : region -> region option
  val literal : eid:int -> Cstring.cst -> region option
  val separated : region -> region -> bool
  val included : region -> region -> bool
  val footprint : region -> region list
end

module Make
    (_:RegionProxy)
    (M:Model)
    (_:MemLoader.Model with type loc = M.loc) : Model
