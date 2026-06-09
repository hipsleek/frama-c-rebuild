(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds
open Abstract_memory

module type Config =
sig
  (* Limit on the number of slice after a write for array segmentations.
     Makes sense above or equal to 1, though below 3 is counter-productive. *)
  val slice_limit : unit -> int
end

module Bound :
sig
  type t
  exception UnsupportedBoundExpression
  val of_exp : Eva_ast.exp -> t
  val of_integer : Z.t -> t
  val succ : t -> t
end

module type Segmentation =
sig
  type bound = Bound.t
  type submemory
  type t
  val pretty : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hull : oracle:oracle -> t -> (bound * bound) or_top
  val raw : t -> Bit.t
  val weak_erase : Bit.t -> t -> t
  val is_included : t -> t -> bool
  val unify : oracle:bioracle -> (submemory -> submemory -> submemory) ->
    t -> t -> t or_top
  val single : bit -> bound -> bound -> submemory -> t
  val read : oracle:oracle ->
    (submemory -> 'a) -> ('a -> 'a -> 'a) -> t -> bound -> bound -> 'a
  val update : oracle:oracle -> (submemory -> submemory or_bottom) ->
    t -> bound -> bound -> t or_top_bottom
  val incr_bound :
    oracle:oracle -> Cil_types.varinfo -> Z.t option -> t -> t or_top
  val map : (submemory -> submemory) -> t -> t
  val fold : (submemory -> 'a -> 'a) -> (bit -> 'b -> 'a) -> t -> 'b -> 'a
  val add_segmentation_bounds : oracle:oracle -> bound list -> t -> t
end

module Make (_ : Config) (M : ProtoMemory) :
  Segmentation with type submemory = M.t
