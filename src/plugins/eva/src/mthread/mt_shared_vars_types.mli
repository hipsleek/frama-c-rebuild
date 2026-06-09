(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_types



(** Sets of zone accesses (used in cfg nodes) *)
module SetZoneAccess: sig
  include Datatype.Set with type elt = rw * Memory_zone.t

  val pretty_sep: sep:Pretty_utils.sformat -> Format.formatter -> t -> unit
end



(** Type of a full access operation to a variable : read or write, statement at
    which the access takes place, thread that does the operation *)
module StmtIdAccess : Datatype.S with type t = rw * stmt * Thread.t

(** More than one full access to a variable. The boolean indicates
    whether all accesses are dummy ones, ie present just to ensure
    convergence of the algorithm *)
module SetStmtIdAccess: sig
  include Lattice_type.Lattice_Set with type O.elt = StmtIdAccess.t

  val pretty_aux:
    StmtIdAccess.t Pretty_utils.formatter -> t Pretty_utils.formatter
end

(** Maps from zones to variables accesses *)
module AccessesByZone: sig
  include Lmap_bitwise.Location_map_bitwise with type v = SetStmtIdAccess.t

  val pretty_map: map Pretty_utils.formatter
end

(** Kind of access: read or write. *)
type access_kind = AccessRead | AccessWrite

module AccessKind : Datatype.S_with_collections with type t = access_kind

(** Protection of an access: unprotected, maybe protected by a mutex,
    fully protected by a mutex. *)
type protection =
  | Unprotected
  | MaybeProtected of Mutex.t
  | Protected of Mutex.t

module Protection : Datatype.S_with_collections with type t = protection
