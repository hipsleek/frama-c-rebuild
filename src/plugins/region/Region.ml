(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Region Analysis API                                                --- *)
(* -------------------------------------------------------------------------- *)

type map = Memory.map
type node = Memory.node
let map kf = Analysis.get kf
let id = Memory.id
let of_id = Memory.of_id
let pretty = Memory.pp_node
let iter = Memory.iter
let equal = Memory.equal
let included = Memory.included
let compare a b = Int.compare (id a) (id b)
let separated = Memory.separated
let singleton = Memory.singleton
let size = Memory.size
let cvars = Memory.cvars
let labels = Memory.labels
let reads = Memory.reads
let writes = Memory.writes
let shifts = Memory.shifts
let typed = Memory.typed
let parents = Memory.parents
let points_to = Memory.points_to
let pointed_by = Memory.pointed_by
let lval = Memory.lval
let exp m e = Option.map Memory.find @@ Memory.exp m e
let cvar = Memory.cvar
let field = Memory.field
let index = Memory.index
let footprint = Memory.footprint
