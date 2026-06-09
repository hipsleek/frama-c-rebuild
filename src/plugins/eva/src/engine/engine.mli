(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Builds an analysis engine for the given abstractions. *)
module Make (Abstract: Abstractions.S) : Engine_sig.S_with_results
  with type Ctx.t = Abstract.Ctx.t
   and type Val.t = Abstract.Val.t
   and type Loc.location = Abstract.Loc.location
   and type Dom.state = Abstract.Dom.state

(** The current analysis engine, with all abstractions and the results. *)
val current : unit -> (module Engine_sig.S_with_results)

(** Builds the current analysis engine according to the Eva parameters. *)
val reset : unit -> (module Engine_sig.S_with_results)

(** Registers a hook that will be called each time the current analyzer
    is changed. This happens when a new analysis is run with different
    abstractions than before, or when the current project is changed. *)
val register_hook: ((module Engine_sig.S_with_results) -> unit) -> unit
