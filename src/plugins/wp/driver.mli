(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Driver for External Files                                          --- *)
(* -------------------------------------------------------------------------- *)

val load_driver : unit -> LogicBuiltins.driver
(** Memoized loading of drivers according to current
    WP options. Finally sets [LogicBuiltins.driver] and returns it. *)
