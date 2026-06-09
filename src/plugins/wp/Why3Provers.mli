(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Why3 (and provers) configuration *)
(* -------------------------------------------------------------------------- *)

(** {2 Why3 configuration } *)

val why3_version : string
val config : unit -> Why3.Whyconf.config
val configure : unit -> unit
val set_procs : int -> unit

(** {2 Prover information } *)

type t = Why3.Whyconf.prover

val ident_why3 : t -> string
val ident_wp : t -> string
val title : ?version:bool -> t -> string
val name : t -> string
val version : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val lookup : ?fallback:bool -> string -> t option
val provers : unit -> t list
val is_auto : t -> bool
val is_available : t -> bool
val is_mainstream : t -> bool
val has_counter_examples : t -> bool
val with_counter_examples : t -> t option

type model = Why3.Model_parser.concrete_syntax_term
val pp_model : model Pretty_utils.formatter

(**************************************************************************)
