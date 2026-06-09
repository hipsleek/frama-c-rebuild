(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Represent a function or statement contract. *)
type contract = {
  location: location;
  (** Location of the function or statement attached to the contract. *)

  named_behaviors_count: int;
  (** Number of named behaviors in the contract
      (excluding the default behavior) *)

  name_to_idx_tbl: (string, int) Hashtbl.t;
  (** Hashtable associating the name of a behavior with its index in the C API
      structure used to store behaviors information at runtime. *)

  mutable var: (varinfo * exp) option;
  (** Elements to access the C API structure used to store contracts
      information at runtime. *)

  mutable all_assumes_translated: bool;
  (** True if all the assumes clauses of the contract could be translated, false
      otherwise.

      If even one assume clause can't be translated, then the complete and
      disjoint clauses can't be computed. *)

  spec: spec
  (** Specification for the contract *)
}
