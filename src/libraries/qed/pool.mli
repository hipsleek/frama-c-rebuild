(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---     Variable Management                                            --- *)
(* -------------------------------------------------------------------------- *)

module type Type =
sig
  type t
  val dummy : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(T : Type) :
sig

  (** Hashconsed *)
  type var =
    private {
    vid : int ;
    vbase : string ;
    vrank : int ;
    vtau : T.t ;
  }

  val dummy : var (** null vid *)

  val hash : var -> int
  (** [vid] *)

  val equal : var -> var -> bool
  (** [==] *)

  val compare : var -> var -> int
  val pretty : Format.formatter -> var -> unit

  type pool
  val create : ?copy:pool -> unit -> pool
  val add : pool -> var -> unit
  val fresh : pool -> string -> T.t -> var
  val alpha : pool -> var -> var

end
