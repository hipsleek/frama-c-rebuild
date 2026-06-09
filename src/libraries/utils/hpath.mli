(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* This module is the internal representation of Hashconsed filepath with
   symbolic names. It should not be used except by Filepath directly. *)

type t

val empty : t
val cwd : t

val of_string : ?base:string -> string -> t
val to_string : t -> string

type base = Absolute | Cwd | Name of string * t

val to_uri : t -> base * string

module Names  : sig
  val add : t -> string -> unit
  val remove : t -> unit
  val all : unit -> (t * string) list
end
