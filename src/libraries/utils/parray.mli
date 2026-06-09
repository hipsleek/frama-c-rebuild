(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Persistent array, based on "A Persistent Union-Find Data Structure" by
   Sylvain Conchon and Jean-Chistophe Filliâtre. For further details, see
   https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf *)

type 'a t

val init : int -> (int -> 'a) -> 'a t
val get  : 'a t -> int -> 'a
val set  : 'a t -> int -> 'a -> 'a t
val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val map  : ('a -> 'a) -> 'a t -> 'a t

val pretty :
  ?sep : Pretty_utils.sformat ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit
