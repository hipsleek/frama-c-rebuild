(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type 'a sformat = ('a,Format.formatter,unit) format
type 'a formatter = Format.formatter -> 'a -> unit

(* Pretty printing for iterators *)

val pp_iter :
  ?pre:unit sformat ->
  ?sep:unit sformat ->
  ?suf:unit sformat ->
  ?format:('a formatter -> 'a -> unit) sformat ->
  (('a -> unit) -> 'b -> unit) ->
  'a formatter -> 'b formatter

val pp_iter2 :
  ?pre:(unit sformat) ->
  ?sep:(unit sformat) ->
  ?suf:(unit sformat) ->
  ?format:('a formatter -> 'a -> 'b formatter -> 'b -> unit) sformat ->
  (('a -> 'b -> unit) -> 'c -> unit) ->
  'a formatter -> 'b formatter -> 'c formatter
