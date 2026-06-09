(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

module type Conversion = sig
  type extended
  type internal
  val extend : internal -> extended
  val replace : internal -> extended -> extended
  val restrict : extended -> internal
end

module Make
    (Loc: Abstract_location.Leaf)
    (Convert : Conversion with type internal := Loc.value)
= struct

  (* Import most of [Loc] *)
  include (Loc: Abstract_location.S
           with type value := Loc.value (* we are converting this type *)
            and type location = Loc.location
            and type offset = Loc.offset)
  type value = Convert.extended

  let structure = Abstract.Location.Leaf (Loc.key, (module Loc))

  (* Now lift the functions that contain {!value} in their type. *)

  let to_value loc = Loc.to_value loc >>-: Convert.extend

  let forward_index typ value offset =
    Loc.forward_index typ (Convert.restrict value) offset

  let forward_pointer typ value offset =
    Loc.forward_pointer typ (Convert.restrict value) offset

  let backward_pointer value offset loc =
    let v = Convert.restrict value in
    Loc.backward_pointer v offset loc >>-: fun (v, off) ->
    Convert.replace v value, off

  let backward_index typ ~index:value ~remaining offset =
    let index = Convert.restrict value in
    Loc.backward_index typ ~index ~remaining offset >>-: fun (v, off) ->
    Convert.replace v value, off
end
