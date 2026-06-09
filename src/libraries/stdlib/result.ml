(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Stdlib.Result

let zip l r =
  match l, r with
  | Ok l, Ok r -> Ok (l, r)
  | Error e, _ -> Error e
  | _, Error e -> Error e

module Operators = struct
  let ( >>-  ) r f = bind r f
  let ( let* ) r f = bind r f
  let ( >>-: ) r f = map f r
  let ( let+ ) r f = map f r
  let ( and* ) l r = zip l r
  let ( and+ ) l r = zip l r
end

let value_or_else ~error res =
  fold ~ok:Fun.id ~error res
