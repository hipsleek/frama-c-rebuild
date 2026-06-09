(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

type t =
  | True
  | False
  | Undefined

let bool3and c1 c2 = match c1, c2 with
  | True, True -> True

  | _, False
  | False, _ -> False

  | Undefined, _
  | _, Undefined -> Undefined

let bool3or c1 c2 = match c1, c2 with
  | True, _
  | _, True -> True

  | _, Undefined
  | Undefined, _ -> Undefined

  | False, False -> False

let bool3not c = match c with
  | True -> False
  | False -> True
  | Undefined -> Undefined

let bool3_of_bool b = if b then True else False
