(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type zero = |
type 'n succ = |
type 'n nat = int
type positive_or_null = PositiveOrNull : 'n nat -> positive_or_null
type strictly_positive = StrictlyPositive : 'n succ nat -> strictly_positive

let zero : zero nat = 0
let one  : zero succ nat = 1
let succ : type n. n nat -> n succ nat = fun n -> n + 1
let prev : type n. n succ nat -> n nat = fun n -> n - 1
let to_int : type n. n nat -> int = fun n -> n

let of_int n =
  if n < 0 then None else Some (PositiveOrNull n)

let of_strictly_positive_int n =
  if n < 1 then None else Some (StrictlyPositive n)
