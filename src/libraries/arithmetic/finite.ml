(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Nat

type 'n finite = int

let first  : type n. n succ finite = 0
let last   : type n. n succ nat -> n succ finite = fun n -> Nat.to_int n - 1
let next   : type n. n finite -> n succ finite = fun n -> n + 1
let to_int : type n. n finite -> int = fun n -> n
let weaken : type n. n finite -> n succ finite = fun n -> n

let strengthen : type n. n nat -> n succ finite -> n finite option =
  fun limit n -> if n < Nat.to_int limit then Some n else None

let of_int : type n. n succ nat -> int -> n succ finite option =
  fun limit n -> if 0 <= n && n < Nat.to_int limit then Some n else None

let fold f ?start ?stop size acc =
  let acc = ref acc in
  let start = Option.value start ~default:first in
  let stop  = Option.value stop  ~default:(Nat.to_int size - 1) in
  for i = start to stop do acc := f i !acc done ;
  !acc

let iter    f ?start ?stop size =
  fold (fun i () -> f i) ?start ?stop size ()

let for_all f ?start ?stop size =
  fold (fun i -> (&&) (f i)) ?start ?stop size true

let ( =  ) : type n. n finite -> n finite -> bool = fun l r -> l =  r
let ( != ) : type n. n finite -> n finite -> bool = fun l r -> l != r
let ( <  ) : type n. n finite -> n finite -> bool = fun l r -> l <  r
let ( <= ) : type n. n finite -> n finite -> bool = fun l r -> l <= r
let ( >  ) : type n. n finite -> n finite -> bool = fun l r -> l >  r
let ( >= ) : type n. n finite -> n finite -> bool = fun l r -> l >= r
