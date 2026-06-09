(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let hash_iter ?(limit=16) iter hash x =
  let acc = ref 1 in
  let count = ref 0 in
  let f x =
    if !count >= limit then raise Exit;
    incr count;
    acc := 257 * !acc + hash x
  in
  (try iter f x with Exit -> ());
  !acc
