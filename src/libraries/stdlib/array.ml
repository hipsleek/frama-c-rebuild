(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Stdlib.Array

let equal f a1 a2 =
  let exception Early_exit in
  let size = length a1 in
  if length a2 != size then false
  else try
      for i = 0 to size - 1 do
        if not (f (get a1 i) (get a2 i)) then raise Early_exit
      done;
      true
    with Early_exit -> false

let compare f a1 a2 =
  let exception Early_exit of int in
  let size1 = length a1 and size2 = length a2 in
  if size1 < size2 then -1
  else if size1 > size2 then 1
  else try
      for i = 0 to size1 do
        let n = f (get a1 i) (get a2 i) in
        if n != 0 then raise (Early_exit n)
      done;
      0
    with Early_exit n -> n

let hash hash_elt = Hash.hash_iter iter hash_elt

let pretty
    ?(format=format_of_string "[| %t |]")
    ?(item=format_of_string "%a")
    ?(sep=format_of_string ";@ ")
    ?(last=sep)
    ?(empty=format_of_string "[||]")
    pp_elt fmt a =
  Pretty.pretty_seq ~format ~item ~sep ~last ~empty pp_elt fmt (to_seq a)
