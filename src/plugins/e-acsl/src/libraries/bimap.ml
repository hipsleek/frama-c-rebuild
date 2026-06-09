(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Make (H : Hashtbl.S) = struct
  let head_tbl = H.create 7
  let tail_tbl = H.create 7

  let clear () =
    H.clear head_tbl;
    H.clear tail_tbl

  let add head tail =
    H.add head_tbl head tail;
    H.add tail_tbl tail head

  let tails head = H.find_all head_tbl head
  let tail head = H.find head_tbl head
  let tail_opt head = H.find_opt head_tbl head
  let heads tail = H.find_all tail_tbl tail
  let head tail = H.find tail_tbl tail
  let head_opt tail = H.find_opt tail_tbl tail

  let tail_or_self head = try tail head with Not_found -> head
  let head_or_self tail = try head tail with Not_found -> tail
end
