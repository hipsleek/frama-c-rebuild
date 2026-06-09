(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier BSD-3-Clause                                      *)
(*  Copyright (C)                                                             *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  All rights reserved.                                                      *)
(*                                                                            *)
(******************************************************************************)

open Unmarshal

let l = [  512;  35;  62;  512;  42;  62;  17 ]

let t_renumber_int =
  let tbl = Hashtbl.create 42 in
  let count = ref 0 in
  let f x =
    match ((Obj.magic x) : int ) with
    |  x ->
      let result =
        try
          Hashtbl.find tbl x
        with Not_found ->
          let c = !count in
          count := succ c;
          Hashtbl.add tbl x c;
          c
      in
      Obj.repr (result : int )
  in
  Transform (t_option t_int, f)

let t_l = t_list t_renumber_int

let () =
  let oc = open_out_bin "test-file" in
  Marshal.to_channel oc l [];
  close_out oc;
  let ic = open_in_bin "test-file" in
  let result = input_val ic t_l in
  close_in ic;
  List.iter (print_int ) result;
  print_endline "fin test1"


let l = [ Some 512; Some 35; Some 62; Some 512; Some 42; Some 62; Some 17 ]

let t_renumber_intopt =
  let tbl = Hashtbl.create 42 in
  let count = ref 0 in
  let f x =
    match ((Obj.magic x) : int option) with
      None -> assert false
    | Some x ->
      let result =
        try
          Hashtbl.find tbl x
        with Not_found ->
          let c = !count in
          count := succ c;
          Hashtbl.add tbl x c;
          c
      in
      Obj.repr (Some(result) : int option)
  in
  Transform (t_option t_int, f)

let t_l = t_list t_renumber_intopt

let () =
  let oc = open_out_bin "test-file" in
  Marshal.to_channel oc l [];
  close_out oc;
  let ic = open_in_bin "test-file" in
  let result = input_val ic t_l in
  close_in ic;
  List.iter (function None -> () | (Some(i)) -> print_int i) result;
  print_endline "fin test2"

let h = Hashtbl.create 12;;

let () =
  Hashtbl.add h 34 "s34";
  Hashtbl.add h 63 "s63"

let t_h1 =
  t_hashtbl_changedhashs Hashtbl.create Hashtbl.add t_renumber_int Abstract

let () =
  let oc = open_out_bin "test-file" in
  Marshal.to_channel oc h [];
  close_out oc;
  let ic = open_in_bin "test-file" in
  let result = input_val ic t_h1 in
  close_in ic;
  Hashtbl.iter (fun k v -> Format.printf "%d %s@." k v) result;
  print_endline "fin test3"

let t_h2 =
  t_hashtbl_unchangedhashs t_int Abstract

let () =
  let oc = open_out_bin "test-file" in
  Marshal.to_channel oc h [];
  close_out oc;
  let ic = open_in_bin "test-file" in
  let result = input_val ic t_h2 in
  close_in ic;
  Hashtbl.iter (fun k v -> Format.printf "%d %s@." k v) result;
  print_endline "fin test4"
