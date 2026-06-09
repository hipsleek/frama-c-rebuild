(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** unmarshal zarith custom blocks *)

open Unmarshal;;

let readz ch =
  let sign = read8u ch in
  let charlen = read32u ch in
  let str = Bytes.create charlen in
  readblock ch (Obj.repr str) 0 charlen;
  (* My beautiful string reversing code;
     now useless :(
     let max = pred charlen in
     for i = 0 to (pred max) / 2 do
      let c = str.[i] in
      str.[i] <- str.[max - i] ;
      str.[max - i] <- c
     done;
  *)
  let n = Z.of_bits (Bytes.to_string str) in
  let z = if sign = 0 then n else Z.neg n in
  Obj.repr z
;;

register_custom "_z" readz;;

(*
  #load "zarith.cma" ;;
  let f = open_out "test" ;;
  let i = ref (-10000000000000000L) ;;

  while !i <= 10000000000000000L do
  output_value f (Z.of_int64 (!i)) ;
  i := Int64.add !i 100000000000L ; done
  ;;


  ocamlc -custom zarith.cma unmarshal.ml unz.ml
*)

(*
let f = open_in "test" ;;

let i = ref (-10000000000000000L) ;;

while !i <= 10000000000000000L do
  let z = input_val f Abstract in
  let r = Z.to_int64 z in
  if (r <> !i)
  then begin
      Format.printf "read: %Ld expected: %Ld@."
  r !i;
      assert false
    end;
  i := Int64.add !i 100000000000L ;
done
;;
*)
