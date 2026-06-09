(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)


(** OCaml types used to represent wide characters and strings *)
type wchar = int64
type wstring = wchar list

let escape_char_internal maybe_trigraph =
  function
  | '\007' -> maybe_trigraph := false; "\\a"
  | '\b' -> maybe_trigraph := false; "\\b"
  | '\t' -> maybe_trigraph := false; "\\t"
  | '\n' -> maybe_trigraph := false; "\\n"
  | '\011' -> maybe_trigraph := false; "\\v"
  | '\012' -> maybe_trigraph := false; "\\f"
  | '\r' -> maybe_trigraph := false; "\\r"
  | '"' -> maybe_trigraph := false; "\\\""
  | '\'' -> maybe_trigraph := false; "\\'"
  | '\\' -> maybe_trigraph := false; "\\\\"
  | '?' ->
    let s = if !maybe_trigraph then "\\?" else "?" in
    maybe_trigraph := true;
    s
  | ' ' .. '~' as printable -> maybe_trigraph := false; String.make 1 printable
  | unprintable -> maybe_trigraph := false; Printf.sprintf "\\%03o" (Char.code unprintable)

let escape_char c =
  let r = ref false in
  escape_char_internal r c

let escape_string str =
  let length = String.length str in
  let buffer = Buffer.create length in
  let maybe_trigraph = ref false in
  for index = 0 to length - 1 do
    Buffer.add_string buffer (escape_char_internal maybe_trigraph (String.get str index))
  done;
  Buffer.contents buffer

(* a wide char represented as an int64 *)
let escape_wchar =
  (* limit checks whether upper > probe *)
  let limit upper probe = (Int64.to_float (Int64.sub upper probe)) > 0.5 in
  let fits_byte = limit (Int64.of_int 0x100) in
  let fits_octal_escape = limit (Int64.of_int 0o1000) in
  let fits_universal_4 = limit (Int64.of_int 0x10000) in
  let fits_universal_8 = limit (Int64.of_string "0x100000000") in
  fun charcode ->
    if fits_byte charcode then
      escape_char (Char.chr (Int64.to_int charcode))
    else if fits_octal_escape charcode then
      Printf.sprintf "\\%03Lo" charcode
    else if fits_universal_4 charcode then
      Printf.sprintf "\\u%04Lx" charcode
    else if fits_universal_8 charcode then
      Printf.sprintf "\\u%04Lx" charcode
    else
      invalid_arg "Cprint.escape_string_intlist"

(* a wide string represented as a list of int64s *)
let escape_wstring (str : int64 list) =
  let length = List.length str in
  let buffer = Buffer.create length in
  let append charcode =
    let addition = escape_wchar charcode in
    Buffer.add_string buffer addition
  in
  List.iter append str;
  Buffer.contents buffer
