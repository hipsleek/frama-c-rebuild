(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Sanitizer                                                          --- *)
(* -------------------------------------------------------------------------- *)

(*
   Keeps only alphanumerical characters,
   remove consecutive, trailing and leading `_`
*)

type state = START | SEP | CHAR | TRUNCATE

type buffer = {
  content : Buffer.t ;
  truncate : int ;
  mutable lastsep : int ;
  mutable state : state ;
}

let create ?(truncate=false) n = {
  content = Buffer.create n ;
  truncate = if truncate then n else max_int ;
  lastsep = 0 ;
  state = START ;
}

let clear buffer =
  begin
    Buffer.clear buffer.content ;
    buffer.state <- START ;
    buffer.lastsep <- 0 ;
  end

let add_sep buffer =
  if buffer.state = CHAR then
    let offset = Buffer.length buffer.content in
    if offset < buffer.truncate then
      begin
        buffer.state <- SEP ;
        buffer.lastsep <- offset ;
      end
    else
      begin
        buffer.state <- TRUNCATE ;
        Buffer.truncate buffer.content buffer.lastsep
      end

let add_char buffer = function
  | ('a'..'z' | 'A'..'Z' | '0'..'9') as c ->
    begin
      match buffer.state with
      | START ->
        Buffer.add_char buffer.content c ;
        buffer.state <- CHAR
      | SEP ->
        Buffer.add_char buffer.content '_' ;
        Buffer.add_char buffer.content c ;
        buffer.state <- CHAR
      | CHAR ->
        Buffer.add_char buffer.content c
      | TRUNCATE -> ()
    end
  | '_' | '-' | ' ' | '\t' | ',' | ';' | '.' | '/' | '\\' | ':' ->
    add_sep buffer
  | _ -> ()

let add_string buffer s = String.iter (add_char buffer) s

let rec add_list buffer = function
  | [] -> ()
  | p::ps -> add_string buffer p ; add_sep buffer ; add_list buffer ps

let contents buffer =
  Buffer.contents buffer.content
