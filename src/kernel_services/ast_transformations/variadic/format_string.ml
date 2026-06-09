(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t =
  | String of string
  | WString of int64 list

exception OutOfBounds
exception NotAscii of int64

let get_char (s : t) (i : int) : char =
  match s with
  | String s ->
    begin try
        String.get s i
      with
        Invalid_argument _ -> raise OutOfBounds
    end
  | WString s ->
    begin try
        let c = List.nth s i in
        if (c >= Int64.zero && c<= (Int64.of_int 255)) then
          Char.chr (Int64.to_int c)
        else
          raise (NotAscii c)
      with
        Failure _ -> raise OutOfBounds
    end

let get_wchar (s : t) (i : int) : int64 =
  match s with
  | String s ->
    begin try
        Int64.of_int (Char.code (String.get s i))
      with
        Invalid_argument _ -> raise OutOfBounds
    end
  | WString s ->
    begin try
        List.nth s i
      with
        Failure _ -> raise OutOfBounds
    end

let sub_string (s : t) (start : int) (len : int) : string =
  let init_char i =
    get_char s (start + i)
  in
  String.init len init_char
