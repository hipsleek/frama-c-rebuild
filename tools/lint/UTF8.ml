(***************************************************************************)
(*                                                                         *)
(*  This file was originally part of Camomile library.                     *)
(*                                                                         *)
(*  Copyright (C) 2002, 2003 Yamagata Yoriyuki.                            *)
(*                                                                         *)
(*  This library is free software; you can redistribute it and/or          *)
(*  modify it under the terms of the GNU Lesser General Public License     *)
(*  as published by the Free Software Foundation; either version 2 of      *)
(*  the License, or (at your option) any later version.                    *)
(*                                                                         *)
(*  As a special exception to the GNU Library General Public License, you  *)
(*  may link, statically or dynamically, a "work that uses this library"   *)
(*  with a publicly distributed version of this library to produce an      *)
(*  executable file containing portions of this library, and distribute    *)
(*  that executable file under terms of your choice, without any of the    *)
(*  additional requirements listed in clause 6 of the GNU Library General  *)
(*  Public License. By "a publicly distributed version of this library",   *)
(*  we mean either the unmodified Library as distributed by the authors,   *)
(*  or a modified version of this library that is distributed under the    *)
(*  conditions defined in clause 3 of the GNU Library General Public       *)
(*  License. This exception does not however invalidate any other reasons  *)
(*  why the executable file might be covered by the GNU Library General    *)
(*  Public License .                                                       *)
(*                                                                         *)
(*  This library is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(*  Lesser General Public License for more details.                        *)
(*                                                                         *)
(*  You should have received a copy of the GNU Lesser General Public       *)
(*  License along with this library; if not, write to the Free Software    *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307    *)
(*  USA                                                                    *)
(*                                                                         *)
(*  You can contact the authour by sending email to                        *)
(*  yoriyuki.y@gmail.com                                                   *)
(*                                                                         *)
(*                                                                         *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives).  *)
(*                                                                         *)
(***************************************************************************)

(* Function extracted from Camomile library and modified by CEA to get the
   first position of malformed UTF-8 string *)

let validate content =
  let exception Malformed_code of (int*int)*int in
  let len = String.length content in
  let rec trail lp c i a =
    if c = 0 then a else
    if i >= len then raise (Malformed_code (lp,(i-1))) else
      let n = Char.code (String.unsafe_get content i) in
      if n < 0x80 || n >= 0xc0 then raise (Malformed_code (lp,(i-1))) else
        trail lp (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let rec main ((l,_) as lp) i =
    if i >= len  then () else
      let n = Char.code (String.unsafe_get content  i) in
      let lp = if n = Char.code '\n' then l+1,i else lp in
      if n < 0x80 then main lp (i + 1) else
      if n < 0xc2 then raise (Malformed_code (lp,i)) else
      if n <= 0xdf then
        if trail lp 1 (i + 1) (n - 0xc0) < 0x80 then raise (Malformed_code (lp,i)) else
          main lp (i + 2)
      else if n <= 0xef then
        if trail lp 2 (i + 1) (n - 0xe0) < 0x800 then raise (Malformed_code (lp,i)) else
          main lp (i + 3)
      else if n <= 0xf7 then
        if trail lp 3 (i + 1) (n - 0xf0) < 0x10000 then raise (Malformed_code (lp,i)) else
          main lp (i + 4)
      else if n <= 0xfb then
        if trail lp 4 (i + 1) (n - 0xf8) < 0x200000 then raise (Malformed_code (lp,i)) else
          main lp (i + 5)
      else if n <= 0xfd then
        let n = trail lp 5 (i + 1) (n - 0xfc) in
        if n lsr 16 < 0x400 then raise (Malformed_code (lp,i)) else
          main lp (i + 6)
      else raise (Malformed_code (lp,i))
  in
  try
    main (1,0) 0 ; None
  with Malformed_code ((l,p),i) -> Some (l,p,i)

(**************************************************************************)
