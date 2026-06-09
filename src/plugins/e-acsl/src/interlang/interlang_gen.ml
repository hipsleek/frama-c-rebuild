(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Interlang
module Term = Cil_datatype.Term

module Conf = struct
  type env =
    {kf : kernel_function;
     loc : location;
     vars : exp Term.Map.t;
     env : Env.t;
     rte : bool}

  type state = exp Term.Map.t (* local variables *)

  type out = unit
  let merge_out () () = ()
  let empty_out () = ()
end


exception Not_covered

include Conf

module M = struct
  include Monad_rws.Make (Conf)
  open Operators

  let not_covered ?pre pp x =
    let* {loc} = read in
    Options.debug
      ~dkey:Options.Dkey.interlang_not_covered "@[<2>@[%a: %a@]@;@[<2>%a@]@]"
      Fileloc.pretty loc
      (Pretty_utils.pp_opt ~suf:": " Format.pp_print_string) pre
      pp x;
    raise Not_covered

  let read_logic_env = let* {env} = read in return @@ Env.Logic_env.get env
end

type 'a m = 'a M.t

let of_binop : Cil_types.binop -> Interlang.binop = function
  | Cil_types.PlusA -> Plus
  | MinusA -> Minus
  | Mult -> Mult
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge
  | Eq -> Eq
  | Ne -> Ne
  | Div -> Div
  | Mod -> Mod
  | _ -> raise Not_covered

let of_relation = function
  | Rlt -> Lt
  | Rgt -> Gt
  | Rle -> Le
  | Rge -> Ge
  | Req -> Eq
  | Rneq -> Ne
