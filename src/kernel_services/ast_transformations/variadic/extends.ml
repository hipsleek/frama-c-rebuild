(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Typ = struct
  let params typ =
    match Ast_types.unroll_node typ with
    | TFun (_, args, _) -> Cil.argsToList args
    | _ -> invalid_arg "params"

  let ghost_partitioned_params typ =
    match Ast_types.unroll_node typ with
    | TFun (_, args, _) -> Cil.argsToPairOfLists args
    | _ -> invalid_arg "params"

  let params_types typ =
    List.map (fun (_,typ,_) -> typ) (params typ)

  let params_count typ =
    List.length (params typ)
end
