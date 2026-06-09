(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t =
  | StructOrUnion
  | Array
  | NotAggregate

(** @return the content of the array type if [ty] is an array, or None
    otherwise. *)
let rec get_array_typ_opt ty =
  if Gmp_types.is_t ty then
    (* GMP pointer types are declared as arrays of one element. They are treated
       as a special case here to ensure that they are not considered as arrays.
    *)
    None
  else
    match ty.tnode with
    | TNamed ti -> get_array_typ_opt ti.ttype
    | TArray (t, eo) -> Some (t, eo, ty.tattr)
    | _ -> None

(** @return true iff the type is an array *)
let is_array ty =
  match get_array_typ_opt ty with
  | Some _ -> true
  | None -> false

let get_t ty =
  if is_array ty then
    Array
  else if Ast_types.is_struct_or_union ty then
    StructOrUnion
  else
    NotAggregate
