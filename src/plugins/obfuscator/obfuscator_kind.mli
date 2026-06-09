(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type k =
  | Behavior
  | Enum
  | Field
  | Formal_var
  | Formal_in_type
  | Function
  | Global_var
  | Label
  | String_literal
  | Local_var
  | Logic_var
  | Predicate
  | Type
  | Logic_type
  | Logic_constructor
  | Axiomatic
  | Lemma

include Datatype.S_with_collections with type t = k
val prefix: t -> string
