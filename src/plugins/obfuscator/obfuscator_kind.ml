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

let name_of_kind = function
  | Behavior -> "behavior"
  | Enum -> "enum"
  | Field -> "field"
  | Formal_var -> "formal variable"
  | Formal_in_type -> "formal variable in fun type"
  | Function -> "function"
  | Global_var -> "global variable"
  | Label -> "label"
  | String_literal -> "literal string"
  | Local_var -> "local variable"
  | Logic_var -> "logic variable"
  | Predicate -> "predicate"
  | Type -> "type"
  | Logic_type -> "logic type"
  | Logic_constructor -> "logic constructor"
  | Axiomatic -> "axiomatic"
  | Lemma -> "lemma"

let prefix = function
  | Behavior -> "B"
  | Enum -> "E"
  | Field -> "M"
  | Formal_var -> "f"
  | Formal_in_type -> "ft"
  | Function -> "F"
  | Global_var -> "G"
  | Label -> "L"
  | String_literal -> "LS"
  | Local_var -> "V"
  | Logic_var -> "LV"
  | Predicate -> "P"
  | Type -> "T"
  | Logic_type -> "LT"
  | Logic_constructor -> "LC"
  | Axiomatic -> "A"
  | Lemma -> "LE"

include Datatype.Make_with_collections
    (struct
      type t = k
      let name = "Obfuscator.kind"
      let reprs = [ Global_var ]
      let hash (k:k) = Hashtbl.hash k
      let equal (k1:k) k2 = k1 = k2
      let compare (k1:k) k2 = Stdlib.compare k1 k2

      let copy = Datatype.identity
      let structural_descr = Structural_descr.t_abstract
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
      let pretty fmt k = Format.fprintf fmt "%s" (name_of_kind k)
    end)
