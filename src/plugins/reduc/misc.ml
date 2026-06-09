(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Options = Reduc_options


exception Not_implemented of string

let not_implemented ~what =
  Options.warning "Not implemented: `%s'. Ignoring." what


let emitter =
  Emitter.create
    "Reduc"
    [ Emitter.Code_annot; Emitter.Property_status ]
    ~correctness:[]
    ~tuning:[]

(* ******************************************************)
(*      Annotations and function contracts helpers      *)
(* ******************************************************)
let validate_ip ip =
  Property_status.emit emitter ~hyps:[] ip Property_status.True

let assert_and_validate ~kf stmt p =
  let p =  { tp_kind = Assert ; tp_statement = p } in
  let annot = Logic_const.new_code_annotation (AAssert([], p)) in
  Annotations.add_code_annot emitter ~kf stmt annot ;
  List.iter
    validate_ip
    (Property.ip_of_code_annot kf stmt annot)
