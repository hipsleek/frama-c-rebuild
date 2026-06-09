(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Basic_blocks
open Basic_alloc
open Cil_types
open Logic_const

let function_name = "malloc"

let unexpected = Options.fatal "Stdlib.Malloc: unexpected: %s"

let generate_global_assigns loc ptr_type size =
  let assigns_result = assigns_result ~loc ptr_type [ size ] in
  let assigns_heap = assigns_heap [ size ] in
  Writes [ assigns_result ; assigns_heap ]

let make_behavior_allocation loc ptr_type size =
  let assumes = [ is_allocable ~loc size ] in
  let assigns = generate_global_assigns loc ptr_type size in
  let alloc   = allocates_result ~loc ptr_type in
  let ensures = [
    Normal, fresh_result ~loc ptr_type size ;
    Normal, aligned_result ~loc ptr_type
  ] in
  make_behavior ~name:"allocation" ~assumes ~assigns ~alloc ~ensures ()

let make_behavior_no_allocation loc ptr_type size =
  let assumes = [ isnt_allocable ~loc size ] in
  let assigns = Writes [assigns_result ~loc ptr_type []] in
  let ensures = [ Normal, null_result ~loc ptr_type ] in
  let alloc = allocates_nothing () in
  make_behavior ~name:"no_allocation" ~assumes ~assigns ~ensures ~alloc ()

let generate_spec alloc_typ loc { svar = vi } =
  let (csize) = match Cil.getFormalsDecl vi with
    | [ size ] -> size
    | _ -> unexpected "ill-formed fundec in specification generation"
  in
  let size = tlogic_coerce ~loc (cvar_to_tvar csize) Linteger in
  let requires = [ valid_size ~loc alloc_typ size ] in
  let assigns = generate_global_assigns loc (Cil_const.mk_tptr alloc_typ) size in
  let alloc = allocates_result ~loc (Cil_const.mk_tptr alloc_typ) in
  make_funspec [
    make_behavior ~requires ~assigns ~alloc () ;
    make_behavior_allocation loc (Cil_const.mk_tptr alloc_typ) size ;
    make_behavior_no_allocation loc (Cil_const.mk_tptr alloc_typ) size
  ] ()

let generate_prototype alloc_t =
  let name = function_name ^ "_" ^ (string_of_typ alloc_t) in
  let params = [
    ("size", size_t (), [])
  ] in
  name, Cil_const.(mk_tfun (mk_tptr alloc_t) (Some params) false)

let well_typed_call ret _fct args =
  match ret, args with
  | Some ret, [ _ ] ->
    let t = Cil.typeOfLval ret in
    Ast_types.is_ptr t && not (Ast_types.is_void_ptr t) &&
    Cil.isCompleteType (Ast_types.direct_pointed_type t)
  | _ -> false

let key_from_call ret _fct _ =
  match ret with
  | Some ret ->
    let ret_t = Ast_types.unroll_deep (Cil.typeOfLval ret) in
    let ret_t = Ast_types.remove_qualifiers_deep ret_t in
    Ast_types.direct_pointed_type ret_t
  | _ -> unexpected "trying to generate a key on an ill-typed call"

let retype_args _typ args = args
let args_for_original _typ args = args

let () = Transform.register (module struct
    module Hashtbl = Cil_datatype.Typ.Hashtbl
    type override_key = typ

    let function_name = function_name
    let well_typed_call = well_typed_call
    let key_from_call = key_from_call
    let retype_args = retype_args
    let generate_prototype = generate_prototype
    let generate_spec = generate_spec
    let args_for_original = args_for_original
  end)
