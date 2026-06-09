(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* ********************************************************************** *)
(* Statements *)
(* ********************************************************************** *)

let stmt sk = Cil.mkStmt ~valid_sid:true sk
let instr i = stmt (Instr i)
let block_stmt blk = stmt (Block blk)
let block_from_stmts stmts = block_stmt (Cil.mkBlock stmts)
let call_instr ~loc ?result lv args = instr (Call(result, lv, args, loc))

let assigns ~loc ~result e = instr (Set(result, e, loc))

let assigns_field ~loc vi name value =
  let ty = vi.vtype in
  let compinfo =
    match Ast_types.unroll_node ty with
    | TComp compinfo -> compinfo
    | _ ->
      Options.fatal
        "type of %a (%a) is not a structure"
        Printer.pp_varinfo vi
        Printer.pp_typ ty
  in
  let field =
    try
      Cil.getCompField compinfo name
    with Not_found ->
      Options.fatal
        "Unable to find field '%s' in structure '%a'"
        name
        Printer.pp_typ ty
  in
  let result = Var vi, (Field (field, NoOffset)) in
  assigns ~loc ~result value

let if_stmt ~loc ~cond ?(else_blk=Cil.mkBlock []) then_blk =
  stmt (If (cond, then_blk, else_blk, loc))

let break ~loc = stmt (Break loc)

let struct_local_init ~loc vi fields =
  vi.vdefined <- true;
  let ty = vi.vtype in
  let compinfo =
    match Ast_types.unroll_node ty with
    | TComp compinfo -> compinfo
    | _ ->
      Options.fatal
        "type of %a (%a) is not a structure"
        Printer.pp_varinfo vi
        Printer.pp_typ ty
  in
  let fields =
    List.map
      (fun (name, e) ->
         try
           let field = Cil.getCompField compinfo name in
           Field (field, NoOffset), SingleInit e
         with Not_found ->
           Options.fatal
             "Unable to find field '%s' in structure '%a'"
             name
             Printer.pp_typ ty)
      fields
  in
  instr (Local_init (vi, AssignInit (CompoundInit (ty, fields)), loc))

let block stmt b = match b.bstmts with
  | [] ->
    (match stmt.skind with
     | Instr(Skip _) -> stmt
     | _ -> assert false)
  | [ s ] -> s
  |  _ :: _ -> block_stmt b

(* ********************************************************************** *)
(* E-ACSL specific code *)
(* ********************************************************************** *)

let do_call ~loc ?result vi args =
  vi.vreferenced <- true;
  let make_args ~variadic args param_ty =
    let rec make_rev_args res args param_ty =
      match args, param_ty with
      | arg :: args_tl, (_, ty, _) :: param_ty_tl ->
        let e =
          match ty.tnode, Ast_types.unroll_node (Cil.typeOf arg), arg.enode with
          | TPtr _, TArray _, Lval lv -> Cil.new_exp ~loc (StartOf lv)
          | TPtr _, TArray _, _ -> assert false
          | _, _, _ -> arg
        in
        let e = Cil.mkCast ~force:false ~newt:ty e in
        make_rev_args (e :: res) args_tl param_ty_tl
      | arg :: args_tl, [] when variadic -> make_rev_args (arg :: res) args_tl []
      | [], [] -> res
      | _, _ ->
        Options.fatal
          "Mismatch between the number of expressions given and the number \
           of arguments in the signature when calling function '%s'"
          vi.vname
    in
    List.rev (make_rev_args [] args param_ty)
  in
  let args = match Ast_types.unroll_node vi.vtype with
    | TFun (_, Some params, variadic) -> make_args ~variadic args params
    | TFun (_, None, _) -> []
    | _ -> assert false
  in
  call_instr ~loc ?result (Var vi) args

let call ~loc ?result fname args =
  let kf =
    try Globals.Functions.find_by_name fname
    with Not_found ->
      Options.fatal "Unable to find function '%s'" fname
  in
  let vi = Globals.Functions.get_vi kf in
  do_call ~loc ?result vi args

let rtl_call ~loc ?result ?(prefix=Functions.RTL.api_prefix) fname args =
  let fname = prefix ^ fname in
  let vi =
    try Rtl.Symbols.find_vi fname
    with Rtl.Symbols.Unregistered _ as exn ->
    try Builtins.find fname
    with Not_found ->
      raise exn
  in
  do_call ~loc ?result vi args

(* ************************************************************************** *)
(** {2 Handling the E-ACSL's C-libraries, part II} *)
(* ************************************************************************** *)

let full_init_stmt vi =
  let loc = vi.vdecl in
  rtl_call ~loc "full_init" [ Cil.evar ~loc vi ]

let initialize ~loc (host, offset as lv) = match host, offset with
  | Var _, NoOffset ->
    rtl_call ~loc "full_init" [ Cil.mkAddrOf ~loc lv ]
  | _ ->
    let typ = Cil.typeOfLval lv in
    rtl_call ~loc
      "initialize"
      [ Cil.mkAddrOf ~loc lv; Cil.new_exp ~loc (SizeOf typ) ]

let named_store_stmt name ?str_size vi =
  let ty = Ast_types.unroll vi.vtype in
  let loc = vi.vdecl in
  let store = rtl_call ~loc name in
  match ty.tnode, str_size with
  | TArray (_, Some _), None ->
    store [ Cil.evar ~loc vi; Cil.sizeOf ~loc ty ]
  | TPtr { tnode = TInt IChar }, Some size ->
    store [ Cil.evar ~loc vi ; size ]
  | TPtr _, Some size ->
    (* a VLA that has been converted into a pointer by the kernel *)
    store [ Cil.evar ~loc vi; size ]
  | _, None ->
    store [ Cil.mkAddrOfVi vi ; Cil.sizeOf ~loc ty ]
  | _, Some size ->
    Options.fatal
      "unexpected types for arguments of function '%s': \
       %s got type %a, while representing a memory block of %a bytes"
      name
      vi.vname
      Printer.pp_typ ty
      Printer.pp_exp size

let store_stmt ?str_size vi =
  named_store_stmt "store_block" ?str_size vi

let duplicate_store_stmt ?str_size vi =
  named_store_stmt "store_block_duplicate" ?str_size vi

let delete_stmt ?(is_addr=false) vi =
  let loc = vi.vdecl in
  let mk = rtl_call ~loc "delete_block" in
  match is_addr, Ast_types.unroll_node vi.vtype with
  | _, TArray (_, Some _) | true, _ -> mk [ Cil.evar ~loc vi ]
  | _ -> mk [ Cil.mkAddrOfVi vi ]

let mark_readonly ~loc exp =
  rtl_call ~loc "mark_readonly" [ exp ]

let set_unsound_verdict ~loc =
  let sound_verdict_vi = Prepare_ast.sound_verdict () in
  assigns ~loc ~result:(Cil.var sound_verdict_vi) (Cil.zero ~loc)
