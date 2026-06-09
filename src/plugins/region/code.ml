(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype
open Memory

module Vmap = Varinfo.Map

(* -------------------------------------------------------------------------- *)
(* ---  L-Values & Expressions                                            --- *)
(* -------------------------------------------------------------------------- *)

type value = node option

let pointer v =
  match v with
  | Some p -> p
  | None -> Options.fatal "Not a pointer value"

let rec add_lval (m:map) (s:stmt) (lv:lval) : node =
  let h = fst lv in
  add_loffset m s (add_lhost m s h) (Cil.typeOfLhost h) (snd lv)

and add_lhost (m:map) (s:stmt) = function
  | Var x -> Memory.add_cvar m x
  | Mem e -> pointer @@ add_exp m s e

and add_loffset (m:map) (s:stmt) (r:node) (ty:typ)= function
  | NoOffset -> r
  | Field(fd,ofs) ->
    add_loffset m s (add_field r fd) fd.ftype ofs
  | Index(e,ofs) ->
    let elt = Ast_types.direct_element_type ty in
    ignore @@ add_exp m s e ;
    add_loffset m s (add_index r elt) elt ofs

and add_value m s e = ignore (add_exp m s e)

and add_exp (m: map) (s:stmt) (e:exp) : value =
  match e.enode with

  | AddrOf lv | StartOf lv -> Some (add_lval m s lv)
  | Lval lv ->
    let rv = add_lval m s lv in
    Memory.add_read rv (Lval(s,lv)) ;
    Memory.add_value rv @@ Cil.typeOfLval lv

  | BinOp((PlusPI|MinusPI),p,k,tr) ->
    add_value m s k ;
    let vp = add_exp m s p in
    let te = Ast_types.pointed_type tr in
    Memory.add_shift (pointer vp) (Exp(s,e)) te ; vp

  | UnOp(_,e,_) ->
    add_value m s e ; None

  | BinOp(_,a,b,_) ->
    add_value m s a ; add_value m s b ; None

  | CastE(_,p) ->
    add_exp m s p

  | Const _
  | SizeOf _ | SizeOfE _
  | AlignOf _ | AlignOfE _
    -> None

(* -------------------------------------------------------------------------- *)
(* --- Compound L-Values                                                  --- *)
(* -------------------------------------------------------------------------- *)

let is_comp lv =
  Ast_types.is_struct_or_union @@ Cil.typeOfLval lv

(* -------------------------------------------------------------------------- *)
(* --- Initializers                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec add_init (m:map) (s:stmt) (lv:lval) (iv:init) =
  match iv with

  | SingleInit { enode = Lval le } when is_comp le ->
    let r = add_lval m s lv in
    let v = add_lval m s le in
    Memory.merge r v

  | SingleInit e ->
    let r = add_lval m s lv in
    let tv = Cil.typeOfLval lv in
    let tr = if Ast_types.is_array tv then Ast_types.element_type tv else tv in
    let acs = Access.Init(s,lv,e) in
    Memory.add_init r acs tr ;
    Option.iter (Memory.add_points_to r) (add_exp m s e)

  | CompoundInit(_,fvs) ->
    List.iter
      (fun (ofs,iv) ->
         let lv = Cil.addOffsetLval ofs lv in
         add_init m s lv iv
      ) fvs

(* -------------------------------------------------------------------------- *)
(* --- Instructions                                                       --- *)
(* -------------------------------------------------------------------------- *)

let add_write ~map ~stmt ~acs (r:node) (e:exp) =
  Memory.add_write r acs ;
  match e.enode with
  | Lval le when is_comp le ->
    let v = add_lval map stmt le in
    Memory.merge r v
  | _ ->
    let v = add_exp map stmt e in
    Option.iter (Memory.add_points_to r) v

let add_function (m:map) (s:stmt) (f:lhost) =
  match f with
  | Var _vf -> ()
  | Mem e -> add_value m s e

let add_returned (m:map) (s:stmt) lv =
  let r = add_lval m s lv in
  Memory.add_write r (Lval(s,lv)) ; r

let add_kf_call m s r kf vs =
  Populate_spec.populate_funspec kf [`Assigns] ;
  let spec = Annotations.funspec kf in
  let formals =
    let rec bind fm xs vs =
      match xs , vs with
      | [] , _ -> fm
      | x::xs , [] -> bind (Vmap.add x pure fm) xs []
      | x::xs , v::vs -> bind (Vmap.add x v fm) xs vs in
    bind Vmap.empty (Kernel_function.get_formals kf) vs
  in Annot.add_spec ~map:m ~called:s ~kf ~formals ~result:r spec

let add_call m s r fct es =
  let vs = List.map (fun e -> Domain.scalar @@ add_exp m s e) es in
  match Kernel_function.get_called fct with
  | Some kf -> add_kf_call m s r kf vs
  | None ->
    begin
      match Dyncall.get s with
      | Some(_,kfs) ->
        List.iter (fun kf -> add_kf_call m s r kf vs) kfs
      | None ->
        Options.not_yet_implemented
          ~source:(fst @@ Stmt.loc s)
          "Dynamic call without @call annotation"
    end

let add_instr ~map ~stmt = function
  | Skip _ -> ()

  | Set(lv,e,_) ->
    let r = add_lval map stmt lv in
    add_write ~map ~stmt ~acs:(Lval(stmt,lv)) r e ;

  | Local_init(x,AssignInit iv,_) ->
    add_init map stmt (Var x,NoOffset) iv

  | Local_init(x,ConsInit (vf,args,kind), loc) ->
    let r = add_cvar map x in
    Memory.add_init r (Lval (stmt,Cil.var x)) x.vtype ;
    Cil.treat_constructor_as_func
      begin fun _res fct args _loc ->
        add_function map stmt fct;
        List.iter (add_value map stmt) args ;
        add_call map stmt (Some r) fct args
      end x vf args kind loc

  | Call(lr,f,es,_) ->
    add_function map stmt f;
    let r = Option.map (add_returned map stmt) lr in
    add_call map stmt r f es

  | Code_annot _ -> ()
  | Asm _ ->
    Options.warning ~source:(fst @@ Stmt.loc stmt)
      "Inline assembly not supported (ignored)"

(* -------------------------------------------------------------------------- *)
(* --- Statements                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec add_stmt ~map ~kf ~result stmt =
  List.iter
    (Annot.add_code_annot ~map ~kf ~stmt ~result)
    (Annotations.code_annot stmt) ;
  match stmt.skind with
  | Instr instr -> add_instr ~map ~stmt instr ;
  | Return(None,_) -> ()
  | Return(Some e,_) ->
    add_write ~map ~stmt ~acs:(Ret(stmt,e)) (Option.get result) e
  | Goto _ | Break _ | Continue _ -> ()
  | If(e,sthen,selse,_) ->
    add_value map stmt e ;
    add_block ~map ~kf ~result sthen ;
    add_block ~map ~kf ~result selse ;
  | Switch(e,b,_,_) ->
    add_value map stmt e ;
    add_block ~map ~kf ~result b ;
  | Block b -> add_block ~map ~kf ~result b
  | Loop(_,b,_,_,_) -> add_block ~map ~kf ~result b
  | UnspecifiedSequence s ->
    add_block ~map ~kf ~result @@ Cil.block_from_unspecified_sequence s
  | Throw(exn,_) -> Option.iter (fun (e,_) -> add_value map stmt e) exn
  | TryCatch(b,handlers,_)  ->
    add_block ~map ~kf ~result b ;
    List.iter
      (fun (c,b) ->
         add_catch ~map ~kf ~result c ;
         add_block ~map ~kf ~result b
      ) handlers
  | TryExcept(a,(ks,e),b,_) ->
    add_block ~map ~kf ~result a ;
    List.iter (add_instr ~map ~stmt) ks ;
    add_value map stmt e ;
    add_block ~map ~kf ~result b ;
  | TryFinally(a,b,_) ->
    add_block ~kf ~map ~result a ;
    add_block ~kf ~map ~result b ;

and add_catch ~map ~kf ~result = function
  | Catch_all -> ()
  | Catch_exn(_,xbs) ->
    List.iter (fun (_,b) -> add_block ~map ~kf ~result b) xbs

and add_block ~map ~kf ~result b =
  List.iter (add_stmt ~map ~kf ~result) b.bstmts

(* -------------------------------------------------------------------------- *)
(* --- Function                                                           --- *)
(* -------------------------------------------------------------------------- *)

type domain = map

let domain kf =
  let map = Memory.create () in
  let result =
    if Kernel_function.returns_void kf
    then None
    else  Some (Memory.add_result map) in
  begin
    try
      let spec = Annotations.funspec kf in
      Annot.add_spec ~map ~kf ~result spec ;
    with Annotations.No_funspec _ -> ()
  end ;
  begin
    try
      let decl = Kernel_function.get_definition kf in
      add_block ~map ~kf ~result decl.sbody ;
    with Kernel_function.No_Definition -> ()
  end ;
  Memory.lock map ; map

(* -------------------------------------------------------------------------- *)
