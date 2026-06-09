(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Logic_ptree
open Cil_types
open Cil_datatype

(* -------------------------------------------------------------------------- *)
(* ---  Region Specifications                                             --- *)
(* -------------------------------------------------------------------------- *)

type path =
  | Alias of location * term_lval
  | Field of location * term_lval * fieldinfo * fieldinfo
  | Range of location * term * typ * term * term

type region = {
  named : string ;
  paths : path list ;
  flags : Attr.flags ;
}

(* -------------------------------------------------------------------------- *)
(* ---  Printers                                                          --- *)
(* -------------------------------------------------------------------------- *)

let pp_named fmt a = if a <> "" then Format.fprintf fmt "%s: " a

let pp_path fmt = function
  | Alias(_,lv) ->
    Printer.pp_term_lval fmt lv
  | Field(_,lv,f,g) ->
    let field lv f = Logic_const.addTermOffsetLval (TField(f,TNoOffset)) lv in
    Format.fprintf fmt "%a..%a"
      Printer.pp_term_lval (field lv f)
      Printer.pp_term_lval (field lv g)
  | Range(_,p,_,a,b) ->
    Format.fprintf fmt "%a[%a..%a]"
      Printer.pp_term p
      Printer.pp_term a
      Printer.pp_term b

let pp_region fmt r =
  match r.paths with
  | [] -> Format.pp_print_string fmt ""
  | p::ps ->
    begin
      Format.fprintf fmt "@[<hov 2>" ;
      pp_named fmt r.named ; pp_path fmt p ;
      List.iter (Format.fprintf fmt ",@ %a" pp_path) ps ;
      Attr.iter (Format.fprintf fmt ",@ \\%a" Attr.pp_attr) r.flags ;
      Format.fprintf fmt "@]" ;
    end

let pp_regions fmt = function
  | [] -> Format.pp_print_string fmt ""
  | r::rs ->
    begin
      Format.fprintf fmt "@[<hv 0>" ;
      pp_region fmt r ;
      List.iter (Format.fprintf fmt ",@ %a" pp_region) rs ;
      Format.fprintf fmt "@]" ;
    end

(* -------------------------------------------------------------------------- *)
(* ---  Parsing Environment                                               --- *)
(* -------------------------------------------------------------------------- *)

type env = {
  context: Logic_typing.typing_context ;
  mutable esource: Filepos.t ;
  mutable enamed: string ;
  mutable eflags: Attr.flags ;
  mutable rpaths: path list ;
  mutable regions: region list ;
}

let error (env:env) ~loc msg = env.context.error loc msg

(* -------------------------------------------------------------------------- *)
(* ---  Syntactic Filter                                                  --- *)
(* -------------------------------------------------------------------------- *)

let lrange env (e: lexpr) =
  match e.lexpr_node with
  | PLrange(None,None) -> ()
  | _ -> error env ~loc:e.lexpr_loc "Range [..] expected"

let rec lpath env (e: lexpr) =
  let loc = e.lexpr_loc in
  match e.lexpr_node with
  | PLvar _ -> ()
  | PLdot( p , _ ) | PLarrow( p , _ )
  | PLunop( Ustar , p ) | PLunop( Uamp , p ) -> lpath env p
  | PLbinop( p , Badd , rg ) | PLarrget(p,rg) -> lpath env p ; lrange env rg
  | PLcast( _ , p ) -> lpath env p
  | _ ->
    error env ~loc "Unexpected l-value for region spec"

(* -------------------------------------------------------------------------- *)
(* ---  Parsers                                                           --- *)
(* -------------------------------------------------------------------------- *)

let parse_term env t =
  let open Logic_typing in
  let g = env.context in
  g.type_term g g.pre_state t

let parse_lval env p =
  let t = parse_term env p in
  match t.term_node with
  | TLval lv -> lv
  | _ -> error env ~loc:p.lexpr_loc "Expected l-value for region path"

let parse_integer env p =
  let v = parse_term env p in
  if not @@ Ast_types.is_logic_integral v.term_type then
    error env ~loc:p.lexpr_loc "Expected integer term for object bounds" ; v

let parse_pointer env p =
  let loc = p.lexpr_loc in
  let a = parse_term env p in
  let te =
    match Ast_types.unroll_logic a.term_type with
    | Ctype { tnode = TPtr te } -> te
    | _ -> error env ~loc "Expected pointer l-value for region object"
  in te,a

let rec last_field = function
  | TNoOffset | TModel _ -> raise Not_found
  | TField(fd,TNoOffset) -> TNoOffset, fd
  | TField(f0,ofs) -> let ofs,fd = last_field ofs in TField(f0,ofs), fd
  | TIndex(k0,ofs) -> let ofs,fd = last_field ofs in TIndex(k0,ofs), fd

let parse_field env p =
  try
    let h,ofs = parse_lval env p in
    let ofs,fd = last_field ofs in
    if not fd.fcomp.cstruct then
      error env ~loc:p.lexpr_loc "Expected struct field for range path" ;
    (h,ofs),fd
  with Not_found ->
    error env ~loc:p.lexpr_loc "Expected field l-value for range path"

let garbage = Attr.(add `Garbage empty)

let applies flags = function
  | Range _ -> true
  | Alias(_,(TVar { lv_origin = Some v },_)) ->
    flags = garbage && v.vformal && Ast_types.is_struct_or_union v.vtype
  | Alias _ | Field _ -> false

let flush source env =
  if env.eflags <> Attr.empty &&
     not @@ List.exists (applies env.eflags) env.rpaths
  then
    Options.warning ~source:env.esource "%a has no object to apply on"
      Attr.pretty env.eflags ;
  if env.rpaths <> [] then
    begin
      env.regions <- {
        named = env.enamed ;
        flags = env.eflags ;
        paths = List.rev env.rpaths ;
      } :: env.regions ;
      env.esource <- source ;
      env.rpaths <- [] ;
      env.eflags <- Attr.empty ;
    end

let rec parse_region (env:env) p =
  match p.lexpr_node with
  | PLvar "\\nullable"  -> env.eflags <- Attr.add `Nullable  env.eflags
  | PLvar "\\allocated" -> env.eflags <- Attr.add `Allocated env.eflags
  | PLvar "\\garbage"   -> env.eflags <- Attr.add `Garbage   env.eflags
  | PLvar "\\readonly"  -> env.eflags <- Attr.add `Readonly  env.eflags
  | PLnamed( name , p ) ->
    flush (fst p.lexpr_loc) env ;
    env.enamed <- name ;
    parse_region env p
  | PLrange(Some a,Some b) ->
    let l1,f = parse_field env a in
    let l2,g = parse_field env b in
    if not (Term_lval.equal l1 l2) then
      error env ~loc:p.lexpr_loc "Field range from different region paths" ;
    env.rpaths <- Field(p.lexpr_loc,l1,f,g) :: env.rpaths
  | PLarrget(p,{ lexpr_node = PLrange(Some a,Some b) }) ->
    let te,q = parse_pointer env p in
    let a = parse_integer env a in
    let b = parse_integer env b in
    env.rpaths <- Range(p.lexpr_loc,q,te,a,b) :: env.rpaths
  | PLunop(Ustar,p) ->
    let te,q = parse_pointer env p in
    let zero = Logic_const.tinteger ~loc:p.lexpr_loc 0 in
    env.rpaths <- Range(p.lexpr_loc,q,te,zero,zero) :: env.rpaths
  | _ ->
    let lv = lpath env p ; parse_lval env p in
    env.rpaths <- Alias(p.lexpr_loc,lv) :: env.rpaths

(* -------------------------------------------------------------------------- *)
(* --- Spec Typechecking & Printing                                       --- *)
(* -------------------------------------------------------------------------- *)

let kspec = ref 0
let registry = Hashtbl.create 0

let of_extid id = try Hashtbl.find registry id with Not_found -> []
let of_extension = function
  | { ext_name="region" ; ext_kind = Ext_id k } -> of_extid k
  | _ -> []
let of_code_annot = function
  | { annot_content = AExtended(_,_,e) } -> of_extension e
  | _ -> []

let of_behavior bhv = List.concat_map of_extension bhv.b_extended

let typecheck typing_context loc ps =
  let env = {
    esource = fst loc ;
    enamed = "" ;
    eflags = Attr.empty ;
    context = typing_context ;
    rpaths = [] ; regions = [] ;
  } in
  List.iter (parse_region env) ps ;
  let id = !kspec in incr kspec ;
  flush (fst loc) env ;
  Hashtbl.add registry id @@ List.rev env.regions ;
  Ext_id id

let printer _pp fmt = function
  | Ext_id k ->
    let rs  = try Hashtbl.find registry k with Not_found -> [] in
    pp_regions fmt rs
  | _ -> ()

let () =
  begin
    Acsl_extension.register_behavior
      ~plugin:"region" "region" typecheck ~printer false ;
    Acsl_extension.register_code_annot
      ~plugin:"region" "alias" typecheck ~printer false ;
  end


(* -------------------------------------------------------------------------- *)
