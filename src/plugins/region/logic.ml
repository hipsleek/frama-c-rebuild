(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

open Spec
open Memory
open Domain

(* -------------------------------------------------------------------------- *)
(* ---  Process ACSL logic terms & predicates                             --- *)
(* -------------------------------------------------------------------------- *)

type env = {
  map : map ;
  result : node option ;
  formals : domain Varinfo.Map.t ;
  context : Access.clause ;
}

let merge a b = Memory.merge a b ; min a b

let pointer (d:domain) : node =
  match Domain.pointed merge d with
  | Some p -> p
  | None -> Options.fatal "Not a pointer value"

type lv_value =
  | VAL of domain
  | VAR of varinfo

let logic_var env lv =
  match lv.lv_origin with
  | None -> VAL (Memory.add_lvar env.map lv)
  | Some x ->
    if x.vformal then
      try VAL (Varinfo.Map.find x env.formals) with Not_found -> VAR x
    else VAR x

(* Load a complete value at l-value lv which has type ty and lives in region r *)
let rec load env lv (ty,r) : domain =
  match Ast_types.unroll_node ty with
  | TArray(te,_) ->
    let re = Memory.add_index r te in
    let ofs = TIndex (Logic_const.trange (None,None), TNoOffset) in
    let lve = Logic_const.addTermOffsetLval ofs lv in
    array (load env lve (te,re))
  | TComp { cfields } ->
    let add_field d fd =
      let ofs = TField (fd,TNoOffset) in
      let lvf = Logic_const.addTermOffsetLval ofs lv in
      merge_domain d
      @@ Domain.field fd
      @@ load env lvf
      @@ (fd.ftype, Memory.add_field r fd)
    in List.fold_left add_field pure @@ Option.value ~default:[] cfields
  | _ ->
    let acs = Access.Term (env.context, lv) in
    Memory.add_read r acs ;
    Domain.scalar @@ Memory.add_value r ty

let rterm = ref (fun _ _ -> assert false)

let rec addr_offset ~loc (env:env) (ty:typ) (r:node) = function
  | TNoOffset -> ty,r
  | TModel _ ->
    Options.not_yet_implemented ~source:(fst loc) "Unsupported model fields"
  | TField (f,offset) ->
    addr_offset ~loc env f.ftype (Memory.add_field r f) offset
  | TIndex(k,offset) ->
    ignore @@ !rterm env k ;
    let te = Ast_types.direct_element_type ty in
    addr_offset ~loc env te (Memory.add_index r ty) offset

let rec term_offset ~loc (env:env) (d:domain) = function
  | TNoOffset -> d
  | TModel _ ->
    Options.not_yet_implemented ~source:(fst loc) "Unsupported model fields"
  | TField (f,offset) ->
    term_offset ~loc env (Domain.get_field merge d f) offset
  | TIndex(k,offset) ->
    ignore @@ !rterm env k ;
    term_offset ~loc env (Domain.get_index merge d) offset

let add_term_lval ~loc (env:env) (lv : term_lval) : domain =
  let lhost, loffset = lv in
  match lhost with
  | TMem e ->
    let rh = pointer (!rterm env e) in
    let te = Logic_typing.ctype_of_pointed e.term_type in
    load env lv @@ addr_offset ~loc env te rh loffset
  | TResult ty ->
    begin match env.result with
      | None -> Options.fatal "\\result undefined" ;
      | Some node ->
        load env lv @@ addr_offset ~loc env ty node loffset
    end
  | TVar v ->
    begin match logic_var env v with
      | VAL d -> term_offset ~loc env d loffset
      | VAR x ->
        let r = Memory.add_cvar env.map x in
        load env lv @@ addr_offset ~loc  env x.vtype r loffset
    end

let add_addr_lval ~loc (env:env) ?(garbage=false) (lv : term_lval) : typ * node =
  let lhost, loffset = lv in
  match lhost with
  | TMem e ->
    let rh = pointer (!rterm env e) in
    let te = Logic_typing.ctype_of_pointed e.term_type in
    addr_offset ~loc env te rh loffset
  | TResult ty ->
    begin match env.result with
      | None -> Options.fatal "\\result undefined" ;
      | Some node -> addr_offset ~loc env ty node loffset
    end
  | TVar v ->
    begin match logic_var env v with
      | VAL _ ->
        Options.fatal "address of logic value (%a)" Printer.pp_term_lval lv ;
      | VAR x ->
        let garbage =
          garbage && x.vformal && Ast_types.is_struct_or_union x.vtype in
        let r = Memory.add_cvar ~garbage env.map x in
        addr_offset ~loc env x.vtype r loffset
    end

let rec update_offset ~loc (env:env) loffest d =
  match loffest with
  | TNoOffset -> d
  | TField(fd,offset) -> Domain.field fd @@ update_offset ~loc env offset d
  | TModel _ ->
    Options.not_yet_implemented ~source:(fst loc) "Unsupported model fields"
  | TIndex(_,offset) -> Domain.array @@ update_offset ~loc env offset d

let call map (l:logic_info) (ds:domain list) : domain =
  let sigma = ref Domain.empty in
  let unify = Domain.unify merge sigma in
  List.iter2 (fun x -> unify (Memory.add_lvar map x)) l.l_profile ds ;
  Domain.subst !sigma @@ Memory.add_logic map l

let cons map (c:logic_ctor_info) (ds:domain list) : domain =
  let sigma = ref Domain.empty in
  let unify = Domain.unify merge sigma in
  let fresh () = Memory.fresh map in
  List.iter2 (fun t -> unify (of_ltype fresh t)) c.ctor_params ds ;
  Domain.logic c.ctor_type @@
  List.map (Domain.getvar !sigma) c.ctor_type.lt_params

let iadd_logic_var m v = ignore @@ add_lvar m v

let rec add_term (env:env) (t:term) : domain =
  match t.term_node with
  | TLval lval ->
    add_term_lval ~loc:t.term_loc env lval
  | TAddrOf lval | TStartOf lval ->
    ptr @@ snd @@ add_addr_lval ~loc:t.term_loc env lval
  | Tif (b,ct,cf) ->
    add_predicate env b ;
    let dt = add_term env ct in
    let df = add_term env cf in
    merge_domain dt df
  | TUnOp(_,t) | TCast(_,_,t) | Tat(t,_) -> add_term env t
  | TBinOp ((PlusPI|MinusPI),t1,t2) ->
    let d1 = add_term env t1 in
    let d2 = add_term env t2 in
    merge_domain d1 d2
  | TBinOp(_,t1,t2) -> iadd_term env t1 ; iadd_term env t2 ; pure
  | Tbase_addr(_,t) | Toffset (_,t) | Tblock_length(_,t) ->
    iadd_term env t ; pure
  | TUpdate(lv,o,v) ->
    merge_domain (add_term env lv) @@
    update_offset ~loc:t.term_loc env o @@ add_term env v
  | Tunion ts | Tinter ts ->
    List.fold_left (fun d t -> merge_domain d @@ add_term env t) pure ts
  | Tcomprehension(t,q,p) ->
    Option.iter (add_predicate env) p ;
    List.iter (iadd_logic_var env.map) q ;
    add_term env t
  | Tapp(f,_,ts) -> call env.map f @@ List.map (add_term env) ts
  | TDataCons(c,ts) -> cons env.map c @@ List.map (add_term env) ts
  | Tlambda(q,t) ->
    Domain.arrow (List.map (Memory.add_lvar env.map) q) @@ add_term env t
  | Tlet({ l_body ; l_var_info=v },b) ->
    begin match l_body with
      | LBterm a ->
        let dv = add_lvar env.map v in
        let da = add_term env a in
        let sigma = ref Domain.empty in
        Domain.unify merge sigma da dv ;
        Domain.subst !sigma @@ add_term env b
      | LBpred p ->
        iadd_logic_var env.map v ;
        add_predicate env p ;
        add_term env t
      | _ ->
        Options.not_yet_implemented
          ~source:(fst t.term_loc) "Unsupported complex \\let"
    end
  | TConst _  | TSizeOf _ | TSizeOfE _ | TAlignOf _ | TAlignOfE _
  | Tnull | Tempty_set | Ttypeof _ | Ttype _  | Trange _ -> pure

and iadd_term env t = ignore @@ add_term env t

and add_predicate (env:env) (p:predicate) = match p.pred_content with
  | Pfalse | Ptrue -> ()
  | Pseparated ts -> List.iter (iadd_term env) ts
  | Prel(_,t1,t2) | Pfresh(_,_,t1,t2) ->
    iadd_term env t1 ;
    iadd_term env t2 ;
  | Pand(p1,p2) | Por(p1,p2) | Pxor(p1,p2) | Piff(p1,p2) | Pimplies(p1,p2) ->
    add_predicate env p1 ;
    add_predicate env p2 ;
  | Pnot p | Pat(p,_) -> add_predicate env p
  | Pif(c,pt,pf) ->
    add_predicate env c ;
    add_predicate env pt ;
    add_predicate env pf ;
  | Pobject_pointer(_,t) | Pvalid(_,t) | Pvalid_read(_,t) | Paligned(t, _)
  | Pvalid_function t | Pinitialized(_,t) | Pdangling(_,t)
  | Pallocable(_,t) | Pfreeable(_,t) -> iadd_term env t
  | Pforall (q,p) | Pexists (q,p) ->
    List.iter (iadd_logic_var env.map) q ; add_predicate env p
  | Plet({ l_var_info = v ; l_body = LBterm t ; },p2) ->
    let dv = add_lvar env.map v in
    let dt = add_term env t in
    let sigma = ref empty in
    Domain.unify merge sigma dt dv ;
    add_predicate env p2
  | Plet({ l_var_info = v ; l_body = LBpred p1 ; },p2) ->
    iadd_logic_var env.map v ;
    add_predicate env p1 ;
    add_predicate env p2
  | Plet({ l_body = LBnone ; },p2) ->
    add_predicate env p2
  | Plet _ ->
    Options.not_yet_implemented
      ~source:(fst p.pred_loc) "Unsupported complex \\let-bindings"
  | Papp(f,_,ts) -> ignore @@ call env.map f @@ List.map (add_term env) ts

let () = rterm := add_term

(* -------------------------------------------------------------------------- *)
(* ---  Process ACSL region annotations                                   --- *)
(* -------------------------------------------------------------------------- *)

let add_path (env: env) Spec.{ named ; flags } = function
  | Spec.Alias(loc,lv) ->
    let garbage = Attr.mem `Garbage flags in
    snd @@ add_addr_lval ~loc ~garbage env lv
  | Spec.Field(loc,lv,f,g) ->
    let r = snd @@ add_addr_lval ~loc env lv in
    Memory.add_field_range r f g
  | Spec.Range(_,ptr,typ,inf,sup) ->
    iadd_term env inf ; iadd_term env sup ;
    let rp = pointer @@ add_term env ptr in
    let re = Memory.add_index rp typ in
    let ip = match env.context with Prop ip -> ip | _ -> assert false in
    let root = Root { ip ; named ; ptr ; typ ; inf ; sup ; flags } in
    Memory.add_root env.map re root ; re

let add_region (env: env) (r : Spec.region) =
  let rs = List.map (add_path env r) r.paths in
  merge_all @@
  if r.named = "" then rs else add_label env.map r.named :: rs

(* -------------------------------------------------------------------------- *)
(* ---  Process ACSL logic                                                --- *)
(* -------------------------------------------------------------------------- *)

let add_body map (l:logic_info) (d:domain) =
  let env = {
    map ;
    result = None ;
    formals = Varinfo.Map.empty ;
    context = Body l ;
  } in
  match l.l_body with
  | LBnone -> ()
  | LBpred p -> add_predicate env p
  | LBterm t -> ignore @@ Memory.merge_domain d (add_term env t)
  | LBreads ts ->
    List.iter (fun t -> iadd_term env t.it_content) ts
  | LBinductive l ->
    List.iter (fun (_,_,_,t) -> add_predicate env t) l

let () = Memory.body[@alert "-internal"] := add_body

(* -------------------------------------------------------------------------- *)
