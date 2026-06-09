(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype

module C = Cil_types
module Cpp = Cil_printer
module L = Wp_parameters
module T = Why3.Theory
module F = Filepath
module W = Why3
module WConf = Why3.Whyconf
module LB = LogicBuiltins
module LT = Logic_typing

let dkey =
  L.register_category
    ~help:"Importer Why3 -> ACSL"
    "why3:import"

(* -------------------------------------------------------------------------- *)
(* ---    Why3 Environment                                                --- *)
(* -------------------------------------------------------------------------- *)

let why3_env loadpath =
  let main = WConf.get_main @@ WConf.read_config None in
  W.Env.create_env @@ WConf.loadpath main @ F.to_string_list loadpath

let extract_path thname =
  let segments = String.split_on_char '.' thname in
  match List.rev segments with
  | hd :: tl -> hd, List.rev tl
  | [] -> "", []

let of_infix s =
  let rec unwrap_any s = function
    | [] -> s
    | prefix::others ->
      if String.starts_with ~prefix s then
        let n = String.length s in
        let p = String.length prefix in
        Printf.sprintf "(%s)" @@ String.sub s p (n-p)
      else unwrap_any s others
  in unwrap_any s ["prefix ";"infix ";"mixfix "]

let acsl_name (id : W.Ident.ident) =
  let (path,name,scope) = T.restore_path id in
  match List.rev scope with
  | (t::q) ->
    String.concat "::" (path @ name :: List.rev_append q [of_infix t])
  | [] -> ""

let pp_id fmt (id: W.Ident.ident) = Format.pp_print_string fmt id.id_string

(* -------------------------------------------------------------------------*)
(* ---    Types                                                           - *)
(* -------------------------------------------------------------------------*)

type tvars = C.logic_type W.Ty.Mtv.t

type why3module = {
  types : (C.logic_type_info * C.location) list;
  logics : (C.logic_info * C.location) list;
}

type env = {
  wenv : W.Env.env;
  tenv : C.logic_type_info W.Ty.Hts.t;
  lenv : C.logic_info W.Term.Hls.t;
  lils : W.Term.lsymbol Logic_info.Hashtbl.t;
  lcts : W.Term.lsymbol Logic_ctor_info.Hashtbl.t;
  ltts : W.Ty.tysymbol Logic_type_info.Hashtbl.t;
  menv : why3module Datatype.String.Hashtbl.t;
}

type menv = {
  mutable lti : (C.logic_type_info * C.location) list;
  mutable li : (C.logic_info * C.location) list;
}

let create wenv =
  let tenv  = W.Ty.Hts.create 0 in
  let lenv  = W.Term.Hls.create 0 in
  let lils  = Logic_info.Hashtbl.create 0 in
  let lcts  = Logic_ctor_info.Hashtbl.create 0 in
  let ltts  = Logic_type_info.Hashtbl.create 0 in
  let menv  = Datatype.String.Hashtbl.create 0 in
  { wenv; tenv; lenv; lils; lcts; ltts; menv }

(* -------------------------------------------------------------------------- *)
(* ---    Built-in                                                        --- *)
(* -------------------------------------------------------------------------- *)

let add_builtin (tenv) ts lt_name lt_params  =
  W.Ty.Hts.add tenv ts C.{lt_name ; lt_params; lt_def=None; lt_attr=[] }

let find_ts env pkg thy name =
  let th = Why3.Env.read_theory env.wenv pkg thy in
  try
    Why3.Theory.ns_find_ts th.th_export name
  with Not_found ->
    L.fatal "Cannot find %s.%s.%s"
      (String.concat "." pkg ) thy (String.concat "." name)

let add_builtins env =
  begin
    let ts_list = find_ts env ["list"] "List" ["list"] in
    let ts_set = find_ts env ["set"] "Set" ["set"] in
    add_builtin env.tenv ts_list "\\list" ["A"];
    add_builtin env.tenv ts_set "set" ["A"];
  end

(* -------------------------------------------------------------------------- *)
(* ---    Location handling                                               --- *)
(* -------------------------------------------------------------------------- *)

let convert_location (wloc : Why3.Loc.position option) : C.location =
  match wloc with
  | Some loc ->
    let (file,lstart,cstart,lend,cend) = Why3.Loc.get loc in
    let path = F.of_string file in
    let pstart = Filepos.make ~path ~line:lstart ~column:cstart ~offset:0 () in
    let pend = Filepos.make ~path ~line:lend ~column:cend ~offset:0 () in
    (pstart, pend)
  | None ->
    (Filepos.unknown, Filepos.unknown)

(* -------------------------------------------------------------------------- *)
(* ---    Type conversion                                                 --- *)
(* -------------------------------------------------------------------------- *)

let tvars_of_txs (txs: W.Ty.tvsymbol list) : string list * tvars =
  List.iter (fun (tv: W.Ty.tvsymbol) ->
      L.debug ~level:3 "Name of : %a" pp_id tv.tv_name) txs;
  List.fold_right
    (fun (tv: W.Ty.tvsymbol) (txs,tvs) ->
       let x = tv.tv_name.id_string in
       x :: txs, W.Ty.Mtv.add tv (C.Lvar x) tvs
    ) txs ([], W.Ty.Mtv.empty)

let rec lt_of_ty env menv (tvs : tvars)  (ty: W.Ty.ty) : C.logic_type =
  match ty.ty_node with
  | Tyvar x -> W.Ty.Mtv.find x tvs
  | Tyapp(s,[]) when W.Ty.(ts_equal s ts_int) -> C.Linteger
  | Tyapp(s,[]) when W.Ty.(ts_equal s ts_real) -> C.Lreal
  | Tyapp(s,[]) when W.Ty.(ts_equal s ts_bool) -> C.Lboolean
  | Tyapp(s,[a;b]) when W.Ty.(ts_equal s ts_func) ->
    let ta = lt_of_ty env menv tvs a in
    let tb = lt_of_ty env menv tvs b in
    C.Larrow([ta],tb)
  | Tyapp(s,ts) ->
    C.Ltype(lt_of_ts env menv s [], List.map (lt_of_ty env menv tvs) ts)

and lt_of_ts env menv (ts : W.Ty.tysymbol) (cs: W.Decl.constructor list) : C.logic_type_info =
  try W.Ty.Hts.find env.tenv ts with Not_found ->
    let lt_params,tvars = tvars_of_txs ts.ts_args in
    let lt_name = acsl_name ts.ts_name in
    let lti = C.{ lt_name ; lt_params ; lt_def = None ; lt_attr = []; } in
    lti.lt_def <-
      (match ts.ts_def with
       | Range _ | Float _ -> None
       | NoDef -> Some (C.LTsum (List.map (cli_of_constr env menv lti) cs))
       | Alias ty -> Some (C.LTsyn (lt_of_ty env menv tvars ty))
      );
    W.Ty.Hts.add env.tenv ts lti ;
    menv.lti <- (lti, (convert_location ts.ts_name.id_loc) ) :: menv.lti;
    Logic_type_info.Hashtbl.add env.ltts lti ts;
    lti

and cli_of_constr env menv ctor_type (ls, _: W.Decl.constructor) : C.logic_ctor_info =
  let _,tvars =
    tvars_of_txs @@ W.Ty.Stv.elements @@  W.Term.ls_ty_freevars ls in
  let l_profile = List.mapi (lv_of_ty env menv tvars ) ls.ls_args in
  let ctor_params = List.map ( fun (lv:C.logic_var) -> lv.lv_type) l_profile in
  let ctor_name = acsl_name ls.ls_name in
  let ctor = Cil_types.{ ctor_name ; ctor_type ; ctor_params } in
  Logic_ctor_info.Hashtbl.add env.lcts ctor ls ;
  ctor

(* -------------------------------------------------------------------------- *)
(* ---    Functions conversion                                            --- *)
(* -------------------------------------------------------------------------- *)

and lv_of_ty env menv (tvars:tvars) (index) (ty:W.Ty.ty) : C.logic_var =
  Cil_const.make_logic_var_formal (Printf.sprintf "x%d" index)
  @@ (lt_of_ty env menv tvars ty)

and lt_of_ty_opt (lt_opt) =
  match lt_opt with
  | None -> C.Ctype Cil_const.voidType (* Same as logic_typing *)
  | Some tr -> tr

let li_of_ls env menv (ls : W.Term.lsymbol) : C.logic_info =
  let l_tparams,tvars =
    tvars_of_txs @@ W.Ty.Stv.elements @@  W.Term.ls_ty_freevars ls in
  let l_type = Option.map (lt_of_ty  env menv tvars ) ls.ls_value in
  let l_profile = List.mapi (lv_of_ty env menv tvars ) ls.ls_args in
  let l_args = List.map ( fun (lv:C.logic_var) -> lv.lv_type) l_profile in
  let l_result = lt_of_ty_opt l_type in
  let l_params = if l_args = [] then l_result else C.Larrow (l_args, l_result) in
  let l_name = acsl_name ls.ls_name in
  let lv = Cil_const.make_logic_var_global l_name l_params in
  let li = C.{
      l_var_info = lv ;
      l_labels = []; l_tparams; l_type; l_profile ;
      l_body = C.LBnone;
    } in W.Term.Hls.add env.lenv ls li;
  menv.li <- (li, (convert_location ls.ls_name.id_loc) ):: menv.li;
  Logic_info.Hashtbl.add env.lils li ls; li

let add_ts env menv (ts : W.Ty.tysymbol) (cs: W.Decl.constructor list)=
  L.debug ~dkey "Importing type %a: %s" pp_id ts.ts_name (acsl_name ts.ts_name);
  ignore @@ lt_of_ts env menv ts cs
let add_ls env menv (ls : W.Term.lsymbol) =
  L.debug ~dkey "Importing logic %a: %s" pp_id ls.ls_name (acsl_name ls.ls_name);
  ignore @@ li_of_ls env menv ls

(* -------------------------------------------------------------------------- *)
(* ---    Theory                                                          --- *)
(* -------------------------------------------------------------------------- *)

let get_theory env (theory_name) (theory_path) =
  try W.Env.read_theory env.wenv theory_path theory_name
  with W.Env.LibraryNotFound _ ->
    L.error "Library %s not found" theory_name; W.Theory.ignore_theory

let parse_theory env (theory:W.Theory.theory) (menv) =
  begin
    List.iter (fun (tdecl : T.tdecl) ->
        match tdecl.td_node with
        | Decl decl ->
          begin
            match decl.d_node with
            | Dtype ts -> add_ts env menv ts []
            | Dparam ls -> add_ls env menv ls
            | Ddata ds -> List.iter (fun (ts, cs) -> add_ts env menv ts cs) ds
            | Dlogic ds -> List.iter (fun (ls,_) -> add_ls env menv ls) ds
            | Dind (_,ds) -> List.iter (fun (ls,_) -> add_ls env menv ls) ds
            | Dprop _ -> ()
          end
        | Use _ | Clone _ | Meta _ -> ()
      ) theory.th_decls;
  end

let kind_of_lt (lt : C.logic_type) : LB.kind =
  match lt with
  | C.Linteger -> LB.Z
  | C.Lreal -> LB.R
  | C.Lboolean -> LB.B
  | _ -> LB.A

let sort_of_lt (lt : C.logic_type) : Qed.Logic.sort =
  match lt with
  | C.Linteger -> Qed.Logic.Sint
  | C.Lreal -> Qed.Logic.Sreal
  | _ -> Qed.Logic.Sdata

let sort_of_result = function
  | Some lt -> sort_of_lt lt
  | None -> Qed.Logic.Sprop

let register_builtin env m =
  begin
    let add_builtin (ls: W.Term.lsymbol) acsl_name profile result =
      let (package, theory, name) = T.restore_path ls.ls_name in
      let kinds = List.map kind_of_lt profile in
      let params = List.map sort_of_lt profile in
      LB.add_builtin acsl_name kinds @@
      Lang.imported_f ~package ~theory ~name ~params ~result  ()
    in
    let add_builtin_li li =
      let ls = Logic_info.Hashtbl.find env.lils li in
      let profile = List.map (fun lv -> lv.C.lv_type) li.l_profile in
      let result = sort_of_result li.l_type in
      add_builtin ls li.l_var_info.lv_name profile result
    in
    let add_builtin_ctor ctor =
      let ls = Logic_ctor_info.Hashtbl.find env.lcts ctor in
      let profile = ctor.Cil_types.ctor_params in
      let result = Qed.Logic.Sdata in
      add_builtin ls ctor.ctor_name profile result
    in
    let add_builtin_t lti =
      let ty = Logic_type_info.Hashtbl.find env.ltts lti in
      let (package,theory,name) = T.restore_path ty.ts_name in
      LB.add_builtin_type lti.lt_name @@
      Lang.imported_t ~package ~theory ~name ;
      begin match lti.lt_def with
        | Some C.LTsum ctors -> List.iter add_builtin_ctor ctors
        | _ -> ()
      end
    in
    List.iter (fun (lti, _) -> add_builtin_t lti) m.types;
    List.iter (fun (li, _) -> add_builtin_li li) m.logics;
  end

let import_theory env thname =
  try
    Datatype.String.Hashtbl.find env.menv thname
  with Not_found ->
    L.debug ~dkey "Parsing Why3 theory %s.@." thname ;
    let theory_name, theory_path = extract_path thname in
    let menv : menv = {li = []; lti = []} in
    let theory = get_theory env theory_name theory_path in
    parse_theory env theory menv;
    let m = { types = List.rev menv.lti; logics =  List.rev menv.li } in
    Datatype.String.Hashtbl.add env.menv thname m;
    register_builtin env m; m

(* -------------------------------------------------------------------------- *)
(* ---    Module registration                                             --- *)
(* -------------------------------------------------------------------------- *)

module Env = WpContext.StaticGenerator
    (Datatype.Unit)
    (struct
      type key = unit
      type data = env
      let name = "Wp.Why3Import.Env"
      let compile () =
        let env = create @@ why3_env @@ L.Library.get () in
        add_builtins env ; env
    end)

let importer (ctxt: LT.module_builder) (_: C.location) (m: string list) =
  begin
    L.debug ~dkey "Importing Why3 theory %s.@." (String.concat "::" m) ;
    let thname = String.concat "." m in
    let m = import_theory (Env.get ()) thname in
    List.iter (fun (lti,loc) -> ctxt.add_logic_type loc lti) m.types ;
    List.iter (fun (li, loc) -> ctxt.add_logic_function loc li) m.logics ;
  end

let registered = ref false

let register () =
  if not !registered then
    begin
      registered := true ;
      Acsl_extension.register_module_importer ~plugin:"wp" "why3" importer ;
    end

let () = Cmdline.run_after_extended_stage register

(* -------------------------------------------------------------------------- *)
