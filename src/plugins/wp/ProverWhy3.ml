(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let dkey = Wp_parameters.register_category "prover"
let dkey_pp_task = Wp_parameters.register_category "prover:pp_task"
let dkey_compile =
  Wp_parameters.register_category
    ~help:"WP -> Why3 compilation"
    "why3:compile"
let dkey_model =
  Wp_parameters.register_category
    ~help:"Counter examples model variable"
    "why3:model"

let option_file = LogicBuiltins.create_option
    ~sanitizer:(fun ~driver_dir x -> Filename.concat driver_dir x)
    "why3" "file"

let option_import = LogicBuiltins.create_option
    ~sanitizer:(fun ~driver_dir:_ x -> x)
    "why3" "import"

let option_qual =
  LogicBuiltins.create_option
    ~sanitizer:(fun ~driver_dir:_ x -> x)
    "why3" "qualifier"

let why3_failure msg =
  Format.kasprintf failwith msg

type why3_conf = {
  env : Why3.Env.env ;
  config : Why3.Whyconf.main ;
}

module Conf = WpContext.Index(struct
    include Datatype.Unit
    type key = unit
    type data = why3_conf
    let name = datatype_name
  end)

let get_why3_conf = Conf.memoize
    begin fun () ->
      let config = Why3Provers.config () in
      let main = Why3.Whyconf.get_main config in
      let ctx = Filepath.to_string_abs (WpContext.directory ()) in
      let wp = Filepath.to_string_abs (Wp_parameters.Share.get_dir "why3") in
      let user = Filepath.to_string_list (Wp_parameters.Library.get ()) in
      let why3 = Why3.Whyconf.loadpath main in
      let ld = ctx :: wp :: (user @ why3) in
      { env = Why3.Env.create_env ld ; config = main }
    end

type context = {
  mutable th : Why3.Theory.theory_uc;
  conf: why3_conf;
}

type convert = {
  th : Why3.Theory.theory_uc;
  env: Why3.Env.env;
  subst: Why3.Term.term Lang.F.Tmap.t;
  pool: Lang.F.pool;
  polarity: Cvalues.polarity;
  incomplete_types: (string, Why3.Ty.tysymbol) Hashtbl.t;
  incomplete_symbols: (string, Why3.Term.lsymbol) Hashtbl.t;
  mutable convert_for_export: Lang.F.term Lang.F.Tmap.t;
}

(** The reason for the rebuild *)
let specific_equalities: Lang.For_export.specific_equality list ref =
  ref [Vlist.specialize_eq_list]

let add_specific_equality ~for_tau ~mk_new_eq =
  specific_equalities := { for_tau; mk_new_eq }::!specific_equalities

(** get symbols *)

let get_ls ~cnv ~f ~l ~p =
  let th = Why3.Env.read_theory cnv.env f l in
  let ls =
    try
      Why3.Theory.ns_find_ls th.th_export p
    with Not_found ->
      why3_failure "The symbol %a can't be found in %a.%s"
        Why3.Pp.(print_list dot string) p
        Why3.Pp.(print_list dot string) f l
  in
  ls

let get_ts ~cnv ~f ~l ~p =
  let th = Why3.Env.read_theory cnv.env f l in
  let ls =
    try
      Why3.Theory.ns_find_ts th.th_export p
    with Not_found ->
      why3_failure "The type %a can't be found in %a.%s"
        Why3.Pp.(print_list dot string) p
        Why3.Pp.(print_list dot string) f l
  in
  ls


let t_app ~cnv ~f ~l ~p tl =
  Why3.Term.t_app_infer (get_ls ~cnv ~f ~l ~p) tl

let t_app' ~cnv ~f ~l ~p tl ty =
  Why3.Term.t_app (get_ls ~cnv ~f ~l ~p) tl ty

(** fold map list of at least one element *)
let fold_map map fold = function
  | [] -> assert false (* absurd: forbidden by qed  *)
  | a::tl ->
    List.fold_left (fun acc a -> fold acc (map a)) (map a) tl

let empty_context name : context = {
  th = Why3.Theory.create_theory (Why3.Ident.id_fresh name);
  conf = get_why3_conf ();
}

let empty_cnv ?(polarity=`NoPolarity) (ctx:context) : convert = {
  th = ctx.th;
  subst = Lang.F.Tmap.empty;
  pool = Lang.F.pool ();
  env = ctx.conf.env;
  polarity;
  incomplete_symbols = Hashtbl.create 3;
  incomplete_types = Hashtbl.create 3;
  convert_for_export = Lang.F.Tmap.empty;
}

let lfun_wname (lfun:Lang.lfun) =
  match lfun with
  | ACSL f -> Qed.Engine.F_call (Lang.logic_id f)
  | CTOR c -> Qed.Engine.F_call (Lang.ctor_id c)
  | FUN({m_source=Generated(_,n)}) -> Qed.Engine.F_call n
  | FUN({m_source=Extern e}) -> e.Lang.ext_link
  | FUN({m_source=Wsymbol(p,m,s)}) ->
    Qed.Engine.F_call (String.concat "." (p @ m :: s))

let coerce ~cnv sort expected r =
  match sort, expected with
  | Qed.Logic.Bool, Qed.Logic.Prop -> Why3.Term.(t_equ r t_bool_true)
  | Qed.Logic.Int, Qed.Logic.Real ->
    t_app ~cnv ~f:["real"] ~l:"FromInt" ~p:["from_int"] [r]
  | _ -> r

let adt_wname = function
  | Lang.Mtype a -> a.Lang.ext_link
  | Mrecord(a,_) -> a.Lang.ext_link
  | Comp (c, KValue) -> Lang.comp_id c
  | Comp (c, KInit) -> Lang.comp_init_id c
  | Atype lt -> Lang.type_id lt
  | Wtype(p,m,s) -> String.concat "." (p @ m :: s)

let tvar =
  let tvar = Datatype.Int.Hashtbl.create 10 in
  fun i ->
    Datatype.Int.Hashtbl.memo tvar i
      (fun i ->
         let id = Why3.Ident.id_fresh (Printf.sprintf "a%i" i) in
         Why3.Ty.create_tvsymbol id)


(** Sharing *)

let shared (_ : Lang.F.term) = false

let shareable e =
  match Lang.F.repr e with
  | Kint _ | Kreal _ | True | False -> false
  | Times _ | Add _ | Mul _ | Div _ | Mod _ -> true
  | Eq _ | Neq _ | Leq _ | Lt _ -> false
  | Aget _ | Aset _ | Rget _ | Rdef _ | Acst _ -> true
  | And _ | Or _ | Not _ | Imply _ | If _ -> false
  | Fun _ -> not (Lang.F.is_prop e)
  | Bvar _ | Fvar _ | Apply _ | Bind _ -> false

let subterms f e =
  match Lang.F.repr e with
  | Rdef fts ->
    begin
      match Lang.F.record_with fts with
      | None -> Lang.F.lc_iter f e
      | Some(a,fts) -> f a ; List.iter (fun (_,e) -> f e) fts
    end
  | _ -> Lang.F.lc_iter f e

(* path splitting *)
let regexp_col = Str.regexp_string ":"
let regexp_com = Str.regexp_string ","
let regexp_dot = Str.regexp_string "."

let cut_path s = Str.split_delim regexp_dot s

let wp_why3_lib library =
  match LogicBuiltins.get_option option_qual ~library with
  | [] -> [library]
  | [ lib ] -> Str.split_delim regexp_dot lib
  | l ->
    let pp_sep fmt () = Format.pp_print_string fmt ", " in
    Wp_parameters.fatal
      "too many bindings for WP-specific Why3 theory file %s:@\n%a"
      library Format.(pp_print_list ~pp_sep pp_print_string) l

(* conversion *)

let of_adt ~cnv = function
  | Lang.Wtype(f,l,p) -> get_ts ~cnv ~f ~l ~p
  | adt ->
    let s = adt_wname adt in
    try Hashtbl.find cnv.incomplete_types s
    with Not_found ->
    try Why3.Theory.(ns_find_ts (get_namespace cnv.th) (cut_path s))
    with Not_found ->
      why3_failure "Can't find type '%s' in why3 namespace" s

let rec of_tau ~cnv (t:Lang.F.tau) =
  match t with
  | Prop -> None
  | Bool -> Some Why3.Ty.ty_bool
  | Int -> Some Why3.Ty.ty_int
  | Real -> Some Why3.Ty.ty_real
  | Array(k,v) ->
    let ts = get_ts ~cnv ~f:["map"] ~l:"Map" ~p:["map"] in
    Some (Why3.Ty.ty_app ts [Option.get (of_tau ~cnv k); Option.get (of_tau ~cnv v)])
  | Data(adt,l) ->
    let ts = of_adt ~cnv adt in
    Some (Why3.Ty.ty_app ts (List.map (fun e -> Option.get (of_tau ~cnv e)) l))
  | Tvar i -> Some (Why3.Ty.ty_var (tvar i))
  | Record _ ->
    why3_failure "Type %a not (yet) convertible" Lang.F.pp_tau t

module Literal =
struct

  open Why3

  let const_int (z:Z.t) =
    let k = BigInt.of_string (Z.to_string z) in
    Term.t_const (Constant.int_const k) Ty.ty_int

  let why3_real ty ~radix ~neg ~int ?(frac="") ?exp () =
    let rc = Number.real_literal ~radix ~neg ~int ~frac ~exp in
    Term.t_const (Constant.ConstReal rc) ty

  let const_real ~cnv (q:Q.t) =
    let mk_real_int z =
      let neg = Z.sign z < 0 in
      let int = Z.to_string (Z.abs z) in
      why3_real Why3.Ty.ty_real ~radix:10 ~neg ~int ()
    in
    if Z.is_one q.den
    then mk_real_int q.num
    else
      t_app ~cnv ~f:["real"] ~l:"Real" ~p:["infix /"]
        [mk_real_int q.num;mk_real_int q.den]

  let cfloat_of_tau tau =
    if      Lang.F.Tau.equal tau Cfloat.t32 then Ctypes.Float32
    else if Lang.F.Tau.equal tau Cfloat.t64 then Ctypes.Float64
    else raise Not_found

  let re_float = Str.regexp
      "-?0x\\([0-9a-f]+\\).\\([0-9a-f]+\\)?0*p?\\([+-]?[0-9a-f]+\\)?$"

  let float_literal_from_q ~cnv tau q =
    let use_hex = true in
    let qf = Q.to_float q in
    let f = match cfloat_of_tau tau with
      | Float32 -> Floating_point.round_to_single_precision qf
      | Float64 -> qf
    in
    let s = Pretty_utils.to_string (Floating_point.pretty_normal ~use_hex) f in
    let s = String.lowercase_ascii s in
    if Str.string_match re_float s 0 then
      let group n r = try Str.matched_group n r with Not_found -> "" in
      let neg = Q.sign q < 0 in
      let int,frac,exp = (group 1 s), (group 2 s), (group 3 s) in
      let exp = if String.equal exp "" then None else Some exp in
      let ty = Option.get (of_tau ~cnv tau) in
      why3_real ty ~radix:16 ~neg ~int ~frac ?exp ()
    else raise Not_found

  let const_float ~cnv tau (repr:Lang.F.QED.repr) =
    match repr with
    | Fun(f, [x]) when Lang.Fun.(equal f Cfloat.fq32 || equal f Cfloat.fq64) ->
      begin match Lang.F.repr x with
        | Kreal q -> float_literal_from_q ~cnv tau q
        | _ -> raise Not_found
      end
    | _ -> raise Not_found

  let is_float_literal ~cnv tau repr =
    try (ignore (const_float ~cnv tau repr) ; true)
    with Not_found | Why3.Number.NonRepresentableFloat _ -> false

end

let rec full_trigger = function
  | Qed.Engine.TgAny -> false
  | TgVar _ -> true
  | TgGet(a,k) -> full_trigger a && full_trigger k
  | TgSet(a,k,v) -> full_trigger a && full_trigger k && full_trigger v
  | TgFun(_,xs) | TgProp(_,xs) -> List.for_all full_trigger xs

let rec full_triggers = function
  | [] -> []
  | ts :: tgs ->
    match List.filter full_trigger ts with
    | [] -> full_triggers tgs
    | ts -> ts :: full_triggers tgs

let rec of_trigger ~cnv t =
  match t with
  | Qed.Engine.TgAny -> assert false (* absurd: filter by full_triggers *)
  | Qed.Engine.TgVar v -> begin
      try Lang.F.Tmap.find (Lang.F.e_var v) cnv.subst
      with Not_found -> why3_failure "Unbound variable %a" Lang.F.pp_var v
    end
  | Qed.Engine.TgGet(m,k) ->
    t_app ~cnv ~f:["map"] ~l:"Map" ~p:["get"] [of_trigger ~cnv m;of_trigger ~cnv k]
  | TgSet(m,k,v) ->
    t_app ~cnv ~f:["map"] ~l:"Map" ~p:["set"] [of_trigger ~cnv m;of_trigger ~cnv k;of_trigger ~cnv v]
  | TgFun (f,l) -> begin
      match lfun_wname f with
      | F_call s ->
        let ls = Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) in
        Why3.Term.t_app_infer ls (List.map (fun e -> of_trigger ~cnv e) l)
      | _ -> why3_failure "can not convert extended calls in triggers"
    end
  | TgProp (f,l) ->
    begin
      match lfun_wname f with
      | F_call s ->
        let ls = Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) in
        Why3.Term.t_app_infer ls (List.map (fun e -> of_trigger ~cnv e) l)
      | _ -> why3_failure "can not convert extended calls in triggers"
    end

let rec of_term ~cnv expected t : Why3.Term.term =
  let sort =
    try Lang.F.typeof t
    with Not_found ->
      why3_failure "@[<hov 2>Untyped term: %a@]" Lang.F.pp_term t
  in
  Wp_parameters.debug ~dkey:dkey_compile
    "of_term %a:%a (expected %a)@."
    Lang.F.pp_term t Lang.F.Tau.pretty sort Lang.F.Tau.pretty expected
  ;

  let ($) f x = f x in
  let r =
    try coerce ~cnv sort expected $ Lang.F.Tmap.find t cnv.subst
    with Not_found ->
    match Lang.F.repr t, sort, expected with
    | (Fvar _, _, _) -> invalid_arg "unbound variable in of_term"
    | (Bvar _, _, _) -> invalid_arg "bound variable in of_term"
    | Bind((Forall|Exists) as q,_,_), _, _ -> begin
        coerce ~cnv Prop expected $
        let why3_vars, t = successive_binders cnv q t in
        let quant = match q with
          | Qed.Logic.Forall -> Why3.Term.Tforall
          | Qed.Logic.Exists -> Why3.Term.Texists
          | _ -> assert false
        in
        Why3.Term.t_quant quant (Why3.Term.t_close_quant why3_vars [] t)
      end
    | True, _, Prop -> Why3.Term.t_true
    | True, _, Bool -> Why3.Term.t_bool_true
    | False, _, Prop -> Why3.Term.t_false
    | False, _, Bool -> Why3.Term.t_bool_false
    | Kint z, Int, _ -> coerce ~cnv sort expected $ Literal.const_int z
    | Kreal q, Real, _ -> coerce ~cnv sort expected $ Literal.const_real ~cnv q
    | repr, t, _ when Literal.is_float_literal ~cnv t repr ->
      coerce ~cnv sort expected $ Literal.const_float ~cnv t repr
    | Times(z,t), Int, _ ->
      coerce ~cnv sort expected $
      t_app ~cnv ~f:["int"] ~l:"Int" ~p:["infix *"] [Literal.const_int z; of_term ~cnv sort t]
    | Times(z,t), Real, _ ->
      coerce ~cnv sort expected $
      t_app ~cnv ~f:["real"] ~l:"Real" ~p:["infix *"]
        [Literal.const_real ~cnv (Q.of_bigint z); of_term ~cnv sort t]
    | Add l, Int, _ ->
      coerce ~cnv sort expected $
      t_app_fold ~f:["int"] ~l:"Int" ~p:["infix +"] ~cnv sort l
    | Add l, Real, _ ->
      coerce ~cnv sort expected $
      t_app_fold ~f:["real"] ~l:"Real" ~p:["infix +"] ~cnv sort l
    | Mul l, Int, _ ->
      coerce ~cnv sort expected $
      t_app_fold ~f:["int"] ~l:"Int" ~p:["infix *"] ~cnv sort l
    | Mul l, Real, _ ->
      coerce ~cnv sort expected $
      t_app_fold ~f:["real"] ~l:"Real" ~p:["infix *"] ~cnv sort l
    | Leq (a,b), _, Prop ->
      int_or_real ~cnv
        ~fint:["int"] ~lint:"Int" ~pint:["infix <="]
        ~freal:["real"] ~lreal:"Real" ~preal:["infix <="]
        a b
    | Div(a,b), Int, _ ->
      coerce ~cnv sort expected $
      t_app ~cnv ~f:["int"] ~l:"ComputerDivision" ~p:["div"]
        [of_term ~cnv sort a; of_term ~cnv sort b]
    | Mod(a,b), Int, _ ->
      coerce ~cnv sort expected $
      t_app ~cnv ~f:["int"] ~l:"ComputerDivision" ~p:["mod"]
        [of_term ~cnv sort a; of_term ~cnv sort b]
    | Div(a,b), Real, _ ->
      coerce ~cnv sort expected $
      t_app ~cnv ~f:["real"] ~l:"Real" ~p:["infix /"]
        [of_term ~cnv sort a; of_term ~cnv sort b]
    | Lt (a,b), _, Prop ->
      int_or_real ~cnv
        ~fint:["int"] ~lint:"Int" ~pint:["infix <"]
        ~freal:["real"] ~lreal:"Real" ~preal:["infix <"]
        a b
    | Leq (a,b), _, Bool ->
      int_or_real ~cnv
        ~fint:(wp_why3_lib "qed") ~lint:"Qed" ~pint:["zleq"]
        ~freal:(wp_why3_lib "qed") ~lreal:"Qed" ~preal:["rleq"]
        a b
    | Lt (a,b), _, Bool ->
      int_or_real ~cnv
        ~fint:(wp_why3_lib "qed") ~lint:"Qed" ~pint:["zlt"]
        ~freal:(wp_why3_lib "qed") ~lreal:"Qed" ~preal:["rlt"]
        a b
    | And l, _, Bool ->
      t_app_fold ~f:["bool"] ~l:"Bool" ~p:["andb"] ~cnv expected l
    | And l, _, Prop ->
      fold_map (of_term ~cnv expected) Why3.Term.t_and l
    | Or l, _, Bool ->
      t_app_fold ~f:["bool"] ~l:"Bool" ~p:["orb"] ~cnv expected l
    | Or l, _, Prop ->
      fold_map (of_term ~cnv expected) Why3.Term.t_or l
    | Not e, _, Bool ->
      let cnv = {cnv with polarity = Cvalues.negate cnv.polarity} in
      t_app ~cnv ~f:["bool"] ~l:"Bool" ~p:["notb"] [of_term ~cnv expected e]
    | Not e, _, Prop ->
      let cnv = {cnv with polarity = Cvalues.negate cnv.polarity} in
      Why3.Term.t_not (of_term ~cnv expected e)
    | Imply (l,e), _, _ ->
      let e = (of_term ~cnv expected) e in
      let cnv' = {cnv with polarity = Cvalues.negate cnv.polarity} in
      let fold acc a =
        let a = of_term ~cnv:cnv' expected a in
        match expected with
        | Prop -> Why3.Term.t_implies a acc
        | _ (* Bool *) ->
          t_app ~cnv:cnv' ~f:["bool"] ~l:"Bool" ~p:["implb"] [a;acc]
      in
      List.fold_left fold e (List.rev l)
    | Eq (a,b), _, Prop -> begin
        match Lang.F.typeof a with
        | Prop | Bool ->
          Why3.Term.t_iff (of_term ~cnv Prop a) (of_term ~cnv Prop b)
        | tau ->
          match List.find (fun spe -> spe.Lang.For_export.for_tau tau) !specific_equalities with
          | spe when cnv.polarity = `Positive -> of_term ~cnv expected (spe.mk_new_eq a b)
          | exception Not_found -> poly ~cnv Why3.Term.t_equ a b
          | _                   -> poly ~cnv Why3.Term.t_equ a b
      end
    | Neq (a,b), _, Prop ->
      begin
        match Lang.F.typeof a with
        | Prop | Bool ->
          Why3.Term.t_not (Why3.Term.t_iff (of_term ~cnv Prop a) (of_term ~cnv Prop b))
        | tau ->
          match List.find (fun spe -> spe.Lang.For_export.for_tau tau) !specific_equalities with
          | spe when cnv.polarity = `Negative ->
            Why3.Term.t_not (of_term ~cnv expected (spe.mk_new_eq a b))
          | exception Not_found -> poly ~cnv Why3.Term.t_neq a b
          | _                   -> poly ~cnv Why3.Term.t_neq a b
      end
    | Eq (a,b), _, Bool ->
      poly ~cnv (fun a b -> t_app ~cnv ~f:(wp_why3_lib "qed") ~l:"Qed" ~p:["eqb"] [a;b]) a b
    | Neq (a,b), _, Bool ->
      poly ~cnv (fun a b -> t_app ~cnv ~f:(wp_why3_lib "qed") ~l:"Qed" ~p:["neqb"] [a;b]) a b
    | If(a,b,c), _, _ ->
      let cnv' = {cnv with polarity = `NoPolarity} in
      Why3.Term.t_if (of_term ~cnv:cnv' Prop a) (of_term ~cnv expected b) (of_term ~cnv expected c)
    | Aget(m,k), _, _ -> begin
        coerce ~cnv sort expected $
        let mtau = Lang.F.typeof m in
        let ksort = match mtau with
          | Array(ksort,_) -> ksort
          | _ -> assert false (* absurd: by qed typing *)in
        t_app ~cnv ~f:["map"] ~l:"Map" ~p:["get"] [of_term ~cnv mtau m;of_term ~cnv ksort k]
      end
    | Aset(m,k,v), Array(ksort,vsort), _ ->
      coerce ~cnv sort expected $
      t_app ~cnv ~f:["map"] ~l:"Map" ~p:["set"] [of_term ~cnv sort m;of_term ~cnv ksort k;of_term ~cnv vsort v]
    | Acst(_,v), Array(_,vsort), _ ->
      coerce ~cnv sort expected $
      t_app' ~cnv ~f:["map"] ~l:"Const" ~p:["const"] [of_term ~cnv vsort v] (of_tau ~cnv sort)
    (* Generic *)
    | Fun (FUN({m_source=Wsymbol(f,l,p)}),ls), tau, expected ->
      coerce ~cnv sort expected $
      t_app' ~cnv ~f ~l ~p (List.map (of_term' cnv) ls) (of_tau ~cnv tau)

    | Fun (f,l), _, _ -> begin
        let t_app ls l r  =
          Why3.Term.t_app ls l r
        in
        let apply_from_ns s l sort =
          let find s =
            try Hashtbl.find cnv.incomplete_symbols s
            with Not_found ->
              Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s))
          in
          match find s, expected with
          | ls, _ -> coerce ~cnv sort expected $ t_app ls l (of_tau ~cnv sort)
          | exception Not_found -> why3_failure "Can't find '%s' in why3 namespace" s
        in
        let apply_from_ns' s l =
          apply_from_ns s (List.map (fun e -> of_term' cnv e) l)
        in
        match lfun_wname f, expected with
        | F_call s, _ -> apply_from_ns' s l sort
        | Qed.Engine.F_subst (s, _), _ -> apply_from_ns' s l sort
        | Qed.Engine.F_left s, _ | Qed.Engine.F_assoc s, _ ->
          let rec aux = function
            | [] -> why3_failure "Empty application"
            | [a] -> of_term ~cnv expected a
            | a::l ->
              apply_from_ns s [of_term' cnv a; aux l] sort
          in
          aux l
        | Qed.Engine.F_right s, _ ->
          let rec aux = function
            | [] -> why3_failure "Empty application"
            | [a] -> of_term ~cnv expected a
            | a::l ->
              apply_from_ns s [aux l;of_term' cnv a] sort
          in
          aux (List.rev l)
        | Qed.Engine.F_list (fcons,fnil), _ ->
          let rec aux = function
            | [] -> apply_from_ns fnil [] sort
            | a::l ->
              apply_from_ns fcons [of_term' cnv a;aux l] sort
          in
          aux l
        | Qed.Engine.F_bool_prop (s,_), Bool | Qed.Engine.F_bool_prop (_,s), Prop ->
          apply_from_ns' s l expected
        | Qed.Engine.F_bool_prop (_,_), _ ->
          why3_failure "badly expected type %a for term %a"
            Lang.F.pp_tau expected Lang.F.pp_term t
      end
    | Rget(a, (Cfield(_,KInit) as f)), _ , tau -> begin
        let s = Lang.name_of_field f in
        match Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) with
        | ls ->
          begin match tau with
            | Prop ->
              Why3.Term.t_equ
                (Why3.Term.t_app ls [of_term' cnv a] (Some Why3.Ty.ty_bool))
                (Why3.Term.t_bool_true)
            | _ ->
              Why3.Term.t_app ls [of_term' cnv a] (of_tau ~cnv tau)
          end
        | exception Not_found -> why3_failure "Can't find '%s' in why3 namespace" s
      end

    | Rget(a,f), _ , _ -> begin
        let s = Lang.name_of_field f in
        match Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) with
        | ls -> Why3.Term.t_app ls [of_term' cnv a] (of_tau ~cnv expected)
        | exception Not_found -> why3_failure "Can't find '%s' in why3 namespace" s
      end
    | Rdef(l), Data(Comp (c, k),_) , _ -> begin
        (* l is already sorted by field *)
        let s = match k with
          | KValue -> Lang.comp_id c
          | KInit -> Lang.comp_init_id c
        in
        match Why3.Theory.(ns_find_ls (get_namespace cnv.th) (cut_path s)) with
        | ls ->
          let l = List.map (fun (_,t) -> of_term' cnv t) l in
          Why3.Term.t_app ls l (of_tau ~cnv expected)
        | exception Not_found -> why3_failure "Can't find '%s' in why3 namespace" s
      end
    | (Rdef _, Data ((Mtype _|Mrecord (_, _)|Atype _|Wtype _), _), _)
    | (Rdef _, (Prop|Bool|Int|Real|Tvar _|Array (_, _)), _)
    | (Aset (_, _, _), (Prop|Bool|Int|Real|Tvar _|Record _|Data (_, _)), _)
    | (Neq (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Eq (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Not _, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Or _, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (And _, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Lt (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Leq (_, _), _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Div (_, _), (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Mod (_, _), (Prop|Bool|Real|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Mul _, (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Add _, (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Times (_, _), (Prop|Bool|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Kreal _, (Prop|Bool|Int|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (Kint _, (Prop|Bool|Real|Tvar _|Array (_, _)|Record _|Data (_, _)), _)
    | (False, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (True, _, (Int|Real|Tvar _|Array (_, _)|Record _|Data (_, _)))
    | (Acst (_, _), (Prop|Bool|Int|Real|Tvar _|Record _|Data (_, _)), _)
      -> assert false (* absurd: by typing *)
    | (Bind (Lambda, _, _), _, _)
    | Apply _ , _, _
    | Rdef _, Record _, _ ->
      why3_failure
        "Can't convert to why3 the qed term %a of type %a"
        Lang.F.pp_term t Lang.F.pp_tau sort
  in
  r

and t_app_fold  ~cnv ~f ~l ~p expected lt =
  let fold acc a =
    t_app ~cnv ~f ~l ~p [acc;a]
  in
  fold_map (of_term ~cnv expected) fold lt

and of_term' cnv t =
  of_term ~cnv (Lang.F.typeof t) t

and share cnv expected t =
  let l = Lang.F.QED.shared ~shareable ~shared ~subterms [t] in
  let cnv,lets = mk_lets cnv l in
  let t = of_term ~cnv expected t in
  let t = List.fold_left (fun t (x,e') ->
      Why3.Term.t_let_close x e' t
    ) t lets
  in t

and mk_lets cnv l =
  List.fold_left (fun (cnv,lets) e ->
      let cnv' = {cnv with polarity = `NoPolarity} in
      let e' = of_term ~cnv:cnv' (Lang.F.typeof e) e in
      match e'.t_ty with
      | None -> ({cnv with subst = Lang.F.Tmap.add e e' cnv.subst},lets)
      | Some ty ->
        let x = Why3.Ident.id_fresh (Lang.F.basename e) in
        let x = Why3.Term.create_vsymbol x ty in
        (* Format.printf "lets %a = %a : %a@."
            Why3.Pretty.print_vsty x
            Why3.Pretty.print_term e'
            Why3.Pretty.print_ty (Why3.Term.t_type e'); *)
        let cnv = {cnv with subst = Lang.F.Tmap.add e (Why3.Term.t_var x) cnv.subst } in
        let lets = (x,e')::lets in
        cnv,lets
    ) (cnv,[]) l

and successive_binders cnv q t =
  match Lang.F.repr t with
  | Bind((Forall|Exists) as q',tau,t) when q' = q ->
    let x = Lang.F.fresh cnv.pool tau in
    let x' = Why3.Ident.id_fresh (Lang.F.Tau.basename tau) in
    let x' = Why3.Term.create_vsymbol x' (Option.get (of_tau ~cnv tau)) in
    let cnv = {cnv with subst = Lang.F.Tmap.add (Lang.F.e_var x) (Why3.Term.t_var x') cnv.subst} in
    let t = Lang.F.QED.e_unbind x t in
    let why3_vars, t = successive_binders cnv q t in
    x'::why3_vars, t
  | _ ->
    [], share cnv Prop t

and int_or_real ~cnv ~fint ~lint ~pint ~freal ~lreal ~preal a b =
  match (Lang.F.typeof a), (Lang.F.typeof b) with
  | Int, Int ->
    t_app_fold ~f:fint ~l:lint ~p:pint ~cnv Int [a; b]
  | Real, Int | Real, Real | Int, Real ->
    t_app_fold ~f:freal ~l:lreal ~p:preal ~cnv Real [a; b]
  | _ -> assert false

and poly ~cnv f a b =
  match Lang.F.typeof a, Lang.F.typeof b with
  | Int,Int ->
    f (of_term ~cnv Int a) (of_term ~cnv Int b)
  | Int,Real | Real,Int | Real,Real ->
    f (of_term ~cnv Real a) (of_term ~cnv Real b)
  | ta,tb ->
    f (of_term ~cnv ta a) (of_term ~cnv tb b)

let rebuild cnv t =
  let t, cache = Lang.For_export.rebuild ~cache:cnv.convert_for_export t in
  cnv.convert_for_export <- cache;
  t

let convert cnv expected t =
  Lang.For_export.in_state (share cnv expected) (rebuild cnv t)

let mk_binders cnv l =
  List.fold_left (fun (cnv,lets) v ->
      match of_tau ~cnv (Lang.F.tau_of_var v) with
      | None -> why3_failure "Quantification on prop"
      | Some ty ->
        let x = Why3.Ident.id_fresh (Lang.F.Var.basename v) in
        let x = Why3.Term.create_vsymbol x ty in
        let ex = Lang.F.e_var v in
        let tx = Why3.Term.t_var x in
        let cnv = { cnv with subst = Lang.F.Tmap.add ex tx cnv.subst } in
        let lets = x::lets in
        cnv,lets
    ) (cnv,[]) (List.rev l)

(** visit definitions and add them in the task *)

module CLUSTERS = WpContext.Index
    (struct
      type key = Definitions.cluster
      type data = int * Why3.Theory.theory
      let name = "ProverWhy3.CLUSTERS"
      let compare = Definitions.cluster_compare
      let pretty = Definitions.pp_cluster
    end)



let filenoext file =
  let basename = Filename.basename file in
  (try Filename.chop_extension basename
   with Invalid_argument _ -> basename)

class visitor (ctx:context) c =
  object(self)

    inherit Definitions.visitor c


    (* --- Files, Theories and Clusters --- *)

    method add_builtin_lib =
      self#add_import_file ["bool"] "Bool" ;
      self#add_import_file ["int"] "Int" ;
      self#add_import_file ["int"] "ComputerDivision" ;
      self#add_import_file ["real"] "RealInfix" ;
      self#on_library "qed";
      self#add_import_file ["map"] "Map"

    method on_cluster c =
      let name = Definitions.cluster_id c in
      Wp_parameters.debug ~dkey:dkey_compile "Start on_cluster %s@." name;
      let th_name = String.capitalize_ascii name in
      let thy =
        let age = try fst (CLUSTERS.find c) with Not_found -> (-1) in
        if age < Definitions.cluster_age c then
          let ctx = empty_context th_name in
          let v = new visitor ctx c in
          v#add_builtin_lib;
          v#vself;
          let th = Why3.Theory.close_theory ctx.th in
          if Wp_parameters.has_print_generated () then
            Log.print_on_output
              begin fun fmt ->
                Format.fprintf fmt "---------------------------------------------@\n" ;
                Format.fprintf fmt "--- Context '%s' Cluster '%s' @\n"
                  (WpContext.get_context () |> WpContext.S.id) name;
                Format.fprintf fmt "---------------------------------------------@\n" ;
                Why3.Pretty.print_theory fmt th;
              end ;
          CLUSTERS.update c (Definitions.cluster_age c, th);
          th
        else
          snd (CLUSTERS.find c)
      in
      let th = ctx.th in
      let th = Why3.Theory.open_scope th name in
      let th = Why3.Theory.use_export th thy in
      let th = Why3.Theory.close_scope th ~import:true in
      Wp_parameters.debug ~dkey:dkey_compile "End  on_cluster %s@." name;
      ctx.th <- th

    method on_theory file thy =
      self#add_import_use ~import:false file thy ("W_" ^ thy)

    method section _ = ()

    method add_import ?was thy =
      match Str.split_delim regexp_dot thy with
      | [] -> why3_failure "[driver] empty import option"
      | l ->
        let file, thy = Why3.Lists.chop_last l in
        self#add_import_use file thy (Option.value ~default:thy was) ~import:true

    method add_import_file file thy =
      self#add_import_use ~import:true file thy thy

    method add_import_file_as file thy name =
      self#add_import_use ~import:false file thy name

    method add_import_use ~import file thy name =
      Wp_parameters.debug ~dkey:dkey_compile
        "@[use@ %s@ @[%a.%s@]@ as@ %s@]"
        (if import then "import" else "")
        Why3.Pp.(print_list (Why3.Pp.constant_string ".") string) file
        thy name ;
      let thy = Why3.Env.read_theory ctx.conf.env file thy in
      let th = ctx.th in
      let th = Why3.Theory.open_scope th name in
      let th = Why3.Theory.use_export th thy in
      let th = Why3.Theory.close_scope th ~import in
      ctx.th <- th

    method on_library thy =
      let copy_file source =
        if not (Filepath.equal
                  (Filepath.dirname source)
                  (Wp_parameters.Share.get_dir "."))
        then
          let tgtdir = WpContext.directory () in
          let why3src = Filepath.basename source in
          let target = Filepath.(tgtdir / why3src) in
          Filesystem.copy_file source target
      in
      let iter_file opt =
        match Str.split_delim regexp_col opt with
        | [file] ->
          let path = Filepath.of_string file in
          let filenoext = filenoext file in
          copy_file path;
          self#add_import_file [filenoext]
            (String.capitalize_ascii filenoext);
        | [file;lib] ->
          let path = Filepath.of_string file in
          copy_file path;
          self#add_import_file [filenoext file] lib;
        | [file;lib;name] ->
          let path = Filepath.of_string file in
          copy_file path;
          self#add_import_file_as [filenoext file] lib name;
        | _ -> why3_failure
                 "[driver] incorrect why3.file %S for library '%s'"
                 opt thy
      in
      let iter_import opt =
        List.iter (fun import ->
            match Str.split_delim regexp_col import with
            | [ th ] -> self#add_import th
            | [ th ; was ] -> self#add_import ~was th
            | _ -> why3_failure
                     "[driver] incorrect why3.import %S for library '%s'"
                     opt thy
          ) (Str.split regexp_com opt)
      in
      begin
        List.iter iter_file
          (LogicBuiltins.get_option option_file ~library:thy) ;
        List.iter iter_import
          (LogicBuiltins.get_option option_import ~library:thy) ;
      end

    method on_type lt def =
      match def with
      | Tabs ->
        let id = Why3.Ident.id_fresh (Lang.type_id lt) in
        let map i _ = tvar i in
        let tv_args = List.mapi map lt.lt_params in
        let id = Why3.Ty.create_tysymbol id tv_args NoDef in
        let decl = Why3.Decl.create_ty_decl id in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      | Tdef t ->
        let id = Why3.Ident.id_fresh (Lang.type_id lt) in
        let map i _ = tvar i in
        let tv_args = List.mapi map lt.lt_params in
        let cnv = empty_cnv ctx in
        let t = Option.get (of_tau ~cnv t) in
        let id = Why3.Ty.create_tysymbol id tv_args (Alias t) in
        let decl = Why3.Decl.create_ty_decl id in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      | Tsum cases ->
        let name = Lang.type_id lt in
        let id = Why3.Ident.id_fresh name in
        let map i _ = tvar i in
        let tv_args = List.mapi map lt.lt_params in
        let tys = Why3.Ty.create_tysymbol id tv_args NoDef in
        let cnv = empty_cnv ctx in
        Hashtbl.add cnv.incomplete_types name tys ;
        let tv_args = List.map Why3.Ty.ty_var tv_args in
        let return_ty = Why3.Ty.ty_app tys tv_args in
        let constr = List.length cases in
        let cases = List.map (fun (c,targs) ->
            let name = match c with | Lang.CTOR c -> Lang.ctor_id c | _ -> assert false in
            let id = Why3.Ident.id_fresh name in
            let targs = List.map (fun t -> Option.get (of_tau ~cnv t)) targs in
            let ls = Why3.Term.create_fsymbol ~constr id targs return_ty in
            let proj = List.map (fun _ -> None) targs in
            (ls,proj)
          ) cases in
        let decl = Why3.Decl.create_data_decl [tys,cases] in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      | Trec fields ->
        let name = Lang.type_id lt in
        let id = Why3.Ident.id_fresh name in
        let map i _ = tvar i in
        let tv_args = List.mapi map lt.lt_params in
        let tys = Why3.Ty.create_tysymbol id tv_args NoDef in
        let cnv = empty_cnv ctx in
        Hashtbl.add cnv.incomplete_types name tys ;
        let tv_args = List.map Why3.Ty.ty_var tv_args in
        let return_ty = Why3.Ty.ty_app tys tv_args in
        let fields,args = List.split @@ List.map (fun (f,ty) ->
            let name = Lang.name_of_field f in
            let id = Why3.Ident.id_fresh name in
            let ty = Option.get (of_tau ~cnv ty) in
            let ls = Why3.Term.create_fsymbol ~proj:true id [return_ty] ty in
            Some ls,ty
          ) fields in
        let id = Why3.Ident.id_fresh (Lang.type_id lt) in
        let cstr = Why3.Term.create_fsymbol ~constr:1 id args return_ty in
        let decl = Why3.Decl.create_data_decl [tys,[cstr,fields]] in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;

    method private on_comp_gen kind c fts =
      begin
        let make_id = match kind with
          | Lang.KValue -> Lang.comp_id
          | Lang.KInit -> Lang.comp_init_id
        in
        let compare_field (f,_) (g,_) =
          let cmp = Lang.Field.compare f g in
          if cmp = 0 then assert false (* by definition *) else cmp
        in
        let fts = Option.map (List.sort compare_field) fts in
        (*TODO:NUPW: manage UNIONS *)
        let id = Why3.Ident.id_fresh (make_id c) in
        let ts = Why3.Ty.create_tysymbol id [] Why3.Ty.NoDef in
        let ty = Why3.Ty.ty_app ts [] in
        let id = Why3.Ident.id_fresh (make_id c) in
        let cnv = empty_cnv ctx in
        let map (f,tau) =
          let ty_ctr = of_tau ~cnv tau in
          let id = Why3.Ident.id_fresh (Lang.name_of_field f) in
          let ls = Why3.Term.create_lsymbol ~proj:true id [ty] ty_ctr in
          (Some ls,Option.get ty_ctr)
        in
        let fields = Option.map (List.map map) fts in
        let decl = match fields with
          | None -> Why3.Decl.create_ty_decl ts
          | Some fields ->
            let constr =
              Why3.Term.create_fsymbol ~constr:1 id (List.map snd fields) ty
            in
            Why3.Decl.create_data_decl [ts,[constr,List.map fst fields]]
        in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
      end

    method on_comp = self#on_comp_gen KValue
    method on_icomp = self#on_comp_gen KInit

    method private make_lemma cnv ?prefix (l: Definitions.dlemma) =
      let name = match prefix with
        | None -> l.l_name
        | Some p -> p ^ "_" ^ l.l_name
      in
      let id = Why3.Ident.id_fresh (Lang.lemma_id name) in
      let id = Why3.Decl.create_prsymbol id in
      List.iter (Lang.F.add_var cnv.pool) l.l_forall;
      let cnv, vars = Lang.For_export.in_state (mk_binders cnv) l.l_forall in
      let t = convert cnv Prop (Lang.F.e_prop l.l_lemma) in
      let triggers = full_triggers l.l_triggers in
      let triggers = Lang.For_export.in_state (List.map (List.map (of_trigger ~cnv))) triggers in
      let t = Why3.Term.t_forall_close vars triggers t in
      id, t

    method on_dlemma l =
      if l.l_kind <> Check then
        let kind = Why3.Decl.(if l.l_kind = Admit then Paxiom else Plemma) in
        let cnv = empty_cnv ctx in
        let id, t = self#make_lemma cnv l in
        let decl = Why3.Decl.create_prop_decl kind id t in
        ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl

    method on_dfun d =
      Wp_parameters.debug ~dkey:dkey_compile "Define %a@." Lang.Fun.pretty d.d_lfun ;
      let cnv = empty_cnv ctx in
      List.iter (Lang.F.add_var cnv.pool) d.d_params;
      begin
        match d.d_definition with
        | Logic t ->
          let id = Why3.Ident.id_fresh (Qed.Export.link_name (lfun_wname d.d_lfun)) in
          let map e = Option.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
          let ty_args = List.map map d.d_params in
          let id = Why3.Term.create_lsymbol id ty_args (of_tau ~cnv t) in
          let decl = Why3.Decl.create_param_decl id in
          ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
        | Function(t,mu,v) -> begin
            match mu with
            | Rec -> (* transform recursive function into an axioms *)
              let name = Qed.Export.link_name (lfun_wname d.d_lfun) in
              let id = Why3.Ident.id_fresh name in
              let map e = Option.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
              let ty_args = List.map map d.d_params in
              let result = of_tau ~cnv t in
              let id = Why3.Term.create_lsymbol id ty_args result in
              let decl = Why3.Decl.create_param_decl id in
              ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
              let cnv = empty_cnv ctx in
              List.iter (Lang.F.add_var cnv.pool) d.d_params;
              let cnv, vars = mk_binders cnv d.d_params in
              let t = share cnv t v in
              let t =
                Why3.Term.t_forall_close vars []
                  (Why3.Term.t_equ
                     (Why3.Term.t_app id (List.map Why3.Term.t_var vars) result)
                     t)
              in
              let decl =
                Why3.Decl.create_prop_decl Why3.Decl.Paxiom
                  (Why3.Decl.create_prsymbol (Why3.Ident.id_fresh (name^"_def")))
                  t in
              ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
            | Def ->
              let id = Why3.Ident.id_fresh (Qed.Export.link_name (lfun_wname d.d_lfun)) in
              let map e = Option.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
              let ty_args = List.map map d.d_params in
              let result = of_tau ~cnv t in
              let id = Why3.Term.create_lsymbol id ty_args result in
              let cnv, vars = mk_binders cnv d.d_params in
              let t = share cnv t v in
              let decl = Why3.Decl.make_ls_defn id vars t in
              let decl = Why3.Decl.create_logic_decl [decl] in
              ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl
          end
        | Predicate(mu,p) -> begin
            match mu with
            | Rec ->
              let name = Qed.Export.link_name (lfun_wname d.d_lfun) in
              let id = Why3.Ident.id_fresh name in
              let map e = Option.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
              let ty_args = List.map map d.d_params in
              let result = None in
              let id = Why3.Term.create_lsymbol id ty_args result in
              let decl = Why3.Decl.create_param_decl id in
              ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
              let cnv = empty_cnv ctx in
              List.iter (Lang.F.add_var cnv.pool) d.d_params;
              let cnv, vars = mk_binders cnv d.d_params in
              let t = share cnv Prop (Lang.F.e_prop p) in
              let t =
                Why3.Term.t_forall_close vars []
                  (Why3.Term.t_iff t
                     (Why3.Term.t_app id (List.map Why3.Term.t_var vars) result))
              in
              let decl =
                Why3.Decl.create_prop_decl Why3.Decl.Paxiom
                  (Why3.Decl.create_prsymbol (Why3.Ident.id_fresh (name^"_def")))
                  t in
              ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl;
            | Def ->
              let id = Why3.Ident.id_fresh (Qed.Export.link_name (lfun_wname d.d_lfun)) in
              let map e = Option.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
              let ty_args = List.map map d.d_params in
              let id = Why3.Term.create_lsymbol id ty_args None in
              let cnv, vars = mk_binders cnv d.d_params in
              let t = share cnv Prop (Lang.F.e_prop p) in
              let decl = Why3.Decl.make_ls_defn id vars t in
              let decl = Why3.Decl.create_logic_decl [decl] in
              ctx.th <- Why3.Theory.add_decl ~warn:false ctx.th decl
          end
        | Inductive dl ->
          (* create predicate symbol *)
          let fname = Qed.Export.link_name (lfun_wname d.d_lfun) in
          let id = Why3.Ident.id_fresh fname in
          let map e = Option.get (of_tau ~cnv (Lang.F.tau_of_var e)) in
          let ty_args = List.map map d.d_params in
          let fid = Why3.Term.create_lsymbol id ty_args None in
          let make_case (l:Definitions.dlemma) =
            let cnv = empty_cnv ctx in
            Hashtbl.add cnv.incomplete_symbols fname fid ;
            self#make_lemma cnv ~prefix:fname l
          in
          let ind_decl = (fid, List.map make_case dl) in
          begin
            try
              ctx.th <- Why3.Theory.add_ind_decl ctx.th Why3.Decl.Ind [ind_decl] ;
            with
            | Why3.Decl.InvalidIndDecl _
            | Why3.Decl.NonPositiveIndDecl _ ->
              begin match d.d_lfun with
                | Lang.ACSL li ->
                  Wp_parameters.abort
                    "After simplification inductive %s appear ill-formed \
                     (hint: check cases premises)"
                    li.l_var_info.lv_name
                | Lang.CTOR ci ->
                  Wp_parameters.fatal
                    "Constructor (%s) in inductive compilation"
                    ci.ctor_name
                  [@ coverage off] (* should not happen *)
                | extern ->
                  Wp_parameters.fatal
                    "Generated symbol %s is an ill-formed inductive"
                    (Qed.Export.link_name @@ lfun_wname extern)
                  [@ coverage off] (* should not happen *)
              end
          end
      end

  end

(* -------------------------------------------------------------------------- *)
(* --- Goal Compilation                                                   --- *)
(* -------------------------------------------------------------------------- *)

let goal_id = (Why3.Decl.create_prsymbol (Why3.Ident.id_fresh "wp_goal"))

let add_model_trace (probes: Lang.F.term Probe.Map.t) cnv t =
  let open Why3 in
  if Probe.Map.is_empty probes then t else
    let task = Task.add_meta t Driver.meta_get_counterexmp [Theory.MAstr ""] in
    let create_id (p:Probe.t) ty =
      let attr = Ident.create_model_trace_attr (string_of_int p.id) in
      let attrs = Ident.Sattr.singleton attr in
      let loc =
        let (pos1,pos2) = p.loc in
        let path = Filepos.path pos1 |> Filepath.to_string
        and l1 = Filepos.line pos1 and c1 = Filepos.input_column pos1 - 1
        and l2 = Filepos.line pos2 and c2 = Filepos.input_column pos2 - 1 in
        Why3.Loc.user_position path l1 c1 l2 c2
      in Term.create_lsymbol (Ident.id_fresh ~loc ~attrs p.name) [] ty
    in
    let fold (p:Probe.t) (term:Lang.F.term) task =
      let term' = share cnv (Lang.F.typeof term) term in
      let id = create_id p term'.t_ty in
      let task = Task.add_param_decl task id in
      let eq_id = Why3.Decl.create_prsymbol (Why3.Ident.id_fresh "ce_eq") in
      let eq = Term.t_equ (Term.t_app id [] term'.t_ty) term' in
      let decl = Why3.Decl.create_prop_decl Paxiom eq_id eq in
      Task.add_decl task decl
    in
    Probe.Map.fold fold probes task

let convert_freevariables ~probes ~cnv t =
  let freevars = Probe.Map.fold
      (fun _ t vars -> Lang.F.Vars.union vars (Lang.F.vars t))
      probes (Lang.F.vars t) in
  let cnv,lss =
    Lang.F.Vars.fold (fun (v:Lang.F.Var.t) (cnv,lss) ->
        let ty = of_tau ~cnv @@ Lang.F.tau_of_var v in
        let x = Why3.Ident.id_fresh (Lang.F.Var.basename v) in
        let ls = Why3.Term.create_lsymbol x [] ty in
        let ex = Lang.F.e_var v in
        let tx = Why3.Term.t_app ls [] ty in
        let cnv = { cnv with subst = Lang.F.Tmap.add ex tx cnv.subst } in
        (cnv,ls::lss)) freevars (cnv,[])
  in
  cnv,lss

let prove_goal ~id ~title ~name ?axioms ?(probes=Probe.Map.empty) t =
  (* Format.printf "why3_of_qed start@."; *)
  let goal = Definitions.cluster ~id ~title () in
  let ctx = empty_context name in
  let v = new visitor ctx goal in
  Wp_parameters.debug ~dkey:dkey_compile "%t"
    begin fun fmt ->
      Format.fprintf fmt "---------------------------------------------@\n" ;
      Format.fprintf fmt "EXPORT GOAL %s@." id ;
      Format.fprintf fmt "PROP @[<hov 2>%a@]@." Lang.F.pp_pred t ;
      Format.fprintf fmt "---------------------------------------------@\n" ;
    end ;
  v#add_builtin_lib;
  v#vgoal axioms t;
  let cnv = empty_cnv ~polarity:`Positive ctx in
  let t = rebuild cnv (Lang.F.e_prop t) in
  let probes = Probe.Map.map (rebuild cnv) probes in
  Lang.For_export.in_state
    begin fun () ->
      let cnv,lss = convert_freevariables ~probes ~cnv t in
      let goal = share cnv Prop t in
      let decl = Why3.Decl.create_prop_decl Pgoal goal_id goal in
      let th = Why3.Theory.close_theory ctx.th in
      if Wp_parameters.has_print_generated () then begin
        let th_uc_tmp = Why3.Theory.add_decl ~warn:false ctx.th decl in
        let th_tmp    = Why3.Theory.close_theory th_uc_tmp in
        Wp_parameters.debug ~dkey:Wp_parameters.cat_print_generated "%a"
          Why3.Pretty.print_theory th_tmp
      end;
      let t = None in
      let t = Why3.Task.use_export t th in
      let t = List.fold_left Why3.Task.add_param_decl t lss in
      let t = add_model_trace probes cnv t in
      Why3.Task.add_decl t decl
    end ()

let prove_prop ?probes ?axioms ~pid prop =
  let id = WpPropId.get_propid pid in
  let title = Pretty_utils.to_string WpPropId.pretty pid in
  let name = "WP" in
  prove_goal ?axioms ?probes ~id ~title ~name prop

let compute_probes ~ce ~pid goal =
  if ce then Wpo.GOAL.compute_probes ~pid goal else Probe.Map.empty

let task_of_wpo ~ce wpo =
  let v = wpo.Wpo.po_formula in
  let pid = wpo.Wpo.po_pid in
  let prop = Wpo.GOAL.compute_proof ~pid ~opened:ce v.goal in
  let probes = compute_probes ~ce ~pid v.goal in
  prove_prop ~pid ?axioms:v.axioms ~probes prop, probes

(* -------------------------------------------------------------------------- *)
(* --- Prover Task                                                        --- *)
(* -------------------------------------------------------------------------- *)

let prover_task env prover task =
  let config = Why3Provers.config () in
  let prover_config = Why3.Whyconf.get_prover_config config prover in
  let drv = Why3.Driver.load_driver_for_prover (Why3.Whyconf.get_main config)
      env prover_config in
  drv , prover_config , Why3.Driver.prepare_task drv task

(* -------------------------------------------------------------------------- *)
(* --- Prover Call                                                        --- *)
(* -------------------------------------------------------------------------- *)

type prover_call = {
  prover : Why3Provers.t ;
  call : Why3.Call_provers.prover_call ;
  steps : int option ;
  timeout : float ;
  mutable timeover : float option ;
  mutable interrupted : bool ;
  mutable killed : bool ;
}

let has_model_attr attrs =
  Why3.Ident.Sattr.fold_left (fun acc (e:Why3.Ident.attribute) ->
      match String.remove_prefix "model_trace:" e.attr_string with
      | None -> acc
      | Some _ as a -> a
    ) None attrs

let debug_model (res:Why3.Call_provers.prover_result) =
  Wp_parameters.debug ~dkey:dkey_model "%t"
    begin fun fmt ->
      List.iter
        begin fun (res,model) ->
          Format.fprintf fmt "@[<hov 2>model %a: %a@]@\n"
            Why3.Call_provers.print_prover_answer res
            (Why3.Model_parser.print_model
               ~print_attrs:true) model
        end
        res.pr_models
    end

let get_model probes (res:Why3.Call_provers.prover_result) =
  if Wp_parameters.has_dkey dkey_model && not @@ Probe.Map.is_empty probes then
    debug_model (res:Why3.Call_provers.prover_result);
  (* we take the second model because it should be the most precise?? *)
  match Why3.Check_ce.select_model_last_non_empty res.pr_models with
  | None -> Probe.Map.empty
  | Some model ->
    let index = Hashtbl.create 0 in
    let elements = Why3.Model_parser.get_model_elements model in
    List.iter
      (fun (e:Why3.Model_parser.model_element) ->
         match has_model_attr e.me_attrs with
         | None -> ()
         | Some id -> Hashtbl.add index id e.me_concrete_value)
      elements ;
    Probe.Map.filter_map
      (fun (p:Probe.t) _ ->
         let id = string_of_int p.id in
         try Some (Hashtbl.find index id)
         with Not_found -> None
      ) probes

let ping_prover_call ~config ~probes p =
  match Why3.Call_provers.query_call p.call with
  | NoUpdates
  | ProverStarted ->
    let () =
      if p.timeout > 0.0 then
        match p.timeover with
        | None ->
          let started = Unix.time () in
          p.timeover <- Some (started +. 2.0 +. p.timeout)
        | Some timeout ->
          let time = Unix.time () in
          if time > timeout then
            begin
              Wp_parameters.debug ~dkey
                "Hard Kill (late why3server timeout)" ;
              p.interrupted <- true ;
              Why3.Call_provers.interrupt_call ~config p.call ;
            end
    in Task.Wait 100
  | InternalFailure exn ->
    let msg = Format.asprintf "@[<hov 2>%a@]"
        Why3.Exn_printer.exn_printer exn in
    Task.Return (Task.Result (VCS.failed msg))
  | ProverInterrupted -> Task.(Return Canceled)
  | ProverFinished _ when p.killed -> Task.(Return Canceled)
  | ProverFinished pr ->
    let r =
      let time = max Rformat.epsilon pr.pr_time in
      match pr.pr_answer with
      | Timeout -> VCS.timeout time
      | Valid -> VCS.result ~time ~steps:pr.pr_steps VCS.Valid
      | OutOfMemory -> VCS.failed "out of memory"
      | StepLimitExceeded -> VCS.result ?steps:p.steps VCS.Stepout
      | Invalid ->
        debug_model pr;
        VCS.result ~time:pr.pr_time ~steps:pr.pr_steps
          ~model:(get_model probes pr) VCS.Invalid
      | Unknown _ ->
        debug_model pr;
        VCS.result ~model:(get_model probes pr) VCS.Unknown
      | _ when p.interrupted -> VCS.timeout p.timeout
      | Failure msg | HighFailure msg -> VCS.failed msg
    in
    Wp_parameters.debug ~dkey
      "@[@[Why3 result for %a:@] @[%a@] and @[%a@]@."
      Why3.Whyconf.print_prover p.prover
      (Why3.Call_provers.print_prover_result ~json:false) pr
      VCS.pp_result r;
    Task.Return (Task.Result r)

let call_prover_task ~timeout ~steps ~config ~probes prover call =
  Wp_parameters.debug ~dkey "Why3 run prover %a with timeout %f, steps %d@."
    Why3.Whyconf.print_prover prover
    (Option.value ~default:(0.0) timeout)
    (Option.value ~default:0 steps) ;
  let timeout = match timeout with None -> 0.0 | Some tlimit -> tlimit in
  let pcall = {
    call ; prover ;
    killed = false ;
    interrupted = false ;
    steps ; timeout ; timeover = None ;
  } in
  let ping = function
    | Task.Kill ->
      pcall.killed <- true ;
      Why3.Call_provers.interrupt_call ~config call ;
      Task.Yield
    | Task.Coin -> ping_prover_call ~config ~probes pcall
  in
  Task.async ping

(* -------------------------------------------------------------------------- *)
(* --- Batch Prover                                                       --- *)
(* -------------------------------------------------------------------------- *)

let output_task wpo drv ?(script : Filepath.t option) prover task =
  let file =
    Wpo.DISK.file_goal ~pid:wpo.Wpo.po_pid ~model:wpo.Wpo.po_model drv prover in
  let open Filesystem.Operators in
  let$ fmt = Filesystem.with_formatter_exn file in
  let pp_header fmt msg data =
    match Filepath.extension file with
    | ".mlw" | ".why" | ".v" ->
      Format.fprintf fmt "(* %s %s *)@\n" msg data
    | ".smt2" | ".psmt2" ->
      Format.fprintf fmt "; %s %s@\n" msg data
    | _ -> ()
  in
  pp_header fmt "WP Task for Prover" @@ Why3Provers.ident_why3 prover ;
  let old = Option.map
      (fun fscript ->
         let hash = Filesystem.digest fscript in
         pp_header fmt "WP Script" hash ;
         open_in (Filepath.to_string_abs fscript)
      ) script in
  let _ = Why3.Driver.print_task_prepared ?old drv fmt task in
  Option.iter close_in old


let digest_task wpo drv ?(script : Filepath.t option) prover task =
  output_task wpo drv ?script prover task;
  Filesystem.digest @@
  Wpo.DISK.file_goal ~pid:wpo.Wpo.po_pid ~model:wpo.Wpo.po_model drv prover

let run_batch pconf driver ~config
    ?(script : Filepath.t option)
    ~timeout ~steplimit ~memlimit
    ?(probes=Probe.Map.empty)
    prover task =
  let steps = match steplimit with Some 0 -> None | _ -> steplimit in
  let limits =
    let config = Why3.Whyconf.get_main @@ Why3Provers.config () in
    let config_mem = Why3.Whyconf.memlimit config in
    let config_time = Why3.Whyconf.timelimit config in
    let config_steps = Why3.Call_provers.empty_limits.limit_steps in
    Why3.Call_provers.{
      limit_time = Option.value ~default:config_time timeout;
      limit_steps = Option.value ~default:config_steps steps;
      limit_mem = Option.value ~default:config_mem memlimit;
    } in
  let with_steps = match steps, pconf.Why3.Whyconf.command_steps with
    | None, _ -> false
    | Some _, Some _ -> true
    | Some _, None ->
      Wp_parameters.warning ~once:true ~current:false
        "%a does not support steps limit (ignored option)"
        Why3.Whyconf.print_prover prover ;
      false in
  let steps = if with_steps then steps else None in
  let command = Why3.Whyconf.get_complete_command pconf ~with_steps in
  Wp_parameters.debug ~dkey "Prover command %S" command ;
  let inplace = if script <> None then Some true else None in
  let call =
    Why3.Driver.prove_task_prepared
      ?old:(Option.map Filepath.to_string_abs script) ?inplace
      ~command ~limits ~config driver task in
  call_prover_task ~config ~timeout ~steps ~probes prover call

(* -------------------------------------------------------------------------- *)
(* --- Interactive Prover (Coq)                                           --- *)
(* -------------------------------------------------------------------------- *)

let editor_mutex = Task.mutex ()

let editor_command pconf =
  let config = Why3Provers.config () in
  try
    let prover = pconf.Why3.Whyconf.prover in
    let ed_id = Why3.Whyconf.get_prover_editor config prover in
    let ed = Why3.Whyconf.editor_by_id config ed_id in
    String.concat " " (ed.editor_command :: ed.editor_options)
  with Not_found ->
    Why3.Whyconf.(default_editor (get_main config))

let scriptfile ~force ~ext wpo =
  let dir = Wp_parameters.Session.get_dir ~create_path:force "interactive" in
  Filepath.(dir / (wpo.Wpo.po_sid ^ ext))

let updatescript ~script driver task =
  let backup = Filepath.extend script ".bak" in
  Filesystem.rename script backup ;
  let _printing_info =
    let open Filesystem.Operators in
    let$ old = Filesystem.with_open_in_exn backup in
    let$ fmt = Filesystem.with_formatter_exn script in
    Why3.Driver.print_task_prepared ~old driver fmt task
  in
  if Filesystem.same_digest backup script then Filesystem.remove_file backup

let editor ~script ~merge ~config pconf driver task =
  Task.sync editor_mutex
    begin fun () ->
      Wp_parameters.feedback ~ontty:`Transient "Editing %a..."
        Filepath.pretty script ;
      if merge then updatescript ~script driver task ;
      let command = editor_command pconf in
      Wp_parameters.debug ~dkey "Editor command %S" command ;
      let probes = Probe.Map.empty in
      call_prover_task ~config ~timeout:None ~steps:None ~probes pconf.prover @@
      Why3.Call_provers.call_editor ~command ~config (Filepath.to_string_abs script)
    end

let compile ~script ~timeout ~memlimit ~config pconf driver prover task =
  run_batch ~config pconf driver ~script ~timeout ~memlimit ~steplimit:None
    ~probes:Probe.Map.empty prover task

let prepare ~mode wpo driver task =
  let ext = Filename.extension (Why3.Driver.file_of_task driver "S" "T" task) in
  let force = mode <> Prover.InteractiveMode.Batch in
  let script = scriptfile ~force wpo ~ext in
  if Filesystem.exists script then Some (script, true) else
  if force then
    begin
      let open Filesystem.Operators in
      let$ fmt = Filesystem.with_formatter_exn script in
      ignore @@ Why3.Driver.print_task_prepared driver fmt task;
      Some (script, false)
    end
  else None

let interactive ~mode ~config wpo pconf driver prover task =
  let time = Wp_parameters.InteractiveTimeout.get () in
  let mem = Wp_parameters.Memlimit.get () in
  let timeout = if time <= 0 then None else Some (float time) in
  let memlimit = if mem <= 0 then None else Some mem in
  match prepare ~mode wpo driver task with
  | None ->
    Wp_parameters.warning ~once:true ~current:false
      "Missing script(s) for prover %a.@\n\
       Use -wp-interactive=fix for interactive proving."
      Why3.Whyconf.print_prover prover ;
    Task.return VCS.unknown
  | Some (script, merge) ->
    Wp_parameters.debug ~dkey "%s %a script %S@."
      (if merge then "Found" else "New")
      Why3.Whyconf.print_prover prover (Filepath.to_string_abs script) ;
    match mode with
    | Prover.InteractiveMode.Batch ->
      compile ~script ~timeout ~memlimit ~config pconf driver prover task
    | Update ->
      if merge then updatescript ~script driver task ;
      compile ~script ~timeout ~memlimit ~config pconf driver prover task
    | Edit ->
      let open Task in
      editor ~script ~merge ~config pconf driver task >>= fun _ ->
      compile ~script ~timeout ~memlimit ~config pconf driver prover task
    | Fix ->
      let open Task in
      compile ~script ~timeout ~memlimit ~config pconf driver prover task
      >>= fun r ->
      if VCS.is_valid r then return r else
        editor ~script ~merge ~config pconf driver task >>= fun _ ->
        compile ~script ~timeout ~memlimit ~config pconf driver prover task
    | FixUpdate ->
      let open Task in
      if merge then updatescript ~script driver task ;
      compile ~script ~timeout ~memlimit ~config pconf driver prover task
      >>= fun r ->
      if VCS.is_valid r then return r else
        let merge = false in
        editor ~script ~merge ~config pconf driver task >>= fun _ ->
        compile ~script ~timeout ~memlimit ~config pconf driver prover task

let automated ~config ~probes ~timeout ~steplimit ~memlimit
    wpo pconf drv prover task =
  if Wp_parameters.Output.exists () then output_task wpo drv prover task;
  if Probe.Map.is_empty probes then
    Cache.get_result
      ~digest:(digest_task wpo drv)
      ~runner:(run_batch ~config ~probes ~memlimit pconf drv ?script:None)
      ~timeout ~steplimit prover task
  else
    run_batch ~config ~probes ~memlimit ~timeout ~steplimit
      pconf drv prover task

(* -------------------------------------------------------------------------- *)
(* --- Prove WPO                                                          --- *)
(* -------------------------------------------------------------------------- *)

let is_trivial (t : Why3.Task.task) =
  let goal = Why3.Task.task_goal_fmla t in
  Why3.Term.t_equal goal Why3.Term.t_true

let print_debug_task wpo drv prover task =
  let pp_task fmt task =
    ignore @@ Why3.Driver.print_task_prepared drv fmt task in
  if Wp_parameters.Output.exists () then
    let out_dir =
      Wp_parameters.Output.get_dir (WpContext.MODEL.id wpo.Wpo.po_model) in
    let prover = Why3Provers.title prover in
    let goal = Wpo.get_gid wpo ^ "_" ^ prover in
    let filename = Why3.Driver.file_of_task drv "" goal task in
    let file = Filepath.(out_dir / filename) in
    let out_channel = open_out (Filepath.to_string_abs file) in
    let fmt = Format.formatter_of_out_channel out_channel in
    Format.fprintf fmt "%a" pp_task task ;
    close_out out_channel
  else
    Wp_parameters.feedback "%a" pp_task task

let build_proof_task ?(mode=Prover.InteractiveMode.Batch) ?timeout ?steplimit ?memlimit
    ~prover wpo () =
  try
    (* Always generate common task *)
    let context = Wpo.get_context wpo in
    let ce,prover =
      if Wp_parameters.CounterExamples.get () then
        match Why3Provers.with_counter_examples prover with
        | Some prover_ce -> true,prover_ce
        | None -> false,prover
      else false, prover in
    let task,probes = WpContext.on_context context (task_of_wpo ~ce) wpo in
    if Wp_parameters.Generate.get ()
    then Task.return VCS.no_result (* Only generate *)
    else
      let {config; _ } as conf = WpContext.on_context context get_why3_conf () in
      let drv , pconf , task = prover_task conf.env prover task in
      if Wp_parameters.is_debug_key_enabled dkey_pp_task then
        print_debug_task wpo drv prover task ;
      if is_trivial task then
        Task.return VCS.valid
      else
      if pconf.interactive then
        interactive ~mode ~config wpo pconf drv prover task
      else
        automated ~config ~probes ~timeout ~steplimit ~memlimit
          wpo pconf drv prover task
  with
  | Log.AbortError _ ->
    Task.failed "[User Error]"
  | Log.AbortFatal _ ->
    Task.failed "[Compilation Error]"
  | exn ->
    if Wp_parameters.has_dkey dkey_compile then
      Wp_parameters.fatal "[Why3 Error] %a@\n%s"
        Why3.Exn_printer.exn_printer exn
        Printexc.(raw_backtrace_to_string @@ get_raw_backtrace ())
    else
      Task.failed "[Why3 Error] %a" Why3.Exn_printer.exn_printer exn

let prove ?mode ?timeout ?steplimit ?memlimit ~prover wpo =
  Task.later
    (build_proof_task ?mode ?timeout ?steplimit ?memlimit ~prover wpo) ()

(* -------------------------------------------------------------------------- *)
