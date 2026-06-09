(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Logic_typing
open Logic_ptree

(* -------------------------------------------------------------------------- *)
(* --- Pattern Engine                                                     --- *)
(* -------------------------------------------------------------------------- *)

type 'a loc = { loc : location ; value : 'a }

type pvar = string loc
type ast = node loc
and node =
  | Any
  | Pany of ast list
  | Pvar of pvar
  | Named of bool * pvar * ast (* boolean is true for debug symbols *)
  | Range of int * int
  | Int of Z.t
  | Bool of bool
  | String of string
  | Not of ast
  | Assoc of assoc * ast list
  | Binop of ast * binop * ast
  | Implies of ast list * ast
  | Call of string * ast list * bool (* trailing .. *)
  | Times of Z.t * ast
  | List of ast list
  | Field of ast * string
  | Get of ast * ast
  | Set of ast * ast * ast
  | Forall of quantifiers * ast
  | Exists of quantifiers * ast
and assoc = [ `Add | `Mul | `Concat | `Band | `Bor | `Bxor | `And | `Or ]
and binop = [ `Div | `Mod | `Repeat | `Eq | `Lt | `Le | `Ne | `Lsl | `Lsr ]

(* this function is used for debugging purposes *)
let named name p =
  let x = { loc = p.loc ; value = name } in
  { loc = p.loc ; value = Named(true, x, p) }

let self p =
  let pattern,self = match p.value with
    | Pvar x -> p , x
    | Named(false, x,_) -> p , x
    | _ ->
      let x = { loc = p.loc ; value= "\\target" } in
      { loc = p.loc ; value = Named(false, x, p) } , x
  in
  pattern , { loc = p.loc ; value = Pvar self }

let unroll op = function
  | { value = Assoc(f,xs) } when f = op -> xs
  | e -> [e]

let assoc op a b =
  {
    loc = fst a.loc, snd b.loc ;
    value = Assoc(op,unroll op a @ unroll op b) ;
  }

let implies a b =
  let hs = unroll `And a in
  let hs,p = match b.value with Implies(rs,p) -> hs @ rs , p | _ -> hs, b in
  {
    loc = fst a.loc, snd b.loc ;
    value = Implies (hs, p) ;
  }

let concat ~loc es =
  let es = List.map (unroll `Concat) es in
  { loc ; value = Assoc(`Concat, List.concat es) }

module Vmap = Map.Make(String)

type context = {
  typing : typing_context option ;
  mutable value : bool ;
  mutable pvars : pvar Vmap.t ;
}

type pattern = ast
type value = ast

let pattern_loc p = p.loc

(* -------------------------------------------------------------------------- *)
(* --- Node Parsing                                                       --- *)
(* -------------------------------------------------------------------------- *)

let context ?tc () = { typing = tc ; value = false ; pvars = Vmap.empty }

exception TypeError of Cil_types.location * string

let error ctxt loc msg =
  match ctxt.typing with
  | None -> Format.kasprintf (fun e -> raise (TypeError(loc, e))) msg
  | Some tc -> tc.error loc msg

let pint ctxt ~loc a =
  try int_of_string a
  with _ -> error ctxt loc "Invalid int %S" a

let pinteger ctxt ~loc a =
  try Z.of_string a
  with _ -> error ctxt loc "Invalid integer %S" a

let pvar ctxt ~loc x =
  try Vmap.find x ctxt.pvars with Not_found ->
    if ctxt.value then
      error ctxt loc "Unknown pattern variable '%s'" x
    else
      let pv = { loc ; value = x } in
      ctxt.pvars <- Vmap.add x pv ctxt.pvars ; pv

let pbound ctxt p =
  let loc = p.lexpr_loc in
  match p.lexpr_node with
  | PLconstant (IntConstant a) -> pint ctxt ~loc a
  | _ -> error ctxt loc "Invalid bound (int expected)"

let rec ptrail rps = function
  | [] -> List.rev rps,false
  | [{ lexpr_node = PLrange(None,None) }] -> List.rev rps,true
  | p::ps -> ptrail (p::rps) ps

let rec parse ctxt p =
  let loc = p.lexpr_loc in
  match p.lexpr_node with
  | PLvar "_" when not ctxt.value -> { loc ; value = Any }
  | PLvar x -> { loc ; value = Pvar (pvar ctxt ~loc x) }
  | PLnamed(x,p) ->
    let pv = pvar ctxt ~loc x in
    let pn = parse ctxt p in
    { loc ; value = Named(false, pv, pn) }
  | PLtrue -> { loc ; value = Bool true }
  | PLfalse -> { loc ; value = Bool false }
  | PLconstant (IntConstant n) ->
    { loc ; value = Int (pinteger ctxt ~loc n) }
  | PLconstant (StringConstant s) ->
    { loc ; value = String s }
  | PLrange(Some a,Some b) ->
    { loc ; value = Range(pbound ctxt a,pbound ctxt b) }
  | PLapp("\\any",[],ps) when not ctxt.value ->
    { loc ; value = Pany (List.map (parse ctxt) ps) }
  | PLapp("\\concat",[],[]) -> { loc ; value = List [] }
  | PLapp("\\concat",[],ps) -> concat ~loc @@ List.map (parse ctxt) ps
  | PLapp("\\repeat",[],[p;q]) -> parse_binop ctxt ~loc `Repeat p q
  | PLapp(lf,[],ps) ->
    let ps,trail = if ctxt.value then ps,false else ptrail [] ps in
    { loc ; value = Call(lf,List.map (parse ctxt) ps,trail) }
  | PLunop(Uminus,a) ->
    let a = parse ctxt a in
    { loc = a.loc ; value = Times(Z.minus_one,a) }
  | PLunop(Ubw_not,a) ->
    let a = parse ctxt a in
    { loc = a.loc ; value = Call("lf:lnot",[a],false) }
  | PLnot a ->
    let a = parse ctxt a in
    { loc = a.loc ; value = Not a }
  | PLbinop(a,Bmul,b) ->
    let a = parse ctxt a in
    let b = parse ctxt b in
    begin
      match a.value with
      | Int k -> { loc ; value = Times(k,b) }
      | _ -> assoc `Mul a b
    end
  | PLbinop(a,Bsub,b) ->
    let a = parse ctxt a in
    let b = parse ctxt b in
    let b = { loc = b.loc ; value = Times(Z.minus_one,b) } in
    assoc `Add a b
  | PLbinop(a,Badd,b) -> assoc `Add (parse ctxt a) (parse ctxt b)
  | PLbinop(a,Bbw_or,b) -> assoc `Bor (parse ctxt a) (parse ctxt b)
  | PLbinop(a,Bbw_and,b) -> assoc `Band (parse ctxt a) (parse ctxt b)
  | PLbinop(a,Bbw_xor,b) -> assoc `Bxor (parse ctxt a) (parse ctxt b)
  | PLbinop(a,Bdiv,b) -> parse_binop ctxt ~loc `Div a b
  | PLbinop(a,Bmod,b) -> parse_binop ctxt ~loc `Mod a b
  | PLbinop(a,Blshift,b) -> parse_binop ctxt ~loc `Lsl a b
  | PLbinop(a,Brshift,b) -> parse_binop ctxt ~loc `Lsr a b
  | PLrel(a,Lt,b) -> parse_binop ctxt ~loc `Lt a b
  | PLrel(a,Le,b) -> parse_binop ctxt ~loc `Le a b
  | PLrel(a,Gt,b) -> parse_binop ctxt ~loc `Lt b a
  | PLrel(a,Ge,b) -> parse_binop ctxt ~loc `Le b a
  | PLrel(a,Eq,b) -> parse_binop ctxt ~loc `Eq a b
  | PLrel(a,Neq,b) -> parse_binop ctxt ~loc `Ne a b
  | PLand(a,b) -> assoc `And (parse ctxt a) (parse ctxt b)
  | PLor(a,b) -> assoc `Or (parse ctxt a) (parse ctxt b)
  | PLimplies(a, b) -> implies (parse ctxt a) (parse ctxt b)
  | PLempty -> { loc ; value = List [] }
  | PLlist ps -> { loc ; value = List (List.map (parse ctxt) ps) }
  | PLrepeat(p,n) -> parse_binop ctxt ~loc `Repeat p n
  | PLdot(a,fd) -> { loc ; value = Field(parse ctxt a,fd) }
  | PLarrget(a,b) ->
    begin
      match b.lexpr_node with
      | PLarrget(k,v) ->
        { loc ; value = Set(parse ctxt a,parse ctxt k,parse ctxt v) }
      | _ ->
        { loc ; value = Get(parse ctxt a,parse ctxt b) }
    end
  | PLforall (qs,p) -> { loc ; value = Forall(qs,parse ctxt p) }
  | PLexists (qs,p) -> { loc ; value = Exists(qs,parse ctxt p) }
  | _ ->
    error ctxt loc
      (if ctxt.value then "Invalid value" else "Invalid pattern")

and parse_binop ctxt ~loc (op:binop) a b =
  { loc ; value = Binop(parse ctxt a,op,parse ctxt b) }

let pa_pattern ctxt p = ctxt.value <- false ; parse ctxt p
let pa_value ctxt p = ctxt.value <- true ; parse ctxt p

(* -------------------------------------------------------------------------- *)
(* --- Pretty                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec pp fmt (a : ast) =
  match a.value with
  | Any ->  Format.pp_print_string fmt "_"
  | Pvar x -> Format.pp_print_string fmt x.value
  | Named (true, _,v) -> Format.fprintf fmt "%a" pp v
  | Named (_, x,v) -> Format.fprintf fmt "%s:%a" x.value pp v
  | Range(a,b) -> Format.fprintf fmt "(%d..%d)" a b
  | Int n -> Z.pretty fmt n
  | Bool b -> Format.pp_print_string fmt (if b then "\\true" else "\\false")
  | String s -> Format.fprintf fmt "%S" s
  | Assoc(`Band,[]) -> Format.pp_print_string fmt "-1"
  | Assoc(`Mul,[]) -> Format.pp_print_string fmt "1"
  | Assoc((`Add|`Bor|`Bxor),[]) -> Format.pp_print_string fmt "0"
  | Assoc(`And,[]) -> Format.pp_print_string fmt "\\true"
  | Assoc(`Or,[]) -> Format.pp_print_string fmt "\\false"
  | Assoc(`Concat,[]) -> Format.pp_print_string fmt "[| |]"
  | Not a -> Format.fprintf fmt "!(%a)" pp a
  | Assoc(op,v::vs) ->
    let op = match op with
      | `Add -> "+"
      | `Mul -> "*"
      | `Concat | `Bxor -> "^"
      | `Band -> "&"
      | `Bor -> "|"
      | `And -> "&&"
      | `Or -> "||"
    in
    Format.fprintf fmt "@[<hov 2>(%a" pp v ;
    List.iter (Format.fprintf fmt "@ %s %a" op pp) vs ;
    Format.fprintf fmt ")@]"
  | Implies([], p) -> pp fmt p
  | Implies(hyps, p) ->
    Format.fprintf fmt "@[<hov 2>%a@ ==> %a@]"
      (Pretty_utils.pp_list ~sep:"@ && " pp) hyps
      pp p
  | Binop(a,op,b) ->
    let op = match op with
      | `Div -> "/"
      | `Mod -> "%"
      | `Eq -> "=="
      | `Ne -> "!="
      | `Lt -> "<"
      | `Le -> "<="
      | `Repeat -> "*^"
      | `Lsl -> "<<"
      | `Lsr -> ">>"
    in Format.fprintf fmt "@[<hov 2>(%a@ %s %a)@]" pp a op pp b
  | Times(k,v) -> Format.fprintf fmt "%a*%a" Z.pretty k pp v
  | Get(a,k) -> Format.fprintf fmt "@[<hov 2>%a[@,%a]@]" pp a pp k
  | Set(a,k,v) -> Format.fprintf fmt "@[<hov 2>%a[@,%a@ -> %a]@]" pp a pp k pp v
  | List [] -> Format.pp_print_string fmt "[| |]"
  | List (v::vs) ->
    Format.fprintf fmt "@[<hov 2>[| %a" pp v ;
    List.iter (Format.fprintf fmt " ;@ %a" pp) vs ;
    Format.fprintf fmt " |]@]"
  | Field(v,id) -> Format.fprintf fmt "%a.%s" pp v id
  | Call(id,[],true) -> Format.fprintf fmt "%s((..))" id
  | Call(id,[],false) -> Format.fprintf fmt "%s()" id
  | Call(id,v::vs,trail) ->
    Format.fprintf fmt "@[<hov 2>%s(%a" id pp v ;
    List.iter (Format.fprintf fmt ",@ %a" pp) vs ;
    if trail then Format.fprintf fmt ",@ (..)" ;
    Format.fprintf fmt ")@]"
  | Pany [] -> Format.pp_print_string fmt "\\never"
  | Pany (v::vs) ->
    Format.fprintf fmt "@[<hov 2>\\any(%a" pp v ;
    List.iter (Format.fprintf fmt ",@ %a" pp) vs ;
    Format.fprintf fmt ")@]"
  | Forall(qs,p) ->
    Format.fprintf fmt "@[<hov 2>\\forall %a;@ %a@]"
      Logic_print.print_quantifiers qs pp p
  | Exists(qs,p) ->
    Format.fprintf fmt "@[<hov 2>\\exists %a;@ %a@]"
      Logic_print.print_quantifiers qs pp p

let pp_value = pp
let pp_pattern = pp

(* -------------------------------------------------------------------------- *)
(* --- Pattern Matching                                                   --- *)
(* -------------------------------------------------------------------------- *)

type sigma = Tactical.selection Vmap.t

let pp_sigma fmt s =
  begin
    Format.fprintf fmt "@[<hv 0>[@[<hv 2>" ;
    Vmap.iter
      (fun x e ->
         Format.fprintf fmt "@ @[<hov 2>%s -> %a@] ;" x Tactical.pp_selection e
      ) s ;
    Format.fprintf fmt "@]@ ]@]" ;
  end

let iter_sigma = Vmap.iter

type penv = {
  pool : Lang.F.pool ;
  select : Lang.F.term -> Tactical.selection ;
  mutable sigma : sigma ;
  mutable binders : Lang.F.term Vmap.t ;
  mutable bvars : Lang.F.Vars.t ;
  mutable marked : Lang.F.Tset.t ;
}

let get env (x : pvar) =
  try Some (Vmap.find x.value env.binders) with Not_found ->
  try Some (Tactical.selected @@ Vmap.find x.value env.sigma) with Not_found ->
    None

let bind env (x : string) (v : Lang.F.var) =
  let { binders ; bvars } = env in
  begin
    env.binders <- Vmap.add x (Lang.F.e_var v) binders ;
    env.bvars <- Lang.F.Vars.add v bvars ;
    binders, bvars
  end

let unbind env (binders,bvars) =
  begin
    env.binders <- binders ;
    env.bvars <- bvars ;
  end

let merge env (x : pvar) e =
  if not @@ Lang.F.Vars.intersect env.bvars (Lang.F.vars e) then
    match get env x with
    | Some v -> if not (Lang.F.equal v e) then raise Not_found
    | None ->
      env.sigma <- Vmap.add x.value (env.select e) env.sigma

let rec is_any (p : pattern) =
  match p.value with
  | Any | Pvar _ -> true
  | Named(_,_,q) -> is_any q
  | _ -> false

let is_type (lt : logic_type) (t : Lang.F.tau) =
  match lt , t with
  | LTreal , Real
  | LTinteger , Int
  | LTboolean , (Bool | Prop)
    -> true
  | _ -> false

let rec pmatch env (p : pattern) e =
  match p.value , Lang.F.repr e with
  | Any , _ -> ()
  | Pvar x , _ -> merge env x e
  | Named(_,x,p) , _ -> merge env x e ; pmatch env p e
  | Range(a,b) , Kint n ->
    begin
      match Z.to_int_opt n with
      | Some v when a <= v && v <= b -> ()
      | _ -> raise Not_found
    end
  | Bool true , True -> ()
  | Bool false , False -> ()
  | Int v1, Kint v2 when Z.equal v1 v2 -> ()
  | Not p , Not e -> pmatch env p e
  | Assoc(`Or,ps) , Or es -> pac env Lang.F.e_or [] ps es
  | Assoc(`And,ps) , And es -> pac env Lang.F.e_and [] ps es
  | Assoc(`Add,ps) , Add es -> pac env Lang.F.e_sum [] ps es
  | Assoc(`Mul,ps) , Mul es -> pac env Lang.F.e_prod [] ps es
  | Assoc(`Bor,ps) , Fun(lf,es) when lf == Cint.f_lor ->
    pac env (Lang.F.e_fun lf) [] ps es
  | Assoc(`Band,ps) , Fun(lf,es) when lf == Cint.f_land ->
    pac env (Lang.F.e_fun lf) [] ps es
  | Assoc(`Bxor,ps) , Fun(lf,es) when lf == Cint.f_lxor ->
    pac env (Lang.F.e_fun lf) [] ps es
  | Assoc(`Concat,ts) , Fun(lf, es) when lf == Vlist.f_concat ->
    pac env (Lang.F.e_fun lf) [] ts es
  | Binop(p,`Div,q) , Div(a,b) -> pbinop env p q a b
  | Binop(p,`Mod,q) , Mod(a,b) -> pbinop env p q a b
  | Binop(p,`Eq,q) , Eq(a,b) -> pbinop env p q a b
  | Binop(p,`Ne,q) , Neq(a,b) -> pbinop env p q a b
  | Binop(p,`Lt,q) , Lt(a,b) -> pbinop env p q a b
  | Binop(p,`Le,q) , Leq(a,b) -> pbinop env p q a b
  | Binop(p,`Lsl,q) , Fun(lf,[a;b]) when lf == Cint.f_lsl -> pbinop env p q a b
  | Binop(p,`Lsr,q) , Fun(lf,[a;b]) when lf == Cint.f_lsr -> pbinop env p q a b
  | Implies(hps, cp), Imply(hs, c) ->
    pac env Lang.F.e_and [] hps hs ;
    pmatch env cp c
  | Forall(qs,p) , _ -> pbind env p.loc Qed.Logic.Forall qs p e
  | Exists(qs,p) , _ -> pbind env p.loc Qed.Logic.Exists qs p e
  | Times(b,p) , Times(a,e) ->
    let q,r = Z.div_rem a b in
    if Z.is_zero r then pmatch env p (Lang.F.e_times q e)
    else raise Not_found
  | Get(pa,pk) , Aget(a,k) ->
    pmatch env pa a ; pmatch env pk k
  | Set(pa,pk,pv) , Aset(a,k,v) ->
    pmatch env pa a ; pmatch env pk k ; pmatch env pv v
  | Field(pv,fid) , Rget(v,fd) when Lang.name_of_field fd = fid ->
    pmatch env pv v
  | Call(fid,ps,trail) , Fun(lf,es) when Lang.name_of_lfun lf = fid ->
    begin
      match Lang.Fun.category lf with
      | Operator op ->
        if op.associative then
          let rps = if trail then [{ loc = p.loc ; value = Any }] else [] in
          if op.commutative then
            pac env (Lang.F.e_fun lf) rps ps es
          else
            passoc env (Lang.F.e_fun lf) rps ps [] es
        else
          pargs env ps trail es
      | _ -> pargs env ps trail es
    end
  | Binop(pl,`Repeat,pn) , Fun(lf,[l;n]) when lf == Vlist.f_repeat ->
    pmatch env pl l ; pmatch env pn n
  | List _vs , _ -> ()
  | Pany ps , _ ->
    let ok = List.exists (fun p -> ptry env p e) ps in
    if not ok then raise Not_found
  | _ -> raise Not_found

and pbinop env p q a b = pmatch env p a ; pmatch env q b

(* Associative matching :
   - rps are (reversed) any-patterns to be matched with (reversed) rvs values
   - invariant is (rev rps @ ps) being matched with (rev rvs @ vs) *)
and passoc env op rps ps rvs vs =
  match ps with
  | [] -> pany env op (List.rev rps) (List.rev_append rvs vs)
  | p::ps ->
    if is_any p then passoc env op (p::rps) ps rvs vs
    else
      match vs with
      | [] -> raise Not_found
      | v::vs ->
        if ptry env p v then
          begin
            pany env op (List.rev rps) (List.rev rvs) ;
            passoc env op [] ps [] vs
          end
        else
          passoc env op rps ps (v::rvs) vs

(* AC matching:
   - rps are (reversed) any-patterns
   - invariant is (rev rs @ ps) being matched with es *)
and pac env op rps ps es =
  match ps with
  | p::ps ->
    if is_any p then pac env op (p::rps) ps es
    else
      let ep = List.find (ptry env p) es in
      let es = List.filter (fun e -> not @@ Lang.F.equal ep e) es in
      pac env op rps ps es
  | [] -> pany env op (List.rev rps) es

(* Match with backtracking *)
and ptry env p e =
  let sigma = env.sigma in
  let bvars = env.bvars in
  let binders = env.binders in
  try pmatch env p e ; true
  with Not_found ->
    env.sigma <- sigma ;
    env.bvars <- bvars ;
    env.binders <- binders ;
    false

(* Matching any-patterns rs with es *)
and pany env op rs es =
  match rs , es with
  | [] , [] -> ()
  | [] , _ | _ , [] -> raise Not_found
  | [r] , _ -> pmatch env r (op es)
  | r::rs , e::es -> pmatch env r e ; pany env op rs es

(* Pairwise matching *)
and pargs env ps trail es =
  match ps , es with
  | [] , [] -> ()
  | [] , _ when trail -> ()
  | p::ps , e::es -> pmatch env p e ; pargs env ps trail es
  | _ -> raise Not_found

and pbind env loc q qs p e =
  match qs with
  | [] -> pmatch env p e
  | (lt,x)::qs ->
    match Lang.F.repr e with
    | Bind(qe,tau,lc) when qe = q && is_type lt tau ->
      let var = Lang.F.fresh env.pool ~basename:x tau in
      let state = bind env x var in
      pbind env loc q qs p @@ Lang.F.QED.e_unbind var lc ;
      unbind env state
    | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Deep matching with marking                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec pchildren env p e =
  let rs = ref [] in
  Lang.F.lc_iter (fun e -> rs := e :: !rs) e ;
  List.exists (pchild env p) (List.rev !rs)

and pchild env p e = (* populate content *)
  if Lang.F.lc_closed e then
    not (Lang.F.Tset.mem e env.marked) &&
    begin
      env.marked <- Lang.F.Tset.add e env.marked ;
      ptry env p e || pchildren env p e
    end
  else
    pchildren env p e

let rec plist f =
  function [] -> None | x::xs ->
  match f x with
  | Some _ as result -> result
  | None -> plist f xs

(* -------------------------------------------------------------------------- *)
(* --- Pattern Lookup                                                     --- *)
(* -------------------------------------------------------------------------- *)

type lookup = {
  head: bool ;
  goal: bool ;
  hyps: bool ;
  split: bool ;
  pattern: pattern ;
}

let pclause ~pool { head ; pattern ; split } clause sigma prop =
  let tprop = Lang.F.e_prop prop in
  let select t =
    if t == tprop then Tactical.Clause clause else Tactical.Inside(clause,t) in
  let env = {
    pool ; sigma ; select ;
    binders = Vmap.empty ;
    bvars = Lang.F.Vars.empty ;
    marked = Lang.F.Tset.empty ;
  } in
  let pcond t =
    (* replace ptry with a populating content version *)
    if ptry env pattern t || (not head && pchildren env pattern t)
    then Some env.sigma else None
  in
  match Lang.F.repr tprop with
  | And ts when split -> plist pcond ts
  | _ -> pcond tprop

(* --- Step Ordering --- *)

let queue = Queue.create ()

let order (s : Conditions.step) : int =
  match s.condition with
  | Have _ -> 0
  | When _ -> 1
  | Branch _ -> 2
  | Core _ -> 3
  | Init _ -> 4
  | Type _ -> 5
  | Either _ -> 6
  | State _ -> 7
  | Probe _ -> 8

let priority sa sb = order sa - order sb

let push (step : Conditions.step) =
  match step.condition with
  | Have _ | When _ | Core _ | Init _ | Type _ | State _ | Probe _ -> ()
  | Branch(_,sa,sb) -> Queue.push sa queue ; Queue.push sb queue
  | Either cs -> List.iter (fun s -> Queue.push s queue) cs

(* --- Step Matching --- *)

let pstep ctxt sigma (step : Conditions.step) =
  let term = Conditions.head step in
  let clause = Tactical.Step step in
  pclause ctxt clause sigma term

(* --- Sequence Matching --- *)

let rec psequence ~pool ctxt sigma (seq : Conditions.sequence) =
  let steps = List.sort priority (Conditions.list seq) in
  match plist (pstep ~pool ctxt sigma) steps with
  | Some _ as result ->
    Queue.clear queue ; result
  | None ->
    List.iter push steps ;
    if Queue.is_empty queue then None else
      psequence ~pool ctxt sigma (Queue.pop queue)

(* --- Hypotheses Matching --- *)

let phyps ~pool ctxt sigma (seq : Conditions.sequent) =
  if not ctxt.hyps then None else
    psequence ~pool ctxt sigma (fst seq)

let pgoal ~pool ctxt sigma (seq : Conditions.sequent) =
  if not ctxt.goal then None else
    let goal = snd seq in
    let clause = Tactical.Goal goal in
    pclause ~pool ctxt clause sigma goal

let empty = Vmap.empty

let psequent ctxt sigma (seq : Conditions.sequent) =
  Conditions.index seq ;
  let pool = Lang.new_pool ~vars:(Conditions.vars_seq seq) () in
  match pgoal ~pool ctxt sigma seq with
  | Some _ as result -> result
  | None -> phyps ~pool ctxt sigma seq

(* -------------------------------------------------------------------------- *)
(* --- Composing Values                                                   --- *)
(* -------------------------------------------------------------------------- *)

let () = Lang.on_lfun
    begin fun lf ->
      let id = "lf:" ^ Lang.name_of_lfun lf in
      Tactical.add_computer id (Lang.F.e_fun lf)
    end

let () = Lang.on_field
    begin fun fd ->
      let id = "fd:" ^ Lang.name_of_field fd in
      Tactical.add_computer id (fun es -> Lang.F.e_getfield (List.hd es) fd)
    end

let log_error ~loc msg =
  Wp_parameters.logwith (fun _evt -> raise Not_found) ~source:(fst loc) msg

let getvar env (x : string loc) : Tactical.selection =
  try Vmap.find x.value env
  with Not_found ->
    log_error ~loc:x.loc "Pattern variable '%s' not bound" x.value

let rec select (env : sigma) (a : value) =
  let loc = a.loc in
  let cc = select env in
  match a.value with
  | Any ->  log_error ~loc "Pattern _ is not a value"
  | Pany _ ->  log_error ~loc "Pattern \\any(…) is not a value"
  | Forall _ -> log_error ~loc "Pattern \\forall is not a value"
  | Exists _ -> log_error ~loc "Pattern \\exists is not a value"
  | String s -> log_error ~loc "Pattern %S can not be used as value" s
  | Pvar x -> getvar env x
  | Named (_,_,v) -> cc v
  | Range(a,b) -> Tactical.range a b
  | Int n -> Tactical.cint n
  | Bool b -> Tactical.compose (if b then "wp:true" else "wp:false") []
  | Not a -> Tactical.compose "wp:not" [cc a]
  | Assoc(op,vs) ->
    let op = match op with
      | `Add -> "wp:add"
      | `Mul -> "wp:mul"
      | `Concat -> "wp:concat"
      | `And -> "wp:and"
      | `Or -> "wp:or"
      | `Bor -> "lf:lor"
      | `Band -> "lf:land"
      | `Bxor -> "lf:lxor"
    in Tactical.compose op (List.map (cc) vs)
  | Implies(hyps, c) ->
    Tactical.compose "wp:imply" (List.rev @@ cc c :: (List.rev_map cc hyps))
  | Binop(a,op,b) ->
    let op = match op with
      | `Div -> "wp:div"
      | `Mod -> "wp:mod"
      | `Eq -> "wp:eq"
      | `Ne -> "wp:neq"
      | `Lt -> "wp:lt"
      | `Le -> "wp:leq"
      | `Repeat -> "wp:repeat"
      | `Lsl -> "lf:lsl"
      | `Lsr -> "lf:lsr"
    in compose env ~loc op [a;b]
  | Times(k,v) -> Tactical.compose "wp:mul" [Tactical.cint k;cc v]
  | Get(a,k) -> Tactical.compose "wp:get" [cc a;cc k]
  | Set(a,k,v) -> Tactical.compose "wp:set" [cc a;cc k;cc v]
  | List vs -> Tactical.compose "wp:list" (List.map cc vs)
  | Field(v,id) -> compose env ~loc ("fd:" ^ id) [v]
  | Call(id,vs,_) -> compose env ~loc ("lf:" ^ id) vs

and compose env ~loc id vs =
  match Tactical.compose id (List.map (select env) vs) with
  | Tactical.Empty -> log_error ~loc "Computer %S not found" id
  | result -> result

let bool (a : value) =
  match a.value with
  | Bool b -> b
  | _ -> log_error ~loc:a.loc "Not a boolean value (%a)" pp a

let string (a : value) =
  match a.value with
  | String s -> s
  | _ -> log_error ~loc:a.loc "Not a string value (%a)" pp a

(* -------------------------------------------------------------------------- *)
(* --- Typechecking                                                       --- *)
(* -------------------------------------------------------------------------- *)

type vtype =
  | Tnone | Tany | Numerical | Boolean | String
  | List of vtype
  | Array of vtype * vtype
  | Type of Lang.F.tau

let vint = Type Qed.Logic.Int
let vbool = Type Qed.Logic.Bool
let vlist = List Tany

let list = function
  | Type t -> Type (Vlist.alist t)
  | Tnone -> Tnone
  | v -> List v

let array vk ve =
  match vk , ve with
  | Type tk, Type te -> Type (Qed.Logic.Array(tk,te))
  | Tnone , _ | _ , Tnone -> Tnone
  | _ -> Array(vk,ve)

let rec vmerge va vb =
  if va == vb then vb else
    match va, vb with
    | Tany , v | v, Tany -> v
    (* numerical *)
    | Numerical, Numerical -> Numerical
    | Numerical, Type (Int | Real) -> vb
    | Type (Int | Real), Numerical -> va
    | Type Int , Type Real -> vb
    | Type Real , Type Int -> va
    (* boolean *)
    | Boolean, Boolean -> Boolean
    | Boolean, Type (Bool | Prop) -> vb
    | Type (Bool | Prop) , Boolean -> va
    | Type Bool , Type Prop -> vb
    | Type Prop , Type Bool -> va
    (* list *)
    | List u , List v -> list (vmerge u v)
    | (List m, Type t) | (Type t , List m) ->
      begin
        match Vlist.elist t with
        | None -> Tnone
        | Some te -> list (vmerge m (Type te))
      end
    (* arrays *)
    | Array(vk,ve) , Array(uk,ue) ->
      array (vmerge vk uk) (vmerge ve ue)
    | (Array(vk,ve) , Type(Array(tk,te)))
    | (Type(Array(tk,te)) , Array(vk,ve)) ->
      array (vmerge vk (Type tk)) (vmerge ve (Type te))
    (* types *)
    | Type ta , Type tb -> if Lang.F.Tau.equal ta tb then vb else Tnone
    | _ -> Tnone

let rec vpretty fmt = function
  | Tnone -> Format.fprintf fmt "\\none"
  | Tany -> Format.fprintf fmt "\\any"
  | List v -> Format.fprintf fmt "\\list(%a)" vpretty v
  | Array(vk,ve) -> Format.fprintf fmt "%a[%a]" vpretty vk vpretty ve
  | String -> Format.fprintf fmt "string"
  | Numerical -> Format.fprintf fmt "number"
  | Boolean -> Format.fprintf fmt "boolean"
  | Type t -> Lang.F.Tau.pretty fmt t

type env = {
  mutable table: vtype Vmap.t ;
  raise: bool ;
}

let env ?(raise=false) () = {
  table = Vmap.empty ;
  raise ;
}

let typecheck_error env loc msg =
  if env.raise
  then Format.kasprintf (fun e -> raise (TypeError(loc, e))) msg
  else Wp_parameters.error ~source:(fst loc) msg

let tc_merge env ~loc ~expected va =
  let v = vmerge va expected in
  if v = Tnone then
    typecheck_error env loc
      "Invalid type %a (expected %a)" vpretty va vpretty expected ; v

let tc_var env ~loc ~expected x =
  let vx = try Vmap.find x env.table with Not_found -> Tany in
  let vy = tc_merge env ~loc ~expected vx in
  if vx != vy then env.table <- Vmap.add x vy env.table ; vy

let rec typecheck env expected (a : ast) =
  let loc = a.loc in
  match a.value with
  | Any -> expected
  | Pany ps -> List.fold_left (typecheck env) expected ps
  | Pvar x -> tc_var env ~loc ~expected x.value
  | Named(_,x,v) -> tc_var env ~loc ~expected:(typecheck env expected v) x.value
  | Range(a,b) ->
    if a > b then
      typecheck_error env loc "Invalid range %d..%d" a b ;
    tc_merge env ~loc ~expected vint
  | Int _ -> tc_merge env ~loc ~expected vint
  | Bool _ -> tc_merge env ~loc ~expected vbool
  | String _ -> tc_merge env ~loc ~expected String
  | Not a -> typecheck env (tc_merge env ~loc ~expected vbool) a
  | Implies (hyps, c) ->
    List.fold_left
      (typecheck env)
      (tc_merge env ~loc ~expected vbool)
      (c :: hyps)
  | Assoc((`And|`Or),vs) ->
    List.fold_left (typecheck env) (tc_merge env ~loc ~expected vbool) vs
  | Assoc((`Bor|`Band|`Bxor),vs) ->
    List.fold_left (typecheck env) (tc_merge env ~loc ~expected vint) vs
  | Assoc((`Add|`Mul),vs) ->
    List.fold_left (typecheck env) (tc_merge env ~loc ~expected Numerical) vs
  | Assoc(`Concat,vs) ->
    List.fold_left (typecheck env) (tc_merge env ~loc ~expected vlist) vs
  | Binop(a,(`Eq | `Ne),b) ->
    let va = typecheck env Tany a in
    let vb = typecheck env Tany b in
    ignore @@ tc_merge env ~loc ~expected:va vb ;
    tc_merge env ~loc ~expected Boolean
  | Binop(a,(`Lt | `Le),b) ->
    let va = typecheck env Numerical a in
    let vb = typecheck env Numerical b in
    ignore @@ tc_merge env ~loc ~expected:va vb ;
    tc_merge env ~loc ~expected Boolean
  | Binop(a,`Div,b) ->
    let vn = tc_merge env ~loc ~expected Numerical in
    let va = typecheck env vn a in
    let vb = typecheck env vn b in
    tc_merge env ~loc ~expected:va vb
  | Binop(a,(`Mod|`Lsl|`Lsr),b) ->
    ignore @@ typecheck env vint a ;
    ignore @@ typecheck env vint b ;
    tc_merge env ~loc ~expected vint
  | Binop(a,`Repeat,b) ->
    ignore @@ typecheck env vint b ;
    typecheck env (tc_merge env ~loc ~expected vlist) a
  | Times(_,v) -> typecheck env (tc_merge env ~loc ~expected Numerical) v
  | List vs ->
    let ve = List.fold_left (typecheck env) Tany vs in
    tc_merge env ~loc ~expected (List ve)
  | Get(a,k) ->
    let vk = typecheck env Tany k in
    begin
      match typecheck env (Array(vk,expected)) a with
      | Array(_,ve) -> ve
      | Type(Array(_,te)) -> Type te
      | va ->
        typecheck_error env a.loc "Not an array type (%a)" vpretty va ;
        expected
    end
  | Set(a,k,v) ->
    let vk = typecheck env Tany k in
    let ve = typecheck env expected v in
    typecheck env (array vk ve) a
  | Field(v,fid) ->
    begin
      match typecheck env Tany v with
      | Type(Record fds) ->
        begin
          try
            let (_,ft) =
              List.find (fun (fd,_) -> Lang.name_of_field fd = fid) fds in
            tc_merge env ~loc ~expected (Type ft)
          with Not_found -> expected
        end
      | Tany -> expected
      | vr ->
        typecheck_error env v.loc "Not a record type (%a)" vpretty vr ;
        expected
    end
  | Call(_f,vs,_) ->
    List.iter (fun v -> ignore @@ typecheck env Tany v) vs ; expected
  | Forall(_,p) | Exists(_,p) ->
    ignore @@ typecheck env Boolean p ;
    tc_merge env ~loc ~expected (Type Prop)

let typecheck_vtau env ?tau v =
  ignore @@ typecheck env (match tau with None -> Tany | Some t -> Type t) v

let typecheck_value = typecheck_vtau
let typecheck_pattern = typecheck_vtau

let typecheck_lookup env p =
  ignore @@ typecheck env (if p.head then Boolean else Tany) p.pattern

(* -------------------------------------------------------------------------- *)
