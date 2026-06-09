(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- L-Value Indexed Memory Model                                       --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Lang
open Lang.F
open Memory

module Logic = Qed.Logic

let datatype = "MemZeroAlias"

let configure () =
  begin
    let orig_pointer = Context.push Lang.pointer Logic.Int in
    let orig_null    = Context.push Cvalues.null (p_equal e_zero) in
    let rollback () =
      Context.pop Lang.pointer orig_pointer ;
      Context.pop Cvalues.null orig_null ;
    in
    rollback
  end
let no_binder = { bind = fun _ f v -> f v }
let configure_ia _ = no_binder

(* TODO: compute actual separation hypotheses *)
let hypotheses p = p

(* -------------------------------------------------------------------------- *)
(* --- Chunks                                                             --- *)
(* -------------------------------------------------------------------------- *)

type chunk = varinfo * path list (* from left to right *)
and path = S | I | F of fieldinfo

let hash_path = function S -> 1 | I -> 2 | F fd -> Fieldinfo.hash fd

let equal_path p q = match p,q with
  | S , S -> true
  | I , I -> true
  | F f , F g -> Fieldinfo.equal f g
  | _ -> false

let compare_path p q = match p,q with
  | S , S -> 0
  | S , _ -> (-1)
  | _ , S -> 1
  | I , I -> 0
  | I , _ -> (-1)
  | _ , I -> 1
  | F f , F g -> Fieldinfo.compare f g

[@@@ warning "-32"]
let pp_path fmt = function
  | S -> Format.pp_print_char fmt '*'
  | I -> Format.pp_print_string fmt "[]"
  | F f -> Format.pp_print_char fmt '.' ; Fieldinfo.pretty fmt f
[@@@ warning "+32"]

let rec object_of_rpath x = function
  | [] -> Ctypes.object_of x.vtype
  | S :: p -> Ctypes.object_of_pointed (object_of_rpath x p)
  | I :: p -> Ctypes.object_of_array_elem (object_of_rpath x p)
  | F f :: _ -> Ctypes.object_of f.ftype

let rec dim_of_path t = function
  | [] -> t
  | S :: p | F _ :: p -> dim_of_path t p
  | I :: p -> dim_of_path Qed.Logic.(Array(Int,t)) p

module Chunk =
struct
  type t = chunk
  let self = "mtree"
  let hash (x,p) = Qed.Hcons.hash_list hash_path (Varinfo.hash x) p
  let equal (x,p) (y,q) = Varinfo.equal x y && Qed.Hcons.equal_list equal_path p q
  let compare (x,p) (y,q) =
    let cmp = Varinfo.compare x y in
    if cmp <> 0 then cmp else Qed.Hcons.compare_list compare_path p q

  let rec pp x fmt = function
    | [] -> Varinfo.pretty fmt x
    | [S] -> Format.fprintf fmt "*%a" Varinfo.pretty x
    | S::ps -> Format.fprintf fmt "*(%a)" (pp x) ps
    | I::ps -> Format.fprintf fmt "%a[]" (pp x) ps
    | F f::S::ps -> Format.fprintf fmt "%a->%a" (pp x) ps Fieldinfo.pretty f
    | F f::ps -> Format.fprintf fmt "%a.%a" (pp x) ps Fieldinfo.pretty f

  let pretty fmt (x,p) = Format.fprintf fmt "@[<hov 2>%a@]" (pp x) (List.rev p)
  let tau_of_chunk (x,p) =
    let te = Lang.tau_of_object (object_of_rpath x (List.rev p)) in
    dim_of_path te p
  let basename_of_chunk (x,_) = LogicUsage.basename x
  let is_init _ = false
  let is_primary _ = true
  let is_framed (x,p) = not x.vglob && p = []
end

module State = Sigma.Make(Chunk)

type loc =
  | Null
  | Var of varinfo
  | Star of loc
  | Array of loc * F.term
  | Field of loc * fieldinfo

type segment = loc rloc

let rec pretty fmt = function
  | Null -> Format.pp_print_string fmt "null"
  | Var x -> Varinfo.pretty fmt x
  | Star(Var x) -> Format.fprintf fmt "*%a" Varinfo.pretty x
  | Star p -> Format.fprintf fmt "*(%a)" pretty p
  | Array(p,k) -> Format.fprintf fmt "%a[%a]" pretty p Lang.F.pp_term k
  | Field(Star p,f) -> Format.fprintf fmt "%a->%a" pretty p Fieldinfo.pretty f
  | Field(p,f) -> Format.fprintf fmt "%a.%a" pretty p Fieldinfo.pretty f

let rec vars = function
  | Var _ | Null -> Vars.empty
  | Star p | Field(p,_) -> vars p
  | Array(p,k) -> Vars.union (vars p) (F.vars k)

let rec occurs x = function
  | Null | Var _ -> false
  | Star p | Field(p,_) -> occurs x p
  | Array(p,k) -> F.occurs x k || occurs x p

let source = "Tree Model"

let null = Null
let pointer_loc _t = Warning.error ~source "No Pointer Loc"
let pointer_val _v = Warning.error ~source "No Pointer Val"

let cvar x = Var x
let field l f = Field(l,f)
let shift l _obj k = Array(l,k)

let base_addr _l = Warning.error ~source "No Base Addr"
let base_offset _l = Warning.error ~source "No Offset Addr"
let block_length _s _obj _l = Warning.error ~source "No Block Length"

let cast _ _l = Warning.error ~source "No Cast"
let loc_of_int _ _ = Warning.error ~source "No Hardware Address"
let int_of_loc _ _ = Warning.error ~source "No Hardware Address"

let rec walk ps ks = function
  | Null -> Warning.error ~source "No Null Walk"
  | Var x -> (x,ps),ks
  | Star l -> walk (S::ps) ks l
  | Array(l,k) -> walk (I::ps) (k::ks) l
  | Field(l,f) -> walk (F f::ps) ks l

let access l = walk [] [] l

let domain _obj l =
  try State.singleton (fst (access l))
  with _ -> Sigma.empty

let is_well_formed _s = p_true

let value sigma l =
  let m,ks = access l in
  let x = State.get sigma m in
  List.fold_left F.e_get (e_var x) ks

let rec update a ks v =
  match ks with
  | [] -> v
  | k::ks -> F.e_set a k (update (F.e_get a k) ks v)

let set s m ks v = if ks = [] then v else update (State.value s m) ks v

let load sigma obj l =
  if Ctypes.is_pointer obj then Loc (Star l) else Val(value sigma l)

let load_init _sigma _obj _l = Warning.error ~source "Mem0Alias: No initialized"

let stored seq _obj l e =
  let m,ks = access l in
  let x = State.value seq.post m in
  [Set( x , set seq.pre m ks e )]

let stored_init _seq _obj _l _e = Warning.error ~source "Mem0Alias: No initialized"

let copied seq obj a b =
  stored seq obj a (value seq.pre b)

let copied_init _seq _obj _a _b = Warning.error ~source "Mem0Alias: No initialized"

let assigned _s _obj _sloc = []

let rec ipath lv = function
  | [] -> lv
  | S::w -> ipath (Mval lv,[]) w
  | I::_ -> raise Not_found
  | F f::w ->
    let (host,path) = lv in
    ipath (host, path @ [Mfield f]) w
let ilval (x,p) = ipath (Mvar x,[]) p

let lookup (s : Sigma.state) (e : F.term) =
  match Sigma.ckind @@ Lang.F.Tmap.find e s with
  | State.Mu lv -> Mlval (ilval lv)
  | _ -> raise Not_found

module Hmap = Sigma.Heap.Map

let heap domain state =
  Tmap.fold
    (fun m c w ->
       if Vars.intersect (F.vars m) domain
       then Hmap.add c m w else w
    ) state Hmap.empty

let updates seq domain =
  let pre = heap domain seq.pre in
  let post = heap domain seq.post in
  let pool = ref Bag.empty in
  Hmap.iter2
    (fun c v1 v2 ->
       match Sigma.ckind c with
       | State.Mu m ->
         begin
           try
             match v1,v2 with
             | _,None -> ()
             | None,Some v ->
               pool := Bag.add (Mstore(ilval m,v)) !pool
             | Some v1,Some v2 ->
               if v2 != v1 then
                 pool := Bag.add (Mstore (ilval m,v2)) !pool
           with Not_found -> ()
         end
       | _ -> ()
    ) pre post ;
  !pool

let no_pointer () = Warning.error ~source "No Pointer"

let is_null = function Null -> F.p_true | _ -> no_pointer ()
let loc_eq _ _ = no_pointer ()
let loc_lt _ _ = no_pointer ()
let loc_leq _ _ = no_pointer ()
let loc_neq _ _ = no_pointer ()
let loc_diff _ _ _ = no_pointer ()

let frame _sigma = []
let alloc sigma _xs = sigma
let scope _seq _s _xs = []
let valid _sigma _acs _l = Warning.error ~source "No validity"
let invalid _sigma _l = Warning.error ~source "No validity"
let initialized _sigma _l = Warning.error ~source "Mem0Alias: No initialized"
let global _sigma _p = p_true

let included _s1 _s2 = no_pointer ()
let separated _s1 _s2 = no_pointer ()
