(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype
module Fmap = Fieldinfo.Map

type 'a t =
  | Pure
  | Dvar   of string
  | Ptr    of 'a
  | Array  of 'a t (* no pure *)
  | Record of 'a t Fmap.t (* not all pure *)
  | Logic  of logic_type_info * 'a t list (* not all pure *)
  | Arrow  of 'a t list * 'a t (* not all pure *)

(* -------------------------------------------------------------------------- *)
(* ---  Printer                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec pretty pp fmt = function
  | Pure -> Format.pp_print_string fmt "_"
  | Dvar v -> Format.fprintf fmt "D.%s" v
  | Ptr r -> pp fmt r
  | Array d -> pretty pp fmt d ; Format.pp_print_string fmt "[]"
  | Record m ->
    Format.fprintf fmt "@[<hov 0>@[<hov 2>{" ;
    Fmap.iter
      (fun fd d ->
         Format.fprintf fmt "@ %a: %a;" Fieldinfo.pretty fd (pretty pp) d
      ) m ;
    Format.fprintf fmt "@]@ }@]"
  | Logic(a,[]) -> Logic_type_info.pretty fmt a
  | Logic(a,d::ds) ->
    Format.fprintf fmt "@[<hov 2>%a<%a" Logic_type_info.pretty a (pretty pp) d ;
    List.iter (Format.fprintf fmt ",@,%a" (pretty pp)) ds ;
    Format.fprintf fmt ">@]"
  | Arrow ([],dr) -> pretty pp fmt dr
  | Arrow (d::ds,dr) ->
    Format.fprintf fmt "@[<hov 2>%a" (pretty pp) d ;
    List.iter (Format.fprintf fmt "->@,%a" (pretty pp)) ds ;
    Format.fprintf fmt "@[:%a@]" (pretty pp) dr

(* -------------------------------------------------------------------------- *)
(* ---  Smart constructors                                                --- *)
(* -------------------------------------------------------------------------- *)

let is_pure d = (d == Pure)
let pure = Pure
let ptr r = Ptr r
let scalar = function None -> Pure | Some r -> Ptr r
let array d = if d == Pure then Pure else Array d
let field fd d = if d == Pure then Pure else Record (Fmap.singleton fd d)
let record m =
  if Fmap.is_empty m || Fmap.for_all (fun _ -> is_pure) m then Pure
  else Record m

let logic s l =
  if Logic_const.is_unrollable_ltdef s then invalid_arg "Region.LDomain.logic"
  else if List.for_all is_pure l then Pure
  else Logic (s,l)

let rec arrow ds d =
  if ds = [] then d
  else if is_pure d && List.for_all is_pure ds then pure
  else match d with
    | Arrow (ds2, d) -> arrow (List.concat [ds;ds2]) d
    | _ -> Arrow (ds, d)

(* -------------------------------------------------------------------------- *)
(* ---  Merge                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec collect f w = function
  | Pure | Dvar _ -> w
  | Ptr r -> Some (match w with None -> r | Some r0 -> f r0 r)
  | Array d -> collect f w d
  | Record m -> Fmap.fold (fun _ d w -> collect f w d) m w
  | Logic(_,ds) -> List.fold_left (collect f) w ds
  | Arrow(ds,dr) -> List.fold_left (collect f) (collect f w dr) ds

let pointed f d = collect f None d

let rec merge f d1 d2 =
  match d1, d2 with
  | Pure, d | d, Pure -> d
  | Dvar a, Dvar b -> Dvar (min a b) (* should never apply *)
  | Ptr r1, Ptr r2 -> Ptr (f r1 r2)
  | Record m1, Record m2 ->
    Record (Fmap.union (fun _ d1 d2 -> Some (merge f d1 d2)) m1 m2)
  | Array d1, Array d2 -> Array (merge f d1 d2)
  | Logic (a1, ds1), Logic (a2, ds2) when Logic_type_info.equal a1 a2 ->
    Logic (a1, List.map2 (merge f) ds1 ds2)
  | Arrow(ds1,dr1), Arrow(ds2,dr2) when List.compare_lengths ds1 ds2 = 0 ->
    arrow (List.map2 (merge f) ds1 ds2) @@ merge f dr1 dr2
  | _ -> scalar @@ collect f (collect f None d1) d2

(* -------------------------------------------------------------------------- *)
(* ---  Getters                                                           --- *)
(* -------------------------------------------------------------------------- *)

let get f = function Pure | Ptr _ as d -> d | d -> scalar @@ pointed f d

let get_index f = function Array d -> d | d -> get f d

let get_field f d fd =
  match d with
  | Record mf -> (try Fmap.find fd mf with Not_found -> Pure)
  | _ -> get f d

let rec iter f = function
  | Pure -> ()
  | Dvar _ -> ()
  | Ptr n -> f n
  | Array d -> iter f d
  | Record m -> Fmap.iter (fun _ -> iter f) m
  | Logic (_,ds) -> List.iter (iter f) ds
  | Arrow (ds,d) -> List.iter (iter f) ds ; iter f d

(* -------------------------------------------------------------------------- *)
(* ---  Transform type into domain                                        --- *)
(* -------------------------------------------------------------------------- *)

module M = Map.Make(String)
type 'a context = 'a t M.t
let empty = M.empty
let make l : 'a context = List.fold_left (fun m (s,r) -> M.add s r m) empty l

let getvar ?(default=pure) ctxt v =
  try M.find v ctxt with Not_found -> default

let rec of_typ create ty : 'a t = match ty.tnode with
  | TBuiltin_va_list  | TFun _ | TPtr _ -> ptr @@ create ()
  | TArray (ty,_) -> array @@ of_typ create ty
  | TComp { cfields = Some fds } ->
    let add_field m fd =
      let v = of_typ create fd.ftype in
      if is_pure v then m else Fmap.add fd v m in
    let m = List.fold_left add_field Fmap.empty fds in
    if Fmap.is_empty m then Pure else Record m
  | TVoid | TInt _ | TFloat _ | TComp _ | TEnum _ -> pure
  | TNamed { ttype } -> of_typ create ttype

let rec of_ltype create lt =
  match Ast_types.unroll_logic ~unroll_typedef:false lt with
  | Ctype ty -> of_typ create ty
  | Lvar v -> Dvar v
  | Ltype (ti,ts) -> logic ti @@ List.map (of_ltype create) ts
  | Lboolean | Linteger | Lreal -> pure
  | Larrow (prms,ty) ->
    arrow (List.map (of_ltype create) prms) @@ of_ltype create ty

(* -------------------------------------------------------------------------- *)
(* ---  Unification                                                       --- *)
(* -------------------------------------------------------------------------- *)

type 'a sigma = 'a context ref

let rec unify (f:'a -> 'a -> 'a) (s:'a sigma) (d1:'a t) (d2:'a t) =
  match d1, d2 with
  | Pure, _ | _, Pure -> ()
  | Dvar v, _ -> s := M.add v (merge f d2 @@ getvar !s v) !s
  | Ptr r1, Ptr r2 -> ignore @@ f r1 r2
  | Array d1, Array d2 -> unify f s d1 d2
  | Record m1, Record m2 ->
    ignore @@ Fmap.union (fun _ d1 d2 -> unify f s d1 d2 ; None) m1 m2
  | Logic (t1,ds1), Logic(t2,ds2) when Logic_type_info.equal t1 t2 ->
    List.iter2 (unify f s) ds1 ds2
  | Arrow(ds1,r1), Arrow(ds2,r2) when List.compare_lengths ds1 ds2 = 0 ->
    List.iter2 (unify f s) (r1::ds1) (r2::ds2)
  | Ptr _, _ -> ignore @@ merge f d1 d2
  | Array d, _ -> unify f s d @@ scalar @@ pointed f d2
  | Record m, _ ->
    begin match pointed f d2 with
      | None -> ()
      | Some r -> let d' = ptr r in Fmap.iter (fun _ d -> unify f s d d') m
    end
  | Logic(_,ds), _ ->
    begin match pointed f d2 with
      | None -> ()
      | Some r -> let d' = ptr r in List.iter (fun d -> unify f s d d') ds
    end
  | Arrow(ds,dr), _ ->
    begin match pointed f d2 with
      | None -> ()
      | Some r -> let d' = ptr r in List.iter (fun d -> unify f s d d') (dr::ds)
    end

let rec subst ctxt = function
  | (Pure | Ptr _) as d -> d
  | (Dvar v) as d -> getvar ~default:d ctxt v
  | Array a -> array @@ subst ctxt a
  | Record m -> record @@ Fmap.map (subst ctxt) m
  | Logic(t,ds) -> logic t @@ List.map (subst ctxt) ds
  | Arrow(ds,d) -> arrow (List.map (subst ctxt) ds) @@ subst ctxt d

(* -------------------------------------------------------------------------- *)
