(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Region Memory Model                                                --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Lang.F
open Memory
open Sigma
open MemMemory

type prim = Int of c_int | Float of c_float | Ptr
type kind = Single of prim | Many of prim | Garbled

let pp_prim fmt = function
  | Int i -> Ctypes.pp_int fmt i
  | Float f -> Ctypes.pp_float fmt f
  | Ptr -> Format.pp_print_string fmt "ptr"

let pp_kind fmt = function
  | Single p -> pp_prim fmt p
  | Many p -> Format.fprintf fmt "[%a]" pp_prim p
  | Garbled -> Format.pp_print_string fmt "[bytes]"

let tau_of_prim = function
  | Int _ -> Qed.Logic.Int
  | Float f -> Cfloat.tau_of_float f
  | Ptr -> MemAddr.t_addr

(* -------------------------------------------------------------------------- *)
(* --- Region Analysis Proxy                                              --- *)
(* -------------------------------------------------------------------------- *)

module type RegionProxy =
sig
  type region
  val id : region -> int
  val of_id : int -> region option
  val pretty : Format.formatter -> region -> unit
  val kind : region -> kind
  val name : region -> string option
  val cvar : varinfo -> region option
  val field : region -> fieldinfo -> region option
  val shift : region -> c_object -> region option
  val points_to : region -> region option
  val literal : eid:int -> Cstring.cst -> region option
  val separated : region -> region -> bool
  val included : region -> region -> bool
  val footprint : region -> region list
end

(* -------------------------------------------------------------------------- *)
(* --- Region Memory Model                                                --- *)
(* -------------------------------------------------------------------------- *)

module Make
    (R:RegionProxy)
    (M:Model)
    (L:MemLoader.Model with type loc = M.loc) =
struct

  type region = R.region
  let datatype = "MemRegion.Make"
  (* For projectification. Must be unique among models. *)

  let configure = M.configure
  let configure_ia = M.configure_ia
  let hypotheses = M.hypotheses

  module Chunk =
  struct
    let self = "MemRegion.Chunk"

    type data = Value of prim | Array of prim | ValInit | ArrInit
    type t = { data : data ; region : R.region }

    let pp_data fmt = function
      | Value p -> Format.fprintf fmt "%t%a" Unicode.pp_mu pp_prim p
      | Array p -> Format.fprintf fmt "%t%a[]" Unicode.pp_mu pp_prim p
      | ValInit -> Format.fprintf fmt "%tinit" Unicode.pp_mu
      | ArrInit -> Format.fprintf fmt "%tinit[]" Unicode.pp_mu

    let hash { data ; region } = Hashtbl.hash (data, R.id region)

    let equal a b = Stdlib.(=) a.data b.data && R.id a.region = R.id b.region
    let compare a b =
      let cmp = Stdlib.compare a.data b.data in
      if cmp <> 0 then cmp else Int.compare (R.id a.region) (R.id b.region)

    let pretty fmt { data ; region } =
      Format.fprintf fmt "%a@%03d" pp_data data (R.id region)

    let tau_of_chunk { data } =
      match data with
      | Value p -> tau_of_prim p
      | ValInit -> Qed.Logic.Bool
      | Array p -> Qed.Logic.Array(MemAddr.t_addr,tau_of_prim p)
      | ArrInit -> Qed.Logic.Array(MemAddr.t_addr,Qed.Logic.Bool)

    let basename_of_chunk c =
      match c.data with
      | ValInit -> "Vinit"
      | ArrInit -> "Minit"
      | Array p -> Format.asprintf "M%04x_%a" (R.id c.region) pp_prim p
      | Value p ->
        match R.name c.region with
        | Some a -> a
        | None -> Format.asprintf "V%04x_%a" (R.id c.region) pp_prim p

    let is_init c =
      match c.data with
      | ValInit | ArrInit -> true
      | Array _ | Value _ -> false

    let is_primary c =
      match c.data with
      | Value _ -> true
      | ValInit | ArrInit | Array _ -> false

    let is_framed _ = false

  end

  module State = Sigma.Make(Chunk)

  (* -------------------------------------------------------------------------- *)
  (* --- Region Loader                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  module LOADER =
  struct
    let name = "MemRegion.LOADER"

    type loc =
      | Null
      | Raw of M.loc
      | Loc of M.loc * region

    let pretty fmt (l: loc) =
      match l with
      | Null -> M.pretty fmt M.null
      | Raw l -> M.pretty fmt l
      | Loc (l,r) -> Format.fprintf fmt "%a@%a" M.pretty l R.pretty r

    let make a = function None -> Raw a | Some r -> Loc(a,r)
    let loc = function Null -> M.null | Raw a | Loc(a,_) -> a
    let reg = function Null | Raw _ -> None | Loc(_,r) -> Some r
    let rfold f = function Null | Raw _ -> None | Loc(_,r) -> f r

    (* ---------------------------------------------------------------------- *)
    (* --- Utilities on locations                                         --- *)
    (* ---------------------------------------------------------------------- *)

    let localized action = function
      | Null ->
        Warning.error ~source:"MemRegion"
          "Attempt to %s at NULL" action
      | Raw a ->
        Warning.error ~source:"MemRegion"
          "Attempt to %s without region (%a)" action M.pretty a
      | Loc(l,r) -> l,r

    let sizeof ty = L.sizeof ty
    let to_addr l = M.pointer_val (loc l)
    let last sigma ty l = L.last sigma ty (loc l)

    let field l fd =
      make (M.field (loc l) fd) (rfold (fun r -> R.field r fd) l)

    let ofield l fd =
      Option.map (fun r -> Loc (M.field (loc l) fd, r))
      @@ rfold (fun r -> R.field r fd) l

    let shift l obj ofs =
      make (M.shift (loc l) obj ofs) (rfold (fun r -> R.shift r obj) l)

    let fresh l =
      let l0,r = localized "quantify loc" l in
      let xs, l1 = L.fresh l0 in
      xs, Loc(l1,r)

    let separated p n p' n' = L.separated (loc p) n (loc p') n'

    let eqmem chunk m0 m1 l n =
      match Sigma.ckind chunk with
      | State.Mu { data = ValInit | Value _ } ->
        p_equal m0 m1
      | State.Mu { data = ArrInit | Array _ } ->
        p_call f_eqmem [m0;m1;to_addr l;n]
      | _ -> L.eqmem chunk m0 m1 (loc l) n

    let memcpy chunk m0 l0 m1 l1 n =
      match Sigma.ckind chunk with
      | State.Mu { data = ValInit | Value _ } -> m1
      | State.Mu { data = ArrInit | Array _ } ->
        e_fun f_memcpy [m0;to_addr l0;m1;to_addr l1;n]
      | _ -> L.memcpy chunk m0 (loc l0) m1 (loc l1) n

    (* ---------------------------------------------------------------------- *)
    (* --- Load                                                           --- *)
    (* ---------------------------------------------------------------------- *)

    let to_region_pointer l =
      let l,r = localized "get region pointer" l in R.id r, M.pointer_val l

    let of_region_pointer r _ t =
      make (M.pointer_loc t) (R.of_id r)

    let check_access action (p:prim) (q:prim) =
      if Stdlib.(<>) p q then
        Warning.error ~source:"MemRegion"
          "Inconsistent %s (%a <> %a)"
          action pp_prim p pp_prim q

    let load_int sigma iota loc : term =
      let l,r = localized "load int" loc in
      match R.kind r with
      | Garbled -> L.load_int sigma iota l
      | Single p ->
        check_access "load" p (Int iota) ;
        State.value sigma { data = Value p ; region = r }
      | Many p ->
        check_access "load" p (Int iota) ;
        e_get
          (State.value sigma { data = Array p ; region = r})
          (M.pointer_val l)

    let load_float sigma flt loc : term =
      let l,r = localized "load float" loc in
      match R.kind r with
      | Garbled -> L.load_float sigma flt l
      | Single p ->
        check_access "load" p (Float flt) ;
        State.value sigma { data = Value p ; region = r }
      | Many p ->
        check_access "load" p (Float flt) ;
        e_get
          (State.value sigma { data = Array p ; region = r})
          (M.pointer_val l)

    let load_pointer sigma ty loc : loc =
      let l,r = localized "load pointer" loc in
      match R.points_to r with
      | None ->
        Warning.error ~source:"MemRegion"
          "Attempt to load pointer without points-to@\n\
           (addr %a, region %a)"
          M.pretty l R.pretty r
      | Some _ as rp ->
        let loc =
          match R.kind r with
          | Garbled -> L.load_pointer sigma ty l
          | Single p ->
            check_access "load" p Ptr ;
            M.pointer_loc @@
            State.value sigma { data = Value p ; region = r }
          | Many p ->
            check_access "load" p Ptr ;
            M.pointer_loc @@
            e_get
              (State.value sigma { data = Array p ; region = r})
              (M.pointer_val l)
        in make loc rp

    (* ---------------------------------------------------------------------- *)
    (* --- Store                                                          --- *)
    (* ---------------------------------------------------------------------- *)

    let store_int sigma iota loc v : Sigma.chunk * term =
      let l,r = localized "store int" loc in
      match R.kind r with
      | Garbled -> L.store_int sigma iota l v
      | Single p ->
        check_access "store" p (Int iota) ;
        State.chunk { data = Value p ; region = r }, v
      | Many p ->
        check_access "store" p (Int iota) ;
        let rc = Chunk.{ data = Array p ; region = r } in
        State.chunk rc, e_set (State.value sigma rc) (M.pointer_val l) v

    let store_float sigma flt loc v : Sigma.chunk * term =
      let l,r = localized "store float" loc in
      match R.kind r with
      | Garbled -> L.store_float sigma flt l v
      | Single p ->
        check_access "store" p (Float flt) ;
        State.chunk { data = Value p ; region = r }, v
      | Many p ->
        check_access "store" p (Float flt) ;
        let rc = Chunk.{ data = Array p ; region = r } in
        State.chunk rc, e_set (State.value sigma rc) (M.pointer_val l) v

    let store_pointer sigma ty loc v : Sigma.chunk * term =
      let l,r = localized "store pointer" loc in
      match R.kind r with
      | Garbled -> L.store_pointer sigma ty l v
      | Single p ->
        check_access "store" p Ptr ;
        State.chunk { data = Value p ; region = r }, v
      | Many p ->
        check_access "store" p Ptr ;
        let rc = Chunk.{ data = Array p ; region = r } in
        State.chunk rc, e_set (State.value sigma rc) (M.pointer_val l) v

    (* ---------------------------------------------------------------------- *)
    (* --- Init                                                           --- *)
    (* ---------------------------------------------------------------------- *)

    let load_init_atom sigma obj loc : term =
      let l,r = localized "init atom" loc in
      match R.kind r with
      | Garbled -> L.load_init_atom sigma obj l
      | Single _-> State.value sigma { data = ValInit ; region = r }
      | Many _ ->
        e_get
          (State.value sigma { data = ArrInit ; region = r })
          (M.pointer_val l)

    let store_init_atom sigma obj loc v : Sigma.chunk * term =
      let l,r = localized "init atom" loc in
      match R.kind r with
      | Garbled -> L.store_init_atom sigma obj l v
      | Single _-> State.chunk { data = ValInit ; region = r }, v
      | Many _ ->
        let rc = Chunk.{ data = ArrInit ; region = r } in
        State.chunk rc, e_set (State.value sigma rc) (M.pointer_val l) v

    (* ---------------------------------------------------------------------- *)
    (* --- Footprints                                                     --- *)
    (* ---------------------------------------------------------------------- *)

    let lfootprint ~init obj l =
      if init
      then L.init_footprint obj l
      else L.value_footprint obj l

    let rec footprint ~init obj loc = match loc with
      | Null  -> lfootprint ~init obj M.null
      | Raw l -> lfootprint ~init obj l
      | Loc(l,r) ->
        match obj with
        | C_comp { cfields = None} -> Domain.empty
        | C_comp { cfields = Some fds } ->
          List.fold_left
            (fun dom fd ->
               let obj = Ctypes.object_of fd.ftype in
               match ofield loc fd with
               | None -> dom
               | Some loc -> Domain.union dom (footprint ~init obj loc)
            ) Domain.empty fds
        | C_array { arr_element = elt } ->
          let obj = object_of elt in
          footprint ~init obj (shift loc obj e_zero)
        | C_int _ | C_float _ | C_pointer _ ->
          match R.kind r with
          | Garbled -> lfootprint ~init obj l
          | Single p ->
            let data = Chunk.(if init then ValInit else Value p) in
            State.singleton { data ; region = r }
          | Many p ->
            let data = Chunk.(if init then ArrInit else Array p) in
            State.singleton { data ; region = r }

    let value_footprint = footprint ~init:false
    let init_footprint = footprint ~init:true

  end

  type loc = LOADER.loc
  type segment = loc rloc

  let pretty = LOADER.pretty

  include MemLoader.Make(LOADER)

  let lookup = M.lookup (*TODO: lookups in MemRegion *)

  let updates = M.updates (*TODO: updates in MemRegion *)

  (* {2 Memory Model API} *)

  let vars l = M.vars @@ LOADER.loc l
  let occurs x l = M.occurs x @@ LOADER.loc l
  let null = LOADER.Null

  let cvar v = LOADER.make (M.cvar v) (R.cvar v)
  let field = LOADER.field
  let shift = LOADER.shift

  let pointer_loc t = LOADER.Raw (M.pointer_loc t)
  let pointer_val l = M.pointer_val @@ LOADER.loc l
  let base_addr l = LOADER.Raw (M.base_addr @@ LOADER.loc l)
  let base_offset l = M.base_offset @@ LOADER.loc l
  let block_length sigma obj l = M.block_length sigma obj @@ LOADER.loc l
  let is_null = function LOADER.Null -> p_true | Raw l | Loc(l,_) -> M.is_null l
  let loc_of_int obj t = LOADER.Raw (M.loc_of_int obj t)
  let int_of_loc iota l = M.int_of_loc iota @@ LOADER.loc l

  let cast conv l =
    let l0 = LOADER.loc l in
    let r0 = LOADER.reg l in
    LOADER.make (M.cast conv l0) r0

  let loc_eq  a b = M.loc_eq  (LOADER.loc a) (LOADER.loc b)
  let loc_lt  a b = M.loc_lt  (LOADER.loc a) (LOADER.loc b)
  let loc_neq a b = M.loc_neq (LOADER.loc a) (LOADER.loc b)
  let loc_leq a b = M.loc_leq (LOADER.loc a) (LOADER.loc b)
  let loc_diff obj a b = M.loc_diff obj (LOADER.loc a) (LOADER.loc b)

  let rloc = function
    | Rloc(obj, l) -> Rloc (obj, LOADER.loc l)
    | Rrange(l, obj, inf, sup) -> Rrange(LOADER.loc l, obj, inf, sup)

  let rloc_region = function Rloc(_,l) | Rrange(l,_,_,_) -> LOADER.reg l

  let valid sigma acs r = M.valid sigma acs @@ rloc r
  let invalid sigma r = M.invalid sigma (rloc r)

  let included (a : segment) (b : segment) =
    match rloc_region a, rloc_region b with
    | Some ra, Some rb when R.separated ra rb -> p_false
    | _ -> M.included (rloc a) (rloc b)

  let separated (a : segment) (b : segment) =
    match rloc_region a, rloc_region b with
    | Some ra, Some rb when R.separated ra rb -> p_true
    | _ -> M.separated (rloc a) (rloc b)

  let alloc = M.alloc
  let scope = M.scope
  let global = M.global

  let frame sigma =
    let pool = ref @@ M.frame sigma in
    let assume p = pool := p :: !pool in
    Sigma.iter
      (fun c m ->
         match Sigma.ckind c with
         | State.Mu { data } ->
           begin
             match data with
             | Value Ptr -> assume @@ global sigma (e_var m)
             | Array Ptr -> assume @@ MemMemory.framed (e_var m)
             | ValInit | ArrInit | Value _ | Array _ -> ()
           end
         | _ -> ()
      ) sigma ;
    !pool

  let is_well_formed sigma =
    let pool = ref @@ [M.is_well_formed sigma] in
    let assume p = pool := p :: !pool in
    Sigma.iter
      (fun c m ->
         match Sigma.ckind c with
         | State.Mu { data } ->
           begin
             match data with
             | ValInit | ArrInit -> ()
             | Value (Int iota) -> assume @@ Cint.range iota (e_var m)
             | Array (Int iota) ->
               let a = Lang.freshvar ~basename:"p" @@ Lang.t_addr () in
               let b = e_get (e_var m) (e_var a) in
               assume @@ p_forall [a] (Cint.range iota b)
             | Value (Float _ | Ptr) | Array (Float _ | Ptr) -> ()
           end
         | _ -> ()
      ) sigma ;
    p_conj !pool

end
