(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Logic Definitions                                                  --- *)
(* -------------------------------------------------------------------------- *)

open LogicUsage
open Cil_types
open Cil_datatype
open Qed.Logic
open Lang
open F

type trigger = (var,lfun) Qed.Engine.ftrigger
type typedef = (tau,field,lfun) Qed.Engine.ftypedef

let rec rev_iter f = function
  | [] -> ()
  | x::w -> rev_iter f w ; f x

type cluster = {
  c_id : string ;
  c_title : string ;
  c_position : Filepos.t option ;
  mutable c_age : int ;
  mutable c_records : compinfo list ;
  mutable c_irecords : compinfo list ;
  mutable c_types : logic_type_info list ;
  mutable c_symbols : dfun list ;
  mutable c_lemmas : dlemma list ;
}

and dlemma = {
  l_name  : string ;
  l_cluster : cluster ;
  l_kind : predicate_kind ;
  l_forall : var list ;
  l_triggers : trigger list list (* OR of AND triggers *) ;
  l_lemma : pred ;
}

and dfun = {
  d_lfun   : lfun ;
  d_cluster : cluster ;
  d_types  : int ;
  d_params : var list ;
  d_definition : definition ;
}

and definition =
  | Logic of tau (* return type of an abstract function *)
  | Function of tau * recursion * term
  | Predicate of recursion * pred
  | Inductive of dlemma list

and recursion = Def | Rec

module Trigger =
struct

  open Qed.Engine

  let rec of_exp mode t =
    match repr t with
    | Fvar x -> TgVar x
    | Aget(a,k) -> TgGet(of_exp Cterm a,of_exp Cterm k)
    | Aset(a,k,v) -> TgSet(of_exp Cterm a,of_exp Cterm k,of_exp Cterm v)
    | Fun(f,ts) ->
      let ts = List.map (of_exp Cterm) ts in
      begin
        match mode with
        | Cterm -> TgFun(f,ts)
        | Cprop -> TgProp(f,ts)
      end
    | _ -> TgAny

  let of_term t = of_exp Cterm t
  let of_pred p = of_exp Cprop (e_prop p)

  let rec collect xs = function
    | TgAny -> xs
    | TgVar x -> Vars.add x xs
    | TgGet(a,k) -> collect (collect xs a) k
    | TgSet(a,k,v) -> collect (collect (collect xs a) k) v
    | TgFun(_,ts) | TgProp(_,ts) -> List.fold_left collect xs ts

  let vars = collect Vars.empty

end

(* -------------------------------------------------------------------------- *)
(* --- Printers                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec pp_trigger fmt (t : trigger) =
  match t with
  | TgAny -> Format.pp_print_string fmt "_"
  | TgVar a -> pp_var fmt a
  | TgGet(a, k) ->
    Format.fprintf fmt "%a[%a]" pp_trigger a pp_trigger k
  | TgSet(a, k, v) ->
    Format.fprintf fmt "@[<hov 2>%a[%a@ <- %a]@]" pp_trigger a pp_trigger k pp_trigger v
  | TgFun(f, vs) | TgProp(f, vs) ->
    Format.fprintf fmt "@[<hov 2>(%a" Fun.pretty f ;
    List.iter (Format.fprintf fmt "@ %a" pp_trigger) vs ;
    Format.fprintf fmt ")@]"

let pp_lemma fmt (l: dlemma) =
  let kind =
    match l.l_kind with
    | Admit -> "Axiom"
    | Assert -> "Lemma"
    | Check  -> "Goal"
  in
  Format.fprintf fmt "@[<hv 2>%s %s:" kind l.l_name ;
  List.iter (fun v ->
      Format.fprintf fmt "@ forall %a: %a."
        pp_var v
        pp_tau (tau_of_var v)
    ) l.l_forall ;
  List.iter (fun ts ->
      if ts <> [] then
        begin
          Format.fprintf fmt "@ @[<hov 2>[" ;
          List.iter (Format.fprintf fmt "@ %a" pp_trigger) ts ;
          Format.fprintf fmt " ]@]" ;
        end
    ) l.l_triggers ;
  Format.fprintf fmt "@ %a@]" pp_pred l.l_lemma

let pp_args fmt vs =
  List.iter
    (fun v ->
       Format.fprintf fmt "@ @[<hov 2>(%a@ : %a)@]"
         pp_var v pp_tau (tau_of_var v)
    ) vs

let pp_dfun fmt d =
  match d.d_definition with
  | Logic Qed.Logic.Prop ->
    Format.fprintf fmt "@[<hov 2>Predicate %a%a@]"
      Fun.pretty d.d_lfun pp_args d.d_params
  | Logic t ->
    Format.fprintf fmt "@[<hov 2>Function %a%a : %a@]"
      Fun.pretty d.d_lfun pp_args d.d_params pp_tau t
  | Function (t, _, e) ->
    Format.fprintf fmt "@[<hov 2>Function %a%a : %a@ = %a@]"
      Fun.pretty d.d_lfun pp_args d.d_params pp_tau t pp_term e
  | Predicate (_, p) ->
    Format.fprintf fmt "@[<hov 2>Predicate %a%a@ = %a@]"
      Fun.pretty d.d_lfun pp_args d.d_params pp_pred p
  | Inductive dl ->
    Format.fprintf fmt "@[<hv 2>Inductive %a%a ="
      Fun.pretty d.d_lfun pp_args d.d_params ;
    List.iter (Format.fprintf fmt "@ %a" pp_lemma) dl ;
    Format.fprintf fmt "@]"

let pp_record fmt (c : compinfo) =
  Format.fprintf fmt "@[<hov 0>@[<hov 2>Type %s = {" (comp_id c) ;
  Option.iter (List.iter (fun fd ->
      Format.fprintf fmt "@ %s : %a;" (field_id fd) pp_tau (tau_of_ctype fd.ftype)
    )) c.cfields ;
  Format.fprintf fmt "@]@ }@]"

let pp_irecord fmt (c : compinfo) =
  Format.fprintf fmt "@[<hov 0>@[<hov 2>Type %s = {" (comp_init_id c) ;
  Option.iter (List.iter (fun fd ->
      Format.fprintf fmt "@ %s : %a;" (field_init_id fd) pp_tau (init_of_ctype fd.ftype)
    )) c.cfields ;
  Format.fprintf fmt "@]@ }@]"

let pp_poly fmt ts = List.iter (Format.fprintf fmt " %s") ts

let pp_typedef fmt (t : logic_type_info) =
  match t.lt_def with
  | None ->
    Format.fprintf fmt "Type %s%a" t.lt_name pp_poly t.lt_params
  | Some (LTsyn lt) ->
    Format.fprintf fmt "@[<hov 2>Type %s%a =@ %a@]"
      t.lt_name pp_poly t.lt_params pp_tau (tau_of_ltype lt)
  | Some (LTsum cs) ->
    Format.fprintf fmt "@[<hv 2>Type %s%a =" t.lt_name pp_poly t.lt_params ;
    List.iter
      (fun c ->
         Format.fprintf fmt "@ | @[<hov 2>%s" c.ctor_name ;
         List.iter
           (fun p -> Format.fprintf fmt "@ %a" pp_tau (tau_of_ltype p))
           c.ctor_params ;
         Format.fprintf fmt "@]"
      ) cs ;
    Format.fprintf fmt "@]"

let is_empty c =
  c.c_records = [] &&
  c.c_irecords = [] &&
  c.c_types = [] &&
  c.c_symbols = [] &&
  c.c_lemmas = []

let dump fmt (c : cluster) =
  Format.fprintf fmt "@[<hv 0>@[<hv 2>Cluster %s {@," c.c_id ;
  List.iter (Format.fprintf fmt "@ %a@," pp_record) c.c_records ;
  List.iter (Format.fprintf fmt "@ %a@," pp_irecord) c.c_irecords ;
  List.iter (Format.fprintf fmt "@ %a@," pp_typedef) c.c_types ;
  List.iter (Format.fprintf fmt "@ %a@," pp_dfun) c.c_symbols ;
  List.iter (Format.fprintf fmt "@ %a@," pp_lemma) c.c_lemmas ;
  Format.fprintf fmt "@]@ }@]"

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

module Cluster = WpContext.Index
    (struct
      type key = string
      type data = cluster
      let name = "Definitions.Cluster"
      let compare = String.compare
      let pretty = Format.pp_print_string
    end)

module Symbol = WpContext.Index
    (struct
      type key = lfun
      type data = dfun
      let name = "Definitions.Symbol"
      let compare = Fun.compare
      let pretty = Fun.pretty
    end)

module Lemma = WpContext.Index
    (struct
      type key = string
      type data = dlemma
      let name = "Definitions.Lemma"
      let compare = String.compare
      let pretty = Format.pp_print_string
    end)

let touch c = c.c_age <- succ c.c_age

let () =
  begin
    Symbol.callback
      (fun _ f ->
         touch f.d_cluster ;
         f.d_cluster.c_symbols <- f :: f.d_cluster.c_symbols) ;
    Lemma.callback
      (fun _ a ->
         touch a.l_cluster ;
         a.l_cluster.c_lemmas <- a :: a.l_cluster.c_lemmas) ;
  end

let find_symbol = Symbol.find
let define_symbol f = Symbol.define f.d_lfun f
let update_symbol f = Symbol.update f.d_lfun f

let find_name = Lemma.find
let find_lemma l = Lemma.find l.lem_name
let compile_lemma cc l = Lemma.compile (fun _name -> cc l) l.lem_name
let define_lemma l = Lemma.define l.l_name l

let define_type c t =
  begin
    touch c ;
    c.c_types <- t :: c.c_types ;
  end

let parameters f =
  if WpContext.is_defined () then
    try List.map QED.sort_of_var (Symbol.find f).d_params
    with Not_found -> []
  else []

let () = Lang.parameters parameters

(* -------------------------------------------------------------------------- *)
(* --- Helpers                                                            --- *)
(* -------------------------------------------------------------------------- *)

let cluster_id c = c.c_id
let cluster_title c = c.c_title
let cluster_position c = c.c_position
let cluster_age c = c.c_age
let cluster_compare a b = String.compare a.c_id b.c_id
let pp_cluster fmt c = Format.pp_print_string fmt c.c_id
let iter f = Cluster.iter_sorted (fun _key c -> f c)

let newcluster ~id ?title ?position () =
  {
    c_id = id ;
    c_title = (match title with Some t -> t | None -> id) ;
    c_position = position ;
    c_age = 0 ;
    c_types = [] ;
    c_records = [] ;
    c_irecords = [] ;
    c_symbols = [] ;
    c_lemmas = [] ;
  }

let cluster ~id ?title ?position () =
  Cluster.memoize (fun id -> newcluster ~id ?title ?position ()) id

let dummy () = cluster ~id:"dummy" ()

let axiomatic ax =
  Cluster.memoize
    (fun id ->
       let title = Printf.sprintf "Axiomatic '%s'" ax.ax_name in
       let position = ax.ax_position in
       let cluster = newcluster ~id ~title ~position () in
       cluster)
    (Printf.sprintf "A_%s" ax.ax_name)

let section = function
  | Toplevel 0 -> cluster ~id:"Axiomatic" ~title:"Global Definitions" ()
  | Toplevel n ->
    let id = "Axiomatic" ^ string_of_int n in
    let title = Printf.sprintf "Global Definitions (continued #%d)" n in
    cluster ~id ~title ()
  | Axiomatic ax -> axiomatic ax

let compinfo c =
  Cluster.memoize
    (fun id ->
       let title =
         if c.cstruct
         then Printf.sprintf "Struct '%s'" c.cname
         else Printf.sprintf "Union '%s'" c.cname in
       let cluster = newcluster ~id ~title ()
       in cluster.c_records <- [c] ; cluster)
    (comp_id c)

let icompinfo c =
  Cluster.memoize
    (fun id ->
       let title =
         if c.cstruct
         then Printf.sprintf "Init Struct '%s'" c.cname
         else Printf.sprintf "Init Union '%s'" c.cname in
       let cluster = newcluster ~id ~title ()
       in cluster.c_irecords <- [c] ; cluster)
    (comp_init_id c)

let matrix () = cluster ~id:"Matrix" ~title:"Basic Arrays" ()

let call_fun ~result lfun cc es =
  Symbol.compile (local cc) lfun ;
  e_fun ~result lfun es

let call_pred lfun cc es =
  Symbol.compile (local cc) lfun ;
  p_call lfun es

(* -------------------------------------------------------------------------- *)
(* --- Cluster Dependencies                                               --- *)
(* -------------------------------------------------------------------------- *)

module DT = Logic_type_info.Set
module DR = Compinfo.Set
module DS = Datatype.String.Set
module DF = Set.Make(Fun)
module DW = Set.Make
    (struct
      type t = string list * string
      let compare = Stdlib.compare
    end)
module DC = Set.Make
    (struct
      type t = cluster
      let compare = cluster_compare
    end)

(* -------------------------------------------------------------------------- *)
(* --- Markers (test and set)                                             --- *)
(* -------------------------------------------------------------------------- *)

type axioms = cluster * logic_lemma list

class virtual visitor main =
  object(self)

    val mutable terms    = Tset.empty
    val mutable types    = DT.empty
    val mutable comps    = DR.empty
    val mutable icomps   = DR.empty
    val mutable symbols  = DF.empty
    val mutable dlemmas  = DS.empty
    val mutable lemmas   = DS.empty
    val mutable clusters = DC.empty
    val mutable libraries = DS.empty
    val mutable theories = DW.empty
    val mutable locals = DC.add main DC.empty

    method set_local c = locals <- DC.add c locals

    method do_local c =
      if DC.mem c locals then true else
        (self#vcluster c ; false)

    method private vtau_of_ltype lt =
      let tau = tau_of_ltype lt in
      self#vtau tau ; tau

    method vtype t =
      if not (DT.mem t types) then
        begin
          types <- DT.add t types ;
          let cluster = section (LogicUsage.section_of_type t) in
          if self#do_local cluster && not (is_builtin t) then
            begin
              let def = match t.lt_def with
                | None -> Qed.Engine.Tabs
                | Some (LTsyn lt) -> Qed.Engine.Tdef (self#vtau_of_ltype lt)
                | Some (LTsum cs) ->
                  let cases = List.map
                      (fun c ->
                         ctor c ,
                         List.map self#vtau_of_ltype c.ctor_params
                      ) cs in
                  Qed.Engine.Tsum cases
              in self#on_type t def ;
            end
        end

    method vcomp r =
      if not (DR.mem r comps) then
        begin
          comps <- DR.add r comps ;
          let c = compinfo r in
          if self#do_local c then
            begin
              let fts = Option.map
                  (List.map
                     (fun f ->
                        let t = tau_of_ctype f.ftype in
                        self#vtau t ; cfield f , t
                     ))
                  r.cfields
              in self#on_comp r fts ;
            end
        end

    method vicomp r =
      if not (DR.mem r icomps) then
        begin
          icomps <- DR.add r icomps ;
          let c = icompinfo r in
          if self#do_local c then
            begin
              let fts = Option.map
                  (List.map
                     (fun f ->
                        let t = init_of_ctype f.ftype in
                        self#vtau t ; cfield ~kind:KInit f , t
                     ))
                  r.cfields
              in self#on_icomp r fts ;
            end
        end

    method vfield = function
      | Mfield(a,_,_,_) -> self#vlibrary a.ext_library
      | Cfield(f, KValue) -> self#vcomp f.fcomp
      | Cfield(f, KInit) -> self#vicomp f.fcomp

    method vadt = function
      | Mtype a | Mrecord(a,_) -> self#vlibrary a.ext_library
      | Comp(r, KValue) -> self#vcomp r
      | Comp(r, KInit) -> self#vicomp r
      | Atype t -> self#vtype t
      | Wtype(p,m,_) -> self#vtheory p m

    method vtau = function
      | Prop | Bool | Int | Real | Tvar _ -> ()
      | Array(a,b) -> self#vtau a ; self#vtau b
      | Record _ -> assert false
      | Data(a,ts) -> self#vadt a ; List.iter self#vtau ts

    method vparam x = self#vtau (tau_of_var x)

    method private repr ~bool t =
      begin
        try self#vtau (typeof t);
        with Not_found ->
          Wp_parameters.debug ~level:2 "@[<hov 2>Untyped term: %a@]" pp_term t ;
      end ;
      match repr t with
      | Fun(f,_) -> self#vsymbol f
      | Rget(_,f) -> self#vfield f
      | Rdef fts -> List.iter (fun (f,_) -> self#vfield f) fts
      | Fvar x -> self#vparam x
      | Bind(_,qt,_) -> self#vtau qt
      | True | False | Kint _ | Kreal _ | Bvar _
      | Times _ | Add _ | Mul _ | Div _ | Mod _
      | Aget _ | Aset _ | Apply _ -> ()
      | Acst _ -> self#on_library "const"
      | Eq _ | Neq _ | Leq _ | Lt _
      | And _ | Or _ | Not _ | Imply _ | If _ ->
        if bool then self#on_library "bool"

    method vterm t =
      if not (Tset.mem t terms) then
        begin
          terms <- Tset.add t terms ;
          self#repr ~bool:true t ;
          lc_iter self#vterm t ;
        end

    method vpred p =
      let t = e_prop p in
      if not (Tset.mem t terms) then
        begin
          self#repr ~bool:false t ;
          lc_iter
            (fun e ->
               if is_prop e
               then self#vpred (p_bool e)
               else self#vterm e) t
        end

    method private vdefinition = function
      | Logic t -> self#vtau t
      | Function(t,_,e) -> self#vtau t ; self#vterm e
      | Predicate(_,p) -> self#vpred p
      | Inductive _ -> ()

    method private vproperties = function
      | Logic _ | Function _ | Predicate _ -> ()
      | Inductive cases -> List.iter (fun l -> self#vdlemma l) cases

    method private vdfun d =
      let old_terms = terms in
      terms <- Tset.empty ;
      begin
        try
          List.iter self#vparam d.d_params ;
          self#vdefinition d.d_definition ;
          self#vproperties d.d_definition ;
          self#on_dfun d ;
        with e ->
          terms <- old_terms ;
          raise e
      end ;
      terms <- old_terms

    method private vlfun f =
      match Symbol.find f with
      | exception Not_found ->
        Wp_parameters.fatal "Undefined symbol '%a'" Fun.pretty f
      | d ->
        let c = d.d_cluster in
        if self#do_local c then self#vdfun d

    method vsymbol f =
      if not (DF.mem f symbols) then
        begin
          symbols <- DF.add f symbols ;
          match f with
          | FUN { m_source = Wsymbol(p,m,_) } -> self#vtheory p m
          | FUN { m_source = Extern e  } -> self#vlibrary e.ext_library
          | FUN { m_source = Generated _ } | ACSL _ -> self#vlfun f
          | CTOR c -> self#vadt (adt c.ctor_type)
        end

    method private vtrigger = function
      | Qed.Engine.TgAny -> ()
      | Qed.Engine.TgVar x -> self#vparam x
      | Qed.Engine.TgGet(a,k) ->
        begin
          self#vtrigger a ;
          self#vtrigger k ;
        end
      | Qed.Engine.TgSet(a,k,v) ->
        begin
          self#vtrigger a ;
          self#vtrigger k ;
          self#vtrigger v ;
        end
      | Qed.Engine.TgFun(f,tgs)
      | Qed.Engine.TgProp(f,tgs) ->
        self#vsymbol f ; List.iter self#vtrigger tgs

    method private vdlemma a =
      if not (DS.mem a.l_name dlemmas) then
        begin
          dlemmas <- DS.add a.l_name dlemmas ;
          List.iter self#vparam a.l_forall ;
          List.iter (List.iter self#vtrigger) a.l_triggers ;
          self#vpred a.l_lemma ;
        end

    method vlemma lem =
      let l = lem.lem_name in
      if not (DS.mem l lemmas) then
        begin
          lemmas <- DS.add l lemmas ;
          try
            let a = Lemma.find l in
            if self#do_local a.l_cluster then (self#vdlemma a; self#on_dlemma a)
          with Not_found ->
            Wp_parameters.fatal "Lemma '%s' undefined" l
        end

    method vcluster c =
      if not (DC.mem c clusters) then
        begin
          clusters <- DC.add c clusters ;
          self#on_cluster c ;
        end

    method vlibrary lib =
      if not (DS.mem lib libraries) then
        begin
          libraries <- DS.add lib libraries ;
          try
            let deps = LogicBuiltins.dependencies lib in
            List.iter self#vlibrary deps ;
            self#on_library lib ;
          with Not_found ->
            Wp_parameters.fatal
              ~current:false "Unknown library '%s'" lib
        end

    method vtheory p m =
      if not (DW.mem (p,m) theories) then
        begin
          theories <- DW.add (p,m) theories ;
          self#on_theory p m
        end

    method vgoal (axioms : axioms option) prop =
      match axioms with
      | None ->
        (* Visit a goal *)
        begin
          let hs = LogicUsage.proof_context () in
          List.iter self#vlemma hs ;
          self#vpred prop ;
        end
      | Some(cluster,hs) ->
        (* Visit the goal corresponding to a lemma *)
        begin
          self#section (cluster_title cluster) ;
          self#set_local cluster ;
          List.iter self#vlemma hs ;
          self#vpred prop ;
        end

    method vtypes = (* Visit the types *)
      rev_iter self#vcomp main.c_records ;
      rev_iter self#vicomp main.c_irecords ;
      rev_iter self#vtype main.c_types

    method vsymbols = (* Visit the definitions *)
      rev_iter (fun d -> self#vsymbol d.d_lfun) main.c_symbols ;

    method vlemmas = (* Visit the lemmas *)
      rev_iter (fun l -> self#vdlemma l; self#on_dlemma l) main.c_lemmas ;

    method vself = (* Visit a cluster *)
      begin
        self#vtypes ;
        self#vsymbols ;
        self#vlemmas ;
      end

    method virtual section : string -> unit
    method virtual on_theory : string list -> string -> unit
    method virtual on_library : string -> unit
    method virtual on_cluster : cluster -> unit
    method virtual on_type : logic_type_info -> typedef -> unit
    method virtual on_comp : compinfo -> (field * tau) list option -> unit
    method virtual on_icomp : compinfo -> (field * tau) list option -> unit
    method virtual on_dlemma : dlemma -> unit
    method virtual on_dfun : dfun -> unit

  end

(* -------------------------------------------------------------------------- *)
