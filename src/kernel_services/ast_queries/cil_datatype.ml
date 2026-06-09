(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type S_with_collections = sig
  include Datatype.S_with_collections
  val dummy: t
end

module type S = sig
  include Datatype.S
  val dummy: t
end

module type S_with_pretty = sig
  include S
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

module type S_with_collections_pretty = sig
  include S_with_collections
  val pretty_ref: (Format.formatter -> t -> unit) ref
end

open Cil_types
let (=?=) = Extlib.compare_basic
let compare_list = List.compare

let empty_list_hash = 1
let hash_list f =
  List.fold_left (fun acc d -> 65537 * acc + f d) empty_list_hash

(* Functions that will clear internal, non-project compliant, caches *)
let clear_caches = ref []

(**************************************************************************)
(** {3 Generic builders for Cil datatypes} *)
(**************************************************************************)

module Make
    (X: sig
       type t
       val name: string
       val reprs: t list
       val pretty: Format.formatter -> t -> unit
     end) =
  Datatype.Make
    (struct
      include Datatype.Undefined
      include X
      let name = "Cil_datatype." ^ name
      let structural_descr = Structural_descr.t_abstract
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
    end)

module Make_with_collections
    (X: sig
       type t
       val name: string
       val reprs: t list
       val compare: t -> t -> int
       val equal: t -> t -> bool
       val pretty: Format.formatter -> t -> unit
       val hash: t -> int
       val copy: t -> t
     end) =
  Datatype.Make_with_collections
    (struct
      include X
      let name = "Cil_datatype." ^ name
      let structural_descr = Structural_descr.t_abstract
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
    end)

let compare_chain cmp x1 x2 next arg1 arg2 =
  let res = cmp x1 x2 in if res = 0 then next arg1 arg2 else res

let rank_term = function
  | TConst _ -> 0
  | TLval _ -> 1
  | TSizeOf _ -> 2
  | TSizeOfE _ -> 3
  | TAlignOf _ -> 5
  | TAlignOfE _ -> 6
  | TUnOp _ -> 7
  | TBinOp _ -> 8
  | TCast (false,_,_) -> 9
  | TAddrOf _ -> 10
  | TStartOf _ -> 11
  | Tapp _ -> 12
  | Tlambda _ -> 13
  | TDataCons _ -> 14
  | Tif _ -> 15
  | Tat _ -> 16
  | Tbase_addr _ -> 17
  | Tblock_length _ -> 18
  | Tnull -> 19
  | TUpdate _ -> 22
  | Ttypeof _ -> 23
  | Ttype _ -> 24
  | Tempty_set -> 25
  | Tunion _ -> 26
  | Tinter _ -> 27
  | Trange _ -> 28
  | Tlet _ -> 29
  | Tcomprehension _ -> 30
  | Toffset _ -> 31
  | TCast (true, _,_) -> 32

let rank_predicate = function
  | Pfalse -> 0
  | Ptrue -> 1
  | Papp _ -> 2
  | Pseparated _ -> 3
  | Prel _ -> 4
  | Pand _ -> 5
  | Por _ -> 6
  | Pxor _ -> 7
  | Pimplies _ -> 8
  | Piff _ -> 9
  | Pnot _ -> 10
  | Pif _ -> 11
  | Plet _ -> 12
  | Pforall _ -> 13
  | Pexists _ -> 14
  | Pat _ -> 15
  | Pobject_pointer _ -> 16
  | Pvalid_read _ -> 17
  | Pvalid _ -> 18
  | Pvalid_function _ -> 19
  | Pinitialized _ -> 20
  | Pdangling _ -> 21
  | Pallocable _ -> 22
  | Pfreeable _ -> 23
  | Pfresh _ -> 24
  | Paligned _ -> 25


(**************************************************************************)
(** {3 Cabs types} *)
(**************************************************************************)

module Cabs_file = struct
  let pretty_ref = ref (fun _ _ -> assert false)

  let dummy = Filepath.dummy, []

  include Make
      (struct
        type t = Cabs.file
        let name = "Cabs_file"
        let reprs =
          let loc = Filepos.(unknown, unknown) in
          let dummy_def =
            Cabs.DECDEF (None, ([SpecType Tint], [("dummy", JUSTBASE, [], loc),
                                                  NO_INIT]), loc)
          in
          List.concat_map
            (fun p -> [(p, []); (p, [true, dummy_def])])
            Filepath.reprs
        let pretty fmt cabs = !pretty_ref fmt cabs
      end)
end

(**************************************************************************)
(** {3 C types} *)
(**************************************************************************)

module Position =  struct
  include Filepos
  let dummy = unknown
  let pp_with_col = Filepos.pretty_long
  let of_lexing_pos pos = of_lexing_pos pos (* Erase the optional parameter *)
end

module Location = struct
  include Fileloc
  let dummy = unknown
  let is_unknown loc = not (is_known loc)
  let compare_start_semantic = compare
  let equal_start_semantic = equal
  let pretty_line fmt loc = Filepos.pretty_long fmt (fst loc)
end

module File = struct

  let dummy = {
    fileName = Filepath.dummy;
    globals  = [];
    globinit = None;
    globinitcalled = false
  }

  include Make
      (struct
        type t = file
        let name = "File"
        let reprs =
          List.map (fun p ->
              { fileName = p;
                globals = [];
                globinit = None;
                globinitcalled = false })
            Filepath.reprs
        include Datatype.Undefined
      end)
end

module Instr = struct

  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = Asm([], ["@dummy_instr@"], None, Fileloc.unknown)

  include Make
      (struct
        type t = instr
        let name = "Instr"
        let reprs = List.map (fun l -> Skip l) Fileloc.reprs
        let pretty fmt x = !pretty_ref fmt x
      end)

  let loc = function
    | Local_init (_,_,l) -> l
    | Skip l
    | Set (_,_,l)
    | Call (_,_,_,l)
    | Asm (_,_,_,l)
    | Code_annot (_,l) -> l

end

module Stmt_Id = struct
  let pretty_ref = ref (fun _ _ -> assert false)

  let dummy = {
    labels = [];
    skind  = Instr Instr.dummy;
    sid    = -1;
    succs  = [];
    preds  = [];
    ghost  = false;
    sattr  = []
  }

  include Make_with_collections
      (struct
        type t = stmt
        let name = "Stmt"
        let reprs = [ dummy ]
        let compare t1 t2 = Datatype.Int.compare t1.sid t2.sid
        let hash t1 = t1.sid
        let equal t1 t2 = t1.sid = t2.sid
        let copy = Datatype.undefined
        let pretty fmt s = !pretty_ref fmt s
      end)
  let id stmt = stmt.sid
end

module Hptset_Info = struct
  let initial_values = []
  let dependencies = [] (* This should be [Ast.self], but cannot be done here *)
end

module Stmt = struct
  include Stmt_Id

  let pretty_sid fmt s = Format.pp_print_int fmt s.sid

  module Hptset = Hptset.Make (Stmt_Id) (Hptset_Info)
  let () = clear_caches := Hptset.clear_caches :: !clear_caches

  let rec loc_skind = function
    | Return(_, l) | Goto(_, l) | Break(l) | Continue l | If(_, _, _, l)
    | Switch (_, _, _, l) | Loop (_, _, l, _, _)
    | TryFinally (_, _, l) | TryExcept (_, _, _, l)
    | Throw (_,l) | TryCatch(_,_,l) -> l
    | Instr hd -> Instr.loc hd
    | Block b -> (match b.bstmts with [] -> Fileloc.unknown | s :: _ -> loc s)
    | UnspecifiedSequence ((s,_,_,_,_) :: _) -> loc s
    | UnspecifiedSequence [] -> Fileloc.unknown

  and loc s = loc_skind s.skind

end

module Kinstr = struct

  let dummy = Kstmt Stmt.dummy

  include Make_with_collections
      (struct
        type t = kinstr
        let name = "Kinstr"
        let reprs = Kglobal :: List.map (fun s -> Kstmt s) Stmt.reprs
        let compare i1 i2 = match i1, i2 with
          | Kglobal, Kglobal -> 0
          | Kglobal, _ -> 1
          | _, Kglobal -> -1
          | Kstmt s1, Kstmt s2 -> Stmt.compare s1 s2
        let equal t1 t2 = compare t1 t2 = 0
        let hash = function
          | Kglobal -> 1 lsl 29
          | Kstmt s -> s.sid
        let copy = Datatype.undefined
        let pretty fmt = function
          | Kglobal ->
            Format.fprintf fmt "Kglobal"
          | Kstmt s -> Stmt.pretty fmt s
      end)

  let kinstr_of_opt_stmt = function
    | None -> Kglobal
    | Some s -> Kstmt s

end

let index_attrparam = function
  | AInt _ -> 0
  | AStr _ -> 1
  | ACons _ -> 2
  | ASizeOf _ -> 3
  | ASizeOfE _ -> 4
  | AAlignOf _ -> 6
  | AAlignOfE _ -> 7
  | AUnOp _ -> 9
  | ABinOp _ -> 10
  | ADot _ -> 11
  | AStar _ -> 12
  | AAddrOf _ -> 13
  | AIndex _ -> 14
  | AQuestion _ -> 15

let index_typ t =
  match t.tnode with
  | TVoid -> 0
  | TInt _ -> 1
  | TFloat _ -> 2
  | TPtr _ -> 3
  | TArray _ -> 4
  | TFun _ -> 5
  | TNamed _ -> 6
  | TComp _ -> 7
  | TEnum _ -> 8
  | TBuiltin_va_list -> 9

let constfoldtoint = Extlib.mk_fun "constfoldtoint"
let punrollType = Extlib.mk_fun "punrollType"
let punrollLogicType = Extlib.mk_fun "punrollLogicType"
let drop_non_logic_attributes = ref Fun.id
let drop_fc_internal_attributes = ref Fun.id
let drop_ignored_attributes = ref Fun.id
let compare_exp_struct_eq = Extlib.mk_fun "compare_exp_struct_eq"

type type_compare_config =
  { by_name : bool;
    logic_type: bool;
    unroll: bool;
    no_attrs:bool; }

let filter_attributes config attrs =
  let attrs' =
    if config.logic_type
    then !drop_non_logic_attributes attrs
    else !drop_fc_internal_attributes attrs
  in
  !drop_ignored_attributes attrs'

let rec compare_attribute config (s1, l1) (s2, l2) =
  compare_chain (=?=) s1 s2 (compare_attrparam_list config) l1 l2
and compare_attributes config  l1 l2 =
  if config.no_attrs then 0
  else
    let l1, l2 = filter_attributes config l1, filter_attributes config l2 in
    compare_list (compare_attribute config) l1 l2
and compare_attrparam_list config l1 l2 =
  compare_list (compare_attrparam config) l1 l2
and compare_attrparam config a1 a2 = match a1, a2 with
  | AInt i1, AInt i2 -> Z.compare i1 i2
  | AStr s1, AStr s2 -> s1 =?= s2
  | ACons ((s1: string), l1), ACons (s2, l2) ->
    let r1 = (=?=) s1 s2 in
    if r1 <> 0 then r1
    else
      compare_attrparam_list config l1 l2
  | ASizeOf t1, ASizeOf t2 -> compare_type config t1 t2
  | ASizeOfE p1, ASizeOfE p2 -> compare_attrparam config p1 p2
  | AAlignOf (t1, i1), AAlignOf (t2, i2) ->
    compare_chain
      (Cil_types.compare_standard_or_gcc) i1 i2
      (compare_type config) t1 t2
  | AAlignOfE (p1, i1), AAlignOfE (p2, i2) ->
    compare_chain
      (Cil_types.compare_standard_or_gcc) i1 i2
      (compare_attrparam config) p1 p2
  | AUnOp (op1, a1), AUnOp (op2, a2) ->
    compare_chain (=?=) op1 op2 (compare_attrparam config) a1 a2
  | ABinOp (op1, a1, a1'), ABinOp (op2, a2, a2') ->
    compare_chain (=?=) op1 op2
      (compare_chain
         (compare_attrparam config) a1 a2 (compare_attrparam config))
      a1' a2'
  | ADot (a1, s1), ADot (a2, s2) ->
    compare_chain (=?=) s1 s2 (compare_attrparam config) a1 a2
  | AStar a1, AStar a2
  | AAddrOf a1, AAddrOf a2 -> compare_attrparam config a1 a2
  | AIndex (a1, a1'), AIndex (a2, a2') ->
    compare_chain
      (compare_attrparam config) a1 a2
      (compare_attrparam config) a1' a2'
  | AQuestion (a1, a1', a1''), AQuestion (a2, a2', a2'') ->
    compare_chain
      (compare_attrparam config) a1 a2
      (compare_chain (compare_attrparam config) a1' a2'
         (compare_attrparam  config))
      a1'' a2''
  | (AInt _ | AStr _ | ACons _ | ASizeOf _ | ASizeOfE _ |
     AAlignOf _ | AAlignOfE _ | AUnOp _ | ABinOp _ | ADot _ |
     AStar _ | AAddrOf _ | AIndex _ | AQuestion _ as a1), a2 ->
    index_attrparam a1 - index_attrparam a2
and compare_array_sizes e1o e2o =
  let compare_non_empty_size e1 e2 =
    let i1 = !constfoldtoint e1 in
    let i2 = !constfoldtoint e2 in
    match i1, i2 with
    | None, None -> (* inconclusive. do not return 0 *)
      !compare_exp_struct_eq e1 e2
    | _ -> Option.compare Z.compare i1 i2
  in
  Option.compare compare_non_empty_size e1o e2o

and compare_type config t1 t2 =
  if t1 == t2 then 0
  else
    let t1, t2 =
      if config.unroll then !punrollType t1, !punrollType t2
      else t1,t2
    in
    let comp_tattr () = compare_attributes config t1.tattr t2.tattr in
    let compare_chain_tattr cmp x1 x2 =
      let res = cmp x1 x2 in if res = 0 then comp_tattr () else res
    in
    match t1.tnode, t2.tnode with
    | TVoid, TVoid -> comp_tattr ()
    | TInt i1, TInt i2 ->
      compare_chain_tattr (=?=) i1 i2
    | TFloat f1, TFloat f2 ->
      compare_chain_tattr (=?=) f1 f2
    | TPtr t1, TPtr t2 ->
      compare_chain_tattr (compare_type config) t1 t2
    | TArray (t1', e1), TArray (t2', e2) ->
      compare_chain compare_array_sizes e1 e2
        (compare_chain_tattr (compare_type config)) t1' t2'
    | TFun (r1, a1, v1), TFun (r2, a2, v2) ->
      compare_chain (compare_type config) r1 r2
        (compare_chain (=?=) v1 v2
           (compare_chain_tattr (compare_arg_list config))) a1 a2
    | TNamed t1, TNamed t2 ->
      assert (not config.unroll);
      compare_chain_tattr (=?=) t1.tname t2.tname
    | TComp c1, TComp c2 ->
      let res =
        if config.by_name
        then (=?=) c1.cname c2.cname
        else (=?=) c1.ckey c2.ckey
      in
      if res <> 0 then res
      else comp_tattr ()
    | TEnum e1, TEnum e2 ->
      compare_chain_tattr (=?=) e1.ename e2.ename
    | TBuiltin_va_list, TBuiltin_va_list -> comp_tattr ()
    | (TVoid | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _ | TNamed _ |
       TComp _ | TEnum _ | TBuiltin_va_list), _ ->
      index_typ t1 - index_typ t2

and compare_arg_list config l1 l2 =
  Option.compare
    (compare_list
       (fun (_n1, t1, l1) (_n2, t2, l2) ->
          compare_chain (compare_type config) t1 t2
            (compare_attributes config) l1 l2
       )) l1 l2

let hash_attribute _config (s, _) =
  (* We do not hash attrparams. There is a recursivity problem with typ, and the
     equal function will be complicated enough in itself *)
  3 * Hashtbl.hash s + 117
let hash_attributes config l =
  if config.no_attrs then empty_list_hash
  else
    let attrs = filter_attributes config l in
    hash_list (hash_attribute config) attrs

let rec hash_type config t =
  let t = if config.unroll then !punrollType t else t in
  let tattr_hash = hash_attributes config t.tattr in
  match t.tnode with
  | TVoid -> Hashtbl.hash (tattr_hash, 1)
  | TInt i -> Hashtbl.hash (i, 2, tattr_hash)
  | TFloat f -> Hashtbl.hash (f, 3, tattr_hash)
  | TPtr t ->
    Hashtbl.hash (hash_type config t, 4, tattr_hash)
  | TArray (t, _) ->
    Hashtbl.hash (hash_type config t, 5, tattr_hash)
  | TFun (r, a, v) ->
    Hashtbl.hash
      (hash_type config r, 6, hash_args config a, v, tattr_hash)
  | TNamed ti ->
    Hashtbl.hash (ti.tname, 7, tattr_hash)
  | TComp c ->
    Hashtbl.hash
      ((if config.by_name then Hashtbl.hash c.cname else c.ckey), 8,
       tattr_hash)
  | TEnum e ->
    Hashtbl.hash (e.ename, 9, tattr_hash)
  | TBuiltin_va_list -> Hashtbl.hash (tattr_hash, 10)
and hash_args config = function
  | None -> 11713
  | Some l ->
    hash_list
      (fun (_, t, l) ->
         Hashtbl.hash (17, hash_type config t, hash_attributes config l)) l

module Attribute=struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = ("@dummy_attribute@", [])

  include Make_with_collections
      (struct
        type t = attribute
        let config =
          { by_name = false; logic_type = false;
            unroll = true; no_attrs = false }
        let name = "Attribute"
        let reprs = [ dummy ]
        let compare = compare_attribute config
        let hash = hash_attribute config
        let equal = Datatype.from_compare
        let copy = Datatype.undefined
        let pretty fmt t = !pretty_ref fmt t
      end)
end

(* Shared between the different modules for types. *)
let pretty_typ_ref = ref (fun _ _ -> assert false)

module Attributes = struct
  include Datatype.List_with_collections(Attribute)
  let dummy = [ Attribute.dummy ]
end

module MakeTyp(M:sig val config: type_compare_config val name: string end) =
struct
  let pretty_ref = pretty_typ_ref
  let dummy = { tnode = TVoid; tattr = [] }

  include Make_with_collections
      (struct
        type t = typ
        let name = M.name
        let reprs = [ dummy ]
        let compare = compare_type M.config
        let hash = hash_type M.config
        let equal = Datatype.from_compare
        let copy = Datatype.undefined
        let pretty fmt t = !pretty_typ_ref fmt t
      end)
end

module Typ = struct
  include
    MakeTyp
      (struct
        let config =
          { by_name = false; logic_type = false;
            unroll = true; no_attrs = false}
        let name = "Typ"
      end)
end

module TypByName =
  MakeTyp
    (struct
      let config = { by_name = true; logic_type = false;
                     unroll = false; no_attrs = false }
      let name = "TypByName"
    end)

module TypNoUnroll =
  MakeTyp
    (struct
      let config = { by_name = false; logic_type = false;
                     unroll = false; no_attrs = false }
      let name = "TypNoUnroll"
    end)

module TypNoAttrs =
  MakeTyp
    (struct
      let config =
        { by_name = false; logic_type = false;
          unroll = true; no_attrs = true}
      let name = "TypNoAttrs"
    end)

module Typeinfo = struct

  let dummy = {
    torig_name  = "@dummy_typeinfo@";
    tname       = "@dummy_typeinfo@";
    ttype       = Typ.dummy;
    treferenced = false
  }

  include Make_with_collections
      (struct
        include Datatype.Undefined
        type t = typeinfo
        let name = "Type_info"
        let reprs =
          [ { torig_name = "";
              tname = "";
              ttype = List.hd Typ.reprs;
              treferenced = false } ]
        let compare v1 v2 = String.compare v1.tname v2.tname
        let hash v = Hashtbl.hash v.tname
        let equal v1 v2 = v1.tname = v2.tname
      end)
end

module Exp = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let zero = CInt64 (Z.zero, IChar, None)
  let dummy = { eid = -1; enode = Const zero; eloc = Fileloc.unknown }
  include Make_with_collections
      (struct
        include Datatype.Undefined
        type t = exp
        let name = "Exp"
        let reprs = [ dummy ]
        let compare e1 e2 = Datatype.Int.compare e1.eid e2.eid
        let hash e = Hashtbl.hash e.eid
        let equal e1 e2 = e1.eid = e2.eid
        let pretty fmt t = !pretty_ref fmt t
      end)
end

module Label = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = Label ("@dummy_label@", Fileloc.unknown, false)

  include Make_with_collections
      (struct
        type t = label
        let name = "Label"
        let reprs = [ dummy; Default Fileloc.unknown ]
        let pretty fmt l = !pretty_ref fmt l
        let hash = function
          | Default _ -> 7
          | Case (e, _) -> Exp.hash e
          | Label (s, _, b) -> Hashtbl.hash s + (if b then 13 else 59)
        let compare l1 l2 = match l1, l2 with
          | Default loc1, Default loc2 -> Fileloc.compare loc1 loc2
          | Case (e1, loc1), Case (e2, loc2) ->
            let c = Exp.compare e1 e2 in
            if c = 0 then Fileloc.compare loc1 loc2
            else c
          | Label (s1, loc1, b1), Label (s2, loc2, b2) ->
            let c = s1 =?= s2 in
            if c = 0 then
              let c = b1 =?= b2 in
              if c = 0 then Fileloc.compare loc1 loc2
              else c
            else c
          | Label _, (Case _ | Default _)
          | Case _, Default _ -> -1
          | Case _, Label _
          | Default _, (Label _ | Case _) -> 1
        let equal = Datatype.from_compare
        let copy = Datatype.undefined
      end)
end

module Varinfo_Id = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    vname = "@dummy_varinfo@";
    vorig_name = "@dummy_varinfo@";
    vtype    = Typ.dummy;
    vattr = [];
    vstorage = NoStorage;
    valignas = None;
    vglob = false;
    vdefined = false;
    vformal = false;
    vinline = false;
    vdecl = Fileloc.unknown;
    vid = -1;
    vaddrof = false;
    vreferenced = false;
    vtemp = false;
    vdescr = None;
    vdescrpure = false;
    vghost = false;
    vsource = false;
    vlogic_var_assoc = None;
  }

  include Make_with_collections
      (struct
        type t = varinfo
        let name = "Varinfo"
        let reprs = [ dummy ]
        let compare v1 v2 = Datatype.Int.compare v1.vid v2.vid
        let hash v = v.vid
        let equal v1 v2 = v1.vid = v2.vid
        let copy = Datatype.undefined
        let pretty fmt v = !pretty_ref fmt v
      end)
  let id v = v.vid
end

module Varinfo = struct
  include Varinfo_Id

  module Hptset = Hptset.Make (Varinfo_Id) (Hptset_Info)
  let () = clear_caches := Hptset.clear_caches :: !clear_caches
end

module Compinfo = struct
  let pretty_ref = Extlib.mk_fun "Cil_datatype.Compinfo.pretty_ref"
  let dummy = {
    cstruct     = false;
    corig_name  = "@dummy_compinfo@";
    cname       = "@dummy_compinfo@";
    ckey        = -1;
    cfields     = None;
    cattr       = [];
    creferenced = false;
  }

  include Make_with_collections
      (struct
        type t = compinfo
        let name = "compinfo"
        let reprs = [ dummy ]
        let compare v1 v2 = Datatype.Int.compare v1.ckey v2.ckey
        let hash v = Hashtbl.hash v.ckey
        let equal v1 v2 = v1.ckey = v2.ckey
        let copy = Datatype.undefined
        let pretty fmt f = !pretty_ref fmt f
      end)
end

module Fieldinfo = struct
  let pretty_ref = Extlib.mk_fun "Cil_datatype.Fieldinfo.pretty_ref"
  let dummy = {
    fcomp      = Compinfo.dummy;
    forder     = 0;
    forig_name = "@dummy_fieldinfo@";
    fname      = "@dummy_fieldinfo@";
    ftype      = Typ.dummy;
    fbitfield  = None;
    falignas   = None;
    fattr      = [];
    floc       = Fileloc.unknown;
    faddrof    = false;
    fsize_in_bits   = None;
    foffset_in_bits = None;
  }

  include Make_with_collections
      (struct
        type t = fieldinfo
        let name = "fieldinfo"
        let reprs =
          List.fold_left
            (fun acc ci ->
               List.fold_left
                 (fun acc typ ->
                    List.fold_left
                      (fun acc loc ->
                         { fcomp = ci;
                           forder = 0;
                           forig_name = "";
                           fname = "";
                           ftype = typ;
                           fbitfield = None;
                           falignas = None;
                           fattr = [];
                           floc = loc;
                           faddrof = false;
                           fsize_in_bits = None;
                           foffset_in_bits = None
                         }
                         :: acc)
                      acc
                      Fileloc.reprs)
                 acc
                 Typ.reprs)
            []
            Compinfo.reprs
        let fid fi = fi.fcomp.ckey, fi.forder
        let compare f1 f2 = Extlib.compare_basic (fid f1) (fid f2)
        let hash f1 = Hashtbl.hash (fid f1)
        let equal f1 f2 = (fid f1) = (fid f2)
        let copy = Datatype.undefined
        let pretty fmt f = !pretty_ref fmt f
      end)
end

module Enuminfo = struct
  let dummy = {
    eorig_name  = "@dummy_enuminfo@";
    ename       = "@dummy_enuminfo@";
    eitems      = [];
    eattr       = [];
    ereferenced = false;
    ekind       = IInt;
  }

  include Make_with_collections
      (struct
        include Datatype.Undefined
        type t = enuminfo
        let name = "Enuminfo"
        let reprs = [ dummy ]
        let compare v1 v2 = String.compare v1.ename v2.ename
        let hash v = Hashtbl.hash v.ename
        let equal v1 v2 = v1.ename = v2.ename
        let pretty = Datatype.undefined
      end)
end

module Enumitem = struct
  let dummy = {
    eiorig_name = "@dummy_enumitem@";
    einame      = "@dummy_enumitem@";
    eival       = Exp.dummy;
    eihost      = Enuminfo.dummy;
    eiloc       = Fileloc.unknown;
  }

  include Make_with_collections
      (struct
        include Datatype.Undefined
        type t = enumitem
        let name = "Enumitem"
        let reprs =
          List.map
            (fun i ->
               { eiorig_name = "";
                 einame = "";
                 eival  = Exp.dummy;
                 eihost = i;
                 eiloc = Fileloc.unknown })
            Enuminfo.reprs
        let compare v1 v2 = String.compare v1.einame v2.einame
        let hash v = Hashtbl.hash v.einame
        let equal v1 v2 = v1.einame = v2.einame
        let pretty = Datatype.undefined
      end)
end

(* If [strict] is true, the comparison of integer and floating-point constants
   takes into account their textual representation (if any). Otherwise,
   constants with the same type and value are equal even if their textual
   representations differ. *)
let compare_constant ~strict c1 c2 = match c1, c2 with
  | CInt64(v1,k1,s1), CInt64(v2,k2,s2) ->
    let r = compare_chain Z.compare v1 v2 Extlib.compare_basic k1 k2 in
    if r = 0 && strict
    then Option.compare Datatype.String.compare s1 s2
    else r
  | CChr c1, CChr c2 -> Datatype.Char.compare c1 c2
  | CReal (f1,k1,s1), CReal(f2,k2,s2) ->
    let r =
      compare_chain Datatype.Float.compare f1 f2 Extlib.compare_basic k1 k2
    in
    if r = 0 && strict
    then Option.compare Datatype.String.compare s1 s2
    else r
  | CEnum e1, CEnum e2 -> Enumitem.compare e1 e2
  | (CInt64 _, (CChr _ | CReal _ | CEnum _)) -> 1
  | (CChr _, (CReal _ | CEnum _)) -> 1
  | (CReal _, CEnum _) -> 1
  | (CChr _ | CReal _ | CEnum _),
    (CInt64 _ | CChr _ | CReal _) -> -1

let hash_const c =
  match c with
  | CChr _ -> Hashtbl.hash c
  | CReal (fn,fk,_) -> Hashtbl.hash fn + Hashtbl.hash fk
  | CInt64 (n,k,_) -> Z.hash n + Hashtbl.hash k
  | CEnum ei -> 95 + Enumitem.hash ei

module type Make_cmp_input = sig
  include Datatype.Make_input
  val dummy: t
  val compare: structural:bool -> strict:bool -> t -> t -> int
end

module Make_compare_non_strict(M: Make_cmp_input) = struct
  let dummy = M.dummy
  include Make_with_collections(struct
      include M
      let compare = M.compare ~structural:false ~strict:false
    end)
end

module Make_compare_strict(M: Make_cmp_input) = struct
  let dummy = M.dummy
  include Make_with_collections(struct
      include M
      let compare = M.compare ~structural:false ~strict:true
      let name = M.name ^ "Strict"
    end)
end

module Make_compare_strict_sized(M: Make_cmp_input) = struct
  let dummy = M.dummy
  include Make_with_collections(struct
      include M
      let compare = M.compare ~structural:true ~strict:true
      let name = M.name ^ "StrictSized"
    end)
end

module Make_compare_non_strict_sized(M: Make_cmp_input) = struct
  let dummy = M.dummy
  include Make_with_collections(struct
      include M
      let compare = M.compare ~structural:true ~strict:false
      let name = M.name ^ "Sized"
    end)
end

module StructEq =
struct

  (* Return true if the types have the same size, machine-independently *)
  let rec compare_structural_typ_size (t1 : typ) (t2 : typ) : int =
    match (!punrollType t1).tnode, (!punrollType t2).tnode with
    | TPtr t1, TPtr t2 when
        (match t1.tnode, t2.tnode with
         | (TVoid | TInt (IChar | ISChar | IUChar)),
           (TVoid | TInt (IChar | ISChar | IUChar)) -> true
         | _ -> false) ->
      (* Void and characters pointers have the same size.
         ISO.6.2.5.28 *)
      0
    | TPtr { tnode = TComp comp1 }, TPtr { tnode = TComp comp2 }
      when comp1.cstruct = comp2.cstruct ->
      (* Struct pointers have the same size.
         Union pointers have the same size.
         ISO.6.2.5.28 *)
      0
    | TPtr t1, TPtr t2 ->
      (* Pointers aren't force to have the same size. Depends on the type.
         ISO.6.2.5.28 *)
      Typ.compare t1 t2
    | TComp c1, TComp c2 ->
      (* Struct/union's id are not necessarily unified.
         We structurally compare them. *)
      begin
        match c1.cfields, c2.cfields with
        | Some c1f, Some c2f when c1.cstruct == c2.cstruct ->
          List.compare
            (fun f1 f2 -> compare_structural_typ_size f1.ftype f2.ftype)
            c1f c2f
        | _ -> Typ.compare t1 t2
      end
    | _ -> Typ.compare t1 t2


  (* If [strict] is true, the comparison of integer and floating-point constants
     takes into account their textual representation (if any). Otherwise,
     constants with the same type and value are equal even if their textual
     representations differ.
     If [structural] is true, the comparison of type into sizeof takes only
     the size into account. Otherwise, we use the usual type comparison.
  *)
  let rec compare_exp ~structural ~strict e1 e2 =
    let compare_exp = compare_exp ~structural ~strict in
    let compare_lval = compare_lval ~structural ~strict in
    match e1.enode, e2.enode with
    | Const c1, Const c2 -> compare_constant ~strict c1 c2
    | Const _, _ -> 1
    | _, Const _ -> -1
    | Lval lv1, Lval lv2 -> compare_lval lv1 lv2
    | Lval _, _ -> 1
    | _, Lval _ -> -1
    | SizeOf t1, SizeOf t2 ->
      if structural
      then compare_structural_typ_size t1 t2
      else Typ.compare t1 t2
    | SizeOf _, _  -> 1
    | _, SizeOf _ -> -1
    | SizeOfE e1, SizeOfE e2 -> compare_exp e1 e2
    | SizeOfE _, _ -> 1
    | _, SizeOfE _ -> -1
    | AlignOf (ty1, i1), AlignOf (ty2, i2) ->
      compare_chain
        Cil_types.compare_standard_or_gcc i1 i2
        Typ.compare ty1 ty2
    | AlignOf _, _ -> 1
    | _, AlignOf _ -> -1
    | AlignOfE (e1, i1), AlignOfE (e2, i2) ->
      compare_chain
        Cil_types.compare_standard_or_gcc i1 i2
        compare_exp e1 e2
    | AlignOfE _, _ -> 1
    | _, AlignOfE _ -> -1
    | UnOp(op1,e1,ty1), UnOp(op2,e2,ty2) ->
      let res = Extlib.compare_basic op1 op2 in
      if res = 0 then
        let res = compare_exp e1 e2 in
        if res = 0 then Typ.compare ty1 ty2 else res
      else res
    | UnOp _, _ -> 1
    | _, UnOp _ -> -1
    | BinOp(op1,e11,e21, ty1), BinOp(op2,e12,e22, ty2) ->
      let res = Extlib.compare_basic op1 op2 in
      if res = 0 then
        let res = compare_exp e11 e12 in
        if res = 0 then
          let res = compare_exp e21 e22 in
          if res = 0 then Typ.compare ty1 ty2 else res
        else res
      else res
    | BinOp _, _ -> 1
    | _, BinOp _ -> -1
    | CastE(t1,e1), CastE(t2, e2) ->
      let res = Typ.compare t1 t2 in
      if res = 0 then compare_exp e1 e2 else res
    | CastE _, _ -> 1
    | _, CastE _ -> -1
    | AddrOf lv1, AddrOf lv2 -> compare_lval lv1 lv2
    | AddrOf _, _ -> 1
    | _, AddrOf _ -> -1
    | StartOf lv1, StartOf lv2 -> compare_lval lv1 lv2

  and compare_lval ~structural ~strict (h1,o1) (h2,o2) =
    let res = compare_lhost ~structural ~strict h1 h2 in
    if res = 0 then compare_offset ~structural ~strict o1 o2 else res

  and compare_lhost ~structural ~strict h1 h2 =
    match h1, h2 with
    | Var v1, Var v2 -> Varinfo.compare v1 v2
    | Var _, Mem _ -> 1
    | Mem e1, Mem e2 -> compare_exp ~structural ~strict e1 e2
    | Mem _, Var _ -> -1

  and compare_offset ~structural ~strict o1 o2 =
    match o1, o2 with
    | NoOffset, NoOffset -> 0
    | NoOffset, _ -> 1
    | _, NoOffset -> -1
    | Field(f1,o1), Field(f2, o2) ->
      let res = Fieldinfo.compare f1 f2 in
      if res = 0 then compare_offset ~structural ~strict o1 o2 else res
    | Field _, _ -> 1
    | _, Field _ -> -1
    | Index(e1, o1), Index(e2, o2) ->
      let res = compare_exp ~structural ~strict e1 e2 in
      if res = 0 then compare_offset ~structural ~strict o1 o2 else res

  let prime = 83047
  let rec hash_exp acc e =
    match e.enode with
    | Const c -> prime * acc lxor hash_const c
    | Lval lv -> hash_lval ((prime*acc) lxor 42) lv
    | SizeOf t -> (prime*acc) lxor Typ.hash t
    | SizeOfE e -> hash_exp ((prime*acc) lxor 75) e
    | AlignOf (t, i) ->
      let r = (prime * acc) lxor Hashtbl.hash i in
      (prime * r) lxor Typ.hash t
    | AlignOfE (e, i) ->
      let r = (prime * acc) lxor Hashtbl.hash i in
      hash_exp ((prime * r) lxor 153) e
    | UnOp(op,e,ty) ->
      let res = hash_exp ((prime*acc) lxor Hashtbl.hash op) e in
      (prime*res) lxor Typ.hash ty
    | BinOp(op,e1,e2,ty) ->
      let res = hash_exp ((prime*acc) lxor Hashtbl.hash op) e1 in
      let res = hash_exp ((prime*res) lxor 257) e2 in
      (prime * res) lxor Typ.hash ty
    | CastE(ty,e) -> hash_exp ((prime*acc) lxor Typ.hash ty) e
    | AddrOf lv -> hash_lval (prime*acc lxor 329) lv
    | StartOf lv -> hash_lval (prime*acc lxor 431) lv
  and hash_lval acc (h,o) =
    hash_offset ((prime * acc) lxor hash_lhost 856 h) o
  and hash_lhost acc = function
    | Var v -> (prime * acc) lxor (Varinfo.hash v)
    | Mem e -> hash_exp ((prime * acc) lxor 967) e
  and hash_offset acc = function
    | NoOffset -> (prime * acc) lxor 1583
    | Index(e,o) ->
      let res = hash_exp 1790 e in
      hash_offset ((prime * acc) lxor res) o
    | Field(f,o) -> hash_offset ((prime * acc) lxor Hashtbl.hash f.fname) o
end

module Wide_string = struct
  let dummy = [ Int64.zero ]
  include Datatype.List_with_collections(Datatype.Int64)
end

module Constant_input =
struct
  let pretty_ref = Extlib.mk_fun "Cil_datatype.Constant.pretty_ref"
  include Datatype.Serializable_undefined
  type t = constant
  let name = "Constant"
  let dummy = CInt64(Z.zero, IInt, Some "0")
  let reprs = [ dummy ]
  let compare ~structural = (fun _ -> compare_constant) structural
  let hash = hash_const
  let equal = Datatype.from_compare
  let pretty fmt t = !pretty_ref fmt t
end

module Constant = struct
  include Make_compare_non_strict(Constant_input)
  let pretty_ref = Constant_input.pretty_ref
end

module ConstantStrict =
  Make_compare_strict(Constant_input)

module ExpStructEq_input = struct
  include Datatype.Serializable_undefined
  type t = exp
  let name = "ExpStructEq"
  let dummy = Exp.dummy
  let reprs = [ dummy ]
  let compare = StructEq.compare_exp
  let hash = StructEq.hash_exp 7863
  let equal = Datatype.from_compare
  let pretty fmt t = !Exp.pretty_ref fmt t
end

module ExpStructEq =
  Make_compare_non_strict(ExpStructEq_input)
let () = compare_exp_struct_eq := ExpStructEq.compare

module ExpStructEqStrict =
  Make_compare_strict(ExpStructEq_input)

module ExpStructEqSized =
  Make_compare_non_strict_sized(ExpStructEq_input)

module ExpStructEqStrictSized =
  Make_compare_strict_sized(ExpStructEq_input)

module Block = struct
  let pretty_ref = Extlib.mk_fun "Cil_datatype.Block.pretty_ref"

  let dummy = {
    battrs   = [];
    blocals  = [];
    bstatics = [];
    bstmts   = [];
    bscoping = false;
  }

  include Make
      (struct
        type t = block
        let name = "Block"
        let reprs =
          [{battrs=[]; blocals=Varinfo.reprs; bstatics = [];
            bstmts=Stmt.reprs; bscoping=true}]
        let pretty fmt b = !pretty_ref fmt b
      end)
  let equal b1 b2 = (b1 == b2)
end

let rec equal_lval (h1, o1) (h2, o2) =
  equal_lhost h1 h2 && equal_offset o1 o2

and equal_lhost h1 h2 =
  match h1,h2 with
  | Var v1, Var v2 -> Datatype.Int.equal v1.vid v2.vid
  | Mem e1, Mem e2 -> Exp.equal e1 e2
  | (Var _ | Mem _), _-> false

and equal_offset o1 o2 =
  match o1,o2 with
  | NoOffset, NoOffset -> true
  | Field(f1,o1), Field(f2,o2) ->
    Fieldinfo.equal f1 f2 && equal_offset o1 o2
  | Index(e1,o1), Index(e2,o2) ->
    Exp.equal e1 e2 && equal_offset o1 o2
  | (NoOffset | Field _ | Index _), _ -> false

let rec compare_lval (h1,o1) (h2,o2) =
  compare_chain compare_lhost h1 h2 compare_offset o1 o2

and compare_lhost h1 h2 =
  match h1,h2 with
    Var v1, Var v2 -> Datatype.Int.compare v1.vid v2.vid
  | Mem e1, Mem e2 -> Exp.compare e1 e2
  | Var _, Mem _ -> 1
  | Mem _, Var _ -> -1

and compare_offset o1 o2 =
  match o1,o2 with
    NoOffset, NoOffset -> 0
  | Field(f1,o1), Field(f2,o2) ->
    compare_chain Fieldinfo.compare f1 f2 compare_offset o1 o2
  | Index(e1,o1), Index(e2,o2) ->
    compare_chain Exp.compare e1 e2 compare_offset o1 o2
  | (NoOffset, (Field _ | Index _)) -> 1
  | (Field _, Index _) -> 1
  | ((Field _ | Index _), (NoOffset | Field _ )) -> -1

let rec hash_lval (h,o) =
  Hashtbl.hash (hash_lhost h, hash_offset o)

and hash_lhost = function
  | Var v -> 17 + v.vid
  | Mem e -> 13 + 5 * e.eid

and hash_offset = function
  | NoOffset -> 19
  | Field(f,o) -> Hashtbl.hash (Fieldinfo.hash f, hash_offset o)
  | Index (e, o) -> Hashtbl.hash (e.eid, hash_offset o)

module Lval = struct
  let pretty_ref = ref (fun _ -> assert false)
  let dummy = Var Varinfo.dummy, NoOffset

  include Make_with_collections
      (struct
        type t = lval
        let name = "Lval"
        let reprs = List.map (fun v -> Var v, NoOffset) Varinfo.reprs
        let compare = compare_lval
        let equal = equal_lval
        let hash = hash_lval
        let copy = Datatype.undefined
        let pretty fmt x = !pretty_ref fmt x
      end)
end

module LvalStructEq_input = struct
  include Datatype.Serializable_undefined
  type t = lval
  let name = "LvalStructEq"
  let dummy = Lval.dummy
  let reprs = List.map (fun v -> Var v, NoOffset) Varinfo.reprs
  let compare = StructEq.compare_lval
  let equal = Datatype.from_compare
  let hash = StructEq.hash_lval 13598
  let copy = Datatype.undefined
  let pretty fmt x = !Lval.pretty_ref fmt x
end

module LvalStructEq =
  Make_compare_non_strict(LvalStructEq_input)

module LvalStructEqStrict =
  Make_compare_strict(LvalStructEq_input)

module Offset = struct
  let pretty_ref = ref (fun _ -> assert false)
  let dummy = NoOffset

  include Make_with_collections
      (struct
        type t = offset
        let name = "Offset"
        let reprs = [ dummy ]
        let compare = compare_offset
        let equal = equal_offset
        let hash = hash_offset
        let copy = Datatype.undefined
        let pretty fmt x = !pretty_ref fmt x
      end)
end

module OffsetStructEq_input = struct
  include Datatype.Serializable_undefined
  type t = offset
  let name = "OffsetStructEq"
  let dummy = Offset.dummy
  let reprs = [ dummy ]
  let compare = StructEq.compare_offset
  let equal = Datatype.from_compare
  let hash = StructEq.hash_offset 75489
  let copy = Datatype.undefined
  let pretty fmt x = !Offset.pretty_ref fmt x
end

module OffsetStructEq = Make_compare_non_strict(OffsetStructEq_input)
module OffsetStructEqStrict = Make_compare_strict(OffsetStructEq_input)

(**************************************************************************)
(** {3 ACSL types} *)
(**************************************************************************)

module Builtin_logic_info = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    bl_name    = "@dummy_builtin_logic_info@";
    bl_labels  = [];
    bl_params  = [];
    bl_type    = None;
    bl_profile = [];
  }

  include Make_with_collections
      (struct
        type t = builtin_logic_info
        let name = "Builtin_logic_info"
        let reprs = [ dummy ]
        let compare i1 i2 = String.compare i1.bl_name i2.bl_name
        let hash i = Hashtbl.hash i.bl_name
        let equal i1 i2 = i1.bl_name = i2.bl_name
        let copy = Datatype.identity (* works only if an AST is never modified *)
        let pretty fmt li = !pretty_ref fmt li
      end)
end

module Logic_type_info = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    lt_name   = "@dummy_logic_type_info@";
    lt_params = [];
    lt_def    = None;
    lt_attr   = [];
  }

  include Make_with_collections
      (struct
        type t = logic_type_info
        let name = "Logic_type_info"
        let reprs = [ dummy ]
        let compare t1 t2 = String.compare t1.lt_name t2.lt_name
        let equal t1 t2 = t1.lt_name = t2.lt_name
        let hash t = Hashtbl.hash t.lt_name
        let copy = Datatype.identity (* works only if an AST is never modified *)
        let pretty fmt lt = !pretty_ref fmt lt
      end)
end

module Logic_ctor_info = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    ctor_name   = "@dummy_logic_ctor_info@";
    ctor_type   = Logic_type_info.dummy;
    ctor_params = [];
  }

  include Make_with_collections
      (struct
        type t = logic_ctor_info
        let name = "Logic_ctor_info"
        let reprs =
          List.map
            (fun v -> { ctor_name = ""; ctor_type = v; ctor_params = [] })
            Logic_type_info.reprs
        let compare t1 t2 = String.compare t1.ctor_name t2.ctor_name
        let equal t1 t2 = t1.ctor_name = t2.ctor_name
        let hash t = Hashtbl.hash t.ctor_name
        let copy = Datatype.identity (* works only if an AST is never modified *)
        let pretty fmt c = !pretty_ref fmt c
      end)
end

module Initinfo = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = { init = None }
  include Make
      (struct
        type t = initinfo
        let name = "Initinfo"
        let reprs =
          dummy ::
          List.map (fun e -> { init = Some (CInit (SingleInit e)) }) Exp.reprs
        let pretty fmt i = !pretty_ref fmt i
      end)
end

let rec compare_logic_type config v1 v2 =
  let rank = function
    | Lboolean -> 0
    | Linteger -> 1
    | Lreal -> 2
    | Ctype _ -> 3
    | Lvar _ -> 4
    | Ltype _ -> 5
    | Larrow _ -> 6
  in
  let v1 = if config.unroll then !punrollLogicType v1 else v1 in
  let v2 = if config.unroll then !punrollLogicType v2 else v2 in
  let k1 = rank v1 in
  let k2 = rank v2 in
  if k1 <> k2 then
    k1-k2
  else
    match v1,v2 with
    | Ctype t1 , Ctype t2 -> compare_type config t1 t2
    | Ltype(a,ts1), Ltype(b,ts2) ->
      let c = Logic_type_info.compare a b in
      if c <> 0 then c
      else compare_list (compare_logic_type config) ts1 ts2
    | Lvar x1, Lvar x2 -> Datatype.String.compare x1 x2
    | Lboolean, Lboolean -> 0
    | Linteger, Linteger -> 0
    | Lreal, Lreal -> 0
    | Larrow(l1, t1), Larrow(l2, t2) ->
      let c = compare_logic_type config t1 t2 in
      if c <> 0 then c else compare_list (compare_logic_type config) l1 l2
    | _ -> assert false

let rec hash_logic_type config = function
  | Lboolean -> 0
  | Linteger -> 1
  | Lreal -> 2
  | Ctype ty -> hash_type config ty
  | Ltype({ lt_def = Some (LTsyn t)},_) when config.unroll ->
    hash_logic_type config t
  | Ltype(t,_) ->
    Logic_type_info.hash t
  | Lvar x -> Datatype.String.hash x
  | Larrow (_,t) -> 41 * hash_logic_type config t

(* Shared between the different modules for logic types *)
let pretty_logic_type_ref = ref (fun _ _ -> assert false)

module Make_Logic_type
    (M: sig val config: type_compare_config val name: string end) = struct
  let pretty_ref = pretty_logic_type_ref
  let dummy = Ctype Typ.dummy
  include Make_with_collections(
    struct
      include Datatype.Undefined
      type t = logic_type
      let name = M.name
      let reprs = List.map (fun t -> Ctype t) Typ.reprs

      let compare = compare_logic_type M.config
      let equal = Datatype.from_compare
      let hash = hash_logic_type M.config

      let pretty fmt t = !pretty_logic_type_ref fmt t

    end)
end

module Logic_type =
  Make_Logic_type(
  struct
    let config = { by_name = false; logic_type = true;
                   unroll = true; no_attrs = false }
    let name = "Logic_type"
  end)

module Logic_type_ByName =
  Make_Logic_type(
  struct
    let name = "Logic_type_ByName"
    let config = { by_name = true; logic_type = true;
                   unroll = false; no_attrs = false }
  end)

module Logic_type_NoUnroll =
  Make_Logic_type(
  struct
    let name = "Logic_type_NoUnroll"
    let config = { by_name = false; logic_type = false;
                   unroll = false; no_attrs = false }
  end)

module Logic_var = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    lv_name   = "@dummy_logic_var@";
    lv_kind   = LVQuant;
    lv_id     = -1;
    lv_type   = Logic_type.dummy;
    lv_origin = None;
    lv_attr   = [];
  }

  include Make_with_collections
      (struct
        type t = logic_var
        let name = "Logic_var"
        let reprs =
          let dummy v =
            let kind = match v with None -> LVQuant | Some _ -> LVC in
            { lv_name = "";
              lv_kind = kind;
              lv_id = -1;
              lv_type = Linteger;
              lv_origin = v;
              lv_attr = [];
            }
          in
          dummy None
          :: List.map (fun v -> dummy (Some v)) Varinfo.reprs
        let compare v1 v2 = Datatype.Int.compare v1.lv_id v2.lv_id
        let hash v = v.lv_id
        let equal v1 v2 = v1.lv_id = v2.lv_id
        let copy = Datatype.undefined
        let pretty fmt t = !pretty_ref fmt t
      end)
end

module Logic_info = struct
  let pretty_ref = ref (fun fmt f -> Logic_var.pretty fmt f.l_var_info)
  let dummy = {
    l_var_info = Logic_var.dummy;
    l_labels   = [];
    l_tparams  = [];
    l_type     = None;
    l_profile  = [];
    l_body     = LBnone;
  }

  include Make_with_collections
      (struct
        type t = logic_info
        let name = "Logic_info"
        let reprs =
          List.map
            (fun v ->
               { l_var_info = v;
                 l_labels = [];
                 l_tparams = [];
                 l_type = None;
                 l_profile = [];
                 l_body = LBnone })
            Logic_var.reprs
        let compare i1 i2 = Logic_var.compare i1.l_var_info i2.l_var_info
        let equal i1 i2 = Logic_var.equal i1.l_var_info i2.l_var_info
        let hash i = Logic_var.hash i.l_var_info
        let copy = Datatype.undefined
        let pretty fmt li = !pretty_ref fmt li
      end)
end

(* Logic_info with structural comparison
   if functions / predicates have the same name (overloading), compare
   their arguments types ; ignore polymorphism *)
module Logic_info_structural = struct
  let pretty_ref = ref (fun fmt f -> Logic_var.pretty fmt f.l_var_info)
  let dummy = {
    l_var_info = Logic_var.dummy;
    l_labels   = [];
    l_tparams  = [];
    l_type     = None;
    l_profile  = [];
    l_body     = LBnone;
  }

  include Make_with_collections
      (struct
        type t = logic_info
        let name = "Logic_info_structural"
        let reprs =
          List.map
            (fun v ->
               { l_var_info = v;
                 l_labels = [];
                 l_tparams = [];
                 l_type = None;
                 l_profile = [];
                 l_body = LBnone })
            Logic_var.reprs
        let compare i1 i2 =
          let name_cmp =
            String.compare i1.l_var_info.lv_name i2.l_var_info.lv_name
          in
          if name_cmp <> 0 then name_cmp else begin
            let config =
              { by_name = true ; logic_type = true ;
                unroll = true ; no_attrs = false }
            in
            let prm_cmp p1 p2 =
              compare_logic_type config p1.lv_type p2.lv_type
            in
            compare_list prm_cmp i1.l_profile i2.l_profile
          end

        let equal = Datatype.from_compare
        let hash i = Logic_var.hash i.l_var_info
        let copy = Datatype.undefined
        let pretty fmt li = !Logic_info.pretty_ref fmt li
      end)
end

module Model_info = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    mi_name       = "@dummy_model_info@";
    mi_base_type  = Typ.dummy;
    mi_field_type = Logic_type.dummy;
    mi_decl       = Fileloc.unknown;
    mi_attr       = [];
  }

  include Make_with_collections(
    struct
      type t = model_info
      include Datatype.Undefined
      let name = "model_info"
      let reprs =
        List.product_map
          (fun base field ->
             { mi_name = "dummy";
               mi_base_type = base;
               mi_field_type = field;
               mi_decl = Fileloc.unknown;
               mi_attr = [];
             })
          Typ.reprs
          Logic_type.reprs
      let compare mi1 mi2 =
        let scmp = String.compare mi1.mi_name mi2.mi_name in
        if scmp <> 0 then scmp
        else
          Typ.compare mi1.mi_base_type mi2.mi_base_type
      let equal = Datatype.from_compare
      let hash mi = Hashtbl.hash mi.mi_name + 3 * Typ.hash mi.mi_base_type
      let copy mi = {
        mi_name = mi.mi_name;
        mi_base_type = Typ.copy mi.mi_base_type;
        mi_field_type = Logic_type.copy mi.mi_field_type;
        mi_decl = Fileloc.copy mi.mi_decl;
        mi_attr = List.map Attribute.copy mi.mi_attr
      }
      let pretty fmt t = !pretty_ref fmt t
    end)
end

(* -------------------------------------------------------------------------- *)
(* --- Comparison Over Terms                                              --- *)
(* -------------------------------------------------------------------------- *)

(* @return [true] is the given logic real represents an exact float *)
let is_exact_float r =
  classify_float r.r_upper = FP_normal &&
  Datatype.Float.equal r.r_upper r.r_lower

[@@@ warning "-3"]
(* [float_compare_total] is used to ensure -0.0 and 0.0 are distinct *)
external float_compare_total : float -> float -> int = "float_compare_total" "noalloc"
[@@@ warning "+3"]

let compare_logic_real r1 r2 =
  let c = float_compare_total r1.r_lower r2.r_lower in
  if c <> 0 then c else
    let c = float_compare_total r1.r_nearest r2.r_nearest in
    if c <> 0 then c else
      let c = float_compare_total r1.r_upper r2.r_upper in
      if c <> 0 then c else
        String.compare r1.r_literal r2.r_literal

let compare_logic_constant c1 c2 = match c1,c2 with
  | Boolean b1, Boolean b2 -> compare b1 b2
  | Integer (i1,_), Integer(i2,_) -> Z.compare i1 i2
  | LStr s1, LStr s2 -> Datatype.String.compare s1 s2
  | LWStr s1, LWStr s2 -> compare_list Datatype.Int64.compare s1 s2
  | LChr c1, LChr c2 -> Datatype.Char.compare c1 c2
  | LReal r1, LReal r2 -> compare_logic_real r1 r2
  | LEnum e1, LEnum e2 -> Enumitem.compare e1 e2
  | Boolean _,(Integer _ | LStr _|LWStr _ |LChr _|LReal _|LEnum _) -> 1
  | Integer _,(LStr _|LWStr _ |LChr _|LReal _|LEnum _) -> 1
  | LStr _ ,(LWStr _ |LChr _|LReal _|LEnum _) -> 1
  | LWStr _ ,(LChr _|LReal _|LEnum _) -> 1
  | LChr _,(LReal _|LEnum _) -> 1
  | LReal _,LEnum _ -> 1
  | (Integer _ |LStr _|LWStr _ |LChr _|LReal _|LEnum _),
    (Boolean _ |Integer _|LStr _|LWStr _ |LChr _|LReal _) -> -1

let rec compare_term t1 t2 =
  let r1 = rank_term t1.term_node in
  let r2 = rank_term t2.term_node in
  if r1 <> r2 then r1 - r2 else
    match t1.term_node , t2.term_node with
    | TConst c1 , TConst c2 -> compare_logic_constant c1 c2
    | TLval lv1 , TLval lv2
    | TAddrOf lv1 , TAddrOf lv2
    | TStartOf lv1 , TStartOf lv2 -> compare_tlval lv1 lv2
    | TSizeOf ty1 , TSizeOf ty2
    | TAlignOf ty1 , TAlignOf ty2 -> Typ.compare ty1 ty2
    | TSizeOfE t1 , TSizeOfE t2
    | TAlignOfE t1 , TAlignOfE t2 -> compare_term t1 t2
    | TUnOp(op1,t1) , TUnOp(op2,t2) ->
      let c = Extlib.compare_basic op1 op2 in
      if c <> 0 then c else compare_term t1 t2
    | TBinOp(op1,x1,y1) , TBinOp(op2,x2,y2) ->
      let c = Extlib.compare_basic op1 op2 in
      if c <> 0 then c else
        let cx = compare_term x1 x2 in
        if cx <> 0 then cx else compare_term y1 y2
    | TCast (false, ty1,e1), TCast (false, ty2,e2)
    | TCast (true, ty1,e1) , TCast (true, ty2,e2) ->
      let ct = Logic_type.compare ty1 ty2 in
      if ct <> 0 then ct
      else compare_term e1 e2
    | Tapp(f1,labs1,ts1) , Tapp(f2,labs2,ts2) ->
      let cf = Logic_info.compare f1 f2 in
      if cf <> 0 then cf else
        let cl = compare_list compare_logic_label labs1 labs2 in
        if cl <> 0 then cl else compare_list compare_term ts1 ts2
    | Tlambda(q1,t1) , Tlambda(q2,t2) ->
      let cq = compare_list Logic_var.compare q1 q2 in
      if cq <> 0 then cq else compare_term t1 t2
    | TDataCons(f1,ts1) , TDataCons(f2,ts2) ->
      let cq = compare_ctor f1 f2 in
      if cq <> 0 then cq else compare_list compare_term ts1 ts2
    | Tif(c1,a1,b1) , Tif(c2,a2,b2) ->
      let c = compare_predicate c1 c2 in
      if c <> 0 then c else compare_list compare_term [a1;b1] [a2;b2]
    | Tbase_addr (l1,t1) , Tbase_addr (l2,t2)
    | Tblock_length (l1,t1) , Tblock_length (l2,t2)
    | Toffset (l1,t1) , Toffset (l2,t2)
    | Tat(t1,l1) , Tat(t2,l2) ->
      let cl = compare_logic_label l1 l2 in
      if cl <> 0 then cl else compare_term t1 t2
    | Tnull , Tnull -> 0
    | TUpdate(x1,off1,y1) , TUpdate(x2,off2,y2) ->
      let cx = compare_term x1 x2 in
      if cx <> 0 then cx else
        let cf = compare_toffset off1 off2 in
        if cf <> 0 then cf else compare_term y1 y2
    | Ttypeof t1 , Ttypeof t2 -> compare_term t1 t2
    | Ttype ty1 , Ttype ty2 -> Typ.compare ty1 ty2
    | Tempty_set , Tempty_set -> 0
    | Tunion ts1 , Tunion ts2
    | Tinter ts1 , Tinter ts2 -> compare_list compare_term ts1 ts2
    | Trange(a1,b1) , Trange(a2,b2) ->
      let c = compare_bound a1 a2 in
      if c <> 0 then c else compare_bound b1 b2
    | Tlet(x1,t1) , Tlet(x2,t2) ->
      let c = Logic_info.compare x1 x2 in
      if c <> 0 then c else compare_term t1 t2
    | Tcomprehension (t1, q1, p1), Tcomprehension (t2, q2, p2) ->
      let c = compare_term t1 t2 in
      if c <> 0 then c
      else
        let cq = compare_list Logic_var.compare q1 q2 in
        if cq <> 0 then cq else Option.compare compare_predicate p1 p2
    | (TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TAlignOf _
      | TAlignOfE _ | TUnOp _ | TBinOp _ | TCast _ | TAddrOf _ | TStartOf _
      | Tapp _ | Tlambda _ | TDataCons _ | Tif _ | Tat _
      | Tbase_addr _ | Tblock_length _ | Toffset _
      | Tnull | TUpdate _ | Ttypeof _
      | Ttype _ | Tempty_set | Tunion _ | Tinter _  | Tcomprehension _
      | Trange _ | Tlet _ ), _ -> assert false

and compare_tlval (h1,off1) (h2,off2) =
  let ch = compare_tlhost h1 h2 in
  if ch <> 0 then ch else compare_toffset off1 off2

and compare_tlhost h1 h2 =
  match h1 , h2 with
  | TVar x1 , TVar x2 -> Logic_var.compare x1 x2
  | TMem m1 , TMem m2 -> compare_term m1 m2
  | TResult ty1 , TResult ty2 -> Typ.compare ty1 ty2
  | TVar _ , TMem _ | TVar _ , TResult _ | TMem _ , TResult _ -> (-1)
  | TMem _ , TVar _ | TResult _ , TVar _ | TResult _ , TMem _ -> 1

and compare_toffset off1 off2 =
  match off1 , off2 with
  | TNoOffset , TNoOffset -> 0
  | TField(f1,next1) , TField(f2,next2) ->
    let cf = Fieldinfo.compare f1 f2 in
    if cf <> 0 then cf else compare_toffset next1 next2
  | TIndex(t1,next1) , TIndex(t2,next2) ->
    let cf = compare_term t1 t2 in
    if cf <> 0 then cf else compare_toffset next1 next2
  | TModel(f1,next1), TModel(f2,next2) ->
    let cf = Model_info.compare f1 f2 in
    if cf <> 0 then cf else compare_toffset next1 next2
  | TNoOffset , (TField _ | TModel _ | TIndex _ )
  | TField _,   (TModel _ | TIndex _)
  | TModel _, TIndex _ -> (-1)
  | TField _, TNoOffset
  | TModel _, (TField _ | TNoOffset)
  | TIndex _, (TModel _ | TField _ | TNoOffset) -> 1

and compare_logic_label l1 l2 = match l1, l2 with
  | StmtLabel s1 , StmtLabel s2 -> Stmt.compare !s1 !s2
  | FormalLabel s1, FormalLabel s2 -> String.compare s1 s2
  | BuiltinLabel l1, BuiltinLabel l2 -> Stdlib.compare l1 l2
  | (StmtLabel _ | FormalLabel _), (FormalLabel _ | BuiltinLabel _) -> -1
  | (BuiltinLabel _ | FormalLabel _), (StmtLabel _ | FormalLabel _) -> 1

and compare_ctor c1 c2 = String.compare c1.ctor_name c2.ctor_name

and compare_bound b1 b2 = Option.compare compare_term b1 b2

and compare_predicate p1 p2 =
  let r1 = rank_predicate p1.pred_content in
  let r2 = rank_predicate p2.pred_content in
  if r1 <> r2 then r1 - r2 else
    match p1.pred_content, p2.pred_content with
    | Pfalse, Pfalse -> 0
    | Ptrue, Ptrue -> 0
    | Papp(i1,labels1,args1), Papp(i2,labels2,args2) ->
      let c = Logic_info.compare i1 i2 in
      if c <> 0 then c
      else
        let c = compare_list compare_logic_label labels1 labels2 in
        if c <> 0 then c
        else compare_list compare_term args1 args2
    | Prel(r1,lt1,rt1), Prel(r2,lt2,rt2) ->
      let c = compare r1 r2 in
      if c <> 0 then c
      else
        let c = compare_term lt1 lt2 in
        if c <> 0 then c
        else compare_term rt1 rt2
    | Pand(lp1,rp1), Pand(lp2,rp2) | Por(lp1,rp1), Por(lp2,rp2)
    | Pxor (lp1,rp1), Pxor(lp2,rp2) | Pimplies(lp1,rp1), Pimplies(lp2,rp2)
    | Piff(lp1,rp1), Piff(lp2,rp2) ->
      let c = compare_predicate lp1 lp2 in
      if c <> 0 then c
      else compare_predicate rp1 rp2
    | Pnot p1, Pnot p2 ->
      compare_predicate p1 p2
    | Pif (c1,t1,e1), Pif(c2,t2,e2) ->
      compare_list compare_predicate [c1;t1;e1] [c2;t2;e2]
    | Plet (d1,p1), Plet(d2,p2) ->
      let c = Logic_info.compare d1 d2 in
      if c <> 0 then c
      else compare_predicate p1 p2
    | Pforall(q1,p1), Pforall(q2,p2) ->
      let c = compare_list Logic_var.compare q1 q2 in
      if c <> 0 then c
      else compare_predicate p1 p2
    | Pexists(q1,p1), Pexists(q2,p2) ->
      let c = compare_list Logic_var.compare q1 q2 in
      if c <> 0 then c
      else compare_predicate p1 p2
    | Pat(p1,l1), Pat(p2,l2) ->
      let c = compare_logic_label l1 l2 in
      if c <> 0 then c else compare_predicate p1 p2
    | Pallocable (l1,t1), Pallocable (l2,t2)
    | Pfreeable (l1,t1), Pfreeable (l2,t2)
    | Pvalid (l1,t1), Pvalid (l2,t2)
    | Pvalid_read (l1,t1), Pvalid_read (l2,t2)
    | Pobject_pointer (l1,t1), Pobject_pointer (l2,t2)
    | Pinitialized (l1,t1), Pinitialized (l2,t2)
    | Pdangling (l1,t1), Pdangling (l2,t2) ->
      let c = compare_logic_label l1 l2 in
      if c <> 0 then c else compare_term t1 t2
    | Pvalid_function t1, Pvalid_function t2 ->
      compare_term t1 t2
    | Pfresh (l1,m1,t1,n1), Pfresh (l2,m2,t2,n2) ->
      let c = compare_logic_label l1 l2 in
      if c <> 0 then c
      else
        let c = compare_logic_label m1 m2 in
        if c <> 0 then c
        else
          let c = compare_term t1 t2 in
          if c <> 0 then c
          else compare_term n1 n2
    | Paligned (t1,n1), Paligned (t2,n2) ->
      let c = compare_term t1 t2 in
      if c <> 0 then c
      else compare_term n1 n2
    | Pseparated(seps1), Pseparated(seps2) ->
      compare_list compare_term seps1 seps2
    | (Pfalse | Ptrue | Papp _ | Prel _ | Pand _ | Por _ | Pimplies _
      | Piff _ | Pnot _ | Pif _ | Plet _ | Pforall _ | Pexists _
      | Pat _ | Pvalid _ | Pvalid_read _ | Pobject_pointer _ | Pvalid_function _
      | Pinitialized _ | Pdangling _ | Paligned _
      | Pfresh _ | Pallocable _ | Pfreeable _ | Pxor _ | Pseparated _
      ), _ -> assert false

exception StopRecursion of int

let hash_logic_constant = function
  | LStr s -> Datatype.String.hash s
  | LWStr l -> hash_list Datatype.Int64.hash l
  | LChr c  -> Datatype.Char.hash c
  | Boolean b -> Datatype.Bool.hash b
  | Integer(n, _) -> Z.hash n
  | LReal r ->
    if is_exact_float r then Datatype.Float.hash r.r_lower
    else Datatype.String.hash r.r_literal
  | LEnum ei -> 95 + Enumitem.hash ei

let hash_label x =
  match x with
    StmtLabel r -> 2*(Stmt.hash !r)
  | BuiltinLabel l -> 2*(Hashtbl.hash l) + 1
  | FormalLabel s -> 2*(Hashtbl.hash s) + 3

let rec hash_term (acc,depth,tot) t =
  if tot <= 0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    match t.term_node with
    | TConst c -> (acc + hash_logic_constant c, tot - 1)
    | TLval lv -> hash_tlval (acc+2,depth - 1,tot -1) lv
    | TSizeOf t -> (acc + 3 + Typ.hash t, tot - 1)
    | TSizeOfE t -> hash_term (acc+5,depth -1, tot-1) t
    | TAlignOf t -> (acc + 11 + Typ.hash t, tot - 1)
    | TAlignOfE t -> hash_term (acc+13,depth-1,tot-1) t
    | TUnOp(op,t) -> hash_term (acc+17+Hashtbl.hash op,depth-1,tot-2) t
    | TBinOp(bop,t1,t2) ->
      let hash1,tot1 =
        hash_term (acc+19+Hashtbl.hash bop,depth-1,tot-2) t1
      in
      hash_term (hash1,depth-1,tot1) t2
    | TCast(false, Ctype ty,t) ->
      let hash1 = Typ.hash ty in
      hash_term (acc+23+hash1,depth-1,tot-2) t
    | TCast(true,_,e) -> hash_term (acc + 113, depth - 1, tot - 1) e
    (* TODO (NB) : Check the magic values here (see if it can't be merged) *)
    | TCast(false,_,_) -> assert false
    | TAddrOf lv -> hash_tlval (acc+29,depth-1,tot-1) lv
    | TStartOf lv -> hash_tlval (acc+31,depth-1,tot-1) lv
    | Tapp (li,labs,apps) ->
      let hash1 = acc + 37 + Logic_info.hash li in
      let hash_lb (acc,tot) l =
        if tot = 0 then raise (StopRecursion acc)
        else (acc + hash_label l,tot - 1)
      in
      let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
      let res = List.fold_left hash_lb (hash1,tot-1) labs in
      List.fold_left hash_one_term res apps
    | Tlambda(quants,t) ->
      let hash_var (acc,tot) lv =
        if tot = 0 then raise (StopRecursion acc)
        else (acc + Logic_var.hash lv,tot-1)
      in
      let (acc,tot) = List.fold_left hash_var (acc+41,tot-1) quants in
      hash_term (acc,depth-1,tot-1) t
    | TDataCons(ctor,args) ->
      let hash = acc + 43 + Logic_ctor_info.hash ctor in
      let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
      List.fold_left hash_one_term (hash,tot-1) args
    | Tif(p,t1,t2) ->
      let hash1,tot1 = hash_predicate (acc+47,depth-1,tot) p in
      let hash2,tot2 = hash_term (hash1,depth-1,tot1) t1 in
      hash_term (hash2,depth-1,tot2) t2
    | Tat(t,l) ->
      let hash = acc + 53 + hash_label l in
      hash_term (hash,depth-1,tot-2) t
    | Tbase_addr (l,t) ->
      let hash = acc + 59 + hash_label l in
      hash_term (hash,depth-1,tot-2) t
    | Tblock_length (l,t) ->
      let hash = acc + 61 + hash_label l in
      hash_term (hash,depth-1,tot-2) t
    | Toffset (l,t) ->
      let hash = acc + 67 + hash_label l in
      hash_term (hash,depth-1,tot-2) t
    | Tnull -> acc+71, tot - 1
    | TUpdate(t1,off,t2) ->
      let hash1,tot1 = hash_term (acc+73,depth-1,tot-1) t1 in
      let hash2,tot2 = hash_toffset (hash1,depth-1,tot1) off in
      hash_term (hash2,depth-1,tot2) t2
    | Ttypeof t -> hash_term (acc+79,depth-1,tot-1) t
    | Ttype t -> acc + 83 + Typ.hash t, tot - 1
    | Tempty_set -> acc + 89, tot - 1
    | Tunion tl ->
      let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
      List.fold_left hash_one_term (acc+97,tot-1) tl
    | Tinter tl ->
      let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
      List.fold_left hash_one_term (acc+101,tot-1) tl
    | Tcomprehension (t,quants,p) ->
      let hash_var (acc,tot) lv =
        if tot = 0 then raise (StopRecursion acc)
        else (acc + Logic_var.hash lv,tot-1)
      in
      let (acc,tot) = List.fold_left hash_var (acc+103,tot-1) quants in
      let (acc,tot) = hash_term (acc,depth-1,tot-1) t in
      (match p with
       | None -> acc, tot - 1
       | Some p -> hash_predicate (acc, depth - 1, tot - 1) p)
    | Trange(t1,t2) ->
      let acc = acc + 107 in
      let acc,tot =
        match t1 with
          None -> acc,tot - 1
        | Some t -> hash_term (acc,depth-1,tot-2) t
      in
      if tot <= 0 then raise (StopRecursion acc)
      else
        (match t2 with
           None -> acc, tot - 1
         | Some t -> hash_term (acc,depth-1,tot-1) t)
    | Tlet(li,t) ->
      hash_term
        (acc + 109 + Hashtbl.hash li.l_var_info.lv_name, depth-1, tot-1)
        t
  end

and hash_tlval (acc,depth,tot) (h,o) =
  if tot <= 0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    let hash, tot = hash_tlhost (acc, depth - 1, tot - 1) h in
    hash_toffset (hash,depth-1,tot) o
  end

and hash_tlhost (acc,depth,tot) t =
  if tot <=0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    match t with
    | TVar v -> acc + 17 + Logic_var.hash v, tot - 1
    | TResult typ -> 31 + 7 * Typ.hash typ, tot - 2
    | TMem t -> hash_term (acc + 71, depth - 1, tot - 1) t
  end

and hash_toffset (acc, depth, tot) t =
  if depth <= 0 || tot <= 0 then raise (StopRecursion acc)
  else begin
    match t with
    | TNoOffset -> acc, tot - 1
    | TField(f,o) ->
      hash_toffset (acc+13+Fieldinfo.hash f, depth -1, tot - 1) o
    | TModel (mi, o) ->
      hash_toffset (acc+41+Model_info.hash mi, depth - 1, tot - 1) o
    | TIndex (t, o) ->
      let hash, tot = hash_term (acc+73, depth - 1, tot - 1) t in
      hash_toffset (hash, depth - 1, tot) o
  end

and hash_predicate (acc, depth, tot) p =
  if tot <= 0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    match p.pred_content with
    | Pfalse -> acc + 2, tot - 1
    | Ptrue -> acc + 3, tot - 1
    | Papp(li,labels,args) ->
      let hash1 = acc + 5 + Logic_info.hash li in
      let hash_lb (acc, tot) l =
        if tot = 0 then raise (StopRecursion acc)
        else (acc + hash_label l, tot - 1)
      in
      let hash_one_term (acc, tot) t = hash_term (acc, depth - 1, tot) t in
      let res = List.fold_left hash_lb (hash1, tot - 1) labels in
      List.fold_left hash_one_term res args
    | Prel(r,lt,rt) ->
      let hashr = acc + 7 + Hashtbl.hash r in
      let hashlt, totlt = hash_term (hashr, depth - 1, tot - 1) lt in
      hash_term (hashlt, depth - 1, totlt) rt
    | Pand(lp,rp) ->
      let hashlp, totlp = hash_predicate (acc + 11, depth - 1, tot - 1) lp in
      hash_predicate (hashlp, depth - 1, totlp) rp
    | Por(lp,rp) ->
      let hashlp, totlp = hash_predicate (acc + 13, depth - 1, tot - 1) lp in
      hash_predicate (hashlp, depth - 1, totlp) rp
    | Pxor (lp,rp) ->
      let hashlp, totlp = hash_predicate (acc + 17, depth - 1, tot - 1) lp in
      hash_predicate (hashlp, depth - 1, totlp) rp
    | Pimplies(lp,rp) ->
      let hashlp, totlp = hash_predicate (acc + 19, depth - 1, tot - 1) lp in
      hash_predicate (hashlp, depth - 1, totlp) rp
    | Piff(lp,rp) ->
      let hashlp, totlp = hash_predicate (acc + 23, depth - 1, tot - 1) lp in
      hash_predicate (hashlp, depth - 1, totlp) rp
    | Pnot p -> hash_predicate (acc + 29, depth - 1, tot - 1) p
    | Pif (c,t,e) ->
      let hashc, totc = hash_predicate (acc + 31, depth - 1, tot - 1) c in
      let hasht, tott = hash_predicate (hashc, depth - 1, totc) t in
      hash_predicate (hasht, depth - 1, tott) e
    | Plet (d,p) ->
      let hashli = acc + 37 + Logic_info.hash d in
      hash_predicate (hashli, depth - 1, tot - 1) p
    | Pforall(q,p) ->
      let hash_var (acc, tot) lv =
        if tot = 0 then raise (StopRecursion acc)
        else (acc + Logic_var.hash lv, tot - 1)
      in
      let (acc, tot) = List.fold_left hash_var (acc + 41, tot - 1) q in
      hash_predicate (acc, depth - 1, tot - 1) p
    | Pexists(q,p) ->
      let hash_var (acc, tot) lv =
        if tot <= 0 then raise (StopRecursion acc)
        else (acc + Logic_var.hash lv, tot - 1)
      in
      let (acc, tot) = List.fold_left hash_var (acc + 43, tot - 1) q in
      hash_predicate (acc, depth - 1, tot - 1) p
    | Pat(p,l) ->
      let hashp, totp = hash_predicate (acc + 47, depth - 1, tot - 1) p in
      hashp + hash_label l, totp - 1
    | Pallocable (l,t) ->
      let hashl = acc + 53 + hash_label l in
      hash_term (hashl, depth - 1, tot - 2) t
    | Pfreeable (l,t) ->
      let hashl = acc + 59 + hash_label l in
      hash_term (hashl, depth - 1, tot - 2) t
    | Pvalid (l,t) ->
      let hashl = acc + 61 + hash_label l in
      hash_term (hashl, depth - 1, tot - 2) t
    | Pvalid_read (l,t) ->
      let hashl = acc + 67 + hash_label l in
      hash_term (hashl, depth - 1, tot - 2) t
    | Pobject_pointer (l,t) ->
      let hashl = acc + 71 + hash_label l in
      hash_term (hashl, depth - 1, tot - 2) t
    | Pinitialized (l,t) ->
      let hashl = acc + 73 + hash_label l in
      hash_term (hashl, depth - 1, tot - 2) t
    | Pdangling (l,t) ->
      let hashl = acc + 79 + hash_label l in
      hash_term (hashl, depth - 1, tot - 2) t
    | Pvalid_function t ->
      hash_term (acc + 83, depth - 1, tot - 1) t
    | Pfresh (l,m,t,n) ->
      let hashl = acc + 89 + hash_label l in
      let hashm = hashl + hash_label m in
      let hasht, tott = hash_term (hashm, depth - 1, tot - 3) t in
      hash_term (hasht, depth - 1, tott) n
    | Pseparated(seps) ->
      let hash_one_term (acc, tot) t = hash_term (acc, depth - 1, tot) t in
      List.fold_left
        hash_one_term
        (acc + 97, tot - 1)
        seps
    | Paligned (t,n) ->
      let hasht, tott = hash_term (acc + 101 , depth - 1, tot - 3) t in
      hash_term (hasht, depth - 1, tott) n
  end

let hash_fct f t = try fst (f (0,10,100) t) with StopRecursion n -> n

module Logic_constant = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = LStr "@dummy_logic_constant@"

  include Make_with_collections
      (struct
        type t = logic_constant
        let name = "Logic_constant"
        let reprs = [ dummy ]
        let compare = compare_logic_constant
        let equal = Datatype.from_compare
        let hash = hash_logic_constant
        let copy = Datatype.undefined
        let pretty fmt lc = !pretty_ref fmt lc

      end)
end

module Term = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    term_node = TConst (LStr "@dummy_term@");
    term_loc  = Fileloc.unknown;
    term_type = Logic_type.dummy;
    term_name = []
  }

  include Make_with_collections
      (struct
        type t = term
        let name = "Term"
        let reprs =
          List.map
            (fun t ->
               { term_node = TConst (LStr "");
                 term_loc = Fileloc.unknown;
                 term_type =  t;
                 term_name = [] })
            Logic_type.reprs
        let compare = compare_term
        let equal = Datatype.from_compare
        let copy = Datatype.undefined
        let hash = hash_fct hash_term
        let pretty fmt t = !pretty_ref fmt t
      end)
end

module Identified_term = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = { it_id = -1; it_content = Term.dummy }

  include Make_with_collections
      (struct
        type t = identified_term
        let name = "Identified_term"
        let reprs =
          List.map (fun t -> { it_id = -1; it_content = t}) Term.reprs
        let compare x y = Extlib.compare_basic x.it_id y.it_id
        let equal x y = x.it_id = y.it_id
        let copy x =
          (* NB: Term.copy itself is undefined. *)
          { it_id = x.it_id; it_content = Term.copy x.it_content }
        let hash x = x.it_id
        let pretty fmt t = !pretty_ref fmt t
      end)
end

module Term_lhost = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = TVar Logic_var.dummy

  include Make_with_collections
      (struct
        type t = term_lhost
        let name = "Term_lhost"
        let reprs =
          List.fold_left
            (fun acc ty ->
               List.fold_left
                 (fun acc t -> TMem t :: acc)
                 (TResult ty :: acc)
                 Term.reprs)
            (List.map (fun lv -> TVar lv) Logic_var.reprs)
            Typ.reprs
        let compare = compare_tlhost
        let equal = Datatype.from_compare
        let hash = hash_fct hash_tlhost
        let copy = Datatype.undefined
        let pretty fmt h = !pretty_ref fmt h
      end)
end

module Term_offset = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = TNoOffset

  include Make_with_collections
      (struct
        type t = term_offset
        let name = "Term_offset"
        let reprs = [ dummy ]
        let compare = compare_toffset
        let equal = Datatype.from_compare
        let hash = hash_fct hash_toffset
        let copy = Datatype.undefined
        let pretty fmt t_o = !pretty_ref fmt t_o
      end)
end

module Term_lval = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = Term_lhost.dummy, Term_offset.dummy
  include Datatype.Pair_with_collections
      (Term_lhost)
      (Term_offset)
  let pretty fmt t = !pretty_ref fmt t
end

module Logic_label = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = StmtLabel (ref Stmt.dummy)

  include Make_with_collections
      (struct
        type t = logic_label
        let name = "Logic_label"
        let reprs =
          (BuiltinLabel Pre)
          :: List.map (fun x -> StmtLabel (ref x)) Stmt.reprs
        let compare = compare_logic_label
        let equal = Datatype.from_compare
        let copy = Datatype.undefined
        let hash = hash_label
        let pretty fmt l = !pretty_ref fmt l
      end)
end

module Logic_real = struct
  let dummy = {
    r_literal = "@dummy_logic_real@";
    r_nearest = 0.0;
    r_lower   = 0.0;
    r_upper   = 0.0
  }

  include Make_with_collections
      (struct
        type t = logic_real
        let name = "Logic_real"
        let reprs = [ dummy ]
        let compare = compare_logic_real
        let hash r =
          let fhash = Datatype.Float.hash in
          fhash r.r_lower + 3 * fhash r.r_nearest + 7 * fhash r.r_upper +
          11 * Datatype.String.hash r.r_literal
        let equal r1 r2 = compare r1 r2 = 0
        let copy = Datatype.undefined
        let pretty = Datatype.undefined
      end)
end

module Global_annotation = struct
  let pretty_ref = ref (fun _ -> assert false)
  let dummy = Dinvariant (Logic_info.dummy, Fileloc.unknown)

  include Make_with_collections
      (struct
        type t = global_annotation
        let name = "Global_annotation"
        let reprs = List.map (fun l -> Daxiomatic ("", [],[], l)) Fileloc.reprs
        let pretty fmt v = !pretty_ref fmt v

        let rec compare g1 g2 =
          match g1,g2 with
          | Dfun_or_pred (l1,_), Dfun_or_pred(l2,_) -> Logic_info.compare l1 l2
          | Dfun_or_pred _,_ -> -1
          | _, Dfun_or_pred _ -> 1
          | Dvolatile (it1,_,_,attr1,_), Dvolatile(it2,_,_,attr2,_) ->
            let res = compare_list Identified_term.compare it1 it2 in
            if res = 0 then Attributes.compare attr1 attr2 else res
          | Dvolatile _,_ -> -1
          | _,Dvolatile _ -> 1
          | Daxiomatic (_,g1,attr1,_), Daxiomatic (_,g2,attr2,_) ->
            (* ACSL does not require the name to be unique. *)
            let res = compare_list compare g1 g2 in
            if res = 0 then Attributes.compare attr1 attr2 else res
          | Daxiomatic _, _ -> -1
          | _, Daxiomatic _ -> 1
          | Dmodule (m1,g1,attr1,loader1,_), Dmodule (m2,g2,attr2,loader2,_) ->
            let res = String.compare m1 m2 in
            if res = 0 then
              let res = compare_list compare g1 g2 in
              if res = 0 then
                let res =  Stdlib.compare loader1 loader2 in
                if res = 0 then Attributes.compare attr1 attr2
                else res
              else res
            else res
          | Dmodule _, _ -> -1
          | _, Dmodule _ -> 1
          | Dtype(t1,_), Dtype(t2,_) -> Logic_type_info.compare t1 t2
          | Dtype _, _ -> -1
          | _, Dtype _ -> 1
          | Dlemma (l1,_,_,_,attr1,_), Dlemma(l2,_,_,_,attr2,_) ->
            let res = Datatype.String.compare l1 l2 in
            if res = 0 then Attributes.compare attr1 attr2 else res
          | Dlemma _, _ -> -1
          | _, Dlemma _ -> 1
          | Dinvariant (l1,_), Dinvariant (l2,_) -> Logic_info.compare l1 l2
          | Dinvariant _, _ -> -1
          | _, Dinvariant _ -> 1
          | Dtype_annot(l1, _), Dtype_annot (l2, _) -> Logic_info.compare l1 l2
          | Dtype_annot _, _ -> -1
          | _, Dtype_annot _ -> 1
          | Dmodel_annot(l1,_), Dmodel_annot(l2,_) -> Model_info.compare l1 l2
          | Dmodel_annot _, _ -> -1
          | _, Dmodel_annot _ -> 1
          | Dextended (ext1, _, _), Dextended (ext2, _, _) ->
            Datatype.Int.compare ext1.ext_id ext2.ext_id

        let equal = Datatype.from_compare

        let rec hash g = match g with
          | Dfun_or_pred (l,_) -> 2 * Logic_info.hash l
          | Dvolatile ([],_,_,_,(_source,_)) ->
            Kernel_log.fatal
              "Empty location list for volatile annotation@."
          | Dvolatile (t::_,_,_,_,_) -> 3 * Identified_term.hash t
          | Daxiomatic (_,[],_,_) -> 5
          (* Empty axiomatic is weird but authorized. *)
          | Daxiomatic (_,g::_,_,_) -> 5 * hash g
          | Dmodule (m,_,_,_,_) -> 5 * Datatype.String.hash m
          | Dtype (t,_) -> 7 * Logic_type_info.hash t
          | Dlemma(n,_,_,_,_,_) -> 11 * Datatype.String.hash n
          | Dinvariant(l,_) -> 13 * Logic_info.hash l
          | Dtype_annot(l,_) -> 17 * Logic_info.hash l
          | Dmodel_annot(l,_) -> 19 * Model_info.hash l
          | Dextended ({ext_id},_,_) -> 29 * Datatype.Int.hash ext_id

        let copy = Datatype.undefined
      end)

  let loc = function
    | Dfun_or_pred(_, loc)
    | Daxiomatic(_, _, _, loc)
    | Dmodule(_, _, _, _, loc)
    | Dtype (_, loc)
    | Dlemma(_, _, _, _, _, loc)
    | Dinvariant(_, loc)
    | Dtype_annot(_, loc) -> loc
    | Dmodel_annot(_, loc) -> loc
    | Dvolatile(_, _, _, _,loc) -> loc
    | Dextended(_,_,loc) -> loc

  let attr = function
    | Dfun_or_pred({ l_var_info = { lv_attr }}, _) -> lv_attr
    | Daxiomatic(_, _, attr, _) | Dmodule(_, _, attr, _, _) -> attr
    | Dtype ({lt_attr}, _) -> lt_attr
    | Dlemma(_, _, _, _, attr, _) -> attr
    | Dinvariant({ l_var_info = { lv_attr }}, _) -> lv_attr
    | Dtype_annot({ l_var_info = { lv_attr}}, _) -> lv_attr
    | Dmodel_annot({ mi_attr }, _) -> mi_attr
    | Dvolatile(_, _, _, attr, _) -> attr
    | Dextended (_,attr,_) -> attr
end

module Global = struct
  let pretty_ref = ref (fun _ -> assert false)
  let dummy = GText "@dummy_global@"

  include Make_with_collections
      (struct
        type t = global
        let name = "Global"
        let reprs = [ dummy ]
        let pretty fmt v = !pretty_ref fmt v

        let compare g1 g2 =
          match g1, g2 with
          | GType (t1,_), GType (t2,_) -> Typeinfo.compare t1 t2
          | GType _,_ -> -1
          | _, GType _ -> 1
          | GCompTag (t1,_), GCompTag(t2,_) -> Compinfo.compare t1 t2
          | GCompTag _,_ -> -1
          | _, GCompTag _ -> 1
          | GCompTagDecl (t1,_), GCompTagDecl(t2,_) -> Compinfo.compare t1 t2
          | GCompTagDecl _,_ -> -1
          | _,GCompTagDecl _ -> 1
          | GEnumTag(t1,_), GEnumTag(t2,_) -> Enuminfo.compare t1 t2
          | GEnumTag _,_ -> -1
          | _, GEnumTag _ -> 1
          | GEnumTagDecl(t1,_), GEnumTagDecl(t2,_) -> Enuminfo.compare t1 t2
          | GEnumTagDecl _, _ -> -1
          | _, GEnumTagDecl _ -> 1
          | GVarDecl (v1,_), GVarDecl(v2,_) -> Varinfo.compare v1 v2
          | GVarDecl _,_ -> -1
          | _,GVarDecl _ -> 1
          | GFunDecl (_,v1,_), GFunDecl(_,v2,_) -> Varinfo.compare v1 v2
          | GFunDecl _,_ -> -1
          | _,GFunDecl _ -> 1
          | GVar (v1,_,_), GVar (v2,_,_) -> Varinfo.compare v1 v2
          | GVar _,_ -> -1
          | _, GVar _ -> 1
          | GFun(f1,_), GFun(f2,_) -> Varinfo.compare f1.svar f2.svar
          | GFun _, _ -> -1
          | _, GFun _ -> 1
          | GAsm (_,l1), GAsm(_,l2) -> Fileloc.compare l1 l2
          | GAsm _, _ -> -1
          | _, GAsm _ -> 1
          | GPragma(_,l1), GPragma(_,l2) -> Fileloc.compare l1 l2
          | GPragma _, _ -> -1
          | _, GPragma _ -> 1
          | GText s1, GText s2 -> Datatype.String.compare s1 s2
          | GText _, _ -> -1
          | _, GText _ -> 1
          | GAnnot (g1,_), GAnnot(g2,_) -> Global_annotation.compare g1 g2

        let equal = Datatype.from_compare

        let hash g = match g with
            GType (t,_) -> 2 * Typeinfo.hash t
          | GCompTag (t,_) -> 3 * Compinfo.hash t
          | GCompTagDecl (t,_) -> 5 * Compinfo.hash t
          | GEnumTag (t,_) -> 7 * Enuminfo.hash t
          | GEnumTagDecl(t,_) -> 11 * Enuminfo.hash t
          | GVarDecl (v,_) -> 13 * Varinfo.hash v
          | GVar (v,_,_) -> 17 * Varinfo.hash v
          | GFun (f,_) -> 19 * Varinfo.hash f.svar
          | GAsm (_,l) -> 23 * Fileloc.hash l
          | GText t -> 29 * Datatype.String.hash t
          | GAnnot (g,_) -> 31 * Global_annotation.hash g
          | GPragma(_,l) -> 37 * Fileloc.hash l
          | GFunDecl (_,v,_) -> 43 * Varinfo.hash v

        let copy = Datatype.undefined
      end)

  let loc = function
    | GFun(_, l)
    | GType(_, l)
    | GEnumTag(_, l)
    | GEnumTagDecl(_, l)
    | GCompTag(_, l)
    | GCompTagDecl(_, l)
    | GVarDecl( _, l)
    | GFunDecl(_, _, l)
    | GVar(_, _, l)
    | GAsm(_, l)
    | GPragma(_, l)
    | GAnnot (_, l) -> l
    | GText _ -> Fileloc.unknown

  let attr = function
    | GVar (vi,_,_) | GFun ({svar = vi},_) | GVarDecl (vi,_) | GFunDecl (_,vi,_)->
      vi.vattr
    | GType (t,_) -> t.ttype.tattr
    | GCompTag(ci,_) | GCompTagDecl(ci,_) -> ci.cattr
    | GEnumTag(ei,_) | GEnumTagDecl(ei,_) -> ei.eattr
    | GAnnot (g,_) -> Global_annotation.attr g
    | _ -> []

end

module Code_annotation = struct

  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = { annot_content = AAssigns([], WritesAny); annot_id = -1 }

  include Make_with_collections
      (struct
        type t = code_annotation
        let name = "Code_annotation"
        let reprs = [ dummy ]
        let hash x = x.annot_id
        let equal x y = x.annot_id = y.annot_id
        let compare x y = Datatype.Int.compare x.annot_id y.annot_id
        let copy = Datatype.undefined
        let pretty fmt ca = !pretty_ref fmt ca
      end)

  let loc ca = match ca.annot_content with
    | AAssert(_,{ tp_statement = {pred_loc=loc}})
    | AInvariant(_,_,{tp_statement = {pred_loc=loc}})
    | AVariant({term_loc=loc},_) -> Some loc
    | AAssigns _ | AAllocation _ | AExtended _
    | AStmtSpec _ -> None

end

module Predicate = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    pred_name    = [ "@dummy_predicate@" ];
    pred_loc     = Fileloc.unknown;
    pred_content = Pfalse
  }

  include Make
      (struct
        type t = predicate
        let name = "Predicate"
        let reprs = [ dummy ]
        let pretty fmt x = !pretty_ref fmt x
      end)
end

module Toplevel_predicate = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = { tp_statement = Predicate.dummy; tp_kind = Assert }

  include Make
      (struct
        type t = toplevel_predicate
        let name = "Toplevel_predicate"
        let reprs = [ dummy ]
        let pretty fmt x = !pretty_ref fmt x
      end)
end

module Identified_predicate = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = { ip_content = Toplevel_predicate.dummy; ip_id = -1 }

  include Make_with_collections
      (struct
        type t = identified_predicate
        let name = "Identified_predicate"
        let reprs = [ dummy ]
        let compare x y = Extlib.compare_basic x.ip_id y.ip_id
        let equal x y = x.ip_id = y.ip_id
        let copy = Datatype.undefined
        let hash x = x.ip_id
        let pretty fmt x = !pretty_ref fmt x
      end)
end

module PredicateStructEq = struct
  let dummy = Predicate.dummy

  include Make_with_collections
      (struct
        type t = predicate
        let name = "PredicateStructEq"
        let reprs = [ dummy ]
        let compare = compare_predicate
        let equal = Datatype.from_compare
        let copy = Datatype.undefined
        let hash = hash_fct hash_predicate
        let pretty = Datatype.undefined
      end)
end

module Funbehavior = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    b_name       = "@dummy_behavior@";
    b_requires   = [];
    b_assumes    = [];
    b_post_cond  = [];
    b_assigns    = WritesAny;
    b_allocation = FreeAllocAny;
    b_extended   = [];
  }

  include Make
      (struct
        include Datatype.Serializable_undefined
        type t = funbehavior
        let name = "Funbehavior"
        let reprs =
          [ {  b_name = "default!"; (* Cil.default_behavior_name *)
               b_requires = Identified_predicate.reprs;
               b_assumes = Identified_predicate.reprs;
               b_post_cond =
                 List.map (fun x -> Normal, x) Identified_predicate.reprs;
               b_assigns = WritesAny;
               b_allocation = FreeAllocAny;
               b_extended = []; } ]
        let pretty fmt x = !pretty_ref fmt x
      end)
end

module Funspec = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    spec_behavior   = [];
    spec_variant    = None;
    spec_terminates = None;
    spec_complete_behaviors = [];
    spec_disjoint_behaviors = [];
  }

  include Make
      (struct
        include Datatype.Serializable_undefined
        type t = funspec
        let name = "Funspec"
        let reprs =
          [ { spec_behavior = Funbehavior.reprs;
              spec_variant = None;
              spec_terminates = None;
              spec_complete_behaviors = [];
              spec_disjoint_behaviors = [] } ]
        let pretty fmt x = !pretty_ref fmt x
      end)
end

module Fundec = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = {
    svar     = Varinfo.dummy;
    sformals = [];
    slocals  = [];
    smaxid   = 0;
    sbody    = Block.dummy;
    smaxstmtid = None;
    sallstmts  = [];
    sspec    = Funspec.dummy;
  }

  let make_reprs vi fs = {
    svar = vi;
    sformals = [];
    slocals = [];
    smaxid = 0;
    sbody = List.hd (Block.reprs);
    smaxstmtid = None;
    sallstmts = [];
    sspec = fs ;
  }

  let reprs =
    List.fold_left (fun acc vi ->
        List.fold_left (fun acc fs ->
            make_reprs vi fs :: acc
          ) acc Funspec.reprs
      ) [] Varinfo.reprs

  include Make_with_collections
      (struct
        type t = fundec
        let name = "Fundec"
        let reprs = reprs
        let compare v1 v2 = Datatype.Int.compare v1.svar.vid v2.svar.vid
        let hash v = v.svar.vid
        let equal v1 v2 = v1.svar.vid = v2.svar.vid
        let copy = Datatype.undefined
        let pretty fmt f = !pretty_ref fmt f
      end)
end

module Kf = struct

  let vi kf = match kf.fundec with
    | Definition (d, _) -> d.svar
    | Declaration (_,vi,_, _) -> vi

  let id kf = (vi kf).vid

  let dummy = {
    fundec = Definition (Fundec.dummy, Fileloc.unknown);
    spec = Funspec.dummy;
  }

  let set_formal_decls = ref (fun _ _ -> assert false)

  include Datatype.Make_with_collections
      (struct
        type t = kernel_function
        let name = "Cil_datatype.Kf"
        let structural_descr = Structural_descr.t_abstract
        let reprs =
          let empty_spec =
            { spec_behavior = [];
              spec_variant = None;
              spec_terminates = None;
              spec_complete_behaviors = [];
              spec_disjoint_behaviors = [] }
          in
          List.fold_left
            (fun acc loc ->
               List.fold_left
                 (fun acc b ->
                    List.fold_left
                      (fun acc vi ->
                         { fundec =
                             Definition
                               ({ svar  = vi;
                                  smaxid = 0;
                                  slocals = [];
                                  sformals = [];
                                  sbody = b;
                                  smaxstmtid = None;
                                  sallstmts = [];
                                  sspec = empty_spec },
                                loc);
                           spec = empty_spec } :: acc)
                      acc
                      Varinfo.reprs)
                 acc
                 Block.reprs)
            []
            Fileloc.reprs
        let compare k1 k2 = Datatype.Int.compare (id k1) (id k2)
        let equal k1 k2 =
          if k1 != k2 then begin
            if id k1 = id k2 then begin
              Kernel_log.fatal "Two kf for %a (%d) and %a (%d) (%d)@."
                Varinfo.pretty (vi k1) (Extlib.address_of_value k1)
                Varinfo.pretty (vi k2) (Extlib.address_of_value k2)
                (id k1)
            end;
            false
          end else true
        let hash = id
        let copy = Datatype.undefined
        let rehash x = match x.fundec with
          | Definition _ | Declaration (_, _, None, _)-> x
          | Declaration (_, v, Some args, _) ->
            !set_formal_decls v args;
            x
        let pretty fmt kf = Varinfo.pretty fmt (vi kf)
        let mem_project = Datatype.never_any_project
      end)

end

(**************************************************************************)
(** {3 Logic_ptree}
    Sorted by alphabetic order. *)
(**************************************************************************)

module Lexpr = struct
  let dummy = Logic_ptree.{
      lexpr_node = PLvar "@dummy_lexpr@";
      lexpr_loc = Fileloc.unknown;
    }

  include Make (struct
      open Logic_ptree
      type t = lexpr
      let name = "Lexpr"
      let reprs = [ dummy ]
      let pretty = Datatype.undefined (* TODO *)
    end)
end

(**************************************************************************)
(** {3 Other types} *)
(**************************************************************************)

module Syntactic_scope = struct
  let dummy = Program

  include Make_with_collections
      (struct
        include Datatype.Serializable_undefined
        type t = syntactic_scope
        let name = "Syntactic_scope"
        let reprs = [ dummy ]
        let compare s1 s2 =
          match s1, s2 with
          | Global, Global -> 0
          | Global, _ -> 1
          | _, Global -> -1
          | Program, Program -> 0
          | Program, _ -> 1
          | _, Program -> -1
          | Translation_unit s1, Translation_unit s2 ->
            Filepath.compare s1 s2
          | Translation_unit _, _ -> 1
          | _, Translation_unit _ -> -1
          | Formal kf1, Formal kf2 -> Kf.compare kf1 kf2
          | Formal _, _ -> 1
          | _, Formal _ -> -1
          | Whole_function kf1, Whole_function kf2 -> Kf.compare kf1 kf2
          | Whole_function _, _ -> 1
          | _, Whole_function _ -> -1
          | Block_scope s1, Block_scope s2 -> Stmt_Id.compare s1 s2
        let equal = Datatype.from_compare
        let hash s =
          match s with
          | Global -> 3
          | Program -> 5
          | Translation_unit s -> 7 * Filepath.hash s + 11
          | Block_scope s -> 13 * Stmt_Id.hash s  + 17
          | Whole_function kf -> 19 * Kf.hash kf + 23
          | Formal kf -> 29 * Kf.hash kf + 31
        let pretty fmt = function
          | Global | Program -> Format.pp_print_string fmt "<Whole Program>"
          | Translation_unit s ->
            Format.fprintf fmt "File %a" Filepath.pretty s
          | Formal kf ->
            Format.fprintf fmt "Parameter of %a" Kf.pretty kf
          | Whole_function kf ->
            Format.fprintf fmt "Local variable of %a" Kf.pretty kf
          | Block_scope s ->
            Format.fprintf fmt "Statement at %a:@\n@[%a@]"
              Fileloc.pretty (Stmt.loc s) Stmt.pretty s
      end)
end

(* -------------------------------------------------------------------------- *)
(* --- Internal                                                           --- *)
(* -------------------------------------------------------------------------- *)

let clear_caches () = List.iter (fun f -> f ()) !clear_caches
