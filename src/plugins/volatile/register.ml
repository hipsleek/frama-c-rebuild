(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)

(*  Tailrec List.map *)
let tailrec_list_map f l = List.rev (List.rev_map f l)

module FILE = File
open Cil_types
open Cil_datatype
open Options.Keys

module StringTbl = Datatype.String.Hashtbl

let dkey_binding =
  Options.register_category
    ~help:"Prints debug messages related to volatile operations"
    "binding"

let dkey_binding_table =
  Options.register_category
    ~help:"Prints debug messages related to volatile operations on internal tables"
    "binding-table"

let dkey_volatile_table =
  Options.register_category
    ~help:"Prints Volatile internal tables"
    "volatile-table"

let dkey_transformation_action =
  Options.register_category
    ~help:"Prints information on generated code"
    "transformation-action"

let dkey_transformation_visit =
  Options.register_category
    ~help:"Prints visitor information during the transformation"
    "transformation-visit"

let has_volatile_attr t =
  Ast_types.get_attributes t |> Ast_attributes.contains "volatile"
let add_volatile_attr = Ast_types.add_attributes [ ("volatile", []) ]

(* This function replaces spaces in type names.
   Note: A previous version also made sure all characters were either a-z, A-Z or
   0-9 (or raised Not_found), it does not seem to be necessary so it was removed
   to simplify the code, but maybe it'll break something (?).
*)
let typename (t: typ) =
  let typename = Pretty_utils.to_string Typ.pretty t in
  String.map (function ' ' -> '_' | c -> c) typename

(* -------------------------------------------------------------------------- *)
(* --- Global Volatile Annotation tables                                  --- *)
(* -------------------------------------------------------------------------- *)

(* normalized l-path
   `p+idx` is normalized to `p` and by the way `p[idx]` is normalised to `*p`.

   Due to CIL decomposition, it is impossible to find the final volatile lvalue
   when the path to that volatile lvalue contents a volatile pointer.
   In a simplest way, volatile of volatile are not handle by volatile clauses.

   From that limitation and type constraints, there is no needs to distinguish
   `*p` from `p` (only one of these lvalue is volatile).
*)
module L_PATH =
struct

  exception Unsupported
  type t =
    | Lram   of Z.t     (* absolute addr *)
    | Lvar   of varinfo       (* [x] *)
    | Lfield of t * fieldinfo (* [e.f] *)

  let rec pretty fmt = function
    | Lvar x -> Varinfo.pretty fmt x
    | Lram p -> Format.fprintf fmt "&%s" (Z.to_string p)
    | Lfield (e, fd) ->
      Format.fprintf fmt "%a:%a" pretty e Fieldinfo.pretty fd

  let rec compare a b =
    match a, b with
    | Lram p , Lram q -> Z.compare p q
    | Lram _ , _ -> -1
    | _ , Lram _ -> 1
    | Lvar x , Lvar y -> Varinfo.compare x y
    | Lvar _ , _ -> -1
    | _ , Lvar _ -> 1
    | Lfield (x, f) , Lfield (y, g) ->
      let cmp = Fieldinfo.compare f g in
      if cmp <> 0 then cmp else compare x y

  let rec of_expr e =
    match e.enode with
    | Lval lv -> of_lval lv
    | AddrOf lv | StartOf lv -> of_lval lv
    | BinOp ((PlusPI | MinusPI), e, _, _)
    | CastE (_, e) -> of_expr e
    | _ ->
      match Cil.constFoldToInt e with
      | Some p -> Lram p
      | None -> raise Unsupported

  and of_lval (host, offset) = of_offset (of_host host) offset

  and of_host = function
    | Var x -> Lvar x
    | Mem e -> of_expr e

  and of_offset p = function
    | NoOffset -> p
    | Index (_, ofs) -> of_offset p ofs
    | Field (fd, ofs) -> of_offset (Lfield (p, fd)) ofs

  let rec to_const t =
    match t.term_node with
    | TCast (false, _, e) -> to_const e
    | Tnull -> Z.zero
    | TConst (Integer (i, _)) -> i
    | TBinOp (PlusA, a, b) -> Z.add (to_const a) (to_const b)
    | _ -> raise Unsupported

  let rec of_term t =
    match t.term_node with
    | TLval lv -> of_tlval lv
    | TBinOp ((PlusPI | MinusPI), e, _)
    | TCast (_, _, e) | Tat (e, _) -> of_term e
    | _ -> Lram (to_const t)

  and of_tlval (host, offset) = of_toffset (of_thost host) offset

  and of_thost = function
    | TVar { lv_origin = Some x } -> Lvar x
    | TMem e -> of_term e
    | _ -> raise Unsupported

  and of_toffset p = function
    | TNoOffset -> p
    | TIndex (_, ofs) -> of_toffset p ofs
    | TField (fd, ofs) -> of_toffset (Lfield (p, fd)) ofs
    | TModel _ -> raise Unsupported

end

module L_MAP = Map.Make (L_PATH)

(* -------------------------------------------------------------------------- *)
(* --- Automatic binding of volatile accesses                             --- *)
(* -------------------------------------------------------------------------- *)

(* Table management for [-volatile-binding-auto] option *)
module BA_TBL = struct

  let get_tbl_access ~is_wr_access kf_tbl =
    if is_wr_access then snd kf_tbl else fst kf_tbl

  (** Looking for a kernel function name starting with prefix^("Wr_"|"Rd_") *)
  let filter_kf_name kf_name =
    let prefix = Options.BindingPrefix.get () in
    let rd_prefix = prefix ^ "Rd_" in
    let wr_prefix = prefix ^ "Wr_" in
    if String.starts_with ~prefix:rd_prefix kf_name then Some false
    else if String.starts_with ~prefix:wr_prefix kf_name then Some true
    else None

  let filter_kf_prototype ~is_wr_access fct =
    (* Verifying the prototype within the kind of access. *)
    let ty = fct.vtype in
    let ret_type, args, is_varg_arg, _attrib = Cil.splitFunctionType ty in
    let volatile_ret_type = add_volatile_attr ret_type in
    Options.debug ~level:2 ~dkey:dkey_binding
      "Verifying prototype of function %s: %a@."
      fct.vorig_name Typ.pretty ty;
    let not_void_or_varg = not (Ast_types.is_void ret_type || is_varg_arg) in
    match is_wr_access, args with
    | false, Some [_, arg1, _] when
        not_void_or_varg
        && Ast_types.is_ptr arg1
        && Typ.equal (Ast_types.direct_pointed_type arg1) volatile_ret_type
      -> true (* matching prototype: T fct (volatile T *arg1) *)
    | false, Some [_, arg1, _] when
        not_void_or_varg
        && Ast_types.is_ptr arg1
        && Typ.equal (Ast_types.direct_pointed_type arg1) ret_type
        && Ast_types.is_volatile ret_type
      -> true (* matching prototype: T fct (T *arg1) when T has some volatile attr. *)
    | true, Some ((_, arg1, _) :: [_, arg2, _]) when
        not_void_or_varg
        && Ast_types.is_ptr arg1
        && Typ.equal arg2 ret_type
        && Typ.equal (Ast_types.direct_pointed_type arg1) volatile_ret_type
      -> true (* matching prototype: T fct (volatile T *arg1, T arg2) *)
    | true, Some ((_, arg1, _) :: [_, arg2, _]) when
        not_void_or_varg
        && Ast_types.is_ptr arg1
        && Typ.equal arg2 ret_type
        && Typ.equal (Ast_types.direct_pointed_type arg1) ret_type
        && Ast_types.is_volatile ret_type
      -> true (* matching prototype: T fct (T *arg1, T arg2) when T has some volatile attr.  *)
    | _, _ ->
      Options.debug ~level:2 ~dkey:dkey_binding
        "Invalid prototype of function %s@."
        fct.vorig_name;
      false

  let build_kf_table kf_tbl =
    match !kf_tbl with
    | Some kf_tbl -> kf_tbl
    | None ->
      let kf_tbl =
        let tbl_rd = StringTbl.create 40 in
        let tbl_wr = StringTbl.create 40 in
        let tbl_rd_wr = tbl_rd, tbl_wr in
        kf_tbl := Some tbl_rd_wr;
        tbl_rd_wr
      in
      let may_add_vi ~is_wr_access kf_tbl kf_name vi_kf =
        if filter_kf_prototype ~is_wr_access vi_kf then begin
          Options.debug ~level:2 ~dkey:dkey_binding_table
            "Adding function into the default binding table: %s@." kf_name;
          StringTbl.add (get_tbl_access ~is_wr_access kf_tbl) kf_name vi_kf
        end
      in
      let may_add_kf kf =
        let vi_kf = Kernel_function.get_vi kf in
        let kf_name = vi_kf.vorig_name in
        match filter_kf_name kf_name with
        | None -> ()
        | Some is_wr_access ->
          may_add_vi ~is_wr_access kf_tbl kf_name vi_kf;
      in
      if Options.BindingAuto.get () then
        (Options.feedback ~level:2 "Building default binding table...@.";
         Globals.Functions.iter may_add_kf);
      kf_tbl

  let clear_kf_table kf_tbl =
    match !kf_tbl with
    | None -> ()
    | Some (rd_tbl, wr_tbl) ->
      StringTbl.clear rd_tbl;
      StringTbl.clear wr_tbl

end

(* -------------------------------------------------------------------------- *)
(* --- Specific binding of volatile accesses                              --- *)
(* -------------------------------------------------------------------------- *)

module T_MAP =
struct

  module M = Typ.Map

  let empty = M.empty

  let basetype t =
    let t = Ast_types.unroll_deep t in
    if Options.Base.get () then
      let rec base t' =
        match t'.tnode with
        | TInt _ | TFloat _ when t'.tattr = [] -> t'
        | TInt i -> Cil_const.mk_tint i
        | TFloat f -> Cil_const.mk_tfloat f
        | TEnum e -> Cil_const.mk_tint e.ekind
        | TPtr bt -> Cil_const.mk_tptr (base bt)
        | _ -> Ast_types.remove_attributes_for_c_cast t'
      in base t
    else t

  let add t d m = M.add (basetype t) d m
  let find_opt t m = M.find_opt (basetype t) m

end

(* Table management for [-volatile-binding] option *)
module B_MAP = struct

  let checks_prototype_kind fct =
    (* Verifying the prototype within the kind of access. *)
    let ty = fct.vtype in
    let ret_type, args, is_varg_arg, _attrib = Cil.splitFunctionType ty in
    let volatile_ret_type = add_volatile_attr ret_type in
    Options.debug ~level:2 ~dkey:dkey_binding
      "Verifying prototype of function %s: %a@."
      fct.vorig_name Typ.pretty ty;
    let result is_wr_access arg1 =
      Some (is_wr_access, (Ast_types.direct_pointed_type arg1))
    in
    match args with
    | Some [_, arg1, _] when
        (not (Ast_types.is_void ret_type || is_varg_arg))
        && Ast_types.is_ptr arg1
        && Typ.equal (Ast_types.direct_pointed_type arg1) volatile_ret_type
      -> result false arg1 (* matching prototype: T fct (volatile T *arg1) *)
    | Some [_, arg1, _] when
        (not (Ast_types.is_void ret_type || is_varg_arg))
        && Ast_types.is_ptr arg1
        && Typ.equal (Ast_types.direct_pointed_type arg1) ret_type
        && Ast_types.is_volatile ret_type
      -> result false arg1 (* matching prototype: T fct (T *arg1) when T has some volatile attr*)
    | Some ((_, arg1, _) :: [_, arg2, _]) when
        (not (Ast_types.is_void ret_type || is_varg_arg))
        && Ast_types.is_ptr arg1
        && Typ.equal arg2 ret_type
        && Typ.equal (Ast_types.direct_pointed_type arg1) volatile_ret_type
      -> result true arg1 (* matching prototype: T fct (volatile T *arg1, T arg2) *)
    | Some ((_, arg1, _) :: [_, arg2, _]) when
        (not (Ast_types.is_void ret_type || is_varg_arg))
        && Ast_types.is_ptr arg1
        && Typ.equal arg2 ret_type
        && Typ.equal (Ast_types.direct_pointed_type arg1) ret_type
        && Ast_types.is_volatile ret_type
      -> result true arg1 (* matching prototype: T fct (T *arg1, T arg2) when T has some volatile attr *)
    | _ -> Options.warning ~once:true ~wkey:wkey_invalid_binding_function
             "Binding function '%s' has an invalid prototype"
             fct.vorig_name;
      None

  let build_binding_map () =
    Kf.Set.fold (fun kf ((map_rd, map_wr) as maps) ->
        let vf = Kernel_function.get_vi kf in
        match checks_prototype_kind vf with
        | None -> maps
        | Some (is_wr_access, volatile_object) ->
          let map = if is_wr_access then map_wr else map_rd in
          let map =
            match T_MAP.find_opt volatile_object map with
            | None -> Some (T_MAP.add volatile_object vf map)
            | Some vf0 ->
              Options.warning ~once:true ~wkey:wkey_invalid_binding_function
                "Functions -volatile-binding '%s' and '%s' %s"
                vf0.vorig_name vf.vorig_name
                (if Options.Base.get ()
                 then "apply to the same base type"
                 else "has same signature");
              None
          in
          match map with
          | None -> maps
          | Some map ->
            Options.feedback
              "Register binding function '%s' for '%s' accesses to type '%a'"
              vf.vorig_name
              (if is_wr_access then "write" else "read")
              Typ.pretty (T_MAP.basetype volatile_object);
            if is_wr_access then (map_rd, map) else (map, map_wr)
      )
      (Options.Binding.get ())
      (T_MAP.empty, T_MAP.empty)

  let find_binding ~is_wr_access map typ =
    T_MAP.find_opt typ (if is_wr_access then snd map else fst map)

end

(* -------------------------------------------------------------------------- *)
(* --- Pointer Call Annotations                                           --- *)
(* -------------------------------------------------------------------------- *)

module SIG =
struct
  module CT = Wp.Ctypes

  type t = CT.c_object option * CT.c_object list * bool

  let pretty fmt (r, ts, va) =
    Format.fprintf fmt "@[<hov 2>%a(" CT.pretty r;
    Pretty_utils.pp_list ~sep:",@ " CT.pretty fmt ts;
    if va then Format.fprintf fmt ",@,...";
    Format.fprintf fmt ")@]"
  [@@warning "-32"]

  let of_return r =
    if Ast_types.is_void r then None else Some (CT.object_of r)

  let of_type t : t =
    let r, args, va, _ = Cil.splitFunctionType t in
    of_return r,
    List.map (fun (_, ty, _) -> CT.object_of ty) (Cil.argsToList args), va

  let of_vi vi = of_type vi.vtype

  let of_kf kf = of_vi (Kernel_function.get_vi kf)
  [@@warning "-32"]

  let rec compare_list xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _ -> (-1)
    | _, [] -> 1
    | p :: ps, q :: qs ->
      let cmp = CT.compare p q in
      if cmp <> 0 then cmp else compare_list ps qs

  let compare_option x y =
    match x, y with
    | None, None -> 0
    | None, Some _ -> (-1)
    | Some _, None -> 1
    | Some p , Some q -> CT.compare p q

  let compare (r1, p1, v1) (r2, p2, v2) =
    match v1, v2 with
    | true , false -> (-1)
    | false , true -> 1
    | true , true | false, false ->
      let cmp = compare_option r1 r2 in
      if cmp<>0 then cmp else compare_list p1 p2

  let stub vf =
    if Ast_types.is_fun vf.vtype then
      let r, args, va, _ = Cil.splitFunctionTypeVI vf in
      match Cil.argsToList args with
      | (_, tf, _) :: ps ->
        let r = of_return r in
        let ts = List.map (fun (_, ty, _) -> CT.object_of ty) ps in
        let sp = of_type (Ast_types.direct_pointed_type tf) in
        let sf = (r, ts, va) in
        if compare sp sf <> 0 then None else Some sf
      | _ -> None
    else None
end

module INDEX = Map.Make (SIG)

let build_call_index () =
  Kf.Set.fold (fun kf idx ->
      let vf = Kernel_function.get_vi kf in
      match SIG.stub vf with
      | None ->
        let f = Kernel_function.get_name kf in
        Options.abort "Function '%s' can not be used as call-pointer stub" f
      | Some s ->
        match INDEX.find_opt s idx with
        | Some vf0 ->
          Options.abort
            "Functions -volatile-call-pointer '%s' and '%s' has same signature"
            vf0.vorig_name vf.vorig_name
        | None -> INDEX.add s vf idx
    )
    (Options.CallPtr.get ())
    INDEX.empty

(* -------------------------------------------------------------------------- *)
(* --- Pointer Calls                                                      --- *)
(* -------------------------------------------------------------------------- *)

let get_called_ptr = function
  | Mem e -> Some e
  | _ -> None

let get_canonical_call ~source f tf =
  let name =
    match tf.tnode with
    | TNamed ti when Ast_types.is_fun tf -> ti.torig_name
    | TFun (r, args, va) ->
      let buffer = Buffer.create 80 in
      Buffer.add_string buffer (Options.BindingPrefix.get ());
      Buffer.add_string buffer "Call_";
      Buffer.add_string buffer (typename r);
      List.iter (fun (_, ty, _) ->
          Buffer.add_char buffer '_';
          Buffer.add_string buffer (typename ty);
        ) (Cil.argsToList args);
      if va then Buffer.add_string buffer "_va";
      Buffer.contents buffer
    | _ ->
      Options.abort ~source
        "@[<hov 0>Call to @[<hov 2>(%a)@]@ with non-function type @[<hov 2>(%a)@]@]"
        Exp.pretty f Typ.pretty tf
  in
  try
    let kf = Globals.Functions.find_by_name name in
    Some (Kernel_function.get_vi kf)
  with Not_found ->
    Options.warning ~source ~once:true
      ~wkey:wkey_untransformed_call_function_not_found
      "@[<hov 0>Call to (%a) with type @[<hov 2>(%a):@]@ Function '%s' not found@]"
      Exp.pretty f Typ.pretty (Cil.typeOf f) name;
    None

let get_pointer_call ~index ~source f =
  let tf = Ast_types.direct_pointed_type (Cil.typeOf f) in
  let res = INDEX.find_opt (SIG.of_type tf) index in
  match res with
  | Some _ -> res
  | None ->
    if Options.BindingCall.get () then
      get_canonical_call ~source f tf
    else None

let add_eventual_cast_to_expression lval_typ e =
  let newt = Ast_types.remove_attributes_for_c_cast lval_typ in
  let e' = Cil.mkCast ~force:false ~newt e in
  if e' != e then
    Options.warning ~source:(fst e.eloc) ~once:true ~wkey:wkey_cast_insertion
      "@[<hov 0>Cast to (%a) inserted@ for expression (%a)@ of type (%a)@]"
      Typ.pretty newt Exp.pretty e Typ.pretty (Cil.typeOf e);
  e'

let add_eventual_cast_to_param arg_typ param =
  let newt = Ast_types.remove_attributes_for_c_cast arg_typ in
  let param' = Cil.mkCast ~force:false ~newt param in
  if param' != param then
    Options.warning ~source:(fst param.eloc) ~once:true
      ~wkey:wkey_cast_insertion
      "@[<hov 0>Cast to (%a) inserted@ for parameter (%a)@ of type (%a)@]"
      Typ.pretty newt Exp.pretty param Typ.pretty (Cil.typeOf param);
  param'

let do_pointer_call ~loc ~index ~transform f es =
  let source = fst loc in
  match get_pointer_call ~index ~source f with
  | None -> None
  | Some vf ->
    let fn = vf.vorig_name in
    let rec wrap ts va es : exp list =
      match ts, es with
      | [], [] -> []
      | (_, t, _) :: ts, e :: es ->
        (add_eventual_cast_to_param t e) :: wrap ts va es
      | [], es when va -> es
      | [], es ->
        Options.warning ~source ~once:true
          ~wkey:wkey_transformed_call_skipped_parameters
          "Using '%s': %d last parameters skipped" fn (List.length es);
        []
      | ts, [] ->
        Options.warning ~source ~once:true
          ~wkey:wkey_transformed_call_missing_parameters
          "Using '%s': missing %d parameters" fn (List.length ts);
        []
    in
    let vf = transform vf in
    let (_, args, va, _) = Cil.splitFunctionTypeVI vf in
    Some (fn, Var vf, wrap (Cil.argsToList args) va (f :: es))

(*-------------------------------------------------------------------------*)

let typename_access ~is_wr_access (t:typ) =
  let typename = typename t in
  let r =
    (Options.BindingPrefix.get ())
    ^ (if is_wr_access then "Wr_" else "Rd_")
    ^ typename
  in
  Options.debug ~dkey:dkey_binding "Looking for function %s@." r;
  r

let find_typename ~is_wr_access kf_tbl typ =
  let open Option.Operators in
  let kf_tbl = Option.value ~default:(BA_TBL.build_kf_table kf_tbl) !kf_tbl in
  let rec find_fct typ =
    let typ = Ast_types.remove_attributes_for_c_cast typ in
    let tbl = BA_TBL.get_tbl_access ~is_wr_access kf_tbl in
    let typ_name = typename_access ~is_wr_access typ in
    match StringTbl.find_opt tbl typ_name with
    | Some vi -> Some vi
    | None ->
      (* Unroll the typedef until finding one function into the kf table. *)
      match typ.tnode with
      | TNamed r -> find_fct r.ttype
      | _ -> None
  in
  Options.debug ~level:2 ~dkey:dkey_binding
    "Looking for a default binding from the type name: %a@."
    Typ.pretty typ;
  (* Verifying the protyping within the type of the volatile access. *)
  let* fct = find_fct typ in
  let ty = fct.vtype in
  let ret, _args, _is_varg_arg, _attrib = Cil.splitFunctionType ty in
  let volatile_ret_type = add_volatile_attr ret in
  Options.debug ~level:2 ~dkey:dkey_binding
    "Verifying the type of the lvalue within the prototype of function %s: %a@."
    fct.vorig_name Typ.pretty ty;
  if not (Typ.equal typ volatile_ret_type) then
    None
  else Some fct

(*-------------------------------------------------------------------------*)

type vmap = {
  mutable rd : varinfo L_MAP.t;
  mutable wr : varinfo L_MAP.t;
}

(** Builds a table of volatile clauses.
    This table can be viewed as a map from term_lhost to a map from term_lval
    to (reads, writes) functions. *)
let build_volatile_table vmap =
  let add_fct kind loc map path = function
    | None -> map
    | Some fct ->
      match L_MAP.find_opt path map with
      | Some old ->
        if not (Varinfo.equal old fct) then
          Options.warning ~source:(fst loc) ~once:true
            ~wkey:wkey_duplicated_access_function
            "%s access function already defined for %a"
            kind L_PATH.pretty path;
        map
      | None -> L_MAP.add path fct map
  in
  let add_clause _emitter = function
    | Dvolatile (tset, fct_rd, fct_wr, _attr, loc) ->
      List.iter (fun l ->
          try
            let p = L_PATH.of_term l.it_content in
            vmap.rd <- add_fct "read" loc vmap.rd p fct_rd;
            vmap.wr <- add_fct "write" loc vmap.wr p fct_wr;
          with L_PATH.Unsupported ->
            Options.error ~source:(fst loc)
              "Unsupported l-value in volatile clause: %a@."
              Identified_term.pretty l
        ) tset
    | _ -> ()
  in
  Options.feedback ~level:2 "Building volatile table...@.";
  Annotations.iter_global add_clause;
  if Options.is_debug_key_enabled dkey_volatile_table then begin
    let dump kind map fmt =
      L_MAP.iter (fun p f ->
          Format.fprintf fmt "@\n@[<hov 2>volatile %a@ %s %a@]"
            L_PATH.pretty p kind Varinfo.pretty f
        ) map
    in
    Options.debug ~dkey:dkey_volatile_table "Volatile table:%t%t@."
      (dump "reads" vmap.rd)
      (dump "writes" vmap.wr)
  end

(*-------------------------------------------------------------------------*)

(* Returns a tuple 'transformed_key, untransformed_key) *)
let get_wkeys ~is_complete =
  if is_complete then
    wkey_transformed_access_lvalue_volatile,
    wkey_untransformed_access_lvalue_volatile
  else
    wkey_transformed_access_lvalue_partially_volatile,
    wkey_untransformed_access_lvalue_partially_volatile

let get_volatile_access ~is_wr_access fct_name binding_map kf_tbl vol_tbl lval =
  let typ = Cil.typeOfLval lval in
  let source = fst (Current_loc.get ()) in
  let warn_access = if is_wr_access then "write" else "read" in
  (* Can raise L_PATH.Unsupported via L_PATH.of_lval *)
  let get_volatile_access ~is_complete =
    let transformed_key, untransformed_key = get_wkeys ~is_complete in
    let warn_complete = if is_complete then "" else "partially " in
    let path = L_PATH.of_lval lval in
    let found fct =
      Options.debug ~level:2 ~dkey:dkey_binding
        "Function found: %s@." fct.vname;
      Options.warning ~source ~once:true ~wkey:transformed_key
        "%s function: Introducing a call to '%s' for %s access to %svolatile left-value: %a"
        fct_name fct.vorig_name warn_access warn_complete Lval.pretty lval;
      Some (fct, (Ast_types.remove_attributes_for_c_cast typ))
    in
    (* Looking for a volatile function relative to the [lval] access. *)
    Options.debug ~level:2 ~dkey:dkey_binding
      "Looking for a function relative to %s access to volatile left-value: %a@."
      warn_access L_PATH.pretty path;
    (* 1 - Looking into the volatile table [vol_tbl]. *)
    let vmap = if is_wr_access then vol_tbl.wr else vol_tbl.rd in
    match L_MAP.find_opt path vmap with
    | Some fct -> found fct
    | None ->
      (* 2 - Looking into binding functions from the type value. *)
      match B_MAP.find_binding ~is_wr_access binding_map typ with
      | Some f -> found f
      | None ->
        (* 3 - Looking into kernel functions for a name inferred from the type value. *)
        match find_typename ~is_wr_access kf_tbl typ with
        | Some fct -> found fct
        | None ->
          let t =
            Ast_types.remove_attributes_for_c_cast
              (if Options.Base.get () then T_MAP.basetype typ else typ)
          in
          Options.warning ~source ~once:true ~wkey:untransformed_key
            "Undefined %s access function for %svolatile left-value: (volatile %a) %a"
            warn_access warn_complete Typ.pretty t Lval.pretty lval;
          None
  in
  try
    if has_volatile_attr typ then
      get_volatile_access ~is_complete:true
    else if Ast_types.is_volatile typ then
      get_volatile_access ~is_complete:false
    else
      None
  with L_PATH.Unsupported ->
    Options.warning ~source "Unsupported volatile l-value: %a"
      Lval.pretty lval;
    None

let get_rd_types fct =
  let ty = fct.vtype in
  let ret, args, _is_varg_arg, _attrib = Cil.splitFunctionType ty in
  match args with
  | Some [_, arg1, _] -> ret, arg1
  | _ -> Options.abort "Invalid prototype of function %s@." fct.vorig_name

let get_wr_types fct =
  let ty = fct.vtype in
  let ret, args, _is_varg_arg, _attrib = Cil.splitFunctionType ty in
  match args with
  | Some ((_, arg1, _) :: [_, arg2, _]) -> ret, arg1, arg2
  | _ -> Options.abort "Invalid prototype of function %s@." fct.vorig_name

let get_cast_type_needed_for_assignation ~ret_typ ~lv =
  let tlv = Cil.typeOfLval lv in
  let tlv = Ast_types.remove_qualifiers tlv in
  if Cabs2cil.allow_return_collapse ~tlv ~tf:ret_typ
  then None
  else Some tlv

module ScopingBlock = struct
  let stack = ref []
  let reset () =
    stack := []
  let push b = stack := b :: !stack
  let pop () = match !stack with
    | [] -> assert false
    | _ :: tail -> stack := tail
  let top () = match !stack with
    | [] -> assert false
    | top :: _ -> top
end

let new_blk () = Cil.mkBlockNonScoping []

class process_volatile_access project binding_map kf_tbl vol_tbl index =
  let callptr = not (INDEX.is_empty index) || Options.BindingCall.get () in

  object(self)
    inherit Visitor.frama_c_copy project

    method private get_volatile_access ~is_wr_access lv =
      let kf_name =
        match self#current_kf with
        | None ->
          Options.fatal "get_volatile_access: this method should always be \
                         called inside a function (i.e. at a point where \
                         current_kf is set)."
        | Some kf -> Kernel_function.get_name kf
      in
      get_volatile_access ~is_wr_access kf_name binding_map kf_tbl vol_tbl lv

    val mutable top_eid = -1
    method private set_top_eid = function
      | Set (lv, {enode=Lval _; eid}, _loc) ->
        begin
          match self#get_volatile_access ~is_wr_access:true lv with
          | None -> top_eid <- eid
          | Some _ -> ()
        end
      | _ -> ()
    method private reset_top_eid () =
      top_eid <- -1

    val mutable blk = new_blk ()

    method private add_instr i =
      Options.debug ~level:2 ~dkey:dkey_transformation_action "Add new stmt to block";
      blk <- {blk with bstmts = (Cil.mkStmt (Instr i)) :: blk.bstmts}

    method private makeTempLval typ =
      Options.debug ~level:2 ~dkey:dkey_transformation_action "Add tmp variable to block";
      let definition =
        Visitor_behavior.Get.fundec self#behavior (Option.get self#current_func)
      in
      Var (Cil.makeLocalVar definition ~scope:(ScopingBlock.top ()) "__volatile_tmp" typ), NoOffset

    method! vblock b =
      Options.debug ~level:2 ~dkey:dkey_transformation_visit "Visit DO blk@.";
      if b.bscoping then ScopingBlock.push b;
      let pop b = if b.bscoping then ScopingBlock.pop (); b in
      let r = Cil.ChangeDoChildrenPost (b, pop) in
      Options.debug ~level:2 ~dkey:dkey_transformation_visit "Visit DONE blk@.";
      r

    method! vstmt_aux s =
      Options.debug ~level:2 ~dkey:dkey_transformation_visit
        "Visit DO stmt: sid=%d, volatile block:@.%a@." s.sid Block.pretty blk;
      let previous_blk =
        if blk.bstmts = [] then
          None
        else
          let previous_blk = blk in
          blk <- new_blk ();
          Some previous_blk
      in
      let do_vstmt st =
        let current_blk = blk in
        blk <- (match previous_blk with
            | None -> new_blk ()
            | Some previous_blk -> previous_blk);
        if current_blk.bstmts = [] then begin
          Options.debug ~level:3 ~dkey:dkey_transformation_visit
            "Do not Transform stmt:@.sid=%d %a@." s.sid Stmt.pretty st;
          st
        end
        else begin
          Options.debug ~level:2 ~dkey:dkey_transformation_visit
            "Transform DO stmt: sid=%d, volatile block:@.%a@."
            s.sid Block.pretty current_blk;
          let stmt = Cil.mkStmt st.skind in
          let stmts =
            { current_blk with bstmts = List.rev (stmt :: current_blk.bstmts)}
          in
          st.skind <- Block stmts;
          Options.debug ~level:2 ~dkey:dkey_transformation_visit
            "Transform Done stmt: sid=%d, new block:@.%a@."
            st.sid Stmt.pretty st;
          st
        end
      in
      let r = Cil.ChangeDoChildrenPost (s, do_vstmt) in
      Options.debug ~level:2 ~dkey:dkey_transformation_visit
        "Visit Done stmt: sid=%d@." s.sid;
      r

    method! vinst instr =
      let do_volatile = function
        | Set (lv, e, loc) as i ->
          begin
            match self#get_volatile_access ~is_wr_access:true lv  with
            | None ->
              let i = match e.enode with
                | Lval lv2 when e.eid = top_eid ->
                  begin
                    match self#get_volatile_access ~is_wr_access:false lv2 with
                    | None -> i
                    | Some (rd_fct, _typ) ->
                      begin (* lv=lv2; -> lv=rd_fct(&lv2); *)
                        (* To get the varinfo of the new project *)
                        let rd_fct = Visitor_behavior.Memo.varinfo self#behavior rd_fct in
                        let ret_typ, arg1_typ = get_rd_types rd_fct in
                        let rd_fct = Var rd_fct in
                        let addr = add_eventual_cast_to_param arg1_typ (Cil.mkAddrOf ~loc lv2) in
                        match get_cast_type_needed_for_assignation ~ret_typ ~lv with
                        | None -> Call (Some lv, rd_fct, [addr], loc)
                        | Some newt -> (* In fact a cast has to be added
                                          lv=lv2; -> vtmp=rd_fct(&lv2); lv=(newt) vtmp *)
                          Options.debug ~level:2 ~dkey:dkey_transformation_visit
                            "@[<hov 0> Cast Needed: Lval-type(%a) Return-type (%a)@]"
                            Typ.pretty (Cil.typeOfLval lv) Typ.pretty ret_typ;
                          let lvtmp = self#makeTempLval ret_typ in
                          let instr = Call (Some lvtmp, rd_fct, [addr], loc) in
                          self#add_instr instr;
                          let etmp = Cil.new_exp ~loc (Lval lvtmp) in
                          Set (lv, (add_eventual_cast_to_expression newt etmp), loc)
                      end
                  end
                | _ -> i
              in self#reset_top_eid (); i
            | Some (wr_fct, _typ) -> (* lv=e; -> wr_fct(&lv, e); *)
              (* To get the varinfo of the new project *)
              let wr_fct = Visitor_behavior.Memo.varinfo self#behavior wr_fct in
              let _, arg1_typ, arg2_typ = get_wr_types wr_fct in
              let wr_fct = Var wr_fct in
              let addr = add_eventual_cast_to_param arg1_typ (Cil.mkAddrOf ~loc lv) in
              let e = add_eventual_cast_to_param arg2_typ e in
              Call (None, wr_fct, [addr;e], loc)
          end
        | Call (Some lv, f, a, loc) as i ->
          begin
            match self#get_volatile_access ~is_wr_access:true lv with
            | None -> i
            | Some (wr_fct, typ) ->
              (* lv=f(a); -> vtmp = f(a); wr_fct(&lv, vtmp); *)
              (* To get the varinfo of the new project *)
              let wr_fct = Visitor_behavior.Memo.varinfo self#behavior wr_fct in
              let _, arg1_typ, arg2_typ = get_wr_types wr_fct in
              let wr_fct = Var wr_fct in
              let addr = add_eventual_cast_to_param arg1_typ (Cil.mkAddrOf ~loc lv) in
              let lvtmp = self#makeTempLval (Ast_types.remove_attributes_for_c_cast typ) in
              let instr = Call (Some lvtmp, f, a, loc) in
              let etmp = add_eventual_cast_to_param arg2_typ (Cil.new_exp ~loc (Lval lvtmp)) in
              self#add_instr instr;
              Call (None, wr_fct, [addr;etmp], loc)
          end
        | i -> i in
      let do_call = function
        | Call (result, ef, xs, loc) as i ->
          begin
            match get_called_ptr ef with
            | None -> i
            | Some f ->
              let transform = Visitor_behavior.Memo.varinfo self#behavior in
              match do_pointer_call ~loc ~index ~transform f xs with
              | Some (fn, g, ys) ->
                Options.warning ~source:(fst loc) ~once:true
                  ~wkey:wkey_transformed_call
                  "%a: use pointer function '%s'"
                  Fileloc.pretty loc fn;
                Call (result, g, ys, loc)
              | None ->
                Options.warning ~source:(fst loc) ~once:true
                  ~wkey:wkey_untransformed_call
                  "Original pointer function kept"; i
          end
        | i -> i in
      let do_vinst i = do_volatile (if callptr then do_call i else i) in
      self#set_top_eid instr;
      Cil.ChangeDoChildrenPost ([instr], tailrec_list_map do_vinst)

    method! vexpr e =
      let do_vexpr e =
        match e.enode with
        | Lval (lv) when e.eid <> top_eid ->
          begin (* ...lv..;. -> vtmp=rd_fct(&lv); ...vtmp...; *)
            match self#get_volatile_access ~is_wr_access:false lv with
            | None -> e
            | Some (rd_fct, _typ) ->
              (* To get the varinfo of the new project *)
              let rd_fct = Visitor_behavior.Memo.varinfo self#behavior rd_fct in
              let ret_typ, arg1_typ = get_rd_types rd_fct in
              let loc =  match self#current_kinstr with
                | Kstmt stmt -> Stmt.loc stmt
                | _ -> assert false (* impossible *)
              in
              let rd_fct = Var rd_fct in
              let addr = add_eventual_cast_to_param arg1_typ (Cil.mkAddrOf ~loc lv) in
              let ret_typ = Ast_types.remove_attributes_for_c_cast ret_typ in
              let lvtmp = self#makeTempLval ret_typ in
              let instr = Call (Some lvtmp, rd_fct, [addr], loc) in
              self#add_instr instr;
              add_eventual_cast_to_expression (Cil.typeOf e) (Cil.new_exp ~loc (Lval lvtmp))
          end
        | CastE (typ, exp) when
            (match Ast_types.unroll_node typ with
             | TPtr typ_pointed -> not (has_volatile_attr typ_pointed)
             | _ -> false) ->
          begin
            let typ_exp = Cil.typeOf exp in
            match Ast_types.unroll_node typ_exp with
            | TPtr typ_pointed when Ast_types.is_volatile typ_pointed ->
              Options.warning ~source:(fst (Current_loc.get ())) ~once:true
                ~wkey:wkey_volatile_cast
                "Cast from type with volatile attribute (%a) to %a. Detection of \
                 volatile access may fail."
                Typ.pretty typ_exp Typ.pretty typ
            | _ -> ()
          end;
          e
        | _ -> e
      in
      match e.enode with
      | SizeOfE _ | AlignOfE _ -> Cil.JustCopy
      | _ -> Cil.ChangeDoChildrenPost (e, do_vexpr)

    method! vglob_aux = function
      | GFun (decl, _) ->
        let dkey = dkey_transformation_visit in
        let f = Globals.Functions.get decl.svar in
        let fs = Options.Process.get () in
        if Kf.Set.is_empty fs || Kf.Set.mem f fs
        then begin
          Options.debug ~level:2 ~dkey "Visit DO fun %s@." decl.svar.vname;
          ScopingBlock.reset ();
          Cil.DoChildren
        end
        else begin
          Options.debug ~level:2 ~dkey "Visit COPY fun %s@." decl.svar.vname;
          Cil.JustCopy
        end
      | _ ->
        Options.debug ~level:3 ~dkey:dkey_transformation_visit "Visit COPY glob@.";
        Cil.JustCopy

  end

let find_volatile_access vol_tbl =
  Options.feedback ~level:2 "Building new project with volatile access transformed...@.";
  let kf_tbl = ref None in
  let index = build_call_index () in
  let binding_map = B_MAP.build_binding_map () in
  let _fresh_project =
    FILE.create_project_from_visitor
      ~reorder:true
      Options.plugin_name
      (fun prj -> new process_volatile_access prj binding_map kf_tbl vol_tbl index)
  in
  BA_TBL.clear_kf_table kf_tbl

let process_volatile () =
  Options.feedback ~level:1 "Running volatile plugin...@.";
  let _ast = Ast.get () in
  let vol_tbl = { rd = L_MAP.empty; wr = L_MAP.empty } in
  Options.feedback ~level:1 "Processing volatile clauses...@.";
  build_volatile_table vol_tbl;
  find_volatile_access vol_tbl

let process_volatile_once, _ =
  State_builder.apply_once "Volatile.process_volatile" [] process_volatile

let run () =
  if Options.Enabled.get () then process_volatile_once ()

let () = Boot.Main.extend run
