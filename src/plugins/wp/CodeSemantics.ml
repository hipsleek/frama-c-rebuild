(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- C-Code Translation                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Cil_datatype
open Cil_types
open Ctypes
open Qed
open Memory
open Sigma
open Lang

module WpLog = Wp_parameters
let constfold_ctyp t =
  match t.tnode with
  | TArray (_,Some {enode = (Const CInt64 _) }) -> t
  | TArray (ty,Some len) -> begin
      match Cil.constFold true len with
      | {enode = (Const CInt64 _) } as len ->
        Cil_const.mk_tarray ~tattr:t.tattr ty (Some len)
      | _ -> t
    end
  | _ -> t

let constfold_coffset = function
  | Index({enode=Const (CInt64 _)}, _) as off -> off
  | Index(idx, next) as off -> begin
      match Cil.constFold true idx with
      | {enode = (Const CInt64 _) } as idx -> Index(idx, next)
      | _ -> off
    end
  | off -> off

module Make(M : Memory.Model) =
struct

  module M = M

  type loc = M.loc
  type value = M.loc Memory.value
  type result = loc Memory.result

  let pp_value fmt = function
    | Val e -> Format.fprintf fmt "Val:%a" F.pp_term e
    | Loc l-> Format.fprintf fmt "Loc:%a" M.pretty l

  let cval = function
    | Val e -> e
    | Loc l -> M.pointer_val l

  let cloc = function
    | Loc l -> l
    | Val e -> M.pointer_loc e

  (* -------------------------------------------------------------------------- *)
  (* --- Initializers                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let is_zero_int = function
    | Val e -> F.p_equal e F.e_zero
    | Loc l -> M.is_null l

  let is_zero_float ft = function
    | Val e -> F.p_equal e @@ Cfloat.float_of_real ft F.e_zero_real
    | Loc l -> M.is_null l

  let is_zero_ptr v = M.is_null (cloc v)

  let rec is_zero sigma obj l =
    match obj with
    | C_int _ -> is_zero_int (M.load sigma obj l)
    | C_float ft -> is_zero_float ft (M.load sigma obj l)
    | C_pointer _ -> is_zero_ptr (M.load sigma obj l)
    | C_comp { cfields = None } ->
      F.p_true (* cannot say anything interesting here *)
    | C_comp { cfields = Some fields } ->
      F.p_all
        (fun f -> is_zero sigma (Ctypes.object_of f.ftype) (M.field l f))
        fields
    | C_array a ->
      (*TODO[LC] make zero-initializers model-dependent.
                 For instance, a[N][M] becomes a[N*M] in MemTyped,
                 but not in MemVar *)
      let x = Lang.freshvar ~basename:"k" Logic.Int in
      let k = F.e_var x in
      let obj = Ctypes.object_of a.arr_element in
      let range = match a.arr_flat with
        | None -> []
        | Some f -> [ F.p_leq F.e_zero k ; F.p_lt k (F.e_int f.arr_size) ] in
      let init = is_zero sigma obj (M.shift l obj k) in
      F.p_forall [x] (F.p_hyps range init)

  let is_exp_range sigma l obj a b v =
    let x = Lang.freshvar ~basename:"k" Logic.Int in
    let k = F.e_var x in
    let range = [ F.p_leq a k ; F.p_leq k b ] in
    let init =
      match v with
      | None -> is_zero sigma obj (M.shift l obj k)
      | Some v ->
        let elt = (M.load sigma obj (M.shift l obj k)) in
        if Ctypes.is_pointer obj then
          M.loc_eq (cloc elt) (cloc v)
        else
          F.p_equal (cval elt) (cval v)
    in
    F.p_forall [x] (F.p_hyps range init)

  (* -------------------------------------------------------------------------- *)
  (* --- Recursion                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let s_exp : (sigma -> exp -> value) ref = ref (fun _ _ -> assert false)
  let s_cond : (sigma -> exp -> F.pred) ref = ref (fun _ _ -> assert false)

  let val_of_exp env e = cval (!s_exp env e)
  let loc_of_exp env e = cloc (!s_exp env e)

  (* -------------------------------------------------------------------------- *)
  (* --- L-Values                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let loc_of_lhost env = function
    | Var x -> M.cvar x
    | Mem e -> loc_of_exp env e

  let rec loc_of_offset env l typ = function
    | NoOffset -> l
    | Field(f,offset) -> loc_of_offset env (M.field l f) f.ftype offset
    | Index(e,offset) ->
      let k = val_of_exp env e in
      let te = Ast_types.direct_element_type typ in
      let obj = Ctypes.object_of te in
      loc_of_offset env (M.shift l obj k) te offset

  let lval env (lhost,offset) =
    loc_of_offset env (loc_of_lhost env lhost) (Cil.typeOfLhost lhost) offset

  (* -------------------------------------------------------------------------- *)
  (* --- Unary Operator                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_unop env typ unop e =
    let v =
      match Ctypes.object_of typ , unop with
      | C_int i , Neg -> Cint.iopp i (val_of_exp env e)
      | C_int i , BNot -> Cint.bnot i (val_of_exp env e)
      | C_float f , Neg -> Cfloat.fopp f (val_of_exp env e)
      | C_int _ , LNot -> Cvalues.bool_eq (val_of_exp env e) F.e_zero
      | C_float _ , LNot -> Cvalues.bool_eq (val_of_exp env e) F.e_zero_real
      | C_pointer _ , LNot -> Cvalues.is_true (M.is_null (loc_of_exp env e))
      | _ ->
        Warning.error "Undefined unary operator (%a)" Printer.pp_typ typ
    in Val v

  (* -------------------------------------------------------------------------- *)
  (* --- Binary Operator                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let arith env tr iop fop e1 e2 =
    match Ctypes.object_of tr with
    | C_int i -> Val (iop i (val_of_exp env e1) (val_of_exp env e2))
    | C_float f -> Val (fop f (val_of_exp env e1) (val_of_exp env e2))
    | _ -> assert false

  let arith_int env tr iop e1 e2 =
    match Ctypes.object_of tr with
    | C_int i -> Val (iop i (val_of_exp env e1) (val_of_exp env e2))
    | _ -> assert false

  let bool_of_comp env iop lop fop e1 e2 =
    let t1 = Cil.typeOf e1 in
    let t2 = Cil.typeOf e2 in
    if Ast_types.is_ptr t1 && Ast_types.is_ptr t2 then
      Cvalues.is_true (lop (loc_of_exp env e1) (loc_of_exp env e2))
    else match Ast_types.unroll_node t1 with
      | TFloat f ->
        let p = fop (Ctypes.c_float f)
            (val_of_exp env e1) (val_of_exp env e2) in
        F.e_if (F.e_prop p) F.e_one F.e_zero
      | _ ->
        iop (val_of_exp env e1) (val_of_exp env e2)

  let bool_of_exp env e =
    match Ctypes.object_of (Cil.typeOf e) with
    | C_int _ -> Cvalues.bool_neq (val_of_exp env e) F.e_zero
    | C_float _ -> Cvalues.bool_neq (val_of_exp env e) F.e_zero_real
    | C_pointer _ -> Cvalues.is_false (M.is_null (loc_of_exp env e))
    | _ -> assert false

  let exp_binop env tr binop e1 e2 = match binop with
    | PlusA   -> arith env tr Cint.iadd Cfloat.fadd e1 e2
    | MinusA  -> arith env tr Cint.isub Cfloat.fsub e1 e2
    | Mult    -> arith env tr Cint.imul Cfloat.fmul e1 e2
    | Div     -> arith env tr Cint.idiv Cfloat.fdiv e1 e2
    | Mod     -> arith_int env tr Cint.imod e1 e2
    | Shiftlt -> arith_int env tr Cint.blsl e1 e2
    | Shiftrt -> arith_int env tr Cint.blsr e1 e2
    | BAnd    -> arith_int env tr Cint.band e1 e2
    | BOr     -> arith_int env tr Cint.bor  e1 e2
    | BXor    -> arith_int env tr Cint.bxor e1 e2
    | Eq      -> Val (bool_of_comp env Cvalues.bool_eq  M.loc_eq  Cfloat.feq e1 e2)
    | Ne      -> Val (bool_of_comp env Cvalues.bool_neq M.loc_neq Cfloat.fneq e1 e2)
    | Lt      -> Val (bool_of_comp env Cvalues.bool_lt  M.loc_lt  Cfloat.flt e1 e2)
    | Gt      -> Val (bool_of_comp env Cvalues.bool_lt  M.loc_lt  Cfloat.flt e2 e1)
    | Le      -> Val (bool_of_comp env Cvalues.bool_leq M.loc_leq Cfloat.fle e1 e2)
    | Ge      -> Val (bool_of_comp env Cvalues.bool_leq M.loc_leq Cfloat.fle e2 e1)
    | LAnd    -> Val (Cvalues.bool_and (bool_of_exp env e1) (bool_of_exp env e2))
    | LOr     -> Val (Cvalues.bool_or  (bool_of_exp env e1) (bool_of_exp env e2))
    | PlusPI ->
      let te = Ast_types.direct_pointed_type (Cil.typeOf e1) in
      let obj = Ctypes.object_of te in
      Loc(M.shift (loc_of_exp env e1) obj (val_of_exp env e2))
    | MinusPI ->
      let te = Ast_types.direct_pointed_type (Cil.typeOf e1) in
      let obj = Ctypes.object_of te in
      Loc(M.shift (loc_of_exp env e1) obj (F.e_opp (val_of_exp env e2)))
    | MinusPP ->
      let te = Ast_types.direct_pointed_type (Cil.typeOf e1) in
      let obj = Ctypes.object_of te in
      Val(M.loc_diff obj (loc_of_exp env e1) (loc_of_exp env e2))

  (* -------------------------------------------------------------------------- *)
  (* --- Cast                                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let cast tr te ve =
    match Ctypes.object_of tr , Ctypes.object_of te with

    | C_int ir , C_int ie ->
      let v = cval ve in
      Val( if Ctypes.sub_c_int ie ir then v else Cint.downcast ir v )

    | C_float fr , C_float fe ->
      let v = cval ve in
      Val( if Ctypes.equal_float fe fr then v else
             Cfloat.float_of_real fr (Cfloat.real_of_float fe v) )

    | C_int ir , C_float fr ->
      Val(Cint.of_real ir (Cfloat.real_of_float fr (cval ve)))

    | C_float fr , C_int _ ->
      Val(Cfloat.float_of_real fr (Cmath.real_of_int (cval ve)))

    | C_pointer tr , C_pointer te ->
      let obj_r = Ctypes.object_of tr in
      let obj_e = Ctypes.object_of te in
      if Ctypes.compare obj_r obj_e = 0
      then ve
      else Loc (M.cast {pre=obj_e;post=obj_r} (cloc ve))

    | C_pointer te , C_int _ ->
      let e = cval ve in
      Loc(if F.equal e (F.e_zero) then M.null
          else M.loc_of_int (Ctypes.object_of te) e)

    | C_int ir , C_pointer _ ->
      Val (M.int_of_loc ir (cloc ve))

    | t1, t2 when Ctypes.equal t1 t2 -> ve

    | _ ->
      Warning.error "cast (%a) into (%a) not yet implemented"
        Printer.pp_typ te Printer.pp_typ tr

  (* -------------------------------------------------------------------------- *)
  (* --- Undefined Exp                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_undefined e =
    let ty = Cil.typeOf e in
    let x = Lang.freshvar ~basename:"w" (Lang.tau_of_ctype ty) in
    Val (F.e_var x)

  (* -------------------------------------------------------------------------- *)
  (* --- Exp-Node                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_node env e =
    match e.enode with
    | Const c -> Val (Cvalues.constant c)

    | Lval lv ->
      if Cil.isVolatileLval lv &&
         Cvalues.volatile ~warn:"unsafe read-access to volatile l-value" ()
      then exp_undefined e
      else
        let loc = lval env lv in
        let typ = Cil.typeOfLval lv in
        let obj = Ctypes.object_of typ in
        let data = M.load env obj loc in
        Lang.assume (Cvalues.is_object obj data) ;
        data

    | AddrOf lv ->
      Loc (lval env lv)
    | StartOf lv ->
      Loc (Cvalues.startof ~shift:M.shift (lval env lv) (Cil.typeOfLval lv))

    | UnOp(op,e,ty) -> exp_unop env ty op e
    | BinOp(op,e1,e2,tr) -> exp_binop env tr op e1 e2

    | AlignOfE _ | AlignOf _
    | SizeOfE _ | SizeOf _  -> Val (Cvalues.constant_exp e)

    | CastE(tr,e) -> cast tr (Cil.typeOf e) (!s_exp env e)

  (* -------------------------------------------------------------------------- *)
  (* --- Exp with Error                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let exp_protected env e =
    Warning.handle
      ~handler:exp_undefined
      ~severe:false
      ~fallback:"Hide sub-term definition"
      (exp_node env) e

  (* -------------------------------------------------------------------------- *)
  (* --- Condition-Node                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let eq_t is_ptr t v1 v2 =
    match v1 , v2 with
    | Loc p , Loc q -> M.loc_eq p q
    | Val a , Val b -> F.p_equal a b
    | _ ->
      if is_ptr t
      then M.loc_eq (cloc v1) (cloc v2)
      else F.p_equal (cval v1) (cval v2)

  let neq_t is_ptr t v1 v2 =
    match v1 , v2 with
    | Loc p , Loc q -> M.loc_neq p q
    | Val a , Val b -> F.p_neq a b
    | _ ->
      if is_ptr t
      then M.loc_neq (cloc v1) (cloc v2)
      else F.p_neq (cval v1) (cval v2)

  let equal_typ t v1 v2 = eq_t Ast_types.is_ptr t v1 v2
  let equal_obj obj v1 v2 = eq_t Ctypes.is_pointer obj v1 v2
  let not_equal_typ t v1 v2 = neq_t Ast_types.is_ptr t v1 v2
  let not_equal_obj obj v1 v2 = neq_t Ctypes.is_pointer obj v1 v2

  let compare env vop lop fop e1 e2 =
    let t1 = Ctypes.object_of (Cil.typeOf e1) in
    let t2 = Ctypes.object_of (Cil.typeOf e2) in
    if not (Ctypes.equal t1 t2) then
      Warning.error "Comparison with different types (%a) and (%a)"
        Ctypes.pretty t1 Ctypes.pretty t2 ;
    match t1 with
    | C_pointer _ -> lop (loc_of_exp env e1) (loc_of_exp env e2)
    | C_float f -> (fop f) (val_of_exp env e1) (val_of_exp env e2)
    | _ -> vop (val_of_exp env e1) (val_of_exp env e2)

  let cond_node env e =
    match e.enode with

    | UnOp(  LNot, e,_)     -> F.p_not (!s_cond env e)
    | BinOp( LAnd, e1,e2,_) -> F.p_and (!s_cond env e1) (!s_cond env e2)
    | BinOp( LOr,  e1,e2,_) -> F.p_or (!s_cond env e1) (!s_cond env e2)
    | BinOp( Eq,   e1,e2,_) -> compare env F.p_equal M.loc_eq Cfloat.feq e1 e2
    | BinOp( Ne,   e1,e2,_) -> compare env F.p_neq M.loc_neq Cfloat.fneq e1 e2
    | BinOp( Lt,   e1,e2,_) -> compare env F.p_lt  M.loc_lt  Cfloat.flt e1 e2
    | BinOp( Gt,   e1,e2,_) -> compare env F.p_lt  M.loc_lt  Cfloat.flt e2 e1
    | BinOp( Le,   e1,e2,_) -> compare env F.p_leq M.loc_leq Cfloat.fle e1 e2
    | BinOp( Ge,   e1,e2,_) -> compare env F.p_leq M.loc_leq Cfloat.fle e2 e1

    | _ ->
      begin
        match Ctypes.object_of (Cil.typeOf e) with
        | C_int _ -> F.p_neq (val_of_exp env e) F.e_zero
        | C_float _ -> F.p_neq (val_of_exp env e) F.e_zero_real
        | C_pointer _ -> F.p_not (M.is_null (loc_of_exp env e))
        | obj -> Warning.error "Condition from (%a)" Ctypes.pretty obj
      end

  (* -------------------------------------------------------------------------- *)
  (* --- BootStrapping                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let exp env e = Current_loc.with_loc e.eloc (exp_protected env) e
  let cond env e = Current_loc.with_loc e.eloc (cond_node env) e
  let result env tr = function
    | R_var x -> F.e_var x
    | R_loc l -> cval (M.load env (Ctypes.object_of tr) l)
  let return env tr e = cval (cast tr (Cil.typeOf e) (exp env e))

  let () = s_exp := exp
  let () = s_cond := cond

  let instance_of floc kf =
    M.loc_eq floc (M.cvar (Kernel_function.get_vi kf))

  (* -------------------------------------------------------------------------- *)
  (* --- Initializers                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let unchanged sa sb v =
    let obj = Ctypes.object_of v.vtype in
    let loc = M.cvar v in
    let va = M.load sa obj loc in
    let vb = M.load sb obj loc in
    equal_obj obj va vb

  let init_value ~sigma lv typ init =
    let obj = Ctypes.object_of typ in
    let outcome = Warning.catch
        ~severe:false ~fallback:"Skip initializer"
        (fun () ->
           let l = lval sigma lv in
           let value_hyp = match init with
             | Some e ->
               let v = M.load sigma obj l in
               F.p_equal (val_of_exp sigma e) (cval v)
             | None -> is_zero sigma obj l
           in
           let init_hyp = match init with
             | Some { enode = Lval lv_init }
               when Ast_types.is_struct_or_union (Cil.typeOfLval lv_init) ->
               let l_initializer = lval sigma lv_init in
               F.p_equal
                 (M.load_init sigma obj l)
                 (M.load_init sigma obj l_initializer)
             | _ ->
               M.initialized sigma (Rloc(obj, l))
           in
           value_hyp, init_hyp
        ) () in
    match outcome with
    | Warning.Failed warn -> warn , (F.p_true, F.p_true)
    | Warning.Result(warn , hyp) -> warn , hyp

  let init_range ~sigma lv typ low up value =
    let obj = Ctypes.object_of typ in
    let outcome = Warning.catch
        ~severe:false ~fallback:"Skip initializer"
        (fun () ->
           let l = lval sigma lv in
           let e = Option.map (exp sigma) value in
           let low = F.e_bigint low and up = F.e_bigint up in
           (is_exp_range sigma l obj low up e),
           (M.initialized sigma (Rrange(l, obj, Some low, Some up)))
        ) () in
    match outcome with
    | Warning.Failed warn -> warn , (F.p_true, F.p_true)
    | Warning.Result(warn , hyp) -> warn , hyp


  type warned_hyp = Warning.Set.t * (Lang.F.pred * Lang.F.pred)

  (* Hypothesis for initialization of one variable *)
  let rec init_variable ~sigma lv init acc =
    match init with

    | SingleInit exp ->
      init_value ~sigma lv (Cil.typeOfLval lv) (Some exp) :: acc

    | CompoundInit ( ct , initl ) ->
      let ct = constfold_ctyp ct in
      let acc = (* updated acc with default init of structure *)
        match ct.tnode with
        | TComp { cfields = None } ->
          Wp_parameters.fatal
            "Initializer for incomplete type %a" Cil_printer.pp_typ ct
        | TComp { cstruct ; cfields = Some fields }
          when cstruct && (* not for union... *)
               (List.length initl) < (List.length fields) ->
          (* default init for uninitialized field of a struct *)
          List.fold_left
            (fun acc f ->
               if List.exists
                   (function
                     | Field(g,_),_ -> Fieldinfo.equal f g
                     | _ ->  WpLog.fatal "Kernel invariant broken into an initializer")
                   initl
               then acc
               else
                 let init =
                   init_value ~sigma
                     (Cil.addOffsetLval (Field(f, NoOffset)) lv)
                     f.ftype None in
                 init :: acc)
            acc (List.rev fields)

        | _ -> acc
      in
      match ct.tnode with
      | TArray (ty, len) ->
        let delayed =
          match len with (* number of required elements *)
          | Some {enode = (Const CInt64 (size,_,_))} ->
            (size, None)
          | _ -> (* CIL invariant broken. *)
            WpLog.fatal "CIL invariant broken: unknown initialized array size"
        in
        let make_quant acc = function
          (* adds delayed initializations from info about
             the last consecutive indices having
             the same value, but that have not yet initialized. *)
          | (_,None) -> acc (* nothing was delayed *)
          | (il,Some (i0,_,exp)) when Z.lt il i0 ->
            (* Added pred: \forall i \in [il .. i0] ; t[i]==exp *)
            init_range ~sigma lv ty il i0 (Some exp) :: acc
          | (_il,Some (_i0,off,exp)) ->
            (* case [_il=_i0], so uses [off] corresponding to [_i0]
               Added pred: t[i]==exp*)
            let lv = Cil.addOffsetLval off lv in
            init_value ~sigma lv ty (Some exp) :: acc
        in
        let add_missing_indices acc i0 = function
          (* adds eventual default value for missing indices. *)
          | (i1, _) ->
            if Z.geq i0 i1 then (* no hole *) acc
            else (* defaults values
                    Added pred: \forall i \in [i0 .. i1[ ; t[i]==default *)
              init_range ~sigma lv ty i0 (Z.pred i1) None :: acc
        in
        let acc, delayed =
          List.fold_left
            (fun (acc,delayed) (off,init) ->
               let off = constfold_coffset off in
               let idx,acc = match off with
                 | Index({enode=Const CInt64 (idx,_,_)}, _) ->
                   (match delayed with
                    | (iprev, _) when Z.lt iprev idx ->
                      (* CIL invariant broken.
                         without that invariant, an algo with a 2sd pass
                         is required for introducing default values *)
                      WpLog.fatal "CIL invariant broken: unordered initializer";
                    | _ -> ()) ;
                   idx,
                   (* adds default values for missing indices *)
                   add_missing_indices acc (Z.succ idx) delayed
                 | _ -> (* CIL invariant broken. *)
                   WpLog.fatal "CIL invariant broken: unknown initialized index"
               in
               match off, init with (* only simple init can be delayed *)
               | Index(_, NoOffset), SingleInit init -> begin
                   match delayed with
                   | (i_prev,(Some (_,_,init_delayed) as delayed_info))
                     when Wp_parameters.InitWithForall.get ()
                       && Z.equal (Z.pred i_prev) idx
                       && ExpStructEq.equal init_delayed init ->
                     acc, (idx,delayed_info)
                   | _ -> (* flush the delayed init, and store the new one *)
                     let acc = make_quant acc delayed in
                     acc, (idx, Some (idx,off,init))
                 end
               | Index(_, _),_ ->
                 (* flush the delayed init, and adds the current one *)
                 let acc = make_quant acc delayed in
                 let lv = Cil.addOffsetLval off lv in
                 (init_variable ~sigma lv init acc), (idx, None)
               | _ -> WpLog.fatal "CIL invariant broken: not an index"
            )
            (acc,delayed)
            (List.rev initl)
        in
        let acc = make_quant acc delayed in
        add_missing_indices acc Z.zero delayed
      | _ ->
        List.fold_left
          (fun acc (off,init) ->
             let lv = Cil.addOffsetLval off lv in
             init_variable ~sigma lv init acc)
          acc (List.rev initl)

  let init_one_char ~sigma:sigma v telt acc idx c =
    let loc = v.vdecl in
    let exp = Cil.new_exp ~loc (Const (CChr c)) in
    let idx = Index (Cil.kinteger ~loc (Machine.sizeof_kind()) idx,NoOffset) in
    init_value ~sigma (Var v, idx) telt (Some exp) :: acc

  let init_one_wchar ~sigma v telt acc idx c =
    let loc = v.vdecl in
    let exp =
      Cil.kinteger64 ~loc  ~kind:(Machine.wchar_kind()) (Z.of_int64_unsigned c)
    in
    let idx = Index (Cil.kinteger ~loc (Machine.sizeof_kind()) idx,NoOffset) in
    init_value ~sigma (Var v, idx) telt (Some exp) :: acc

  let init_end_array ~sigma v telt low up =
    let loc = v.vdecl in
    match up with
    | Some len when Z.lt low len ->
      [init_range ~sigma (Cil.var v) telt low (Z.pred len) (Some (Cil.zero ~loc))]
    | Some _ -> []
    | None -> [] (* TODO: emit a warning *)

  let init_string_literal ~sigma v s =
    if not (Ast_types.is_array v.vtype) then
      WpLog.fatal
        "CIL invariant broken: String literal can only initialize an array";
    let telt, len = Ast_types.array_elem_type_and_size v.vtype in
    let len = Option.bind (Cil.constFoldToInt ~machdep:true) len in
    match s with
    | Str s ->
      let lit_len = Z.of_int (String.length s) in
      let rest = init_end_array ~sigma v telt lit_len len in
      Seq.fold_lefti (init_one_char ~sigma v telt) rest (String.to_seq s)
    | Wstr l ->
      let lit_len = Z.of_int (List.length l) in
      let rest = init_end_array ~sigma v telt lit_len len in
      Seq.fold_lefti (init_one_wchar ~sigma v telt) rest (List.to_seq l)

  let init ~sigma v = function
    | None -> [init_value ~sigma (Cil.var v) v.vtype None]
    | Some (CInit init) -> List.rev (init_variable ~sigma (Cil.var v) init [])
    | Some (StrInit s) ->
      if Wp_parameters.Literals.get () then
        init_string_literal ~sigma v s
      else
        [init_value ~sigma (Cil.var v) v.vtype None]

end
