(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

open Memory
open Logic

(* -------------------------------------------------------------------------- *)
(* ---  Utils                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Vmap = Cil_datatype.Varinfo.Map

let iadd_term env t = ignore @@ add_term env t
let add_iterm env = function { it_content = t } -> add_term env t
let add_ipred env ip = add_predicate env ip.ip_content.tp_statement

(* -------------------------------------------------------------------------- *)
(* ---  Process Assigns (From)                                            --- *)
(* -------------------------------------------------------------------------- *)

let wkey =
  Options.register_warn_category
    ~help:"Missing assigns or assigns-\\from"
    ~default:Werror "froms"

let add_write env lv tgt =
  Memory.add_write tgt @@ Access.Term(env.context,lv)

let dpoints_to env deps =
  merge_points_to @@
  List.fold_left
    (fun d t -> merge_domain d (add_iterm env t))
    pure deps

let add_dpoints_to env tgt = function
  | Some src -> Memory.add_points_to tgt src
  | None ->
    let loc = Access.location env.context in
    Options.warning ~wkey ~source:(fst loc)
      "No pointer found in assigns-\\from to pointer location(s)"

let add_points_to env tgt = function
  | FromAny ->
    let loc = Access.location env.context in
    Options.warning ~wkey ~source:(fst loc)
      "Missing \\from in assigns to pointer location(s)"
  | From [] -> () (* avoid warning for purely allocating functions *)
  | From deps ->
    add_dpoints_to env tgt @@ dpoints_to env deps

let rec store env lv ty tgt deps =
  match Ast_types.unroll_skel ty with
  | TNamed _ | TVoid -> () (* should not occur *)
  | TPtr _ | TFun _ | TBuiltin_va_list ->
    add_write env lv tgt ;
    add_dpoints_to env tgt (Lazy.force deps)
  | TInt _ | TFloat _ | TEnum _ -> add_write env lv tgt
  | TArray(te,_) ->
    let re = Memory.add_index tgt te in
    let ofs = TIndex (Logic_const.trange (None,None), TNoOffset) in
    let lve = Logic_const.addTermOffsetLval ofs lv in
    store env lve te re deps
  | TComp { cfields } ->
    List.iter
      (fun fd ->
         let ofs = TField(fd,TNoOffset) in
         let lvf = Logic_const.addTermOffsetLval ofs lv in
         let tgf = Memory.add_field tgt fd in
         store env lvf fd.ftype tgf deps
      ) @@ Option.value ~default:[] cfields

let is_ctype lt ty =
  Logic_const.plain_or_set
    (fun lt ->
       match Ast_types.unroll_logic lt with
       | Ctype tr -> Cil_datatype.Typ.equal ty tr
       | _ -> false
    ) lt

let add_write_compound env lv ty tgt from =
  let copies, others =
    List.partition_map
      (fun d ->
         let t = d.it_content in
         match t.term_node with
         | TLval ls when is_ctype t.term_type ty -> Left (t.term_loc,ls)
         | _ -> Right d)
      (match from with FromAny -> [] | From ws -> ws) in
  begin
    List.iter
      (fun (loc,ls) -> Memory.merge tgt @@ snd @@ add_addr_lval ~loc env ls)
      copies ;
    if copies = [] || others <> [] then
      store env lv ty tgt (lazy (dpoints_to env others)) ;
  end

let add_writes_from ~loc env (lv : term_lval) ~(from:deps) =
  let ty,tgt = add_addr_lval ~loc env lv in
  if Ast_types.is_arithmetic ty then
    add_write env lv tgt
  else if Ast_types.is_fun_or_ptr ty then
    begin
      add_write env lv tgt ;
      add_points_to env tgt from ;
    end
  else if Ast_types.is_struct_or_union ty then
    add_write_compound env lv ty tgt from
  else
    Options.not_yet_implemented
      ~source:(fst loc)
      "Unsupported assigns to type (%a)" Printer.pp_typ ty

let is_result = function (TResult _,_) -> true | _ -> false

let rec add_assigns_from env ~iscalled ~from tgt =
  match tgt.term_node with
  | TLval lv ->
    begin
      let loc = tgt.term_loc in
      match iscalled with
      | Some result ->
        if is_result lv then result := true ;
        add_writes_from env ~loc lv ~from
      | None ->
        ignore (add_addr_lval ~loc env lv)
    end
  | Tat(t,_) -> add_assigns_from env ~iscalled ~from t
  | Tunion ts | Tinter ts -> List.iter (add_assigns_from env ~iscalled ~from) ts
  | Tcomprehension _ ->
    Options.not_yet_implemented
      ~source:(fst tgt.term_loc)
      "Unsupported set-comprehension"
  | Tlet _ ->
    Options.not_yet_implemented
      ~source:(fst tgt.term_loc)
      "Unsupported \\let-assigns"
  | Tif (c,tt,te) ->
    add_predicate env c ;
    add_assigns_from env ~iscalled ~from tt ;
    add_assigns_from env ~iscalled ~from te ;
  | TConst _ | TSizeOf _ | TSizeOfE _ | TAlignOf _ | TAlignOfE _
  | TUnOp _ | TBinOp _ | TCast _ | TAddrOf _ | TStartOf _
  | Tapp _ | Tlambda _ | TDataCons _
  | Tbase_addr _ | Toffset (_, _) | Tblock_length _ | Tnull
  | TUpdate _ | Ttypeof _ | Ttype _ | Tempty_set | Trange _ ->
    Options.warning ~source:(fst tgt.term_loc)
      "Non-assignable term (skipped)@ (%a)"
      Printer.pp_term tgt

(* -------------------------------------------------------------------------- *)
(* ---  Process Behaviors                                                 --- *)
(* -------------------------------------------------------------------------- *)

let context ~called ~kf ip =
  match called with
  | None -> Access.Prop ip
  | Some stmt -> Access.Call(stmt,kf,ip)

let add_requires ~map ~called ~kf ~ki ~bhv ~formals ~result ip =
  let context = context ~called ~kf @@ Property.ip_of_requires kf ki bhv ip in
  add_ipred { map ; context ; formals ; result } ip

let add_assumes ~map ~called ~kf ~ki ~bhv ~formals ~result ip =
  let context = context ~called ~kf @@ Property.ip_of_assumes kf ki bhv ip in
  add_ipred { map ; context ; formals ; result } ip

let add_bassigns ~called ~map ~kf ~ki ~bhv ~formals ~result = function
  | WritesAny ->
    if called <> None then
      let loc = Kernel_function.get_location kf in
      Options.warning ~wkey ~source:(fst loc)
        "Precise assigns are required for calls"
  | Writes ws as asgn ->
    let bhv = Property.Id_contract (Datatype.String.Set.empty,bhv) in
    let ip = Option.get @@ Property.ip_of_assigns kf ki bhv asgn in
    let context = context ~called ~kf ip in
    let env = { map ; context ; formals ; result } in
    let iscalled = Option.map (fun _ -> ref false) called in
    List.iter
      (fun (t,from) -> add_assigns_from env ~iscalled ~from t.it_content) ws ;
    match iscalled with
    | None -> ()
    | Some result ->
      if not !result &&
         Ast_types.is_fun_or_ptr @@ Kernel_function.get_return_type kf
      then
        let loc = Access.location context in
        Options.warning ~wkey ~source:(fst loc)
          "Missing assigns \\result \\from for returned pointer"

let add_allocation ~map ~called ~kf ~ki ~bhv ~formals ~result alloc =
  match alloc with
  | FreeAllocAny -> ()
  | FreeAlloc _ ->
    ignore map ; ignore called ; ignore ki ;
    ignore bhv ; ignore formals ; ignore result ;
    let loc = Kernel_function.get_location kf in
    Options.not_yet_implemented ~source:(fst loc )
      "Unsupported \\allocates and \\frees"
(* | FreeAlloc (its1, its2) ->
   let bhv = Property.Id_contract (Datatype.String.Set.empty,bhv) in
   let clause = clause ~called @@ Option.get @@ Property.ip_of_allocation kf ki bhv alloc in
   let env = { map ; clause ; formals ; result } in
   (*TODO: FIX THIS, Cf. assigns *)
   let add_alloc env it1 it2 =
   let d1 = add_iterm env it1 in
   let d2 = add_iterm env it2 in
   ignore @@ merge_domain d1 d2
   in
   List.iter2 (add_alloc env) its1 its2 *)

let add_ensures ~map ~called ~kf ~ki ~bhv ~formals ~result ensures =
  List.iter
    (fun kp ->
       let ip = Property.ip_of_ensures kf ki bhv kp in
       let context = context ~called ~kf ip in
       add_ipred { map ; context ; formals ; result } (snd kp)
    ) ensures

let rec add_extension ~map ~called ~kf ~ki ~formals ~result acsl =
  let eloc =
    match ki with
    | Kglobal -> Property.ELContract kf
    | Kstmt stmt -> Property.ELStmt (kf, stmt) in
  let context = context ~called ~kf @@ Property.ip_of_extended eloc acsl in
  match acsl.ext_kind with
  | Ext_id id ->
    if acsl.ext_plugin = "region" then
      match called with
      | None ->
        let env = { map ; context ; formals ; result } in
        List.iter (Logic.add_region env) (Spec.of_extid id)
      | Some stmt ->
        let loc = Cil_datatype.Stmt.loc stmt in
        Options.not_yet_implemented ~source:(fst loc)
          "Unsupported region specification for calls"
    else
      let loc = Access.location context in
      Options.not_yet_implemented ~source:(fst loc)
        "Unsupported \\%s:%s extensions" acsl.ext_plugin acsl.ext_name

  | Ext_terms ts ->
    let env = { map ; context ; formals ; result } in
    List.iter (iadd_term env) ts
  | Ext_preds ps ->
    let env = { map ; context ; formals ; result } in
    List.iter (add_predicate env) ps
  | Ext_annot (_,acsls) ->
    List.iter (add_extension ~map ~called ~kf ~ki ~formals ~result) acsls

let add_behavior ~map ~called ~kf ~ki ~formals ~result bhv =
  begin
    List.iter (add_requires ~map ~called ~kf ~ki ~bhv ~formals ~result) bhv.b_requires ;
    List.iter (add_assumes ~map ~called ~kf ~ki ~bhv ~formals ~result) bhv.b_assumes ;
    add_ensures ~map ~called ~kf ~ki ~bhv ~formals ~result bhv.b_post_cond ;
    add_bassigns ~map ~called ~kf ~ki ~bhv ~formals ~result bhv.b_assigns ;
    add_allocation ~map ~called ~kf ~ki ~bhv ~formals ~result bhv.b_allocation ;
    List.iter (add_extension ~map ~called ~kf ~ki ~formals ~result) bhv.b_extended ;
  end

(* -------------------------------------------------------------------------- *)
(* ---  Process Code Annotation                                           --- *)
(* -------------------------------------------------------------------------- *)

let add_variant ~map ~called ~kf ~ki ~formals ~result variant =
  let context = context ~called ~kf @@ Property.ip_of_decreases kf ki variant in
  let env = { map ; context ; formals ; result } in
  ignore @@ add_term env @@ fst variant

let add_terminates ~map ~called ~kf ~ki ~formals ~result spec condition =
  let ip = Property.ip_terminates_of_spec kf ki spec in
  let context = context ~called ~kf @@ Option.get ip in
  let env = { map ; context ; formals ; result } in
  add_ipred env condition

let add_spec ~map ?called ~kf ?(ki=Kglobal) ?(formals=Vmap.empty) ~result s =
  begin
    Option.iter (add_variant ~map ~called ~kf ~ki ~formals ~result) s.spec_variant ;
    Option.iter (add_terminates ~map ~called ~kf ~ki ~formals ~result s) s.spec_terminates ;
    List.iter (add_behavior ~map ~called ~kf ~ki ~formals ~result) s.spec_behavior ;
  end

(* -------------------------------------------------------------------------- *)
(* ---  Process Code Annotations                                          --- *)
(* -------------------------------------------------------------------------- *)

let iprop ip = Access.Prop ip

let add_code_annot ~map ~kf ~stmt ~result ca =
  let ki = Kstmt stmt in
  let formals = Vmap.empty in
  match ca.annot_content with
  | AAssert (_,{ tp_statement = p }) ->
    let context = iprop @@ Property.ip_of_code_annot_single kf stmt ca in
    let env = { map ; context ; formals ; result } in
    add_predicate env p
  | AStmtSpec (_,spec) ->
    add_spec ~map ~kf ~ki ~result spec
  | AInvariant (_,_,{ tp_statement = p }) ->
    let context = iprop @@ Property.ip_of_code_annot_single kf stmt ca in
    add_predicate { map ; context ; formals ; result } p
  | AVariant v ->
    add_variant ~map ~called:None ~kf ~ki ~formals ~result v
  | AAssigns (_,WritesAny) -> ()
  | AAssigns (_,Writes ws) ->
    let context = iprop @@ Property.ip_of_code_annot_single kf stmt ca in
    let env = { map ; context ; formals ; result } in
    List.iter
      begin fun (tgt,_) ->
        add_assigns_from env ~iscalled:None tgt.it_content ~from:FromAny
      end ws
  | AAllocation (_,FreeAllocAny) -> ()
  | AAllocation (_,FreeAlloc _) ->
    let loc = Cil_datatype.Stmt.loc stmt in
    Options.not_yet_implemented
      ~source:(fst loc)
      "Unsupported \\allocates and \\frees" ;
    (*TODO FIX THIS, Cf. assigns & add_allocates *)
  | AExtended (_,_, acsl) ->
    add_extension ~map ~called:None ~kf ~ki ~formals ~result acsl

(* -------------------------------------------------------------------------- *)
