(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Orig_project =
  State_builder.Option_ref(Project.Datatype)(
  struct
    let name = "Ast_diff.OrigProject"
    let dependencies = []
  end)

type 'a correspondence =
  [ `Same of 'a (** symbol with identical definition has been found. *)
  | `Not_present (** no correspondence *)
  ]

module Correspondence_input =
struct
  type 'a t = 'a correspondence
  let name a = Type.name a ^ " correspondence"
  let structural_descr _ = Structural_descr.t_abstract
  let reprs x = [ `Not_present; `Same x]
  let equal eq x y =
    match x,y with
    | `Same x, `Same y -> eq x y
    | `Not_present, `Not_present -> true
    | `Same _, `Not_present
    | `Not_present, `Same _ -> false
  let compare cmp x y =
    match x,y with
    | `Not_present, `Not_present -> 0
    | `Not_present, `Same _ -> -1
    | `Same x, `Same y -> cmp x y
    | `Same _, `Not_present -> 1
  let hash h = function
    | `Same x -> 117 * h x
    | `Not_present -> 43
  let map f = function
    | `Same x -> `Same (f x)
    | `Not_present -> `Not_present
  let pretty pp fmt = function
    | `Not_present -> Format.pp_print_string fmt "N/A"
    | `Same x -> Format.fprintf fmt " => %a" pp x
  let mem_project mem query = function
    | `Not_present -> false
    | `Same x -> mem query x
end

module Correspondence = Datatype.Polymorphic(Correspondence_input)

(* for kernel function, we are a bit more precise than a yes/no answer.
   More precisely, we check whether a function has the same spec,
   the same body, and whether its callees have changed (provided
   the body itself is identical, otherwise, there's no point in checking
   the callees.
*)
type partial_correspondence =
  [ `Spec_changed (* body and callees haven't changed *)
  | `Body_changed (* spec hasn't changed *)
  | `Callees_changed (* spec and body haven't changed *)
  | `Callees_spec_changed (* body hasn't changed *)
  ]

type body_correspondence =
  [ `Body_changed
  | `Callees_changed
  | `Same_body
  ]

let (<=>) res (cmp,x,y) = if res <> 0 then res else cmp x y

let compare_pc pc1 pc2 =
  match pc1, pc2 with
  | `Spec_changed, `Spec_changed -> 0
  | `Spec_changed, _ -> -1
  | _, `Spec_changed -> 1
  | `Body_changed, `Body_changed -> 0
  | `Body_changed, _ -> -1
  | _, `Body_changed -> 1
  | `Callees_changed, `Callees_changed -> 0
  | `Callees_changed, _ -> -1
  | _, `Callees_changed -> 1
  | `Callees_spec_changed, `Callees_spec_changed -> 0

let pretty_pc fmt =
  let open Format in
  function
  | `Spec_changed -> pp_print_string fmt "(spec changed)"
  | `Body_changed -> pp_print_string fmt "(body changed)"
  | `Callees_changed -> pp_print_string fmt "(callees changed)"
  | `Callees_spec_changed -> pp_print_string fmt "(callees and spec changed)"

type 'a code_correspondence =
  [ 'a correspondence
  | `Partial of 'a * partial_correspondence
  ]

module Code_correspondence_input =
struct
  type 'a t = 'a code_correspondence
  let name a = Type.name a ^ " code_correspondence"
  let structural_descr _ = Structural_descr.t_abstract
  let reprs = Correspondence_input.reprs
  let equal eq x y =
    match x,y with
    | `Partial(x,pc), `Partial(x',pc') -> eq x x' && (compare_pc pc pc' = 0)
    | `Partial _, _ | _, `Partial _ -> false
    | (#correspondence as c), (#correspondence as c') ->
      Correspondence_input.equal eq c c'

  let compare cmp x y =
    match x,y with
    | `Partial(x,pc), `Partial(x',pc') ->
      cmp x x' <=> (compare_pc,pc,pc')
    | `Partial _, `Same _ -> -1
    | `Same _, `Partial _ -> 1
    | `Partial _, `Not_present -> 1
    | `Not_present, `Partial _ -> -1
    | (#correspondence as c), (#correspondence as c') ->
      Correspondence_input.compare cmp c c'

  let hash hash = function
    | `Partial (x,_) -> 57 * hash x
    | #correspondence as x -> Correspondence_input.hash hash x

  let map f = function
    | `Partial(x,pc) -> `Partial(f x,pc)
    | (#correspondence as x) -> Correspondence_input.map f x

  let pretty pp fmt = function
    | `Partial(x,flags) ->
      Format.fprintf fmt "-> %a %a" pp x pretty_pc flags
    | #correspondence as x -> Correspondence_input.pretty pp fmt x

  let mem_project f p = function
    | `Partial (x,_) -> f p x
    | #correspondence as x -> Correspondence_input.mem_project f p x
end

module Code_correspondence = Datatype.Polymorphic(Code_correspondence_input)

module Info(I: sig val name: string end) =
  (struct
    let name = "Ast_diff." ^ I.name
    let dependencies = [ Ast.self; Orig_project.self ]
    let size = 43
  end)

(* Map of symbols under analysis, in case of recursion.
   Note that this can only happen with aggregate types, logic
   types, and function (both C and ACSL). Other symbols must be
   correctly ordered in a well-formed AST
*)
type is_same_env =
  {
    compinfo: compinfo Cil_datatype.Compinfo.Map.t;
    kernel_function: kernel_function Kernel_function.Map.t;
    local_vars: varinfo Cil_datatype.Varinfo.Map.t;
    logic_info: logic_info Cil_datatype.Logic_info.Map.t;
    logic_type_info: logic_type_info Cil_datatype.Logic_type_info.Map.t;
    logic_local_vars: logic_var Cil_datatype.Logic_var.Map.t;
    logic_type_vars: string Datatype.String.Map.t;
    (* goto targets pairs are checked afterwards, so that forward gotos
       do not interrupt the linear visit.
       We thus collect them in the environment.
    *)
    goto_targets: (stmt * stmt) list;
    (* enum items are added collectively when the whole enuminfo is
       found to be identical (or not). On the other hand, the
       definition of an enumitem can refer to the previous ones in the
       same enuminfo. We keep the list of previously visited tags here.
    *)
    enumitem: enumitem Cil_datatype.Enumitem.Map.t;
  }

let is_same_acsl_extension_kind = ref (fun ~plugin:_ _ _ _ _ -> assert false)

let set_extension_diff ~is_same_ext =
  is_same_acsl_extension_kind := is_same_ext

let is_same_acsl_extension ext1 ext2 env =
  ext1.ext_name = ext2.ext_name &&
  ext1.ext_plugin = ext2.ext_plugin &&
  !is_same_acsl_extension_kind ~plugin:ext1.ext_plugin ext1.ext_name ext1.ext_kind ext2.ext_kind env

module type Correspondence_table = sig
  include State_builder.Hashtbl
  val pretty_data: Format.formatter -> data -> unit
end

module Build(H:Datatype.S_with_collections)(D:Datatype.S):
  Correspondence_table with type key = H.t and type data = D.t =
struct
  include
    State_builder.Hashtbl(H.Hashtbl)(D)
      (Info(struct let name = "Ast_diff." ^ D.datatype_name end))
  let pretty_data = D.pretty
end

module Build_correspondence(H:Datatype.S_with_collections) =
  Build(H)(Correspondence.Make(H))

module Build_code_correspondence(H:Datatype.S_with_collections) =
  Build(H)(Code_correspondence.Make(H))

module Varinfo = Build_correspondence(Cil_datatype.Varinfo)

module Compinfo = Build_correspondence(Cil_datatype.Compinfo)

module Enuminfo = Build_correspondence(Cil_datatype.Enuminfo)

module Enumitem = Build_correspondence(Cil_datatype.Enumitem)

module Typeinfo = Build_correspondence(Cil_datatype.Typeinfo)

module Stmt = Build_code_correspondence(Cil_datatype.Stmt)

module Logic_info = Build_correspondence(Cil_datatype.Logic_info)

module Logic_type_info = Build_correspondence(Cil_datatype.Logic_type_info)

module Logic_ctor_info = Build_correspondence(Cil_datatype.Logic_ctor_info)

module Fieldinfo = Build_correspondence(Cil_datatype.Fieldinfo)

module Model_info = Build_correspondence(Cil_datatype.Model_info)

module Logic_var = Build_correspondence(Cil_datatype.Logic_var)

module Kf = Kernel_function

module Kernel_function = Build_code_correspondence(Kernel_function)

module Fundec = Build_correspondence(Cil_datatype.Fundec)

let make_correspondence candidate has_same_spec code_corres =
  match has_same_spec, code_corres with
  | false, `Body_changed -> `Not_present
  | false, `Callees_changed ->
    `Partial(candidate,`Callees_spec_changed)
  | false, `Same_body ->
    `Partial(candidate, `Spec_changed)
  | true, `Same_body ->
    `Same candidate
  | true, ((`Body_changed|`Callees_changed) as c) ->
    `Partial(candidate, c)

let make_body_correspondence has_same_spec code_corres =
  if has_same_spec then code_corres else `Body_changed

let (&&>) (res,env) f =
  match res with
  | `Body_changed -> `Body_changed, env
  | `Same_body -> f env
  | `Callees_changed ->
    let res', env = f env in
    match res' with
    | `Body_changed -> `Body_changed, env
    | `Same_body | `Callees_changed -> `Callees_changed, env

let (&&&) (res, env) f = if res then f env else false, env

let empty_env =
  { compinfo = Cil_datatype.Compinfo.Map.empty;
    kernel_function = Kf.Map.empty;
    local_vars = Cil_datatype.Varinfo.Map.empty;
    logic_info = Cil_datatype.Logic_info.Map.empty;
    logic_type_info = Cil_datatype.Logic_type_info.Map.empty;
    logic_local_vars = Cil_datatype.Logic_var.Map.empty;
    logic_type_vars = Datatype.String.Map.empty;
    goto_targets = [];
    enumitem = Cil_datatype.Enumitem.Map.empty;
  }

let add_locals f f' env =
  let add_one env v v' =
    { env with local_vars = Cil_datatype.Varinfo.Map.add v v' env.local_vars }
  in
  List.fold_left2 add_one env f f'

(* local static variables are in fact global. As soon as we have determined
   that they have a correspondence, we add them to the global bindings *)
let add_statics l l' =
  let add_one v v' = Varinfo.add v (`Same v') in
  List.iter2 add_one l l'

let add_logic_vars p p' env =
  let add_one env lv lv' =
    { env with
      logic_local_vars =
        Cil_datatype.Logic_var.Map.add lv lv' env.logic_local_vars }
  in
  List.fold_left2 add_one env p p'

let add_logic_info v v' env =
  { env with logic_info = Cil_datatype.Logic_info.Map.add v v' env.logic_info }

let logic_type_vars_env l l' env =
  if List.length l = List.length l' then begin
    let logic_type_vars =
      List.fold_left2 (fun env s s' -> Datatype.String.Map.add s s' env)
        env.logic_type_vars l l'
    in
    true, { env with logic_type_vars }
  end else false, env

let formals_correspondence f f' =
  let add_one v v' = Varinfo.add v (`Same v') in
  List.iter2 add_one f f'

let logic_prms_correspondence p p' =
  let add_one lv lv' =
    Logic_var.add lv (`Same lv') in
  List.iter2 add_one p p'

(** TODO: use location info to detect potential renaming.
    Requires some information about syntactic diff. *)
let find_candidate_type ?loc:_loc ti =
  if Globals.Types.mem_type Logic_typing.Typedef ti.tname then begin
    match Globals.Types.global Logic_typing.Typedef ti.tname with
    | GType(ti,_) -> Some ti
    | g ->
      Kernel.fatal
        "Expected typeinfo instead of %a" Cil_datatype.Global.pretty g
  end else None

let find_candidate_compinfo ?loc:_loc ci =
  let su = if ci.cstruct then Logic_typing.Struct else Logic_typing.Union in
  if Globals.Types.mem_type su ci.cname then begin
    match Globals.Types.find_type su ci.cname with
    | { tnode = TComp ci' } -> Some ci'
    | t ->
      Kernel.fatal
        "Expected compinfo instead of %a"
        Printer.pp_typ t
  end else None

let find_candidate_enuminfo ?loc:_loc ei =
  if Globals.Types.mem_type Logic_typing.Enum ei.ename then begin
    match Globals.Types.find_type Logic_typing.Enum ei.ename with
    | { tnode = TEnum ei } -> Some ei
    | t ->
      Kernel.fatal
        "Expected enuminfo instead of %a"
        Printer.pp_typ t
  end else None

let find_candidate_varinfo ?loc:_loc vi where =
  try
    Some (Globals.Vars.find_from_astinfo vi.vname where)
  with Not_found -> None

let find_candidate_func ?loc:_loc kf =
  try
    Some (Globals.Functions.find_by_name (Kf.get_name kf))
  with Not_found -> None

let find_candidate_logic_type ?loc:_loc ti =
  try
    Some (Logic_env.find_logic_type ti.lt_name)
  with Not_found -> None

let is_same_opt f o o' env =
  match o, o' with
  | None, None -> true
  | Some v, Some v' -> f v v' env
  | _ -> false

let is_same_opt_env f o o' env =
  match o, o' with
  | None, None -> true, env
  | Some v, Some v' -> f v v' env
  | _ -> false, env

let is_same_pair f1 f2 (x1,x2) (y1,y2) env = f1 x1 y1 env && f2 x2 y2 env

let rec is_same_list f l l' env =
  match l, l' with
  | [], [] -> true
  | h::t, h'::t' -> f h h' env && is_same_list f t t' env
  | _ -> false

let rec is_same_list_env f l l' env =
  match l, l' with
  | [], [] -> true, env
  | h::t, h'::t' -> f h h' env &&& is_same_list_env f t t'
  | _ -> false, env

let get_original_kf vi =
  let selection = State_selection.of_list
      [Kernel_function.self; Annotations.funspec_state; Globals.Functions.self]
  in
  Project.on ~selection (Orig_project.get()) Globals.Functions.get vi

let check_goto_targets env =
  let check_one (s,s') =
    match Stmt.find s with
    | `Not_present -> false
    | `Same s'' | `Partial (s'',_) ->
      (* From the goto point of view, what matters is that the targets
         themselves have a correspondence. If they're e.g. calls to a
         function that has itself changed, or blocks whose content has
         changed, it has already been detected when comparing the targets,
         and will be dealt with accordingly as the fundec level. *)
      Cil_datatype.Stmt.equal s' s''
    | exception Not_found -> false
  in
  if List.for_all check_one env.goto_targets then `Same_body, env
  else `Body_changed, env

let is_matching_fieldinfo fi fi' =
  match Fieldinfo.find fi with
  | `Not_present -> false
  | `Same fi'' -> Cil_datatype.Fieldinfo.equal fi' fi''
  | exception Not_found ->
    Kernel.fatal "Unbound field %a in AST diff"
      Cil_datatype.Fieldinfo.pretty fi

let is_matching_model_info mf mf' =
  match Model_info.find mf with
  | `Not_present -> false
  | `Same mf'' -> Cil_datatype.Model_info.equal mf' mf''
  | exception Not_found ->
    Kernel.fatal "Unbound model field %a in AST diff"
      Cil_datatype.Model_info.pretty  mf

let is_matching_logic_type_var a a' env =
  match Datatype.String.Map.find_opt a env.logic_type_vars with
  | None -> false
  | Some a'' -> Datatype.String.equal a' a''

let is_same_behavior_set l l' =
  Datatype.String.Set.(equal (of_list l) (of_list l'))

let are_same_cd_clauses l l' =
  let module StringSetSet = Set.Make(Datatype.String.Set) in
  let of_list l =
    StringSetSet.of_list (List.map Datatype.String.Set.of_list l)
  in
  StringSetSet.equal (of_list l) (of_list l')

let is_same_logic_label l l' _env =
  match l, l' with
  | StmtLabel s, StmtLabel s' ->
    (* Contrarily to labels in goto statement, C labels in the logic can only
       refer to previous statements thanks to ACSL typing rules. Hence, we can
       directly check the Stmt table without resorting to deferred check by
       updating env.goto_targets
    *)
    (match Stmt.find !s with
     | `Not_present -> false
     | `Same s'' | `Partial(s'',_) ->
       Cil_datatype.Stmt.equal !s' s''
     | exception Not_found -> false)
  | FormalLabel s, FormalLabel s' -> Datatype.String.equal s s'
  | BuiltinLabel l, BuiltinLabel l' -> equal_logic_builtin_label l l'
  | (StmtLabel _ | FormalLabel _ | BuiltinLabel _), _ -> false

let rec is_same_predicate p p' env =
  (* names are semantically irrelevant. *)
  is_same_predicate_node p.pred_content p'.pred_content env

and is_same_predicate_node p p' env =
  match p, p' with
  | Pfalse, Pfalse -> true
  | Ptrue, Ptrue -> true
  | Papp(p,labs,args), Papp(p',labs',args') ->
    is_matching_logic_info p p' env &&
    is_same_list is_same_logic_label labs labs' env &&
    is_same_list is_same_term args args' env
  | Pseparated t, Pseparated t' -> is_same_list is_same_term t t' env
  | Prel (r,t1,t2), Prel(r',t1',t2') ->
    equal_relation r r' && is_same_term t1 t1' env && is_same_term t2 t2' env
  | Pand(p1,p2), Pand(p1',p2')
  | Por(p1,p2), Por(p1',p2')
  | Pxor(p1,p2), Pxor(p1',p2')
  | Pimplies(p1,p2), Pimplies(p1',p2')
  | Piff(p1,p2), Piff(p1',p2') ->
    is_same_predicate p1 p1' env && is_same_predicate p2 p2' env
  | Pnot p, Pnot p' -> is_same_predicate p p' env
  | Pif(c,p1,p2), Pif(c',p1',p2') ->
    is_same_predicate c c' env &&
    is_same_predicate p1 p1' env &&
    is_same_predicate p2 p2' env
  | Plet(v,p), Plet(v',p') ->
    if is_same_logic_info v v' env then begin
      let env = add_logic_info v v' env in
      let env = add_logic_vars [v.l_var_info] [v'.l_var_info] env in
      is_same_predicate p p' env
    end else false
  | Pforall(q,p), Pforall(q',p')
  | Pexists(q,p), Pexists(q',p') ->
    if is_same_list is_same_logic_var q q' env then begin
      let env = add_logic_vars q q' env in
      is_same_predicate p p' env
    end else false
  | Pat(p,l), Pat(p',l') ->
    is_same_predicate p p' env && is_same_logic_label l l' env
  | Pobject_pointer(l,t), Pobject_pointer(l',t')
  | Pvalid_read(l,t), Pvalid_read(l',t')
  | Pvalid(l,t), Pvalid(l',t')
  | Pinitialized(l,t), Pinitialized(l',t')
  | Pdangling(l,t), Pdangling(l',t')
  | Pallocable(l,t), Pallocable(l',t')
  | Pfreeable(l,t), Pfreeable(l',t') ->
    is_same_logic_label l l' env && is_same_term t t' env
  | Paligned(p, n), Paligned(p', n') ->
    is_same_term p p' env && is_same_term n n' env
  | Pfresh(l1,l2,p,s), Pfresh(l1',l2',p',s') ->
    is_same_logic_label l1 l1' env &&
    is_same_logic_label l2 l2' env &&
    is_same_term p p' env &&
    is_same_term s s' env
  | Pvalid_function(t), Pvalid_function(t') -> is_same_term t t' env
  | (Pfalse | Ptrue | Papp _ | Pseparated _ | Prel _ | Pand _ | Por _ | Pxor _
    | Pimplies _ | Piff _ | Pnot _ | Pif _ | Plet _ | Pforall _ | Pexists _
    | Pat _ | Pobject_pointer _ | Pvalid_read _ | Pvalid _ | Pinitialized _
    | Pdangling _ | Paligned _ | Pallocable _ | Pfreeable _ | Pfresh _
    | Pvalid_function _), _ -> false

and is_same_logic_constant c c' env =
  match c,c' with
  | LEnum ei, LEnum ei' ->
    (match enumitem_correspondence ei env with
     | `Same ei'' -> Cil_datatype.Enumitem.equal ei' ei''
     | `Not_present -> false)
  | LEnum _, _ | _, LEnum _ -> false
  | (Boolean _ | Integer _ | LStr _ | LWStr _ | LChr _ | LReal _), _ ->
    Cil_datatype.Logic_constant.equal c c'

and is_same_term t t' env =
  is_same_term_node t.term_node t'.term_node env

and is_same_term_node t t' env =
  match t,t' with
  | TConst c, TConst c' -> is_same_logic_constant c c' env
  | TLval lv, TLval lv' -> is_same_term_lval lv lv' env
  | TSizeOf t, TSizeOf t'
  | TAlignOf t, TAlignOf t' -> is_same_type t t' env
  | TSizeOfE t, TSizeOfE t'
  | TAlignOfE t, TAlignOfE t' -> is_same_term t t' env
  | TUnOp(op,t), TUnOp(op',t') -> equal_unop op op' && is_same_term t t' env
  | TBinOp(op,t1,t2), TBinOp(op',t1',t2') ->
    equal_binop op op' && is_same_term t1 t1' env && is_same_term t2 t2' env
  | TCast(is_logic, typ, term), TCast(is_logic', typ', term') ->
    is_logic = is_logic' && is_same_logic_type typ typ' env && is_same_term term term' env
  | TAddrOf lv, TAddrOf lv'
  | TStartOf lv, TStartOf lv' -> is_same_term_lval lv lv' env
  | Tapp(f,labs,args), Tapp(f',labs',args') ->
    is_matching_logic_info f f' env &&
    is_same_list is_same_logic_label labs labs' env &&
    is_same_list is_same_term args args' env
  | Tlambda(q,t), Tlambda(q',t') ->
    if is_same_list is_same_logic_var q q' env then begin
      let env = add_logic_vars q q' env in
      is_same_term t t' env
    end else false
  | TDataCons(c,args), TDataCons(c',args') ->
    is_matching_logic_ctor c c' env &&
    is_same_list is_same_term args args' env
  | Tif(c,t1,t2), Tif(c',t1',t2') ->
    is_same_predicate c c' env &&
    is_same_term t1 t1' env &&
    is_same_term t2 t2' env
  | Tat(t,l), Tat(t',l') ->
    is_same_term t t' env && is_same_logic_label l l' env
  | Tbase_addr(l,t), Tbase_addr(l',t')
  | Toffset(l,t), Toffset(l',t')
  | Tblock_length(l,t), Tblock_length(l',t') ->
    is_same_logic_label l l' env && is_same_term t t' env
  | Tnull, Tnull -> true
  | TUpdate(a,o,v), TUpdate(a',o',v') ->
    is_same_term a a' env &&
    is_same_term_offset o o' env &&
    is_same_term v v' env
  | Ttypeof t, Ttypeof t' -> is_same_term t t' env
  | Ttype t, Ttype t' -> is_same_type t t' env
  | Tempty_set, Tempty_set -> true
  | Tunion l, Tunion l'
  | Tinter l, Tinter l' -> is_same_list is_same_term l l' env
  | Tcomprehension(t,q,p), Tcomprehension(t',q',p') ->
    if is_same_list is_same_logic_var q q' env then begin
      let env = add_logic_vars q q' env in
      is_same_term t t' env && is_same_opt is_same_predicate p p' env
    end else false
  | Trange(l,u), Trange(l',u') ->
    is_same_opt is_same_term l l' env && is_same_opt is_same_term u u' env
  | Tlet(v,t), Tlet(v',t') ->
    if is_same_logic_info v v' env then begin
      let env = add_logic_info v v' env in
      let env = add_logic_vars [v.l_var_info] [v'.l_var_info] env in
      is_same_term t t' env
    end else false
  | (TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TAlignOf _
    | TAlignOfE _ |  TUnOp _ | TBinOp _ | TCast _ | TAddrOf _ | TStartOf _
    | Tapp _ | Tlambda _ | TDataCons _ | Tif _ | Tat _ | Tbase_addr _
    | Toffset _ | Tblock_length _ | Tnull | TUpdate _
    | Ttypeof _ | Ttype _ | Tempty_set | Tunion _ | Tinter _ | Tcomprehension _
    | Tlet _ | Trange _), _ -> false

and is_same_term_lval (lh,lo) (lh',lo') env =
  is_same_term_lhost lh lh' env && is_same_term_offset lo lo' env

and is_same_term_lhost lh lh' env =
  match lh, lh' with
  | TVar lv, TVar lv' -> is_matching_logic_var lv lv' env
  | TResult _, TResult _ -> true
  | TMem p, TMem p' -> is_same_term p p' env
  | (TVar _ | TResult _ | TMem _), _ -> false

and is_matching_logic_var lv lv' env =
  match lv.lv_origin, lv'.lv_origin with
  | Some vi, Some vi' -> is_matching_varinfo vi vi' env
  | None, None ->
    (match Cil_datatype.Logic_var.Map.find_opt lv env.logic_local_vars with
     | Some lv'' -> Cil_datatype.Logic_var.equal lv' lv''
     | None ->
       (match Logic_var.find lv with
        | `Not_present -> false
        | `Same lv'' -> Cil_datatype.Logic_var.equal lv' lv''
        | exception Not_found ->
          if lv.lv_name = "\\exit_status" && lv'.lv_name = "\\exit_status"
          then begin Logic_var.add lv (`Same lv'); true end
          else begin
            match logic_var_correspondence lv env with
            | None -> false
            | Some lv'' -> Cil_datatype.Logic_var.equal lv' lv''
          end))
  | _ -> false

and logic_var_correspondence lv env =
  match find_candidate_logic_var lv env with
  | None -> None
  | Some lv' -> Logic_var.add lv (`Same lv'); Some lv'

and is_same_term_offset lo lo' env =
  match lo, lo' with
  | TNoOffset, TNoOffset -> true
  | TField(f,o), TField(f',o') ->
    is_matching_fieldinfo f f' && is_same_term_offset o o' env
  | TModel(f,o), TModel(f',o') ->
    is_matching_model_info f f' && is_same_term_offset o o' env
  | TIndex(i,o), TIndex(i',o') ->
    is_same_term i i' env && is_same_term_offset o o' env
  | (TNoOffset | TField _ | TModel _ | TIndex _), _ -> false

and is_same_toplevel_predicate p p' env =
  equal_predicate_kind p.tp_kind p'.tp_kind &&
  is_same_predicate p.tp_statement p'.tp_statement env

and is_same_identified_predicate p p' env =
  is_same_toplevel_predicate p.ip_content p'.ip_content env

and is_same_identified_term t t' env =
  is_same_term t.it_content t'.it_content env

and is_same_post_cond (k,p) (k',p') env =
  equal_termination_kind k k' && is_same_identified_predicate p p' env

and is_same_deps d d' env =
  match d,d' with
  | FromAny, FromAny -> true
  | From l, From l' -> is_same_list is_same_identified_term l l' env
  | (FromAny | From _), _ -> false

and is_same_from (t,f) (t',f') env =
  is_same_identified_term t t' env && is_same_deps f f' env

and is_same_assigns a a' env =
  match a,a' with
  | WritesAny, WritesAny -> true
  | Writes l, Writes l' -> is_same_list is_same_from l l' env
  | (WritesAny | Writes _), _ -> false

and is_same_allocation a a' env =
  match a,a' with
  | FreeAllocAny, FreeAllocAny -> true
  | FreeAlloc(f,a), FreeAlloc(f',a') ->
    is_same_list is_same_identified_term f f' env &&
    is_same_list is_same_identified_term a a' env
  | (FreeAllocAny | FreeAlloc _),_ -> false

and is_same_behavior b b' env =
  is_same_list is_same_identified_predicate b.b_requires b'.b_requires env &&
  is_same_list is_same_identified_predicate b.b_assumes b'.b_assumes env &&
  is_same_list is_same_post_cond b.b_post_cond b'.b_post_cond env &&
  is_same_assigns b.b_assigns b'.b_assigns env &&
  is_same_allocation b.b_allocation b'.b_allocation env &&
  is_same_list is_same_acsl_extension b.b_extended b'.b_extended env

and is_same_variant (v,m) (v',m') env =
  is_same_term v v' env && is_same_opt is_matching_logic_info m m' env

and are_same_behaviors bhvs bhvs' env =
  let treat_one_behavior acc b =
    match List.partition (fun b' -> b.b_name = b'.b_name) acc with
    | [], _ -> raise Exit
    | [b'], acc ->
      if is_same_behavior b b' env then acc else raise Exit
    | _ ->
      Kernel.fatal "found several behaviors with the same name %s" b.b_name
  in
  try
    match List.fold_left treat_one_behavior bhvs' bhvs with
    | [] -> true
    | _ -> (* new behaviors appeared: spec has changed. *) false
  with Exit -> false

and is_same_funspec s s' env =
  are_same_behaviors s.spec_behavior s'.spec_behavior env &&
  is_same_opt is_same_variant s.spec_variant s'.spec_variant env &&
  is_same_opt is_same_identified_predicate
    s.spec_terminates s'.spec_terminates env &&
  are_same_cd_clauses s.spec_complete_behaviors s'.spec_complete_behaviors &&
  are_same_cd_clauses s.spec_disjoint_behaviors s'.spec_disjoint_behaviors

and is_same_code_annotation a a' env =
  match a.annot_content, a'.annot_content with
  | AAssert (bhvs, p), AAssert(bhvs',p') ->
    is_same_behavior_set bhvs bhvs' && is_same_toplevel_predicate p p' env
  | AStmtSpec (bhvs, s), AStmtSpec(bhvs', s') ->
    is_same_behavior_set bhvs bhvs' && is_same_funspec s s' env
  | AInvariant (bhvs, is_loop, p), AInvariant(bhvs', is_loop', p') ->
    is_same_behavior_set bhvs bhvs' && is_loop = is_loop' &&
    is_same_toplevel_predicate p p' env
  | AVariant v, AVariant v' -> is_same_variant v v' env
  | AAssigns(bhvs, a), AAssigns(bhvs', a') ->
    is_same_behavior_set bhvs bhvs' && is_same_assigns a a' env
  | AAllocation(bhvs, a), AAllocation(bhvs',a') ->
    is_same_behavior_set bhvs bhvs' && is_same_allocation a a' env
  | AExtended (bhvs, is_next, ext),
    AExtended (bhvs', is_next', ext') ->
    is_same_behavior_set bhvs bhvs' && is_next = is_next' &&
    is_same_acsl_extension ext ext' env
  | (AAssert _ | AStmtSpec _ | AInvariant _ | AVariant _ | AAssigns _
    | AAllocation _ | AExtended _), _ -> false

and is_same_logic_type t t' env =
  match t,t' with
  | Ctype t, Ctype t' -> is_same_type t t' env
  | Ltype (t,prms), Ltype (t',prms') ->
    is_matching_logic_type_info t t' env &&
    is_same_list is_same_logic_type prms prms' env
  | Lvar s, Lvar s' -> is_matching_logic_type_var s s' env
  | Lboolean, Lboolean -> true
  | Linteger, Linteger -> true
  | Lreal, Lreal -> true
  | Larrow(args,rt), Larrow(args', rt')  ->
    is_same_list is_same_logic_type args args' env &&
    is_same_logic_type rt rt' env
  | (Ctype _ | Ltype _ | Lvar _ | Linteger | Lboolean | Lreal | Larrow _),_ ->
    false

and is_same_inductive_case (_,labs,tprms,p) (_,labs',tprms',p') env =
  let res, env =
    (is_same_list is_same_logic_label labs labs' env, env) &&&
    logic_type_vars_env tprms tprms'
  in
  res && is_same_predicate p p' env

and is_same_logic_body b b' env =
  match b,b' with
  | LBnone, LBnone -> true
  | LBreads l, LBreads l' ->
    is_same_list is_same_identified_term l l' env
  | LBterm t, LBterm t' -> is_same_term t t' env
  | LBpred p, LBpred p' -> is_same_predicate p p' env
  | LBinductive l, LBinductive l' ->
    is_same_list is_same_inductive_case l l' env
  | (LBnone | LBreads _ | LBterm _ | LBpred _ | LBinductive _), _ -> false

and is_same_logic_ctor_info c c' env =
  (* we rely on order in the type declaration to match constructors,
     not on names. *)
  is_same_list is_same_logic_type c.ctor_params c'.ctor_params env

and is_same_logic_type_def d d' env =
  match d,d' with
  | LTsum l, LTsum l' ->
    if is_same_list is_same_logic_ctor_info l l' env then begin
      List.iter2 (fun c c' -> Logic_ctor_info.add c (`Same c')) l l';
      true
    end else begin
      List.iter (fun c -> Logic_ctor_info.add c `Not_present) l;
      false
    end
  | LTsyn t, LTsyn t' -> is_same_logic_type t t' env
  | (LTsum _ | LTsyn _), _ -> false

and is_same_logic_info li li' env =
  let res,env =
    (is_same_list is_same_logic_label li.l_labels li'.l_labels env, env) &&&
    logic_type_vars_env li.l_tparams li'.l_tparams &&&
    logic_vars_env li.l_profile li'.l_profile
  in
  res && is_same_opt is_same_logic_type li.l_type li'.l_type env &&
  is_same_logic_body li.l_body li'.l_body env

and is_same_logic_type_info ti ti' env =
  let res,env =
    (Cil_datatype.Attributes.equal ti.lt_attr ti'.lt_attr, env) &&&
    logic_type_vars_env ti.lt_params ti'.lt_params
  in
  res && is_same_opt is_same_logic_type_def ti.lt_def ti'.lt_def env

and is_same_model_info mi mi' env =
  is_same_type mi.mi_base_type mi'.mi_base_type env &&
  is_same_logic_type mi.mi_field_type mi'.mi_field_type env &&
  Cil_datatype.Attributes.equal mi.mi_attr mi'.mi_attr

and is_same_type t t' env =
  let is_same_tattr () = Cil_datatype.Attributes.equal t.tattr t'.tattr in
  match t.tnode, t'.tnode with
  | TVoid, TVoid -> is_same_tattr ()
  | TInt ik, TInt ik' ->
    equal_ikind ik ik' && is_same_tattr ()
  | TFloat fk, TFloat fk' ->
    equal_fkind fk fk' && is_same_tattr ()
  | TBuiltin_va_list, TBuiltin_va_list -> is_same_tattr ()
  | TPtr bt, TPtr bt' ->
    is_same_type bt bt' env && is_same_tattr ()
  | TArray (bt, s), TArray (bt', s') ->
    is_same_type bt bt' env &&
    is_same_opt is_same_exp s s' env &&
    is_same_tattr ()
  | TFun (rt, l, var), TFun(rt', l', var') ->
    is_same_type rt rt' env &&
    is_same_opt (is_same_list is_same_formal) l l' env &&
    (var = var') &&
    is_same_tattr ()
  | TNamed ti, TNamed ti' ->
    let correspondence = typeinfo_correspondence ti env in
    (match correspondence with
     | `Not_present -> false
     | `Same ti'' -> Cil_datatype.Typeinfo.equal ti' ti'') &&
    is_same_tattr ()
  | TComp ci, TComp ci' ->
    let correspondence = compinfo_correspondence ci env in
    (match correspondence with
     | `Not_present -> false
     | `Same ci'' -> Cil_datatype.Compinfo.equal ci' ci'') &&
    is_same_tattr ()
  | TEnum ei, TEnum ei' ->
    let correspondence = enuminfo_correspondence ei env in
    (match correspondence with
     | `Not_present -> false
     | `Same ei'' -> Cil_datatype.Enuminfo.equal ei' ei'') &&
    is_same_tattr ()
  | (TVoid | TInt _ | TFloat _ | TBuiltin_va_list | TPtr _ | TArray _
    | TFun _ | TNamed _ | TComp _ | TEnum _), _ -> false

and is_same_compinfo ci ci' env =
  ci.cstruct = ci'.cstruct &&
  Cil_datatype.Attributes.equal ci.cattr ci'.cattr &&
  is_same_opt (is_same_list is_same_fieldinfo) ci.cfields ci'.cfields env

and is_same_enuminfo ei ei' env =
  let res, _ =
    (Cil_datatype.Attributes.equal ei.eattr ei'.eattr &&
     equal_ikind ei.ekind ei'.ekind, env) &&&
    is_same_list_env is_same_enumitem ei.eitems ei'.eitems
  in
  res

and is_same_fieldinfo fi fi' env =
  (* we don't compare names: it's the order in which they appear in the
     definition of the aggregate that counts. *)
  fi.forder = fi'.forder &&
  is_same_type fi.ftype fi'.ftype env &&
  is_same_opt (fun x y _ -> x = y) fi.fbitfield fi'.fbitfield env &&
  Cil_datatype.Attributes.equal fi.fattr fi'.fattr

and is_same_enumitem ei ei' env =
  if is_same_exp ei.eival ei'.eival env then
    true,
    { env with enumitem = Cil_datatype.Enumitem.Map.add ei ei' env.enumitem }
  else
    false, env

and is_same_formal (_,t,a) (_,t',a') env =
  is_same_type t t' env && Cil_datatype.Attributes.equal a a'

and is_same_compound_init (o,i) (o',i') env =
  is_same_offset o o' env && is_same_init i i' env

and is_same_init i i' env =
  match i, i' with
  | SingleInit e, SingleInit e' -> is_same_exp e e' env
  | CompoundInit (t,l), CompoundInit (t', l') ->
    is_same_type t t' env &&
    (is_same_list is_same_compound_init) l l' env
  | (SingleInit _ | CompoundInit _), _ -> false

and is_same_init_or_str i i' env =
  match i,i' with
  | CInit i, CInit i' -> is_same_init i i' env
  | StrInit (Str s), StrInit (Str s') -> String.equal s s'
  | StrInit (Wstr s), StrInit (Wstr s') ->
    is_same_list (fun v v' _ -> Int64.equal v v') s s' env
  | (CInit _ | StrInit _), _ -> false

and is_same_initinfo i i' env =
  is_same_opt is_same_init_or_str i.init i'.init env

and is_same_local_init i i' env =
  match i, i' with
  | AssignInit i, AssignInit i' ->
    if is_same_init i i' env then `Same_body
    else `Body_changed
  | (ConsInit(c,args,Plain_func), ConsInit(c',args',Plain_func))
  | (ConsInit(c,args,Constructor),ConsInit(c',args',Constructor))  ->
    if is_same_varinfo c c' env &&
       is_same_list is_same_exp args args' env
    then begin
      match gfun_correspondence c env with
      | `Partial _ | `Not_present -> `Callees_changed
      | `Same _ -> `Same_body
    end else `Body_changed
  | (AssignInit _| ConsInit _), _ -> `Body_changed

and is_same_constant c c' env =
  match c,c' with
  | CEnum ei, CEnum ei' ->
    (match enumitem_correspondence ei env with
     | `Same ei'' -> Cil_datatype.Enumitem.equal ei' ei''
     | `Not_present -> false)
  | CEnum _, _ | _, CEnum _ -> false
  | (CInt64 _ | CChr _ | CReal _), _ ->
    Cil_datatype.Constant.equal c c'

and is_same_exp e e' env =
  match e.enode, e'.enode with
  | Const c, Const c' -> is_same_constant c c' env
  | Lval lv, Lval lv' -> is_same_lval lv lv' env
  | SizeOf t, SizeOf t' -> is_same_type t t' env
  | SizeOfE e, SizeOfE e' -> is_same_exp e e' env
  | AlignOf (t, i), AlignOf (t', i') ->
    Cil_types.equal_standard_or_gcc i i' &&
    is_same_type t t' env
  | AlignOfE (e, i), AlignOfE (e', i') ->
    Cil_types.equal_standard_or_gcc i i' &&
    is_same_exp e e' env
  | UnOp(op,e,t), UnOp(op',e',t') ->
    equal_unop op op' && is_same_exp e e' env && is_same_type t t' env
  | BinOp(op,e1,e2,t), BinOp(op',e1',e2',t') ->
    equal_binop op op' && is_same_exp e1 e1' env && is_same_exp e2 e2' env
    && is_same_type t t' env
  | CastE(t,e), CastE(t',e') -> is_same_type t t' env && is_same_exp e e' env
  | AddrOf lv, AddrOf lv' -> is_same_lval lv lv' env
  | StartOf lv, StartOf lv' -> is_same_lval lv lv' env
  | (Const _ | Lval _ | SizeOf _ | SizeOfE _ | AlignOf _
    | AlignOfE _ | UnOp _ | BinOp _  | CastE _
    | AddrOf _ | StartOf _),_-> false

and is_same_lval lv lv' env =
  is_same_pair is_same_lhost is_same_offset lv lv' env

and is_same_lhost h h' env =
  match h, h' with
  | Var vi, Var vi' -> is_matching_varinfo vi vi' env
  | Mem p, Mem p' -> is_same_exp p p' env
  | (Var _ | Mem _), _ -> false

and is_matching_varinfo vi vi' env =
  if vi.vglob then begin
    match gvar_correspondence vi env with
    | `Not_present -> false
    | `Same vi'' -> Cil_datatype.Varinfo.equal vi' vi''
  end else begin
    try
      let vi'' = Cil_datatype.Varinfo.Map.find vi env.local_vars in
      Cil_datatype.Varinfo.equal vi' vi''
    with Not_found ->
      Kernel.fatal "Unbound variable %a in AST diff"
        Cil_datatype.Varinfo.pretty vi
  end

and is_same_offset o o' env =
  match o, o' with
  | NoOffset, NoOffset -> true
  | Field (i,o), Field(i',o') ->
    is_matching_fieldinfo i i' && is_same_offset o o' env
  | Index(i,o), Index(i',o') ->
    is_same_exp i i' env && is_same_offset o o' env
  | (NoOffset | Field _ | Index _), _ -> false

and is_same_extended_asm a a' env =
  let is_same_out (_,c,l) (_,c',l') env =
    Datatype.String.equal c c' && is_same_lval l l' env
  and is_same_in (_,c,e) (_,c',e') env =
    Datatype.String.equal c c' && is_same_exp e e' env
  in
  let res =
    is_same_list is_same_out a.asm_outputs a'.asm_outputs env &&
    is_same_list is_same_in a.asm_inputs a'.asm_inputs env &&
    is_same_list
      (fun s1 s2 _ -> Datatype.String.equal s1 s2)
      a.asm_clobbers a'.asm_clobbers env
    &&
    List.length a.asm_gotos = List.length a'.asm_gotos
  in
  let bind s s' env =
    (true, { env with goto_targets = (!s,!s') :: env.goto_targets})
  in
  (res, env) &&& is_same_list_env bind a.asm_gotos a'.asm_gotos

and is_same_instr i i' env: body_correspondence*is_same_env =
  match i, i' with
  | Set(lv,e,_), Set(lv',e',_) ->
    if is_same_lval lv lv' env && is_same_exp e e' env then
      `Same_body, env
    else
      `Body_changed, env
  | Call(lv,f,args,_), Call(lv',f',args',_) ->
    if is_same_opt is_same_lval lv lv' env &&
       is_same_list is_same_exp args args' env
    then begin
      match f, f' with
      | Var f, Var f' ->
        (match gfun_correspondence f env with
         | `Partial _ | `Not_present -> `Callees_changed, env
         | `Same f'' ->
           if Cil_datatype.Varinfo.equal f' (Kf.get_vi f'') then
             `Same_body, env
           else
             `Callees_changed, env)
      | _ -> `Callees_changed, env
      (* by default, we consider that indirect call might have changed *)
    end else `Body_changed, env
  | Local_init(v,i,_), Local_init(v',i',_) ->
    if is_same_varinfo v v' env then begin
      let env = add_locals [v] [v'] env in
      let res = is_same_local_init i i' env in
      res, env
    end else `Body_changed, env
  | Asm(a,c,e,_), Asm(a',c',e',_) ->
    let res =
      Cil_datatype.Attributes.equal a a' &&
      is_same_list (fun s1 s2 _ -> Datatype.String.equal s1 s2) c c' env
    in
    let (res,env) = (res,env) &&& is_same_opt_env is_same_extended_asm e e' in
    if res then `Same_body, env else `Body_changed, env
  | Skip _, Skip _ -> `Same_body, env
  | Code_annot _, Code_annot _ ->
    (* should not be present in normalized AST *)
    Kernel.fatal "Unexpected Code_annot instruction in normalized AST"
  | _ -> `Body_changed, env

and is_same_instr_list l l' env =
  match l, l' with
  | [], [] -> `Same_body, env
  | [], _ | _, [] -> `Body_changed, env
  | i::tl, i'::tl' ->
    is_same_instr i i' env &&> is_same_instr_list tl tl'

and same_switch_labels l l' env =
  let l = List.filter Cil.is_case_label l in
  let l' = List.filter Cil.is_case_label l' in
  let is_same_label lab lab' =
    match lab, lab' with
    | (Label _ as l, _) |  (_, (Label _ as l)) ->
      Kernel.fatal "Label %a should have been filtered beforehand"
        Printer.pp_label l
    | Case(e,_), Case(e', _) -> is_same_exp e e' env
    | Default _, Default _ -> true
    | (Case _ | Default _), _ -> false
  in
  let exist_same_label l lab = List.exists (is_same_label lab) l in
  (* it is not sufficient to ensure that any case label present in the original
     also exists in the new AST: we also must check that no new label was
     introduced that would change the cfg for this case.
  *)
  List.for_all (exist_same_label l') l &&
  List.for_all (exist_same_label l) l'

and is_same_stmt s s' env =
  let selection =
    State_selection.with_codependencies Annotations.code_annot_state
  in
  let annots =
    Project.on ~selection (Orig_project.get()) Annotations.code_annot s
  in
  let annots' = Annotations.code_annot s' in
  let annot_res = is_same_list is_same_code_annotation annots annots' env in
  let code_res, env =
    if s.ghost = s'.ghost && Cil_datatype.Attributes.equal s.sattr s'.sattr &&
       same_switch_labels s.labels s'.labels env
    then
      begin
        match s.skind,s'.skind with
        | Instr i, Instr i' -> is_same_instr i i' env
        | Return (r,_), Return(r', _) ->
          if is_same_opt is_same_exp r r' env then `Same_body, env
          else `Body_changed, env
        | Goto (s,_), Goto(s',_) ->
          `Same_body, { env with goto_targets = (!s,!s') :: env.goto_targets }
        | Break _, Break _ -> `Same_body, env
        | Continue _, Continue _ -> `Same_body, env
        | If(e,b1,b2,_), If(e',b1',b2',_) ->
          if is_same_exp e e' env then begin
            is_same_block b1 b1' env &&>
            is_same_block b2 b2'
          end else `Body_changed, env
        | Switch(e,b,_,_), Switch(e',b',_,_) ->
          if is_same_exp e e' env then begin
            is_same_block b b' env
          end else `Body_changed, env
        | Loop(_,b,_,_,_), Loop(_,b',_,_,_) ->
          is_same_block b b' env
        | Block b, Block b' ->
          is_same_block b b' env
        | UnspecifiedSequence l, UnspecifiedSequence l' ->
          let b = Cil.block_from_unspecified_sequence l in
          let b' = Cil.block_from_unspecified_sequence l' in
          is_same_block b b' env
        | Throw (e,_), Throw (e',_) ->
          if is_same_opt (is_same_pair is_same_exp is_same_type) e e' env then
            `Same_body, env
          else `Body_changed, env
        | TryCatch (b,c,_), TryCatch(b',c',_) ->
          let rec is_same_catch_list l l' env =
            match l, l' with
            | [], [] -> `Same_body, env
            | [],_ | _, [] -> `Body_changed, env
            | (bind, b) :: tl, (bind', b') :: tl' ->
              is_same_binder bind bind' env &&>
              is_same_block b b' &&>
              is_same_catch_list tl tl'
          in
          is_same_block b b' env &&> is_same_catch_list c c'
        | TryFinally(b1,b2,_), TryFinally(b1',b2',_) ->
          is_same_block b1 b1' env &&>
          (is_same_block b2 b2')
        | TryExcept(b1,(h,e),b2,_), TryExcept(b1',(h',e'),b2',_) ->
          if is_same_exp e e' env then begin
            is_same_block b1 b1' env &&>
            is_same_instr_list h h' &&>
            is_same_block b2 b2'
          end else `Body_changed, env
        | _ -> `Body_changed, env
      end else `Body_changed, env
  in
  let corres = make_correspondence s' annot_res code_res in
  let res = make_body_correspondence annot_res code_res in
  Stmt.add s corres; res, env

(* is_same_block will return its modified environment in order
   to update correspondence table with respect to locals, in case
   the body of the enclosing function is unchanged. *)
and is_same_block b b' env =
  let local_decls = List.filter (fun x -> not x.vdefined) b.blocals in
  let local_decls' = List.filter (fun x -> not x.vdefined) b'.blocals in
  if is_same_list is_same_varinfo b.bstatics b'.bstatics env &&
     Cil_datatype.Attributes.equal b.battrs b'.battrs
  then begin
    let res, env = is_same_list_env varinfo_env local_decls local_decls' env in
    if res then begin
      add_statics b.bstatics b'.bstatics;
      let rec is_same_stmts l l' env =
        match l, l' with
        | [], [] -> `Same_body,env
        | [], _ | _, [] -> `Body_changed, env
        | s :: tl, s' :: tl' ->
          is_same_stmt s s' env &&> (is_same_stmts tl tl')
      in
      is_same_stmts b.bstmts b'.bstmts env
    end else `Body_changed, env
  end else `Body_changed, env

and is_same_binder b b' env =
  match b, b' with
  | Catch_exn(v,conv), Catch_exn(v', conv') ->
    if is_same_varinfo v v' env then begin
      let env = add_locals [v] [v'] env in
      let rec is_same_conv l l' env =
        match l, l' with
        | [], [] -> `Same_body, env
        | [], _ | _, [] -> `Body_changed, env
        | (v,b)::tl, (v',b')::tl' ->
          if is_same_varinfo v v' env then begin
            let env = add_locals [v] [v'] env in
            is_same_block b b' env &&>
            (is_same_conv tl tl')
          end else `Body_changed, env
      in
      is_same_conv conv conv' env
    end else `Body_changed, env
  | Catch_all, Catch_all -> `Same_body, env
  | (Catch_exn _ | Catch_all), _ -> `Body_changed, env

(* correspondence of formals is supposed to have already been checked,
   and formals mapping to have been put in the local env
*)
and is_same_fundec f f' env: body_correspondence =
  (* The goto_targets field of [env] accumulates visited goto targets to be
     verified after the function body. If the given environment is not empty,
     resets this field for this function. *)
  let env = { env with goto_targets = [] } in
  let res, env =
    is_same_block f.sbody f'.sbody env &&>
    check_goto_targets
  in
  (* Since we add the locals only if the body is the same,
     we have explored all nodes, and added all locals bindings.
     Otherwise, is_same_block would have returned `Body_changed.
     Hence [Not_found] cannot be raised. *)
  let add_local v =
    let v' = Cil_datatype.Varinfo.Map.find v env.local_vars in
    Varinfo.add v (`Same v')
  in
  (match res with
   | `Same_body | `Callees_changed ->
     List.iter add_local f.slocals
   | `Body_changed -> ());
  res

and is_same_varinfo vi vi' env =
  is_same_type vi.vtype vi'.vtype env &&
  Cil_datatype.Attributes.equal vi.vattr vi'.vattr

and varinfo_env vi vi' env =
  if is_same_varinfo vi vi' env then true, add_locals [vi] [vi'] env
  else false, env

and is_same_logic_var lv lv' env =
  is_same_logic_type lv.lv_type lv'.lv_type env &&
  Cil_datatype.Attributes.equal lv.lv_attr lv'.lv_attr

and logic_vars_env l l' env =
  if is_same_list is_same_logic_var l l' env then
    true, add_logic_vars l l' env
  else
    false, env

and find_candidate_logic_var ?loc:_loc lv env =
  let candidates = Logic_env.find_all_logic_functions lv.lv_name in
  match List.find_opt (fun li -> li.l_profile = []) candidates with
  | None -> None
  | Some li ->
    if is_same_logic_var lv li.l_var_info env then
      Some li.l_var_info
    else None

(* because of overloading, we have to check for a corresponding profile,
   leading to potentially recursive calls to is_same_* functions. *)
and find_candidate_logic_info ?loc:_loc li env =
  let candidates = Logic_env.find_all_logic_functions li.l_var_info.lv_name in
  let find_one li' =
    let res, env = logic_type_vars_env li.l_tparams li'.l_tparams env in
    res && is_same_list is_same_logic_var li.l_profile li'.l_profile env
    && is_same_opt is_same_logic_type li.l_type li'.l_type env
  in
  List.find_opt find_one candidates

and find_candidate_model_info ?loc:_loc mi env =
  let candidates = Logic_env.find_all_model_fields mi.mi_name in
  let find_one mi' =
    is_same_type mi.mi_base_type mi'.mi_base_type env
  in
  List.find_opt find_one candidates

and typeinfo_correspondence ?loc ti env =
  let add ti =
    match find_candidate_type ?loc ti with
    | None -> `Not_present
    | Some ti' ->
      let res = is_same_type ti.ttype ti'.ttype env in
      if res then `Same ti' else `Not_present
  in
  Typeinfo.memo add ti

and compinfo_correspondence ?loc ci env =
  let add ci =
    match find_candidate_compinfo ?loc ci with
    | None -> `Not_present
    | Some ci' ->
      let env' =
        {env with compinfo = Cil_datatype.Compinfo.Map.add ci ci' env.compinfo}
      in
      let res = is_same_compinfo ci ci' env' in
      if res then begin
        (match ci.cfields, ci'.cfields with
         | Some fl, Some fl' ->
           (* by definition, if is_same_compinfo returns true,
              we have the same number of fields in ci and ci'. *)
           List.iter2 (fun fi fi' -> Fieldinfo.add fi (`Same fi')) fl fl'
         | _ -> ());
        `Same ci'
      end else begin
        (* fields are considered different, even if it might be possible
           to consider that the beginning of the struct hasn't changed. *)
        (match ci.cfields with
         | Some fl -> List.iter (fun fi -> Fieldinfo.add fi `Not_present) fl
         | None -> ());
        `Not_present
      end
  in
  match Cil_datatype.Compinfo.Map.find_opt ci env.compinfo with
  | Some ci' -> `Same ci'
  | None -> Compinfo.memo add ci

and enuminfo_correspondence ?loc ei env =
  let add ei =
    match find_candidate_enuminfo ?loc ei with
    | None -> `Not_present
    | Some ei' ->
      if is_same_enuminfo ei ei' env then begin
        (* add items correspondence. By definition, we have
           the same number of items here. *)
        List.iter2 (fun ei ei' -> Enumitem.add ei (`Same ei'))
          ei.eitems ei'.eitems;
        `Same ei'
      end else begin
        (* consider that all items are different.
           Might be refined at some point. *)
        List.iter (fun ei -> Enumitem.add ei `Not_present) ei.eitems;
        `Not_present
      end
  in
  Enuminfo.memo add ei

(* For now, all enumitems are treated with their parent enuminfo: if
   we don't find one, we let enuminfo_correspondence do the job. *)
and enumitem_correspondence ?loc ei env =
  let add ei =
    match enuminfo_correspondence ?loc ei.eihost env with
    | `Not_present -> `Not_present
    | `Same _ -> Enumitem.find ei
  in
  match Cil_datatype.Enumitem.Map.find_opt ei env.enumitem with
  | Some ei' -> `Same ei'
  | None ->
    (try Enumitem.find ei with
     | Not_found -> add ei)

and gvar_correspondence ?loc vi env =
  let add vi =
    match find_candidate_varinfo ?loc vi Cil_types.Global with
    | None when Ast_types.is_fun vi.vtype ->
      begin
        match gfun_correspondence ?loc vi env with
        | `Same kf' -> `Same (Kf.get_vi kf')
        | `Partial(kf',_) ->
          (* a partial match at kf level is still the
             identity for the underlying varinfo *)
          `Same (Kf.get_vi kf')
        | `Not_present -> `Not_present
      end
    | None -> `Not_present
    | Some vi' ->
      if is_same_varinfo vi vi' env
      then
        let selection = State_selection.singleton Globals.Vars.self in
        let init =
          Project.on ~selection (Orig_project.get()) Globals.Vars.find vi
        in
        let init' = Globals.Vars.find vi' in
        let res = is_same_initinfo init init' env in
        if res then `Same vi' else `Not_present
      else `Not_present
  in
  Varinfo.memo add vi

and gfun_correspondence ?loc vi env =
  (* NB: we also take care of the correspondence between the underlying varinfo,
     in case we have to refer to it directly, e.g. as an AddrOf argument.
  *)
  let kf = get_original_kf vi in
  let add kf =
    match find_candidate_func ?loc kf with
    | None -> Varinfo.add vi `Not_present; `Not_present
    | Some kf' ->
      let formals = Kf.get_formals kf in
      let formals' = Kf.get_formals kf' in
      let res, env =
        (is_same_type (Kf.get_return_type kf) (Kf.get_return_type kf') env, env)
        &&& is_same_list_env varinfo_env formals formals'
      in
      if res then begin
        (* from a variable point of view, e.g. if we take its address,
           they are similar *)
        Varinfo.add vi (`Same (Kf.get_vi kf'));
        (* we only add formals to global correspondence tables if some
           part of the kf is unchanged (otherwise, we can't reuse information
           about the formals anyways). Hence, we only add them into the local
           env for now. *)
        let env =
          { env with kernel_function = Kf.Map.add kf kf' env.kernel_function }
        in
        let same_spec = is_same_funspec kf.spec kf'.spec env in
        let same_body =
          match (Kf.has_definition kf, Kf.has_definition kf') with
          | false, false -> `Same_body
          | false, true | true, false -> `Body_changed
          | true, true ->
            is_same_fundec (Kf.get_definition kf) (Kf.get_definition kf') env
        in
        let res = make_correspondence kf' same_spec same_body in
        (match res with
         | `Not_present -> ()
         | `Same _ | `Partial _ -> formals_correspondence formals formals');
        res
      end else begin
        (* signatures do not match, we consider that pointers
           are not equivalent. *)
        Varinfo.add vi `Not_present;
        `Not_present
      end
  in
  match Kf.Map.find_opt kf env.kernel_function with
  | Some kf' -> `Same kf'
  | None -> Kernel_function.memo add kf

and is_matching_logic_info li li' env =
  match Cil_datatype.Logic_info.Map.find_opt li env.logic_info with
  | None ->
    (match Logic_info.find li with
     | `Not_present -> false
     | `Same li'' -> Cil_datatype.Logic_info.equal li' li''
     | exception Not_found ->
       begin
         let res = logic_info_correspondence li env in
         Logic_info.add li res;
         match res with
         | `Not_present -> false
         | `Same li'' -> Cil_datatype.Logic_info.equal li' li''
       end)
  | Some li'' -> Cil_datatype.Logic_info.equal li' li''

and logic_info_correspondence ?loc li env =
  let add li =
    match find_candidate_logic_info ?loc li env with
    | None -> `Not_present
    | Some li' ->
      let env =
        { env with
          logic_info=Cil_datatype.Logic_info.Map.add li li' env.logic_info }
      in
      let res = is_same_logic_info li li' env in
      if res then begin
        logic_prms_correspondence li.l_profile li'.l_profile;
        `Same li'
      end else `Not_present
  in
  match Cil_datatype.Logic_info.Map.find_opt li env.logic_info with
  | Some li' -> `Same li'
  | None -> Logic_info.memo add li

and is_matching_logic_ctor c c' env =
  match Logic_ctor_info.find c with
  | `Not_present -> false
  | `Same c'' -> Cil_datatype.Logic_ctor_info.equal c' c''
  | exception Not_found ->
    let ty = c.ctor_type in
    let res = logic_type_correspondence ty env in
    Logic_type_info.add ty res;
    if not (Logic_ctor_info.mem c) then
      Kernel.fatal "Unbound logic type constructor %a in AST diff"
        Cil_datatype.Logic_ctor_info.pretty c;
    is_matching_logic_ctor c c' env

and is_matching_logic_type_info t t' env =
  match Logic_type_info.find t with
  | `Not_present -> false
  | `Same t'' -> Cil_datatype.Logic_type_info.equal t' t''
  | exception Not_found ->
    (match Cil_datatype.Logic_type_info.Map.find_opt t env.logic_type_info with
     | Some t'' -> Cil_datatype.Logic_type_info.equal t' t''
     | None ->
       let res = logic_type_correspondence t env in
       Logic_type_info.add t res;
       (match res with
        | `Same t'' -> Cil_datatype.Logic_type_info.equal t' t''
        | `Not_present -> false))

and logic_type_correspondence ?loc ti env =
  let add ti =
    match find_candidate_logic_type ?loc ti with
    | None -> `Not_present
    | Some ti' ->
      let env =
        { env with
          logic_type_info =
            Cil_datatype.Logic_type_info.Map.add ti ti' env.logic_type_info }
      in
      let res = is_same_logic_type_info ti ti' env in
      (* In case of a sum type, the constructors table
         is updated by is_same_logic_type_info. *)
      if res then `Same ti' else `Not_present
  in
  match Cil_datatype.Logic_type_info.Map.find_opt ti env.logic_type_info with
  | Some ti' -> `Same ti'
  | None -> Logic_type_info.memo add ti

let model_info_correspondence ?loc mi =
  let add mi =
    match find_candidate_model_info ?loc mi empty_env with
    | None -> `Not_present
    | Some mi' ->
      let res = is_same_model_info mi mi' empty_env in
      if res then `Same mi' else `Not_present
  in
  Model_info.memo add mi

let rec gannot_correspondence =
  function
  | Dfun_or_pred (li,loc) ->
    ignore (logic_info_correspondence ~loc li empty_env)

  | Dvolatile _ -> ()
  (* reading and writing function themselves will be checked elsewhere. *)

  | Daxiomatic(_,l,_,_) | Dmodule(_,l,_,_,_) ->
    List.iter gannot_correspondence l
  (* TODO: for modules, we should check the loader if it exists. But like
     for lemmas, we don't have an appropriate structure to store the info *)
  | Dtype (ti,loc) -> ignore (logic_type_correspondence ~loc ti empty_env)
  | Dlemma _ -> ()
  (* TODO: we currently do not have any appropriate structure for
     storing information about lemmas. *)
  | Dinvariant(li, loc) ->
    ignore (logic_info_correspondence ~loc li empty_env)
  | Dtype_annot(li,loc) ->
    ignore (logic_info_correspondence ~loc li empty_env)
  | Dmodel_annot (mi,loc) ->
    ignore (model_info_correspondence ~loc mi)
  | Dextended _ -> () (* TODO: as for lemmas, we don't really have a structure
                         where to look for a matching extended annotation. *)

let global_correspondence g =
  match g with
  | GType (ti,loc) -> ignore (typeinfo_correspondence ~loc ti empty_env)
  | GCompTag(ci,loc) | GCompTagDecl(ci,loc) ->
    ignore (compinfo_correspondence ~loc ci empty_env)
  | GEnumTag(ei,loc) | GEnumTagDecl(ei,loc) ->
    ignore (enuminfo_correspondence ~loc ei empty_env)
  | GVar(vi,_,loc) | GVarDecl(vi,loc) ->
    ignore (gvar_correspondence ~loc vi empty_env)
  | GFunDecl(_,vi,loc) -> ignore (gfun_correspondence ~loc vi empty_env)
  | GFun(f,loc) -> ignore (gfun_correspondence ~loc f.svar empty_env)
  | GAnnot (g,_) -> gannot_correspondence g
  | GAsm _ | GPragma _ | GText _ -> ()


let compare_ast () =
  let prj = Orig_project.get () in
  let ast = Project.on prj Ast.get () in
  Cil.iterGlobals ast global_correspondence

let compare_from_prj prj =
  Orig_project.set prj;
  compare_ast ()

let prepare_project () =
  if Kernel.AstDiff.get () then begin
    let orig = Project.create_by_copy ~last:false
        ("orig_" ^ Project.get_name (Project.current()))
    in
    Orig_project.set orig
  end

let () = Cmdline.run_after_configuring_stage prepare_project

let compute_diff _ =
  if Kernel.AstDiff.get () then begin
    Ast.compute (); compare_ast ()
  end

let () = Cmdline.run_after_setting_files compute_diff
