(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(* Modified by TrustInSoft *)

(*
 * CIL: An intermediate language for analyzing C programs.
 *
 * Version Tue Dec 12 15:21:52 PST 2000
 * Scott McPeak, George Necula, Wes Weimer
 *
 *)

open Logic_const
open Cil_datatype
open Cil_types

(* ************************************************************************* *)
(* Reporting messages *)
(* ************************************************************************* *)

(* Set this to true to check that your code correctly calls some of the
   functions below. *)
let check_invariants = false

let pp_thisloc fmt = Fileloc.pretty fmt (Current_loc.get ())

let new_exp ~loc e = { eloc = loc; eid = Cil_const.new_raw_eid (); enode = e }

let dummy_exp e = { eid = -1; enode = e; eloc = Fileloc.unknown }

let argsToList :
  (string * typ * attributes) list option -> (string * typ * attributes) list =
  Option.value ~default:[]

(* A hack to allow forward reference of d_exp *)
let pp_typ_ref = Extlib.mk_fun "Cil.pp_typ_ref"
let pp_binop_ref = Extlib.mk_fun "Cil.pp_binop_ref"
let pp_global_ref = Extlib.mk_fun "Cil.pp_global_ref"
let pp_exp_ref = Extlib.mk_fun "Cil.pp_exp_ref"
let pp_lval_ref = Extlib.mk_fun "Cil.pp_lval_ref"
let pp_ikind_ref = Extlib.mk_fun "Cil.pp_ikind_ref"
let pp_attribute_ref = Extlib.mk_fun "Cil.pp_attribute_ref"
let pp_attributes_ref = Extlib.mk_fun "Cil.pp_attributes_ref"
let pp_term_ref = Extlib.mk_fun "Cil.pp_term_ref"
let pp_logic_type_ref = Extlib.mk_fun "Cil.pp_logic_type_ref"
let pp_identified_term_ref = Extlib.mk_fun "Cil.pp_identified_term_ref"
let pp_from_ref = Extlib.mk_fun "Cil.pp_from_ref"
let pp_behavior_ref = Extlib.mk_fun "Cil.pp_behavior_ref"
let pp_block_ref = Extlib.mk_fun "Cil.pp_block_ref"
let pp_varinfo_ref = Extlib.mk_fun "Cil.pp_varinfo_ref"

let default_behavior_name = "default!"
let is_default_behavior b =
  default_behavior_name = b.b_name && b.b_assumes = []

let find_default_behavior spec =
  List.find_opt is_default_behavior spec.spec_behavior

let find_default_requires behaviors =
  try (List.find is_default_behavior behaviors).b_requires
  with Not_found -> []

let rec addOffset (toadd: offset) (off: offset) : offset =
  match off with
  | NoOffset -> toadd
  | Field(fid', offset) -> Field(fid', addOffset toadd offset)
  | Index(e, offset) -> Index(e, addOffset toadd offset)

let mkBlock (slst: stmt list) : block =
  { battrs = []; bstmts = slst; blocals = []; bstatics = []; bscoping = true }

let mkBlockNonScoping l = let b = mkBlock l in b.bscoping <- false; b

let mkStmt ?(ghost=false) ?(valid_sid=false) ?(sattr=[]) (sk: stmtkind) : stmt =
  { skind = sk;
    labels = [];
    (* It is better to create statements with a valid sid, so that they can
       safely be used in tables. I only do it when performing Jessie
       analysis, as other plugins rely on specific sid values for their tests
       (e.g. slicing). *)
    sid = if valid_sid then Cil_const.new_raw_sid () else -1;
    succs = []; preds = [];
    ghost = ghost;
    sattr = sattr;}

let stmt_of_instr_list ?(loc=Fileloc.unknown) = function
  | [] -> Instr (Skip loc)
  | [i] -> Instr i
  | il ->
    let b = mkBlockNonScoping (List.map (fun i -> mkStmt (Instr i)) il) in
    Block b

(**** Utility functions ******)

let is_ghost_else block =
  Ast_attributes.contains Ast_attributes.frama_c_ghost_else block.battrs

let rec enforceGhostStmtCoherence ?(force_ghost=false) stmt =
  let force_ghost = force_ghost || stmt.ghost in
  stmt.ghost <- force_ghost ;
  begin match stmt.skind with
    | Break(_) | Continue(_) | Goto(_) | Throw(_)
    | Instr(_) | Return(_) -> ()
    | UnspecifiedSequence(_) -> ()
    | If(_, b1, b2, _) | TryFinally(b1, b2, _) | TryExcept(b1, _, b2, _) ->
      enforceGhostBlockCoherence ~force_ghost b1 ;
      enforceGhostBlockCoherence ~force_ghost b2
    | Switch(_, b, _, _) | Loop(_, b, _, _, _) | Block(b) ->
      enforceGhostBlockCoherence ~force_ghost b
    | TryCatch(b, l, _) ->
      enforceGhostBlockCoherence ~force_ghost b ;
      List.iter (fun (_, b) -> enforceGhostBlockCoherence ~force_ghost b) l
  end
and enforceGhostBlockCoherence ?(force_ghost=false) block =
  let force_ghost = force_ghost || is_ghost_else block  in
  List.iter (enforceGhostStmtCoherence ~force_ghost) block.bstmts

(* makes sure that the type of a C variable and the type of its associated
   logic variable -if any- stay synchronized. See bts 1538 *)
let update_var_type v t =
  v.vtype <- if v.vghost then Ast_types.add_ghost t else t;
  match v.vlogic_var_assoc with
  | None -> ()
  | Some lv ->
    (* ghost attribute is irrelevant in ACSL. *)
    lv.lv_type <- Ctype t

(* Make a varinfo. Used mostly as a helper function below  *)
let makeVarinfo
    ?(source=true) ?(temp=false) ?(referenced=false) ?(ghost=false) ?alignas
    ?(loc=Fileloc.unknown)
    global formal name typ
  =

  let vi =
    { vorig_name = name;
      vname = name;
      vid   = -1;
      vglob = global;
      vdefined = false;
      vformal = formal;
      vtemp = temp;
      vtype = if ghost then Ast_types.add_ghost typ else typ;
      vdecl = loc;
      vinline = false;
      vattr = [];
      vstorage = NoStorage;
      valignas = alignas;
      vaddrof = false;
      vreferenced = referenced;
      vdescr = None;
      vdescrpure = true;
      vghost = ghost;
      vsource = source;
      vlogic_var_assoc = None
    }
  in
  Cil_const.set_vid vi;
  vi

module FormalsDecl =
  State_builder.Hashtbl
    (Varinfo.Hashtbl)
    (Datatype.List(Varinfo))
    (struct
      let name = "Cil.FormalsDecl"
      let dependencies = [] (* depends on Ast.self; see below *)
      let size = 47
    end)

let selfFormalsDecl = FormalsDecl.self

let makeFormalsVarDecl ?ghost (n,t,a) =
  let vi = makeVarinfo ?ghost ~temp:false false true n t in
  vi.vattr <- a;
  vi

let isGhostFormalVarinfo vi =
  Ast_attributes.(contains frama_c_ghost_formal vi.vattr)

let isGhostFormalVarDecl (_name, _type, attr) =
  Ast_attributes.(contains frama_c_ghost_formal attr)

let setFormalsDecl vi typ =
  match Ast_types.unroll_skel typ with
  | TFun (_, Some args, _) ->
    let is_ghost d = vi.vghost || isGhostFormalVarDecl d in
    let makeFormalsVarDecl i (n,t,a as x) =
      let x = if n = "" then begin
          let a  = Ast_attributes.(add anonymous_attribute a) in
          "__x" ^ string_of_int i,t,a
        end
        else x
      in
      makeFormalsVarDecl ~ghost:(is_ghost x) x
    in
    FormalsDecl.replace vi (List.mapi makeFormalsVarDecl args)
  | TFun (_, None, _) -> ()
  | _ ->
    Kernel.error ~current:true
      "trying to assigns formal parameters to an object \
       that is not a function prototype"

let getFormalsDecl vi = FormalsDecl.find vi

let unsafeSetFormalsDecl vi args = FormalsDecl.replace vi args

let removeFormalsDecl vi = FormalsDecl.remove vi

let iterFormalsDecl = FormalsDecl.iter

let () = Cil_datatype.Kf.set_formal_decls := unsafeSetFormalsDecl

(* Set the formals and re-create the function name based on the information*)
let setFormals (f: fundec) (forms: varinfo list) =
  unsafeSetFormalsDecl f.svar forms;
  List.iter (fun v -> v.vformal <- true) forms;
  f.sformals <- forms; (* Set the formals *)
  match Ast_types.unroll f.svar.vtype with
  | { tnode = TFun (rt, _, isva); tattr } ->
    let args = Some (List.map (fun a -> (a.vname, a.vtype, a.vattr)) forms) in
    let t' = Cil_const.mk_tfun ~tattr rt args isva in
    update_var_type f.svar t'
  | _ ->
    Kernel.fatal "Set formals. %s does not have function type" f.svar.vname

let empty_funspec () =
  { spec_behavior = [];
    spec_variant = None;
    spec_terminates = None;
    spec_complete_behaviors = [];
    spec_disjoint_behaviors = [] }

let no_behavior l =
  match l with
  | [] -> true
  | [ b ] ->
    b.b_name = default_behavior_name &&
    b.b_requires = [] &&
    b.b_post_cond = [] &&
    b.b_assigns = WritesAny &&
    b.b_allocation = FreeAllocAny &&
    b.b_extended = []
  | _ -> false

let is_empty_funspec (spec : funspec) =
  (no_behavior spec.spec_behavior) &&
  spec.spec_variant = None && spec.spec_terminates = None &&
  spec.spec_complete_behaviors = [] && spec.spec_disjoint_behaviors = []

let is_empty_behavior b =
  b.b_assumes = [] && b.b_requires = [] && b.b_post_cond = [] &&
  b.b_assigns = WritesAny && b.b_allocation = FreeAllocAny && b.b_extended = []

let missingFieldName = "" (* "___missing_field_name"*)

(** Get the full name of a comp *)
let compFullName comp =
  (if comp.cstruct then "struct " else "union ") ^ comp.cname

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc. *)
type 'a visitAction =
    SkipChildren                        (** Do not visit the children. Return
                                            the node as it is. *)
  | DoChildren                          (** Continue with the children of this
                                            node. Rebuild the node on return
                                            if any of the children changes
                                            (use == test) *)
  | DoChildrenPost of ('a -> 'a)
  | JustCopy
  | JustCopyPost of ('a -> 'a)
  | ChangeTo of 'a                      (** Replace the expression with the
                                            given one *)
  | ChangeToPost of 'a * ('a -> 'a)

  | ChangeDoChildrenPost of 'a * ('a -> 'a) (** First consider that the entire
                                                exp is replaced by the first
                                                parameter. Then continue with
                                                the children. On return rebuild
                                                the node if any of the children
                                                has changed and then apply the
                                                function on the node *)


let id = Fun.id
let alphabetabeta _ x = x
let alphabetafalse _ _ = false
let alphatrue _ = true
let alphafalse _ = false

module Extensions = struct
  let initialized = ref false
  let ref_visit = ref (fun ~plugin:_ _ _ _ -> assert false)

  let set_handler ~visit =
    assert (not !initialized) ;
    ref_visit := visit ;
    initialized := true ;
    ()

  let visit ~plugin name = !ref_visit ~plugin name

end
let set_extension_handler = Extensions.set_handler

(* sm/gn: cil visitor interface for traversing Cil trees. *)
(* Use visitCilStmt and/or visitCilFile to use this. *)
(* Some of the nodes are changed in place if the children are changed. Use
 * one of Change... actions if you want to copy the node *)

(** A visitor interface for traversing CIL trees. Create instantiations of
 * this type by specializing the class {!Cil.nopCilVisitor}. *)
class type cilVisitor = object
  method behavior: Visitor_behavior.t

  method project: Project.t option

  method plain_copy_visitor: cilVisitor

  method vfile: file -> file visitAction
  (** visit a file. *)

  method vvdec: varinfo -> varinfo visitAction
  (** Invoked for each variable declaration. The subtrees to be traversed
   * are those corresponding to the type and attributes of the variable.
   * Note that variable declarations are all the [GVar], [GVarDecl], [GFun],
   * all the [varinfo] in formals of function types, and the formals and
   * locals for function definitions. This means that the list of formals
   * in a function definition will be traversed twice, once as part of the
   * function type and second as part of the formals in a function
   * definition. *)

  method vvrbl: varinfo -> varinfo visitAction
  (** Invoked on each variable use. Here only the [SkipChildren] and
   * [ChangeTo] actions make sense since there are no subtrees. Note that
   * the type and attributes of the variable are not traversed for a
   * variable use *)

  method vexpr: exp -> exp visitAction
  (** Invoked on each expression occurrence. The subtrees are the
   * subexpressions, the types (for a [Cast] or [SizeOf] expression) or the
   * variable use. *)

  method vlhost: lhost -> lhost visitAction

  method vlval: lval -> lval visitAction
  (** Invoked on each lvalue occurrence *)

  method voffs: offset -> offset visitAction
  (** Invoked on each offset occurrence that is *not* as part
    * of an initializer list specification, i.e. in an lval or
    * recursively inside an offset. *)

  method vinitoffs: offset -> offset visitAction
  (** Invoked on each offset appearing in the list of a
    * CompoundInit initializer.  *)

  method vinst: instr -> instr list visitAction
  (** Invoked on each instruction occurrence. The [ChangeTo] action can
   * replace this instruction with a list of instructions *)

  method vstmt: stmt -> stmt visitAction
  (** Control-flow statement. *)

  method vblock: block -> block visitAction
  (** Block. Replaced in place. *)

  method vfunc: fundec -> fundec visitAction
  (** Function definition. Replaced in place. *)

  method vglob: global -> global list visitAction
  (** Global (vars, types, etc.)  *)

  method vinit: varinfo -> offset -> init -> init visitAction
  (** Initializers for globals, pass the global where this occurs, and the
      offset *)

  method vstr_literal: varinfo -> str_literal -> str_literal visitAction

  method vinit_or_str: varinfo -> init_or_str -> init_or_str visitAction

  method vlocal_init: varinfo -> local_init -> local_init visitAction

  method vtype: typ -> typ visitAction
  (** Use of some type. Note that for structure/union and enumeration types the
      definition of the composite type is not visited. Use [vglob] to visit it.
  *)

  method vcompinfo: compinfo -> compinfo visitAction

  method venuminfo: enuminfo -> enuminfo visitAction

  method vfieldinfo: fieldinfo -> fieldinfo visitAction

  method venumitem: enumitem -> enumitem visitAction

  method vattr: attribute -> attributes visitAction
  (** Attribute. Each attribute can be replaced by a list *)

  method vattrparam: attrparam -> attrparam visitAction
  (** Attribute parameters. *)

  (** Add here instructions while visiting to queue them to
   * precede the current statement being processed *)
  method queueInstr: instr list -> unit

  (** Gets the queue of instructions and resets the queue *)
  method unqueueInstr: unit -> instr list

  val current_stmt : stmt Stack.t
  method push_stmt: stmt -> unit
  method  pop_stmt: stmt -> unit
  method current_stmt: stmt option
  method current_kinstr: kinstr

  method current_func: fundec option
  method set_current_func: fundec -> unit
  method reset_current_func: unit -> unit

  method vlogic_type: logic_type -> logic_type visitAction

  method vmodel_info: model_info -> model_info visitAction

  method videntified_term: identified_term -> identified_term visitAction

  method vterm: term -> term visitAction

  method vterm_node: term_node -> term_node visitAction

  method vterm_lval: term_lval -> term_lval visitAction

  method vterm_lhost: term_lhost -> term_lhost visitAction

  method vterm_offset: term_offset -> term_offset visitAction

  method vlogic_label: logic_label -> logic_label visitAction

  method vlogic_info_decl: logic_info -> logic_info visitAction

  method vlogic_info_use: logic_info -> logic_info visitAction

  method vlogic_type_info_decl: logic_type_info -> logic_type_info visitAction

  method vlogic_type_info_use: logic_type_info -> logic_type_info visitAction

  method vlogic_type_def: logic_type_def -> logic_type_def visitAction

  method vlogic_ctor_info_decl: logic_ctor_info -> logic_ctor_info visitAction

  method vlogic_ctor_info_use: logic_ctor_info -> logic_ctor_info visitAction

  method vlogic_var_use: logic_var -> logic_var visitAction

  method vlogic_var_decl: logic_var -> logic_var visitAction

  method vquantifiers: quantifiers -> quantifiers visitAction

  method videntified_predicate:
    identified_predicate -> identified_predicate visitAction

  method vpredicate_node: predicate_node -> predicate_node visitAction

  method vpredicate: predicate -> predicate visitAction

  method vbehavior: funbehavior -> funbehavior visitAction

  method vspec: funspec -> funspec visitAction

  method vassigns:
    assigns -> assigns visitAction

  method vfrees:
    identified_term list -> identified_term list visitAction
  method vallocates:
    identified_term list -> identified_term list visitAction
  method vallocation:
    allocation -> allocation visitAction

  method vdeps:
    deps -> deps visitAction

  method vfrom:
    from -> from visitAction

  method vcode_annot: code_annotation -> code_annotation visitAction

  method vannotation: global_annotation -> global_annotation visitAction
  method fill_global_tables: unit
  method get_filling_actions: (unit -> unit) Queue.t
end

class internal_genericCilVisitor current_func behavior queue: cilVisitor =
  object(self)
    method behavior = behavior

    method project = Visitor_behavior.get_project behavior

    method plain_copy_visitor =
      let obj =
        new internal_genericCilVisitor current_func behavior queue
      in
      assert (obj#get_filling_actions == self#get_filling_actions); obj

    method fill_global_tables =
      let action () = Queue.iter (fun f -> f()) queue in
      (match self#project with
       | None -> action ()
       | Some prj -> Project.on prj action ());
      Queue.clear queue

    method get_filling_actions = queue

    method vfile _f = DoChildren
    val current_stmt = Stack.create ()
    method push_stmt s = Stack.push s current_stmt
    method pop_stmt _s = ignore (Stack.pop current_stmt)
    method current_stmt =
      try Some (Stack.top current_stmt) with Stack.Empty -> None

    method current_kinstr =
      try Kstmt (Stack.top current_stmt) with Stack.Empty -> Kglobal

    method current_func = !current_func
    method set_current_func f = current_func := Some f
    method reset_current_func () = current_func := None

    method vvrbl (_v:varinfo) = DoChildren
    method vvdec (_v:varinfo) = DoChildren
    method vexpr (_e:exp) = DoChildren
    method vlval (_l:lval) = DoChildren
    method vlhost (_h:lhost) = DoChildren
    method voffs (_o:offset) = DoChildren
    method vinitoffs (_o:offset) = DoChildren
    method vinst (_i:instr) = DoChildren
    method vstmt (_s:stmt) = DoChildren
    method vblock (_b: block) = DoChildren
    method vfunc (_f:fundec) = DoChildren
    method vglob (_g:global) = DoChildren
    method vinit (_forg: varinfo) (_off: offset) (_i:init) = DoChildren
    method vstr_literal _ _ = DoChildren
    method vinit_or_str _ _ = DoChildren
    method vlocal_init _ _ = DoChildren
    method vtype (_t:typ) = DoChildren
    method vcompinfo _ = DoChildren
    method venuminfo _ = DoChildren
    method vfieldinfo _ = DoChildren
    method venumitem _ = DoChildren
    method vattr (_a: attribute) = DoChildren
    method vattrparam (_a: attrparam) = DoChildren

    val mutable instrQueue = []

    method queueInstr (il: instr list) =
      List.iter (fun i -> instrQueue <- i :: instrQueue) il

    method unqueueInstr () =
      let res = List.rev instrQueue in
      instrQueue <- [];
      res

    method vmodel_info _ = DoChildren

    method vlogic_type _lt = DoChildren

    method videntified_term _t = DoChildren

    method vterm _t = DoChildren

    method vlogic_label _l = DoChildren

    method vterm_node _tn = DoChildren

    method vterm_lval _tl = DoChildren

    method vterm_lhost _tl = DoChildren

    method vterm_offset _vo = DoChildren

    method vlogic_info_decl _li = DoChildren

    method vlogic_info_use _li = DoChildren

    method vlogic_type_info_decl _ = DoChildren

    method vlogic_type_info_use _ = DoChildren

    method vlogic_type_def _ = DoChildren

    method vlogic_ctor_info_decl _ = DoChildren

    method vlogic_ctor_info_use _ = DoChildren

    method vlogic_var_decl _lv = DoChildren

    method vlogic_var_use _lv = DoChildren

    method vquantifiers _q = DoChildren

    method videntified_predicate _ip = DoChildren

    method vpredicate_node _p = DoChildren

    method vpredicate _p = DoChildren

    method vbehavior _b = DoChildren

    method vspec _s = DoChildren

    method vassigns _s = DoChildren
    method vfrees _s = DoChildren
    method vallocates _s = DoChildren
    method vallocation _s = DoChildren

    method vdeps _ = DoChildren

    method vfrom _ = DoChildren

    method vcode_annot _ca = DoChildren

    method vannotation _a = DoChildren

  end

class genericCilVisitor bhv =
  let current_func = ref None in
  let queue = Queue.create () in
  internal_genericCilVisitor current_func bhv queue

class nopCilVisitor = object
  inherit genericCilVisitor (Visitor_behavior.inplace ())
end

let apply_on_project ?selection vis f arg =
  match vis#project with
  | None -> f arg
  | Some prj -> Project.on ?selection prj f arg

let assertEmptyQueue vis =
  if vis#unqueueInstr () <> [] then
    (* Either a visitor inserted an instruction somewhere that it shouldn't
       have (i.e. at the top level rather than inside of a statement), or
       there's a bug in the visitor engine. *)
    Kernel.fatal
      "Visitor's instruction queue is not empty.@\n\
       You should only use queueInstr inside a function body!";
  ()

let vis_tmp_attr = "FRAMAC_VIS_TMP_ATTR"

let transient_block b =
  if b.blocals <> [] then begin
    if List.exists
        (function
          | { skind = Instr (Local_init (v,_,_)) } ->
            not (List.exists (Cil_datatype.Varinfo.equal v) b.blocals)
          | _ -> false)
        b.bstmts
    then
      Kernel.fatal
        "Attempting to mark as transient a block that declares local variables";
    Kernel.warning
      ~wkey:Kernel.wkey_transient
      "ignoring request to mark transient a block with local variables:@\n%a"
      !pp_block_ref b
  end else
    b.battrs <- Ast_attributes.add (vis_tmp_attr,[]) b.battrs; b

let block_of_transient b =
  if Ast_attributes.contains vis_tmp_attr b.battrs then begin
    if b.blocals <> [] then
      Kernel.fatal
        "Block that is supposed to be transient declares local variables";
    b.battrs <- Ast_attributes.drop vis_tmp_attr b.battrs;
    b.bscoping <- false
  end;
  b

let is_transient_block b = Ast_attributes.contains vis_tmp_attr b.battrs

let flatten_transient_sub_blocks b =
  let prev = ref None in
  let previous_is_annot () =
    match !prev with
    | None -> false
    | Some {
        skind =
          Instr (Code_annot ({ annot_content = AStmtSpec _ }, _))}
      -> true
    | Some _ -> false
  in
  let treat_one_stmt acc s =
    match s.skind with
    | Block b when is_transient_block b ->
      if previous_is_annot () then begin
        s.skind <- Block (block_of_transient b);
        prev := Some s;
        s :: acc
      end else begin
        match s.labels, b.bstmts with
        | [], _ -> prev:= None; List.rev_append b.bstmts acc
        | _, [] ->
          (* Empty block, but we have a label attached to the statement, so
             that it is difficult to get rid of it (see below). Replace with
             a Skip.
          *)
          s.skind <- Instr (Skip (Cil_datatype.Stmt.loc s));
          prev:=Some s;
          s :: acc
        | _, s'::tl when s'.labels = [] ->
          (* res is the target of a label (either goto or case). Removing the
             block would imply updating the origin of the jump, which is
             quite complicated at this point. On the other hand, since s' is
             not referred to elsewhere, we can just put its skind in place of
             the block, and return the list. *)
          s.skind <- s'.skind;
          prev:=None;
          List.rev_append tl (s :: acc)
        | _ ->
          (* both the block and the first statement have labels. Just keep
             the block. *)
          s.skind <- Block (block_of_transient b);
          prev:=Some s;
          s :: acc
      end
    | _ -> prev:= Some s; s :: acc
  in
  b.bstmts <- List.rev (List.fold_left treat_one_stmt [] b.bstmts);
  b

let stmt_of_instr_list_visitor ?loc l =
  let res =  stmt_of_instr_list ?loc l in
  match res with
  | Block b -> Block (transient_block b)
  | _ -> res

(*** Define the visiting engine ****)
(* visit all the nodes in a Cil expression *)
let doVisit (vis: 'visitor)
    only_copy_vis
    (previsit: 'a -> 'a)
    (startvisit: 'a -> 'a visitAction)
    (children: 'visitor -> 'a -> 'a)
    (node: 'a) : 'a =
  let node' = previsit node in
  let action = startvisit node' in
  match action with
    SkipChildren -> node'
  | ChangeTo node' -> node'
  | ChangeToPost (node',f) -> f node'
  | DoChildren | DoChildrenPost _
  | JustCopy | ChangeDoChildrenPost _ | JustCopyPost _ ->
    let nodepre = match action with
        ChangeDoChildrenPost (node', _) -> node'
      | _ -> node'
    in
    let vis = match action with
        JustCopy | JustCopyPost _ -> only_copy_vis
      | _ -> vis
    in
    let nodepost = children vis nodepre in
    match action with
    | DoChildrenPost f | ChangeDoChildrenPost (_, f) | JustCopyPost f ->
      f nodepost
    | _ -> nodepost

let doVisitCil vis previsit startvisit children node =
  doVisit vis vis#plain_copy_visitor previsit startvisit children node

(* A visitor for lists *)
let doVisitList  (vis: 'visit)
    only_copy_vis
    (previsit: 'a -> 'a)
    (startvisit: 'a -> 'a list visitAction)
    (children: 'visit -> 'a -> 'a)
    (node: 'a) : 'a list =
  let node' = previsit node in
  let action = startvisit node' in
  match action with
    SkipChildren -> [node']
  | ChangeTo nodes' -> nodes'
  | ChangeToPost (nodes',f) -> f nodes'
  | _ ->
    let nodespre = match action with
        ChangeDoChildrenPost (nodespre, _) -> nodespre
      | _ -> [node']
    in
    let vis = match action with
        JustCopy | JustCopyPost _ -> only_copy_vis
      | _ -> vis
    in
    let nodespost = List.map_no_copy (children vis) nodespre in
    match action with
    | DoChildrenPost f | ChangeDoChildrenPost (_, f) | JustCopyPost f ->
      f nodespost
    | _ -> nodespost

let doVisitListCil vis previsit startvisit children node =
  doVisitList vis vis#plain_copy_visitor previsit startvisit children node

let debugVisit = false

let visitCilConst vis c =
  match c with
  | CEnum ei -> (* In case of deep copy, we must change the enumitem*)
    let ei' = Visitor_behavior.Get.enumitem vis#behavior ei in
    if ei' != ei then CEnum ei' else c
  |  _ -> c

let visitCilLConst vis c =
  match c with
  | LEnum ei -> (* In case of deep copy, we must change the enumitem*)
    let ei' = Visitor_behavior.Get.enumitem vis#behavior ei in
    if ei' != ei then LEnum ei' else c
  |  _ -> c

let copy_logic_label is_copy l =
  if is_copy then begin
    match l with
    | StmtLabel s -> StmtLabel (ref !s)
    | FormalLabel s -> FormalLabel s
    | BuiltinLabel s -> BuiltinLabel s
    (* we don't copy the associated statement. It will be recomputed
       if needed. *)
  end else l

let nopChildren _vis elt = elt

let visitCilStr_literal vis vi lit =
  doVisitCil vis id (vis#vstr_literal vi) nopChildren lit

let rec visitCilTerm vis t =
  Current_loc.with_loc t.term_loc
    (doVisitCil vis (fun x-> x) vis#vterm childrenTerm) t

and childrenTerm vis t =
  let tn' = visitCilTermNode vis t.term_node in
  let tt' = visitCilLogicType vis t.term_type in
  if tn' != t.term_node || tt' != t.term_type then
    { t with term_node = tn'; term_type = tt' }
  else t
and visitCilTermNode vis tn =
  doVisitCil vis id vis#vterm_node childrenTermNode tn
and childrenTermNode vis tn =
  let vTerm t = visitCilTerm vis t in
  let vTermLval tl = visitCilTermLval vis tl in
  let vTyp t = visitCilType vis t in
  let vLogicInfo li = visitCilLogicInfoUse vis li in
  match tn with
  | TConst c ->
    let c' = visitCilLConst vis c in
    if c' != c then TConst c' else tn
  | TDataCons (ci,args) ->
    let ci' =
      doVisitCil vis id vis#vlogic_ctor_info_use alphabetabeta ci
    in
    let args' = List.map_no_copy vTerm args in
    if ci' != ci || args != args' then TDataCons(ci',args') else tn
  | TLval tl ->
    let tl' = vTermLval tl in
    if tl' != tl then TLval tl' else tn
  | TSizeOf t ->
    let t' = vTyp t in if t' != t then TSizeOf t' else tn
  | TSizeOfE t ->
    let t' = vTerm t in if  t' != t then TSizeOfE t' else tn
  | TAlignOf t ->
    let t' = vTyp t in if t' != t then TAlignOf t' else tn
  | TAlignOfE t ->
    let t' = vTerm t in if  t' != t then TAlignOfE t' else tn
  | TUnOp (op,t) ->
    let t' = vTerm t in if  t' != t then TUnOp (op,t') else tn
  | TBinOp(op,t1,t2) ->
    let t1' = vTerm t1 in
    let t2' = vTerm t2 in
    if t1' != t1 || t2' != t2 then TBinOp(op,t1',t2') else tn
  | TCast(false, Ctype ty,te) ->
    let ty' = vTyp ty in
    let te' = vTerm te in
    if ty' != ty || te' != te then TCast(false, Ctype ty',te') else tn
  | TCast (true, ty,t) ->
    let ty' = visitCilLogicType vis ty in
    let t' = visitCilTerm vis t in
    if ty' != ty || t' != t then TCast (true, ty',t') else tn
  | TCast(false,_,_) -> Kernel.fatal "TCast to ctype without Ctype"
  | TAddrOf tl ->
    let tl' = vTermLval tl in
    if tl' != tl then TAddrOf tl' else tn
  | TStartOf tl ->
    let tl' = vTermLval tl in
    if tl' != tl then TStartOf tl' else tn
  | Tapp(li,labels,args) ->
    let li' = vLogicInfo li in
    let labels' =
      List.map_no_copy (visitCilLogicLabel vis) labels in
(*
 Format.eprintf "Cil.children_term_node: li = %s(%d), li' = %s(%d)@."
   li.l_var_info.lv_name li.l_var_info.lv_id
          li'.l_var_info.lv_name li'.l_var_info.lv_id;
*)
    let args' = List.map_no_copy vTerm args in
    if li' != li || labels' != labels || args' != args then
      Tapp(li',labels',args') else tn
  | Tif(test,ttrue,tfalse) ->
    let test' = visitCilPredicate vis test in
    let ttrue' = vTerm ttrue in
    let tfalse' = vTerm tfalse in
    if test' != test || ttrue' != ttrue || tfalse' != tfalse then
      Tif(test',ttrue',tfalse')
    else tn
  | Tat(t,s) ->
    let t' = vTerm t in
    let s' = visitCilLogicLabel vis s in
    if t' != t || s' != s then Tat (t',s') else tn
  | Toffset (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s' != s then Toffset (s',t') else tn
  | Tbase_addr (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s' != s then Tbase_addr (s',t') else tn
  | Tblock_length (s,t)->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s' != s then Tblock_length (s',t') else tn
  | Tnull -> tn
  | TUpdate (tc,toff,te) ->
    let tc' = vTerm tc in
    let te' = vTerm te in
    let toff'  = visitCilTermOffset vis toff in
    if tc' != tc || (te' != te || toff' != toff)
    then TUpdate(tc',toff',te') else tn
  | Tlambda(prms,te) ->
    let prms' = visitCilQuantifiers vis prms in
    let te' = vTerm te in
    if prms' != prms || te' != te then Tlambda(prms',te') else tn
  | Ttypeof t ->
    let t' = vTerm t in if t' != t then Ttypeof t' else tn
  | Ttype ty ->
    let ty' = vTyp ty in if ty' != ty then Ttype ty' else tn
  | Tunion locs ->
    let locs' = List.map_no_copy (visitCilTerm vis) locs in
    if locs != locs' then Tunion(locs') else tn
  | Tinter locs ->
    let locs' = List.map_no_copy (visitCilTerm vis) locs in
    if locs != locs' then Tinter(locs') else tn
  | Tcomprehension(lval,quant,pred) ->
    let quant' = visitCilQuantifiers vis quant in
    let lval' = visitCilTerm vis lval in
    let pred' = (Option.map_no_copy (visitCilPredicate vis)) pred in
    if lval' != lval || quant' != quant || pred' != pred
    then
      Tcomprehension(lval',quant',pred')
    else
      tn
  | Tempty_set -> tn
  | Trange(low,high) ->
    let low' = Option.map_no_copy (visitCilTerm vis) low in
    let high' = Option.map_no_copy (visitCilTerm vis) high in
    if low != low' || high != high' then Trange(low',high')
    else tn
  | Tlet(def,body) ->
    let def'= visitCilLogicInfo vis def in
    let body' = visitCilTerm vis body in
    if def != def' || body != body' then Tlet(def',body') else tn

and visitCilLogicLabel vis l =
  doVisitCil vis
    (copy_logic_label (Visitor_behavior.is_copy vis#behavior))
    vis#vlogic_label childrenLogicLabel l

and childrenLogicLabel vis l =
  match l with
    StmtLabel s -> s := Visitor_behavior.Get.stmt vis#behavior !s; l
  | FormalLabel _ | BuiltinLabel _ -> l

and visitCilTermLval vis tl =
  doVisitCil vis id vis#vterm_lval childrenTermLval tl

and childrenTermLval vis ((tlv,toff) as tl)=
  let tlv' = visitCilTermLhost vis tlv in
  let toff' = visitCilTermOffset vis toff in
  if tlv' != tlv || toff' != toff then (tlv',toff') else tl

and visitCilTermLhost vis tl =
  doVisitCil vis id vis#vterm_lhost childrenTermLhost tl

and childrenTermLhost vis tl = match tl with
    TVar v ->
    let v' = visitCilLogicVarUse vis v in if v' != v then TVar v' else tl
  | TResult ty ->
    let ty' = visitCilType vis ty in if ty' != ty then TResult ty' else tl
  | TMem t ->
    let t' = visitCilTerm vis t in if t' != t then TMem t' else tl

and visitCilTermOffset vis toff =
  doVisitCil vis id
    vis#vterm_offset childrenTermOffset toff

and childrenTermOffset vis toff =
  let vOffset o = visitCilTermOffset vis o in
  let vTerm t = visitCilTerm vis t in
  match toff with
    TNoOffset -> toff
  | TField (fi, t) ->
    let t' = vOffset t in
    let fi' = Visitor_behavior.Get.fieldinfo vis#behavior fi in
    if t' != t || fi != fi' then TField(fi',t') else toff
  | TIndex(t,o) ->
    let t' = vTerm t in let o' = vOffset o in
    if t' != t || o' != o then TIndex(t',o') else toff
  | TModel (mi,t) ->
    let t' = vOffset t in
    let mi' = Visitor_behavior.Get.model_info vis#behavior mi in
    if t' != t || mi != mi' then TModel(mi', t') else toff

and visitCilLogicInfoUse vis li =
  (* First, visit the underlying varinfo to fill the copy tables if needed. *)
  let new_v = visitCilLogicVarUse vis li.l_var_info in
  let new_li =
    doVisitCil vis (Visitor_behavior.Get.logic_info vis#behavior)
      vis#vlogic_info_use alphabetabeta li
  in
  new_li.l_var_info <- new_v;
  new_li

and visitCilLogicInfo vis li =
  (* visit first the underlying varinfo. This will fill internal tables
     of copy behavior if needed.
  *)
  let new_v = visitCilLogicVarDecl vis li.l_var_info in
  let res =
    doVisitCil
      vis (Visitor_behavior.Memo.logic_info vis#behavior)
      vis#vlogic_info_decl childrenLogicInfo li
  in res.l_var_info <- new_v; res

and childrenLogicInfo vis li =
  (* NB: underlying varinfo has been already visited. *)
  let lt = Option.map_no_copy (visitCilLogicType vis) li.l_type in
  let lp = List.map_no_copy (visitCilLogicVarDecl vis) li.l_profile in
  li.l_type <- lt;
  li.l_profile <- lp;
  li.l_body <-
    begin
      match li.l_body with
      | LBnone -> li.l_body
      | LBreads ol ->
        let l = List.map_no_copy (visitCilIdTerm vis) ol in
        if l != ol then LBreads l else li.l_body
      | LBterm ot ->
        let t = visitCilTerm vis ot in
        if t != ot then LBterm t else li.l_body
      | LBinductive inddef ->
        let i =
          List.map_no_copy
            (fun (id,labs,tvars,p) ->
               (id, labs, tvars, visitCilPredicate vis p))
            inddef
        in
        if i != inddef then LBinductive i else li.l_body
      | LBpred odef ->
        let def = visitCilPredicate vis odef in
        if def != odef then LBpred def else li.l_body
    end;
  li

and visitCilLogicTypeInfo vis lt =
  doVisitCil vis (Visitor_behavior.Memo.logic_type_info vis#behavior)
    vis#vlogic_type_info_decl childrenLogicTypeInfo lt

and childrenLogicTypeInfo vis lt =
  let def = Option.map_no_copy (visitCilLogicTypeDef vis) lt.lt_def in
  lt.lt_def <- def; lt

and visitCilLogicTypeDef vis def =
  doVisitCil vis id vis#vlogic_type_def childrenLogicTypeDef def

and childrenLogicTypeDef vis def =
  match def with
  | LTsum l ->
    let l' = List.map_no_copy (visitCilLogicCtorInfoAddTable vis) l in
    if l != l' then LTsum l' else def
  | LTsyn typ ->
    let typ' = visitCilLogicType vis typ in
    if typ != typ' then LTsyn typ else def

and visitCilLogicCtorInfoAddTable vis ctor =
  let ctor' = visitCilLogicCtorInfo vis ctor in
  if Visitor_behavior.is_copy vis#behavior then
    Queue.add
      (fun () ->
         Logic_env.add_logic_ctor ctor'.ctor_name ctor')
      vis#get_filling_actions;
  ctor'

and visitCilLogicCtorInfo vis ctor =
  doVisitCil vis id vis#vlogic_ctor_info_decl childrenLogicCtorInfo ctor

and childrenLogicCtorInfo vis ctor =
  let ctor_type = doVisitCil vis (Visitor_behavior.Get.logic_type_info vis#behavior)
      vis#vlogic_type_info_use alphabetabeta ctor.ctor_type
  in
  let ctor_params = ctor.ctor_params in
  let ctor_params' = List.map_no_copy (visitCilLogicType vis) ctor_params in
  if ctor_type != ctor.ctor_type || ctor_params != ctor_params' then
    { ctor with ctor_type = ctor_type; ctor_params = ctor_params' }
  else ctor

and visitCilLogicType vis t =
  doVisitCil vis id vis#vlogic_type childrenLogicType t

and childrenLogicType vis ty =
  match ty with
    Ctype t ->
    let t' = visitCilType vis t in
    if t != t' then Ctype t' else ty
  | Lboolean | Linteger | Lreal -> ty
  | Ltype (s,l) ->
    let s' = doVisitCil vis (Visitor_behavior.Get.logic_type_info vis#behavior)
        vis#vlogic_type_info_use alphabetabeta s in
    let l' = List.map_no_copy (visitCilLogicType vis) l in
    if s' != s || l' != l then Ltype (s',l') else ty
  | Larrow(args,rttyp) ->
    let args' = List.map_no_copy (visitCilLogicType vis) args in
    let rttyp' = visitCilLogicType vis rttyp in
    if args' != args || rttyp' != rttyp then Larrow(args',rttyp') else ty
  | Lvar _ -> ty

and visitCilLogicVarDecl vis lv =
  (* keep names in C and logic worlds in sync *)
  (match lv.lv_origin with
     None -> ()
   | Some cv -> lv.lv_name <- cv.vname);
  doVisitCil vis (Visitor_behavior.Memo.logic_var vis#behavior) vis#vlogic_var_decl
    childrenLogicVarDecl lv

and childrenLogicVarDecl vis lv =
  lv.lv_type <- visitCilLogicType vis lv.lv_type;
  lv.lv_origin <-
    Option.map_no_copy (visitCilVarUse vis) lv.lv_origin;
  lv

and visitCilLogicVarUse vis lv =
  if Visitor_behavior.is_copy vis#behavior &&
     (* In a copy visitor, there's always a project. Furthermore, if
        we target the current project, builtins are by definition already
        tied to logic_infos and should not be copied.
     *)
     not (Project.is_current (Option.get vis#project)) &&
     Logic_env.is_builtin_logic_function lv.lv_name
  then begin
    (* Do as if the variable has been declared.
       We'll fill the logic info table of the new project at the end.
       Behavior's logic_var table is filled as a side effect.
    *)
    let siblings = Logic_env.find_all_logic_functions lv.lv_name in
    let siblings' = List.map (visitCilLogicInfo vis) siblings in
    (*Format.printf "new vars:@.";
      List.iter (fun x -> Format.printf "%s#%d@." x.l_var_info.lv_name x.l_var_info.lv_id) siblings';
    *)
    Queue.add
      (fun () ->
         if not (Logic_env.Logic_builtin_used.mem lv.lv_name) then begin
           (*  Format.printf
               "Adding info for %s#%d@."
               x.l_var_info.lv_name x.l_var_info.lv_id; *)
           Logic_env.Logic_builtin_used.add lv.lv_name siblings';
           Logic_env.Logic_info.add lv.lv_name siblings'
         end)
      vis#get_filling_actions;
  end;
  doVisitCil vis (Visitor_behavior.Get.logic_var vis#behavior) vis#vlogic_var_use
    childrenLogicVarUse lv

and childrenLogicVarUse vis lv =
  lv.lv_origin <- Option.map_no_copy (visitCilVarUse vis) lv.lv_origin; lv

and visitCilQuantifiers vis lv =
  doVisitCil vis id vis#vquantifiers
    (fun vis l -> List.map_no_copy (visitCilLogicVarDecl vis) l) lv

and visitCilIdPredicate vis ip =
  doVisitCil
    vis
    (Visitor_behavior.cidentified_predicate vis#behavior)
    vis#videntified_predicate
    childrenIdentified_predicate
    ip
and visitCilPredicateNode vis p =
  doVisitCil vis id vis#vpredicate_node childrenPredicateNode p

and visitCilPredicate vis p =
  doVisitCil vis
    id vis#vpredicate childrenPredicate p

and visitCilToplevel_predicate vis p =
  let s = p.tp_statement in
  let s' = visitCilPredicate vis s in
  if s != s' then { p with tp_statement = s' } else p

and childrenIdentified_predicate vis ip =
  let p = ip.ip_content in
  let p' = visitCilToplevel_predicate vis p in
  if p != p' then { ip with ip_content = p' }
  else ip

and childrenPredicate vis p =
  let content = visitCilPredicateNode vis p.pred_content in
  if content != p.pred_content then { p with pred_content = content} else p

and childrenPredicateNode vis p =
  let vPred p = visitCilPredicate vis p in
  let vLogicInfo li = visitCilLogicInfoUse vis li in
  let vTerm t = visitCilTerm vis t in
  match p with
    Pfalse | Ptrue -> p
  | Papp (pred,labels,args) ->
    let pred' = vLogicInfo pred in
    let labels' = List.map_no_copy (visitCilLogicLabel vis) labels in
    let args' = List.map_no_copy vTerm args in
    if pred' != pred || labels' != labels || args' != args then
      Papp(pred',labels',args')
    else p
  | Prel(rel,t1,t2) ->
    let t1' = vTerm t1 in
    let t2' = vTerm t2 in
    if t1' != t1 || t2' != t2 then
      Prel(rel,t1',t2')
    else p
  | Pand(p1,p2) ->
    let p1' = vPred p1 in
    let p2' = vPred p2 in
    if p1' != p1 || p2' != p2 then
      Pand(p1',p2')
    else p
  | Por(p1,p2) ->
    let p1' = vPred p1 in
    let p2' = vPred p2 in
    if p1' != p1 || p2' != p2 then
      Por(p1',p2')
    else p
  | Pxor(p1,p2) ->
    let p1' = vPred p1 in
    let p2' = vPred p2 in
    if p1' != p1 || p2' != p2 then
      Pxor(p1',p2')
    else p
  | Pimplies(p1,p2) ->
    let p1' = vPred p1 in
    let p2' = vPred p2 in
    if p1' != p1 || p2' != p2 then
      Pimplies(p1',p2')
    else p
  | Piff(p1,p2) ->
    let p1' = vPred p1 in
    let p2' = vPred p2 in
    if p1' != p1 || p2' != p2 then
      Piff(p1',p2')
    else p
  | Pnot p1 ->
    let p1' = vPred p1 in
    if p1' != p1 then Pnot p1' else p
  | Pif(c,ptrue,pfalse) ->
    let c' = vPred c in
    let ptrue' = vPred ptrue in
    let pfalse' = vPred pfalse in
    if c' != c || ptrue' != ptrue || pfalse' != pfalse then
      Pif(c', ptrue',pfalse')
    else p
  | Plet(def,p1) ->
    let def' = visitCilLogicInfo vis def in
    let p1' = vPred p1 in
    if def' != def || p1' != p1 then
      Plet(def',p1')
    else p
  | Pforall(quant,p1) ->
    let quant' = visitCilQuantifiers vis quant in
    let p1' = vPred p1 in
    if quant' != quant || p1' != p1 then
      Pforall(quant', p1')
    else p
  | Pexists(quant,p1) ->
    let quant' = visitCilQuantifiers vis quant in
    let p1' = vPred p1 in
    if quant' != quant || p1' != p1 then
      Pexists(quant', p1')
    else p
  | Pat(p1,s) ->
    let p1' = vPred p1 in
    let s' = visitCilLogicLabel vis s in
    if p1' != p1 || s != s' then Pat(p1',s') else p
  | Pallocable (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s != s' then Pallocable (s',t') else p
  | Pfreeable (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s != s' then Pfreeable (s',t') else p
  | Pvalid (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s != s' then Pvalid (s',t') else p
  | Pvalid_read (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s != s' then Pvalid_read (s',t') else p
  | Pobject_pointer (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s != s' then Pobject_pointer (s',t') else p
  | Pvalid_function t ->
    let t' = vTerm t in
    if t' != t then Pvalid_function t' else p
  | Pinitialized (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s != s' then Pinitialized (s',t') else p
  | Pdangling (s,t) ->
    let s' = visitCilLogicLabel vis s in
    let t' = vTerm t in
    if t' != t || s != s' then Pdangling (s',t') else p
  | Paligned (t,n) ->
    let t' = vTerm t in
    let n' = vTerm n in
    if t' != t || n' != n then Paligned (t',n') else p
  | Pseparated seps ->
    let seps' = List.map_no_copy vTerm seps in
    if seps' != seps then Pseparated seps' else p
  | Pfresh (s1,s2,t,n) ->
    let s1' = visitCilLogicLabel vis s1 in
    let s2' = visitCilLogicLabel vis s2 in
    let t' = vTerm t in
    let n' = vTerm n in
    if t' != t || n' != n || s1 != s1' || s2 != s2' then Pfresh (s1',s2',t',n') else p

and visitCilIdTerm vis loc =
  doVisitCil vis (Visitor_behavior.cidentified_term vis#behavior) vis#videntified_term
    childrenIdentified_term loc
and childrenIdentified_term vis loc =
  let loc' = visitCilTerm vis loc.it_content in
  if loc' != loc.it_content then { loc with it_content = loc' } else loc

and visitCilAllocation vis fa =
  doVisitCil vis id vis#vallocation childrenAllocation fa
and childrenAllocation vis fa =
  match fa with
    FreeAllocAny -> fa
  | FreeAlloc(f,a)  ->
    let f' = visitCilFrees vis f in
    let a' = visitCilAllocates vis a in
    if f != f' || a' != a then FreeAlloc(f',a') else fa

and visitCilFrees vis l =
  doVisitCil vis id vis#vfrees childrenFreeAlloc l
and visitCilAllocates vis l =
  doVisitCil vis id vis#vallocates childrenFreeAlloc l
and childrenFreeAlloc vis l =
  List.map_no_copy (visitCilIdTerm vis) l

and visitCilAssigns vis a =
  doVisitCil vis id vis#vassigns childrenAssigns a
and childrenAssigns vis a =
  match a with
    WritesAny -> a
  | Writes l ->
    let l' = List.map_no_copy (visitCilFrom vis) l in
    if l' != l then Writes l' else a

and visitCilFrom vis f =
  doVisitCil vis id vis#vfrom childrenFrom f
and childrenFrom vis ((b,f) as a) =
  let b' = visitCilIdTerm vis b in
  let f' = visitCilDeps vis f in
  if b!=b' || f!=f' then (b',f') else a

and visitCilDeps vis d =
  doVisitCil vis id vis#vdeps childrenDeps d
and childrenDeps vis d =
  match d with
    FromAny -> d
  | From l ->
    let l' = List.map_no_copy (visitCilIdTerm vis) l in
    if l !=l' then From l' else d

and visitCilBehavior vis b =
  doVisitCil vis (Visitor_behavior.cfunbehavior vis#behavior)
    vis#vbehavior childrenBehavior b

and childrenBehavior vis b =
  b.b_assumes <- visitCilPredicates vis b.b_assumes;
  b.b_requires <- visitCilPredicates vis b.b_requires;
  b.b_post_cond <-
    List.map_no_copy
      (function ((k,p) as pc) ->
         let p' = visitCilIdPredicate vis p in if p != p' then (k,p') else pc)
      b.b_post_cond;
  b.b_assigns <- visitCilAssigns vis b.b_assigns;
  b.b_allocation <- visitCilAllocation vis b.b_allocation ;
  b.b_extended <- List.map_no_copy (visitCilExtended vis) b.b_extended;
  b

and visitCilExtended vis orig =
  let visit = Extensions.visit orig.ext_name in
  let e' = doVisitCil vis id (visit ~plugin:orig.ext_plugin vis) childrenCilExtended orig.ext_kind in
  if Visitor_behavior.is_fresh vis#behavior then
    Logic_const.new_acsl_extension ~plugin:orig.ext_plugin orig.ext_name
      orig.ext_loc orig.ext_has_status e'
  else if orig.ext_kind == e' then orig else {orig with ext_kind = e'}

and childrenCilExtended vis p =
  match p with
  | Ext_id _ -> p
  | Ext_terms terms ->
    let terms' = List.map_no_copy (visitCilTerm vis) terms in
    if terms == terms' then p else Ext_terms terms'
  | Ext_preds preds ->
    let preds' = List.map_no_copy (visitCilPredicate vis) preds in
    if preds == preds' then p else Ext_preds preds'
  | Ext_annot (id,annots) ->
    let annots' = List.map_no_copy (visitCilExtended vis) annots in
    if annots == annots' then p else Ext_annot (id,annots')

and visitCilPredicates vis ps = List.map_no_copy (visitCilIdPredicate vis) ps

and visitCilBehaviors vis bs = List.map_no_copy (visitCilBehavior vis) bs

and visitCilFunspec vis s =
  doVisitCil vis (Visitor_behavior.cfunspec vis#behavior) vis#vspec childrenSpec s

and childrenSpec vis s =
  s.spec_behavior <- visitCilBehaviors vis s.spec_behavior;
  s.spec_variant <-
    Option.map_no_copy (fun x -> (visitCilTerm vis (fst x), snd x)) s.spec_variant;
  s.spec_terminates <-
    Option.map_no_copy (visitCilIdPredicate vis) s.spec_terminates;
  (* nothing is done now for behaviors names, no need to visit complete and
     disjoint behaviors clauses
  *)
  s

and childrenModelInfo vis m =
  let field_type = visitCilLogicType vis m.mi_field_type in
  let base_type = visitCilType vis m.mi_base_type in
  let mi_attr = visitCilAttributes vis m.mi_attr in
  if
    field_type != m.mi_field_type || base_type != m.mi_base_type
  then
    {
      mi_name = m.mi_name;
      mi_field_type = field_type;
      mi_base_type = base_type;
      mi_decl = Fileloc.copy m.mi_decl;
      mi_attr;
    }
  else begin m.mi_attr <- mi_attr; m end

and visitCilModelInfo vis m =
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = m.mi_decl in
  let m' =
    doVisitCil
      vis (Visitor_behavior.Memo.model_info vis#behavior) vis#vmodel_info childrenModelInfo m
  in
  if m' != m then begin
    (* reflect changes in the behavior tables for copy visitor. *)
    Visitor_behavior.Set.model_info vis#behavior m m';
    Visitor_behavior.Set_orig.model_info vis#behavior m' m;
  end;
  m'

and visitCilAnnotation vis a =
  Current_loc.with_loc (Global_annotation.loc a)
    (doVisitCil vis id vis#vannotation childrenAnnotation) a

and childrenAnnotation vis a =
  match a with
  | Dfun_or_pred (li,loc) ->
    let li' = visitCilLogicInfo vis li in
    if Visitor_behavior.is_copy vis#behavior then
      Queue.add
        (fun () ->
           Logic_env.add_logic_function_gen alphabetafalse li')
        vis#get_filling_actions;
    if li' != li then Dfun_or_pred (li',loc) else a
  | Dtype (ti,loc) ->
    let ti' = visitCilLogicTypeInfo vis ti in
    if Visitor_behavior.is_copy vis#behavior then
      Queue.add
        (fun () ->
           Logic_env.add_logic_type ti'.lt_name ti')
        vis#get_filling_actions;
    if ti' != ti then Dtype (ti',loc) else a
  | Dlemma(s,labels,tvars,p,attr,loc) ->
    let p' = visitCilToplevel_predicate vis p in
    let attr' = visitCilAttributes vis attr in
    if p' != p || attr != attr' then
      Dlemma(s,labels,tvars,p',attr',loc)
    else a
  | Dinvariant (p,loc) ->
    let p' = visitCilLogicInfo vis p in
    if Visitor_behavior.is_copy vis#behavior then
      Queue.add
        (fun () -> Logic_env.add_logic_function_gen alphabetafalse p')
        vis#get_filling_actions;
    if p' != p then Dinvariant (p',loc) else a
  | Dtype_annot (ta,loc) ->
    let ta' = visitCilLogicInfo vis ta in
    if Visitor_behavior.is_copy vis#behavior then
      Queue.add
        (fun () -> Logic_env.add_logic_function_gen alphabetafalse ta')
        vis#get_filling_actions;
    if ta' != ta then Dtype_annot (ta',loc) else a
  | Dmodel_annot (mfi,loc) ->
    let mfi' = visitCilModelInfo vis mfi in
    if Visitor_behavior.is_copy vis#behavior then
      Queue.add (fun () -> Logic_env.add_model_field mfi')
        vis#get_filling_actions;
    if mfi' != mfi then Dmodel_annot (mfi',loc) else a
  | Dvolatile(tset,rvi,wvi,attr,loc) ->
    let tset' = List.map_no_copy (visitCilIdTerm vis) tset in
    let rvi' = Option.map_no_copy (visitCilVarUse vis) rvi in
    let wvi' = Option.map_no_copy (visitCilVarUse vis) wvi in
    let attr' = visitCilAttributes vis attr in
    if tset' != tset || rvi' != rvi || wvi' != wvi || attr' != attr then
      Dvolatile(tset',rvi',wvi',attr',loc)
    else a
  | Daxiomatic(id,l,attr,loc) ->
    let l' = List.map_no_copy (visitCilAnnotation vis) l in
    let attr' = visitCilAttributes vis attr in
    if l' != l || attr != attr' then Daxiomatic(id,l',attr',loc) else a
  | Dmodule(id,l,attr,loader,loc) ->
    let l' = List.map_no_copy (visitCilAnnotation vis) l in
    let attr' = visitCilAttributes vis attr in
    if l' != l || attr != attr' then Dmodule(id,l',attr',loader,loc) else a
  | Dextended (e,attr,loc) ->
    let e' = visitCilExtended vis e in
    let attr' = visitCilAttributes vis attr in
    if e != e' || attr != attr' then Dextended(e',attr', loc) else a

and visitCilCodeAnnotation vis ca =
  doVisitCil
    vis (Visitor_behavior.ccode_annotation vis#behavior) vis#vcode_annot childrenCodeAnnot ca

and childrenCodeAnnot vis ca =
  let vPred p = visitCilToplevel_predicate vis p in
  let vTerm t = visitCilTerm vis t in
  let vSpec s = visitCilFunspec vis s in
  let change_content annot = { ca with annot_content = annot } in
  match ca.annot_content with
    AAssert (behav,p) ->
    let p' = vPred p in if p' != p then
      change_content (AAssert (behav,p'))
    else ca
  | AStmtSpec (behav,s) ->
    let s' = vSpec s in
    if s' != s then change_content (AStmtSpec (behav,s')) else ca
  | AInvariant(behav,f,p) ->
    let p' = vPred p in
    if p' != p then change_content (AInvariant (behav,f,p')) else ca
  | AVariant ((t,s)) ->
    let t' = vTerm t in
    if t != t' then  change_content (AVariant ((t',s))) else ca
  | AAssigns(behav, a) ->
    let a' = visitCilAssigns vis a in
    if a != a' then change_content (AAssigns (behav,a')) else ca
  | AAllocation(behav, fa) ->
    let fa' = visitCilAllocation vis fa in
    if fa != fa' then change_content (AAllocation (behav,fa')) else ca
  | AExtended(behav, is_loop, ext) ->
    let ext' = visitCilExtended vis ext in
    if ext' != ext then
      change_content (AExtended (behav, is_loop, ext'))
    else ca

and visitCilExpr (vis: cilVisitor) (e: exp) : exp =
  Current_loc.with_loc (e.eloc)
    (doVisitCil vis (Visitor_behavior.cexpr vis#behavior) vis#vexpr childrenExp)
    e

and childrenExp (vis: cilVisitor) (e: exp) : exp =
  let vExp e = visitCilExpr vis e in
  let vTyp t = visitCilType vis t in
  let vLval lv = visitCilLval vis lv in
  let new_exp e' = { e with enode = e' } in
  match e.enode with
  | Const c ->
    let c' = visitCilConst vis c in
    if c' != c then new_exp (Const c') else e

  | SizeOf t ->
    let t'= vTyp t in
    if t' != t then new_exp (SizeOf t') else e
  | SizeOfE e1 ->
    let e1' = vExp e1 in
    if e1' != e1 then new_exp (SizeOfE e1') else e

  | AlignOf (t, i) ->
    let t' = vTyp t in
    if t' != t then new_exp (AlignOf (t', i)) else e
  | AlignOfE (e1, i) ->
    let e1' = vExp e1 in
    if e1' != e1 then new_exp (AlignOfE (e1', i)) else e

  | Lval lv ->
    let lv' = vLval lv in
    if lv' != lv then new_exp (Lval lv') else e
  | UnOp (uo, e1, t) ->
    let e1' = vExp e1 in let t' = vTyp t in
    if e1' != e1 || t' != t then new_exp (UnOp(uo, e1', t')) else e
  | BinOp (bo, e1, e2, t) ->
    let e1' = vExp e1 in let e2' = vExp e2 in let t' = vTyp t in
    if e1' != e1 || e2' != e2 || t' != t then
      new_exp (BinOp(bo, e1',e2',t'))
    else e
  | CastE (t, e1) ->
    let t' = vTyp t in let e1' = vExp e1 in
    if t' != t || e1' != e1 then new_exp (CastE(t', e1')) else e
  | AddrOf lv ->
    let lv' = vLval lv in
    if lv' != lv then new_exp (AddrOf lv') else e
  | StartOf lv ->
    let lv' = vLval lv in
    if lv' != lv then new_exp (StartOf lv') else e

and visitCilInit (vis: cilVisitor) (forglob: varinfo)
    (atoff: offset) (i: init) : init =
  let childrenInit (vis: cilVisitor) (i: init) : init =
    let fExp e = visitCilExpr vis e in
    let fTyp t = visitCilType vis t in
    match i with
    | SingleInit e ->
      let e' = fExp e in
      if e' != e then SingleInit e' else i
    | CompoundInit (t, initl) ->
      let t' = fTyp t in
      (* Collect the new initializer list, in reverse. We prefer two
       * traversals to ensure tail-recursion. *)
      let newinitl : (offset * init) list ref = ref [] in
      (* Keep track whether the list has changed *)
      let hasChanged = ref false in
      let doOneInit ((o, i) as oi) =
        let o' = visitCilInitOffset vis o in    (* use initializer version *)
        let i' = visitCilInit vis forglob (addOffset o' atoff) i in
        let newio =
          if o' != o || i' != i then
            begin hasChanged := true; (o', i') end else oi
        in
        newinitl := newio :: !newinitl
      in
      List.iter doOneInit initl;
      let initl' = if !hasChanged then List.rev !newinitl else initl in
      if t' != t || initl' != initl then CompoundInit (t', initl') else i
  in
  doVisitCil vis id (vis#vinit forglob atoff) childrenInit i

and visitCilInit_or_str vis vi i =
  let childrenInit_or_str vis i =
    match i with
    | CInit init ->
      let init' = visitCilInit vis vi NoOffset init in
      if init != init' then CInit init' else i
    | StrInit lit ->
      let lit' = visitCilStr_literal vis vi lit in
      if lit' != lit then StrInit lit' else i
  in
  doVisitCil vis id (vis#vinit_or_str vi) childrenInit_or_str i

and visitCilLval (vis: cilVisitor) (lv: lval) : lval =
  doVisitCil vis id vis#vlval childrenLval lv
and childrenLval (vis: cilVisitor) (h,off as lv : lval): lval =
  (* and visit its subexpressions *)
  let h' = visitCilLhost vis h in
  let off' = visitCilOffset vis off in
  if h' != h || off != off' then (h', off') else lv

and visitCilLhost (vis: cilVisitor) h =
  doVisitCil vis id vis#vlhost childrenLhost h
and childrenLhost (vis:cilVisitor) h =
  match h with
    Var v ->
    let v'= visitCilVarUse vis v in
    if v' != v then Var v' else h
  | Mem e ->
    let e' = visitCilExpr vis e in
    if e' != e then Mem e' else h

and visitCilOffset (vis: cilVisitor) (off: offset) : offset =
  doVisitCil vis id vis#voffs childrenOffset off
and childrenOffset (vis: cilVisitor) (off: offset) : offset =
  let vOff off = visitCilOffset vis off in
  match off with
    Field (f, o) ->
    let o' = vOff o in
    let f' = Visitor_behavior.Get.fieldinfo vis#behavior f in
    if o' != o || f' != f then Field (f', o') else off
  | Index (e, o) ->
    let e' = visitCilExpr vis e in
    let o' = vOff o in
    if e' != e || o' != o then Index (e', o') else off
  | NoOffset -> off

(* sm: for offsets in initializers, the 'startvisit' will be the
 * vinitoffs method, but we can re-use the childrenOffset from
 * above since recursive offsets are visited by voffs.  (this point
 * is moot according to cil.mli which claims the offsets in
 * initializers will never recursively contain offsets)
*)
and visitCilInitOffset (vis: cilVisitor) (off: offset) : offset =
  doVisitCil vis id vis#vinitoffs childrenOffset off

and visitCilLocal_init (vis: cilVisitor) vi li =
  doVisitCil vis id (vis#vlocal_init vi) (childrenLocal_init vi) li

and childrenLocal_init vi (vis: cilVisitor) li =
  match li with
  | AssignInit i ->
    let i' = visitCilInit vis vi NoOffset i in
    if i != i' then AssignInit i' else li
  | ConsInit(f,args, k) ->
    let f' = visitCilVarUse vis f in
    let args' = List.map_no_copy (visitCilExpr vis) args in
    if f' != f || args' != args then ConsInit(f',args',k) else li

and visitCilInstr (vis: cilVisitor) (i: instr) : instr list =
  Current_loc.with_loc (Cil_datatype.Instr.loc i)
    (doVisitListCil vis id vis#vinst childrenInstr) i

and childrenInstr (vis: cilVisitor) (i: instr) : instr =
  let fExp = visitCilExpr vis in
  let fLval = visitCilLval vis in
  let fLhost = visitCilLhost vis in
  match i with
  | Skip _l ->
    i
  | Local_init (vi, li, l) ->
    let vi' = visitCilVarUse vis vi in
    let li' = visitCilLocal_init vis vi' li in
    if vi' != vi || li' != li then
      Local_init(vi', li', l)
    else i
  | Set(lv,e,l) ->
    let lv' = fLval lv in let e' = fExp e in
    if lv' != lv || e' != e then Set(lv',e',l) else i
  | Call(None,f,args,l) ->
    let f' = fLhost f in let args' = List.map_no_copy fExp args in
    if f' != f || args' != args then Call(None,f',args',l) else i
  | Call(Some lv,fn,args,l) ->
    let lv' = fLval lv in let fn' = fLhost fn in
    let args' = List.map_no_copy fExp args in
    if lv' != lv || fn' != fn || args' != args
    then Call(Some lv', fn', args', l) else i

  | Asm(sl,isvol,ext_asm,l) ->
    (match ext_asm with
     | None -> i (* only strings and location, nothing to visit. *)
     | Some ext ->
       let asm_outputs_pre = ext.asm_outputs in
       let asm_outputs =
         List.map_no_copy
           (fun ((id,s,lv) as pair) ->
              let lv' = fLval lv in
              if lv' != lv then (id,s,lv') else pair) asm_outputs_pre
       in
       let asm_inputs_pre = ext.asm_inputs in
       let asm_inputs =
         List.map_no_copy
           (fun ((id,s,e) as pair) ->
              let e' = fExp e in
              if e' != e then (id,s,e') else pair) asm_inputs_pre
       in
       let asm_gotos =
         if Visitor_behavior.is_copy vis#behavior then
           List.map (fun s -> ref (Visitor_behavior.Memo.stmt vis#behavior !s)) ext.asm_gotos
         else ext.asm_gotos
       in
       if asm_outputs != asm_outputs_pre
       || asm_inputs != asm_inputs_pre
       || asm_gotos != ext.asm_gotos
       then
         begin
           let ext = { ext with asm_outputs; asm_inputs; asm_gotos } in
           Asm(sl,isvol,Some ext,l)
         end else i)
  | Code_annot (a,l) ->
    let a' = visitCilCodeAnnotation vis a in
    if a != a' then Code_annot(a',l) else i

(* visit all nodes in a Cil statement tree in preorder *)
and visitCilStmt (vis:cilVisitor) (s: stmt) : stmt =
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = Cil_datatype.Stmt.loc s in
  vis#push_stmt s; (*(vis#behavior.memo_stmt s);*)
  assertEmptyQueue vis;
  let toPrepend : instr list ref = ref [] in (* childrenStmt may add to this *)
  let res =
    doVisitCil vis
      (Visitor_behavior.Memo.stmt vis#behavior) vis#vstmt (childrenStmt toPrepend) s in
  (* Now see if we have saved some instructions *)
  toPrepend := !toPrepend @ vis#unqueueInstr ();
  match !toPrepend with
    [] -> vis#pop_stmt s; res (* Return the same statement *)
  | _ :: _ as instr_list ->
    let make i = mkStmt ~ghost:res.ghost (Instr i) in
    let last = mkStmt ~ghost:res.ghost res.skind in
    let block = mkBlockNonScoping (List.map make instr_list @ [ last ]) in
    block.battrs <- Ast_attributes.add (vis_tmp_attr, []) block.battrs;
    (* Make our statement contain the instructions to prepend *)
    res.skind <- Block block;
    vis#pop_stmt s; res

and childrenStmt (toPrepend: instr list ref) (vis:cilVisitor) (s:stmt): stmt =
  let fExp e = (visitCilExpr vis e) in
  let fBlock b = visitCilBlock vis b in
  let fInst i = visitCilInstr vis i in
  let fLoopAnnot a = List.map_no_copy (visitCilCodeAnnotation vis) a in
  (* Just change the statement kind *)
  let skind' =
    match s.skind with
      Break _ | Continue _ | Return (None, _) -> s.skind
    | UnspecifiedSequence seq ->
      let seq' =
        List.map_no_copy
          (function (stmt,modified,writes,reads,calls) as orig->
             let stmt' = visitCilStmt vis stmt in
             (match stmt'.skind with
              | Block b -> b.battrs <- Ast_attributes.drop vis_tmp_attr b.battrs;
              | _ -> ());
             (* might make sense for the default to be
                to just copy the varinfo when using the copy visitor,
                and not apply vvrbl, i.e. not using vis but generic_visitor ?
             *)
             let modified' = List.map_no_copy (visitCilLval vis) modified in
             let writes' = List.map_no_copy (visitCilLval vis) writes in
             let reads' = List.map_no_copy (visitCilLval vis) reads in
             let calls' =
               if Visitor_behavior.is_copy vis#behavior then
                 (* we need new references anyway, no need for Extlib.map_no_copy *)
                 List.map (fun x -> ref (Visitor_behavior.Memo.stmt vis#behavior !x)) calls
               else calls
             in
             if stmt' != stmt || writes' != writes || reads' != reads ||
                modified != modified' || calls' != calls
             then
               (stmt',modified', writes',reads',calls')
             else orig)
          seq
      in
      if seq' != seq then UnspecifiedSequence seq' else s.skind
    | Goto (sr,l) ->
      if Visitor_behavior.is_copy vis#behavior then
        Goto(ref (Visitor_behavior.Memo.stmt vis#behavior !sr),l)
      else s.skind
    | Return (Some e, l) ->
      let e' = fExp e in
      if e' != e then Return (Some e', l) else s.skind
    | Loop (a, b, l, s1, s2) ->
      let a' = fLoopAnnot a in
      let b' = fBlock b in
      if a' != a || b' != b then Loop (a', b', l, s1, s2) else s.skind
    | If(e, s1, s2, l) ->
      let e' = fExp e in
      (*if e queued any instructions, pop them here and remember them so that
        they are inserted before the If stmt, not in the then block. *)
      toPrepend := vis#unqueueInstr ();
      let s1'= fBlock s1 in let s2'= fBlock s2 in
      (* the stmts in the blocks should have cleaned up after themselves.*)
      assertEmptyQueue vis;
      if e' != e || s1' != s1 || s2' != s2 then
        If(e', s1', s2', l) else s.skind
    | Switch (e, b, stmts, l) ->
      let e' = fExp e in
      toPrepend := vis#unqueueInstr (); (* insert these before the switch *)
      let b' = fBlock b in
      let stmts' = List.map_no_copy (Visitor_behavior.Get.stmt vis#behavior) stmts in
      (* the stmts in b should have cleaned up after themselves.*)
      assertEmptyQueue vis;
      if e' != e || b' != b || stmts' != stmts then
        Switch (e', b', stmts', l) else s.skind
    | Instr i ->
      begin match fInst i with
        | [i'] when i' == i -> s.skind
        | il -> stmt_of_instr_list_visitor ~loc:(Cil_datatype.Instr.loc i) il
      end
    | Block b ->
      let b' = fBlock b in
      if b' != b then Block b' else s.skind
    | Throw (e,loc) ->
      let visit (e,t as exc) =
        let e' = fExp e in
        let t' = visitCilType vis t in
        if e != e' || t != t' then (e',t') else exc
      in
      let e' = Option.map_no_copy visit e in
      if e != e' then Throw (e,loc) else s.skind
    | TryCatch (b,l,loc) ->
      let b' = fBlock b in
      let visit (v,b as catch) =
        let v' = visitCilCatch_binder vis v in
        let b' = fBlock b in
        if v != v' || b != b' then (v', b') else catch
      in
      let l' = List.map_no_copy visit l in
      if b != b' || l != l' then TryCatch (b', l',loc) else s.skind
    | TryFinally (b, h, l) ->
      let b' = fBlock b in
      let h' = fBlock h in
      if b' != b || h' != h then TryFinally(b', h', l) else s.skind
    | TryExcept (b, (il, e), h, l) ->
      let b' = fBlock b in
      assertEmptyQueue vis;
      (* visit the instructions *)
      let il' = List.concat_map_no_copy fInst il in
      (* Visit the expression *)
      let e' = fExp e in
      let il'' =
        let more = vis#unqueueInstr () in
        if more != [] then
          il' @ more
        else
          il'
      in
      let h' = fBlock h in
      (* Now collect the instructions *)
      if b' != b || il'' != il || e' != e || h' != h then
        TryExcept(b', (il'', e'), h', l)
      else s.skind
  in
  if skind' != s.skind then s.skind <- skind';
  enforceGhostStmtCoherence s ;
  (* Visit the labels *)
  let labels' =
    let fLabel = function
        Case (e, l) as lb ->
        let e' = fExp e in
        if e' != e then Case (e', l) else lb
      | lb -> lb
    in
    List.map_no_copy fLabel s.labels
  in
  if labels' != s.labels then s.labels <- labels';
  s

and visitCilCatch_binder vis cb =
  match cb with
  | Catch_exn (v,l) ->
    let visit_one_conversion (v, b as conv) =
      let v' = visitCilVarDecl vis v in
      let b' = visitCilBlock vis b in
      if v != v' || b != b' then (v', b') else conv
    in
    let v' = visitCilVarDecl vis v in
    let l' = List.map_no_copy visit_one_conversion l in
    if v != v' || l != l' then Catch_exn(v',l') else cb
  | Catch_all -> cb
and visitCilBlock (vis: cilVisitor) (b: block) : block =
  let b' = Visitor_behavior.cblock vis#behavior b in
  if Visitor_behavior.is_copy vis#behavior then begin
    (* in case we are the main block of the current function,
       update immediately the sbody, so that makeLocalVar can be used
       seamlessly by the underlying visitor and associate the
       local variable to the appropriate sbody when no inner block is present.
    *)
    match vis#current_func with
    | Some fd when fd.sbody == b ->
      (Visitor_behavior.Get.fundec vis#behavior fd).sbody <- b'
    | Some _ | None -> ()
  end;
  doVisitCil vis id vis#vblock childrenBlock b'
and childrenBlock (vis: cilVisitor) (b: block) : block =
  let fStmt s = visitCilStmt vis s in
  (* first visit locals and update the field. This way, statements visitors
     that wish to create a local into the innermost scope can simply append
     it to the current block.
  *)
  let locals' = List.map_no_copy (Visitor_behavior.Get.varinfo vis#behavior) b.blocals in
  let statics' = List.map_no_copy (Visitor_behavior.Get.varinfo vis#behavior) b.bstatics in
  b.blocals <- locals';
  b.bstatics <- statics';
  let stmts' = List.map_no_copy fStmt b.bstmts in
  b.bstmts <- stmts';
  flatten_transient_sub_blocks b

and visitCilType (vis : cilVisitor) (t : typ) : typ =
  doVisitCil vis id vis#vtype childrenType t
and childrenType (vis : cilVisitor) (t : typ) : typ =
  (* look for types referred to inside t's definition *)
  let fTyp t  = visitCilType vis t in
  let fAttr a = visitCilAttributes vis a in
  let tattr = visitCilAttributes vis t.tattr in
  match t.tnode with
  | TPtr t1 ->
    let t1' = fTyp t1 in
    if t1' != t1 || tattr != t.tattr
    then Cil_const.mk_tptr ~tattr t1'
    else t
  | TArray (t1, None) ->
    let t1' = fTyp t1 in
    if t1' != t1 || tattr != t.tattr
    then Cil_const.mk_tarray ~tattr t1' None
    else t
  | TArray(t1, Some e) ->
    let t1' = fTyp t1 in
    let e' = visitCilExpr vis e in
    if t1' != t1 || e' != e  || tattr != t.tattr
    then Cil_const.mk_tarray ~tattr t1' (Some e')
    else t

  (* DON'T recurse into the compinfo, this is done in visitCilGlobal.
     User can iterate over cinfo.cfields manually, if desired.*)
  | TComp cinfo ->
    let cinfo' = Visitor_behavior.Get.compinfo vis#behavior cinfo in
    if tattr != t.tattr || cinfo' != cinfo
    then Cil_const.mk_tcomp ~tattr cinfo'
    else t

  | TFun (rettype, args, isva) ->
    let rettype' = fTyp rettype in
    (* iterate over formals, as variable declarations *)
    let argslist = argsToList args in
    let visitArg ((an,at,aa) as arg) =
      let at' = fTyp at in
      let aa' = fAttr aa in
      if at' != at || aa' != aa then (an,at',aa') else arg
    in
    let argslist' = List.map_no_copy visitArg argslist in
    if rettype' != rettype || argslist' != argslist || tattr != t.tattr then
      let args' = if argslist' == argslist then args else Some argslist' in
      Cil_const.mk_tfun ~tattr rettype' args' isva
    else t

  | TNamed t1 ->
    let t1' = Visitor_behavior.Get.typeinfo vis#behavior t1 in
    if tattr != t.tattr || t1' != t1
    then Cil_const.mk_tnamed ~tattr t1'
    else t
  | TEnum enum ->
    let enum' = Visitor_behavior.Get.enuminfo vis#behavior enum in
    if tattr != t.tattr || enum' != enum
    then Cil_const.mk_tenum ~tattr enum'
    else t
  | TVoid | TInt _ | TFloat _ | TBuiltin_va_list ->
    (* no nested type. visit only the attributes. *)
    if tattr != t.tattr then {t with tattr} else t

(* for declarations, we visit the types inside; but for uses, *)
(* we just visit the varinfo node *)
and visitCilVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  Current_loc.with_loc v.vdecl
    (doVisitCil vis (Visitor_behavior.Memo.varinfo vis#behavior)
       vis#vvdec childrenVarDecl) v

and childrenVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  (* in case of refresh visitor, the associated new logic var has a different
     id. We must visit the original logic var associated to it. *)
  let visit_orig_var_assoc lv =
    let o = Visitor_behavior.Get_orig.logic_var vis#behavior lv in
    visitCilLogicVarDecl vis o
  in
  let typ = visitCilType vis v.vtype in
  v.vattr <- visitCilAttributes vis v.vattr;
  v.vlogic_var_assoc <- Option.map_no_copy visit_orig_var_assoc v.vlogic_var_assoc;
  update_var_type v typ;
  v

and visitCilVarUse vis v =
  doVisitCil vis (Visitor_behavior.Get.varinfo vis#behavior) vis#vvrbl alphabetabeta v

and visitCilAttributes (vis: cilVisitor) (al: attributes) : attributes =
  let al' =
    List.concat_map_no_copy
      (doVisitListCil vis
         id vis#vattr childrenAttribute) al in
  if al' != al then
    (* Must re-sort *)
    Ast_attributes.add_list al' []
  else
    al
and childrenAttribute (vis: cilVisitor) ((n, args) as a: attribute) : attribute =
  let fAttrP a = visitCilAttrParams vis a in
  let args' = List.map_no_copy fAttrP args in
  if args' != args then (n, args') else a

and visitCilAttrParams (vis: cilVisitor) (a: attrparam) : attrparam =
  doVisitCil vis id vis#vattrparam childrenAttrparam a
and childrenAttrparam (vis: cilVisitor) (aa: attrparam) : attrparam =
  let fTyp t  = visitCilType vis t in
  let fAttrP a = visitCilAttrParams vis a in
  match aa with
    AInt _ | AStr _ -> aa
  | ACons(n, args) ->
    let args' = List.map_no_copy fAttrP args in
    if args' != args then ACons(n, args') else aa
  | ASizeOf t ->
    let t' = fTyp t in
    if t' != t then ASizeOf t' else aa
  | ASizeOfE e ->
    let e' = fAttrP e in
    if e' != e then ASizeOfE e' else aa
  | AAlignOf (t, i) ->
    let t' = fTyp t in
    if t' != t then AAlignOf (t', i) else aa
  | AAlignOfE (e, i) ->
    let e' = fAttrP e in
    if e' != e then AAlignOfE (e', i) else aa
  | AUnOp (uo, e1) ->
    let e1' = fAttrP e1 in
    if e1' != e1 then AUnOp (uo, e1') else aa
  | ABinOp (bo, e1, e2) ->
    let e1' = fAttrP e1 in
    let e2' = fAttrP e2 in
    if e1' != e1 || e2' != e2 then ABinOp (bo, e1', e2') else aa
  | ADot (ap, s) ->
    let ap' = fAttrP ap in
    if ap' != ap then ADot (ap', s) else aa
  | AStar ap ->
    let ap' = fAttrP ap in
    if ap' != ap then AStar ap' else aa
  | AAddrOf ap ->
    let ap' = fAttrP ap in
    if ap' != ap then AAddrOf ap' else aa
  | AIndex (e1, e2) ->
    let e1' = fAttrP e1 in
    let e2' = fAttrP e2 in
    if e1' != e1 || e2' != e2 then AIndex (e1', e2') else aa
  | AQuestion (e1, e2, e3) ->
    let e1' = fAttrP e1 in
    let e2' = fAttrP e2 in
    let e3' = fAttrP e3 in
    if e1' != e1 || e2' != e2 || e3' != e3
    then AQuestion (e1', e2', e3') else aa


let rec fix_succs_preds_block b block =
  List.iter (fix_succs_preds b) block.bstmts
and fix_succs_preds b stmt =
  stmt.succs <- List.map_no_copy (Visitor_behavior.Get.stmt b) stmt.succs;
  stmt.preds <- List.map_no_copy (Visitor_behavior.Get.stmt b) stmt.preds;
  match stmt.skind with
    If(_,bthen,belse,_) ->
    fix_succs_preds_block b bthen;
    fix_succs_preds_block b belse
  | Switch(e,cases,stmts,l) ->
    fix_succs_preds_block b cases;
    stmt.skind <- Switch(e,cases,List.map (Visitor_behavior.Get.stmt b) stmts,l)
  | Loop(annot,block,loc,stmt1,stmt2) ->
    fix_succs_preds_block b block;
    let stmt1' = Option.map_no_copy (Visitor_behavior.Get.stmt b) stmt1 in
    let stmt2' = Option.map_no_copy (Visitor_behavior.Get.stmt b) stmt2 in
    stmt.skind <- Loop(annot,block,loc,stmt1',stmt2')
  | Block block -> fix_succs_preds_block b block
  | TryFinally(block1,block2,_) ->
    fix_succs_preds_block b block1;
    fix_succs_preds_block b block2
  | TryExcept(block1,_,block2,_) ->
    fix_succs_preds_block b block1;
    fix_succs_preds_block b block2
  | _ -> ()

let rec visitCilFunction (vis : cilVisitor) (f : fundec) : fundec =
  if debugVisit then Kernel.feedback "Visiting function %s" f.svar.vname ;
  assertEmptyQueue vis;
  vis#set_current_func f;
  (* update fundec tables *)
  let f = Visitor_behavior.Memo.fundec vis#behavior f in
  let f =
    doVisitCil vis id (* copy has already been done *)
      vis#vfunc childrenFunction f
  in
  let toPrepend = vis#unqueueInstr () in
  if toPrepend <> [] then
    f.sbody.bstmts <-
      (List.map (fun i -> mkStmt (Instr i)) toPrepend) @ f.sbody.bstmts;
  if Visitor_behavior.is_copy vis#behavior then begin
    fix_succs_preds_block vis#behavior f.sbody;
    f.sallstmts <-
      List.rev (List.rev_map (Visitor_behavior.Get.stmt vis#behavior) f.sallstmts)
  end;
  vis#reset_current_func ();
  f

and childrenFunction (vis : cilVisitor) (f : fundec) : fundec =
  (* we have already made a copy of the svar, but not visited it.
     Use the original variable as argument of visitCilVarDecl,
     update fundec table in case the vid gets changed. *)
  let v = Visitor_behavior.Get_orig.varinfo vis#behavior f.svar in
  let nv = visitCilVarDecl vis v in
  if not (Cil_datatype.Varinfo.equal nv f.svar) then begin
    Kernel.fatal
      "Visiting the varinfo declared for function %a changes its id."
      !pp_varinfo_ref nv
  end;
  f.svar <- nv; (* hit the function name *)
  (* visit the formals *)
  let newformals = List.map_no_copy (visitCilVarDecl vis) f.sformals in
  (* visit local declarations *)
  f.slocals <- List.map_no_copy (visitCilVarDecl vis) f.slocals;
  (* Make sure the type reflects the formals *)
  let selection = State_selection.singleton FormalsDecl.self in
  if Visitor_behavior.is_copy vis#behavior || newformals != f.sformals then begin
    apply_on_project ~selection vis (setFormals f) newformals;
  end;
  (* Remember any new instructions that were generated while visiting
     variable declarations. *)
  let toPrepend = vis#unqueueInstr () in
  f.sbody <- visitCilBlock vis f.sbody;       (* visit the body *)
  if toPrepend <> [] then
    f.sbody.bstmts <-
      (List.map (fun i -> mkStmt (Instr i)) toPrepend) @ f.sbody.bstmts;
  if not (is_empty_funspec f.sspec) then
    f.sspec <- visitCilFunspec vis f.sspec;
  f

let childrenFieldInfo vis fi =
  (* already done at copy creation *)
  (* fi.fcomp <- vis#behavior.get_compinfo fi.fcomp; *)
  fi.ftype <- visitCilType vis fi.ftype;
  fi.fattr <- visitCilAttributes vis fi.fattr;
  fi

let visitCilFieldInfo vis f =
  let f = Visitor_behavior.Get_orig.fieldinfo vis#behavior f in
  doVisitCil vis (Visitor_behavior.Memo.fieldinfo vis#behavior) vis#vfieldinfo childrenFieldInfo f

let childrenCompInfo vis comp =
  comp.cfields <- Option.map_no_copy (List.map_no_copy (visitCilFieldInfo vis)) comp.cfields;
  comp.cattr <- visitCilAttributes vis comp.cattr;
  comp

let visitCilCompInfo vis c =
  doVisitCil vis (Visitor_behavior.Memo.compinfo vis#behavior) vis#vcompinfo childrenCompInfo c

let childrenEnumItem vis e =
  e.eival <- visitCilExpr vis e.eival;
  e.eihost <- Visitor_behavior.Get.enuminfo vis#behavior e.eihost;
  e

let visitCilEnumItem vis e =
  doVisitCil vis (Visitor_behavior.Memo.enumitem vis#behavior) vis#venumitem childrenEnumItem e

let childrenEnumInfo vis e =
  e.eitems <- List.map_no_copy (visitCilEnumItem vis) e.eitems;
  e.eattr <- visitCilAttributes vis e.eattr;
  e

let visitCilEnumInfo vis e =
  doVisitCil vis (Visitor_behavior.Memo.enuminfo vis#behavior) vis#venuminfo childrenEnumInfo e

let rec visitCilGlobal (vis: cilVisitor) (g: global) : global list =
  Current_loc.with_loc (Global.loc g)
    (doVisitListCil vis id vis#vglob childrenGlobal) g
and childrenGlobal (vis: cilVisitor) (g: global) : global =
  match g with
  | GFun (f, l) ->
    let f' = visitCilFunction vis f in
    if f' != f then GFun (f', l) else g
  | GType(t, l) ->
    let t' = Visitor_behavior.Memo.typeinfo vis#behavior t in
    t'.ttype <- visitCilType vis t'.ttype;
    if t' != t then GType(t',l) else g
  | GEnumTagDecl (enum,l) ->
    let enum' = Visitor_behavior.Memo.enuminfo vis#behavior enum in
    if enum != enum' then GEnumTagDecl(enum',l) else g
  (* real visit'll be done in the definition *)
  | GCompTagDecl (comp,l) ->
    let comp' = Visitor_behavior.Memo.compinfo vis#behavior comp in
    if comp != comp' then GCompTagDecl(comp',l) else g
  | GEnumTag (enum, l) ->
    let enum' = visitCilEnumInfo vis enum in
    if enum != enum' then GEnumTag(enum',l) else g
  | GCompTag (comp, l) ->
    let comp' = visitCilCompInfo vis comp in
    if comp != comp' then GCompTag(comp',l) else g
  | GVarDecl(v, l) ->
    let v' = visitCilVarDecl vis v in
    if v' != v then GVarDecl (v', l) else g
  | GFunDecl(spec, v, l) ->
    let form =
      try Some (getFormalsDecl v) with Not_found -> None
    in
    let v' = visitCilVarDecl vis v in
    let form' = Option.map_no_copy (List.map_no_copy (visitCilVarDecl vis)) form in
    let spec' =
      if is_empty_funspec spec then begin
        if Visitor_behavior.is_copy vis#behavior then
          empty_funspec ()
        else spec (* do not need to change it if it's not a copy visitor. *)
      end else begin
        visitCilFunspec vis spec
      end
    in
    if v' != v || spec' != spec || form != form' then
      begin
        (match form' with
         | Some formals
           when Visitor_behavior.is_copy vis#behavior || form != form' ->
           let selection = State_selection.singleton FormalsDecl.self in
           apply_on_project
             ~selection vis (unsafeSetFormalsDecl v') formals
         | Some _ | None -> ());
        GFunDecl (spec', v', l)
      end
    else g
  | GVar (v, inito, l) ->
    let v' = visitCilVarDecl vis v in
    let inito' = Visitor_behavior.cinitinfo vis#behavior inito in
    (match inito'.init with
       None -> ()
     | Some i -> let i' = visitCilInit_or_str vis v i in
       if i' != i then inito'.init <- Some i');
    if v' != v || inito' != inito then GVar (v', inito', l) else g
  | GPragma (a, l) -> begin
      match visitCilAttributes vis [a] with
        [a'] -> if a' != a then GPragma (a', l) else g
      | _ -> Kernel.fatal "visitCilAttributes returns more than one attribute"
    end
  | GAnnot (a,l) ->
    let a' = visitCilAnnotation vis a in
    if a' != a then GAnnot(a',l) else g
  | GText _ | GAsm _ -> g

let bytesSizeOfInt (ik: ikind): int =
  match ik with
  | IChar | ISChar | IUChar | IBool -> 1
  | IInt | IUInt -> Machine.Sizeof.int ()
  | IShort | IUShort -> Machine.Sizeof.short ()
  | ILong | IULong -> Machine.Sizeof.long ()
  | ILongLong | IULongLong -> Machine.Sizeof.longlong ()
  | IInt128 | IUInt128 -> 16

let bitsSizeOfInt ik = 8 * bytesSizeOfInt ik

let intKindForSize (s:int) (unsigned:bool) : ikind =
  let open Machine in
  if unsigned then
    (* Test the most common sizes first *)
    if s = 1 then IUChar
    else if s = Sizeof.int () then IUInt
    else if s = Sizeof.long () then IULong
    else if s = Sizeof.short () then IUShort
    else if s = Sizeof.longlong () then IULongLong
    else raise Not_found
  else
    (* Test the most common sizes first *)
  if s = 1 then ISChar
  else if s = Sizeof.int () then IInt
  else if s = Sizeof.long () then ILong
  else if s = Sizeof.short () then IShort
  else if s = Sizeof.longlong () then ILongLong
  else raise Not_found

let uint128_t () = Cil_const.mk_tint (intKindForSize 16 true)
let uint64_t  () = Cil_const.mk_tint (intKindForSize 8 true)
let uint32_t  () = Cil_const.mk_tint (intKindForSize 4 true)
let uint16_t  () = Cil_const.mk_tint (intKindForSize 2 true)
let int128_t  () = Cil_const.mk_tint (intKindForSize 16 false)
let int64_t   () = Cil_const.mk_tint (intKindForSize 8 false)
let int32_t   () = Cil_const.mk_tint (intKindForSize 4 false)
let int16_t   () = Cil_const.mk_tint (intKindForSize 2 false)

let floatKindForSize (s:int) =
  if s = Machine.Sizeof.double () then FDouble
  else if s = Machine.Sizeof.float () then FFloat
  else if s = Machine.Sizeof.longdouble () then FLongDouble
  else raise Not_found

(** Returns true if and only if the given integer type is signed. *)
let isSigned = function
  | IUChar | IBool
  | IUShort
  | IUInt
  | IULong
  | IULongLong
  | IUInt128 ->
    false
  | ISChar
  | IShort
  | IInt
  | ILong
  | ILongLong
  | IInt128 ->
    true
  | IChar ->
    not (Machine.char_is_unsigned ())

let max_signed_number nrBits =
  let n = nrBits-1 in
  Z.(pred (shift_left one n))
let max_unsigned_number nrBits =
  Z.(pred (shift_left one nrBits))
let min_signed_number nrBits =
  let n = nrBits-1 in
  Z.(neg (shift_left one n))

let debugTruncation = false

(* True if the integer fits within the kind's range *)
let fitsInInt k i =
  let signed = isSigned k in
  let nrBits =
    let unsignedbits = 8 * (bytesSizeOfInt k) in
    if signed then
      unsignedbits-1
    else
      unsignedbits
  in
  let max_strict_bound =
    Z.(shift_left one nrBits)
  in
  let min_bound = if signed then Z.neg max_strict_bound
    else Z.zero
  in
  let fits = Z.leq min_bound i && Z.lt i max_strict_bound in
  if debugTruncation then
    Kernel.debug "Fits in %a %a : %b@."
      !pp_ikind_ref k Z.pretty i fits;
  fits

(* Represents an integer as for a given kind.
   Returns a flag saying whether the value was changed
   during truncation (because it was too large to fit in k). *)
let truncateInteger64 (k: ikind) i =
  if fitsInInt k i then
    i, false
  else
    let i' =
      let nrBits = 8 * (bytesSizeOfInt k) in
      let max_strict_bound = Z.(shift_left one nrBits) in
      let modulo = Z.erem i max_strict_bound in
      let signed = isSigned k in
      if signed then
        let max_signed_strict_bound =
          Z.shift_right max_strict_bound 1
        in
        if Z.geq modulo max_signed_strict_bound then
          Z.sub modulo max_strict_bound
        else if Z.lt modulo (Z.neg max_signed_strict_bound)
        then Z.add modulo max_strict_bound
        else modulo
      else
      if Z.lt modulo Z.zero then
        Z.add modulo max_strict_bound
      else
        modulo
    in
    if debugTruncation then
      Kernel.debug ~level:3 "Truncate %a to %a: %a"
        Z.pretty i !pp_ikind_ref k Z.pretty i';
    i', true

exception Not_representable
let intKindForValue i (unsigned: bool) =
  if unsigned then
    if fitsInInt IUChar i then IUChar
    else if fitsInInt IUShort i then IUShort
    else if fitsInInt IUInt i then IUInt
    else if fitsInInt IULong i then IULong
    else if fitsInInt IULongLong i then IULongLong
    else raise Not_representable
  else
  if fitsInInt ISChar i then ISChar
  else if fitsInInt IShort i then IShort
  else if fitsInInt IInt i then IInt
  else if fitsInInt ILong i then ILong
  else if fitsInInt ILongLong i then ILongLong
  else raise Not_representable

(* True is an double constant is finite for a kind *)
let isFiniteFloat fk f =
  let f = Floating_point.round_if_single_precision fk f in
  Floating_point.is_finite f

let isExactFloat fk r =
  r.r_upper = r.r_lower && isFiniteFloat fk r.r_nearest

(* Construct an integer constant with possible truncation if the kind is not
   specified  *)
let kinteger64 ~loc ?repr ?kind i =
  if debugTruncation then
    Kernel.debug ~level:3 "kinteger64 %a" Z.pretty i;
  let kind = match kind with
    | None ->
      (* compute the best ikind: [int] whenever possible and, if no signed type
         is possible, try unsigned long long. *)
      if fitsInInt IInt i then IInt
      else begin
        try intKindForValue i false
        with Not_representable as exn ->
          if fitsInInt IULongLong i then IULongLong else raise exn
      end
    | Some k -> k
  in
  let i', _truncated = truncateInteger64 kind i in
  new_exp ~loc (Const (CInt64(i' , kind,  repr)))

(* Construct an integer of a given kind. *)
let kinteger ~loc kind (i: int) = kinteger64 ~loc ~kind (Z.of_int i)

(* Construct an integer. Use only for values that fit on 31 bits *)
let integer_constant i = CInt64(Z.of_int i, IInt, None)
(* Construct an integer. Use only for values that fit on 31 bits *)
let integer ~loc (i: int) = new_exp ~loc (Const (integer_constant i))

let kfloat ~loc k f =
  let f = Floating_point.round_if_single_precision k f in
  new_exp ~loc (Const (CReal(f,k,None)))

let zero      ~loc = integer ~loc 0
let one       ~loc = integer ~loc 1
let mone      ~loc = integer ~loc (-1)

let integer_lconstant v = TConst (Integer (Z.of_int v,None))

let lconstant ?(loc=Fileloc.unknown) v =
  { term_node = TConst (Integer (v,None)); term_loc = loc;
    term_name = []; term_type = Linteger;}

let lzero ?(loc=Fileloc.unknown) () = lconstant ~loc Z.zero
let lone  ?(loc=Fileloc.unknown) () = lconstant ~loc Z.one
let lmone ?(loc=Fileloc.unknown) () = lconstant ~loc (Z.minus_one)

(** Given the character c in a (CChr c), sign-extend it to 32 bits.
    (This is the official way of interpreting character constants, according
    to ISO C 6.4.4.4.10, which says that character constants are chars cast
    to ints) *)
let charConstToInt c =
  let c' = Char.code c in
  if c' < 128
  then Z.of_int c'
  else Z.of_int (c' - 256)

let charConstToIntConstant c =
  CInt64(charConstToInt c, IInt, None)

let rec isInteger e = match e.enode with
  | Const(CInt64 (n,_,_)) -> Some n
  | Const(CChr c) -> Some (charConstToInt c)
  | Const(CEnum {eival = v}) -> isInteger v
  | CastE(_, e) -> isInteger e (* BY: This is really strange... *)
  | _ -> None

let isZero (e: exp) : bool =
  match isInteger e with
  | None -> false
  | Some i -> Z.is_zero i

let rec isLogicZero t = match t.term_node with
  | TConst (Integer (n,_)) -> Z.is_zero n
  | TConst (LChr c) -> Char.code c = 0
  | TCast(_, _, t) -> isLogicZero t
  | _ -> false

let isLogicNull t =
  isLogicZero t ||
  (let rec aux t = match t.term_node with
      | Tnull -> true
      | TCast(_,_, t) -> aux t
      | _ -> false
   in aux t)

let parseIntAux (str:string) =
  let hasSuffix str =
    let l = String.length str in
    fun s ->
      let ls = String.length s in
      l >= ls &&
      s = String.uppercase_ascii (String.sub str (l - ls) ls)
  in
  let l = String.length str in
  (* See if it is octal or hex or binary *)
  let octalhexbin = l >= 1 && str.[0] = '0' in
  (* The length of the suffix and a list of possible kinds. See ISO
   * 6.4.4.1 *)
  let hasSuffix = hasSuffix str in
  let suffixlen, kinds =
    if hasSuffix "ULL" || hasSuffix "LLU" then
      3, [IULongLong]
    else if hasSuffix "LL" then
      2, if octalhexbin then [ILongLong; IULongLong] else [ILongLong]
    else if hasSuffix "UL" || hasSuffix "LU" then
      2, [IULong; IULongLong]
    else if hasSuffix "L" then
      1, if octalhexbin then [ILong; IULong; ILongLong; IULongLong]
      else [ILong; ILongLong]
    else if hasSuffix "U" then
      1, [IUInt; IULong; IULongLong]
    else
      0, if octalhexbin
      then [IInt; IUInt; ILong; IULong; ILongLong; IULongLong]
      else [IInt; ILong; ILongLong]
  in
  (* Convert to integer. To prevent overflow we do the arithmetic
   * on Big_int and we take care of overflow. We work only with
   * positive integers since the lexer takes care of the sign *)
  let rec toInt base (acc: Z.t) (idx: int) =
    let doAcc what =
      if Z.geq what base
      then
        Error (Format.asprintf
                 "Invalid digit %a in integer literal '%s' in base %a."
                 Z.pretty what
                 str
                 Z.pretty base)
      else
        let acc' = Z.add what (Z.mul base acc) in
        toInt base acc' (idx + 1)
    in
    if idx >= l - suffixlen then begin
      Ok acc
    end else
      let ch = String.get str idx in
      if ch >= '0' && ch <= '9' then
        doAcc (Z.of_int (Char.code ch - Char.code '0'))
      else if  ch >= 'a' && ch <= 'f'  then
        doAcc (Z.of_int (10 + Char.code ch - Char.code 'a'))
      else if  ch >= 'A' && ch <= 'F'  then
        doAcc (Z.of_int (10 + Char.code ch - Char.code 'A'))
      else
        Error (Format.asprintf
                 "Invalid character %c in integer literal: %s" ch str)
  in
  let i =
    if octalhexbin && l >= 2 then
      (match String.get str 1 with
       | 'x' | 'X' (* Hexadecimal number *) ->
         toInt Z.(of_int 16) Z.zero 2
       | 'b' | 'B' ->  (* Binary number *)
         toInt Z.(of_int 2) Z.zero 2
       | _ -> (* Octal number *)
         toInt Z.(of_int 8) Z.zero 1)
    else
      toInt Z.(of_int 10) Z.zero 0
  in
  i,kinds

let parseIntRes s = fst (parseIntAux s)

let parseInt s =
  match parseIntRes s with
  | Ok i -> i
  | Error msg -> Kernel.abort ~current:true "%s" msg

let parseIntLogic ~loc str =
  let i = parseInt str in
  { term_node = TConst (Integer (i,Some str)) ; term_loc = loc;
    term_name = []; term_type = Linteger;}

let parseIntExpRes ~loc repr =
  let i, kinds = parseIntAux repr in
  Result.bind i
    (fun i ->
       let rec loop = function
         | k::rest ->
           if fitsInInt k i then (* i fits in the current type. *)
             Ok (kinteger64 ~loc ~repr ~kind:k i)
           else loop rest
         | [] ->
           Error (Format.asprintf "Cannot represent the integer %s" repr)
       in
       loop kinds)

let parseIntExp ~loc repr =
  match parseIntExpRes ~loc repr with
  | Ok e -> e
  | Error msg -> Kernel.fatal ~current:true "%s" msg

let mkStmtCfg ~before ~(new_stmtkind:stmtkind) ~(ref_stmt:stmt) : stmt =
  let new_ = { skind = new_stmtkind;
               labels = [];
               sid = -1; succs = []; preds = []; ghost = false; sattr = [] }
  in
  new_.sid <- Cil_const.new_raw_sid ();
  if before then begin
    new_.succs <- [ref_stmt];
    let old_preds = ref_stmt.preds in
    ref_stmt.preds <- [new_];
    new_.preds <- old_preds;
    List.iter
      (fun pred_stmt ->
         pred_stmt.succs <-
           (List.map
              (fun a_succ -> if a_succ.sid = ref_stmt.sid then new_ else a_succ)
              pred_stmt.succs))
      old_preds
  end else begin
    let old_succs = ref_stmt.succs in
    ref_stmt.succs <- [new_];
    new_.preds <- [ref_stmt];
    new_.succs <- old_succs;
    List.iter
      (fun succ_stmt ->
         succ_stmt.preds <-
           (List.map
              (fun a_pred -> if a_pred.sid = ref_stmt.sid then new_ else a_pred)
              succ_stmt.preds))
      old_succs
  end;
  new_

let mkStmtCfgBlock sl =
  let sid = Cil_const.new_raw_sid () in
  let n = mkStmt (Block (mkBlock sl)) in
  n.sid <- sid;
  match sl with
  | [] -> n
  | s::_ ->
    let old_preds = s.preds in
    n.succs <- [s];
    n.preds <- s.preds;
    List.iter
      (fun pred_stmt ->
         pred_stmt.succs <-
           (List.map
              (fun a_succ -> if a_succ.sid = s.sid then
                  n
                else a_succ)
              pred_stmt.succs))
      old_preds;
    n

let mkEmptyStmt ?ghost ?valid_sid ?sattr ?(loc=Fileloc.unknown) () =
  mkStmt ?ghost ?valid_sid ?sattr (Instr (Skip loc))

let mkStmtOneInstr ?ghost ?valid_sid ?sattr i =
  mkStmt ?ghost ?valid_sid ?sattr (Instr i)

let isSignedInteger ty =
  match Ast_types.unroll_skel ty with
  | TInt ik | TEnum {ekind=ik} -> isSigned ik
  | _ -> false

let isUnsignedInteger ty =
  match Ast_types.unroll_skel ty with
  | TInt ik | TEnum {ekind=ik} -> not (isSigned ik)
  | _ -> false

let var vi : lval = (Var vi, NoOffset)
(* let assign vi e = Cil_datatype.Instrs(Set (var vi, e), lu) *)

let evar ?(loc=Fileloc.unknown) vi = new_exp ~loc (Lval (var vi))

let mkLoop ?sattr ~(guard:exp) ~(body: stmt list) () : stmt list =
  (* Do it like this so that the pretty printer recognizes it *)
  [ mkStmt ~valid_sid:true ?sattr
      (Loop ([],
             mkBlock
               (mkStmt ~valid_sid:true
                  (If(guard,
                      mkBlock [],
                      mkBlock [ mkStmt ~valid_sid:true (Break guard.eloc)], guard.eloc)) ::
                body), guard.eloc, None, None)) ]

let mkWhile ?sattr ~(guard:exp) ~(body: stmt list) () : stmt list =
  let sattr = [("while", [])] @ Option.value ~default:[] sattr in
  mkLoop ~sattr ~guard ~body ()

let mkDoWhile ?sattr ~(body: stmt list) ~(guard:exp) () : stmt list =
  let sattr = [("dowhile", [])] @ Option.value ~default:[] sattr in
  let exit_stmt =
    mkStmt ~valid_sid:true
      (If(guard, mkBlock [mkStmt ~valid_sid:true (Break guard.eloc)],
          mkBlock [], guard.eloc))
  in
  let true_exp = one ~loc:guard.eloc in
  mkLoop ~sattr ~guard:true_exp ~body:(body @ [exit_stmt]) ()

let mkFor ?sattr ~(start: stmt list) ~(guard: exp) ~(next: stmt list)
    ~(body: stmt list) () : stmt list =
  let sattr = [("for", [])] @ Option.value ~default:[] sattr in
  (start @
   (mkLoop ~sattr ~guard ~body:(body @ next)) ())

let mkForIncr ?sattr ~(iter : varinfo) ~(first: exp) ~(stopat: exp) ~(incr: exp)
    ~(body: stmt list) () : stmt list =
  (* See what kind of operator we need *)
  let nextop = match Ast_types.unroll_skel iter.vtype with
    | TPtr _ -> PlusPI
    | _ -> PlusA
  in
  mkFor ?sattr
    ~start:[ mkStmtOneInstr ~valid_sid:true (Set (var iter, first, first.eloc)) ]
    ~guard:(new_exp ~loc:stopat.eloc (BinOp(Lt, evar iter, stopat, Cil_const.intType)))
    ~next:[ mkStmtOneInstr ~valid_sid:true
              (Set
                 (var iter,
                  (new_exp ~loc:incr.eloc
                     (BinOp(nextop, evar iter, incr, iter.vtype))),
                  incr.eloc))]
    ~body ()

let block_from_unspecified_sequence us =
  mkBlock (List.map (fun (x,_,_,_,_) ->x) us)

let rec stripCasts (e: exp) =
  match e.enode with CastE(_, e') -> stripCasts e' | _ -> e

let rec stripTermCasts (t: term) =
  match t.term_node with TCast(_,_, t') -> stripTermCasts t' | _ -> t

let getReturnType t =
  match Ast_types.unroll_skel t with
  | TFun(rt, _, _) -> rt
  | _ -> Kernel.fatal "getReturnType: not a function type"

let setReturnTypeVI (v: varinfo) (t: typ) =
  match Ast_types.unroll v.vtype with
  | { tnode = TFun (_, args, va); tattr } ->
    let t' = Cil_const.mk_tfun ~tattr t args va in
    update_var_type v t'
  | _ -> Kernel.fatal "setReturnType: not a function type"

let setReturnType (f:fundec) (t:typ) =
  setReturnTypeVI f.svar t

let no_op_coerce typ t =
  match typ with
  | Lreal -> Ast_types.is_logic_arithmetic t.term_type
  | Linteger -> Ast_types.is_logic_integral t.term_type
  | Ltype _ when Logic_const.is_boolean_type typ ->
    Ast_types.is_logic_pure_boolean t.term_type
  | Ltype ({lt_name="set"},_) -> true
  | _ -> false

let typeOf_string_literal ?(loc=Fileloc.unknown) s =
  let len = kinteger ~loc (Machine.sizeof_kind()) (String.length s + 1) in
  Cil_const.(mk_tarray ~tattr:[("const",[])] charType (Some len))

let typeOf_wstring_literal ?(loc=Fileloc.unknown) l =
  let len = kinteger ~loc (Machine.sizeof_kind()) (List.length l + 1) in
  Cil_const.mk_tarray ~tattr:[("const",[])] (Machine.wchar_type()) (Some len)

(**** Compute the type of an expression ****)
let rec typeOf (e: exp) : typ =
  match e.enode with
  | Const(CInt64 (_, ik, _)) -> Cil_const.mk_tint ik

  (* Character constants have type int.  ISO/IEC 9899:1999 (E),
   * section 6.4.4.4 [Character constants], paragraph 10, if you
   * don't believe me. *)
  | Const(CChr _) -> Cil_const.intType

  | Const(CReal (_, fk, _)) -> Cil_const.mk_tfloat fk

  | Const(CEnum {eival=v}) -> typeOf v

  (* l-values used as r-values lose their qualifiers (C99 6.3.2.1:2) *)
  | Lval lv -> Ast_types.remove_qualifiers (typeOfLval lv)

  | SizeOf _ | SizeOfE _ -> (Machine.sizeof_type ())
  | AlignOf _ | AlignOfE _ -> (Machine.sizeof_type ())
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf lv -> Cil_const.mk_tptr (typeOfLval lv)
  | StartOf lv ->
    match Ast_types.unroll (typeOfLval lv) with
    | { tnode = TArray (t,_); tattr } -> Cil_const.mk_tptr ~tattr t
    | _ ->  Kernel.fatal ~current:true "typeOf: StartOf on a non-array"

and typeOfInit (i: init) : typ =
  match i with
  | SingleInit e -> typeOf e
  | CompoundInit (t, _) -> t

and typeOfLval = function
  | Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match (Ast_types.unroll (typeOf addr)).tnode with
      | TPtr t -> typeOffset t off
      | _ -> Kernel.fatal ~current:true
               "typeOfLval: Mem on a non-pointer (%a)" !pp_exp_ref addr
    end

and typeOfLhost = function
  | Var x -> x.vtype
  | Mem e -> Ast_types.direct_pointed_type (typeOf e)

and typeOffset basetyp = function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match Ast_types.unroll_node basetyp with
      | TArray (t, _) -> typeOffset t o
      | _ -> Kernel.fatal ~current:true "typeOffset: Index on a non-array"
    end
  | Field (fi, o) ->
    match Ast_types.unroll basetyp with
    | { tnode = TComp _; tattr } ->
      let attrs = Ast_attributes.filter_qualifiers tattr in
      (* if the field is mutable, it can written to even if it is
         part of a const object (but a const subpart of the field
         is still const (except potentially a mutable subsubpart, etc.)
      *)
      let attrs =
        if Ast_attributes.(contains frama_c_mutable fi.fattr) then
          Ast_attributes.drop "const" attrs
        else attrs
      in
      typeOffset (Ast_types.add_attributes attrs fi.ftype) o
    | basetyp ->
      Kernel.fatal ~current:true
        "typeOffset: Field %s on a non-compound type '%a'"
        fi.fname !pp_typ_ref basetyp

(**** Compute the type of a term lval ****)
let rec typeOfTermLval = function
    TVar vi, off ->
    let ty = match vi.lv_origin with
      | Some v -> Ctype v.vtype
      | None -> vi.lv_type
    in
    typeTermOffset ty off
  | TResult ty, off -> typeTermOffset (Ctype ty) off
  | TMem addr, off -> begin
      let rec type_of_pointed = function
        | Ctype typ ->
          begin match Ast_types.unroll_skel typ with
            | TPtr t -> typeTermOffset (Ctype t) off
            | _ ->
              Kernel.fatal ~current:true
                "typeOfTermLval: Mem on a non-pointer"
          end
        | Lboolean | Linteger | Lreal ->
          Kernel.fatal ~current:true "typeOfTermLval: Mem on a logic type"
        | Ltype (s,_) as ty when is_unrollable_ltdef s ->
          type_of_pointed (unroll_ltdef ty)
        | Ltype (s,_) ->
          Kernel.fatal ~current:true
            "typeOfTermLval: Mem on a non-C type (%s)" s.lt_name
        | Lvar s ->
          Kernel.fatal ~current:true
            "typeOfTermLval: Mem on a non-C type ('%s)" s
        | Larrow _ ->
          Kernel.fatal ~current:true
            "typeOfTermLval: Mem on a function type"
      in
      Logic_const.transform_element type_of_pointed addr.term_type
    end

and typeTermOffset basetyp =
  let blendAttributes baseAttrs t =
    let (_, _, contagious) =
      Ast_attributes.partition ~default:(AttrName false) baseAttrs in
    let rec putAttributes = function
      | Ctype typ ->
        Ctype (Ast_types.add_attributes contagious typ)
      | Lboolean | Linteger | Lreal ->
        Kernel.fatal ~current:true
          "typeTermOffset: Attribute on a logic type"
      | Ltype (s,_) as ty when is_unrollable_ltdef s ->
        putAttributes (unroll_ltdef ty)
      | Ltype (s,_) ->
        Kernel.fatal ~current:true
          "typeTermOffset: Attribute on a non-C type (%s)" s.lt_name
      | Lvar s ->
        Kernel.fatal ~current:true
          "typeTermOffset: Attribute on a non-C type ('%s)" s
      | Larrow _ ->
        Kernel.fatal ~current:true
          "typeTermOffset: Attribute on a function type"
    in
    Logic_const.transform_element putAttributes t
  in
  function
  | TNoOffset -> basetyp
  | TIndex (e, o) -> begin
      let rec elt_type basetyp =
        match basetyp with
        | Ctype typ ->
          begin match Ast_types.unroll typ with
            | { tnode = TArray (t, _); tattr } ->
              let elementType = typeTermOffset (Ctype t) o in
              blendAttributes tattr elementType
            | _ ->
              Kernel.fatal ~current:true
                "typeTermOffset: Index on a non-array"
          end
        | Lboolean | Linteger | Lreal ->
          Kernel.fatal ~current:true "typeTermOffset: Index on a logic type"
        | Ltype (s,_) as ty when is_unrollable_ltdef s ->
          elt_type (unroll_ltdef ty)
        | Ltype (s,_) ->
          Kernel.fatal ~current:true "typeTermOffset: Index on a non-C type (%s)" s.lt_name
        | Lvar s -> Kernel.fatal ~current:true "typeTermOffset: Index on a non-C type ('%s)" s
        | Larrow _ -> Kernel.fatal ~current:true "typeTermOffset: Index on a function type"
      in
      Logic_const.set_conversion
        (Logic_const.transform_element elt_type basetyp) e.term_type
    end
  | TModel (m,o) -> typeTermOffset m.mi_field_type o
  | TField (fi, o) ->
    let rec elt_type = function
      | Ctype typ ->
        begin match Ast_types.unroll typ with
          | { tnode = TComp _; tattr } ->
            let fieldType = typeTermOffset (Ctype fi.ftype) o in
            blendAttributes tattr fieldType
          | _ ->  Kernel.fatal ~current:true "typeTermOffset: Field on a non-compound"
        end
      | Lboolean | Linteger | Lreal ->
        Kernel.fatal ~current:true "typeTermOffset: Field on a logic type"
      | Ltype (s,_) as ty when is_unrollable_ltdef s ->
        elt_type (unroll_ltdef ty)
      | Ltype (s,_) ->
        Kernel.fatal ~current:true "typeTermOffset: Field on a non-C type (%s)" s.lt_name
      | Lvar s ->  Kernel.fatal ~current:true "typeTermOffset: Field on a non-C type ('%s)" s
      | Larrow _ -> Kernel.fatal ~current:true "typeTermOffset: Field on a function type"
    in Logic_const.transform_element elt_type basetyp

(**** Check for const attribute ****)

let isGlobalInitConst vi =
  (* Note: the type must be fully const, not a part of it *)
  vi.vglob && vi.vstorage <> Extern && Ast_types.has_qualifier "const" vi.vtype

(**** Check for volatile attribute ****)

let isVolatileLval lv = Ast_types.is_volatile (typeOfLval lv)
let isVolatileTermLval lv =
  Logic_const.plain_or_set Ast_types.is_logic_volatile (typeOfTermLval lv)

(**** MACHINE DEPENDENT PART ****)

exception SizeOfError of string * typ

type sizeof_or_error =
  | Size of int
  | SizeError of string * typ

module SizeOfOrError = Datatype.Make(struct
    include Datatype.Undefined

    let name = "Cil.SizeOfOrError"
    type t  = sizeof_or_error
    let reprs = [
      Size 0 ;
      SizeError ("", Cil_const.voidType)
    ]
    let compare a b =
      match a, b with
      | Size a, Size b -> Int.compare a b
      | SizeError (sa, ta), SizeError(sb, tb) ->
        let s = String.compare sa sb in
        if s = 0 then Cil_datatype.Typ.compare ta tb
        else s
      | Size _, _ -> 1
      | _, Size _ -> -1

    let equal = Datatype.from_compare
    let hash = Hashtbl.hash
    let rehash = Datatype.identity
    let copy = Datatype.identity
    let mem_project = Datatype.never_any_project
  end)

module TypSize =
  State_builder.Hashtbl
    (Typ.Hashtbl)
    (SizeOfOrError)
    (struct
      let name = "Cil.CompInfoSize"
      let dependencies = [] (* depends on Ast.self; see below *)
      let size = 47
    end)

let find_sizeof t f =
  try match TypSize.find t with
    | Size size -> size
    | SizeError (msg, t') -> raise (SizeOfError(msg, t'))
  with Not_found ->
  try
    let t', size = f () in
    TypSize.add t' (Size size) ;
    size
  with SizeOfError(msg, t') as e ->
    TypSize.add t' (SizeError (msg, t')) ;
    raise e

let selfTypSize = TypSize.self

(* Some basic type utilities *)
let rank : ikind -> int =
  function
  (* these are just unique numbers representing the integer
     conversion rank. *)
  | IBool | IChar | ISChar | IUChar -> 1
  | IShort | IUShort -> 2
  | IInt | IUInt -> 3
  | ILong | IULong -> 4
  | ILongLong | IULongLong -> 5
  | IInt128 | IUInt128 -> 6

let unsignedVersionOf (ik:ikind): ikind =
  match ik with
  | ISChar | IChar -> IUChar
  | IShort -> IUShort
  | IInt -> IUInt
  | ILong -> IULong
  | ILongLong -> IULongLong
  | IInt128 -> IUInt128
  | _ -> ik

let frank = function
  | FFloat  | FFloat32 -> 1
  | FDouble | FFloat64 -> 2
  | FLongDouble -> 3


(* Convert 2 integer constants to integers with the same type, in preparation
   for a binary operation.   See ISO C 6.3.1.8p1 *)
let convertInts i1 ik1 i2 ik2 =
  if ik1 = ik2 then (* nothing to do *)
    i1, i2, ik1
  else begin
    let r1 = rank ik1 in
    let r2 = rank ik2 in
    let ik' =
      if (isSigned ik1) = (isSigned ik2) then begin
        (* Both signed or both unsigned. *)
        if r1 > r2 then ik1 else ik2
      end
      else begin
        let signedKind, unsignedKind, signedRank, unsignedRank =
          if isSigned ik1 then ik1, ik2, r1, r2 else ik2, ik1, r2, r1
        in
        (* The rules for signed + unsigned get hairy.
           (unsigned short + long) is converted to signed long,
           but (unsigned int + long) is converted to unsigned long.*)
        if unsignedRank >= signedRank then unsignedKind
        else if (bytesSizeOfInt signedKind) > (bytesSizeOfInt unsignedKind) then
          signedKind
        else
          unsignedVersionOf signedKind
      end
    in
    let i1',_ = truncateInteger64 ik' i1 in
    let i2',_ = truncateInteger64 ik' i2 in
    i1', i2', ik'
  end

(* Local type to compute alignments of struct field. *)
type offsetAcc =
  { oaFirstFree: int;        (* The first free bit *)
    oaLastFieldStart: int;   (* Where the previous field started *)
    oaLastFieldWidth: int;   (* The width of the previous field. Might not
                              * be same as FirstFree - FieldStart because
                              * of internal padding *)
    oaPrevBitPack: (int * ikind * int) option; (* If the previous fields
                                                * were packed bitfields,
                                                * the bit where packing
                                                * has started, the ikind
                                                * of the bitfield and the
                                                * width of the ikind *)
  }



(* Hack to prevent infinite recursion in alignments *)
let ignoreAlignmentAttrs = ref false

let foldAlignasToInt = function
  | None -> None
  | Some v ->
    match !constfoldtoint v with
    | None -> Kernel.fatal "%a should be a constant expression" !pp_exp_ref v
    | Some v when not @@ Z.fits_int v -> Kernel.fatal "_Alignas is too big"
    | Some v when Z.equal Z.zero v -> None
    | Some v -> Some (Z.to_int v)

(* Get the minimum alignment in bytes for a given type *)
let rec bytesAlignOf ~standard_or_gcc t =
  let open Machine in
  let alignOfType () =
    let get_alignof : (module Machine.AlignofInfo) =
      match standard_or_gcc with
      | `Standard -> (module Machine.Alignof)
      | `GCC -> (module Machine.GCCAlignof)
    in
    let module Alignof = (val get_alignof) in
    match t.tnode with
    | TInt (IChar|ISChar|IUChar|IBool) -> 1
    | TInt (IShort|IUShort)->
      Alignof.short ()
    | TInt (IInt|IUInt) ->
      Alignof.int ()
    | TInt (ILong|IULong) ->
      Alignof.long ()
    | TInt (ILongLong|IULongLong) ->
      Alignof.longlong ()
    | TInt (IInt128|IUInt128) ->
      16
    | TEnum ei ->  bytesAlignOf ~standard_or_gcc (Cil_const.mk_tint ei.ekind)
    | TFloat (FFloat|FFloat32) ->
      Alignof.float ()
    | TFloat (FDouble|FFloat64) ->
      Alignof.double ()
    | TFloat FLongDouble ->
      Alignof.longdouble ()
    | TNamed t -> bytesAlignOf ~standard_or_gcc t.ttype
    | TArray (t, _) -> bytesAlignOf ~standard_or_gcc t
    | TPtr _ | TBuiltin_va_list ->
      Alignof.ptr ()
    (* For composite types get the maximum alignment of any field inside *)
    | TComp c ->
      (* On GCC the zero-width fields do not contribute to the alignment. On
       * MSVC only those zero-width that _do_ appear after other
       * bitfields contribute to the alignment. So we drop those that
       * do not occur after other bitfields *)
      (* This is not correct for Diab-C compiler. *)
      let rec dropZeros (afterbitfield: bool) = function
        | f :: rest when f.fbitfield = Some 0 && not afterbitfield ->
          dropZeros afterbitfield rest
        | f :: rest -> f :: dropZeros (f.fbitfield <> None) rest
        | [] -> []
      in
      let fields = dropZeros false (Option.value ~default:[] c.cfields) in
      List.fold_left
        (fun sofar f ->
           (* Bitfields with zero width do not contribute to the alignment in
            * GCC *)
           if not (msvcMode ()) && f.fbitfield = Some 0 then sofar else
             max sofar (bytesAlignOfField ~standard_or_gcc f)) 1 fields
    (* These are some error cases *)
    | TFun _ when Alignof.func () > 0 ->
      Alignof.func ()
    | TFun _ -> raise (SizeOfError ("Undefined alignof on a function.", t))
    | TVoid  when Alignof.void () > 0 ->
      Alignof.void ()
    | TVoid -> raise (SizeOfError ("Undefined alignof(void).", t))
  in
  process_aligned_attribute ~may_reduce:true
    (fun fmt -> !pp_typ_ref fmt t)
    (Ast_types.get_attributes t) alignOfType

(* Alignment of a possibly-packed or aligned struct field.
   From the GCC manual (https://gcc.gnu.org/onlinedocs/gcc/Common-Type-Attributes.html):
   "Specifying the packed attribute for struct and union types is equivalent
    to specifying the packed attribute on each of the structure or union members."
   Note that is not the case for the aligned attribute, which behaves
   differently when put into a struct, or in each of its fields.
*)
and bytesAlignOfField ~standard_or_gcc (fi: fieldinfo) =
  let fieldIsPacked =
    Ast_attributes.(contains "packed" fi.fattr || contains "packed" fi.fcomp.cattr)
  in
  if fieldIsPacked && not @@ Ast_attributes.contains "aligned" fi.fattr then 1
  else
    let aligned = process_aligned_attribute ~may_reduce:fieldIsPacked
        (fun fmt -> Format.fprintf fmt "field %s" fi.fname)
        fi.fattr
        (fun () -> bytesAlignOf ~standard_or_gcc fi.ftype)
    in
    let alignas = match foldAlignasToInt fi.falignas with
      | None -> 0
      | Some v -> v
    in
    max alignas aligned

and intOfAttrparam (a:attrparam) : int option =
  let rec doit a : int =
    match a with
    | AInt(n) ->
      Option.get ~exn:(SizeOfError ("Overflow in integer attribute.", Cil_const.voidType))
        (Z.to_int_opt n)
    | ABinOp(PlusA, a1, a2) -> doit a1 + doit a2
    | ABinOp(MinusA, a1, a2) -> doit a1 - doit a2
    | ABinOp(Mult, a1, a2) -> doit a1 * doit a2
    | ABinOp(Div, a1, a2) -> (doit a1) / (doit a2)
    | ABinOp(Shiftlt, a1, a2) -> (doit a1) lsl (doit a2)
    | ABinOp(Lt, a1, a2) -> if doit a1 < doit a2 then 1 else 0
    | ABinOp(Gt, a1, a2) -> if doit a1 > doit a2 then 1 else 0
    | ABinOp(Le, a1, a2) -> if doit a1 <= doit a2 then 1 else 0
    | ABinOp(Ge, a1, a2) -> if doit a1 >= doit a2 then 1 else 0
    | ABinOp(Eq, a1, a2) -> if doit a1 = doit a2 then 1 else 0
    | ABinOp(Ne, a1, a2) -> if doit a1 <> doit a2 then 1 else 0
    | ABinOp(BAnd, a1, a2) -> doit a1 land doit a2
    | ABinOp(BXor, a1, a2) -> doit a1 lxor doit a2
    | ABinOp(BOr, a1, a2) -> doit a1 lor doit a2
    | AQuestion(c,a1,a2) -> if doit c <> 0 then doit a1 else doit a2
    | ASizeOf(t) ->
      let bs = bitsSizeOf t in
      bs / 8
    | AAlignOf(t, i) -> bytesAlignOf ~standard_or_gcc:i t
    | _ -> raise (SizeOfError ("Cannot convert an attribute to int.", Cil_const.voidType))
  in
  (* Use ignoreAlignmentAttrs here to prevent stack overflow if a buggy
     program does something like
     struct s {...} __attribute__((aligned(sizeof(struct s))))
     This is too conservative, but it's often enough.
  *)
  assert (not !ignoreAlignmentAttrs);
  ignoreAlignmentAttrs := true;
  try
    let n = doit a in
    ignoreAlignmentAttrs := false;
    Some n
  with Z.Overflow | SizeOfError _ -> (* Can't compile *)
    ignoreAlignmentAttrs := false;
    None
and process_aligned_attribute (pp:Format.formatter->unit) ~may_reduce attrs default_align =
  (* [may_reduce] indicates if we can reduce the alignment
     (e.g. a "packed" attribute is present).
     According to GCC's doc
     (https://gcc.gnu.org/onlinedocs/gcc/Common-Type-Attributes.html):
     "When used on a struct, or struct member, the aligned attribute can only
     increase the alignment; in order to decrease it, the packed attribute must
     be specified as well. When used as part of a typedef, the aligned attribute
     can both increase and decrease alignment, and specifying the packed
     attribute generates a warning." *)
  match Ast_attributes.filter "aligned" attrs with
  | [] ->
    (* no __aligned__ attribute, so get the default alignment *)
    default_align ()
  | _ when !ignoreAlignmentAttrs ->
    Kernel.warning ~current:true "ignoring recursive align attributes on %t"
      pp;
    default_align ()
  | ((_, [a]) as at)::rest -> begin
      if rest <> [] then
        Kernel.warning ~current:true "ignoring duplicate align attributes on %t"
          pp;
      match intOfAttrparam a with
      | Some n -> if may_reduce then n else max n (default_align ())
      | None ->
        Kernel.warning ~current:true "alignment attribute \"%a\" not understood on %t"
          !pp_attribute_ref at pp;
        default_align ()
    end
  | (_, [])::rest ->
    (* aligned with no arg means a power of two at least as large as
       any alignment on the system.*)
    if rest <> [] then
      Kernel.warning ~current:true "ignoring duplicate align attributes on %t"
        pp;
    Machine.Alignof.aligned ()
  | at::_ ->
    Kernel.warning ~current:true "alignment attribute \"%a\" not understood on %t"
      !pp_attribute_ref at pp;
    default_align ()

(* Computation of the offset of the field [fi], given the information [sofar]
   computed for the previous fields. [last] indicates that we are considering
   the last field of the struct. Set to [false] by default for unions. *)
and offsetOfFieldAcc ~last ~(fi: fieldinfo) ~(sofar: offsetAcc) : offsetAcc =
  if Machine.msvcMode () then offsetOfFieldAcc_MSVC last fi sofar
  else offsetOfFieldAcc_GCC last fi sofar

(* GCC version *)
(* Does not use the sofar.oaPrevBitPack *)
and offsetOfFieldAcc_GCC last (fi: fieldinfo) (sofar: offsetAcc) : offsetAcc =
  (* field type *)
  let ftype = Ast_types.unroll fi.ftype in
  let ftypeAlign = 8 * bytesAlignOfField ~standard_or_gcc:`Standard fi in
  let ftypeBits = (if last then bitsSizeOfEmptyArray else bitsSizeOf) ftype in
  match ftype, fi.fbitfield with
  (* A width of 0 means that we must end the current packing. It seems that
   * GCC pads only up to the alignment boundary for the type of this field.
   * *)
  | _, Some 0 ->
    let firstFree      = addTrailing sofar.oaFirstFree ftypeAlign in
    { oaFirstFree      = firstFree;
      oaLastFieldStart = firstFree;
      oaLastFieldWidth = 0;
      oaPrevBitPack    = None }

  (* A bitfield cannot span more alignment boundaries of its type than the
   * type itself *)
  | _, Some wdthis
    when (sofar.oaFirstFree + wdthis + ftypeAlign - 1) / ftypeAlign
         - sofar.oaFirstFree / ftypeAlign > ftypeBits / ftypeAlign ->
    let start = addTrailing sofar.oaFirstFree ftypeAlign in
    { oaFirstFree      = start + wdthis;
      oaLastFieldStart = start;
      oaLastFieldWidth = wdthis;
      oaPrevBitPack    = None }

  (* Try a simple method. Just put the field down *)
  | _, Some wdthis ->
    { oaFirstFree      = sofar.oaFirstFree + wdthis;
      oaLastFieldStart = sofar.oaFirstFree;
      oaLastFieldWidth = wdthis;
      oaPrevBitPack    = None
    }

  (* Non-bitfield *)
  | _, None ->
    (* Align this field *)
    let newStart = addTrailing sofar.oaFirstFree ftypeAlign  in
    { oaFirstFree = newStart + ftypeBits;
      oaLastFieldStart = newStart;
      oaLastFieldWidth = ftypeBits;
      oaPrevBitPack = None;
    }

(* MSVC version *)
and offsetOfFieldAcc_MSVC last (fi: fieldinfo)
    (sofar: offsetAcc) : offsetAcc =
  (* field type *)
  let ftype = Ast_types.unroll fi.ftype in
  let ftypeAlign = 8 * bytesAlignOfField ~standard_or_gcc:`Standard fi in
  let ftypeBits = (if last then bitsSizeOfEmptyArray else bitsSizeOf) ftype in
  match ftype.tnode, fi.fbitfield, sofar.oaPrevBitPack with
  (* Ignore zero-width bitfields that come after non-bitfields *)
  | TInt _ikthis, Some 0, None ->
    let firstFree      = sofar.oaFirstFree in
    { oaFirstFree      = firstFree;
      oaLastFieldStart = firstFree;
      oaLastFieldWidth = 0;
      oaPrevBitPack    = None }

  (* If we are in a bitpack and we see a bitfield for a type with the
   * different width than the pack, then we finish the pack and retry *)
  | _, Some _, Some (packstart, _, wdpack) when wdpack != ftypeBits ->
    let firstFree =
      if sofar.oaFirstFree = packstart then packstart else
        packstart + wdpack
    in
    offsetOfFieldAcc_MSVC last fi
      { oaFirstFree      = addTrailing firstFree ftypeAlign;
        oaLastFieldStart = sofar.oaLastFieldStart;
        oaLastFieldWidth = sofar.oaLastFieldWidth;
        oaPrevBitPack    = None }

  (* A width of 0 means that we must end the current packing. *)
  | TInt ikthis, Some 0, Some (packstart, _, wdpack) ->
    let firstFree =
      if sofar.oaFirstFree = packstart then packstart else
        packstart + wdpack
    in
    let firstFree      = addTrailing firstFree ftypeAlign in
    { oaFirstFree      = firstFree;
      oaLastFieldStart = firstFree;
      oaLastFieldWidth = 0;
      oaPrevBitPack    = Some (firstFree, ikthis, ftypeBits) }

  (* Check for a bitfield that fits in the current pack after some other
   * bitfields *)
  | TInt _ikthis, Some wdthis, Some (packstart, _ikprev, wdpack)
    when  packstart + wdpack >= sofar.oaFirstFree + wdthis ->
    { oaFirstFree = sofar.oaFirstFree + wdthis;
      oaLastFieldStart = sofar.oaFirstFree;
      oaLastFieldWidth = wdthis;
      oaPrevBitPack = sofar.oaPrevBitPack
    }


  | _, _, Some (packstart, _, wdpack) -> (* Finish up the bitfield pack and
                                          * restart. *)
    let firstFree =
      if sofar.oaFirstFree = packstart then packstart else
        packstart + wdpack
    in
    offsetOfFieldAcc_MSVC last fi
      { oaFirstFree      = addTrailing firstFree ftypeAlign;
        oaLastFieldStart = sofar.oaLastFieldStart;
        oaLastFieldWidth = sofar.oaLastFieldWidth;
        oaPrevBitPack    = None }

  (* No active bitfield pack. But we are seeing a bitfield. *)
  | TInt ikthis, Some wdthis, None ->
    let firstFree     = addTrailing sofar.oaFirstFree ftypeAlign in
    { oaFirstFree     = firstFree + wdthis;
      oaLastFieldStart = firstFree;
      oaLastFieldWidth = wdthis;
      oaPrevBitPack = Some (firstFree, ikthis, ftypeBits); }

  (* No active bitfield pack. Non-bitfield *)
  | _, None, None ->
    (* Align this field *)
    let firstFree = addTrailing sofar.oaFirstFree ftypeAlign  in
    { oaFirstFree = firstFree + ftypeBits;
      oaLastFieldStart = firstFree;
      oaLastFieldWidth = ftypeBits;
      oaPrevBitPack = None;
    }

  | _, Some _, None -> Kernel.fatal ~current:true "offsetAcc"

(** This is a special version of [bitsSizeOf] that accepts empty arrays.
    Currently, we only use it for flexible array members *)
and bitsSizeOfEmptyArray typ =
  match Ast_types.unroll_node typ with
  | TArray (_, None) -> 0
  | TArray (_, Some e) -> begin
      match constFoldToInt e with
      | Some i when Z.is_zero i ->
        (* Used for GCC extension of non-C99 flexible array members.
           Note that Cabs2cil no longer rewrites top-level zero-sized arrays,
           so this can also happen in such cases. *)
        0
      | _ -> bitsSizeOf typ
    end
  | _ -> bitsSizeOf typ

(* The size of a type, in bits. If struct or array then trailing padding is
 * added *)
and bitsSizeOf t =
  match t.tnode with
  | TInt ik            -> 8 * (bytesSizeOfInt ik)
  | TFloat FDouble     -> 8 * Machine.Sizeof.double ()
  | TFloat FLongDouble -> 8 * Machine.Sizeof.longdouble ()
  | TFloat _           -> 8 * Machine.Sizeof.float ()
  | TEnum ei           -> bitsSizeOf (Cil_const.mk_tint ei.ekind)
  | TPtr _             -> 8 * Machine.Sizeof.ptr ()
  | TBuiltin_va_list   -> 8 * Machine.Sizeof.ptr ()
  | TNamed t           -> bitsSizeOf t.ttype
  | TComp ({cfields=None} as comp) ->
    raise
      (SizeOfError
         (Format.sprintf "abstract type '%s'" (compFullName comp), t))
  | TComp {cfields=Some[]} when Machine.acceptEmptyCompinfo() ->
    find_sizeof t (fun () -> t,0)
  | TComp ({cfields=Some[]} as comp) ->
    find_sizeof t
      (fun () ->
         (* sizeof() empty structs/arrays is only allowed on GCC/MSVC *)
         raise
           (SizeOfError
              (Format.sprintf "empty struct '%s'" (compFullName comp), t)))
  | TComp comp when comp.cstruct -> (* Struct *)
    find_sizeof t
      (fun () ->
         (* Go and get the last offset *)
         let startAcc =
           { oaFirstFree = 0;
             oaLastFieldStart = 0;
             oaLastFieldWidth = 0;
             oaPrevBitPack = None;
           } in
         let lastoff =
           fold_struct_fields
             (fun ~last acc fi -> offsetOfFieldAcc ~last ~fi ~sofar:acc)
             startAcc (Option.get comp.cfields) (* Note: we treat None above *)
         in
         if Machine.msvcMode () && lastoff.oaFirstFree = 0
         then
           (* On MSVC if we have just a zero-width bitfields then the length
            * is 32 and is not padded  *)
           t, 32
         else
           (* Add final padding bits according to the struct alignment, but not
              according to possible alignment attributes of type [t]. *)
           let struct_typ = Cil_const.mk_tcomp comp in
           let alignof = bytesAlignOf ~standard_or_gcc:`Standard struct_typ in
           t, addTrailing lastoff.oaFirstFree (8 * alignof))

  | TComp comp -> (* Union *)
    find_sizeof t
      (fun () ->
         (* Get the maximum of all fields *)
         let startAcc =
           { oaFirstFree = 0;
             oaLastFieldStart = 0;
             oaLastFieldWidth = 0;
             oaPrevBitPack = None;
           } in
         let fold acc fi =
           let lastoff = offsetOfFieldAcc ~last:false ~fi ~sofar:startAcc in
           if lastoff.oaFirstFree > acc
           then lastoff.oaFirstFree
           else acc
         in
         (* Note: we treat None above *)
         let max = List.fold_left fold 0 (Option.get comp.cfields) in
         (* Add trailing by simulating adding an extra field *)
         let union_typ = Cil_const.mk_tcomp comp in
         let alignof = bytesAlignOf ~standard_or_gcc:`Standard union_typ in
         t, addTrailing max (8 * alignof))

  | TArray(bt, Some len) ->
    find_sizeof t
      (fun () ->
         begin
           let v = constFold true len in
           let norm_typ = Cil_const.mk_tarray ~tattr:t.tattr bt (Some v) in
           match v with
             { enode = Const(CInt64(l,_,_)) } ->
             let sz = Z.mul (Z.of_int (bitsSizeOf bt)) l in
             let sz' =
               match Z.to_int_opt sz with
               | Some i -> i
               | None ->
                 raise
                   (SizeOfError
                      ("Array is so long that its size can't be "
                       ^"represented with an OCaml int.", norm_typ))

             in
             (norm_typ, sz') (*WAS: addTrailing sz' (8 * bytesAlignOf t)*)
           | _ ->
             raise (SizeOfError ("Array with non-constant length.", norm_typ))
         end)
  | TVoid when Machine.Sizeof.void () > 0 -> 8 * Machine.Sizeof.void ()
  | TVoid -> raise (SizeOfError ("Undefined sizeof(void).", t))
  | TFun _ when Machine.Sizeof.func () > 0 -> 8 * Machine.Sizeof.func ()
  | TFun _  -> raise (SizeOfError ("Undefined sizeof on a function.", t))

  | TArray (_, None) ->
    find_sizeof t
      (fun () ->
         raise (SizeOfError ("Size of array without number of elements.", t)))

(* Iterator on the fields of a structure, with additional information about
   having reached the last field (for flexible member arrays) *)
and fold_struct_fields f acc l = match l with
  | [] -> acc
  | [fi_last] -> f ~last:true acc fi_last
  | fi :: (_ :: _ as q) -> fold_struct_fields f (f ~last:false acc fi) q

and addTrailing nrbits roundto =
  (nrbits + roundto - 1) land (lnot (roundto - 1))

and bytesSizeOf t = (bitsSizeOf t) lsr 3

and sizeOf ~loc t =
  try
    integer ~loc (bytesSizeOf t)
  with SizeOfError _ -> new_exp ~loc (SizeOf(t))

and fieldBitsOffset (f : fieldinfo) : int * int =
  if  not f.fcomp.cstruct (* union *) then
    (* All union fields start at offset 0 *)
    0, bitsSizeOf f.ftype
  else begin
    if f.foffset_in_bits = None then begin
      let aux ~last acc fi =
        let acc' = offsetOfFieldAcc ~last ~fi ~sofar:acc in
        fi.fsize_in_bits <- Some acc'.oaLastFieldWidth;
        fi.foffset_in_bits <- Some acc'.oaLastFieldStart;
        acc'
      in
      ignore (
        fold_struct_fields aux
          { oaFirstFree      = 0;
            oaLastFieldStart = 0;
            oaLastFieldWidth = 0;
            oaPrevBitPack    = None }
          (Option.value ~default:[] f.fcomp.cfields)
      );
    end;
    Option.get f.foffset_in_bits, Option.get f.fsize_in_bits
  end

and bitsOffset (baset: typ) (off: offset) : int * int =
  let rec loopOff (baset: typ) (width: int) (start: int) = function
    | NoOffset -> start, width
    | Index(e, off) -> begin
        let ei =
          match constFoldToInt e with
          | Some i -> Z.to_int i
          | None -> raise (SizeOfError ("Index is not constant", baset))
        in
        let bt = Ast_types.direct_element_type baset in
        let bitsbt = bitsSizeOf bt in
        loopOff bt bitsbt (start + ei * bitsbt) off
      end
    | Field(f, off) ->
      if check_invariants then
        (match Ast_types.unroll_skel baset with
         | TComp ci -> assert (ci == f.fcomp)
         | _ -> assert false);
      let offsbits, size = fieldBitsOffset f in
      loopOff f.ftype size (start + offsbits) off
  in
  loopOff baset (bitsSizeOf baset) 0 off

(** Do constant folding on an expression. If the first argument is true then
    will also compute compiler-dependent expressions such as sizeof.
    See also {!Cil.constFoldVisitor}, which will run constFold on all
    expressions in a given AST node.*)
and constFold (machdep: bool) (e: exp) : exp =
  let dkey = Kernel.dkey_constfold in
  Kernel.debug ~dkey "ConstFold %a@." !pp_exp_ref e;
  let loc = e.eloc in
  match e.enode with
  | BinOp (bop, e1, e2, tres) -> constFoldBinOp ~loc machdep bop e1 e2 tres
  | UnOp (unop, e1, tres) when Ast_types.is_integral tres -> begin
      let tk =
        match Ast_types.unroll_skel tres with
        | TInt ik  -> ik
        | TEnum ei -> ei.ekind
        | _ -> assert false (* tres is an integral type *)
      in
      let e1c = constFold machdep e1 in
      match e1c.enode with
        Const(CInt64(i,_ik,repr)) -> begin
          match unop with
          | Neg ->
            let repr = Option.map (fun s -> "-" ^ s) repr in
            kinteger64 ~loc ?repr ~kind:tk (Z.neg i)
          | BNot -> kinteger64 ~loc ~kind:tk (Z.lognot i)
          | LNot ->
            if Z.is_zero i then one ~loc
            else zero ~loc
        end
      | _ -> if e1 == e1c then e else new_exp ~loc (UnOp(unop, e1c, tres))
    end
  | UnOp (unop, e1, tres) when Ast_types.is_arithmetic tres -> begin
      let tk =
        match Ast_types.unroll_skel tres with
        | TFloat fk -> fk
        | _ -> assert false (*tres is arithmetic but not integral, i.e. Float *)
      in
      let e1c = constFold machdep e1 in
      match e1c.enode with
      | Const (CReal(f,_,_)) -> begin
          match unop with
          | Neg -> kfloat ~loc tk (-. f)
          | _ -> if e1 == e1c then e else new_exp ~loc (UnOp(unop,e1c,tres))
        end
      | _ -> if e1 == e1c then e else new_exp ~loc (UnOp(unop,e1c,tres))
    end
  | UnOp _ -> e
  (* Characters are integers *)
  | Const(CChr c) -> new_exp ~loc (Const(charConstToIntConstant c))
  | Const(CEnum {eival = v}) -> constFold machdep v
  | Const (CReal _ | CInt64 _) -> e (* a constant *)
  | SizeOf t when machdep ->
    begin
      try kinteger ~loc (Machine.sizeof_kind ()) (bytesSizeOf t)
      with SizeOfError _ -> e
    end
  | SizeOfE e when machdep ->
    constFold machdep (new_exp ~loc:e.eloc (SizeOf (typeOf e)))
  | AlignOf (t, standard_or_gcc) when machdep ->
    begin
      try kinteger ~loc (Machine.sizeof_kind ()) (bytesAlignOf ~standard_or_gcc t)
      with SizeOfError _ -> e
    end
  | AlignOfE (e, i) when machdep ->
    constFold machdep (new_exp ~loc:e.eloc (AlignOf (typeOf e, i)))
  | AlignOfE _ | AlignOf _ | SizeOfE _ | SizeOf _ ->
    e (* Depends on machdep. Do not evaluate in this case*)

  | CastE (t, e) -> begin
      Kernel.debug ~dkey "ConstFold CAST to %a@." !pp_typ_ref t ;
      let e = constFold machdep e in
      let t' = Ast_types.unroll t in
      match e.enode, t'.tnode with
      | Const (CInt64(i,_k,_)), (TInt nk | TEnum {ekind = nk})
        when Ast_attributes.(drop_list fc_internal_attributes t'.tattr) = [] ->
        begin
          (* If the cast has attributes, leave it alone. *)
          Kernel.debug ~dkey "ConstFold to %a : %a@."
            !pp_ikind_ref nk Z.pretty i;
          (* Downcasts might truncate silently *)
          kinteger64 ~loc ~kind:nk i
        end
      | Const (CReal(f,_,_)), (TInt ik | TEnum {ekind = ik}) when t.tattr = [] ->
        (* See above *)
        let truncated i = truncateInteger64 ik i |> snd in
        begin match Floating_point.truncate_to_integer f with
          | Underflow | Overflow -> new_exp ~loc (CastE (t, e))
          | Integer i when truncated i -> new_exp ~loc (CastE (t, e))
          | Integer i -> kinteger64 ~loc ~kind:ik i
        end
      | Const (CReal(f,_,_)), TFloat FFloat when t.tattr = [] ->
        let f = Floating_point.round_to_single_precision f in
        new_exp ~loc (Const (CReal (f,FFloat,None)))
      | Const (CReal(f,_,_)), TFloat FDouble when t.tattr = [] ->
        new_exp ~loc (Const (CReal (f,FDouble,None)))
      (* We don't treat cast to long double, as we don't really know
         how to handle this type anyway. *)
      | Const (CInt64(i,_,_)), TFloat FFloat when t.tattr = [] ->
        let f = Z.to_float i in
        let f = Floating_point.round_to_single_precision f in
        new_exp ~loc (Const (CReal (f,FFloat,None)))
      | Const (CInt64(i,_,_)), TFloat FDouble when t.tattr = [] ->
        let f = Z.to_float i in
        new_exp ~loc (Const (CReal (f,FDouble,None)))
      | _, _ -> new_exp ~loc (CastE (t, e))
    end
  | Lval lv -> new_exp ~loc (Lval (constFoldLval machdep lv))
  | AddrOf lv -> new_exp ~loc (AddrOf (constFoldLval machdep lv))
  | StartOf lv -> new_exp ~loc (StartOf (constFoldLval machdep lv))

and constFoldLval machdep (host,offset) =
  let newhost =
    match host with
    | Mem e -> Mem (constFold machdep e)
    | Var _ -> host
  in
  (newhost, constFoldOffset machdep offset)

and constFoldOffset machdep = function
  | NoOffset -> NoOffset
  | Field (fi,offset) -> Field (fi, constFoldOffset machdep offset)
  | Index (exp,offset) ->
    Index (constFold machdep exp, constFoldOffset machdep offset)

and constFoldBinOp ~loc (machdep: bool) bop e1 e2 tres =
  let e1' = constFold machdep e1 in
  let e2' = constFold machdep e2 in
  if Ast_types.is_integral tres then begin
    let newe =
      let rec mkInt e =
        let loc = e.eloc in
        match e.enode with
        | Const(CChr c) -> new_exp ~loc (Const(charConstToIntConstant c))
        | Const(CEnum {eival = v}) -> mkInt v
        | CastE(typ, e') -> begin
            match Ast_types.unroll typ with
            | { tnode = TInt ik } as t -> begin
                let e = mkInt e' in
                match e.enode with
                | Const(CInt64(i, _, _)) -> kinteger64 ~loc ~kind:ik i
                | _ -> {e with enode = CastE(t, e)}
              end
            | _ -> e
          end
        | _ -> e
      in
      let tk =
        match Ast_types.unroll_skel tres with
        | TInt ik  -> ik
        | TEnum ei -> ei.ekind
        | _ -> Kernel.fatal ~current:true "constFoldBinOp"
      in
      (* See if the result is unsigned *)
      let isunsigned typ = not (isSigned typ) in
      let shiftInBounds i2 =
        (* We only try to fold shifts if the second arg is positive and
           less than the size of the type of the first argument.
           Otherwise, the semantics are processor-dependent, so let the
           compiler sort it out. *)
        if machdep then
          try
            (Z.geq i2 Z.zero)
            && Z.lt i2 (Z.of_int (bitsSizeOf (typeOf e1')))
          with SizeOfError _ -> false
        else false
      in
      (* Assume that the necessary promotions have been done *)
      let e1'' = mkInt e1' in
      let e2'' = mkInt e2' in
      match bop, e1''.enode, e2''.enode with
      | PlusA, Const(CInt64(z,_,_)), _
        when Z.is_zero z -> e2''
      | (PlusA | MinusA), _, Const(CInt64(z,_,_))
        when Z.is_zero z -> e1''
      | PlusPI, _, Const(CInt64(z,_,_))
        when Z.is_zero z -> e1''
      | MinusPI, _, Const(CInt64(z,_,_))
        when Z.is_zero z -> e1''
      | PlusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
        kinteger64 ~loc ~kind:tk (Z.add i1 i2)
      | MinusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_))
        when ik1 = ik2 ->
        kinteger64 ~loc ~kind:tk (Z.sub i1 i2)
      | Mult, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
        kinteger64 ~loc ~kind:tk (Z.mul i1 i2)
      | Mult, Const(CInt64(z,_,_)), _
        when Z.is_zero z -> e1''
      | Mult, Const(CInt64(one,_,_)), _
        when Z.is_one one -> e2''
      | Mult, _,    Const(CInt64(z,_,_))
        when Z.is_zero z -> e2''
      | Mult, _, Const(CInt64(one,_,_))
        when Z.is_one one -> e1''
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
        begin
          try kinteger64 ~loc ~kind:tk (Z.div i1 i2)
          with Division_by_zero -> new_exp ~loc (BinOp(bop, e1', e2', tres))
        end
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_))
        when bytesSizeOfInt ik1 = bytesSizeOfInt ik2 -> begin
          try kinteger64 ~loc ~kind:tk (Z.div i1 i2)
          with Division_by_zero -> new_exp ~loc (BinOp(bop, e1', e2', tres))
        end
      | Div, _, Const(CInt64(one,_,_))
        when Z.is_one one -> e1''
      | Mod, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
        begin
          try kinteger64 ~loc ~kind:tk (Z.rem i1 i2)
          with Division_by_zero -> new_exp ~loc (BinOp(bop, e1', e2', tres))
        end
      | BAnd, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
        kinteger64 ~loc ~kind:tk (Z.logand i1 i2)
      | BAnd, Const(CInt64(z,_,_)), _
        when Z.is_zero z -> e1''
      | BAnd, _, Const(CInt64(z,_,_))
        when Z.is_zero z -> e2''
      | BOr, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
        kinteger64 ~loc ~kind:tk (Z.logor i1 i2)
      | BOr, _, _ when isZero e1' -> e2'
      | BOr, _, _ when isZero e2' -> e1'
      | BXor, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
        kinteger64 ~loc ~kind:tk (Z.logxor i1 i2)
      | Shiftlt, Const(CInt64(i1,_ik1,_)),Const(CInt64(i2,_,_))
        when shiftInBounds i2 ->
        kinteger64 ~loc ~kind:tk (Z.shift_left_z i1 i2)
      | Shiftlt, Const(CInt64(z,_,_)), _
        when Z.is_zero z -> e1''
      | Shiftlt, _, Const(CInt64(z,_,_))
        when Z.is_zero z -> e1''
      | Shiftrt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,_,_))
        when shiftInBounds i2 ->
        if isunsigned ik1 then
          kinteger64 ~loc ~kind:tk
            (Z.shift_right_logical i1 i2)
        else
          kinteger64 ~loc ~kind:tk (Z.shift_right_z i1 i2)
      | Shiftrt, Const(CInt64(z,_,_)), _
        when Z.is_zero z -> e1''
      | Shiftrt, _, Const(CInt64(z,_,_))
        when Z.is_zero z -> e1''
      | Eq, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
        let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
        if Z.equal i1' i2' then one ~loc else zero ~loc
      | Ne, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
        let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
        if Z.equal i1' i2' then zero ~loc else one ~loc
      | Le, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
        let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
        if Z.leq i1' i2' then one ~loc else zero ~loc
      | Ge, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
        let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
        if Z.geq i1' i2' then one ~loc else zero ~loc
      | Lt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
        let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
        if Z.lt i1' i2' then one ~loc else zero ~loc
      | Gt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
        let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
        if Z.gt i1' i2' then one ~loc else zero ~loc
      | Eq, Const(CReal(f1,fk1,_)),Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
        if f1 = f2 then one ~loc else zero ~loc
      | Ne, Const(CReal(f1,fk1,_)),Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
        if f1 = f2 then zero ~loc else one ~loc
      | Le, Const(CReal(f1,fk1,_)),Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
        if f1 <= f2 then one ~loc else zero ~loc
      | Ge, Const(CReal(f1,fk1,_)),Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
        if f1 >= f2 then one ~loc else zero ~loc
      | Lt, Const(CReal(f1,fk1,_)),Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
        if f1 < f2 then one ~loc else zero ~loc
      | Gt, Const(CReal(f1,fk1,_)),Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
        if f1 > f2 then one ~loc else zero ~loc

      (* We rely on the fact that LAnd/LOr appear in global initializers
         and should not have side effects. *)
      | LAnd, _, _ when isZero e1' || isZero e2' -> zero ~loc
      | LAnd, _, _ when isInteger e1' <> None -> e2'  (* e1' is TRUE *)
      | LAnd, _, _ when isInteger e2' <> None -> e1'  (* e2' is TRUE *)
      | LOr, _, _ when isZero e1' -> e2'
      | LOr, _, _ when isZero e2' -> e1'
      | LOr, _, _ when isInteger e1' <> None || isInteger e2' <> None ->
        (* One of e1' or e2' is a nonzero constant *)
        one ~loc
      | _ -> new_exp ~loc (BinOp(bop, e1', e2', tres))
    in
    Kernel.debug ~dkey:Kernel.dkey_constfold "Folded %a to %a@."
      !pp_exp_ref (new_exp ~loc (BinOp(bop, e1', e2', tres)))
      !pp_exp_ref newe;
    newe
  end
  else if Ast_types.(is_arithmetic tres && not (is_long_double tres)) then begin
    let tk =
      match Ast_types.unroll_skel tres with
      | TFloat fk -> fk
      | _ -> Kernel.fatal "constFoldBinOp: not a floating type"
    in
    match bop, e1'.enode, e2'.enode with
    | PlusA, Const(CReal(f1,fk1,_)), Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
      kfloat ~loc tk (f1 +. f2)
    | MinusA, Const(CReal(f1,fk1,_)), Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
      kfloat ~loc tk (f1 -. f2)
    | Mult, Const(CReal(f1,fk1,_)), Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
      kfloat ~loc tk (f1 *. f2)
    | Div, Const(CReal(f1,fk1,_)), Const(CReal(f2,fk2,_)) when fk1 = fk2 ->
      (*might be infinity or NaN, but that's still a float*)
      kfloat ~loc tk (f1 /. f2)
    | _ -> new_exp ~loc (BinOp(bop,e1',e2',tres))
  end else
    new_exp ~loc (BinOp(bop, e1', e2', tres))

and constFoldToInt ?(machdep=true) e =
  match (constFold machdep e).enode with
  | Const(CInt64(c,_,_)) -> Some c
  | CastE (typ, e) when machdep && Ast_types.is_ptr typ -> begin
      (* Those casts are left left by constFold *)
      match constFoldToInt ~machdep e with
      | None -> None
      | Some i as r -> if fitsInInt (Machine.uintptr_kind ()) i then r else None
    end
  | _ -> None

let bitsSizeOfBitfield typlv =
  match Ast_types.unroll typlv with
  | { tnode = TInt _; tattr } | { tnode = TEnum _; tattr } as t ->
    (match Ast_attributes.(find_params bitfield_attribute_name tattr) with
     | [AInt i] -> Z.to_int i
     | _ -> bitsSizeOf t)
  | t -> bitsSizeOf t

let () = constfoldtoint := constFoldToInt ~machdep:true

let bytesAlignOf t =
  bytesAlignOf ~standard_or_gcc:`Standard t

let bytesAlignOfField f =
  bytesAlignOfField ~standard_or_gcc:`Standard f

let bytesAlignOfVarinfo vi =
  match foldAlignasToInt vi.valignas with
  | None -> bytesAlignOf vi.vtype
  | Some align -> align

let intTypeIncluded kind1 kind2 =
  let bitsize1 = bitsSizeOfInt kind1 in
  let bitsize2 = bitsSizeOfInt kind2 in
  match isSigned kind1, isSigned kind2 with
  | true, true
  | false, false -> bitsize1 <= bitsize2
  | true, false -> false
  | false, true -> bitsize1 < bitsize2


(* CEA: moved from cabs2cil.ml. See cil.mli for infos *)
(* Weimer
 * multi-character character constants
 * In MSVC, this code works:
 *
 * long l1 = 'abcd';  // note single quotes
 * char * s = "dcba";
 * long * lptr = ( long * )s;
 * long l2 = *lptr;
 * assert(l1 == l2);
 *
 * We need to change a multi-character character literal into the
 * appropriate integer constant. However, the plot sickens: we
 * must also be able to handle things like 'ab\nd' (value = * "d\nba")
 * and 'abc' (vale = *"cba").
 *
 * First we convert 'AB\nD' into the list [ 65 ; 66 ; 10 ; 68 ], then we
 * multiply and add to get the desired value.
*)

(* Given a character constant (like 'a' or 'abc') as a list of 64-bit
 * values, turn it into a CIL constant.  Multi-character constants are
 * treated as multi-digit numbers with radix given by the bit width of
 * the specified type (either char or wchar_t). *)
let reduce_multichar typ : int64 list -> int64 =
  let radix = bitsSizeOf typ in
  List.fold_left
    (fun acc -> Int64.add (Int64.shift_left acc radix))
    Int64.zero

let interpret_character_constant char_list =
  let value = reduce_multichar Cil_const.charType char_list in
  if value < (Int64.of_int 256) then
    (* ISO C 6.4.4.4.10: single-character constants have type int *)
    (CChr(Char.chr (Int64.to_int value))), Cil_const.intType
  else begin
    let orig_rep = None (* Some("'" ^ (String.escaped str) ^ "'") *) in
    if value <= (Int64.of_int32 Int32.max_int) then
      (CInt64(Z.of_int64 value,IULong,orig_rep)), Cil_const.ulongType
    else
      (CInt64(Z.of_int64 value,IULongLong,orig_rep)), Cil_const.ulongLongType
  end

let invalidStmt = mkStmt (Instr (Skip Fileloc.unknown))

let range_loc loc1 loc2 = fst loc1, snd loc2

(* JS 2012/11/16: probably broken since it may call constFold on some exp: this
   operation modifies this expression in-place! *)
let compareConstant c1 c2 =
  match c1, c2 with
  | CEnum e1, CEnum e2 ->
    e1.einame = e2.einame && e1.eihost.ename = e2.eihost.ename &&
    (match constFoldToInt e1.eival, constFoldToInt e2.eival with
     | Some i1, Some i2 -> Z.equal i1 i2
     | _ -> false)
  | CInt64 (i1,k1,_), CInt64(i2,k2,_) ->
    k1 = k2 && Z.equal i1 i2
  | CChr c1, CChr c2 -> c1 = c2
  | CReal(f1,k1,_), CReal(f2,k2,_) -> k1 = k2 && f1 = f2
  | (CEnum _ | CInt64 _ | CChr _ | CReal _), _ -> false

(* Iterate over all globals, including the global initializer *)
let iterGlobals (fl: file) (doone: global -> unit) : unit =
  let doone' g =
    Current_loc.with_loc (Global.loc g) doone g
  in
  List.iter doone' fl.globals;
  match fl.globinit with
  | None -> ()
  | Some g -> doone' (GFun(g, Fileloc.unknown))

(* Fold over all globals, including the global initializer *)
let foldGlobals (fl: file) (doone: 'a -> global -> 'a) (acc: 'a) : 'a =
  let doone' acc g =
    Current_loc.with_loc (Global.loc g) (doone acc) g
  in
  let acc' = List.fold_left doone' acc fl.globals in
  match fl.globinit with
  | None -> acc'
  | Some g -> doone' acc' (GFun(g, Fileloc.unknown))

let is_skip = function Instr (Skip _) -> true | _ -> false

(** [b_assumes] must be always empty for behavior named
    [Cil.default_behavior_name] *)
let mk_behavior ?(name=default_behavior_name) ?(assumes=[]) ?(requires=[])
    ?(post_cond=[]) ?(assigns=WritesAny) ?(allocation=FreeAllocAny)  ?(extended=[]) ()
  =
  { b_name = name;
    b_assumes = assumes; (* must be always empty for default_behavior_name *)
    b_requires = requires;
    b_assigns = assigns ;
    b_allocation = allocation ;
    b_post_cond = post_cond ;
    b_extended = extended;
  }

let need_cast ?(force=false) oldt newt =
  let oldt = Ast_types.(remove_attributes_for_c_cast (unroll oldt)) in
  let newt = Ast_types.(remove_attributes_for_c_cast (unroll newt)) in
  not (Cil_datatype.Typ.equal oldt newt) &&
  (force ||
   match oldt, newt with
   | { tnode = TInt ik; tattr = ai }, { tnode = TEnum e; tattr = ae }
   | { tnode = TEnum e; tattr = ae }, { tnode = TInt ik; tattr = ai }
     when Attributes.equal ai ae -> ik <> e.ekind
   | _ -> true)

let cvar_to_lvar vi = match vi.vlogic_var_assoc with
  | None ->
    let lv =
      { lv_name = vi.vname;
        lv_id = vi.vid;
        lv_kind = LVC;
        lv_type = Ctype vi.vtype ;
        lv_attr = [];
        lv_origin = Some vi}
    in
    vi.vlogic_var_assoc <- Some lv; lv
  | Some lv -> lv

let cvar_to_term ~loc vi = tvar ~loc (cvar_to_lvar vi)

let copyVarinfo (vi: varinfo) (newname: string) : varinfo =
  let vi' = Cil_const.copy_with_new_vid vi in
  vi'.vname <- newname;
  (match vi.vlogic_var_assoc with
     None -> ()
   | Some _ ->
     vi'.vlogic_var_assoc <- None;
     ignore(cvar_to_lvar vi'));
  vi'

let rec findUniqueName ?(suffix="") fdec name =
  let current_name = name ^ suffix in
  (* Is this check a performance problem?  We could bring the old
     unchecked makeTempVar back as a separate function that assumes
     the prefix name does not occur in the original program. *)
  if (List.exists (fun vi -> vi.vname = current_name) fdec.slocals)
  || (List.exists (fun vi -> vi.vname = current_name) fdec.sformals) then begin
    fdec.smaxid <- 1 + fdec.smaxid;
    findUniqueName ~suffix:("_" ^ (string_of_int (1 + fdec.smaxid))) fdec name
  end else
    current_name

let refresh_local_name fdec vi =
  let new_name = findUniqueName fdec vi.vname in vi.vname <- new_name

let makeLocal ?(temp=false) ?referenced ?ghost ?(formal=false) ?loc fdec name typ =
  (* a helper function *)
  let name = findUniqueName fdec name in
  fdec.smaxid <- 1 + fdec.smaxid;
  let vi = makeVarinfo ~temp ?referenced ?ghost ?loc false formal name typ in
  vi

(* Make a local variable and add it to a function *)
let makeLocalVar fdec ?scope ?(temp=false) ?referenced ?(insert=true) ?ghost ?loc name typ =
  let vi = makeLocal ~temp ?referenced ?ghost ?loc fdec name typ in
  refresh_local_name fdec vi;
  if insert then
    begin
      fdec.slocals <- fdec.slocals @ [vi];
      let local_block =
        match scope with
        | None -> fdec.sbody
        | Some b -> b
      in
      local_block.blocals <- vi::local_block.blocals
    end;
  vi

let makeTempVar fdec ?insert ?ghost ?(name = "__cil_tmp") ?descr ?(descrpure = true) ?loc typ : varinfo =
  let vi = makeLocalVar fdec ~temp:true ?insert ?ghost ?loc name typ in
  vi.vdescr <- descr;
  vi.vdescrpure <- descrpure;
  vi

(* Set the types of arguments and results as given by the function type
 * passed as the second argument *)
let setFunctionType (f: fundec) (t: typ) =
  match Ast_types.unroll_skel t with
  | TFun (_rt, Some args, _va) ->
    if List.length f.sformals <> List.length args then
      Kernel.fatal ~current:true "setFunctionType: number of arguments differs from the number of formals" ;
    (* Change the function type. *)
    update_var_type f.svar t;
    (* Change the sformals and we know that indirectly we'll change the
     * function type *)
    List.iter2
      (fun (_an,at,aa) f -> update_var_type f at; f.vattr <- aa)
      args f.sformals

  | _ -> Kernel.fatal ~current:true "setFunctionType: not a function type"

(* Set the types of arguments and results as given by the function type
   passed as the second argument *)
let setFunctionTypeMakeFormals (f: fundec) (t: typ) =
  match Ast_types.unroll_node t with
  | TFun (_rt, Some args, _va) ->
    if f.sformals <> [] then
      Kernel.fatal ~current:true "setFunctionTypMakeFormals called on function %s with some formals already"
        f.svar.vname ;
    (* Change the function type. *)
    update_var_type f.svar t;
    f.sformals <-
      List.map (fun (n,t,_a) -> makeLocal ~formal:true f n t) args;
    setFunctionType f t

  | _ ->
    Kernel.fatal ~current:true "setFunctionTypeMakeFormals: not a function type: %a"
      !pp_typ_ref t

let setMaxId (f: fundec) =
  f.smaxid <- List.length f.sformals + List.length f.slocals

(* Make a formal variable for a function. Insert it in both the sformals
 * and the type of the function. You can optionally specify where to insert
 * this one. If where = "^" then it is inserted first. If where = "$" then
 * it is inserted last. Otherwise where must be the name of a formal after
 * which to insert this. By default it is inserted at the end. *)
let makeFormalVar fdec ?(ghost=fdec.svar.vghost) ?(where = "$") ?loc name typ : varinfo =
  assert ((not fdec.svar.vghost) || ghost) ;
  let makeit name =
    let vi = makeLocal ~ghost ?loc ~formal:true fdec name typ in
    if ghost && not fdec.svar.vghost then
      vi.vattr <- Ast_attributes.(add (frama_c_ghost_formal, []) vi.vattr);
    vi
  in
  let error () = Kernel.fatal ~current:true
      "makeFormalVar: cannot find insert-after formal %s" where
  in
  (* Search for the insertion place *)
  let rec loopFormals acc = function
    | [] ->
      if where = "$" || (ghost && where = "^") then
        let vi = makeit name in vi, List.rev (vi :: acc)
      else error ()
    | f :: rest when not ghost && f.vghost ->
      if where = "$" then
        let vi = makeit name in vi, List.rev_append acc (vi :: f :: rest)
      else error ()
    | f :: rest when f.vname = where && f.vghost = ghost ->
      let vi = makeit name in vi, List.rev_append acc (f :: vi :: rest)
    | f :: rest when ghost && f.vghost && where = "^" ->
      let vi = makeit name in vi, List.rev_append acc (vi :: f :: rest)
    | f :: rest -> loopFormals (f::acc) rest
  in
  let vi, newformals =
    if where = "^" && not ghost then let vi = makeit name in vi, vi :: fdec.sformals
    else
      loopFormals [] fdec.sformals
  in
  setFormals fdec newformals;
  vi

(* Make a global variable. Your responsibility to make sure that the name
 * is unique *)
let makeGlobalVar ?source ?temp ?referenced ?ghost ?alignas ?loc name typ =
  makeVarinfo ?source ?temp ?referenced ?ghost ?alignas ?loc true false name typ

let create_string_literal =
  let module StrLitCounter =
    State_builder.SharedCounter(struct let name = "StrLitCounter" end)
  in
  fun ?(loc=Fileloc.unknown) s ->
    let i = StrLitCounter.next() in
    let vname = "__fc_lit_string" ^ (string_of_int i) in
    let typ = typeOf_string_literal ~loc s in
    let var = makeGlobalVar ~source:true ~temp:false ~loc vname typ in
    var.vdefined <- true;
    var.vattr <- Ast_attributes.(add (fc_literal,[]) var.vattr);
    var

let create_wstring_literal =
  let module WStrLitCounter =
    State_builder.SharedCounter(struct let name = "WStrLitCounter" end)
  in
  fun ?(loc=Fileloc.unknown) l ->
    let i = WStrLitCounter.next() in
    let vname = "__fc_lit_wstring" ^ (string_of_int i) in
    let typ = typeOf_wstring_literal ~loc l in
    let var = makeGlobalVar ~source:true ~temp:false ~loc vname typ in
    var.vdefined <- true;
    var.vattr <- Ast_attributes.(add (fc_literal,[]) var.vattr);
    var

let mkPureExprInstr ~fundec ~scope ?loc e =
  let loc = match loc with None -> e.eloc | Some l -> l in
  let typ = typeOf e in
  let descr = Format.asprintf "%a" !pp_exp_ref e in
  let tmp = makeLocalVar ~temp:true ~scope ~loc fundec "tmp" typ in
  tmp.vdescr <- Some descr;
  tmp.vdefined <- true;
  Local_init(tmp, AssignInit (SingleInit e), loc)

let mkPureExpr ?ghost ?valid_sid ~(fundec:fundec) ?loc (e : exp) : stmt =
  let scope = mkBlock [] in
  let instr = mkPureExprInstr ~fundec ~scope ?loc e in
  scope.bstmts <- [ mkStmtOneInstr ?ghost ?valid_sid instr];
  mkStmt ?ghost ?valid_sid (Block scope)

let emptyFunctionFromVI vi =
  let r =
    { svar  = vi;
      smaxid = 0;
      slocals = [];
      sformals = [];
      sbody = mkBlock [];
      smaxstmtid = None;
      sallstmts = [];
      sspec =   empty_funspec ()
    }
  in
  setFormalsDecl r.svar r.svar.vtype;
  r

(* Make an empty function *)
let emptyFunction name =
  let vi =
    makeGlobalVar ~temp:false name Cil_const.(mk_tfun voidType (Some []) false)
  in emptyFunctionFromVI vi

let rec lastOffset (off: offset) : offset =
  match off with
  | NoOffset | Field(_,NoOffset) | Index(_,NoOffset) -> off
  | Field(_,off) | Index(_,off) -> lastOffset off

let isBitfield lval =
  match lval with
  | _, off ->
    let off = lastOffset off in
    match off with
      Field({fbitfield=Some _}, _) -> true
    | _ -> false

let addOffsetLval toadd (b, off) : lval =
  b, addOffset toadd off

let rec removeOffset (off: offset) : offset * offset =
  match off with
    NoOffset -> NoOffset, NoOffset
  | Field(_f, NoOffset) -> NoOffset, off
  | Index(_i, NoOffset) -> NoOffset, off
  | Field(f, restoff) ->
    let off', last = removeOffset restoff in
    Field(f, off'), last
  | Index(i, restoff) ->
    let off', last = removeOffset restoff in
    Index(i, off'), last

let removeOffsetLval ((b, off): lval) : lval * offset =
  let off', last = removeOffset off in
  (b, off'), last

class copyVisitExpr = object
  inherit genericCilVisitor (Visitor_behavior.copy (Project.current ()))
  method! vexpr e =
    ChangeDoChildrenPost ({e with eid = Cil_const.new_raw_eid ()}, fun x -> x)
end

let copy_exp e = visitCilExpr (new copyVisitExpr) e

(** A visitor that does constant folding. If "machdep" is true then we do
 * machine dependent simplification (e.g., sizeof) *)
class constFoldVisitorClass (machdep: bool) : cilVisitor = object
  inherit nopCilVisitor

  method! vinst i =
    match i with
    (* Skip two functions to which we add Sizeof to the type arguments.
       See the comments for these above. *)
      Call(_,Var vi,_,_)
      when ((vi.vname = "__builtin_va_arg")
            || (vi.vname = "__builtin_types_compatible_p")) ->
      SkipChildren
    | _ -> DoChildren
  method! vexpr (e: exp) =
    (* Do it bottom up *)
    ChangeDoChildrenPost (e, constFold machdep)

  method! vglob = function
    | GFun _ | GVar _ | GVarDecl _ | GType _ | GCompTag _ | GCompTagDecl _
    | GEnumTag _ | GEnumTagDecl _ ->
      DoChildren
    | _ -> SkipChildren

  method! vspec _ = SkipChildren
  method! vcode_annot _ = SkipChildren
end
let constFoldVisitor (machdep: bool) = new constFoldVisitorClass machdep

let rec constFoldTermNodeAtTop = function
  | TSizeOf typ as t ->
    begin
      try integer_lconstant (bytesSizeOf typ)
      with SizeOfError _ -> t
    end
  | TAlignOf typ as t ->
    begin
      try integer_lconstant (bytesAlignOf typ)
      with SizeOfError _ -> t
    end
  | TSizeOfE { term_type= Ctype typ } -> constFoldTermNodeAtTop (TSizeOf typ)
  | TAlignOfE { term_type= Ctype typ }
    -> constFoldTermNodeAtTop (TAlignOf typ)
  | TSizeOfE _ | TAlignOfE _ ->
    assert false (* sizeof/alignof of logic types are rejected
                    by typing anyway. *)
  | TUnOp (op, ({ term_node = n1 } as t1)) ->
    begin
      let constFoldTermUnOp int_unop =
        match constFoldTermNodeAtTop n1 with
        | TConst (Integer (i, _)) -> TConst (Integer (int_unop i, None))
        | n1 -> TUnOp (op, {t1 with term_node = n1})
      in
      match op with
      | Neg -> constFoldTermUnOp Z.neg
      | BNot -> constFoldTermUnOp Z.lognot
      | LNot -> constFoldTermUnOp
                  (fun i -> if Z.is_zero i
                    then Z.one else Z.zero)
    end
  | TBinOp (op, ({term_node = n1} as t1), ({term_node = n2} as t2)) ->
    begin
      let n1 = constFoldTermNodeAtTop n1 in
      let n2 = constFoldTermNodeAtTop n2 in
      let constFoldTermBinOp int_bop =
        match n1, n2 with
        | TConst (Integer (i1, _)), TConst (Integer (i2, _)) ->
          TConst (Integer (int_bop i1 i2, None))
        | n1, n2 ->
          TBinOp (op, {t1 with term_node = n1}, {t2 with term_node = n2})

      in
      match op with
      | PlusA -> constFoldTermBinOp Z.add
      | MinusA -> constFoldTermBinOp Z.sub
      | Mult -> constFoldTermBinOp Z.mul
      | Shiftlt -> constFoldTermBinOp Z.shift_left_z
      | Shiftrt -> (* right-shifting Lintegers is always arithmetic *)
        constFoldTermBinOp Z.shift_right_z
      | BAnd -> constFoldTermBinOp Z.logand
      | BXor -> constFoldTermBinOp Z.logxor
      | BOr -> constFoldTermBinOp Z.logor
      | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr ->
        let bool_op = match op with
          | Lt -> Z.lt
          | Gt -> Z.gt
          | Le -> Z.leq
          | Ge -> Z.geq
          | Eq -> Z.equal
          | Ne -> (fun i1 i2 -> not (Z.equal i1 i2))
          | LAnd ->
            (fun i1 i2 -> not (Z.is_zero i1) && not (Z.is_zero i2))
          | LOr ->
            (fun i1 i2 -> not (Z.is_zero i1) || not (Z.is_zero i2))
          | _ -> assert false
        in
        constFoldTermBinOp
          (fun i1 i2 -> if bool_op i1 i2 then Z.one else Z.zero)
      | _ ->
        TBinOp (op, {t1 with term_node = n1}, {t2 with term_node = n2})
    end
  | t -> t

let constFoldTerm t =
  let visitor = object
    inherit nopCilVisitor
    method! vterm_node t =
      ChangeToPost (t, constFoldTermNodeAtTop)
  end
  in
  visitCilTerm visitor t


(** Find a function or function prototype with the given name in the file.
  * If it does not exist, create a prototype with the given type, and return
  * the new varinfo.  This is useful when you need to call a libc function
  * whose prototype may or may not already exist in the file.
  *
  * Because the new prototype is added to the start of the file, you shouldn't
  * refer to any struct or union types in the function type.*)
let findOrCreateFunc (f:file) (name:string) (t:typ) : varinfo =
  let rec search glist =
    match glist with
    | GFunDecl(_, vi, _) :: _rest when vi.vname = name -> vi
    | GVarDecl(vi,_) :: _rest when vi.vname = name ->
      Kernel.fatal ~current:true
        "findOrCreateFunc: can't create %s because another global exists \
         with that name." name ;
    | _ :: rest -> search rest (* tail recursive *)
    | [] -> (*not found, so create one *)
      let t' = Ast_types.unroll_deep t in
      let new_decl = makeGlobalVar ~temp:false name t' in
      setFormalsDecl new_decl t';
      f.globals <- GFunDecl(empty_funspec (), new_decl, Fileloc.unknown) :: f.globals;
      new_decl
  in
  search f.globals

let childrenFileSameGlobals vis f =
  let fGlob g = visitCilGlobal vis g in
  iterGlobals f
    (fun g ->
       match fGlob g with
         [g'] when g' == g -> ()
       | gl ->
         Kernel.fatal ~current:true
           "You used visitCilFileSameGlobals but \
            the global got physically changed:\n %a\nchanged to %a\n"
           !pp_global_ref g
           (Pretty_utils.pp_list ~sep:"@\n" !pp_global_ref) gl ;
    );
  f

let post_file vis f =
  let res = vis#vfile f in
  let post_action res = vis#fill_global_tables; res in
  match res with
    SkipChildren -> ChangeToPost(f, post_action)
  | JustCopy -> JustCopyPost post_action
  | JustCopyPost f -> JustCopyPost (fun x -> f (post_action x))
  | ChangeTo res -> ChangeToPost(res, post_action)
  | ChangeToPost (res, f) -> ChangeToPost (res, fun x -> f (post_action x))
  | DoChildren -> DoChildrenPost post_action
  | DoChildrenPost f -> DoChildrenPost (fun x -> f (post_action x))
  | ChangeDoChildrenPost(f,post) ->
    ChangeDoChildrenPost(f, fun x -> post (post_action x))

(* A visitor for the whole file that does not change the globals *)
let visitCilFileSameGlobals (vis : cilVisitor) (f : file) : unit =
  if Visitor_behavior.is_copy vis#behavior then
    Kernel.fatal ~current:true "You used visitCilFileSameGlobals with a copy visitor. Nothing is done"
  else
    ignore
      (doVisitCil vis (Visitor_behavior.cfile vis#behavior) (post_file vis) childrenFileSameGlobals f)

let visitCilFileFunctions vis file =
  let process_one_global = function
    | GFun (fundec, _) -> ignore (visitCilFunction vis fundec)
    | _ -> ()
  in
  iterGlobals file process_one_global

let childrenFileCopy vis f =
  let fGlob g = visitCilGlobal vis g in
  (* Scan the globals. Make sure this is tail recursive. *)
  let rec loop (acc: global list) = function
      [] -> f.globals <- List.rev acc
    | g :: restg ->
      loop (List.rev_append (fGlob g) acc) restg
  in
  loop [] f.globals;
  (* the global initializer *)
  (match f.globinit with
     None -> ()
   | Some g -> f.globinit <- Some (visitCilFunction vis g));
  f

(* Be careful with visiting the whole file because it might be huge. *)
let visitCilFileCopy (vis : cilVisitor) (f : file) : file =
  if Visitor_behavior.is_copy vis#behavior then begin
    Queue.add Logic_env.prepare_tables vis#get_filling_actions;
  end;
  doVisitCil vis (Visitor_behavior.cfile vis#behavior) (post_file vis) childrenFileCopy f

let visitCilFile vis f =
  if Visitor_behavior.is_copy vis#behavior then
    Kernel.fatal ~current:true "You used visitCilFile with a copy visitor. Nothing is done"
  else ignore (visitCilFileCopy vis f)


let appears_in_expr v e =
  let module M = struct exception Found end in
  let vis = object
    inherit nopCilVisitor
    method! vvrbl v' =
      if Cil_datatype.Varinfo.equal v v' then raise M.Found;
      SkipChildren
  end
  in
  try ignore (visitCilExpr vis e); false
  with M.Found -> true

(* Fold over all globals, including the global initializer *)
let mapGlobals (fl: file)
    (doone: global -> global) : unit =
  fl.globals <- List.map doone fl.globals;
  (match fl.globinit with
     None -> ()
   | Some g -> begin
       match doone (GFun(g, Fileloc.unknown)) with
         GFun(g', _) -> fl.globinit <- Some g'
       | _ -> Kernel.fatal ~current:true "mapGlobals: globinit is not a function"
     end)

let global_annotation_attributes = function
  | Dfun_or_pred ({l_var_info = { lv_attr }}, _) -> lv_attr
  | Dvolatile (_,_,_,attrs,_) -> attrs
  | Daxiomatic (_,_,attrs,_) | Dmodule(_,_,attrs,_,_) -> attrs
  | Dtype ({ lt_attr }, _) -> lt_attr
  | Dlemma (_,_,_,_,attrs,_) -> attrs
  | Dinvariant ({l_var_info = { lv_attr }}, _) -> lv_attr
  | Dtype_annot ({l_var_info = { lv_attr }}, _) -> lv_attr
  | Dmodel_annot ({ mi_attr }, _) -> mi_attr
  | Dextended (_,attrs,_) -> attrs

let global_attributes = function
  | GType ({ttype},_) -> Ast_types.get_attributes ttype
  | GCompTag({cattr = attrs},_) | GCompTagDecl({cattr = attrs},_)
  | GEnumTag({eattr = attrs},_) | GEnumTagDecl({eattr = attrs},_)
  | GVarDecl({vattr = attrs},_) | GVar({vattr = attrs},_,_) -> attrs
  | GFun({svar = {vattr = attrs}},_)
  | GFunDecl(_,{vattr = attrs},_) -> attrs
  | GPragma (attr, _) -> [attr]
  | GAnnot (gannot,_) -> global_annotation_attributes gannot
  | GAsm _ | GText _ -> []

let is_in_libc attrs =
  Ast_attributes.(contains fc_stdlib attrs) ||
  Ast_attributes.(contains fc_stdlib_generated attrs)

let global_is_in_libc g =
  is_in_libc (global_attributes g)

(***************************************************************************)

(* Convert an expression into an attribute, if possible. Otherwise raise
   NotAnAttrParam *)
exception NotAnAttrParam of exp
let rec expToAttrParam (e: exp) : attrparam =
  match (constFold true e).enode with
  | Const(CInt64(i,k,_)) ->
    let i', _trunc = truncateInteger64 k i in
    AInt i'
  | Const(CEnum ei) -> expToAttrParam ei.eival
  | Lval (Var v, NoOffset) -> ACons(v.vname, [])
  | SizeOf t -> ASizeOf t
  | SizeOfE e' -> ASizeOfE (expToAttrParam e')
  | UnOp(uo, e', _)  -> AUnOp (uo, expToAttrParam e')
  | BinOp(bo, e1',e2', _)  -> ABinOp (bo, expToAttrParam e1',
                                      expToAttrParam e2')
  | _ -> raise (NotAnAttrParam e)


(******************** OPTIMIZATIONS *****)
let rec peepHole1 (* Process one statement and possibly replace it *)
    (doone: instr -> instr list option)
    (* Scan a block and recurse inside nested blocks *)
    (ss: stmt list) : unit =
  let rec doInstrList (il: instr list) : instr list =
    match il with
      [] -> []
    | i :: rest -> begin
        match doone i with
          None -> i :: doInstrList rest
        | Some sl -> doInstrList (sl @ rest)
      end
  in
  List.iter
    (fun s ->
       begin match s.skind with
         | Instr i -> s.skind <- stmt_of_instr_list (doInstrList [i])
         | If (_e, tb, eb, _) ->
           peepHole1 doone tb.bstmts;
           peepHole1 doone eb.bstmts
         | Switch (_e, b, _, _) -> peepHole1 doone b.bstmts
         | Loop (_, b, _l, _, _) -> peepHole1 doone b.bstmts
         | Block b -> peepHole1 doone b.bstmts
         | UnspecifiedSequence seq ->
           peepHole1 doone (List.map (fun (x,_,_,_,_) -> x) seq)
         | TryCatch(b,l,_) ->
           peepHole1 doone b.bstmts;
           List.iter (fun (_,b) -> peepHole1 doone b.bstmts) l
         | TryFinally (b, h, _l) ->
           peepHole1 doone b.bstmts;
           peepHole1 doone h.bstmts
         | TryExcept (b, (il, e), h, l) ->
           peepHole1 doone b.bstmts;
           peepHole1 doone h.bstmts;
           s.skind <- TryExcept(b, (doInstrList il, e), h, l);
         | Return _ | Goto _ | Break _ | Continue _ | Throw _ -> ()
       end ;
       enforceGhostStmtCoherence s)
    ss

(* Process two statements and possibly replace them both *)
let rec peepHole2 ~aggressive (dotwo: stmt * stmt -> stmt list option) (ss: stmt list) =
  let rec doStmtList acc (il: stmt list) : stmt list =
    match il with
      [] -> List.rev acc
    | [i] -> process i; List.rev (i::acc)
    | (i1 :: ((i2 :: rest) as rest2)) ->
      begin
        match dotwo (i1,i2) with
          None -> process i1; doStmtList (i1::acc) rest2
        | Some sl ->
          if aggressive then
            doStmtList acc (sl @ rest)
          else
            doStmtList (List.rev_append sl acc) rest
      end
  and doUnspecifiedStmtList il =
    match il with
      [] -> []
    | [ (s,_,_,_,_) ] -> process s; il
    | ((i1,m1,w1,r1,_) as hd)::(((i2,m2,w2,r2,_)::rest) as rest2) ->
      begin
        match dotwo (i1,i2) with
          None -> process i1; hd :: doUnspecifiedStmtList rest2
        | Some [] -> doUnspecifiedStmtList rest
        | Some (hd::tl) ->
          let call s = match s.skind with
            | Instr(Call _ | Local_init (_, ConsInit _, _)) -> [ref s]
            | _ -> []
          in
          let res =
            (hd, m1@m2, w1 @ w2, r1 @ r2,call hd) ::
            (List.map (fun x -> x,[],[],[],call x) tl)
          in
          if aggressive then doUnspecifiedStmtList (res @ rest)
          else res @ doUnspecifiedStmtList rest
      end
  and process s =
    match s.skind with
      Instr _i -> ()
    | If (_e, tb, eb, _) ->
      tb.bstmts <- peepHole2 ~aggressive dotwo tb.bstmts;
      eb.bstmts <- peepHole2 ~aggressive dotwo eb.bstmts
    | Switch (_e, b, _, _) -> b.bstmts <- peepHole2 ~aggressive dotwo b.bstmts
    | Loop (_, b, _l, _, _) -> b.bstmts <- peepHole2 ~aggressive dotwo b.bstmts
    | Block b -> b.bstmts <- doStmtList [] b.bstmts
    | TryCatch (b,l,_) ->
      b.bstmts <- peepHole2 ~aggressive dotwo b.bstmts;
      List.iter
        (fun (_,b) ->
           b.bstmts <-  peepHole2 ~aggressive dotwo b.bstmts)
        l
    | TryFinally (b, h, _l) ->
      b.bstmts <- peepHole2 ~aggressive dotwo b.bstmts;
      b.bstmts <- peepHole2 ~aggressive dotwo h.bstmts
    | TryExcept (b, (_il, _e), h, _l) ->
      b.bstmts <- peepHole2 ~aggressive dotwo b.bstmts;
      h.bstmts <- peepHole2 ~aggressive dotwo h.bstmts;
      () (*s.skind <- TryExcept (b, (doInstrList il, e), h, l)*)

    | UnspecifiedSequence seq ->
      s.skind <- UnspecifiedSequence (doUnspecifiedStmtList seq)
    | Return _ | Goto _ | Break _ | Continue _ | Throw _ -> ()
  in
  if aggressive then List.iter process ss;
  doStmtList [] ss

(* Make an AddrOf. Given an lval of type T will give back an expression of
 * type ptr(T)  *)
let mkAddrOf ~loc ((_b, _off) as lval) : exp =
  (* Never take the address of a register variable *)
  (match lval with
     Var vi, _off when vi.vstorage = Register -> vi.vstorage <- NoStorage
   | _ -> ());
  match lval with
    Mem e, NoOffset -> e
  | b, Index(z, NoOffset) when isZero z -> new_exp ~loc (StartOf (b, NoOffset))
  (* array *)
  | _ -> new_exp ~loc (AddrOf lval)

let mkAddrOfVi vi = mkAddrOf ~loc:vi.vdecl (var vi)

let mkAddrOrStartOf ~loc (lv: lval) : exp =
  match Ast_types.unroll_skel (typeOfLval lv) with
  | TArray _ -> new_exp ~loc (StartOf lv)
  | _ -> mkAddrOf ~loc lv

let mkMem ~(addr: exp) ~(off: offset) : lval =
  let res =
    match addr.enode, off with
    | AddrOf lv, _ -> addOffsetLval off lv
    | StartOf lv, _ -> (* Must be an array *)
      addOffsetLval (Index(zero ~loc:addr.eloc, off)) lv
    | _, _ -> Mem addr, off
  in
  (* ignore (E.log "memof : %a:%a\nresult = %a\n" d_plainexp addr d_plainoffset
     off d_plainexp res); *)
  res

let mkTermMem ~(addr: term) ~(off: term_offset) : term_lval =
  let loc = addr.term_loc in
  let res =
    match addr.term_node, off with
      TAddrOf lv, _ -> addTermOffsetLval off lv
    | TStartOf lv, _ -> (* Must be an array *)
      addTermOffsetLval (TIndex(lzero ~loc (), off)) lv
    | _, _ -> TMem addr, off
  in
  (*  ignore (E.log "memof : %a:%a\nresult = %a\n"
      d_plainexp addr d_plainoffset off d_plainexp res); *)
  res

let treat_constructor_as_func action v f args kind loc =
  let lv, args =
    match kind with
    | Plain_func -> Some (var v), args
    | Constructor -> None, mkAddrOfVi v :: args
  in
  action lv (Var f) args loc

let fold_local_init b f acc =
  let rec find_stmt acc s =
    match s.skind with
    | Instr(Local_init(v',i,l)) -> f s (v',i,l) acc
    | UnspecifiedSequence l -> List.fold_left find_stmt_seq acc l
    | Block b when not b.bscoping -> List.fold_left find_stmt acc b.bstmts
    | _ -> acc
  and find_stmt_seq acc (s, _, _, _, _) = find_stmt acc s
  in
  List.fold_left find_stmt acc b.bstmts

let find_def_stmt b v =
  if not (v.vdefined && List.exists (Cil_datatype.Varinfo.equal v) b.blocals)
  then Kernel.fatal "inconsistent arguments for find_def_stmt";
  let module M = struct exception Found of stmt end in
  let action s (v',_,_) () =
    if Cil_datatype.Varinfo.equal v v' then raise (M.Found s) else ()
  in
  try
    fold_local_init b action ();
    Kernel.fatal ~source:(fst v.vdecl)
      "inconsistent AST: local variable %a is supposed to be initialized, \
       but no initialization statement found." !pp_varinfo_ref v
  with M.Found s -> s

let has_extern_local_init b =
  (* a scoping block defines all the locals inside it.*)
  if b.bscoping then false
  else begin
    let action _ _ () = raise Exit in
    try fold_local_init b action (); false with Exit -> true
  end

let instr_falls_through = function
  | Call (_, f, _, _) -> not (Ast_types.has_attribute "noreturn" (typeOfLhost f))
  | _ -> true

let splitFunctionType (ftype: typ)
  : typ * (string * typ * attributes) list option * bool * attributes =
  match Ast_types.unroll ftype with
  | { tnode = TFun (rt, args, isva); tattr } -> rt, args, isva, tattr
  | _ -> Kernel.fatal ~current:true "splitFunctionType invoked on a non function type %a"
           !pp_typ_ref ftype

let splitFunctionTypeVI (fvi: varinfo)
  : typ * (string * typ * attributes) list option * bool * attributes =
  match Ast_types.unroll fvi.vtype with
  | { tnode = TFun (rt, args, isva); tattr } -> rt, args, isva, tattr
  | _ -> Kernel.abort "Function %s invoked on a non function type" fvi.vname

let argsToPairOfLists args =
  List.partition
    (fun f -> not(isGhostFormalVarDecl f))
    (argsToList args)

let remove_attributes_for_integral_promotion a =
  let to_remove =
    Ast_attributes.(bitfield_attribute_name :: spare_attributes_for_c_cast)
  in
  Ast_attributes.drop_list to_remove a

let rec integralPromotion t = (* c.f. ISO 6.3.1.1 *)
  let open Cil_const in
  match Ast_types.unroll t with
  | { tnode = TInt (IShort|ISChar|IBool); tattr } ->
    let tattr = remove_attributes_for_integral_promotion tattr in
    mk_tint ~tattr IInt
  | { tnode = TInt (IUChar|IUShort as k); tattr } ->
    let tattr = remove_attributes_for_integral_promotion tattr in
    let ik = if bitsSizeOfInt k < bitsSizeOf intType then IInt else IUInt in
    mk_tint ~tattr ik
  | { tnode = TInt IChar; tattr } ->
    let k = if isSigned IChar then ISChar else IUChar in
    integralPromotion (mk_tint ~tattr k)
  | { tnode = TInt k; tattr } ->
    begin match Ast_attributes.(find_params bitfield_attribute_name tattr) with
      | [AInt size] ->
        (* This attribute always fits in int. *)
        let size = Z.to_int size in
        let sizeofint = bitsSizeOf intType in
        let tattr = remove_attributes_for_integral_promotion tattr in
        let kind =
          if size < sizeofint then IInt
          else if size = sizeofint then
            if isSigned k then IInt
            else IUInt
          else k
        in
        mk_tint ~tattr kind
      | [] -> t
      | _ -> assert false
    end
  | { tnode = TEnum ei; tattr } -> (* gcc packed enums can be < int *)
    integralPromotion (mk_tint ~tattr ei.ekind)
  | _ -> Kernel.fatal ~current:true "integralPromotion: not expecting %a" !pp_typ_ref t

let arithmeticConversion t1 t2 = (* c.f. ISO 6.3.1.8 *)
  let checkToInt _ = () in  (* dummies for now *)
  let checkToFloat _ = () in
  match Ast_types.unroll_skel t1, Ast_types.unroll_skel t2 with
  | TFloat FLongDouble, _ -> checkToFloat t2; t1
  | _, TFloat FLongDouble -> checkToFloat t1; t2
  | TFloat (FFloat64|FDouble), _ -> checkToFloat t2; t1
  | _, TFloat (FFloat64|FDouble) -> checkToFloat t1; t2
  | TFloat (FFloat|FFloat32), _ -> checkToFloat t2; t1
  | _, TFloat (FFloat|FFloat32) -> checkToFloat t1; t2
  | _, _ -> begin
      let t1' = integralPromotion t1 in
      let t2' = integralPromotion t2 in
      match Ast_types.unroll_skel t1', Ast_types.unroll_skel t2' with

      (* 128-bit ints: copied from 'long long'-based lines below *)
      | TInt IUInt128, _ -> checkToInt t2'; t1'
      | _, TInt IUInt128 -> checkToInt t1'; t2'

      | TInt IInt128, _
        when bitsSizeOf t1' <= bitsSizeOf t2' &&
             (not (isSignedInteger t2')) -> Cil_const.uint128Type
      | _, TInt IInt128
        when bitsSizeOf t2' <= bitsSizeOf t1' &&
             (not (isSignedInteger t1')) -> Cil_const.uint128Type

      | TInt IInt128, _ -> checkToInt t2'; t1'
      | _, TInt IInt128 -> checkToInt t1'; t2'


      | TInt IULongLong, _ -> checkToInt t2'; t1'
      | _, TInt IULongLong -> checkToInt t1'; t2'

      | TInt ILongLong, _
        when bitsSizeOf t1' <= bitsSizeOf t2' &&
             (not (isSignedInteger t2')) -> Cil_const.ulongLongType
      | _, TInt ILongLong
        when bitsSizeOf t2' <= bitsSizeOf t1' &&
             (not (isSignedInteger t1')) -> Cil_const.ulongLongType

      | TInt ILongLong, _ -> checkToInt t2'; t1'
      | _, TInt ILongLong -> checkToInt t1'; t2'

      | TInt IULong, _ -> checkToInt t2'; t1'
      | _, TInt IULong -> checkToInt t1'; t2'


      | TInt ILong, TInt IUInt
        when bitsSizeOf t1' <= bitsSizeOf t2' -> Cil_const.ulongType
      | TInt IUInt, TInt ILong
        when bitsSizeOf t2' <= bitsSizeOf t1' -> Cil_const.ulongType

      | TInt ILong, _ -> checkToInt t2'; t1'
      | _, TInt ILong -> checkToInt t1'; t2'

      | TInt IUInt, _ -> checkToInt t2'; t1'
      | _, TInt IUInt -> checkToInt t1'; t2'

      | TInt IInt, TInt IInt -> t1'

      | n1, n2 ->
        Kernel.fatal ~current:true "arithmeticConversion %a -> %a@."
          !pp_typ_ref (Cil_const.mk_typ n1) !pp_typ_ref (Cil_const.mk_typ n2)
    end

let rec isConstantGen is_varinfo_cst f e = match e.enode with
  | Const c -> f c
  | UnOp (_, e, _) -> isConstantGen is_varinfo_cst f e
  | BinOp (_, e1, e2, _) ->
    isConstantGen is_varinfo_cst f e1 &&
    isConstantGen is_varinfo_cst f e2
  | Lval (Var vi, NoOffset) ->
    is_varinfo_cst vi ||
    (vi.vglob && Ast_types.is_array vi.vtype) ||
    Ast_types.is_fun vi.vtype
  | Lval (Var vi, offset) ->
    is_varinfo_cst vi && isConstantOffsetGen is_varinfo_cst f offset
  | Lval _ -> false
  | SizeOf _ | SizeOfE _ | AlignOf _ | AlignOfE _ -> true
  (* see ISO 6.6.6 *)
  | CastE(t,{ enode = Const(CReal _)}) when Ast_types.is_integral t -> true
  | CastE(t, e) ->
    begin
      match t.tnode, (typeOf e).tnode with
      | TInt i, TPtr _ ->
        (* gcc/clang/ccomp only consider non-truncated constant ptr values
           to be constant. If it is truncated, we consider it non-const
           in any case.
        *)
        bytesSizeOfInt (Machine.uintptr_kind ()) <= bytesSizeOfInt i &&
        isConstantGen is_varinfo_cst f e
      | _ -> isConstantGen is_varinfo_cst f e
    end
  | AddrOf (Var vi, off) | StartOf (Var vi, off) ->
    vi.vglob && isConstantOffsetGen is_varinfo_cst f off
  | AddrOf (Mem e, off) | StartOf(Mem e, off) ->
    isConstantGen is_varinfo_cst f e &&
    isConstantOffsetGen is_varinfo_cst f off

and isConstantOffsetGen is_varinfo_cst f = function
    NoOffset -> true
  | Field(_fi, off) -> isConstantOffsetGen is_varinfo_cst f off
  | Index(e, off) ->
    isConstantGen is_varinfo_cst f e &&
    isConstantOffsetGen is_varinfo_cst f off

let isConstant ?(is_varinfo_cst = alphafalse) e =
  isConstantGen is_varinfo_cst alphatrue e
let isConstantOffset ?(is_varinfo_cst = alphafalse) o =
  isConstantOffsetGen is_varinfo_cst alphatrue o

let isIntegerConstant ?(is_varinfo_cst = alphafalse) e =
  isConstantGen is_varinfo_cst
    (function
      | CInt64 _ | CChr _ | CEnum _ -> true
      | CReal _ -> false)
    e

let getCompField cinfo fieldName =
  List.find
    (fun fi -> fi.fname = fieldName)
    (Option.value ~default:[] cinfo.cfields)

let getCompType typ =
  match Ast_types.unroll_skel typ with
  | TComp comp -> comp
  | _ -> raise Not_found

let sameSizeInt ?(machdep=false) (ik1 : ikind) (ik2 : ikind) =
  if machdep then bytesSizeOfInt ik1 == bytesSizeOfInt ik2
  else
    match ik1, ik2 with
    | (IChar | ISChar | IUChar), (IChar | ISChar | IUChar) -> true
    | (IShort | IUShort), (IShort | IUShort) -> true
    | (IInt | IUInt), (IInt | IUInt) -> true
    | (ILong | IULong), (ILong | IULong) -> true
    | (ILongLong | IULongLong), (ILongLong | IULongLong) -> true
    | _ -> false


let sameSign ?(machdep=false) (ik1 : ikind) (ik2 : ikind) =
  if machdep then isSigned ik1 = isSigned ik2
  else
    match ik1, ik2 with
    | IChar, (ISChar | IUChar)
    | ISChar, (IChar | IUChar)
    | IUChar, (IChar | ISChar) -> false
    | _ -> isSigned ik1 = isSigned ik2

let same_int64 ?(machdep=true) e1 e2 =
  match constFoldToInt ~machdep e1, constFoldToInt ~machdep e2 with
  | Some i, Some i' -> Z.equal i i'
  | _ -> false

(* how type qualifiers must be checked *)
type qualifier_check_context =
  | Identical (* identical qualifiers. *)
  | IdenticalToplevel (* ignore at toplevel, use Identical when going under a
                         pointer. *)
  | Covariant (* first type can have const-qualifications
                 the second doesn't have. *)
  | CovariantToplevel
  (* accepts everything for current type, use Covariant when going under a
     pointer. *)
  | Contravariant (* second type can have const-qualifications
                     the first doesn't have. *)
  | ContravariantToplevel
  (* accepts everything for current type, use Contravariant when going under
     a pointer. *)

let qualifier_context_fun_arg = function
  | Identical | IdenticalToplevel -> IdenticalToplevel
  | Covariant | CovariantToplevel -> ContravariantToplevel
  | Contravariant | ContravariantToplevel -> CovariantToplevel

let qualifier_context_fun_ret = function
  | Identical | IdenticalToplevel -> IdenticalToplevel
  | Covariant | CovariantToplevel -> CovariantToplevel
  | Contravariant | ContravariantToplevel -> ContravariantToplevel

let qualifier_context_ptr = function
  | Identical | IdenticalToplevel -> Identical
  | Covariant | CovariantToplevel -> Covariant
  | Contravariant | ContravariantToplevel -> Contravariant

let included_qualifiers ?(context=Identical) a1 a2 =
  let open Ast_attributes in
  let a1 = filter_qualifiers a1 in
  let a2 = filter_qualifiers a2 in
  let a1 = drop "restrict" a1 in
  let a2 = drop "restrict" a2 in
  let a1_no_cv = drop_list ["const"; "volatile"] a1 in
  let a2_no_cv = drop_list ["const"; "volatile"] a2 in
  let is_equal = Cil_datatype.Attributes.equal a1 a2 in
  if is_equal then true
  else begin
    match context with
    | Identical -> false
    | Covariant -> Cil_datatype.Attributes.equal a1_no_cv a2
    | Contravariant -> Cil_datatype.Attributes.equal a1 a2_no_cv
    | CovariantToplevel | ContravariantToplevel | IdenticalToplevel -> true
  end

(* precondition: t1 and t2 must be "compatible" as per combineTypes, i.e.
   you must have called [combineTypes t1 t2] before calling this function. *)
let rec have_compatible_qualifiers_deep ?(context=Identical) t1 t2 =
  let t1 = Ast_types.unroll t1 and t2 = Ast_types.unroll t2 in
  match t1.tnode, t2.tnode with
  | TFun (tres1, Some args1, _), TFun (tres2, Some args2, _) ->
    have_compatible_qualifiers_deep
      ~context:(qualifier_context_fun_ret context) tres1 tres2 &&
    let context = qualifier_context_fun_arg context in
    List.for_all2 (fun (_, t1', a1) (_, t2', a2) ->
        have_compatible_qualifiers_deep ~context t1' t2' &&
        included_qualifiers ~context a1 a2)
      args1 args2
  | TPtr t1', TPtr t2'
  | TArray (t1', _), TArray (t2', _) ->
    (included_qualifiers ~context t1.tattr t2.tattr) &&
    let context = qualifier_context_ptr context in
    have_compatible_qualifiers_deep ~context t1' t2'
  | _, _ ->
    included_qualifiers ~context
      (Ast_types.get_attributes t1)
      (Ast_types.get_attributes t2)


let rec is_nullptr e =
  match e.enode with
  | Const (CInt64 (i,_,_)) -> Z.is_zero i
  | CastE (t,e) when Ast_types.is_ptr t -> is_nullptr e
  | _ -> false

(* true if the expression is known to be a boolean result, i.e. 0 or 1. *)
let rec is_boolean_result e =
  (Ast_types.is_bool (typeOf e)) ||
  match e.enode with
  | Const _ ->
    (match isInteger e with
     | Some i -> Z.is_zero i || Z.is_one i
     | None -> false)
  | CastE (_, e) -> is_boolean_result e
  | BinOp ((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr), _, _, _) -> true
  | BinOp
      ((PlusA | PlusPI | MinusA | MinusPI | MinusPP | Mult
       | Div | Mod | Shiftlt | Shiftrt | BAnd | BXor | BOr), _, _, _) -> false
  | UnOp (LNot, _, _) -> true
  | UnOp ((Neg | BNot), _, _) -> false
  | Lval _ | SizeOf _ | SizeOfE _ | AlignOf _
  | AlignOfE _ | AddrOf _ | StartOf _ -> false


(** A hook into the code that creates casts.  By default this
    returns the new type.
    Casts in the source code are exempt from this hook. *)
let typeForInsertedCast:
  (Cil_types.exp -> Cil_types.typ -> Cil_types.typ -> Cil_types.typ) ref =
  ref (fun _ _ t -> t)


exception Cannot_combine of string

type combineWhat =
    CombineFundef of bool
  (* The new definition is for a function definition. The old
   * is for a prototype. arg is [true] for an old-style declaration *)
  | CombineFunarg of bool
  (* Comparing a function argument type with an old prototype argument.
     arg is [true] for an old-style declaration, which
     triggers some ad hoc treatment in GCC mode. *)
  | CombineFunret (* Comparing the return of a function with that from an old
                   * prototype *)
  | CombineOther

(* [combineAttributes what olda a] combines the attributes in [olda] and [a]
   according to [what]:
   - if [what == CombineFunarg], then override old attributes;
     this is used to ensure that attributes from formal argument types in a
     function definition are not mixed with attributes from arguments in other
     (compatible, but with different qualifiers) declarations;
   - else, perform the union of old and new attributes. *)
let combineAttributes what olda a =
  match what with
  | CombineFunarg _ -> a (* override old attributes with new ones *)
  | _ -> Ast_attributes.add_list olda a (* union of attributes *)

type combineFunction =
  {
    typ_combine : combineFunction ->
      strictInteger:bool -> strictReturnTypes:bool ->
      combineWhat -> typ -> typ -> typ;

    enum_combine : combineFunction ->
      enuminfo -> enuminfo -> enuminfo;

    comp_combine : combineFunction ->
      compinfo -> compinfo -> compinfo;

    name_combine : combineFunction -> combineWhat ->
      typeinfo -> typeinfo -> typeinfo;
  }

(* Combine the types. Raises the Cannot_combine exception with an error message.
   [what] is used to recursively deal with function return types and function
   arguments in special ways.
   Note: we cannot force the qualifiers of oldt and t to be the same here,
   because in some cases (e.g. string literals and char pointers) it is
   allowed to have differences, while in others we want to be more strict. *)
let combineTypesGen ?emitwith (combF : combineFunction)
    ~strictInteger ~strictReturnTypes
    (what : combineWhat) (oldt : typ) (t : typ) : typ =
  let warning = Kernel.warning ?emitwith in
  let add_attributes what attrs t =
    match what with
    | CombineFunarg _ -> t
    | _ -> Ast_types.add_attributes attrs t
  in
  let tattr = combineAttributes what oldt.tattr t.tattr in
  match oldt.tnode, t.tnode with
  | TVoid, TVoid ->
    Cil_const.mk_tvoid ~tattr ()

  | _, TVoid when what = CombineFunret && not strictReturnTypes -> t

  | TInt oldik, TInt ik ->
    let result k oldk = if rank oldk<rank k then k else oldk in
    let check_gcc_mode oldk k =
      if Machine.gccMode () && oldk == IInt &&
         bytesSizeOf t <= bytesSizeOfInt IInt &&
         (what = CombineFunarg true || what = CombineFunret)
      then k
      else
        let msg =
          Format.asprintf
            "different integer types:@ '%a' and '%a'"
            !pp_typ_ref oldt !pp_typ_ref t in
        raise (Cannot_combine msg)
    in
    let combineIK oldk k =
      if oldk == k then oldk else
      if not strictInteger
      then
        if sameSizeInt ~machdep:false oldk k && sameSign ~machdep:false oldk k
        then
          (* The types contain the same sort of values but are not equal.
             For example on x86_16 machdep unsigned short and unsigned int. *)
          result k oldk
        else
        if sameSizeInt ~machdep:true oldk k && sameSign ~machdep:true oldk k
        then
          begin
            warning
              ~wkey:Kernel.wkey_int_conversion
              ~current:true
              "Integer compatibility is machine-dependent: %a and %a\n"
              !pp_typ_ref oldt !pp_typ_ref t;
            result k oldk
          end
        else
          check_gcc_mode oldk k
      else
        check_gcc_mode oldk k
    in
    Cil_const.mk_tint ~tattr (combineIK oldik ik)

  | TFloat oldfk, TFloat fk ->
    let combineFK oldk k =
      if oldk == k then oldk else
      if Machine.gccMode () && oldk == FDouble && k == FFloat
         && (what = CombineFunarg true || what = CombineFunret)
      then k
      else
        raise (Cannot_combine "different floating point types")
    in
    Cil_const.mk_tfloat ~tattr (combineFK oldfk fk)

  | TEnum oldei, TEnum ei ->
    (* Matching enumerations always succeeds. But sometimes it maps both
     * enumerations to integers *)
    let ei' = combF.enum_combine combF oldei ei in
    Cil_const.mk_tenum ~tattr ei'

  (* Strange one. But seems to be handled by GCC *)
  | TEnum oldei , TInt IInt ->
    Cil_const.mk_tenum ~tattr oldei

  (* Strange one. But seems to be handled by GCC. Warning. Here we are
   * leaking types from new to old  *)
  | TInt IInt, TEnum ei ->
    Cil_const.mk_tenum ~tattr ei

  | TComp oldci , TComp ci ->
    let ci' = combF.comp_combine combF oldci ci in
    Cil_const.mk_tcomp ~tattr ci'

  | TArray (oldbt, oldsz), TArray (bt, sz) ->
    let newbt =
      combF.typ_combine combF
        ~strictInteger ~strictReturnTypes CombineOther oldbt bt
    in
    let newsz =
      match oldsz, sz with
      | None, Some _ -> sz
      | Some _, None -> oldsz
      | None, None -> sz
      | Some oldsz', Some sz' ->
        (* They are not structurally equal. But perhaps they are equal if we
           evaluate them. Check first machine independent comparison. *)
        let checkEqualSize (machdep: bool) =
          match constFoldToInt ~machdep oldsz', constFoldToInt ~machdep sz' with
          | Some m, Some n -> m = n
          | _ -> ExpStructEqSized.equal oldsz' sz'
        in
        if checkEqualSize false then
          oldsz
        else if checkEqualSize true then begin
          warning
            ~wkey:Kernel.wkey_int_conversion
            ~current:true
            "Array type comparison succeeds only based on machine-dependent \
             constant evaluation: %a and %a\n"
            !pp_typ_ref oldt !pp_typ_ref t;
          oldsz
        end else
          raise (Cannot_combine "different array lengths")
    in
    Cil_const.mk_tarray ~tattr newbt newsz

  | TPtr oldbt, TPtr bt ->
    let newbt =
      combF.typ_combine combF
        ~strictInteger ~strictReturnTypes CombineOther oldbt bt
    in
    Cil_const.mk_tptr ~tattr newbt

  | TFun (oldrt, oldargs, oldva), TFun (rt, args, va) ->
    let newrt =
      combF.typ_combine combF
        ~strictInteger ~strictReturnTypes CombineFunret oldrt rt
    in
    if oldva != va then
      raise (Cannot_combine "different vararg specifiers");
    (* If one does not have arguments, believe the one with the
     * arguments *)
    let newargs =
      if oldargs = None then args
      else if args = None then oldargs
      else
        let (oldargslist, oldghostargslist) = argsToPairOfLists oldargs in
        let (argslist, ghostargslist) = argsToPairOfLists args in
        if List.length oldargslist <> List.length argslist then
          raise (Cannot_combine "different number of arguments")
        else if List.length oldghostargslist <> List.length ghostargslist then
          raise (Cannot_combine "different number of ghost arguments")
        else
          let oldargslist = oldargslist @ oldghostargslist in
          let argslist = argslist @ ghostargslist in
          (* Go over the arguments and update the old ones with the
           * adjusted types *)
          (* Format.printf "new type is %a@." !pp_typ_ref t; *)
          let what =
            match what with
            | CombineFundef b -> CombineFunarg b
            | _ -> CombineOther
          in
          Some
            (List.map2
               (fun (on, ot, oa) (an, at, aa) ->
                  (* Update the names. Always prefer the new name. This is
                     very important if the prototype uses different names than
                     the function definition. *)
                  let n = if an <> "" then an else on in
                  let t =
                    combF.typ_combine combF
                      ~strictInteger ~strictReturnTypes what ot at
                  in
                  let a = Ast_attributes.add_list oa aa in
                  (n, t, a))
               oldargslist argslist)
    in
    (* Drop missingproto as soon as one of the type is a properly declared one*)
    let olda' =
      if not (Ast_attributes.(contains fc_missingproto t.tattr)) then
        Ast_attributes.(drop fc_missingproto oldt.tattr)
      else oldt.tattr
    in
    let a' =
      if not (Ast_attributes.(contains fc_missingproto oldt.tattr)) then
        Ast_attributes.(drop fc_missingproto t.tattr)
      else t.tattr
    in
    let tattr = combineAttributes what olda' a' in
    Cil_const.mk_tfun ~tattr newrt newargs oldva

  | TBuiltin_va_list, TBuiltin_va_list ->
    Cil_const.mk_tbuiltin ~tattr ()

  | TNamed oldti, TNamed ti ->
    let ti' = combF.name_combine combF what oldti ti in
    Cil_const.mk_tnamed ~tattr ti'

  | _, TNamed ti ->
    let res =
      combF.typ_combine combF
        ~strictInteger ~strictReturnTypes what oldt ti.ttype
    in
    add_attributes what t.tattr res

  | TNamed oldti, _ ->
    let res =
      combF.typ_combine combF
        ~strictInteger ~strictReturnTypes what oldti.ttype t
    in
    add_attributes what t.tattr res

  | _ ->
    raise
      (Cannot_combine
         (Format.asprintf "different type constructors:@ %a and %a"
            !pp_typ_ref oldt !pp_typ_ref t))


let default_combines = {
  typ_combine = (fun combF -> combineTypesGen combF);
  enum_combine = (fun _ _ ei -> ei);
  comp_combine = (fun _ oldci ci ->
      if oldci.cstruct <> ci.cstruct then
        raise (Cannot_combine "different struct/union types");
      if oldci.cname = ci.cname then
        oldci
      else
        raise (Cannot_combine
                 (Format.sprintf "%ss with different tags"
                    (if oldci.cstruct then "struct" else "union"))));
  name_combine = (fun c what oldt t ->
      if oldt.tname = t.tname then oldt
      else
        begin
          ignore (c.typ_combine c ~strictInteger:true ~strictReturnTypes:false
                    what oldt.ttype t.ttype);
          oldt
        end);
}


let combineTypes ?(strictInteger=true) ?(strictReturnTypes=false)
    what (oldt: typ) (t: typ) : typ =
  combineTypesGen default_combines
    ~strictInteger ~strictReturnTypes what oldt t

(***************** Compatibility ******)

let compatibleTypes ?strictReturnTypes ?context t1 t2 =
  let r = combineTypes ?strictReturnTypes CombineOther t1 t2 in
  (* C99, 6.7.3 §9: "... to be compatible, both shall have the identically
     qualified version of a compatible type;" *)
  if not (have_compatible_qualifiers_deep ?context t1 t2) then
    raise (Cannot_combine "different qualifiers");
  (* Note: different non-qualifier attributes will be silently dropped. *)
  r

let areCompatibleTypes ?strictReturnTypes ?context t1 t2 =
  try
    ignore (compatibleTypes ?strictReturnTypes ?context t1 t2); true
  with Cannot_combine _ -> false

(***************** Completeness ******)

(* NB: these functions depend on isZero, hence can't be moved to Ast_types
   without creating cyclic dependencies or ugly references.
*)

let rec has_flexible_array_member t =
  let is_flexible_array t =
    match Ast_types.unroll_skel t with
    | TArray (_, None) -> true
    | TArray (_, Some z) -> Machine.(msvcMode() || gccMode()) && isZero z
    | _ -> false
  in
  match Ast_types.unroll_skel t with
  | TComp { cfields = Some ((_::_) as l) } ->
    let last = (List.last l).ftype in
    is_flexible_array last ||
    (Machine.(gccMode() || msvcMode()) && has_flexible_array_member last)
  | _ -> false

(* last_field is [true] if the given type is the type of the last field of
   a struct (which could be a FAM, making the whole struct complete even if
   the array type isn't. *)
let rec isCompleteType ?(allowZeroSizeArrays=Machine.gccMode ())
    ?(last_field=false) t =
  match Ast_types.unroll_node t with
  | TVoid -> false (* void is an incomplete type by definition (6.2.5§19) *)
  | TArray(t, None) ->
    last_field && is_complete_agg_member ~allowZeroSizeArrays ~last_field  t
  | TArray(t, Some z) when isZero z ->
    allowZeroSizeArrays &&
    is_complete_agg_member ~allowZeroSizeArrays ~last_field t
  | TArray(t, Some _) ->
    is_complete_agg_member ~allowZeroSizeArrays ~last_field t
  | TComp { cfields = None } -> false
  | TComp { cstruct ; cfields = Some flds } -> (* Struct or union *)
    complete_type_fields ~allowZeroSizeArrays cstruct flds
  | TEnum {eitems = []} -> false
  | TEnum _ -> true
  | TInt _ | TFloat _ | TPtr _ | TBuiltin_va_list -> true
  | TFun _ -> true (* only object types can be incomplete (6.2.5§1) *)
  | TNamed _ -> assert false (* unroll should have removed it. *)

and complete_type_fields ~allowZeroSizeArrays is_struct fields =
  let rec aux is_first l =
    let last_field = is_struct && not is_first in
    match l with
    | [] -> true
    | [ f ] ->
      is_complete_agg_member ~allowZeroSizeArrays ~last_field f.ftype
    | f :: tl ->
      is_complete_agg_member ~allowZeroSizeArrays f.ftype && aux false tl
  in
  aux true fields

and is_complete_agg_member ~allowZeroSizeArrays ?last_field t =
  isCompleteType ~allowZeroSizeArrays ?last_field t &&
  (allowZeroSizeArrays || not (has_flexible_array_member t))

(******************** CASTING *****)

let checkCast ?context ?(nullptr_cast=false) ?(fromsource=false) =
  let dkey = Kernel.dkey_typing_cast in
  let origin = if fromsource then "explicit cast:" else " implicit cast:" in
  let error msg = Errorloc.abort_context ("%s " ^^ msg) origin in
  let rec default_rec oldt newt =
    let oldt' = Ast_types.unroll oldt in
    let newt' = Ast_types.unroll newt in
    match oldt'.tnode, newt'.tnode with
    | TNamed _, _
    | _, TNamed _ ->
      Kernel.fatal ~current:true "Ast_types.unroll failed in checkCast"
    | _, TInt IBool when Ast_types.is_scalar oldt' -> ()
    | TInt _, TInt _ -> ()
    | TFloat _, TInt _ -> (* ISO 6.3.1.4.1 *) ()
    | TInt _, TFloat _ -> (* ISO 6.3.1.4.2 *) ()
    | TFloat _, TFloat _ -> (* ISO 6.3.1.5.1 *) ()

    (* ISO 6.4.4.3 is only about enum constant but ccomp is more permissive. *)
    | TEnum _, (TInt _ | TFloat _ | TPtr _ | TEnum _)
    | TFloat _ , TEnum _ -> ()
    | TPtr _, TEnum _ ->
      Kernel.debug ~dkey ~current:true
        "Casting a pointer into an enumeration type"
    | TInt _, TEnum _ -> ()

    | _, TVoid ->
      (* ISO 6.3.2.2 *)
      Kernel.debug ~level:3
        "Casting a value into void: expr is evaluated for side effects"
    | TPtr t, TPtr { tnode = TVoid } when Ast_types.is_object t ->
      (* ISO 6.3.2.3.1 *) ()
    | TPtr { tnode = TVoid }, TPtr t when Ast_types.is_object t ->
      (* ISO 6.3.2.3.1 *) ()
    | TInt _, TPtr _ -> (* ISO 6.3.2.3.5 *) ()
    | TPtr _, TInt _ -> (* ISO 6.3.2.3.6 *)
      if not fromsource && newt != Machine.uintptr_type ()
      then
        Kernel.warning
          ~wkey:Kernel.wkey_int_conversion
          ~current:true
          "Conversion from a pointer to an integer without an explicit cast"
    | TPtr t1, TPtr t2 when Ast_types.is_object t1 && Ast_types.is_object t2 ->
      (* ISO 6.3.2.3.7 *) ()
    | TPtr t1, TPtr t2 when Ast_types.is_fun t1 && Ast_types.is_fun t2 ->
      (* ISO 6.3.2.3.8 *)
      if not (areCompatibleTypes ?context oldt newt)
      then
        Kernel.warning
          ~wkey:Kernel.wkey_incompatible_types_call
          ~current:true
          "implicit conversion between incompatible function types:@ \
           %a@ and@ %a"
          !pp_typ_ref oldt !pp_typ_ref newt

    (* accept converting a ptr to function to/from a ptr to void, even though
       not really accepted by the standard.
       Main compilers supports it. *)
    | TPtr { tnode = TFun _ }, TPtr { tnode = TVoid } -> ()
    | TPtr { tnode = TVoid } , TPtr { tnode = TFun _ } -> ()

    | TFun _, TPtr { tnode = TFun _ } -> (* ISO 6.3.2.1.4 *) ()

    | TArray (t1, _), TPtr t2 ->
      (* ISO 6.3.2.1.3 *)
      if not (areCompatibleTypes ?context t1 t2)
      then
        Kernel.warning
          ~wkey:Kernel.wkey_incompatible_types_call
          ~current:true
          "conversion between incompatible from array type to pointer type:@ \
           %a@ and@ %a"
          !pp_typ_ref oldt !pp_typ_ref newt

    | TArray (t1, _), TArray (t2, _) ->
      if not (areCompatibleTypes ?context t1 t2)
      then
        Kernel.warning
          ~wkey:Kernel.wkey_incompatible_types_call
          ~current:true
          "conversion between incompatible array types :@ \
           %a@ and@ %a"
          !pp_typ_ref oldt !pp_typ_ref newt

    (* pointer to potential function type. Note that we do not
       use unroll_typeDeep above in order to avoid needless divergence with
       original type in the sources.
    *)
    | TPtr { tnode = TFun _ }, TPtr { tnode = TNamed ti; tattr } ->
      let t' = Ast_types.add_attributes tattr ti.ttype in
      let t'' = Cil_const.mk_tptr ~tattr:newt'.tattr t' in
      default_rec t'' newt

    | TPtr { tnode = TNamed ti; tattr }, TPtr { tnode = TFun _ } ->
      let t' = Ast_types.add_attributes tattr ti.ttype in
      let t'' = Cil_const.mk_tptr ~tattr:oldt'.tattr t' in
      default_rec t'' newt

    | TFloat _, TPtr _
    | TPtr _, TFloat _ ->
      (* ISO 6.5.4.4 *)
      error "illegal conversion between float and pointers"

    (* No other conversion implying a pointer to function
          and a pointer to object are supported. *)
    | TPtr t1, TPtr t2 when Ast_types.is_fun t1 && Ast_types.is_object t2 ->
      if not nullptr_cast then
        Kernel.warning
          ~wkey:Kernel.wkey_incompatible_pointer_types
          ~current:true
          "casting function to %a" !pp_typ_ref newt
    | TPtr t1, TPtr t2 when Ast_types.is_fun t2 && Ast_types.is_object t1 ->
      if not nullptr_cast then
        Kernel.warning
          ~wkey:Kernel.wkey_incompatible_pointer_types
          ~current:true
          "casting function from %a" !pp_typ_ref oldt

    | _, TPtr t1 when Ast_types.is_fun t1 ->
      error "cannot cast %a to function type"
        !pp_typ_ref oldt

    | _, _ when Ast_types.is_arithmetic oldt' && Ast_types.is_arithmetic newt' ->
      (* ISO 6.5.16.1.1#1 *) ()


    | TComp _, TComp _ when areCompatibleTypes oldt newt ->
      (* ISO 6.5.16.1.1#2*) ()

    (* If we try to pass a transparent union value to a function
       expecting a transparent union argument, the argument type would
       have been changed to the type of the first argument, and we'll
       see a cast from a union to the type of the first argument. Turn
       that into a field access *)
    | TComp _, _ ->
      begin
        match Ast_types.is_transparent_union oldt with
        | None ->
          error "cast from %a to %a"
            !pp_typ_ref oldt !pp_typ_ref newt
        | Some _ -> ()
      end

    | TBuiltin_va_list, (TInt _ | TPtr _) -> ()

    | (TInt _ | TPtr _), TBuiltin_va_list ->
      Kernel.debug ~dkey ~current:true
        "Casting %a to __builtin_va_list" !pp_typ_ref oldt

    | _, _ when fromsource && not (Ast_types.is_scalar newt') ->
      (* ISO 6.5.4.2 *)
      error "cast over a non-scalar type %a" !pp_typ_ref newt

    | _ ->
      error "cannot cast from %a to %a@\n"
        !pp_typ_ref oldt !pp_typ_ref newt
  in default_rec

let rec castReduce fromsource force =
  let dkey = Kernel.dkey_typing_cast in
  let origin = if fromsource then "explicit cast:" else " implicit cast:" in
  let error msg = Errorloc.abort_context ("%s " ^^ msg) origin in
  let rec rec_default oldt newt e =
    let loc = e.eloc in
    let normalized_newt =
      Ast_types.(remove_attributes_for_c_cast (unroll newt))
    in
    let res e = new_exp ~loc (CastE (Ast_types.remove_qualifiers newt, e)) in
    let oldt' = Ast_types.unroll oldt in
    match oldt'.tnode, (normalized_newt).tnode, e.enode with
    (* In the case were we have a representation for the literal,
         explicitly add the cast. *)
    | _, TInt newik, Const (CInt64 (i, _, None)) ->
      (* ISO 6.3.1.3.2 *) kinteger64 ~loc ~kind:newik i

    | _, TPtr _, CastE (_, e') ->
      begin
        match Ast_types.unroll (typeOf e'), e'.enode with
        | { tnode = TPtr _ } as typ'', _ ->
          (* Old cast can be removed...*)
          if need_cast ~force newt typ'' then res e'
          else (* In fact, both casts can be removed. *) e'
        | _, Const (CInt64 (i, _, _)) when Z.is_zero i -> res e'
        | _ -> res e
      end

    | TInt ik, TEnum ei, Const (CEnum { eihost = ei'}) when
        ei.ename = ei'.ename && not fromsource &&
        bytesSizeOfInt ik = bytesSizeOfInt ei'.ekind -> e

    | TFun _, TPtr { tnode = TFun _ }, Lval lv -> mkAddrOf ~loc lv

    | _, TInt IBool, _ when Ast_types.is_scalar oldt' ->
      let cmp = expression_to_bool e in
      if Exp.equal e cmp then begin
        Kernel.debug ~dkey "Explicit cast to Boolean: %a" !pp_exp_ref e;
        res e
      end else begin
        Kernel.debug ~dkey
          "bool conversion by checking !=0: %a" !pp_exp_ref e;
        let oldt = typeOf cmp in
        rec_default oldt newt cmp
      end

    | TComp _, _, _ ->
      begin
        match Ast_types.is_transparent_union oldt with
        | None ->
          error "cast from %a to %a"
            !pp_typ_ref oldt !pp_typ_ref newt
        | Some fstfield ->
          (* We do it now only if the expression is an lval *)
          let e' =
            match e.enode with
            | Lval lv ->
              new_exp ~loc:e.eloc
                (Lval (addOffsetLval (Field (fstfield, NoOffset)) lv))
            | _ ->
              error "transparent union expression is not an lval: %a\n"
                !pp_exp_ref e

          in
          (* Continue casting *)
          rec_default fstfield.ftype newt e'
      end

    | _ -> res e
  in rec_default

and mkCastTGen ?(check=true) ?context ?(fromsource=false) ?(force=false)
    ~(oldt: typ) ~(newt: typ) e =
  let dkey = Kernel.dkey_typing_cast in
  let force_cast = fromsource || force in
  let need_cast = need_cast ~force:force_cast oldt newt in
  if not need_cast then begin
    Kernel.debug ~dkey "no cast to perform";
    let returned_type =
      match newt.tnode with
      | TNamed _ -> newt
      | _ -> oldt
    in
    (returned_type, e)
  end
  else
    let newt = if fromsource then newt else !typeForInsertedCast e oldt newt in
    let nullptr_cast = is_nullptr e in
    if check then checkCast ?context ~nullptr_cast ~fromsource oldt newt;
    (newt, castReduce fromsource force oldt newt e)

and mkCastT ?(check=true) ?(force=false) ~(oldt: typ) ~(newt: typ) e =
  snd (mkCastTGen ~check ~context:Identical ~force ~oldt ~newt e)

and mkCast ?(check=true) ?force ~(newt: typ) e =
  mkCastT ~check ?force ~oldt:(typeOf e) ~newt e

and mkBinOp ?(constfold=false) ~loc op e1 e2 =
  let open Ast_types in
  let open Result.Operators in
  let t1 = typeOf e1 in
  let t2 = typeOf e2 in
  let machdep = true in
  let promote_cast t e =
    let t' = integralPromotion t in
    t', mkCastT ~oldt:t ~newt:t' e
  in
  let make_expr t e1 e2 =
    if constfold then
      Ok (constFoldBinOp ~loc machdep op e1 e2 t)
    else
      Ok (new_exp ~loc (BinOp(op, e1, e2, t)))
  in
  (* Error handling *)
  let error ?loc msg =
    Format.kasprintf (fun str -> Result.error (loc,str)) msg
  in
  let error_one ~loc t reason =
    error ~loc "operand of operator '%a' with %s type '%a'"
      !pp_binop_ref op reason !pp_typ_ref t
  in
  let error_both reason =
    error "operator '%a' on %s types '%a' and '%a'"
      !pp_binop_ref op reason !pp_typ_ref t1 !pp_typ_ref t2
  in
  let error_any c1 c2 reason =
    if c1 && c2 then error_both reason
    else if c1 then error_one ~loc:e1.eloc t1 reason
    else error_one ~loc:e2.eloc t2 reason
  in
  (* checks *)
  let check_left ~check reason =
    if check t1 then Ok () else error_one ~loc:e1.eloc t1 reason
  in
  let check_right ~check reason =
    if check t2 then Ok () else error_one ~loc:e2.eloc t2 reason
  in
  let check_both ~check reason =
    let c1 = check t1 and c2 = check t2 in
    if c1 && c2 then Ok ()
    else error_any (not c1) (not c2) reason
  in
  let check_not_function_ptr ~side_check =
    (* Cf. GNU C 6.13.2: GCC allows arithmetic on function pointers. Allow
       pointers arithmetic on Machdeps which define sizeof_fun. *)
    if Machine.Sizeof.func () > 0 then Ok ()
    else side_check ~check:is_object_ptr "non-object (function) pointer"
  in
  let check_complete_pointed ~side_check =
    let is_complete t = isCompleteType (direct_pointed_type t) in
    let check_complete () = side_check ~check:is_complete "incomplete pointer" in
    (* Cf. 6.13.2: GCC allows arithmetic on void pointers. *)
    if Machine.gccMode () then
      match side_check ~check:is_void_ptr "non-void pointer" with
      | Ok () -> Ok ()
      | Error _ -> check_complete ()
    else check_complete ()
  in
  let check_pointer_offset () =
    let* () = check_not_function_ptr ~side_check:check_left in
    let* () = check_complete_pointed ~side_check:check_left in
    check_right ~check:is_integral "non-integral"
  in
  let arith_conv ~tres =
    let t = arithmeticConversion t1 t2 in
    let tres = Option.value ~default:t tres in
    make_expr tres
      (mkCastT ~oldt:t1 ~newt:t e1)
      (mkCastT ~oldt:t2 ~newt:t e2)
  in
  let do_arithmetic ~tres =
    let* () = check_both ~check:is_arithmetic "non-arithmetic" in
    arith_conv ~tres
  in
  let do_integral_arithmetic () =
    let* () = check_both ~check:is_integral "non-integral" in
    arith_conv ~tres:None
  in
  let compatible_pointed_types () =
    let t1p = direct_pointed_type t1
    and t2p = direct_pointed_type t2 in
    areCompatibleTypes
      (remove_qualifiers_deep t1p)
      (remove_qualifiers_deep t2p)
  in
  let do_compare () =
    (* We are more lenient than the ISO C here, if two pointers do not
       point to compatible types, we cast them to a common type. *)
    let e1, e2 =
      if not (need_cast ~force:true t1 t2) then
        e1, e2
      else if compatible_pointed_types () then
        e1, mkCastT ~oldt:t2 ~newt:t1 e2
      else if is_object_ptr t1 && is_object_ptr t2 then
        (* Only object types can safely be casted to void*. *)
        mkCastT ~oldt:t1 ~newt:Cil_const.voidPtrType e1,
        mkCastT ~oldt:t2 ~newt:Cil_const.voidPtrType e2
      else
        mkCastT ~oldt:t1 ~newt:(Machine.uintptr_type ()) e1,
        mkCastT ~oldt:t2 ~newt:(Machine.uintptr_type ()) e2
    in
    make_expr Cil_const.intType e1 e2
  in
  match op with
  | Mult | Div -> do_arithmetic ~tres:None
  | Mod  | BAnd | BOr | BXor -> do_integral_arithmetic ()
  | LAnd | LOr ->
    let* () = check_both ~check:is_scalar "non-scalar" in
    make_expr Cil_const.intType
      (expression_to_bool e1)
      (expression_to_bool e2)
  | Shiftlt | Shiftrt -> (* ISO 6.5.7. Only integral promotions. The result
                          * has the same type as the left hand side *)
    if Machine.msvcMode () then
      (* MSVC has a bug. We duplicate it here *)
      do_integral_arithmetic ()
    else
      let* () = check_both ~check:is_integral "non-integral" in
      let t1', e1' = promote_cast t1 e1
      and _  , e2' = promote_cast t2 e2 in
      make_expr t1' e1' e2'
  | PlusA | MinusA -> do_arithmetic ~tres:None
  | PlusPI ->
    let* () = check_pointer_offset () in
    begin match e1.enode with
      | StartOf lv ->
        Ok { e1 with enode = AddrOf (addOffsetLval (Index (e2,NoOffset)) lv) }
      | _ -> make_expr t1 e1 (snd @@ promote_cast t2 e2)
    end
  | MinusPI ->
    let* () = check_pointer_offset () in
    make_expr t1 e1 (snd @@ promote_cast t2 e2)
  | MinusPP ->
    (* ISO C11 6.5.6§3 and 6.5.6§9 : Both types should be pointers to complete
       and compatible object types and the result is of type ptrdiff_t. *)
    let* () = check_not_function_ptr ~side_check:check_both in
    let* () = check_complete_pointed ~side_check:check_both in
    if compatible_pointed_types () then
      make_expr (Machine.ptrdiff_type ()) e1 (mkCastT ~oldt:t2 ~newt:t1 e2)
    else error_both "incompatible"
  | Eq | Ne when (is_ptr t1 || is_variadic_list t1) && isZero e2 ->
    make_expr Cil_const.intType e1 (mkCast ~newt:t1 e2)
  | Eq | Ne when (is_ptr t2 || is_variadic_list t2) && isZero e1 ->
    make_expr Cil_const.intType (mkCast ~newt:t2 e1) e2
  | Lt | Le | Ge | Gt when is_fun_ptr t1 || is_fun_ptr t2 ->
    (* ISO 6.5.8§2: only pointers to object types are allowed. *)
    error_any (is_fun_ptr t1) (is_fun_ptr t2) "non-object (function) pointer"
  | Eq | Ne | Lt | Le | Ge | Gt when is_ptr t1 && is_ptr t2 ->
    do_compare ()
  | Eq | Ne | Lt | Le | Ge | Gt when is_arithmetic t1 && is_arithmetic t2 ->
    arith_conv ~tres:(Some Cil_const.intType)
  | Eq | Ne | Lt | Le | Ge | Gt ->
    error "operator '%a' on types '%a' and '%a', both should be either \
           arithmetic or pointer types"
      !pp_binop_ref op !pp_typ_ref t1 !pp_typ_ref t2

and mkBinOp_exn ?constfold ~loc op e1 e2 =
  match mkBinOp ?constfold ~loc op e1 e2 with
  | Ok e -> e
  | Error (loc, msg) ->
    let current = Option.is_none loc in
    let source = Option.map fst loc in
    Kernel.fatal ~current ?source "Cil.mkBinOp: typing expression '%a' failed: %s"
      !pp_exp_ref (dummy_exp(BinOp(op, e1, e2, Cil_const.intType))) msg

and expression_to_bool e =
  if is_boolean_result e then e
  else begin
    let loc = e.eloc in
    let zero =
      match Ast_types.unroll_node (typeOf e) with
      | TInt ik -> kinteger ~loc ik 0
      | TEnum ei -> kinteger ~loc ei.ekind 0
      | TFloat fk -> kfloat ~loc fk 0.0
      | TPtr _ -> kinteger ~loc (Machine.uintptr_kind()) 0
      | _ ->
        Kernel.fatal "expression_to_bool: called on non-scalar expression '%a'"
          !pp_exp_ref e
    in
    mkBinOp_exn ~loc Ne e zero
  end

type existsAction =
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are
                                         * looking for but maybe its
                                         * successors are *)
let existsType (f: typ -> existsAction) (t: typ) : bool =
  let memo : (int, unit) Hashtbl.t = Hashtbl.create 17 in  (* Memo table *)
  let rec loop t =
    match f t with
      ExistsTrue -> true
    | ExistsFalse -> false
    | ExistsMaybe ->
      (match t.tnode with
       | TNamed t' -> loop t'.ttype
       | TComp c   -> loopComp c
       | TArray (t', _) -> loop t'
       | TPtr t' -> loop t'
       | TFun (rt, args, _) ->
         (loop rt || List.exists (fun (_, at, _) -> loop at)
            (argsToList args))
       | _ -> false)
  and loopComp c =
    if Hashtbl.mem memo c.ckey then
      (* We are looping, the answer must be false *)
      false
    else begin
      Hashtbl.add memo c.ckey ();
      List.exists (fun f -> loop f.ftype) (Option.value ~default:[] c.cfields)
    end
  in
  loop t


(* Try to do an increment, with constant folding *)
let increm (e: exp) (i: int) =
  let e' = constFold false e in
  let et = typeOf e' in
  let bop = if Ast_types.is_ptr et then PlusPI else PlusA in
  let i = match et.tnode with
    | TInt k | TEnum {ekind = k } -> kinteger k ~loc:e.eloc i
    | _ -> integer ~loc:e.eloc i
  in
  constFoldBinOp ~loc:e.eloc false bop e' i et

(* Try to do an increment, with constant folding *)
let increm64 (e: exp) i =
  let et = typeOf e in
  let bop = if Ast_types.is_ptr et then PlusPI else PlusA in
  constFold
    false
    (new_exp ~loc:e.eloc (BinOp(bop, e, kinteger64 ~loc:e.eloc i, et)))

type incorrect_array_length = Not_constant | Not_integer | Negative | Too_big

let pp_incorrect_array_length fmt = function
  | Not_constant -> Format.pp_print_string fmt "not a compile-time constant"
  | Negative -> Format.pp_print_string fmt "negative"
  | Not_integer -> Format.pp_print_string fmt "not an integral constant"
  | Too_big -> Format.pp_print_string fmt "too big"

exception LenOfArray of incorrect_array_length

let lenOfArray64 eo =
  match eo with
    None -> raise (LenOfArray Not_constant)
  | Some e -> begin
      match (constFold true e).enode with
      | Const(CInt64(ni, _, _)) when Z.geq ni Z.zero ->
        ni
      | Const(CInt64 _) -> raise (LenOfArray Negative)
      | Const _ -> raise (LenOfArray Not_integer)
      | _ -> raise (LenOfArray Not_constant)
    end
let lenOfArray eo =
  match Z.to_int_opt (lenOfArray64 eo) with
  | None -> raise (LenOfArray Too_big)
  | Some l -> l

(*** Make an initializer for zeroing a data type ***)
let rec makeZeroInit ~loc (t: typ) : init =
  let t' = Ast_types.unroll t in
  match t'.tnode with
  | TInt ik ->
    SingleInit (new_exp ~loc (Const(CInt64(Z.zero, ik, None))))
  | TFloat fk -> SingleInit(new_exp ~loc (Const(CReal(0.0, fk, None))))
  | TEnum _ -> SingleInit (zero ~loc)
  | TComp comp when comp.cstruct ->
    let inits =
      List.fold_right
        (fun f acc ->
           if f.fname <> missingFieldName then
             (Field(f, NoOffset), makeZeroInit ~loc f.ftype) :: acc
           else
             acc)
        (Option.value ~default:[] comp.cfields) []
    in
    CompoundInit (t', inits)
  | TComp comp when not comp.cstruct ->
    (match comp.cfields with
     | Some [] -> CompoundInit(t, []) (* tolerate empty initialization. *)
     | Some (f :: _rest) ->
       (* ISO C99 [6.7.8.10] says that the first field of the union
          is the one we should initialize. *)
       CompoundInit(t, [(Field(f, NoOffset), makeZeroInit ~loc f.ftype)])
     | None ->
       Kernel.fatal "Initialization of incomplete struct")
  | TArray (bt, Some len) ->
    let n =
      match constFoldToInt len with
      | Some n -> Z.to_int n
      | _ -> Kernel.fatal ~current:true "Cannot understand length of array"
    in
    let initbt = makeZeroInit ~loc bt in
    let rec loopElems acc i =
      if i < 0 then acc
      else loopElems ((Index(integer ~loc i, NoOffset), initbt) :: acc) (i - 1)
    in
    CompoundInit(t', loopElems [] (n - 1))

  | TArray (_bt, None) ->
    (* Unsized array, allow it and fill it in later
     * (see cabs2cil.ml, collectInitializer) *)
    CompoundInit (t', [])

  | TPtr _ ->
    SingleInit(
      if (Machine.insert_implicit_casts ()) then mkCast ~newt:t' (zero ~loc)
      else zero ~loc)
  | _ -> Kernel.fatal ~current:true "Cannot initialize type: %a" !pp_typ_ref t'

(** Fold over the list of initializers in a Compound (not also the nested
 * ones). [doinit] is called on every present initializer, even if it is of
 * compound type. The parameters of [doinit] are: the offset in the compound
 * (this is [Field(f,NoOffset)] or [Index(i,NoOffset)]), the initializer
 * value, expected type of the initializer value, accumulator. In the case of
 * arrays there might be missing zero-initializers at the end of the list.
 * These are scanned only if [implicit] is true. This is much like
 * [List.fold_left] except we also pass the type of the initializer. *)
let foldLeftCompound
    ~(implicit: bool)
    ~(doinit: offset -> init -> typ -> 'a -> 'a)
    ~(ct: typ)
    ~(initl: (offset * init) list)
    ~(acc: 'a) : 'a =
  match Ast_types.unroll_node ct with
  | TArray (bt, leno) -> begin
      let default () =
        (* iter over the supplied initializers *)
        List.fold_left (fun acc (o, i) -> doinit o i bt acc) acc initl
      in
      if implicit then
        match leno with
        | Some lene -> begin
            match constFoldToInt lene with
            | Some i ->
              let len_array = Z.to_int i in
              let len_init = List.length initl in
              if len_array <= len_init then
                default () (* enough elements in the initializers list *)
              else
                (* Some initializers are missing. Iterate over all the indexes in
                   the array, and use either the supplied initializer, or a generic
                   zero one.  *)
                let loc = Current_loc.get () in
                let zinit = makeZeroInit ~loc bt in
                let acc = ref acc in
                let initl = ref initl in
                (* Is [off] the offset for the index [i] we are currently at.
                   Works because [initl] is sorted by Cabs2cil.*)
                let good_offset i off = match off with
                  | Index (i', NoOffset) ->
                    Z.(equal (Option.get (constFoldToInt i')) (of_int i))
                  | _ -> Kernel.fatal ~current:true
                           "Invalid initializer"
                in
                for i = 0 to len_array - 1 do
                  match !initl with
                  | (off, init) :: qinitl when good_offset i off->
                    acc := doinit off init bt !acc;
                    initl := qinitl
                  | _ ->
                    acc := doinit (Index(integer ~loc i, NoOffset)) zinit bt !acc
                done;
                assert (!initl = []);
                !acc
            | _ -> Kernel.fatal ~current:true
                     "foldLeftCompoundAll: array with initializer and non-constant length"
          end
        | _ -> Kernel.fatal ~current:true
                 "foldLeftCompoundAll: TArray with initializer and no length"
      else default ()
    end

  | TComp _comp ->
    let getTypeOffset = function
        Field(f, NoOffset) -> f.ftype
      | _ -> Kernel.fatal ~current:true "foldLeftCompound: malformed initializer"
    in
    List.fold_left
      (fun acc (o, i) -> doinit o i (getTypeOffset o) acc) acc initl

  | _ -> Kernel.fatal ~current:true "Type of Compound is not array or struct or union"

let pointer_decay t =
  let t' = Ast_types.unroll t in
  match t'.tnode with
  | TArray (typ, _) -> Cil_const.mk_tptr typ
  | TFun _ ->  Cil_const.mk_tptr t'
  | _ -> t'

(* C11 6.3.2.1:  If the lvalue has qualified type, the value has the
   unqualified version of the type of the lvalue; additionally, if the lvalue
   has atomic type, the value has the non-atomic version of the type of the
   lvalue; otherwise, the value has the type of the lvalue. If the lvalue has
   an incomplete type and does not have array type, the behavior is undefined.
*)
let lvalue_conversion (t : typ) : (typ, string) result =
  if not (isCompleteType t) && not (Ast_types.is_array t) then
    Error (Format.asprintf
             "Invalid lvalue conversion of incomplete non-array type %a"
             !pp_typ_ref t)
  else
    let t' = pointer_decay t in
    (* NOTE: remove atomicity when it will be supported by Frama-C.
             also, note that currently 'ghost' is removed. *)
    Ok (Ast_types.remove_qualifiers_deep t')

let rec is_variably_modified_type (t : typ) : bool =
  match Ast_types.unroll_node t with
  | TArray(t', osize) -> begin
      match osize with
      | None -> is_variably_modified_type t'
      | Some s ->
        if not (isConstant s) then true
        else is_variably_modified_type t'
    end
  | TComp _ ->
    (* GCC supports VLA fields as an extension; if we ever support it,
       add extra code here to take them into account *)
    false
  | TVoid | TInt _ | TEnum _ | TFloat _
  | TPtr _ | TFun _ | TNamed _ | TBuiltin_va_list -> false

let is_mutable (lhost, offset) =
  let rec aux can_mutate typ off =
    let can_mutate = can_mutate && not (Ast_types.is_const typ) in
    let typ' = Ast_types.unroll typ in
    match typ'.tnode, off with
    | _, NoOffset -> can_mutate
    | _, Field (fi, off) ->
      let can_mutate =
        can_mutate || Ast_attributes.(contains frama_c_mutable fi.fattr)
      in
      aux can_mutate fi.ftype off
    | TArray (typ, _), Index(_, off) -> aux can_mutate typ off
    | _, Index _ ->
      Kernel.fatal "Index on a non-array type '%a'" !pp_typ_ref typ'
  in
  aux false (typeOfLhost lhost) offset

let rec is_initialized_aux on_same_obj e =
  match e.enode with
  | Lval (lh, _) | AddrOf (lh, _) | StartOf (lh, _) ->
    is_initialized_lhost on_same_obj lh
  | BinOp ((PlusPI|MinusPI), e, _, _) | CastE (_, e) ->
    is_initialized_aux on_same_obj e
  | _ -> false

and is_initialized_lhost on_same_obj lhost =
  match lhost with
  | Var vi -> Ast_attributes.(contains frama_c_init_obj vi.vattr)
  | Mem e -> on_same_obj && is_initialized_aux false e

let is_initialized e =
  is_initialized_aux true e

let is_mutable_or_initialized lval =
  is_mutable lval || is_initialized_lhost true (fst lval)

let is_modifiable_lval lv =
  let t = typeOfLval lv in
  match Ast_types.unroll_skel t with
  | TArray _ -> false
  | TFun _ -> false
  | _ ->
    (not (Ast_types.is_const t) || is_mutable_or_initialized lv)
    && isCompleteType t

(** Uniquefy the variable names *)
let uniqueVarNames (f: file) : unit =
  (* Setup the alpha conversion table for globals *)
  let gAlphaTable = Hashtbl.create 113 in
  (* Keep also track of the global names that we have used. Map them to the
     variable ID. We do this only to check that we do not have two globals
     with the same name. *)
  let globalNames: (string, int) Hashtbl.t = Hashtbl.create 113 in
  (* Scan the file and add the global names to the table *)
  iterGlobals f
    (function
      | GVarDecl(vi, _) | GVar(vi, _, _)
      | GFunDecl(_, vi, _) | GFun({svar = vi}, _) ->
        (* See if we have used this name already for something else *)
        (try
           let oldid = Hashtbl.find globalNames vi.vname in
           if oldid <> vi.vid && not vi.vinline then
             Kernel.warning
               "The name %s is used for two distinct globals" vi.vname
         (* Here if we have used this name already. Go ahead *)
         with Not_found -> begin
             (* Here if this is the first time we define a name *)
             Hashtbl.add globalNames vi.vname vi.vid;
             (* And register it *)
             Alpha.registerAlphaName ~alphaTable:gAlphaTable
               ~lookupname:vi.vname ~data:(Current_loc.get ())
           end)
      | _ -> ());

  (* Now we must scan the function bodies and rename the locals *)
  iterGlobals f
    (function
        GFun(fdec, _) -> begin
          (* Setup an undo list to be able to revert the changes to the
           * global alpha table *)
          let undolist = ref [] in
          (* Process one local variable *)
          let processLocal (v: varinfo) =
            (* start from original name to avoid putting another _0 in case
               of conflicts. *)
            let lookupname =
              if v.vorig_name = "" then v.vname else v.vorig_name
            in
            let data = Current_loc.get () in
            let newname, oldloc =
              Alpha.newAlphaName
                ~alphaTable:gAlphaTable ~undolist:(Some undolist)
                ~lookupname ~data
            in
            if false && newname <> v.vname then (* Disable this warning *)
              Kernel.warning
                "Changing the name of local %s in %s to %s \
                 (due to duplicate at %a)"
                v.vname
                fdec.svar.vname
                newname Fileloc.pretty oldloc ;
            v.vname <- newname
          in
          let ghost vi = vi.vghost in
          let formals_ghost, formals = List.partition ghost fdec.sformals in
          let locals_ghost, locals = List.partition ghost fdec.slocals in
          List.iter processLocal formals;
          List.iter processLocal locals;
          List.iter processLocal formals_ghost;
          List.iter processLocal locals_ghost;
          (* Fix the type again *)
          setFormals fdec fdec.sformals;
          (* Undo the changes to the global table *)
          Alpha.undoAlphaChanges ~alphaTable:gAlphaTable ~undolist:!undolist;
          ()
        end
      | _ -> ());
  ()

let is_case_label l = match l with
  | Case _ | Default _ -> true
  | _ -> false

(* We want to bring all type declarations before the data declarations. This
 * is needed for code of the following form:

   int f(); // Prototype without arguments
   typedef int FOO;
   int f(FOO x) { ... }

   In CIL the prototype also lists the type of the argument as being FOO,
   which is undefined.

   There is one catch with this scheme. If the type contains an array whose
   length refers to variables then those variables must be declared before
   the type *)

let pullTypesForward = true


(* Scan a type and collect the variables that are referred *)
class getVarsInGlobalClass (pacc: varinfo list ref) = object
  inherit nopCilVisitor
  method! vvrbl (vi: varinfo) =
    pacc := vi :: !pacc;
    SkipChildren

  method! vglob = function
      GType _ | GCompTag _ -> DoChildren
    | _ -> SkipChildren

end

let getVarsInGlobal (g : global) : varinfo list =
  let pacc : varinfo list ref = ref [] in
  let v : cilVisitor = new getVarsInGlobalClass pacc in
  ignore (visitCilGlobal v g);
  !pacc

let pushGlobal (g: global)
    ~(types:global list ref)
    ~(variables: global list ref) =
  if not pullTypesForward then
    variables := g :: !variables
  else
    begin
      (* Collect a list of variables that are referred from the type. Return
       * Some if the global should go with the types and None if it should go
       * to the variables. *)
      let varsintype : (varinfo list * location) option =
        match g with
          GType (_, l) | GCompTag (_, l) -> Some (getVarsInGlobal g, l)
        | GEnumTag (_, l) | GPragma (("pack", _), l)
        | GCompTagDecl (_, l) | GEnumTagDecl (_, l) -> Some ([], l)
        (* Move the warning pragmas early
            | GPragma(Attr(s, _), l) when hasPrefix "warning" s -> Some ([], l)
        *)
        | _ -> None (* Does not go with the types *)
      in
      match varsintype with
        None -> variables := g :: !variables
      | Some (vl, loc) ->
        types :=
          (* insert declarations for referred variables ('vl'), before
           * the type definition 'g' itself *)
          let aux acc v =
            if Ast_types.is_fun v.vtype
            then GFunDecl (empty_funspec (),v, loc) :: acc
            else begin
              let is_same_decl = function
                | GVarDecl(v',_) -> Cil_datatype.Varinfo.equal v v'
                | _ -> false
              in
              if List.exists is_same_decl !types then acc
              else begin
                variables :=
                  List.filter (fun x -> not (is_same_decl x)) !variables;
                GVarDecl (v, loc) :: acc
              end
            end
          in
          g :: (List.fold_left aux !types vl)
    end

let make_temp_logic_var =
  let counter = ref 0 in
  fun ty ->
    incr counter;
    let name = "__framac_tmp" ^ (string_of_int !counter) in
    Cil_const.make_logic_var_local name ty

let extract_varinfos_from_exp vexp =
  let visitor = object
    inherit nopCilVisitor
    val mutable varinfos = Varinfo.Set.empty;
    method varinfos = varinfos
    method! vvrbl (symb:varinfo) =
      varinfos <- Varinfo.Set.add symb varinfos;
      SkipChildren
  end
  in ignore (visitCilExpr (visitor :> nopCilVisitor) vexp) ;
  visitor#varinfos

let extract_varinfos_from_lval vlval =
  let visitor = object
    inherit nopCilVisitor
    val mutable varinfos = Varinfo.Set.empty;
    method varinfos = varinfos
    method! vvrbl (symb:varinfo) =
      varinfos <- Varinfo.Set.add symb varinfos;
      SkipChildren
  end
  in ignore (visitCilLval (visitor :> nopCilVisitor) vlval) ;
  visitor#varinfos

let rec free_vars_term bound_vars t = match t.term_node with
  | TConst _   | TSizeOf _
  | TAlignOf _
  | Tnull
  | Ttype _ -> Logic_var.Set.empty
  | TLval lv
  | TAddrOf lv
  | TStartOf lv -> free_vars_lval bound_vars lv
  | TSizeOfE t
  | TAlignOfE t
  | TUnOp (_,t)
  | TCast (_,_,t)
  | Tat (t,_)
  | Toffset (_,t)
  | Tbase_addr (_,t)
  | Tblock_length (_,t)
  | Ttypeof t -> free_vars_term bound_vars t
  | TBinOp (_,t1,t2) ->
    Logic_var.Set.union
      (free_vars_term bound_vars t1)
      (free_vars_term bound_vars t2)
  | TUpdate (t1,toff,t2) ->
    Logic_var.Set.union
      (Logic_var.Set.union
         (free_vars_term bound_vars t1)
         (free_vars_term_offset bound_vars toff))
      (free_vars_term bound_vars t2)
  | Tif (cond,t1,t2) ->
    Logic_var.Set.union
      (free_vars_predicate bound_vars cond)
      (Logic_var.Set.union
         (free_vars_term bound_vars t1)
         (free_vars_term bound_vars t2))
  | TDataCons(_,t) | Tapp (_,_,t) ->
    List.fold_left
      (fun acc t ->
         Logic_var.Set.union (free_vars_term bound_vars t) acc)
      Logic_var.Set.empty t
  | Tlambda(prms,expr) ->
    let bound_vars =
      List.fold_left (Fun.flip Logic_var.Set.add) bound_vars prms
    in
    free_vars_term bound_vars expr
  | Trange(i1,i2) ->
    let fv = match i1 with
      | None -> Logic_var.Set.empty
      | Some i -> free_vars_term bound_vars i
    in
    (match i2 with
     | None -> fv
     | Some i ->
       Logic_var.Set.union fv (free_vars_term bound_vars i))
  | Tempty_set -> Logic_var.Set.empty
  | Tunion l | Tinter l ->
    List.fold_left
      (fun acc t ->
         Logic_var.Set.union (free_vars_term bound_vars t) acc)
      Logic_var.Set.empty
      l
  | Tcomprehension(t,q,p) ->
    let new_bv =
      List.fold_left
        (fun acc v -> Logic_var.Set.add v acc) bound_vars q
    in
    let fv = free_vars_term new_bv t in
    (match p with
     | None -> fv
     | Some p ->
       Logic_var.Set.union fv (free_vars_predicate new_bv p))
  | Tlet(d,b) ->
    let fvd =
      match d.l_body with
      | LBterm term -> free_vars_term bound_vars term
      | LBpred p -> free_vars_predicate bound_vars p
      | LBnone
      | LBreads _ | LBinductive _ ->
        Kernel.fatal ~current:true
          "definition of local variable %s is not a term or a predicate"
          d.l_var_info.lv_name
    in
    let fvb =
      free_vars_term (Logic_var.Set.add d.l_var_info bound_vars) b
    in
    Logic_var.Set.union fvd fvb

and free_vars_lval bv (h,o) =
  Logic_var.Set.union
    (free_vars_lhost bv h) (free_vars_term_offset bv o)

and free_vars_lhost bv = function
  | TVar log_v ->
    if Logic_var.Set.mem log_v bv then
      Logic_var.Set.empty
    else
      Logic_var.Set.singleton log_v
  | TResult _ -> Logic_var.Set.empty
  | TMem t -> free_vars_term bv t

and free_vars_term_offset bv = function
  | TNoOffset -> Logic_var.Set.empty
  | TField (_,o) | TModel(_,o) -> free_vars_term_offset bv o
  | TIndex (t,o) ->
    Logic_var.Set.union
      (free_vars_term bv t)
      (free_vars_term_offset bv o)

and free_vars_predicate bound_vars p = match p.pred_content with
  | Pfalse | Ptrue -> Logic_var.Set.empty
  | Papp (_,_,tl) ->
    List.fold_left
      (fun acc t ->
         Logic_var.Set.union (free_vars_term bound_vars t) acc)
      Logic_var.Set.empty tl
  | Pallocable (_,t) | Pfreeable (_,t)
  | Pvalid (_,t) | Pvalid_read (_,t) | Pobject_pointer (_, t) | Pvalid_function t
  | Pinitialized (_,t) | Pdangling (_,t) ->
    free_vars_term bound_vars t
  | Paligned(t, n) ->
    Logic_var.Set.union
      (free_vars_term bound_vars t)
      (free_vars_term bound_vars n)
  | Pseparated seps ->
    List.fold_left
      (fun free_vars tset ->
         Logic_var.Set.union
           (free_vars_term bound_vars tset) free_vars)
      Logic_var.Set.empty
      seps
  | Pfresh (_,_,t1,t2)
  | Prel (_,t1,t2)
    ->
    Logic_var.Set.union
      (free_vars_term bound_vars t1)
      (free_vars_term bound_vars t2)
  | Pand (p1,p2)
  | Por (p1,p2)
  | Pxor (p1,p2)
  | Pimplies (p1,p2)
  | Piff (p1,p2) ->
    Logic_var.Set.union
      (free_vars_predicate bound_vars p1)
      (free_vars_predicate bound_vars p2)
  | Pnot p
  | Pat (p,_)
    (*  | Pnamed (_,p) *) ->
    free_vars_predicate bound_vars p
  | Pif (c,p1,p2) ->
    Logic_var.Set.union
      (free_vars_predicate bound_vars c)
      (Logic_var.Set.union
         (free_vars_predicate bound_vars p1)
         (free_vars_predicate bound_vars p2))
  | Plet (d, p) ->
    let fvd =
      match d.l_body with
      | LBterm t -> free_vars_term bound_vars t
      | LBpred p -> free_vars_predicate bound_vars p
      | LBnone
      | LBreads _ | LBinductive _ ->
        Kernel.fatal ~current:true
          "Local logic var %s is not a defined term or predicate"
          d.l_var_info.lv_name
    in
    let new_bv = Logic_var.Set.add d.l_var_info bound_vars in
    Logic_var.Set.union fvd (free_vars_predicate new_bv p)

  | Pforall (lvs,p) | Pexists (lvs,p) ->
    let new_bv =
      List.fold_left
        (Fun.flip Logic_var.Set.add) bound_vars lvs
    in
    free_vars_predicate new_bv p

let extract_free_logicvars_from_term t =
  free_vars_term Logic_var.Set.empty t

let extract_free_logicvars_from_predicate p =
  free_vars_predicate Logic_var.Set.empty p

let extract_applied_logic_infos_from_predicate p =
  let visitor = object
    inherit nopCilVisitor
    val mutable logic_infos = Logic_info.Set.empty;
    method logic_infos = logic_infos
    method! vpredicate_node = function
      | Papp (li, _, _) ->
        logic_infos <- Logic_info.Set.add li logic_infos;
        DoChildren
      | _ -> DoChildren
    method! vterm_node = function
      | Tapp (li, _, _) ->
        logic_infos <- Logic_info.Set.add li logic_infos;
        DoChildren
      | _ -> DoChildren
  end in
  ignore @@ visitCilPredicate (visitor :> nopCilVisitor) p;
  visitor#logic_infos

class extract_labels = object
  inherit nopCilVisitor
  val mutable labels = Logic_label.Set.empty;
  method labels = labels
  method! vlogic_label label =
    labels <- Logic_label.Set.add label labels;
    SkipChildren
end

let extract_labels_from_annot annot =
  let visitor = new extract_labels in
  ignore (visitCilCodeAnnotation (visitor :> nopCilVisitor) annot) ;
  visitor#labels

let extract_labels_from_term term =
  let visitor = new extract_labels in
  ignore (visitCilTerm (visitor :> nopCilVisitor) term) ;
  visitor#labels

let extract_labels_from_pred pred =
  let visitor = new extract_labels in
  ignore (visitCilPredicate (visitor :> nopCilVisitor) pred) ;
  visitor#labels

let extract_stmts_from_labels labels =
  Logic_label.Set.fold
    (fun l a -> match l with
       | StmtLabel (stmt) -> Stmt.Set.add !stmt a
       | FormalLabel _ -> a
       | BuiltinLabel _ -> a)
    labels Stmt.Set.empty

let close_predicate p =
  let free_vars = free_vars_predicate Logic_var.Set.empty p in
  if Logic_var.Set.is_empty free_vars then p
  else
    { pred_name = [];
      pred_loc = p.pred_loc;
      pred_content = Pforall (Logic_var.Set.elements free_vars, p)}

class alpha_conv tbl ltbl =
  object
    inherit nopCilVisitor
    method! vvrbl v =
      try let v' = Hashtbl.find tbl v.vid in ChangeTo v'
      with Not_found -> DoChildren
    method! vlogic_var_use v =
      try let v' = Hashtbl.find ltbl v.lv_id in ChangeTo v'
      with Not_found -> DoChildren
  end

let create_alpha_renaming old_args new_args =
  let conversion = Hashtbl.create 7 in
  let lconversion = Hashtbl.create 7 in
  List.iter2
    (fun old_vi new_vi ->
       Hashtbl.add conversion old_vi.vid new_vi;
       if Ast_attributes.(contains anonymous_attribute_name new_vi.vattr) &&
          not (Ast_attributes.(contains anonymous_attribute_name old_vi.vattr))
       then begin
         new_vi.vname <- old_vi.vname;
         new_vi.vattr <- Ast_attributes.(drop anonymous_attribute_name new_vi.vattr);
       end;
       match old_vi.vlogic_var_assoc, new_vi.vlogic_var_assoc with
       | None, _ -> () (* nothing to convert in logic spec. *)
       | Some old_lv, Some new_lv ->
         Hashtbl.add lconversion old_lv.lv_id new_lv
       | Some old_lv, None ->
         Hashtbl.add lconversion old_lv.lv_id (cvar_to_lvar new_vi))
    old_args new_args;
  new alpha_conv conversion lconversion

(** Returns [true] whenever the type contains only arithmetic types *)
let is_fully_arithmetic ty =
  not (existsType
         (fun typ -> match typ.tnode with
            | TNamed _
            | TComp _
            | TArray _ -> ExistsMaybe
            | TPtr _ | TBuiltin_va_list | TFun _ | TVoid -> ExistsTrue
            | TEnum _ |TFloat _ | TInt _ ->  ExistsFalse)
         ty)

(* Note: The implementation preserves the order of s.succs in the returned list. *)
let separate_switch_succs s =
  let cases = match s.skind with
    | Switch (_, _, cases, _) -> cases
    | _ -> raise (Invalid_argument "separate_switch_succs")
  in
  let cases_set =
    List.fold_left (fun s stmt -> Stmt.Set.add stmt s) Stmt.Set.empty cases
  in
  let is_in_cases stmt = Stmt.Set.mem stmt cases_set in
  let contains_default_label stmt =
    let is_default_label = function
      | Default _ -> true
      | _ -> false
    in List.exists is_default_label stmt.labels
  in
  let contains_case_label stmt =
    let is_case_label = function
      | Case _ -> true
      | _ -> false
    in List.exists is_case_label stmt.labels
  in
  let default = ref None in
  let set_default s =
    if !default != None
    then Kernel.fatal ~current:true "Bad CFG: switch with multiple non-case successors.";
    default := Some s
  in
  let cases_non_default = ref [] in
  List.iter (fun stmt ->
      if not (is_in_cases stmt)
      then set_default stmt
      else
      if contains_default_label stmt
      then
        (set_default stmt;
         if contains_case_label stmt
         then cases_non_default := stmt::!cases_non_default)
      else
        (assert (contains_case_label stmt);
         cases_non_default := stmt::!cases_non_default)) s.succs;
  match !default with
  | None ->
    Kernel.fatal ~current:true "Bad CFG: switch with no non-case successors."
  | Some(d) -> (List.rev cases, d)
;;

(** Get the two successors of an If statement *)
let separate_if_succs (s:stmt) : stmt * stmt =
  match s.skind, s.succs with
  | If _, [sthen; selse] -> sthen, selse
  | _-> Kernel.fatal ~current:true "ifSuccs on an invalid If statement."

module Switch_cases =
  State_builder.Hashtbl
    (Stmt.Hashtbl)
    (Datatype.Pair(Datatype.List(Stmt))(Stmt))
    (struct
      let name = "Switch_cases"
      let dependencies = []
      let size = 49
    end)

let switch_case_state_self = Switch_cases.self
let separate_switch_succs = Switch_cases.memo separate_switch_succs

class dropAttributes ?select () = object(self)
  inherit genericCilVisitor (Visitor_behavior.copy (Project.current ()))
  method! vattr a =
    match select with
    | None -> ChangeTo []
    | Some l ->
      (match a with
       | (s,_) when List.mem s l -> ChangeTo []
       | _  -> DoChildren)
  method! vtype ty = match ty.tnode with
    | TNamed internal_ty ->
      let tty = Ast_types.add_attributes ty.tattr internal_ty.ttype in
      (* keep the original type whenever possible *)
      ChangeToPost
        (visitCilType (self:>cilVisitor) tty,
         fun x -> if x == internal_ty.ttype then ty else x)
    | TVoid | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _
    | TComp _ | TEnum _ | TBuiltin_va_list -> DoChildren
end

let typeDeepDropAllAttributes t =
  let vis = new dropAttributes () in
  visitCilType vis t

(* Deprecated *)

let mkBinOp_safe_ptr_cmp = mkBinOp_exn ~constfold:false
