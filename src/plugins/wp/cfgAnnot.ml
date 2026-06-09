(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* -------------------------------------------------------------------------- *)
(* --- Smoke Tests                                                        --- *)
(* -------------------------------------------------------------------------- *)

let smoke kf ~id ?doomed ?unreachable () =
  WpPropId.mk_smoke kf ~id ?doomed ?unreachable () , Logic_const.pfalse

let get_unreachable kf stmt =
  WpPropId.mk_smoke kf ~id:"unreachabled" ~unreachable:stmt ()

(* -------------------------------------------------------------------------- *)
(* --- Memoization                                                        --- *)
(* -------------------------------------------------------------------------- *)

module CodeKey =
struct
  type t = kernel_function * stmt
  let compare (a:t) (b:t) = Stdlib.compare (snd a).sid (snd b).sid
  let pretty fmt (a:t) = Format.fprintf fmt "s%d" (snd a).sid
end

(* -------------------------------------------------------------------------- *)
(* --- Property Accessors : Behaviors                                     --- *)
(* -------------------------------------------------------------------------- *)

type behavior = {
  bhv_assumes: WpPropId.pred_info list ;
  bhv_requires: WpPropId.pred_info list ;
  bhv_smokes: WpPropId.pred_info list ;
  bhv_ensures: WpPropId.pred_info list ;
  bhv_exits: WpPropId.pred_info list ;
  bhv_post_assigns: WpPropId.assigns_full_info ;
  bhv_exit_assigns: WpPropId.assigns_full_info ;
}

let normalize_assumes h =
  let module L = NormAtLabels in
  L.preproc_annot L.labels_fct_pre h

let implies ?assumes p =
  match assumes with None -> p | Some h -> Logic_const.pimplies (h,p)

let filter ~goal ip =
  if goal
  then Logic_utils.verify_predicate ip.ip_content.tp_kind
  else Logic_utils.use_predicate ip.ip_content.tp_kind

let normalize_pre ~goal kf bhv ?assumes ip =
  if filter ~goal ip then
    let module L = NormAtLabels in
    let labels = L.labels_fct_pre in
    let id = WpPropId.mk_pre_id kf Kglobal bhv ip in
    let pre = ip.ip_content.tp_statement in
    let assumes = Option.map normalize_assumes assumes in
    let precond = L.preproc_annot labels pre in
    Some (id, implies ?assumes precond)
  else None

let normalize_post ~goal kf bhv tk ?assumes (itk,ip) =
  if tk = itk && filter ~goal ip then
    let module L = NormAtLabels in
    let at_pre e = Logic_const.pat (e, BuiltinLabel Pre) in
    let assumes = Option.map (fun p -> normalize_assumes @@ at_pre p) assumes in
    let labels = L.labels_fct_post ~exit:(tk=Exits) in
    let id = WpPropId.mk_post_id kf Kglobal bhv (tk,ip) in
    let post = L.preproc_annot labels ip.ip_content.tp_statement in
    Some (id , implies ?assumes post)
  else None

let normalize_decreases (d, li) =
  let module L = NormAtLabels in
  let at_pre e = Logic_const.tat (e, BuiltinLabel Pre) in
  let labels = L.labels_fct_pre in
  (at_pre @@ L.preproc_term labels d, li)

let normalize_froms tk froms =
  let module L = NormAtLabels in
  let labels = L.labels_fct_assigns ~exit:(tk=Exits) in
  L.preproc_assigns labels froms

let normalize_fct_assigns kf ~exits bhv = function
  | WritesAny ->
    WpPropId.empty_assigns_info, WpPropId.empty_assigns_info
  | Writes froms ->
    let make tk =
      match WpPropId.mk_fct_assigns_id kf exits bhv tk froms with
      | None -> WpPropId.empty_assigns_info
      | Some id ->
        let assigns = normalize_froms tk froms in
        let desc = WpPropId.mk_kf_assigns_desc assigns in
        WpPropId.mk_assigns_info id desc
    in
    make Normal,
    if exits then make Exits else WpPropId.empty_assigns_info

let get_behavior_goals kf ?(smoking=false) ?(exits=false) bhv =
  let pre_cond = normalize_pre ~goal:false kf bhv in
  let post_cond = normalize_post ~goal:true kf bhv in
  let p_asgn, e_asgn = normalize_fct_assigns kf ~exits bhv bhv.b_assigns in
  let smokes =
    let do_assumes =
      Wp_parameters.SmokeDeadassumes.get() && bhv.b_assumes <> [] in
    if smoking && (bhv.b_requires <> [] || do_assumes) then
      let bname =
        if Cil.is_default_behavior bhv then "default" else bhv.b_name in
      let id, doomed =
        if bhv.b_requires <> []
        then
          bname ^ "_requires",
          Property.ip_requires_of_behavior kf Kglobal bhv
        else
          bname ^ "_assumes",
          Property.ip_assumes_of_behavior kf Kglobal bhv
      in
      [smoke kf ~id ~doomed ()]
    else []
  in
  {
    bhv_assumes = List.filter_map pre_cond bhv.b_assumes;
    bhv_requires = List.filter_map pre_cond bhv.b_requires;
    bhv_ensures = List.filter_map (post_cond Normal) bhv.b_post_cond ;
    bhv_exits = List.filter_map (post_cond Exits) bhv.b_post_cond ;
    bhv_post_assigns = p_asgn ;
    bhv_exit_assigns = e_asgn ;
    bhv_smokes = smokes;
  }

(* -------------------------------------------------------------------------- *)
(* --- Side Behavior Requires                                             --- *)
(* -------------------------------------------------------------------------- *)

let get_requires ~goal kf bhv =
  List.filter_map (normalize_pre ~goal kf bhv) bhv.b_requires

let get_preconditions ~goal kf =
  let module L = NormAtLabels in
  let mk_pre = L.preproc_annot L.labels_fct_pre in
  List.map
    (fun bhv ->
       let p = Ast_info.behavior_precondition ~goal bhv in
       let ip = Logic_const.new_predicate p in
       WpPropId.mk_pre_id kf Kglobal bhv ip,  mk_pre p
    ) (Annotations.behaviors kf)

let get_complete_behaviors kf =
  let spec = Annotations.funspec kf in
  let module L = NormAtLabels in
  List.map
    (fun bs ->
       WpPropId.mk_compl_bhv_id (kf,Kglobal,[],bs) ,
       L.preproc_annot L.labels_fct_pre @@ Ast_info.complete_behaviors spec bs
    ) spec.spec_complete_behaviors

let get_disjoint_behaviors kf =
  let spec = Annotations.funspec kf in
  let module L = NormAtLabels in
  List.map
    (fun bs ->
       WpPropId.mk_disj_bhv_id (kf,Kglobal,[],bs) ,
       L.preproc_annot L.labels_fct_pre @@ Ast_info.disjoint_behaviors spec bs
    ) spec.spec_disjoint_behaviors

let normalize_terminates p =
  let module L = NormAtLabels in
  L.preproc_annot L.labels_fct_pre @@
  Logic_const.pat (p.ip_content.tp_statement, BuiltinLabel Pre)

let get_terminates_goal kf =
  let make_pred_info p =
    WpPropId.mk_terminates_id kf Kglobal p, normalize_terminates p in
  Option.map make_pred_info @@ Annotations.terminates kf

let get_terminates_hyp kf =
  match get_terminates_goal kf with
  | None -> true, Logic_const.pfalse
  | Some (_, p) -> false, p

let check_variant_relation = function
  | (_, None) -> ()
  | ({ term_loc }, Some rel) ->
    Wp_parameters.hypothesis
      ~source:(fst term_loc) ~once:true
      "'%a' relation must be well-founded" Cil_printer.pp_logic_info rel

let get_decreases_goal kf =
  let defined t = WpPropId.mk_decrease_id kf Kglobal t, normalize_decreases t in
  match Annotations.decreases kf with
  | None -> None
  | Some v -> check_variant_relation v ; Some (defined v)

let get_decreases_hyp kf =
  Annotations.decreases kf

(* -------------------------------------------------------------------------- *)
(* --- Contracts                                                          --- *)
(* -------------------------------------------------------------------------- *)

type contract = {
  contract_cond : WpPropId.pred_info list ;
  contract_hpre : WpPropId.pred_info list ;
  contract_post : WpPropId.pred_info list ;
  contract_exit : WpPropId.pred_info list ;
  contract_smoke : WpPropId.pred_info list ;
  contract_assigns : Cil_types.assigns ;
  contract_terminates : bool * Cil_types.predicate ;
  contract_decreases : Cil_types.variant option ;
}

let default_assigns behaviors =
  try (List.find Cil.is_default_behavior behaviors).b_assigns
  with Not_found -> WritesAny

let unguarded_behavior_assigns behaviors =
  let unguarded_assigns b = b.b_assumes = [] && b.b_assigns <> WritesAny in
  try (List.find unguarded_assigns behaviors).b_assigns
  with Not_found -> WritesAny

let assigns_of_complete behaviors complete =
  let in_complete b = List.exists (String.equal b.b_name) complete in
  let behaviors = List.filter in_complete behaviors in
  let concat a bhv = Logic_utils.concat_assigns a bhv.b_assigns in
  List.fold_left concat (Writes []) behaviors

let complete_assigns behaviors completes =
  let exception Found of assigns in
  let find_complete complete =
    match assigns_of_complete behaviors complete with
    | WritesAny -> ()
    | assigns -> raise (Found assigns)
  in
  try List.iter find_complete completes ; WritesAny
  with Found assigns -> assigns

let assigns_upper_bound behaviors completes =
  match default_assigns behaviors with
  | Writes _ as assigns -> assigns
  | WritesAny ->
    match unguarded_behavior_assigns behaviors with
    | Writes _ as assigns ->
      Wp_parameters.warning ~once:true ~current:true
        "No default assigns clause, using unguarded behavior assigns" ;
      assigns
    | WritesAny ->
      match complete_assigns behaviors completes with
      | Writes _ as assigns ->
        Wp_parameters.warning ~once:true ~current:true
          "No default assigns clause, using complete behaviors assigns" ;
        assigns
      | WritesAny ->
        (* We don't warn here, WritesAny have special treatment in CfgCalculus*)
        WritesAny

(* -------------------------------------------------------------------------- *)
(* --- Call Contracts                                                     --- *)
(* -------------------------------------------------------------------------- *)

(*TODO: put it in Status_by_call ? *)
module AllPrecondStatus =
  State_builder.Hashtbl(Kernel_function.Hashtbl)(Datatype.Unit)
    (struct
      let name = "Wp.CfgAnnot.AllPrecondStatus"
      let dependencies = [Ast.self]
      let size = 32
    end)

let setup_preconditions kf =
  if not (AllPrecondStatus.mem kf) then
    begin
      AllPrecondStatus.add kf () ;
      Statuses_by_call.setup_all_preconditions_proxies kf ;
    end

let get_precond_at kf stmt (id,p) =
  let pi = WpPropId.property_of_id id in
  let pi_at = Statuses_by_call.precondition_at_call kf pi stmt in
  let id_at = WpPropId.mk_call_pre_id kf stmt pi pi_at in
  id_at , p

module CallContract = WpContext.StaticGenerator(Kernel_function)
    (struct
      type key = kernel_function
      type data = contract
      let name = "Wp.CfgAnnot.CallContract"
      let compile kf =
        let wcond : WpPropId.pred_info list ref = ref [] in
        let whpre : WpPropId.pred_info list ref = ref [] in
        let wpost : WpPropId.pred_info list ref = ref [] in
        let wexit : WpPropId.pred_info list ref = ref [] in
        let add w f x = match f x with Some y -> w := y :: !w | None -> () in
        let behaviors = Annotations.behaviors kf in
        let completes = Annotations.complete kf in
        setup_preconditions kf ;
        List.iter
          begin fun bhv ->
            (* Normalization of assumes is specific to each case *)
            let assumes = Ast_info.behavior_assumes bhv in
            let mk_cond = normalize_pre ~goal:true kf bhv ~assumes in
            let mk_hpre = normalize_pre ~goal:false kf bhv ~assumes in
            let mk_post = normalize_post ~goal:false kf bhv ~assumes in
            List.iter (add wcond @@ mk_cond) bhv.b_requires ;
            List.iter (add whpre @@ mk_hpre) bhv.b_requires ;
            List.iter (add wpost @@ mk_post Normal) bhv.b_post_cond ;
            List.iter (add wexit @@ mk_post Exits) bhv.b_post_cond ;
          end behaviors ;
        let assigns = match assigns_upper_bound behaviors completes with
          | WritesAny -> WritesAny
          | Writes froms -> Writes (normalize_froms Normal froms)
        in
        let terminates = get_terminates_hyp kf in
        let decreases = Option.map normalize_decreases @@ get_decreases_hyp kf in
        {
          contract_cond = List.rev !wcond ;
          contract_hpre = List.rev !whpre ;
          contract_post = List.rev !wpost ;
          contract_exit = List.rev !wexit ;
          contract_smoke = [] ;
          contract_assigns = assigns ;
          contract_terminates = terminates ;
          contract_decreases = decreases ;
        }
    end)

let get_call_contract ?smoking kf stmt =
  let cc = CallContract.get kf in
  let preconds = List.map (get_precond_at kf stmt) cc.contract_cond in
  match smoking with
  | None ->
    { cc with contract_cond = preconds }
  | Some s ->
    let g = smoke kf ~id:"dead_call" ~unreachable:s () in
    { cc with contract_cond = preconds ; contract_smoke = [ g ] }

(* -------------------------------------------------------------------------- *)
(* --- Assembly Code                                                      --- *)
(* -------------------------------------------------------------------------- *)

let is_assembly stmt =
  match stmt.skind with
  | Instr (Asm _) -> true
  | _ -> false

let get_stmt_assigns kf stmt =
  let asgn =
    Annotations.fold_code_annot
      begin fun _emitter ca l ->
        match ca.annot_content with
        | AStmtSpec(fors,s) ->
          List.fold_left
            (fun l bhv ->
               match bhv.b_assigns with
               | WritesAny -> l
               | Writes froms ->
                 let module L = NormAtLabels in
                 let labels = L.labels_stmt_assigns ~kf stmt in
                 match
                   WpPropId.mk_stmt_assigns_id kf stmt fors bhv froms
                 with
                 | None -> l
                 | Some id ->
                   let froms = L.preproc_assigns labels froms in
                   let desc = WpPropId.mk_stmt_assigns_desc stmt froms in
                   WpPropId.mk_assigns_info id desc :: l
            ) l s.spec_behavior
        | _ -> l
      end stmt []
  in if asgn = [] then [WpPropId.mk_stmt_any_assigns_info stmt] else asgn

(* -------------------------------------------------------------------------- *)
(* --- Code Assertions                                                    --- *)
(* -------------------------------------------------------------------------- *)

type code_assertion = {
  code_admitted: WpPropId.pred_info option ;
  code_verified: WpPropId.pred_info option ;
}

(* Note: collected in REVERSE order *)
module CodeAssertions = WpContext.StaticGenerator(CodeKey)
    (struct
      type key = CodeKey.t
      type data = code_assertion list
      let name = "Wp.CfgAnnot.CodeAssertions"
      let compile (kf,stmt) =
        let labels = NormAtLabels.labels_assert ~kf stmt in
        let normalize_pred p = NormAtLabels.preproc_annot labels p in
        let all_annot = (* ensures that the order is the one displayed in GUI *)
          List.sort
            Cil_datatype.Code_annotation.compare
            (Annotations.code_annot stmt)
        in
        List.fold_left
          begin fun l ca ->
            match ca.annot_content with
            | AStmtSpec _ when not @@ is_assembly stmt ->
              let source = fst (Cil_datatype.Stmt.loc stmt) in
              Wp_parameters.warning ~once:true ~source
                "Statement specifications not yet supported (skipped)." ; l
            | AInvariant(_,false,_) ->
              let source = fst (Cil_datatype.Stmt.loc stmt) in
              Wp_parameters.warning ~once:true ~source
                "Generalized invariant not yet supported (skipped)." ; l
            | AAssert(_,a) ->
              let p =
                WpPropId.mk_assert_id kf stmt ca ,
                normalize_pred a.tp_statement in
              let admit = Logic_utils.use_predicate a.tp_kind in
              let verif = Logic_utils.verify_predicate a.tp_kind in
              let use flag p = if flag then Some p else None in
              {
                code_admitted = use admit p ;
                code_verified = use verif p ;
              } :: l
            | _ -> l
          end [] all_annot
    end)

let get_code_assertions ?(smoking=false) kf stmt =
  let ca = CodeAssertions.get (kf,stmt) in
  (* Make sure that smoke tests are in the end so that it can see surely false
     assertions associated to this statement, in particular RTE assertions.   *)
  List.rev @@
  if smoking then
    let s = smoke kf ~id:"dead_code" ~unreachable:stmt () in
    { code_admitted = None ; code_verified = Some s } :: ca
  else ca

(* -------------------------------------------------------------------------- *)
(* --- Loop Invariants                                                    --- *)
(* -------------------------------------------------------------------------- *)

let mk_variant_properties kf s ca v =
  let vpos_id = WpPropId.mk_var_pos_id kf s ca in
  let vdecr_id = WpPropId.mk_var_decr_id kf s ca in
  let loc = v.term_loc in
  let lcurr = Clabels.to_logic (Clabels.loop_current s) in
  let vcurr = Logic_const.tat ~loc (v, lcurr) in
  let zero = Cil.lzero ~loc () in
  let vpos = Logic_const.prel ~loc (Rle, zero, vcurr) in
  let vdecr = Logic_const.prel ~loc (Rlt, v, vcurr) in
  (vpos_id, vpos), (vdecr_id, vdecr)

let mk_variant_relation_property kf s ca v li =
  check_variant_relation (v, Some li) ;
  let vid = WpPropId.mk_var_id kf s ca in
  let loc = v.term_loc in
  let lcurr = Clabels.to_logic (Clabels.loop_current s) in
  let vcurr = Logic_const.tat ~loc (v, lcurr) in
  let variant = Logic_const.papp ~loc (li,[],[vcurr ; v]) in
  (vid, variant)

type loop_hypothesis =
  | NoHyp
  | Check of WpPropId.prop_id
  | Always of WpPropId.prop_id

type loop_invariant = {
  loop_hyp : loop_hypothesis ;
  loop_est : WpPropId.prop_id option ;
  loop_ind : WpPropId.prop_id option ;
  loop_pred : Cil_types.predicate ;
}

type loop_contract = {
  loop_terminates: predicate option;
  loop_invariants : loop_invariant list ;
  (* to be proved after loop invariants *)
  loop_smoke: WpPropId.pred_info list;
  (* assigned by loop body *)
  loop_assigns: WpPropId.assigns_full_info list;
}

let default_assigns stmt l =
  { l with
    loop_assigns =
      if l.loop_assigns <> [] then l.loop_assigns
      else [WpPropId.mk_loop_any_assigns_info stmt] }

module LoopContract = WpContext.StaticGenerator(CodeKey)
    (struct
      type key = CodeKey.t
      type data = loop_contract
      let name = "Wp.CfgAnnot.LoopContract"
      let compile (kf,stmt) =
        let labels = NormAtLabels.labels_loop stmt in
        let normalize_pred p = NormAtLabels.preproc_annot labels p in
        let normalize_annot (i,p) = i, normalize_pred p in
        let normalize_assigns w = NormAtLabels.preproc_assigns labels w in
        let intro_terminates_variant ~loc (pid, v) =
          pid,
          let t = snd @@ get_terminates_hyp kf in
          if Wp_parameters.TerminatesVariantHyp.get () then begin
            if Logic_utils.is_same_predicate t Logic_const.pfalse then
              Wp_parameters.warning
                ~source:(fst loc) ~once:true
                "Loop variant is always trivially verified \
                 (terminates \\false)" ;
            Logic_const.pimplies (t, v)
          end else v
        in
        let variant_as_inv ~loc (i, p) =
          let i, p = intro_terminates_variant ~loc @@ normalize_annot (i, p) in
          { loop_pred = p ;
            loop_hyp = NoHyp ; loop_est = None ; loop_ind = Some i }
        in
        let all_annot = (* ensures that the order is the one displayed in GUI *)
          List.rev @@ List.sort
            Cil_datatype.Code_annotation.compare
            (Annotations.code_annot stmt)
        in
        default_assigns stmt @@
        List.fold_left
          begin fun l ca ->
            match ca.annot_content with
            | AInvariant(_,true,inv) ->
              let g_hyp = WpPropId.mk_inv_hyp_id kf stmt ca in
              let g_est, g_ind = WpPropId.mk_loop_inv kf stmt ca in
              let admit = Logic_utils.use_predicate inv.tp_kind in
              let verif = Logic_utils.verify_predicate inv.tp_kind in
              let loop_hyp = if admit then Always g_hyp else Check g_hyp in
              let use flag id = if flag then Some id else None in
              let inv =
                { loop_pred = normalize_pred inv.tp_statement ;
                  loop_hyp ;
                  loop_est = use verif g_est ;
                  loop_ind = use verif g_ind ; }
              in
              { l with
                loop_invariants  = inv :: l.loop_invariants ; }
            | AVariant(term, None) ->
              let vpos , vdec = mk_variant_properties kf stmt ca term in
              let vpos = variant_as_inv ~loc:term.term_loc vpos in
              let vdec = variant_as_inv ~loc:term.term_loc vdec in
              { l with loop_terminates = None ;
                       loop_invariants = vdec :: vpos :: l.loop_invariants }
            | AVariant(term, Some rel) ->
              let vrel = mk_variant_relation_property kf stmt ca term rel in
              let vrel = variant_as_inv ~loc:term.term_loc vrel in
              { l with loop_terminates = None ;
                       loop_invariants = vrel :: l.loop_invariants }
            | AAssigns(_,WritesAny) ->
              let asgn = WpPropId.mk_loop_any_assigns_info stmt in
              { l with loop_assigns = asgn :: l.loop_assigns }
            | AAssigns(_,Writes w) ->
              begin match WpPropId.mk_loop_assigns_id kf stmt ca w with
                | None -> l (* shall not occur *)
                | Some id ->
                  let w = normalize_assigns w in
                  let a = WpPropId.mk_loop_assigns_desc stmt w in
                  let asgn = WpPropId.mk_assigns_info id a in
                  { l with loop_assigns = asgn :: l.loop_assigns }
              end
            | _ -> l
          end
          { loop_terminates = Some Logic_const.pfalse ;
            loop_invariants = [] ;
            loop_smoke = [] ;
            loop_assigns = [] ;
          }
          all_annot
    end)

let get_loop_contract ?(smoking=false) ?terminates kf stmt =
  (* Loop Contract *)
  let lc = LoopContract.get (kf,stmt) in
  (* Loop Smoking *)
  let lc =
    if smoking && not (WpReached.is_dead_code stmt) then
      let g = smoke kf ~id:"dead_loop" ~unreachable:stmt () in
      { lc with loop_smoke = g :: lc.loop_smoke }
    else lc
  in
  (* Loop Termination *)
  match lc.loop_terminates, terminates with
  | None, _ -> lc
  | Some _, None -> { lc with loop_terminates = None }
  | Some loop_terminates, Some terminates ->
    let prop = Logic_const.pimplies(terminates, loop_terminates) in
    { lc with loop_terminates = Some prop }

(* -------------------------------------------------------------------------- *)
(* --- Clear Tablesnts                                                    --- *)
(* -------------------------------------------------------------------------- *)

let clear () =
  CallContract.clear () ;
  LoopContract.clear () ;
  CodeAssertions.clear ()

(* -------------------------------------------------------------------------- *)
