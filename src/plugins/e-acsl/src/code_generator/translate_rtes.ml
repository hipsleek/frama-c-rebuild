(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Generate and translate RTE annotations. *)

open Cil_types
let dkey = Options.Dkey.translation

let rte_annots pp elt kf env l =
  let old_kind = Env.annotation_kind env in
  let env = Env.set_annotation_kind env RTE in
  let env =
    List.fold_left
      (fun env a -> match a.annot_content with
         | AAssert(_, p) ->
           Env.handle_error
             (fun env ->
                Options.feedback ~dkey ~level:4 "prevent RTE from %a" pp elt;
                (* The logic scope MUST NOT be reset here since we still might
                   be in the middle of the translation of the original
                   predicate. *)
                let lscope_reset_old = Env.Logic_scope.get_reset env in
                let env = Env.Logic_scope.set_reset env false in
                let env =
                  Env.with_params ~rte:false ~env
                    (fun env -> Translate_predicates.do_it kf env p)
                in
                let env = Env.Logic_scope.set_reset env lscope_reset_old in
                env)
             env
         | _ -> assert false)
      env
      l
  in
  Env.set_annotation_kind env old_kind

let exp ?filter kf env e =
  Assert.push_pending_register_data();
  let stmt = Cil.mkStmtOneInstr ~valid_sid:true (Skip e.eloc) in
  let l = Rte.exp kf stmt e in
  let l =
    match filter with
    | Some f -> List.filter f l
    | None -> l
  in
  List.iter (Typing.preprocess_rte ~logic_env:(Env.Logic_env.get env)) l;
  let env = rte_annots Printer.pp_exp e kf env l in
  Assert.do_pending_register_data env

let () =
  Translate_terms.translate_rte_exp_ref := exp;
  Translate_predicates.Translate_rtes.translate_rte_annots_ref := rte_annots;
  Translate_predicates.Translate_rtes.translate_rte_exp_ref := exp;
  Logic_array.Translate_rtes.exp_ref := exp
