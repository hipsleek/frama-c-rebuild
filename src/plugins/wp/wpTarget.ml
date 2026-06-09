(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Fct = Cil_datatype.Kf.Set

module TargetKfs =
  State_builder.Set_ref
    (Fct)
    (struct
      let dependencies = [ Ast.self ]
      let name = "WpTarget.TargetKfs"
    end)

let get_called_stmt stmt =
  match stmt.skind with
  | Instr (Call(_, fct, _, _)) ->
    begin match Kernel_function.get_called fct with
      | Some kf -> [kf]
      | None -> Option.value ~default:[] @@ Option.map snd (Dyncall.get stmt)
    end
  | Instr (Local_init (_,ConsInit(vi,_,_),_)) -> [ Globals.Functions.get vi ]
  | _ -> []

module Callees =
  State_builder.Hashtbl
    (Cil_datatype.Kf.Hashtbl)
    (Fct)
    (struct
      let dependencies = [Ast.self]
      let name = "WpTarget.Callees"
      let size = 17
    end)

(** Note: we add the kf received in parameter in the set only if it has a
    definition (and thus if it does not have one, we add nothing as it has
    no visible callee).

    This prevent to warn on prototypes that have a contract but are unused. If
    the function is used, it will be added to the set via its caller(s) if they
    are under verification.
*)
let with_callees kf =
  try
    let stmts = (Kernel_function.get_definition kf).sallstmts in
    let fold s stmt =
      List.fold_left (fun s kf -> Fct.add kf s) s (get_called_stmt stmt)
    in
    List.fold_left fold (Fct.singleton kf) stmts
  with Kernel_function.No_Definition -> Fct.empty

let with_callees = Callees.memo with_callees

let add_with_callees kf =
  let add kf =
    let insert_spec kf =
      let specs =
        if Kernel_function.has_definition kf
        then [ `Exits ; `Terminates ]
        else [ `Exits ; `Terminates ; `Assigns ]
      in
      Populate_spec.populate_funspec ~do_body:true kf specs
    in
    insert_spec kf ;
    TargetKfs.add kf
  in
  Fct.iter add (with_callees kf)

exception Found

let check_properties behaviors props kf =
  let open Property in
  let exists_selected_behavior l =
    behaviors = [] || List.exists (fun b -> List.mem b behaviors) l
  in
  let check_ip ip =
    if exists_selected_behavior (WpPropId.user_bhv_names ip) then
      let names = WpPropId.user_prop_names ip in
      if props = [] || WpPropId.are_selected_names props names then raise Found
  in
  let check_bhv_requires kf kinstr bv =
    List.iter (fun p -> check_ip (ip_of_requires kf kinstr bv p)) bv.b_requires
  in
  let check_bhv_ensures kf kinstr bv =
    List.iter (fun p -> check_ip (ip_of_ensures kf kinstr bv p)) bv.b_post_cond
  in
  let opt_check = function None -> () | Some p -> check_ip p in
  let check_bhv_assigns kf kinstr bv =
    opt_check (ip_assigns_of_behavior kf kinstr ~active:[] bv)
  in
  let check_bhv_allocation kf kinstr bv =
    opt_check (ip_allocation_of_behavior kf kinstr ~active:[] bv)
  in
  let check_complete_disjoint kf kinstr =
    try
      let spec = Annotations.funspec kf in
      let comp = ip_complete_of_spec kf kinstr ~active:[] spec in
      let disj = ip_disjoint_of_spec kf kinstr ~active:[] spec in
      List.iter check_ip comp ;
      List.iter check_ip disj ;
    with Annotations.No_funspec _ -> ()
  in
  let check_bhv kf kinstr bv =
    if CfgInfos.is_entry_point kf then
      check_bhv_requires kf kinstr bv ;
    check_bhv_assigns kf kinstr bv ;
    check_bhv_allocation kf kinstr bv ;
    check_bhv_ensures kf kinstr bv ;
    check_complete_disjoint kf kinstr
  in
  let check_code () =
    let stmts =
      try (Kernel_function.get_definition kf).sallstmts
      with Kernel_function.No_Definition -> []
    in
    let check stmt _ ca =
      List.iter check_ip (ip_of_code_annot kf stmt ca)
    in
    let check_call stmt =
      let check_callee kf =
        let kf_behaviors = Annotations.behaviors kf in
        List.iter (check_bhv_requires kf Kglobal) kf_behaviors
      in
      List.iter check_callee (get_called_stmt stmt)
    in
    let check_stmt stmt =
      check_call stmt ;
      Annotations.iter_code_annot (check stmt) stmt
    in
    List.iter check_stmt stmts
  in
  let check_funbhv bv = check_bhv kf Kglobal bv in
  Annotations.iter_behaviors check_funbhv kf ;
  check_code ()

let add_with_behaviors behaviors props kf =
  if behaviors = [] && props = [] then
    add_with_callees kf
  else begin
    try check_properties behaviors props kf
    with Found -> add_with_callees kf
  end

let compute_kf model bhv prop kf =
  let rtes = Wp_parameters.RTE.get () in
  if rtes then WpRTE.generate model kf ;
  add_with_behaviors bhv prop kf

let compute model
    ?(fct=Wp_parameters.get_fct())
    ?(bhv=Wp_parameters.Behaviors.get())
    ?(prop=Wp_parameters.Properties.get ()) ()
  =
  Wp_parameters.iter_fct (compute_kf model bhv prop) fct

let compute_kf model kf =
  compute_kf model [] [] kf

let iter = TargetKfs.iter
