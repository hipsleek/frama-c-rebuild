(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

open Pdg_types

let rec pp_stmt fmt s = match s.skind with
  | Instr _ | Return _ | Goto _ | Break _ | Continue _ | TryFinally _
  | TryExcept _ | Throw _ | TryCatch _ ->
    Printer.without_annot Printer.pp_stmt fmt s
  | If (e, _, _, _) ->
    Format.fprintf fmt "if(%a) <..>" Printer.pp_exp e
  | Switch (e, _, _, _) ->
    Format.fprintf fmt "switch(%a)<..>" Printer.pp_exp e
  | Loop _ -> Format.fprintf fmt "while (...)"
  | Block b ->
    begin match b.bstmts with
      | [] -> Format.fprintf fmt "<Block {}>"
      | s :: _ -> Format.fprintf fmt "<Block { %a }>" pp_stmt s
    end
  | UnspecifiedSequence _ -> Format.fprintf fmt "TODO"

let print_results fmt a =
  Pretty_utils.pp_list
    (fun fmt s ->
       Format.fprintf fmt "@[<hov 2>%a (sid %d): %a@]"
         Fileloc.pretty (Stmt.loc s) s.sid pp_stmt s
    ) fmt a

let compute_from_stmt stmt =
  let kf = Kernel_function.find_englobing_kf stmt in
  let skip = Compute_impact.skip () in
  let reason = Options.Reason.get () in
  Compute_impact.stmts_impacted ~skip ~reason kf [stmt]

let compute_from_nodes kf nodes =
  let skip = Compute_impact.skip () in
  let reason = Options.Reason.get () in
  let r = Compute_impact.nodes_impacted ~skip ~reason kf nodes in
  Pdg_aux.NS.fold
    (fun (n, _z) acc -> PdgTypes.NodeSet.add n acc)
    r PdgTypes.NodeSet.empty


let compute_multiple_stmts skip kf ls =
  Options.debug "computing impact of statement(s) %a"
    (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls;
  let reason = Options.Reason.get () in
  let res, _, _ = Compute_impact.nodes_impacted_by_stmts ~skip ~reason kf ls in
  let res_nodes = Compute_impact.result_to_nodes res in
  let res_stmts = Compute_impact.nodes_to_stmts res_nodes in
  if Options.Print.get () then begin
    Options.result "@[<v 2>@[impacted statements of stmt(s) %a are:@]@ %a@]"
      (Pretty_utils.pp_list ~sep:",@ " Stmt.pretty_sid) ls
      print_results res_stmts
  end;
  res_nodes

(* Slice on the given list of stmts *)
let slice (stmts:stmt list) =
  Options.feedback ~level:2 "beginning slicing";
  let name = "impact slicing" in
  Slicing.Api.Project.reset_slicing ();
  let select sel ({ sid = id } as stmt) =
    let kf = Kernel_function.find_englobing_kf stmt in
    Options.debug ~level:3 "selecting sid %d (of %s)"
      id
      (Kernel_function.get_name kf);
    Slicing.Api.Select.select_stmt sel ~spare:false stmt kf
  in
  let sel = List.fold_left select Slicing.Api.Select.empty_selects stmts in
  Options.debug ~level:2 "applying slicing request";
  Slicing.Api.Request.add_persistent_selection sel;
  Slicing.Api.Request.apply_all_internal ();
  Slicing.Api.Slice.remove_uncalled ();
  let extracted_prj = Slicing.Api.Project.extract name in
  Options.feedback ~level:2 "slicing done";
  extracted_prj


(* Register impact ACSL extension. *)
let () =
  let typer typing_context loc args =
    match args with
    | [] -> Ext_terms []
    | _ -> typing_context.Logic_typing.error loc "Invalid impact directive"
  in
  Acsl_extension.register_code_annot_next_both
    ~plugin:"impact" "impact_stmt" typer false

let is_impact_annot code_annot =
  match code_annot.annot_content with
  | AExtended (_, _, { ext_name = "impact_stmt"; }) -> true
  | _ -> false

let compute_annots () =
  Ast.compute ();
  (* Returns the list of statements of function [kf] that have an impact
     annotation. *)
  let compute_impact_stmts kf acc =
    (* Annot option only accept defined functions. *)
    let fundec = Kernel_function.get_definition kf in
    let has_impact_annot stmt =
      let impact_annots = Annotations.code_annot ~filter:is_impact_annot stmt in
      not (impact_annots = [])
    in
    let impact_stmts = List.filter has_impact_annot fundec.sallstmts in
    if impact_stmts = [] then acc
    else (kf, impact_stmts) :: acc
  in
  let impact_stmts = Options.Annot.fold compute_impact_stmts [] in
  let skip = Compute_impact.skip () in
  (* compute impact analyses on each kf *)
  let nodes = List.fold_left
      (fun nodes (kf, stmts) ->
         Pdg_aux.NS.union nodes (compute_multiple_stmts skip kf stmts))
      Pdg_aux.NS.empty impact_stmts
  in
  let stmts = Compute_impact.nodes_to_stmts nodes in
  if Options.Slicing.get () then ignore (slice stmts);
  stmts;
;;

let from_stmt = compute_from_stmt

let from_nodes = compute_from_nodes

let main () =
  if Options.is_on () then begin
    Options.feedback "beginning analysis";
    assert (not (Options.Annot.is_empty ()));
    ignore (compute_annots ());
    Options.feedback "analysis done"
  end
let () = Boot.Main.extend main
