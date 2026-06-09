(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Internal State} *)

module Result_pair =
  Datatype.Pair_with_collections(Datatype.Bool)(Datatype.Bool)
module Result =
  State_builder.Hashtbl
    (Datatype.Hashtbl
       (Result_pair.Hashtbl)
       (Result_pair))
    (Project.Datatype)
    (struct
      let name = "Sparecode"
      let size = 7
      let dependencies = [ Ast.self; Eva.Analysis.self ] (* delayed, see below *)
    end)

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:Result.self
         [ Pdg.Api.self; Inout.self ])

module P = Sparecode_params

(** {2 State_builder} *)

let rm_unused_globals ?new_proj_name ?(project=Project.current ()) () =
  let new_proj_name =
    match new_proj_name with
    | Some name -> name
    | None -> (Project.get_name project)^ " (without unused globals)"
  in
  P.feedback "remove unused global declarations from project '%s'"
    (Project.get_name project);
  P.result "removed unused global declarations in new project '%s'" new_proj_name;
  Project.on project Globs.rm_unused_decl new_proj_name

let run select_annot select_slice_annot =
  P.feedback "remove unused code...";
  (*let initial_file = Ast.get () in*)
  let kf_entry, _library = Globals.entry_point () in
  let proj =
    Spare_marks.select_useful_things
      ~select_annot ~select_slice_annot kf_entry
  in
  let old_proj_name = Project.get_name (Project.current ()) in
  let new_proj_name = (old_proj_name^" without sparecode") in

  P.feedback "remove unused global declarations...";
  let tmp_prj = Transform.Info.build_cil_file ~last:false "tmp_prj" proj in
  let new_prj = Project.on tmp_prj Globs.rm_unused_decl new_proj_name in

  P.result "result in new project '%s'." (Project.get_name new_prj);
  Project.remove ~project:tmp_prj ();
  let ctx = Parameter_state.get_selection_context () in
  Project.copy ~selection:ctx new_prj;
  new_prj

let get ~select_annot ~select_slice_annot =
  Result.memo
    (fun _ -> run select_annot select_slice_annot)
    (select_annot, select_slice_annot)

let main () =
  if Sparecode_params.Analysis.get () then begin
    let select_annot = Sparecode_params.Annot.get () in
    let select_slice_annot = true in
    let new_proj = get ~select_annot ~select_slice_annot in
    File.pretty_ast ~prj:new_proj ()
  end
  else if Sparecode_params.GlobDecl.get () then begin
    let new_proj = rm_unused_globals () in
    File.pretty_ast ~prj:new_proj ()
  end

let () = Boot.Main.extend main
