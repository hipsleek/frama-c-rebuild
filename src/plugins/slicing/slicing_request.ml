(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server

let package =
  Package.package ~plugin:"slicing" ~name:"slicing" ~title:"Slicing" ()

(* ----- Slicing functions -------------------------------------------------- *)

let mk_selection fselect = fselect Api.Select.empty_selects ~spare:false

let kf_of_varinfo vi =
  try Globals.Functions.get vi
  with Not_found -> Data.failure "%a is not a function" Printer.pp_varinfo vi

let select_kf_aux fselect marker  =
  match (marker: Printer_tag.localizable) with
  | PVDecl (_, Kglobal, vi)
  | PLval (_, _, (Var vi, NoOffset)) ->
    mk_selection fselect (kf_of_varinfo vi)
  | marker ->
    Data.failure "Marker %a is not a function" Printer_tag.pp_localizable marker

let select_calls_to = select_kf_aux Api.Select.select_func_calls_to
let select_calls_into = select_kf_aux Api.Select.select_func_calls_into
let select_result = select_kf_aux Api.Select.select_func_return

(* ----- Slicing statements ------------------------------------------------- *)

let select_stmt_aux fselect marker =
  let kinstr = Printer_tag.ki_of_localizable marker in
  let kf = Printer_tag.kf_of_localizable marker in
  match kf, kinstr with
  | Some kf, Kstmt stmt ->
    mk_selection fselect stmt kf
  | _ ->
    Data.failure "No statement related to marker %a"
      Printer_tag.pp_localizable marker

let select_stmt = select_stmt_aux Api.Select.select_stmt
let select_stmt_control = select_stmt_aux Api.Select.select_stmt_ctrl

(* ----- Slicing lvalues ---------------------------------------------------- *)

let lval_of_marker = function
  | Printer_tag.PLval (Some kf, Kstmt stmt, lval) ->
    (* For dubious reasons, Api.Select requires strings instead of the lvalue.
       Thus, we convert the lval into string, so that it may be parsed back… *)
    let lval_str = Pretty_utils.to_string Printer.pp_lval lval in
    let lval_str_set = Datatype.String.Set.singleton lval_str in
    (kf, stmt, lval_str_set)
  | marker ->
    Data.failure "Marker %a is not an lvalue" Printer_tag.pp_localizable marker

let mk_selection_lval fselect =
  let pdg_mark = Api.Mark.make ~ctrl:true ~addr:true ~data:true in
  fselect Api.Select.empty_selects pdg_mark

let select_lval marker =
  let kf, stmt, lval = lval_of_marker marker in
  mk_selection_lval
    Api.Select.select_stmt_lval lval ~before:true stmt ~eval:stmt kf

let empty = Datatype.String.Set.empty

let select_lval_reads marker =
  let kf, stmt, lval = lval_of_marker marker in
  mk_selection_lval
    Api.Select.select_func_lval_rw ~rd:lval ~wr:empty ~eval:stmt kf

let select_lval_writes marker =
  let kf, stmt, lval = lval_of_marker marker in
  mk_selection_lval
    Api.Select.select_func_lval_rw ~rd:empty ~wr:lval ~eval:stmt kf

(* ----- Slicing requests --------------------------------------------------- *)

let mk_slice build_selection = fun marker ->
  let selection = build_selection marker in
  Api.Project.reset_slicing ();
  Api.Request.add_persistent_selection selection;
  Api.Request.apply_all_internal ();
  if SlicingParameters.Mode.Callers.get () then
    Api.Slice.remove_uncalled ();
  let project_name = SlicingParameters.ProjectName.get () in
  let suffix = SlicingParameters.ExportedProjectPostfix.get () in
  let project = Api.Project.extract (project_name ^ suffix) in
  project.name, project.pid

module Output = Data.Jpair (Data.Jstring) (Data.Jint)

(* All requests below are EXEC requests from an AST marker to the name and id
   of the new project containing the sliced AST. *)
let register_request ~name ~descr select =
  Request.register ~package ~kind:`EXEC ~name ~descr:(Markdown.plain descr)
    ~input:(module Kernel_ast.Marker) ~output:(module Output)
    (mk_slice select)

let () = register_request
    ~name:"sliceCallsTo"
    ~descr:"Slice effects of the given function"
    select_calls_to

let () = register_request
    ~name:"sliceCallsInto"
    ~descr:"Slice entrance into the given function"
    select_calls_into

let () = register_request
    ~name:"sliceResult"
    ~descr:"Slice the returned value of the given function"
    select_result

let () = register_request
    ~name:"sliceStmt"
    ~descr:"Slice effects of the given statement"
    select_stmt

let () = register_request
    ~name:"sliceStmtCtrl"
    ~descr:"Slice accessibility of the given statement"
    select_stmt_control

let () = register_request
    ~name:"sliceLval"
    ~descr:"Slice the given lvalue"
    select_lval

let () = register_request
    ~name:"sliceLvalReads"
    ~descr:"Slice read accesses of the given lvalue"
    select_lval_reads

let () = register_request
    ~name:"sliceLvalWrites"
    ~descr:"Slice write accesses of the given lvalue"
    select_lval_writes
