(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* ---------------------------------------------------------------------- *)
(** Global data management *)

let split_slice s =
  SlicingParameters.debug ~level:1 "[Api.split_slice]";
  SlicingProject.split_slice s

let merge_slices ff_1 ff_2 ~replace =
  SlicingParameters.debug ~level:1 "[Api.merge_slices]";
  SlicingProject.merge_slices ff_1 ff_2 replace

let copy_slice ff =
  SlicingParameters.debug ~level:1 "[Api.copy_slice]";
  Fct_slice.copy_slice ff

(* ---------------------------------------------------------------------- *)
(** {1 Global setting } *)

let self = SlicingState.self

(* ---------------------------------------------------------------------- *)

let set_modes calls callers sliceUndef keepAnnotations () =
  SlicingParameters.Mode.Calls.set calls ;
  SlicingParameters.Mode.Callers.set callers ;
  SlicingParameters.Mode.SliceUndef.set sliceUndef;
  SlicingParameters.Mode.KeepAnnotations.set keepAnnotations

let set_modes ?(calls=SlicingParameters.Mode.Calls.get ())
    ?(callers=SlicingParameters.Mode.Callers.get ())
    ?(sliceUndef=SlicingParameters.Mode.SliceUndef.get ())
    ?(keepAnnotations=SlicingParameters.Mode.KeepAnnotations.get ())
    () =
  set_modes calls callers sliceUndef keepAnnotations ()

(* ---------------------------------------------------------------------- *)

(** {1 Slicing project } *)
module Project = struct

  (** {2 Values } *)

  let default_slice_names = SlicingTransform.default_slice_names

  let reset_slicing = SlicingState.reset_slicing

  let extract ?(f_slice_names=default_slice_names) new_proj_name =
    SlicingTransform.extract ~f_slice_names new_proj_name

  let print_dot ~filepath ~title =
    PrintSlice.build_dot_project filepath title

  let change_slicing_level = SlicingMacros.change_slicing_level

  (** {2 No needs of Journalization} *)

  let is_directly_called_internal = SlicingMacros.is_src_fun_called
  let is_called = Fct_slice.is_src_fun_called
  let has_persistent_selection = SlicingMacros.has_persistent_selection

  (** {2 Debug} *)

  let pretty = SlicingProject.print_project_and_worklist

end

(* ---------------------------------------------------------------------- *)

(** {1 Mark} *)
module Mark = struct

  type t = SlicingTypes.sl_mark
  let dyn_t = SlicingTypes.dyn_sl_mark

  (** {2 No needs of Journalization} *)

  let compare = SlicingMarks.compare_marks
  let pretty = SlicingMarks.pretty_mark
  let make = SlicingMarks.mk_user_mark
  let is_bottom = SlicingMarks.is_bottom_mark
  let is_spare = SlicingMarks.is_spare_mark
  let is_ctrl = SlicingMarks.is_ctrl_mark
  let is_data = SlicingMarks.is_addr_mark
  let is_addr = SlicingMarks.is_data_mark
  let get_from_src_func = Fct_slice.get_mark_from_src_fun
end

(* ---------------------------------------------------------------------- *)

(** {1 Selection} *)
module Select = struct

  type t = SlicingTypes.sl_select
  let dyn_t = SlicingTypes.Sl_select.ty

  module S = Cil_datatype.Varinfo.Map.Make(SlicingTypes.Fct_user_crit)
  let dyn_set = S.ty
  let empty_selects = Cil_datatype.Varinfo.Map.empty

  include SlicingCmds
  let get_function = get_select_kf

  let merge_internal = SlicingSelect.merge_db_select
  let add_to_selects_internal = SlicingSelect.Selections.add_to_selects
  let iter_selects_internal = SlicingSelect.Selections.iter_selects_internal
  let fold_selects_internal = SlicingSelect.Selections.fold_selects_internal
  let select_stmt_internal = SlicingSelect.select_stmt_computation
  let select_label_internal = SlicingSelect.select_label
  let select_min_call_internal = SlicingSelect.select_minimal_call
  let select_stmt_zone_internal = SlicingSelect.select_stmt_zone
  let select_zone_at_entry_point_internal = SlicingSelect.select_zone_at_entry
  let select_zone_at_end_internal = SlicingSelect.select_zone_at_end
  let select_modified_output_zone_internal = SlicingSelect.select_modified_output_zone
  let select_stmt_ctrl_internal = SlicingSelect.select_stmt_ctrl
  let select_entry_point_internal = SlicingSelect.select_entry_point
  let select_return_internal = SlicingSelect.select_return
  let select_decl_var_internal = SlicingSelect.select_decl_var
  let select_pdg_nodes_internal = SlicingSelect.select_pdg_nodes

  (** {2 Debug} *)

  let pretty = SlicingSelect.print_select

end

(* ---------------------------------------------------------------------- *)

(** {1 Slice} *)
module Slice = struct

  type t = SlicingTypes.sl_fct_slice
  let dyn_t = SlicingTypes.dyn_sl_fct_slice

  let create =
    SlicingProject.create_slice

  let remove =
    SlicingProject.remove_ff

  let remove_uncalled =
    SlicingProject.remove_uncalled_slices

  (** {2 No needs of Journalization} *)

  let get_all = SlicingProject.get_slices
  let get_function = SlicingMacros.get_ff_kf
  let get_callers = SlicingProject.get_slice_callers

  let get_called_slice ff stmt =
    match stmt.skind with
    | Instr (Call _ | Local_init (_, ConsInit _, _)) ->
      fst (Fct_slice.get_called_slice ff stmt)
    | _ -> None

  let get_called_funcs ff stmt =
    match stmt.skind with
    | Instr (Call _) ->
      if snd (Fct_slice.get_called_slice ff stmt) then
        Eva.Results.callee stmt
      else
        []
    | Instr (Local_init (_, ConsInit (f, _, _), _)) -> [ Globals.Functions.get f ]
    | _ -> []

  let get_mark_from_stmt = Fct_slice.get_stmt_mark
  let get_mark_from_label = Fct_slice.get_label_mark
  let get_mark_from_local_var = Fct_slice.get_local_var_mark

  let get_mark_from_formal ff var =
    let kf = SlicingMacros.get_ff_kf ff in
    let param_list = Kernel_function.get_formals kf in
    let rec find n var_list = match var_list with
      | [] -> raise Not_found
      | v :: var_list -> if Cil_datatype.Varinfo.equal v var then n
        else find (n+1) var_list
    in let n = find 1 param_list in
    Fct_slice.get_param_mark ff n

  let get_user_mark_from_inputs = Fct_slice.merge_inputs_m1_mark

  let get_num_id = SlicingMacros.get_ff_id

  let from_num_id kf num =
    List.find
      (fun f -> num = SlicingMacros.get_ff_id f)
      (SlicingProject.get_slices kf)

  (** {2 Debug} *)

  let pretty = SlicingProject.pretty_slice

end

(* ---------------------------------------------------------------------- *)

(** {1 Slicing request} *)
module Request = struct

  let apply_all propagate_to_callers =
    SlicingCmds.apply_all ~propagate_to_callers
  let apply_all ~propagate_to_callers =
    apply_all propagate_to_callers

  let apply_all_internal =
    SlicingCmds.apply_all_actions

  let apply_next_internal =
    SlicingCmds.apply_next_action

  let propagate_user_marks =
    SlicingCmds.topologic_propagation

  let copy_slice = copy_slice

  let split_slice = split_slice

  let merge_slices ff_1 ff_2 ~replace =
    merge_slices ff_1 ff_2 ~replace

  let add_call_slice caller to_call =
    SlicingSelect.call_ff_in_caller ~caller ~to_call
  let add_call_slice ~caller ~to_call =
    add_call_slice caller to_call

  let add_call_fun caller to_call =
    SlicingSelect.call_fsrc_in_caller ~caller ~to_call
  let add_call_fun ~caller ~to_call =
    add_call_fun caller to_call

  let add_call_min_fun caller to_call =
    SlicingSelect.call_min_f_in_caller ~caller ~to_call
  let add_call_min_fun ~caller ~to_call =
    add_call_min_fun caller to_call

  let add_selection =
    SlicingCmds.add_selection

  let add_persistent_selection =
    SlicingCmds.add_persistent_selection

  let add_persistent_cmdline =
    SlicingCmds.add_persistent_cmdline

  (** {2 No needs of Journalization} *)

  let is_request_empty_internal = SlicingProject.is_request_empty

  let add_slice_selection_internal = SlicingSelect.add_ff_selection
  let add_selection_internal = SlicingSelect.add_fi_selection

  (** {2 Debug} *)

  let pretty = SlicingProject.print_proj_worklist

end
(* ---------------------------------------------------------------------- *)
