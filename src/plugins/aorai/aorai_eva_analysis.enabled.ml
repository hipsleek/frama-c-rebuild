(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

let show_aorai_variable state fmt var_name =
  let vi = Data_for_aorai.(get_varinfo var_name) in
  let cvalue = Cvalue.Model.find state (Locations.of_varinfo vi) in
  try
    let i = Ival.project_int (Cvalue.V.project_ival cvalue) in
    let state_name = Data_for_aorai.getStateName (Z.to_int i) in
    Format.fprintf fmt "%s" state_name
  with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int |
       Z.Overflow | Not_found ->
    Format.fprintf fmt "?"

let show_val fmt (expr, v) =
  Format.fprintf fmt "%a in %a"
    Eva.Eva_ast.pp_exp expr
    (Cvalue.V.pretty_typ (Some expr.typ)) v

let show_non_det_state fmt state =
  let (states,_) = Data_for_aorai.getGraph () in
  let first_print = ref true in
  let print_state s sure =
    if not !first_print then Format.fprintf fmt ",@ ";
    first_print := false;
    Format.fprintf fmt "%s%s"
      (if sure then "" else "(?)")
      s.Automaton_ast.name
  in
  let print_one s =
    (* TODO: sync Data_for_aorai.get_state_var with current project*)
    let vi = Data_for_aorai.get_varinfo s.Automaton_ast.name in
    let e = Cil.evar vi in
    let cvalue =
      Eva.Results.(in_cvalue_state state |> eval_exp e |> as_cvalue)
    in
    if Cvalue.V.contains_non_zero cvalue then
      print_state s (not (Cvalue.V.contains_zero cvalue))
  in
  List.iter print_one states

let show_aorai_state = "Frama_C_show_aorai_state"

let builtin_show_aorai_state state args =
  if Aorai_option.is_on() then begin
    if not (Aorai_option.Deterministic.get()) then begin
      Aorai_option.result ~current:true
        "@[<hv 2>Possible states:@ %a@]" show_non_det_state state;
    end else begin
      let history = Data_for_aorai.(curState :: (whole_history ())) in
      Aorai_option.result ~current:true "@[<hv>%a@]"
        (Pretty_utils.pp_list ~sep:" <- " (show_aorai_variable state)) history;
    end;
    if args <> [] then begin
      Aorai_option.result ~current:true "@[<hv>%a@]"
        (Pretty_utils.pp_list ~sep:"," show_val) args
    end;
  end else begin
    Aorai_option.(warning ~wkey:not_aorai_wkey)
      "Calling %s builtin without Aorai instrumentation"
      show_aorai_state
  end;
  (* Return value : returns nothing, changes nothing *)
  Eva.Builtins.States [state]


let () =
  Cil_builtins.add_custom_builtin
    (fun () -> (show_aorai_state,Cil_const.voidType,[],true))

let () =
  Eva.Builtins.register_builtin show_aorai_state builtin_show_aorai_state;
  Cil_builtins.add_special_builtin show_aorai_state

let add_slevel_annotation vi kind =
  match kind with
  | Aorai_visitors.Aux_funcs.(Pre _ | Post _) ->
    let kf = Globals.Functions.get vi in
    let stmt = Kernel_function.find_first_stmt kf
    and emitter = Aorai_option.emitter in
    Eva.Eva_annotations.(add_slevel_annot ~emitter stmt SlevelFull)
  | _ -> ()

let add_slevel_annotations () =
  Aorai_visitors.Aux_funcs.iter add_slevel_annotation

let add_nondet_partitioning () =
  let (states,_) = Data_for_aorai.getGraph() in
  let add_one_state state =
    let vi = Data_for_aorai.get_state_var state in
    Eva.Parameters.use_global_value_partitioning vi
  in
  List.iter add_one_state states

let add_partitioning varname =
  match Data_for_aorai.get_varinfo_option varname with
  | None -> add_nondet_partitioning ()
  | Some vi -> Eva.Parameters.use_global_value_partitioning vi

let add_aux_partitioning () =
  List.iter
    Eva.Parameters.use_global_value_partitioning
    (Data_for_aorai.aux_variables())

let add_state_variables_partitioning () =
  add_partitioning Data_for_aorai.curState;
  add_aux_partitioning ();
  List.iter add_partitioning (Data_for_aorai.whole_history ())

let setup () =
  add_slevel_annotations ();
  add_state_variables_partitioning ()
