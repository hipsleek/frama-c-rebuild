(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module EdgeLabel = Abstract_state.EdgeLabel

(** Points-to graphs datastructure. *)
module G = Abstract_state.G

type v = G.V.t

let vid = Abstract_state.vid

module LSet = Abstract_state.LSet
module VarSet = Abstract_state.VarSet

module Abstract_state = Abstract_state

let check_computed () =
  if not (Analysis.is_computed ())
  then
    Options.abort "Static analysis must be called before any function of the API can be called"

let get_of_stmt ~stmt empty (get : Abstract_state.t -> 'a) =
  check_computed ();
  match Analysis.get_state_before_stmt stmt with
  | None -> empty
  | Some state -> get state

let lset ~stmt get_set = get_of_stmt ~stmt LSet.empty get_set
let vars ~stmt get_set = get_of_stmt ~stmt VarSet.empty get_set
let get_list ~stmt get = get_of_stmt ~stmt [] get

module Statement = struct
  let points_to_vars ~stmt lv = vars ~stmt (Abstract_state.points_to_vars lv)
  let points_to_lvals ~stmt lv = lset ~stmt (Abstract_state.points_to_lvals lv)
  let alias_sets_vars ~stmt = get_list ~stmt Abstract_state.alias_sets_vars
  let alias_sets_lvals ~stmt = get_list ~stmt Abstract_state.alias_sets_lvals
  let alias_vars ~stmt lv = vars ~stmt (Abstract_state.alias_vars lv)
  let alias_lvals ~stmt lv = lset ~stmt (Abstract_state.alias_lvals lv)

  let new_aliases_lvals ~stmt lv =
    let get_set state =
      let new_state = Analysis.do_stmt state stmt in
      Abstract_state.alias_lvals lv new_state
    in
    lset ~stmt:stmt get_set

  let new_aliases_vars ~stmt lv =
    let get_set state =
      let new_state = Analysis.do_stmt state stmt in
      Abstract_state.alias_vars lv new_state
    in
    vars ~stmt:stmt get_set

  let are_aliased ~stmt (lv1: lval) (lv2:lval) : bool =
    (* TODO: more efficient algorithm: do they share a successor? *)
    LSet.mem lv2 @@ alias_lvals ~stmt lv1
end

module Function = struct
  let return_stmt kf =
    if Kernel_function.has_definition kf
    then Kernel_function.find_return kf
    else Options.abort "function %a has no definition" Kernel_function.pretty kf

  let points_to_vars ~kf = Statement.points_to_vars ~stmt:(return_stmt kf)
  let points_to_lvals ~kf = Statement.points_to_lvals ~stmt:(return_stmt kf)
  let alias_sets_vars ~kf = Statement.alias_sets_vars ~stmt:(return_stmt kf)
  let alias_sets_lvals ~kf = Statement.alias_sets_lvals ~stmt:(return_stmt kf)
  let alias_vars ~kf = Statement.alias_vars ~stmt:(return_stmt kf)
  let alias_lvals ~kf = Statement.alias_lvals ~stmt:(return_stmt kf)
  let are_aliased ~kf = Statement.are_aliased ~stmt:(return_stmt kf)

  let fundec_stmts ~kf lv =
    if Kernel_function.has_definition kf
    then
      List.map
        (fun stmt -> stmt, Statement.new_aliases_lvals ~stmt lv)
        (Kernel_function.get_definition kf).sallstmts
    else
      Options.abort "fundec_stmts: function %a has no definition" Kernel_function.pretty kf
end

let fold_fundec_stmts (f_fold: 'a -> stmt -> lval -> 'a) (acc: 'a) (kf:kernel_function) (lv:lval) : 'a =
  List.fold_left
    (fun acc (s, set) ->
       LSet.fold (fun lv a -> f_fold a s lv) set acc
    )
    acc
    (Function.fundec_stmts ~kf lv)

let fold_vertex (f_fold : 'a -> G.V.t -> lval -> 'a) (acc: 'a) (_kf: kernel_function) (s:stmt) (lv: lval) : 'a =
  check_computed ();
  match Analysis.get_state_before_stmt s with
    None -> acc
  | Some state ->
    let v : G.V.t = Abstract_state.find_vertex lv state in
    let set_aliases = Abstract_state.find_synonyms lv state in
    LSet.fold (fun lv a -> f_fold a v lv) set_aliases acc

let fold_vertex_closure  (f_fold : 'a -> G.V.t -> lval -> 'a) (acc: 'a) (_kf: kernel_function)  (s:stmt) (lv: lval) : 'a =
  check_computed ();
  match Analysis.get_state_before_stmt s with
    None -> acc
  | Some state ->
    let list_closure : (G.V.t * LSet.t) list = Abstract_state.find_transitive_closure lv state in
    List.fold_left
      (fun acc (i,s) -> LSet.fold (fun lv a -> f_fold a i lv) s acc)
      acc
      list_closure

let get_state_before_stmt _kf =
  Analysis.get_state_before_stmt

let call_function a f res args =
  match Analysis.get_summary f with
    None -> None
  | Some su -> Some(Abstract_state.call a res args su)

let simplify_lval = Simplified.Lval.simplify
