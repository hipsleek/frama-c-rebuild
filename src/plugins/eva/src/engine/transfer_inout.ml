(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Register read and written memory zones to {!Inout_access}. *)

open Eval

module Make (Engine : Engine_abstractions_sig.S) = struct
  module Location = Engine.Loc
  module Eval = Engine.Eval
  module EvaAstDeps = Eva_ast.MakeDepsOf (Location)

  type location = Location.location
  type value = Engine.Val.t
  type valuation = Eval.Valuation.t

  let register_and_return_access pos access =
    Inout_access.register pos access;
    access

  let logic_assign_access clause location =
    let write = Location.enumerate_valid_bits Write location in
    let read =
      match clause with
      | Assigns (_, from_deps) ->
        List.fold_left
          (fun acc from_dep ->
             match from_dep.location with
             | Address _ ->
               acc
             | Location from_loc ->
               let read = Location.enumerate_valid_bits Read from_loc in
               Memory_zone.join acc read)
          Memory_zone.bottom
          from_deps
      | _ -> Memory_zone.bottom
    in
    Inout_access.Access.make ~read ~write ()

  let register_logic_assign pos clause location =
    logic_assign_access clause location
    |> register_and_return_access pos

  let find_loc valuation = Eval.Valuation.find_loc_def valuation

  let compute_zones to_loc (lval : Eva_ast.lval) =
    match lval.node with
    | Var vi, NoOffset ->
      Locations.zone_of_varinfo vi, Memory_zone.bottom
    | _ ->
      let loc = to_loc lval in
      let lv_zone = Location.enumerate_valid_bits Write loc in
      let lv_indirect_zone = EvaAstDeps.indirect_zone_of_lval to_loc lval in
      lv_zone, lv_indirect_zone

  let assign_lval_access valuation lval exp =
    let to_loc = find_loc valuation in
    let written_zone, lv_indirect_zone = compute_zones to_loc lval in
    let exp_zone = EvaAstDeps.zone_of_exp to_loc exp in
    let read_zone = Memory_zone.join lv_indirect_zone exp_zone in
    Inout_access.Access.make ~read:read_zone ~write:written_zone ()

  let register_assign_lval pos valuation lval exp =
    assign_lval_access valuation lval exp
    |> register_and_return_access pos

  let assign_var_access valuation vi exp =
    let lval = Eva_ast.Build.var vi in
    assign_lval_access valuation lval exp

  let register_assign_var pos valuation vi exp =
    assign_var_access valuation vi exp
    |> register_and_return_access pos

  let read_exp_access valuation exp =
    let to_loc = find_loc valuation in
    let read = EvaAstDeps.zone_of_exp to_loc exp in
    Inout_access.Access.make ~read ()

  let register_read_exp pos valuation exp =
    read_exp_access valuation exp
    |> register_and_return_access pos

  let call_args_access valuation call =
    let access = Inout_access.Access.bottom in
    (* Register read and written zone for named arguments. *)
    let f acc { formal; concrete } =
      let access = assign_var_access valuation formal concrete in
      Inout_access.Access.join acc access
    in
    let access = List.fold_left f access call.arguments in
    (* Register read zones for the rest of the arguments. *)
    let f acc (concrete, _) =
      let access = read_exp_access valuation concrete in
      Inout_access.Access.join acc access
    in
    List.fold_left f access call.rest

  let register_call_args pos valuation call =
    call_args_access valuation call
    |> register_and_return_access pos

end
