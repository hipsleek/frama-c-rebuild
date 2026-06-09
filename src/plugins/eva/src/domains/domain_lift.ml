(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

module type Input_Domain = sig
  include Abstract_domain.S
  val key: t Structure.Key_Domain.key
end

module type Conversion = sig
  type extended
  type internal
  val extend: internal -> extended
  val restrict: extended -> internal
end

module Make
    (Domain: Input_Domain)
    (Ctx: Conversion with type internal := Domain.context)
    (Val: Conversion with type internal := Domain.value)
    (Loc: Conversion with type internal := Domain.location)
= struct

  include (Domain : Datatype.S_with_collections with type t = Domain.t)
  include (Domain : Abstract_domain.Lattice with type state = Domain.state)

  let name = Domain.name

  let structure = Abstract.Domain.Leaf (Domain.key, (module Domain))

  let log_category = Domain.log_category

  type context = Ctx.extended
  type value = Val.extended
  type location = Loc.extended
  type origin = Domain.origin

  let extract_expr ~oracle context state exp =
    let oracle exp = oracle exp >>=: Val.restrict in
    Domain.extract_expr ~oracle context state exp >>=: fun (value, origin) ->
    Val.extend value, origin

  let extract_lval ~oracle context state lval loc =
    let oracle exp = oracle exp >>=: Val.restrict in
    let loc = Loc.restrict loc in
    Domain.extract_lval ~oracle context state lval loc
    >>=: fun (value, origin) ->
    Val.extend value, origin

  let backward_location state lval loc value =
    Domain.backward_location state lval (Loc.restrict loc) (Val.restrict value)
    >>-: fun (loc, value) ->
    Loc.extend loc, Val.extend value

  let reduce_further state expr value =
    let list = Domain.reduce_further state expr (Val.restrict value) in
    List.map (fun (e, v) -> e, Val.extend v) list

  let build_context state =
    let open Bottom.Operators in
    let+ context = Domain.build_context state in
    Ctx.extend context

  let lift_left left = { left with lloc = Loc.restrict left.lloc }
  let lift_flagged_value value =
    { value with v = value.v >>-: Val.restrict }
  let lift_assigned = function
    | Assign value -> Assign (Val.restrict value)
    | Copy (lval, value) -> Copy (lift_left lval, lift_flagged_value value)

  let lift_argument arg = { arg with avalue = lift_assigned arg.avalue }

  let lift_call call =
    let arguments = List.map lift_argument call.arguments in
    let rest =
      List.map (fun (exp, assigned) -> exp, lift_assigned assigned) call.rest
    in
    { call with arguments; rest }

  let lift_valuation valuation =
    let (>>>) v f = match v with
      | `Value v -> `Value (f v)
      | `Top -> `Top
    in
    let lift_record r = { r with value = lift_flagged_value r.value } in
    let lift_loc_record r = { r with loc = Loc.restrict r.loc } in
    let open Abstract_domain in
    let find expr = valuation.find expr >>> lift_record in
    let find_loc lval = valuation.find_loc lval >>> lift_loc_record in
    let find_loc_def lval = valuation.find_loc_def lval |> Loc.restrict in
    let fold f acc =
      valuation.fold (fun exp record acc -> f exp (lift_record record) acc) acc
    in
    { find; fold; find_loc; find_loc_def }

  let update valuation = Domain.update (lift_valuation valuation)

  let assign ~pos lv expr value valuation state =
    Domain.assign ~pos
      (lift_left lv) expr (lift_assigned value) (lift_valuation valuation) state

  let assume ~pos expr positive valuation state =
    Domain.assume ~pos expr positive (lift_valuation valuation) state

  let start_call ~pos call recursion valuation state =
    Domain.start_call
      ~pos (lift_call call) recursion (lift_valuation valuation) state

  let finalize_call ~pos call recursion ~pre ~post =
    Domain.finalize_call ~pos (lift_call call) recursion ~pre ~post

  let show_expr valuation = Domain.show_expr (lift_valuation valuation)

  let lift_logic_dep dep =
    let location =
      match dep.location with
      | Location loc -> Location (Loc.restrict loc)
      | Address _ as x -> x
    in
    { dep with location }

  let lift_logic_assigns = function
    | Assigns (term, dep) -> Assigns (term, List.map lift_logic_dep dep)
    | (Allocates _ | Frees _) as x -> x

  let logic_assign assigns location state =
    let assigns = Option.map (fun (a, s) -> lift_logic_assigns a, s) assigns in
    Domain.logic_assign assigns (Loc.restrict location) state

  let evaluate_predicate = Domain.evaluate_predicate
  let reduce_by_predicate = Domain.reduce_by_predicate
  let interpret_acsl_extension = Domain.interpret_acsl_extension

  let enter_scope = Domain.enter_scope
  let leave_scope = Domain.leave_scope

  let enter_loop = Domain.enter_loop
  let incr_loop_counter = Domain.incr_loop_counter
  let leave_loop = Domain.leave_loop

  let empty = Domain.empty
  let initialize_variable lval loc ~initialized init_value state =
    let loc = Loc.restrict loc in
    Domain.initialize_variable lval loc ~initialized init_value state
  let initialize_variable_using_type = Domain.initialize_variable_using_type

  let relate = Domain.relate
  let filter = Domain.filter
  let reuse = Domain.reuse
  let project = Domain.project
  let overwrite = Domain.overwrite

  module Store = Domain.Store

  let post_analysis = Domain.post_analysis

end
