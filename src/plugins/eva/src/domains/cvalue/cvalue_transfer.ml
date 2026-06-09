(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval
open Eva_ast

type value = Main_values.CVal.t
type origin = value
type location = Main_locations.PLoc.location

let unbottomize = function
  | `Bottom -> Cvalue.V.bottom
  | `Value v -> v

(* ---------------------------------------------------------------------- *)
(*                        Garbled mix warnings                            *)
(* ---------------------------------------------------------------------- *)

let warn_imprecise_value ?prefix lval value =
  match value with
  | Addresses.Bytes.Top (bases, origin) ->
    if Origin.register_write bases origin then
      let prefix = Option.fold ~none:"A" ~some:(fun s -> s ^ ": a") prefix in
      Self.warning ~wkey:Self.wkey_garbled_mix_write ~once:true ~current:true
        ~stacktrace:true
        "@[%sssigning imprecise value to %a@ because of %s.@]"
        prefix Eva_ast.pp_lval lval (Origin.descr origin)
  | _ -> ()

let warn_imprecise_location ?prefix loc =
  match loc.Locations.addr with
  | Addresses.Bits.Top (Base.SetLattice.Top, orig) ->
    let prefix = Option.fold ~none:"" ~some:(fun s -> s ^ ": ") prefix in
    Self.fatal ~current:true
      "@[%swriting at a completely unknown address@ because of %s.@]@\nAborting."
      prefix (Origin.descr orig)
  | _ -> ()

let warn_imprecise_write ?prefix lval loc value =
  warn_imprecise_location ?prefix loc;
  warn_imprecise_value ?prefix lval value

let warn_imprecise_offsm_write ?prefix lval offsm =
  let warn value =
    warn_imprecise_value ?prefix lval (Cvalue.V_Or_Uninitialized.get_v value)
  in
  Cvalue.V_Offsetmap.iter_on_values warn offsm

(* ---------------------------------------------------------------------- *)
(*                               Assumptions                              *)
(* ---------------------------------------------------------------------- *)

let reduce valuation lval value t =
  if Ast_types.has_qualifier "volatile" lval.typ
  then t
  else
    match valuation.Abstract_domain.find_loc lval with
    | `Value record ->
      let loc = Precise_locs.imprecise_location record.loc in
      if Locations.cardinal_zero_or_one loc
      then Cvalue.Model.reduce_indeterminate_binding t loc value
      else t
    | `Top -> t (* Cannot reduce without the location of the lvalue. *)

let is_smaller_value typ v1 v2 =
  let size = Z.of_int (Cil.bitsSizeOf typ) in
  let card1 = Cvalue.V.cardinal_estimate v1 ~size
  and card2 = Cvalue.V.cardinal_estimate v2 ~size in
  Z.lt card1 card2

(* Update the state according to a Valuation. *)
let update valuation t =
  let process exp record t =
    match exp.node with
    | Lval lv ->
      if record.reductness = Reduced && Ast_types.is_scalar lv.typ
      then
        let {v; initialized; escaping} = record.value in
        let v = unbottomize v in
        let v =
          (* The origin contains the value already stored in the state, when
             its type is incompatible with the lvalue [lv]. The precision of
             this previous value and [v] are then incomparable (none is
             included in the other). We use some notion of cardinality of
             abstract values to choose the best value to keep. *)
          match record.origin with
          | Some previous_v ->
            if is_smaller_value lv.typ v previous_v then v else previous_v
          | _ -> v
        in
        let value = Cvalue.V_Or_Uninitialized.make ~initialized ~escaping v in
        reduce valuation lv value t
      else t
    | _ -> t
  in
  valuation.Abstract_domain.fold process t

(* ---------------------------------------------------------------------- *)
(*                              Assignments                               *)
(* ---------------------------------------------------------------------- *)

let write_abstract_value state (lval, loc) assigned_value =
  let {v; initialized; escaping} = assigned_value in
  let value = unbottomize v in
  let value =
    if Ast_types.has_qualifier "volatile" lval.typ
    then Cvalue_forward.make_volatile value
    else value
  in
  warn_imprecise_write lval loc value;
  let exact = Locations.cardinal_zero_or_one loc in
  let value = Cvalue.V_Or_Uninitialized.make ~initialized ~escaping value in
  Cvalue.Model.add_indeterminate_binding ~exact state loc value;

exception Do_assign_imprecise_copy

let copy_one_loc state left_lv right_lv =
  let left_lval, left_loc = left_lv
  and right_lval, right_loc = right_lv in
  (* top size is tested before this function is called, in which case
     the imprecise copy mode is used. *)
  let size = Z_or_top.project right_loc.Locations.size in
  let right_addr = right_loc.Locations.addr in
  let offsetmap = Cvalue.Model.copy_offsetmap right_addr size state in
  let make_volatile =
    Ast_types.has_qualifier "volatile" left_lval.typ ||
    Ast_types.has_qualifier "volatile" right_lval.typ
  in
  match offsetmap with
  | `Bottom -> `Bottom
  | `Value offsm ->
    (* TODO: this is the good place to handle partially volatile
       struct, whether as source or destination *)
    let offsetmap =
      if make_volatile then
        Cvalue.V_Offsetmap.map_on_values
          (Cvalue.V_Or_Uninitialized.map Cvalue_forward.make_volatile) offsm
      else offsm
    in
    if not (Eval_typ.offsetmap_matches_type left_lval.typ offsetmap) then
      raise Do_assign_imprecise_copy;
    warn_imprecise_offsm_write left_lval offsetmap;
    `Value
      (Cvalue.Model.paste_offsetmap ~exact:true
         ~from:offsetmap ~dst_addr:left_loc.Locations.addr ~size state)

let make_determinate value =
  { v = `Value value; initialized = true; escaping = false }

let copy_right_lval state left_lv right_lv copied_value =
  let lval, loc = left_lv in
  (* Size mismatch between left and right size, or imprecise size.
     This cannot be done by copies, but require a conversion *)
  let right_size = Main_locations.PLoc.size right_lv.lloc
  and left_size = Main_locations.PLoc.size loc in
  if not (Z_or_top.equal left_size right_size) || Z_or_top.is_top right_size
  then
    fun loc -> write_abstract_value state (lval, loc) copied_value
  else
    fun loc ->
      try
        let process right_loc acc =
          let left_lv = lval, loc
          and right_lv = right_lv.lval, right_loc in
          match copy_one_loc state left_lv right_lv with
          | `Bottom -> acc
          | `Value state -> Cvalue.Model.join acc state
        in
        Precise_locs.fold process right_lv.lloc Cvalue.Model.bottom
      with
        Do_assign_imprecise_copy ->
        write_abstract_value state (lval, loc) copied_value

let assign ~pos:_ { lval; lloc } _expr assigned valuation state =
  let state = update valuation state in
  let assign_one_loc =
    match assigned with
    | Assign value ->
      let assigned_value = make_determinate value in
      fun loc -> write_abstract_value state (lval, loc) assigned_value
    | Copy (right_lv, copied_value) ->
      copy_right_lval state (lval, lloc) right_lv copied_value
  in
  let aux_loc loc acc_state =
    let s = assign_one_loc loc in
    Cvalue.Model.join acc_state s
  in
  let state = Precise_locs.fold aux_loc lloc Cvalue.Model.bottom in
  if not (Cvalue.Model.is_reachable state)
  then `Bottom
  else `Value state

(* ---------------------------------------------------------------------- *)
(*                             Function Calls                             *)
(* ---------------------------------------------------------------------- *)

let actualize_formals state arguments =
  let treat_one_formal state arg =
    let offsm =
      Cvalue_offsetmap.offsetmap_of_assignment state arg.concrete arg.avalue
    in
    warn_imprecise_offsm_write (Eva_ast.Build.var arg.formal) offsm;
    Cvalue.Model.add_base (Base.of_varinfo arg.formal) offsm state
  in
  List.fold_left treat_one_formal state arguments

let start_call ~pos:_ call _recursion _valuation state =
  `Value (actualize_formals state call.arguments)

let finalize_call ~pos:_ _call _recursion ~pre:_ ~post:state =
  `Value state

let show_expr valuation state fmt expr =
  match expr.node with
  | Lval lval | StartOf lval ->
    let loc = match valuation.Abstract_domain.find_loc lval with
      | `Value record -> record.loc
      | `Top -> assert false
    in
    let offsm = Eval_op.offsetmap_of_loc loc state in
    Bottom.pretty (Eval_op.pretty_offsetmap lval.typ) fmt offsm
  | _ -> Unicode.pp_top fmt


(* ----------------- Export assumption functions -------------------------- *)

let update valuation state = `Value (update valuation state)
let assume ~pos:_ _expr _positive = update
