(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Stmt = Cil_datatype.Stmt
module Logic_label = Cil_datatype.Logic_label

(* Functions flagged as commonly related to user-provided input, therefore
   targeted in the option for automatic tainting. *)
let auto_taint_arg_functions = (* auto taint the arguments *) [
  "fgets";
  "gets";
  "fread";
  "fread_unlocked";
  "fgets_unlocked";
  "getline";
  "read"
]

(* Variadic function expecting a string literal argument as nth argument. *)
let auto_taint_variadic_functions = [
  "fscanf", 1;
  "scanf", 0;
]

let auto_taint_res_functions = (* auto taint the result *) [
  "getchar";
  "getc"
]

let auto_taint () = Parameters.TaintAuto.get ()
let ignore_singletons () = not (Parameters.TaintSingletons.get ())

let secure_flow_analysis () = Parameters.SecureFlow.get ()

(* Default namespace for taints, when no custom one is provided by the user. *)
let default_taint_namespace = "default"

(* Custom taint namespaces for secure-flow/non-interference analysis. *)
let private_taint_namespace = "private"
let public_taint_namespace = "public"
let is_private_namespace = String.equal private_taint_namespace
let is_public_namespace = String.equal public_taint_namespace


(* Debug key to also include [assume_stmts] in the output of the
   Frama_C_domain_show_each directive. *)
let dkey_debug = Self.register_category "d-taint-debug"
    ~help:"debug print of the taint domain"

let wkey =
  Self.register_warn_category "taint"
    ~help:"warnings related to the taint analysis from \"-eva-domains taint\""

(* -------------------------------------------------------------------------- *)
(*             Checks and warnings related to -eva-secure-flow                *)
(* -------------------------------------------------------------------------- *)

let _wkey_secure_flow =
  Self.register_warn_category "secure-flow"
    ~help:"warnings related to secure-flow analysis from \"-eva-domains taint\""

let wkey_secure_flow_direct =
  Self.register_warn_category "secure-flow:direct"
    ~help:"warnings related to direct interference when performing \
           secure-flow analysis from \"-eva-domains taint\""

let wkey_secure_flow_indirect =
  Self.register_warn_category "secure-flow:indirect"
    ~help:"warnings related to indirect interference when performing \
           secure-flow analysis from \"-eva-domains taint\""

let wkey_secure_flow_assume =
  Self.register_warn_category "secure-flow:condition"
    ~help:"warnings related to interference on conditions when \
           performing secure-flow analysis from \"-eva-domains taint\""

let filter_public_zone =
  let base_is_public base =
    match Base.typeof base with
    | None -> false
    | Some typ -> Ast_types.has_qualifier public_taint_namespace typ
  in
  Memory_zone.filter_base base_is_public

let warn_assign_interference ~pos ~data_tainted ~ctrl_tainted zone =
  if secure_flow_analysis () && (data_tainted || ctrl_tainted) then
    let zone = filter_public_zone zone in
    if not (Memory_zone.is_bottom zone) then
      let source = fst (Position.loc pos) in
      let warn wkey kind zone =
        Self.warning ~wkey ~source ~once:true
          "@[<hv 2>%s non-interference violation on@ @[<hov>{%a}@]"
          kind Memory_zone.pretty zone
      in
      if data_tainted then warn wkey_secure_flow_direct "direct" zone;
      if ctrl_tainted then warn wkey_secure_flow_indirect "indirect" zone

let warn_assume_interference ~pos zone =
  if secure_flow_analysis () then
    let source = fst (Position.loc pos) in
    Self.warning ~wkey:wkey_secure_flow_assume ~source ~once:true
      "@[<hv 2>non-interference violation on condition involving@ @[<hov>{%a}@]"
      Memory_zone.pretty zone

(* -------------------------------------------------------------------------- *)
(*                          Lattice for one taint                             *)
(* -------------------------------------------------------------------------- *)

type taint_state = {
  (* Over-approximation of the memory locations that are tainted due to a data
     dependency. *)
  locs_data: Memory_zone.t;
  (* Over-approximation of the memory locations that are tainted due to a
     control dependency. *)
  locs_control: Memory_zone.t;
  (* Set of assume statements over a tainted expression. This set is needed to
     implement control-dependency: all left-values appearing in statements whose
     evaluation depends on at least one of the assume expressions is to be
     tainted. This set is restricted to statements of the current function. *)
  assume_stmts: Stmt.Set.t;
  (* Whether the current call depends on a tainted assume statement: if true,
     all assignments in the current call should be control tainted. *)
  tainted_call: bool;
}

module LatticeSingleTaint = struct

  let pp_locs_only fmt t =
    Format.fprintf fmt
      "@[<v>@[<hv 2>Locations (data):@ @[<hov>%a@]@]@,\
       @[<hv 2>Locations (control):@ @[<hov>%a@]@]@]"
      Memory_zone.pretty t.locs_data
      Memory_zone.pretty t.locs_control

  let pp_state fmt t =
    Format.fprintf fmt
      "@[<v>@[<hv 2>Locations (data):@ @[<hov>%a@]@]@,\
       @[<hv 2>Locations (control):@ @[<hov>%a@]@]@,\
       @[<hv 2>Assume statements:@ @[<hov>%a@]@]@,\
       @[<hv 2>Tainted call:@ @[<hov>%b@]@]@]"
      Memory_zone.pretty t.locs_data
      Memory_zone.pretty t.locs_control
      Stmt.Set.pretty t.assume_stmts
      t.tainted_call

  (* Frama-C "datatype" for type [taint]. *)
  include Datatype.Make_with_collections (struct
      include Datatype.Serializable_undefined

      type t = taint_state

      let name = "single-taint"

      let reprs =
        [ { locs_data = List.hd Memory_zone.reprs;
            locs_control = List.hd Memory_zone.reprs;
            assume_stmts = Stmt.Set.empty;
            tainted_call = false; } ]

      let structural_descr =
        Structural_descr.t_record
          [| Memory_zone.packed_descr; Memory_zone.packed_descr;
             Stmt.Set.packed_descr; Datatype.Bool.packed_descr |]

      let compare t1 t2 =
        let (<?>) c (cmp,x,y) = if c = 0 then cmp x y else c in
        Memory_zone.compare t1.locs_data t2.locs_data
        <?> (Memory_zone.compare, t1.locs_control, t2.locs_control)
        <?> (Stmt.Set.compare, t1.assume_stmts, t2.assume_stmts)
        <?> (Datatype.Bool.compare, t1.tainted_call, t2.tainted_call)

      let equal = Datatype.from_compare

      let pretty fmt t =
        if Self.is_debug_key_enabled dkey_debug
        then pp_state fmt t
        else pp_locs_only fmt t

      let hash t =
        Hashtbl.hash
          (Memory_zone.hash t.locs_data,
           Memory_zone.hash t.locs_control,
           Stmt.Set.hash t.assume_stmts,
           t.tainted_call)

      let copy c = c

    end)

  (* Initial state at the start of the computation: nothing is tainted yet. *)
  let empty = {
    locs_data = Memory_zone.bottom;
    locs_control = Memory_zone.bottom;
    assume_stmts = Stmt.Set.empty;
    tainted_call = false;
  }

  (* Top state: everything is tainted. *)
  let top = {
    locs_data = Memory_zone.top;
    locs_control = Memory_zone.top;
    assume_stmts = Stmt.Set.empty;
    tainted_call = false;
  }

  (* Join: keep pointwise over-approximation. *)
  let join t1 t2 =
    { locs_data = Memory_zone.join t1.locs_data t2.locs_data;
      locs_control = Memory_zone.join t1.locs_control t2.locs_control;
      assume_stmts = Stmt.Set.union t1.assume_stmts t2.assume_stmts;
      tainted_call = t1.tainted_call || t2.tainted_call; }

  (* The memory locations are finite, so the ascending chain property is
     already verified. We simply use a join. *)
  let widen _ _ t1 t2 = join t1 t2

  let narrow t1 t2 =
    `Value {
      locs_data = Memory_zone.narrow t1.locs_data t2.locs_data;
      locs_control = Memory_zone.narrow t1.locs_control t2.locs_control;
      assume_stmts = Stmt.Set.inter t1.assume_stmts t2.assume_stmts;
      tainted_call = t1.tainted_call && t2.tainted_call;
    }

  (* Inclusion testing: pointwise, on locs only. *)
  let is_included t1 t2 =
    Memory_zone.is_included t1.locs_data t2.locs_data &&
    Memory_zone.is_included t1.locs_control t2.locs_control

  (* Intersection testing: pointwise, on locs only. *)
  let intersects t e =
    Memory_zone.intersects t.locs_data e ||
    Memory_zone.intersects t.locs_control e

end

(* -------------------------------------------------------------------------- *)
(*                           Multi-taint lattice                              *)
(* -------------------------------------------------------------------------- *)

module LatticeMultiTaint = struct

  module Info = struct
    let name = "Eva.Taint_domain.TaintNames"
    let dependencies = [ Self.state ]
  end

  (* Stores the set of taint names encountered during an analysis. *)
  module TaintNamesRef = State_builder.Set_ref (Datatype.String.Set) (Info)

  (* Maps a taint name to its corresponding state. *)
  module TaintNamespace = struct
    include Datatype.String.Map
    include Datatype.String.Map.Make (LatticeSingleTaint)

    let add name = TaintNamesRef.add name; add name

    let find_or_empty key map =
      try find key map
      with Not_found -> LatticeSingleTaint.empty

    let compare t1 t2 =
      Datatype.String.Map.compare LatticeSingleTaint.compare t1 t2

    let hash t =
      fold (fun _ state acc -> LatticeSingleTaint.hash state + acc) t 0

    let pp_per_taint fmt ~pp taint =
      Format.fprintf fmt "@[%a@]" pp taint

    let pretty fmt t =
      let pp =
        if Self.is_debug_key_enabled dkey_debug
        then LatticeSingleTaint.pp_state
        else LatticeSingleTaint.pp_locs_only
      in
      Pretty_utils.pp_iter2 ~pre:"@[<v>" ~sep:"@," ~between:":@;<1 2>" iter
        Format.pp_print_string
        (pp_per_taint ~pp)
        fmt t

    let join t1 t2 =
      let merge_per_key _key maybe_state1 maybe_state2 =
        match maybe_state1, maybe_state2 with
        | state, None | None, state ->
          state
        | Some state1, Some state2 ->
          Some (LatticeSingleTaint.join state1 state2)
      in
      merge merge_per_key t1 t2

    let widen kf stmt t1 t2 =
      let widen_per_key _key maybe_state1 maybe_state2 =
        match maybe_state1, maybe_state2 with
        | state, None | None, state -> state
        | Some state1, Some state2 ->
          Some (LatticeSingleTaint.widen kf stmt state1 state2)
      in
      merge widen_per_key t1 t2

    let narrow t1 t2 =
      let merge_per_key _key maybe_state1 maybe_state2 =
        match maybe_state1, maybe_state2 with
        | _, None | None, _ -> None
        | Some state1, Some state2 ->
          let `Value v = LatticeSingleTaint.narrow state1 state2 in
          Some v
      in
      merge merge_per_key t1 t2

    let is_included t1 t2 =
      let fold2 f t1 t2 base =
        let f key state1 acc =
          let state2 = find_or_empty key t2 in
          f state1 state2 acc
        in
        fold f t1 base
      in
      fold2 (fun state1 state2 acc ->
          LatticeSingleTaint.is_included state1 state2 && acc) t1 t2 true
  end

  include TaintNamespace
  include Lattice_bounds.Top.Bound_Lattice (TaintNamespace)
  let name = "taint"

  let empty = `Value TaintNamespace.empty

  let widen kf stmt t1 t2 =
    let open Lattice_bounds.Top.Operators in
    let+ t1 and+ t2 in
    TaintNamespace.widen kf stmt t1 t2

  let narrow t1 t2 =
    `Value (Lattice_bounds.Top.narrow TaintNamespace.narrow t1 t2)

end

(* -------------------------------------------------------------------------- *)
(*                         Propagation of one taint                           *)
(* -------------------------------------------------------------------------- *)

module TransferSingleTaint = struct

  let loc_of_lval valuation lv = valuation.Abstract_domain.find_loc_def lv

  (* Keeps only active tainted assumes for [stmt]. A tainted assume in [state]
     is considered active on a statement [stmt] whenever there exists a path
     from the tainted assume that not go through [stmt], ie [stmt] is not a
     postdominator for the tainted assume. *)
  let filter_active_tainted_assumes stmt state =
    let assume_stmts =
      Stmt.Set.filter
        (fun assume_stmt -> not (Dominators.postdominates stmt assume_stmt))
        state.assume_stmts
    in
    { state with assume_stmts }

  (* No update about taint wrt information provided by the other domains. *)
  let _update _valuation state = `Value state

  (* Given a lvalue, returns:
     - its memory location (as a zone);
     - its indirect dependencies, i.e. the memory zone its location depends on;
     - whether its location is a singleton. *)
  let lval_deps to_loc lval =
    match (lval : Eva_ast.lval).node with
    | Var vi, NoOffset ->
      (* Special case for direct access to variable: do not use [to_loc] here,
         as it will fail for the formal parameters of calls. *)
      let zone = Locations.zone_of_varinfo vi in
      zone, Memory_zone.bottom, true
    | _ ->
      let ploc = to_loc lval in
      let is_singleton = Precise_locs.cardinal_zero_or_one ploc in
      let Deps.{ data; indirect } =
        Eva_ast.PreciseDepsOf.deps_of_lval to_loc Write lval
      in
      data, indirect, is_singleton

  let bottom_loc =
    let size = Z_or_top.of_int 0 in
    Precise_locs.make_precise_loc Precise_locs.bottom_addr_bits ~size

  let dont_taint_singleton valuation to_loc =
    fun lval ->
    let lv_exp = Eva_ast.Build.lval lval in
    match valuation.Abstract_domain.find lv_exp with
    | `Top ->
      to_loc lval
    | `Value r ->
      match r.value.v with
      | `Bottom -> bottom_loc
      | `Value v ->
        if Cvalue.V.cardinal_zero_or_one v then bottom_loc else to_loc lval

  let is_in_tainted_scope state =
    (* Current state defines a tainted scope if:
       - the current call depends on a tainted assume statement of a caller;
       - the current statement depends on a tainted assume statement. *)
    state.tainted_call
    || not (Stmt.Set.is_empty state.assume_stmts)

  (* Propagates data- and control-taints for an assignment [lval = exp]. *)
  let assign_aux ~namespace ~pos lval exp v to_loc state =
    let lv_data, lv_indirect, is_singleton = lval_deps to_loc lval in
    let exp_deps =
      let to_loc =
        if ignore_singletons () then
          (* Do not data-taint [lval] in case it contains a singleton value. *)
          dont_taint_singleton v to_loc
        else
          (* Data-taint [lval] in case a memory location on which the value of
             [exp] depends on is data-tainted. *)
          to_loc
      in
      Eva_ast.PreciseDepsOf.deps_of_exp to_loc exp
    in
    let data_tainted = Memory_zone.intersects state.locs_data exp_deps.data in
    (* [lval] becomes control-tainted if:
       - the assignment is in a tainted scope;
       - the [lval] location depends on tainted values;
       - the value of [exp] is control-tainted;
       - the address of a location read to compute the value of [exp] depends on
         tainted values. *)
    let ctrl_tainted =
      is_in_tainted_scope state
      || LatticeSingleTaint.intersects state lv_indirect
      || Memory_zone.intersects state.locs_control exp_deps.data
      || LatticeSingleTaint.intersects state exp_deps.indirect
    in
    if is_private_namespace namespace
    then warn_assign_interference ~pos ~data_tainted ~ctrl_tainted lv_data;
    let update tainted locs =
      if tainted
      then Memory_zone.join locs lv_data
      else if is_singleton
      then Memory_zone.diff locs lv_data
      else locs
    in
    { state with locs_data = update data_tainted state.locs_data;
                 locs_control = update ctrl_tainted state.locs_control; }

  let assign ~namespace ~pos lv exp _v valuation state =
    match Position.stmt pos with
    | None ->
      state
    | Some stmt ->
      let state = filter_active_tainted_assumes stmt state in
      let to_loc = loc_of_lval valuation in
      assign_aux ~namespace ~pos lv.Eval.lval exp valuation to_loc state

  let assume ~namespace ~pos exp _b valuation state =
    match Position.stmt pos with
    | None ->
      state
    | Some stmt ->
      let state = filter_active_tainted_assumes stmt state in
      (* Add [stmt] as assume statement in [state] as soon as [exp] is tainted. *)
      let to_loc = loc_of_lval valuation in
      let exp_zone = Eva_ast.PreciseDepsOf.zone_of_exp to_loc exp in
      let tainted = LatticeSingleTaint.intersects state exp_zone in
      if tainted && is_private_namespace namespace
      then warn_assume_interference ~pos exp_zone;
      if tainted && not state.tainted_call
      then { state with assume_stmts = Stmt.Set.add stmt state.assume_stmts; }
      else state

  let start_call ~namespace ~pos call _recursion valuation state =
    let stmt = Position.Local.stmt pos in
    let state = filter_active_tainted_assumes stmt state in
    let tainted_call = is_in_tainted_scope state in
    let state = { state with assume_stmts = Stmt.Set.empty; tainted_call } in
    (* Add tainted actual parameters in [state]. *)
    let to_loc = loc_of_lval valuation in
    List.fold_left
      (fun s { Eval.concrete; formal; _ } ->
         assign_aux ~namespace ~pos:(Position.of_local pos)
           (Eva_ast.Build.var formal) concrete valuation to_loc s)
      state
      call.Eval.arguments

  let get_formats_number s =
    let split = String.split_on_char '%' s in
    List.length split - 1

  (* If [kf] is a known variadic function, returns the position of the
     expected string literal argument; returns None otherwise. *)
  let is_auto_taint_variadic kf =
    let vi = Kernel_function.get_vi kf in
    if not (Ast_attributes.contains "fc_stdlib_generated" vi.vattr)
    then None
    else List.assoc_opt vi.vorig_name auto_taint_variadic_functions

  let is_auto_taint_arg kf =
    let vi = Kernel_function.get_vi kf in
    List.mem vi.vorig_name auto_taint_arg_functions

  let arg_to_zone arg =
    match Eval.(value_assigned arg.avalue) with
    | `Bottom -> Memory_zone.bottom (* should not happen *)
    | `Value value ->
      let addr_bits = Addresses.Bits.of_bytes value in
      let size = Bit_utils.sizeof_pointed arg.formal.vtype in
      let loc = Locations.make addr_bits size in
      Locations.enumerate_valid_bits Write loc

  let rec get_n_first l n =
    match l with
    | curr :: rest when n > 0 -> curr :: get_n_first rest (n - 1)
    | _ -> []

  (* Can be replaced by List.drop with OCaml 5.3. *)
  let rec drop n = function
    | _elt :: l when n > 0 -> drop (n - 1) l
    | l -> l

  let rec find_tainted_argument args =
    match args with
    | [] -> raise Not_found
    | arg :: rest ->
      match arg.Eval.formal.vtype.tnode with
      | TPtr _ | TArray _ -> arg
      | _ -> find_tainted_argument rest

  let is_auto_taint_res kf =
    let vi = Kernel_function.get_vi kf in
    List.mem vi.vorig_name auto_taint_res_functions

  let zone_of_return ret =
    match ret with
    | Some vi ->
      let loc = Locations.of_varinfo vi in
      Locations.enumerate_valid_bits Write loc
    | _ -> Memory_zone.bottom

  let finalize_call ~pos:_ _call _recursion ~pre ~post =
    (* Recover assume statements from the [pre] abstract state: we assume the
       control-dependency does not extended beyond the function scope. *)
    { post with assume_stmts = pre.assume_stmts;
                tainted_call = pre.tainted_call; }

  (* Adds automatic taint from [call] to [state] for some libc functions.
     Should be called after [finalize_call] only if -eva-taint-auto is set. *)
  let add_call_auto_taint call state =
    match is_auto_taint_variadic call.Eval.kf with
    | Some str_literal_pos ->
      begin
        match drop str_literal_pos call.arguments with
        | { concrete = { node = StartOf { node = (Var vi,NoOffset)} } } :: rest
          when Ast_info.is_string_literal vi ->
          begin
            match Globals.Vars.get_string_literal vi with
            | Str s ->
              let zones = List.map arg_to_zone rest in
              let n = get_formats_number s in
              let vars_to_taint = get_n_first zones n in
              let locs_data =
                List.fold_left Memory_zone.join state.locs_data vars_to_taint
              in
              { state with locs_data }
            | Wstr _ -> state
          end
        | _ -> state
      end
    | None ->
      if is_auto_taint_arg call.kf then
        begin
          try
            let to_taint = find_tainted_argument call.arguments in
            let zone = arg_to_zone to_taint in
            { state with locs_data = Memory_zone.join state.locs_data zone }
          with
          | Not_found -> state
        end
      else if is_auto_taint_res call.kf then
        begin
          let zone = zone_of_return call.return in
          { state with locs_data = Memory_zone.join state.locs_data zone }
        end
      else
        state

  let show_expr valuation state fmt exp =
    let to_loc = loc_of_lval valuation in
    let exp_zone = Eva_ast.PreciseDepsOf.zone_of_exp to_loc exp in
    Format.fprintf fmt "%B" (LatticeSingleTaint.intersects state exp_zone)
end

(* -------------------------------------------------------------------------- *)
(*                            Multi-taint domain                              *)
(* -------------------------------------------------------------------------- *)

module TransferMultiTaint = struct

  let update _valuation state_map = `Value state_map

  let assign ~pos lv exp v valuation state =
    `Value (
      let open Lattice_bounds.Top.Operators in
      let+ state_map = state in
      let assign_per_taint namespace state =
        TransferSingleTaint.assign ~namespace ~pos lv exp v valuation state
      in
      LatticeMultiTaint.mapi assign_per_taint state_map)

  let assume ~pos exp b valuation state =
    `Value (
      let open Lattice_bounds.Top.Operators in
      let+ state_map = state in
      let assume_per_taint namespace state =
        TransferSingleTaint.assume ~namespace ~pos exp b valuation state
      in
      LatticeMultiTaint.mapi assume_per_taint state_map)

  let start_call ~pos call recursion valuation state =
    `Value (
      let open Lattice_bounds.Top.Operators in
      let+ state_map = state in
      let start_call_per_taint namespace =
        TransferSingleTaint.start_call ~namespace ~pos call recursion valuation
      in
      LatticeMultiTaint.mapi start_call_per_taint state_map)

  let finalize_call ~pos call recursion ~pre ~post =
    `Value (
      let open Lattice_bounds.Top.Operators in
      let+ pre and+ post in
      let get_or_empty = function
        | None -> LatticeSingleTaint.empty
        | Some state -> state
      in
      (* Finalizes taint state for each taint label. *)
      let merge_per_key _key pre_opt post_opt =
        let pre = get_or_empty pre_opt
        and post = get_or_empty post_opt in
        let state =
          TransferSingleTaint.finalize_call ~pos call recursion ~pre ~post
        in
        if LatticeSingleTaint.(equal empty state) then None else Some state
      in
      let map_state = LatticeMultiTaint.merge merge_per_key pre post in
      (* Adds auto taints if -eva-taint-auto is set. *)
      let map_state =
        if auto_taint () then
          let auto_state = LatticeMultiTaint.find_or_empty "auto" map_state in
          let auto_state =
            TransferSingleTaint.add_call_auto_taint call auto_state
          in
          LatticeMultiTaint.add "auto" auto_state map_state
        else map_state
      in
      map_state)

  let show_expr valuation state fmt exp =
    let show_expr_per_taint namespace state =
      Format.fprintf fmt "%s@." namespace;
      TransferSingleTaint.show_expr valuation state fmt exp
    in
    Lattice_bounds.Top.iter (LatticeMultiTaint.iter show_expr_per_taint) state
end


module QueriesTaint = struct

  let top_query = `Value (Cvalue.V.top, None), Alarmset.all

  let extract_expr ~oracle:_ _context _state _expr = top_query
  let extract_lval ~oracle:_ _context _state _lv _locs = top_query

end


module Domain = struct
  type state = LatticeMultiTaint.t
  type value = Cvalue.V.t
  type location = Precise_locs.precise_location
  type origin

  let value_dependencies = Main_values.cval
  let location_dependencies = Main_locations.ploc

  include (LatticeMultiTaint: sig
             include Datatype.S_with_collections with type t = state
             include Abstract_domain.Lattice with type state := state
           end)

  include Domain_builder.Complete (LatticeMultiTaint)

  include QueriesTaint

  include (TransferMultiTaint: Abstract_domain.Transfer
           with type state := state
            and type value := value
            and type location := location
            and type origin := origin)


  (* Logic. *)

  let logic_assign_per_taint assign location state =
    let exists_tainted_from state deps =
      let single_from_contents dep =
        match dep.Eval.location with
        | Address _ -> false
        | Location location ->
          let loc_zone = Precise_locs.enumerate_valid_bits Read location in
          LatticeSingleTaint.intersects state loc_zone
      in
      List.exists single_from_contents deps
    in
    match assign with
    | (_, taint) when taint = LatticeSingleTaint.empty ->
      state
    | ((Eval.Frees _ | Allocates _), _) ->
      state
    | (Assigns (_, deps), pre_state) ->
      if exists_tainted_from pre_state deps
      then
        let loc_zone = Precise_locs.enumerate_valid_bits Write location in
        { state with locs_data = Memory_zone.join state.locs_data loc_zone }
      else
        state

  let logic_assign assign location state =
    match assign with
    | None -> state
    | Some (loc_assign, taint) ->
      let open Lattice_bounds.Top.Operators in
      let+ state_map = state and+ taint_map = taint in
      LatticeMultiTaint.mapi (fun key state ->
          let current_taint = LatticeMultiTaint.find_or_empty key taint_map in
          logic_assign_per_taint (loc_assign, current_taint) location state)
        state_map

  (* Scoping and Initialization. *)

  let enter_scope _kind vars state =
    if not (secure_flow_analysis ()) then
      state
    else
      let namespace = private_taint_namespace in
      let is_private vi = Ast_types.has_qualifier namespace vi.vtype in
      match List.filter is_private vars with
      | [] -> state
      | private_vars ->
        let var_zones = List.map Locations.zone_of_varinfo private_vars in
        let private_zone = List.fold_left Memory_zone.join Memory_zone.bottom var_zones in
        let open Lattice_bounds.Top.Operators in
        let+ state_map = state in
        let taint_state = LatticeMultiTaint.find_or_empty namespace state_map in
        let locs_data = Memory_zone.join taint_state.locs_data private_zone in
        let taint_state = { taint_state with locs_data } in
        LatticeMultiTaint.add namespace taint_state state_map

  let remove_bases_per_taint bases state =
    let remove = Memory_zone.filter_base (fun b -> not (Base.Hptset.mem b bases)) in
    { state with locs_data = remove state.locs_data;
                 locs_control = remove state.locs_control; }

  let remove_bases bases state =
    let open Lattice_bounds.Top.Operators in
    let+ state_map = state in
    LatticeMultiTaint.map (remove_bases_per_taint bases) state_map

  let leave_scope _kf vars state =
    let bases = Base.Hptset.of_list (List.map Base.of_varinfo vars) in
    remove_bases bases state


  (* Initial state: initializers are singletons, so we store nothing. *)
  let empty () = LatticeMultiTaint.empty
  let initialize_variable _ _ ~initialized:_ _ state = state
  let initialize_variable_using_type _ _ state  = state


  (* MemExec cache. *)
  let relate _bases _state = Base.SetLattice.empty

  let filter bases state =
    let open Lattice_bounds.Top.Operators in
    let+ state_map = state in
    let filter_state bases state =
      let filter_base = Memory_zone.filter_base (fun b -> Base.Hptset.mem b bases) in
      { state with locs_data = filter_base state.locs_data;
                   locs_control = filter_base state.locs_control;
                   assume_stmts = Stmt.Set.empty; }
    in
    LatticeMultiTaint.map (filter_state bases) state_map

  let project = filter

  let overwrite bases ~on:state ~by =
    let state = remove_bases bases state in
    LatticeMultiTaint.join state by

  let reuse bases ~current_input ~previous_output =
    overwrite bases ~on:current_input ~by:previous_output
end

include Domain

(* Registers the domain. *)
let registered =
  let name = "taint"
  and descr = "Taint analysis" in
  let auto_enable = Parameters.SecureFlow.get in
  Abstractions.Domain.register ~name ~descr ~priority:6
    ~experimental:true ~auto_enable (module Domain)

(* -------------------------------------------------------------------------- *)
(*                        Register taint annotations                          *)
(* -------------------------------------------------------------------------- *)

exception Parse_error of string option

let error ?msg loc typing_context =
  typing_context.Logic_typing.error loc
    "invalid taint annotation %a"
    (Pretty_utils.pp_opt ~pre:": " Format.pp_print_string) msg

let _parse_error ?msg () = raise (Parse_error msg)

(* Registers ACSL builtin predicates. *)
let () =
  let a_names = [
    "tainted"; (* Both direct (data) and indirect (control) taints. *)
    "tainted_directly"; (* Only direct (data) taints. *)
    "tainted_indirectly" (* Only indirect (control) taints. *)
  ]
  in
  let mk_builtin_logic_info a_name =
    { bl_name = "\\" ^ a_name;
      bl_labels = [];
      bl_params = [ a_name ];
      bl_type = None;
      bl_profile = ["p", Lvar a_name];
    }
  in
  List.iter
    (fun a_name -> Logic_builtin.register (mk_builtin_logic_info a_name))
    a_names

(* Registers ACSL logic function security_status. *)
let security_status_lf_name = "security_status"
let is_security_status = String.equal security_status_lf_name
let () =
  let security_status_lf =
    { bl_name = security_status_lf_name;
      bl_labels = [];
      bl_params = ["x"];
      bl_type = Some (Ctype Cil_const.intType);
      bl_profile = [("x", Lvar "x")];
    }
  in
  if not (Logic_env.is_logic_function security_status_lf_name)
  then Logic_builtin.register security_status_lf

(* Registers ACSL logic constants public/private. *)
let () =
  let mk_builtin_logic_info a_name =
    { bl_name = a_name;
      bl_labels = [];
      bl_params = [];
      bl_type = Some (Ctype Cil_const.intType);
      bl_profile = [];
    }
  in
  List.iter
    (fun a_name ->
       if not (Logic_env.is_logic_function a_name)
       then Logic_builtin.register (mk_builtin_logic_info a_name))
    [public_taint_namespace; private_taint_namespace]

(* Registers AST attributes corresponding to public/private taint namespaces. *)
let () =
  let register_ast_attribute_type attr =
    let a_class = Ast_attributes.AttrType in
    match Ast_attributes.find_known attr with
    | Some { attr_class } when attr_class = a_class -> ()
    | None | Some _ -> Ast_attributes.register ~ignore:true a_class attr
  in
  List.iter register_ast_attribute_type
    [public_taint_namespace; private_taint_namespace]

let rec parse_lval names kind typing_context loc arg =
  match arg.Logic_ptree.lexpr_node with
  | PLnamed (name, node) ->
    (* name:x to taint variable x in 'name' namespace *)
    let names = if List.mem name names then names else name :: names in
    parse_lval names kind typing_context loc node
  | PLconstant (StringConstant str) ->
    Logic_const.tstring ~loc str
  | _ ->
    let open Logic_typing in
    let get_state context =
      match kind with
      | `Pre -> context.pre_state
      | `Post -> context.post_state [Normal]
    in
    let term =
      typing_context.type_term typing_context (get_state typing_context) arg
    in
    { term with term_name = names }

let terms_of_parsed_taint_namespaces typing_context loc args kind =
  try
    List.map (parse_lval [] kind typing_context loc) args
  with
  | Parse_error msg ->
    error ?msg loc typing_context

(* Registers ACSL extension "taint" (statement annotation)
   and "taints" (behavior extension). *)
let () =
  let typer kind context loc args =
    Ext_terms (terms_of_parsed_taint_namespaces context loc args kind)
  in
  Acsl_extension.register_behavior ~plugin:"eva" "taints" (typer `Post) false;
  Acsl_extension.register_code_annot_next_stmt ~plugin:"eva" "taint"
    (typer `Pre) false

(* -------------------------------------------------------------------------- *)
(*                     Interpretation of taint annotations                    *)
(* -------------------------------------------------------------------------- *)

type taint_predicate =
  { namespaces: string list; (* List of taint namespaces. *)
    name: string; (* \tainted, \tainted_directly or \tainted_indirectly *)
    arg: term; (* The predicate is applied to this term. *)
    positive: bool; (* If false, negation of the predicate. *)
  }

(* The taint namespace of a term is stored as its term name.
   If no term name is present, the term namespace defaults to "default". *)
let term_taint_namespaces term =
  if term.term_name = [] then [ default_taint_namespace ] else term.term_name

let is_tainted_name = function
  | "\\tainted" | "\\tainted_directly" | "\\tainted_indirectly" -> true
  | _ -> false

let find_security_status ~positive term =
  match term.term_node with
  | TLval (TVar { lv_name }, TNoOffset) when is_private_namespace lv_name ->
    Some positive
  | TLval (TVar { lv_name }, TNoOffset) when is_public_namespace lv_name ->
    Some (not positive)
  | _ -> None

(* Returns a [taint_predicate] if [predicate] is a \tainted predicate.
   Relations such as "security_status(arg) = private" are considered as a
   \tainted(private:arg) predicate. Returns None if the [predicate] cannot
   be interpreted as a \tainted predicate. *)
let find_tainted_predicate ?(positive=true) predicate =
  match predicate.pred_content with
  | Papp ({l_var_info = { lv_name }}, _, [arg]) when is_tainted_name lv_name ->
    let namespaces = term_taint_namespaces arg in
    Some { namespaces; name = lv_name; arg; positive }
  | Prel ((Req | Rneq as op), {term_node = Tapp (f, _, [arg])}, t)
  | Prel ((Req | Rneq as op), t, {term_node = Tapp (f, _, [arg])}) ->
    if is_security_status f.l_var_info.lv_name then
      let open Option.Operators in
      let+ status = find_security_status ~positive t in
      let positive = if op = Rneq then not status else status in
      let namespaces = [ private_taint_namespace ] in
      let name = "\\tainted" in
      { namespaces; name; arg; positive }
    else None
  | _ -> None


(* Interpretation of logic by the taint domain, using the cvalue domain. *)
module TaintLogic = struct

  let eval_tlval_zone cvalue_env term =
    let alarm_mode = Eval_terms.Fail in
    try
      let access = Locations.Read in
      Some (Eval_terms.eval_tlval_as_zone_under_over
              ~alarm_mode access cvalue_env term)
    with Eval_terms.LogicEvalError _ -> None

  let eval_term_deps cvalue_env term =
    let alarm_mode = Eval_terms.Fail in
    try
      let result = Eval_terms.eval_term ~alarm_mode cvalue_env term in
      match Logic_label.Map.bindings result.ldeps with
      | [ BuiltinLabel Here, zone ] -> Some (Memory_zone.bottom, zone)
      | _ -> None
    with Eval_terms.LogicEvalError _ -> None

  let eval_term_zone cvalue_env term =
    match eval_tlval_zone cvalue_env term with
    | Some _ as x -> x
    | None -> eval_term_deps cvalue_env term

  let reduce_by_taint_predicate cvalue_env state taint_predicate =
    match eval_term_zone cvalue_env taint_predicate.arg with
    | None -> state
    | Some (under, _over) ->
      let zone_op = if taint_predicate.positive then Memory_zone.join else Memory_zone.diff in
      let reduce state =
        match taint_predicate.name with
        | "\\tainted" ->
          { state with locs_data = zone_op state.locs_data under;
                       locs_control = zone_op state.locs_control under; }
        | "\\tainted_directly" ->
          { state with locs_data = zone_op state.locs_data under }
        | "\\tainted_indirectly" ->
          { state with locs_control = zone_op state.locs_control under }
        | _ -> state
      in
      let open Lattice_bounds.Top.Operators in
      let+ state_map = state in
      let should_reduce name = List.mem name taint_predicate.namespaces in
      LatticeMultiTaint.mapi
        (fun name state -> if should_reduce name then reduce state else state)
        state_map

  let rec reduce_by_predicate cvalue_env state predicate positive =
    match positive, predicate.pred_content with
    | true, Pand (p1, p2)
    | false, Por (p1, p2) ->
      let state = reduce_by_predicate cvalue_env state p1 positive in
      reduce_by_predicate cvalue_env state p2 positive
    | true, Por (p1, p2)
    | false, Pand (p1, p2) ->
      let state1 = reduce_by_predicate cvalue_env state p1 positive in
      let state2 = reduce_by_predicate cvalue_env state p2 positive in
      join state1 state2
    | _, Pnot p -> reduce_by_predicate cvalue_env state p (not positive)
    | _ ->
      match find_tainted_predicate ~positive predicate with
      | Some taint_pred -> reduce_by_taint_predicate cvalue_env state taint_pred
      | None -> state

  let evaluate_taint_term cvalue_env zone term =
    match eval_term_zone cvalue_env term with
    | None -> Alarmset.Unknown
    | Some (_under, over) ->
      if Memory_zone.intersects over zone
      then Alarmset.Unknown
      else Alarmset.False

  let evaluate_taint_predicate cvalue_env state predicate : Alarmset.status =
    match state with
    | `Top -> Unknown
    | `Value state_map ->
      let get_zone state =
        match predicate.name with
        | "\\tainted_directly" -> state.locs_data
        | "\\tainted_indirectly" -> state.locs_control
        | "\\tainted" | _ -> Memory_zone.join state.locs_data state.locs_control
      in
      let add_zone acc namespace =
        let state = LatticeMultiTaint.find_or_empty namespace state_map in
        Memory_zone.join acc (get_zone state)
      in
      let zone = List.fold_left add_zone Memory_zone.bottom predicate.namespaces in
      let truth = evaluate_taint_term cvalue_env zone predicate.arg in
      if predicate.positive then truth else Abstract_interp.inv_truth truth

  let evaluate_predicate cvalue_env state predicate =
    let rec evaluate predicate : Alarmset.status =
      match predicate.pred_content with
      | Ptrue -> True
      | Pfalse -> False
      | Pand (p1, p2) ->
        begin
          match evaluate p1, evaluate p2 with
          | True, True -> True
          | False, _ | _, False -> False
          | _ -> Unknown
        end
      | Por (p1, p2) ->
        begin
          match evaluate p1, evaluate p2 with
          | True, _ | _, True -> True
          | False, False -> False
          | _ -> Unknown
        end
      | Pnot p -> Abstract_interp.inv_truth (evaluate p)
      | _ ->
        match find_tainted_predicate predicate with
        | Some pred -> evaluate_taint_predicate cvalue_env state pred
        | None -> Unknown
    in
    evaluate predicate

  let interpret_taint_extension cvalue_env state terms =
    let taint_term state_map term =
      match eval_tlval_zone cvalue_env term with
      | None ->
        Self.warning ~wkey ~current:true ~once:true
          "Cannot evaluate term %a in taint annotation; ignoring."
          Printer.pp_term term;
        state_map
      | Some (under, over) ->
        if not (Memory_zone.equal under over)
        then
          Self.warning ~wkey ~current:true ~once:true
            "Cannot precisely evaluate term %a in taint annotation; \
             over-approximating."
            Printer.pp_term term;
        let taint_names = term_taint_namespaces term in
        let add_taint state name =
          let taint = LatticeMultiTaint.find_or_empty name state in
          let locs_data = Memory_zone.join taint.locs_data over in
          LatticeMultiTaint.add name { taint with locs_data } state
        in
        List.fold_left add_taint state_map taint_names
    in
    let open Lattice_bounds.Top.Operators in
    let+ state_map = state in
    List.fold_left taint_term state_map terms
end

let interpret_taint_logic
    (module Abstract: Abstractions.S) : (module Abstractions.S) =
  match Abstract.Dom.get Cvalue_domain.State.key, Abstract.Dom.get key with
  | None, _
  | _, None -> (module Abstract)
  | Some get_cvalue_state, Some get_taint_state ->
    let module Dom = struct
      include Abstract.Dom

      let get_states env state =
        let taint = get_taint_state state in
        let get_cvalue state = fst (get_cvalue_state state) in
        let states label = get_cvalue (env.Abstract_domain.states label) in
        let cvalue_env = Abstract_domain.{ env with states = states } in
        Eval_terms.make_env cvalue_env (get_cvalue state), taint

      let evaluate_predicate env state predicate =
        match evaluate_predicate env state predicate with
        | Unknown ->
          let cvalue_env, taint = get_states env state in
          TaintLogic.evaluate_predicate cvalue_env taint predicate
        | x -> x

      let reduce_by_predicate env state predicate positive =
        match reduce_by_predicate env state predicate positive with
        | `Bottom -> `Bottom
        | `Value state ->
          let cvalue_env, taint = get_states env state in
          let taint =
            TaintLogic.reduce_by_predicate cvalue_env taint predicate positive
          in
          `Value (Abstract.Dom.set key taint state)

      let interpret_acsl_extension extension env state =
        if String.equal extension.ext_name "taint"
        || String.equal extension.ext_name "taints"
        then
          match extension.ext_kind with
          | Ext_terms terms ->
            let cvalue_env, taint = get_states env state in
            let taint =
              TaintLogic.interpret_taint_extension cvalue_env taint terms
            in
            Abstract.Dom.set key taint state
          | _ ->
            Self.warning ~wkey ~current:true ~once:true
              "Invalid taint annotation %a; ignoring."
              Printer.pp_extended extension;
            state
        else state
    end
    in
    (module struct
      module Ctx = Abstract.Ctx
      module Val = Abstract.Val
      module Loc = Abstract.Loc
      module Dom = Dom
    end)

let () = Abstractions.Hooks.register interpret_taint_logic

(* -------------------------------------------------------------------------- *)
(*                                   API                                      *)
(* -------------------------------------------------------------------------- *)

type taint = Direct | Indirect | Untainted

let is_tainted ?(names=[]) state zone =
  let taint_state =
    match state, names with
    | `Top, _ -> LatticeSingleTaint.top
    | `Value state_map, [] ->
      LatticeMultiTaint.fold (fun _name -> LatticeSingleTaint.join)
        state_map LatticeSingleTaint.empty
    | `Value state_map, names ->
      List.fold_left
        (fun acc name ->
           LatticeSingleTaint.join acc
             (LatticeMultiTaint.find_or_empty name state_map))
        LatticeSingleTaint.empty names
  in
  let { locs_data; locs_control } = taint_state in
  if Memory_zone.intersects zone locs_data then Direct
  else if Memory_zone.intersects zone locs_control then Indirect
  else Untainted

let taint_names () =
  LatticeMultiTaint.TaintNamesRef.get () |> Datatype.String.Set.elements

type taint_names_by_kind =
  { direct_taint_names: Datatype.String.Set.t;
    indirect_taint_names: Datatype.String.Set.t;
  }

let taint_names_by_kind state zone =
  let open Lattice_bounds.Top.Operators in
  let+ state_map = state in
  let add_name locs name names =
    if Memory_zone.intersects zone locs
    then Datatype.String.Set.add name names
    else names
  in
  let empty = Datatype.String.Set.empty, Datatype.String.Set.empty in
  let direct_taint_names, indirect_taint_names =
    LatticeMultiTaint.fold (fun name taint_state (direct, indirect) ->
        let direct = add_name taint_state.locs_data name direct in
        let indirect = add_name taint_state.locs_control name indirect in
        direct, indirect)
      state_map empty
  in
  { direct_taint_names; indirect_taint_names }
