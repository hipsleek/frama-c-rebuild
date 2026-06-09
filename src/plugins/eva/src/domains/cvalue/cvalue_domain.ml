(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

let dkey_cardinal = Self.register_category "cardinal" ~level:11
    ~help:"estimate the number of concrete states approximated by the analysis \
           at the end of each function"

(* Do not pretty Cil-generated variables or out-of-scope local variables *)
let filter_generated_and_locals kf =
  let fundec = Kernel_function.get_definition kf in
  (* only locals of outermost block *)
  let is_innerblock_local vi =
    Kernel_function.is_local vi kf
    && not (List.exists (Cil_datatype.Varinfo.equal vi) fundec.sbody.blocals)
  in
  function
  | Base.Var (vi, _) ->
    if vi.vtemp then vi.vname = "__retres" else not (is_innerblock_local vi)
  | _ -> true

(* Prints the final state [values] of function [kf] with outputs [outs]. *)
let print_final_state kf values =
  let outs = Eva_dynamic.Inout.kf_outputs kf in
  let outs = Memory_zone.filter_base (filter_generated_and_locals kf) outs in
  (* NB: there's no need to filter the variables representing string literals
     here, since by definition they won't appear as _output_ of any function. *)
  let print_filtered_state fmt =
    if Cvalue.Model.(equal values bottom)
    then Format.fprintf fmt "@[  NON TERMINATING FUNCTION@]"
    else
      let values =
        match outs with
        | Top (Top, _) ->
          Format.fprintf fmt "Cannot filter: dumping raw memory \
                              (including unchanged variables)@\n";
          values
        | Top (Set set, _) -> Cvalue.Model.filter_by_shape set values
        | Map m -> Cvalue.Model.filter_by_shape (Memory_zone.shape m) values
      in
      Format.fprintf fmt "@[  %a@]" Cvalue.Model.pretty values
  in
  let print_cardinal fmt =
    if Self.is_debug_key_enabled dkey_cardinal then
      Format.fprintf fmt " (~%a states)"
        Cvalue.CardinalEstimate.pretty (Cvalue.Model.cardinal_estimate values)
  in
  let header fmt =
    Format.fprintf fmt "Values at end of function %a:%t"
      Kernel_function.pretty kf print_cardinal
  in
  Self.printf ~dkey:Self.dkey_final_states ~header "%t" print_filtered_state

let pretty_wo_string_literal fmt s =
  let filtered =
    if Self.(is_debug_key_enabled dkey_include_string_literal)
    then s
    else Cvalue.Model.filter_base (fun b -> not (Base.is_string_literal b)) s
  in
  Cvalue.Model.pretty fmt filtered

module State = struct

  type state = Cvalue.Model.t * Locals_scoping.clobbered_set
  type value = Main_values.CVal.t
  type location = Main_locations.PLoc.location

  let value_dependencies = Main_values.cval
  let location_dependencies = Main_locations.ploc

  type context = unit
  let context_dependencies = Abstract_context.Leaf (module Unit_context)

  let log_category = Self.dkey_cvalue_domain

  include Datatype.Make_with_collections (
    struct
      include Datatype.Serializable_undefined
      type t = state
      let name = Cvalue.Model.datatype_name ^ "+clobbered_set"
      let reprs =
        List.map (fun s -> s, Locals_scoping.bottom ()) Cvalue.Model.reprs
      let structural_descr =
        Structural_descr.t_tuple
          [| Cvalue.Model.packed_descr; Locals_scoping.packed_descr |]
      let pretty fmt (s, _) = pretty_wo_string_literal fmt s
      let equal (a, _) (b, _) = Cvalue.Model.equal a b
      let compare (a, _) (b, _) = Cvalue.Model.compare a b
      let hash (s, _) = Cvalue.Model.hash s
      let rehash = Datatype.identity
      let copy = Datatype.undefined
      let mem_project = Datatype.never_any_project
    end )

  let name = "cvalue"
  let key = Structure.Key_Domain.create_key name

  let top = Cvalue.Model.top, Locals_scoping.bottom ()
  let is_included (a, _) (b, _) = Cvalue.Model.is_included a b
  let join (a, clob) (b, _) = Cvalue.Model.join a b, clob

  let widen kf stmt (a, clob) (b, _) =
    let priority, hint = Widen.getWidenHints kf stmt in
    Cvalue.Model.widen ~priority ~hint a b, clob

  let narrow (a, clob) (b, _) =
    let s = Cvalue.Model.narrow a b in
    if Cvalue.Model.(equal bottom s) then `Bottom else `Value (s, clob)

  type origin = Cvalue_queries.origin

  let extract_expr ~oracle context (state, _) expr =
    Cvalue_queries.extract_expr ~oracle context state expr

  let extract_lval ~oracle context (state, _) lval loc =
    Cvalue_queries.extract_lval ~oracle context state lval loc

  let backward_location (state, _) lval precise_loc value =
    Cvalue_queries.backward_location state lval precise_loc value

  let reduce_further (state, _) expr value =
    Cvalue_queries.reduce_further state expr value

  let build_context (state, _) = Cvalue_queries.build_context state

  (* ------------------------------------------------------------------------ *)
  (*                            Transfer Functions                            *)
  (* ------------------------------------------------------------------------ *)

  let update valuation (s, clob) =
    Cvalue_transfer.update valuation s >>-: fun s -> s, clob

  let assign ~pos lv expr assigned valuation (s, clob) =
    Cvalue_transfer.assign ~pos lv expr assigned valuation s >>-: fun s ->
    (* TODO: use the value in assignment *)
    let _ =
      Eval.value_assigned assigned >>-: fun value ->
      let location = Precise_locs.imprecise_location lv.lloc in
      Locals_scoping.remember_if_locals_in_value clob location value
    in
    s, clob

  let assume ~pos expr positive valuation (s, clob) =
    Cvalue_transfer.assume ~pos expr positive valuation s >>-: fun s ->
    s, clob

  let is_direct_recursion ~pos call =
    try
      let kf = Position.Local.kf pos in
      Kernel_function.equal kf call.kf
    with Not_found -> false (* Should not happen *)

  let start_recursive_call ~pos call recursion (state, clob) =
    let direct = is_direct_recursion ~pos call in
    let state = Cvalue.Model.remove_variables recursion.withdrawal state in
    let substitution = recursion.base_substitution in
    let clob = if direct then clob else Locals_scoping.top () in
    let state = Locals_scoping.substitute substitution clob state in
    Cvalue.Model.replace_base substitution state

  let start_call ~pos call recursion valuation (state, clob) =
    (* Uses the [valuation] to update the [state] before the substitution
       for recursive calls. *)
    Cvalue_transfer.update valuation state >>- fun state ->
    let state =
      match recursion with
      | None -> state
      | Some recursion -> start_recursive_call ~pos call recursion (state, clob)
    in
    Cvalue_transfer.start_call ~pos call recursion valuation state
    >>-: fun state -> state, Locals_scoping.bottom ()

  let finalize_recursive_call ~pos call ~pre recursion state =
    let direct = is_direct_recursion ~pos call in
    let pre, clob = pre in
    let substitution = recursion.base_substitution in
    let state = Cvalue.Model.replace_base substitution state in
    let clob = if direct then clob else Locals_scoping.top () in
    let state = Locals_scoping.substitute substitution clob state in
    let inter = Cvalue.Model.filter_by_shape recursion.base_withdrawal pre in
    Cvalue.Model.merge ~into:state inter

  let finalize_call ~pos call recursion ~pre ~post =
    let (pre, clob) = pre in
    let (post, post_clob) = post in
    Locals_scoping.(remember_bases_with_locals clob post_clob.clob);
    let finalize a =
      finalize_recursive_call ~pos call ~pre:(pre, clob) a post
    in
    let post = Option.fold ~some:finalize ~none:post recursion in
    (* Deallocate memory allocated via alloca().
       To minimize computations, only do it for function definitions. *)
    let post =
      if Kernel_function.is_definition call.kf then
        Builtins_malloc.free_automatic_bases call.callstack post
      else post
    in
    Cvalue_transfer.finalize_call ~pos call recursion ~pre ~post
    >>-: fun state ->
    state, clob

  let show_expr valuation (state, _) = Cvalue_transfer.show_expr valuation state

  (* ------------------------------------------------------------------------ *)
  (*                                 Mem Exec                                 *)
  (* ------------------------------------------------------------------------ *)

  let relate _bases _state = Base.SetLattice.empty

  (* Auxiliary function that keeps only some bases inside a memory state *)
  let filter bases (state, clob) =
    Cvalue.Model.filter_by_shape bases state, clob

  let project = filter

  let reuse _bases ~current_input:(state, _) ~previous_output:(output, clob) =
    Cvalue.Model.merge ~into:state output, clob

  let overwrite bases ~on:(state, clob) ~by =
    let (output, _) = project bases by in
    Cvalue.Model.merge ~into:state output, clob

  (* ------------------------------------------------------------------------ *)
  (*                                  Logic                                   *)
  (* ------------------------------------------------------------------------ *)

  let lift_env logic_env =
    Abstract_domain.{ states = (fun label -> fst (logic_env.states label));
                      result = logic_env.result; }

  let evaluate_predicate logic_env (state, _clob) pred =
    let eval_env = Eval_terms.make_env (lift_env logic_env) state in
    match Eval_terms.eval_predicate eval_env pred with
    | Eval_terms.True -> Alarmset.True
    | Eval_terms.False -> Alarmset.False
    | Eval_terms.Unknown -> Alarmset.Unknown

  let reduce_by_predicate logic_env (state, clob) pred b =
    let eval_env = Eval_terms.make_env (lift_env logic_env) state in
    let eval_env = Eval_terms.reduce_by_predicate eval_env b pred in
    let state = Eval_terms.env_current_state eval_env in
    if Cvalue.Model.is_reachable state
    then `Value (state, clob)
    else `Bottom

  let interpret_acsl_extension _extension _env state = state

  (* Computes an over-approximation of the value written by an assigns clause
     according to the list of dependencies stated by its \from part. *)
  let evaluate_from_clause state deps =
    (* Only direct dependencies  to the written value. *)
    let direct_deps = List.filter (fun dep -> dep.direct) deps in
    (* Evaluates the content of one dependency. *)
    let content dep =
      match dep.location with
      | Address vi ->
        (* Case "\from &x": the value depends of "&x" (not the value of x). *)
        Cvalue.V.inject (Base.of_varinfo vi) Ival.zero
      | Location location ->
        (* Use [conflate_bottom] to join all bits of the location content. *)
        let location = Precise_locs.imprecise_location location in
        Cvalue.Model.find ~conflate_bottom:false state location
    in
    let values = List.map content direct_deps in
    (* Join all values and topify them. *)
    let joined_value = List.fold_left Cvalue.V.join Cvalue.V.top_int values in
    Cvalue.V.topify Origin.Leaf joined_value

  let logic_assign logic_assign location (state, sclob) =
    match logic_assign with
    | None ->
      let location = Precise_locs.imprecise_location location
      and value = Cvalue.V.top in
      Cvalue.Model.add_binding ~exact:false state location value, sclob
    | Some (Assigns (_assign, deps), (pre_state, _)) ->
      let location = Precise_locs.imprecise_location location in
      let value = evaluate_from_clause pre_state deps in
      Locals_scoping.remember_if_locals_in_value sclob location value;
      Cvalue.Model.add_binding ~exact:false state location value, sclob
    | Some ((Frees _ | Allocates _), _) -> state, sclob

  (* ------------------------------------------------------------------------ *)
  (*                             Initialization                               *)
  (* ------------------------------------------------------------------------ *)

  let initialize_variable  _lval loc ~initialized init_value (state, clob) =
    let value = match init_value with
      | Abstract_domain.Top  -> Cvalue.V.top_int
      | Abstract_domain.Zero -> Cvalue.V.singleton_zero
    in
    let cvalue =
      if initialized
      then Cvalue.V_Or_Uninitialized.C_init_noesc value
      else Cvalue.V_Or_Uninitialized.C_uninit_noesc value
    in
    let loc = Precise_locs.imprecise_location loc in
    Cvalue.Model.add_indeterminate_binding ~exact:true state loc cvalue, clob

  let empty () =
    let open Cvalue in
    let state = Model.empty_map in
    let min_valid = Base.min_valid_absolute_address () in
    let max_valid = Base.max_valid_absolute_address () in
    if Z.leq min_valid max_valid
    then begin
      (* Bind everything between [0..max] to bottom. Offsetmaps cannot
         contain holes, which can happen when min > 0 holds. *)
      let bot =
        V_Offsetmap.create_isotropic
          ~size:max_valid (V_Or_Uninitialized.initialized V.bottom)
      in
      let v = if true (* TODO: command line option *)
        then V_Or_Uninitialized.initialized V.top_int
        else V_Or_Uninitialized.uninitialized
      in
      let offsm =
        V_Offsetmap.add
          (min_valid, max_valid) (v, Z.one, Abstract_interp.Rel.zero) bot
      in
      Cvalue.Model.add_base Base.null offsm state, Locals_scoping.bottom ()
    end
    else state, Locals_scoping.bottom ()

  let initialize_variable_using_type kind varinfo (state, clob) =
    match kind with
    | Abstract_domain.(Global | Formal _ | Local _) ->
      Cvalue_init.initialize_var_using_type varinfo state, clob
    | Abstract_domain.Result kf ->
      let value = Library_functions.returned_value kf in
      let loc = Locations.of_varinfo varinfo in
      Cvalue.Model.add_binding ~exact:true state loc value, clob

  (* ------------------------------------------------------------------------ *)
  (*                                  Misc                                    *)
  (* ------------------------------------------------------------------------ *)

  let bind_local state vi =
    let b = Base.of_varinfo vi in
    let offsm =
      if Parameters.InitializedLocals.get () then
        let v = Cvalue.(V_Or_Uninitialized.initialized V.top_int) in
        match Cvalue.V_Offsetmap.size_from_validity (Base.validity b) with
        | `Bottom -> assert false
        | `Value size -> Cvalue.V_Offsetmap.create_isotropic ~size v
      else
        Bottom.non_bottom (Cvalue.Default_offsetmap.default_offsetmap b)
    in
    Cvalue.Model.add_base b offsm state

  let bind_global state varinfo =
    let base = Base.of_varinfo varinfo in
    let loc = Locations.of_base base in
    let value = Cvalue.V_Or_Uninitialized.uninitialized in
    Cvalue.Model.add_indeterminate_binding ~exact:true state loc value

  let enter_scope kind vars (state, clob) =
    let bind =
      match kind with
      | Abstract_domain.Global -> bind_global
      | Abstract_domain.(Local _ | Formal _ | Result _)  -> bind_local
    in
    List.fold_left bind state vars, clob

  let leave_scope kf vars (state, clob) =
    let state = Cvalue.Model.remove_variables vars state in
    try
      let fdec = Kernel_function.get_definition kf in
      Locals_scoping.make_escaping_fundec fdec clob vars state, clob
    with Kernel_function.No_Definition -> state, clob

  let enter_loop _stmt (s, clob) = s, clob

  let leave_loop _stmt (s, clob) = s, clob

  let incr_loop_counter _stmt (s, clob) = s, clob


  (* ------------------------------------------------------------------------ *)
  (*                                Storage                                   *)
  (* ------------------------------------------------------------------------ *)

  (* Do not register the Locals_scoping.clobbered_set part of the state. *)
  module Store = struct
    let set_state ?callstack control_point (state, _clob) =
      Cvalue_results.set_state ?callstack control_point state

    let get_state ?callstack control_point =
      let open Lattice_bounds.TopBottom.Operators in
      let+ state = Cvalue_results.get_state ?callstack control_point in
      state, Locals_scoping.top ()

    let callstacks = Cvalue_results.callstacks
    let is_computed = Cvalue_results.is_computed
  end

  let get_state_before stmt =
    match Cvalue_results.get_state (Before stmt) with
    | `Bottom -> Cvalue.Model.bottom
    | `Top -> Cvalue.Model.top
    | `Value v -> v

  let display_final_state kf =
    try
      let values = get_state_before (Kernel_function.find_return kf) in
      let fst_values = get_state_before (Kernel_function.find_first_stmt kf) in
      if Cvalue.Model.(is_reachable fst_values && not (is_top fst_values))
      then print_final_state kf values
    with Kernel_function.No_Statement -> ()

  let display_results () =
    Self.result "====== VALUES COMPUTED ======";
    Eva_dynamic.Callgraph.iter_in_rev_order display_final_state

  let post_analysis _state =
    if Plugin.is_present "inout"
    && Self.is_debug_key_enabled Self.dkey_final_states
    && Self.verbose_atleast 1
    && Parameters.Eva.get ()
    then display_results ();
    Self.result "%t" Eva_perf.display

end


let registered =
  let name = "cvalue"
  and descr =
    "Main analysis domain, enabled by default. Should not be disabled."
  in
  Abstractions.Domain.register ~name ~descr ~priority:10 (module State)


type prefix = Hptmap.prefix
module Subpart = struct
  type t = Cvalue.Model.subtree
  let hash = Cvalue.Model.hash_subtree
  let equal = Cvalue.Model.equal_subtree
end
let distinct_subpart (a, _) (b, _) =
  if Cvalue.Model.equal a b then None
  else
    try Cvalue.Model.comp_prefixes a b; None
    with Cvalue.Model.Found_prefix (p, s1, s2) -> Some (p, s1, s2)
let find_subpart (s, _) prefix = Cvalue.Model.find_prefix s prefix


module type Getters = sig
  type t
  val get_cvalue : (t -> Cvalue.Model.t) option
  val get_cvalue_or_top : t -> Cvalue.Model.t
  val get_cvalue_or_bottom : t Lattice_bounds.or_bottom -> Cvalue.Model.t
end

module Getters (Dom : Abstract.Domain.External) = struct
  let get_cvalue =
    match Dom.get State.key with
    | None -> None
    | Some get -> Some (fun s -> fst (get s))

  let get_cvalue_or_top =
    match Dom.get State.key with
    | None -> fun _ -> Cvalue.Model.top
    | Some get -> fun s -> fst (get s)

  let get_cvalue_or_bottom = function
    | `Bottom -> Cvalue.Model.bottom
    | `Value state -> get_cvalue_or_top state
end
