(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

type 'value builtin = 'value list -> 'value or_bottom
type widen_hint = Z.Set.t * Datatype.Float.Set.t

module type Value = sig
  include Abstract_value.Leaf
  val widen : Addresses.Bytes.widen_hint -> t -> t -> t
  val track_variable: Cil_types.varinfo -> bool
  val builtins: (string * t builtin) list
end

module type S = sig
  type t
  type value
  val add: Precise_locs.precise_location -> Cil_types.typ -> value -> t -> t
  val find: Precise_locs.precise_location -> Cil_types.typ -> t -> value
  val remove: Precise_locs.precise_location -> t -> t
  val remove_variables: Cil_types.varinfo list -> t -> t
  val fold: (Base.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make_Memory (Info: sig val name: string end) (Value: Value) = struct

  module Hptmap_Info = struct
    let initial_values = []
    let dependencies = [ Ast.self ]
  end

  include Hptmap.Make (Base.Base) (Value) (Hptmap_Info)

  let name = Info.name

  let cache_name s =
    Hptmap_sig.PersistentCache ("Value." ^ name ^ "." ^ s)

  let disjoint_union =
    let cache = cache_name "union" in
    let decide _key _v1 _v2 = assert false in
    join ~cache ~symmetric:true ~idempotent:true ~decide

  let narrow =
    let module E = struct exception Bottom end in
    let cache = cache_name "narrow" in
    let decide _ v1 v2 =
      match Value.narrow v1 v2 with
      | `Bottom -> raise E.Bottom
      | `Value v -> v
    in
    fun a b ->
      try `Value (join ~cache ~symmetric:true ~idempotent:true ~decide a b)
      with E.Bottom -> `Bottom

  let join =
    let cache = cache_name "join" in
    let decide _ v1 v2 =
      let r = Value.join v1 v2 in
      if Value.(equal top r) then None else Some r
    in
    inter ~cache ~symmetric:true ~idempotent:true ~decide

  let widen kf stmt =
    let _, get_hints = Widen.getWidenHints kf stmt in
    let cache = cache_name "widen"  in
    let decide base b1 b2 =
      let widen_hints = get_hints base in
      let r = Value.widen widen_hints b1 b2 in
      if Value.(equal top r) then None else Some r
    in
    inter ~cache ~symmetric:false ~idempotent:true ~decide

  let is_included =
    let cache = cache_name "is_included" in
    let decide_fst _b _v1 = true (* v2 is top *) in
    let decide_snd _b _v2 = false (* v1 is top, v2 is not *) in
    let decide_both _ v1 v2 = Value.is_included v1 v2 in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let top = empty

  type loc_for_base = Precise | Imprecise

  (* Checks whether the offset [o] and the size [size] corresponds to the
     tracked location for [b].
     The conditions are as follow:
     - the variable corresponding to [b] is not volatile.
     - the variable corresponding to [b] must be tracked.
     - the location must assign the entire variable.
     - the type of the variable matches [typ]. *)
  let covers_base b o size typ =
    match b with
    | Base.Var (vi, Base.Known (_, max)) -> (* "standard" varinfos only *)
      if not (Ast_types.has_qualifier "volatile" vi.vtype) &&
         Value.track_variable vi &&
         Cil_datatype.Typ.equal typ vi.vtype &&
         Ival.is_zero o &&
         (match size with
          | `Value size -> Z.equal size (Z.succ max)
          | `Top -> false)
      then Precise
      else Imprecise
    | _ -> Imprecise

  let find_or_top b state = try find b state with Not_found -> Value.top

  let add loc typ v state =
    let loc = Precise_locs.imprecise_location loc in
    (* exact means that the location is precise and that we can perform
       a strong update. *)
    let exact = Addresses.Bits.cardinal_zero_or_one loc.addr in
    let aux_base b o state =
      match covers_base b o loc.size typ with
      | Precise ->
        (* The location exactly matches [b]: we are able to store the result.
           If the location is not exact, performs a weak update: join [v] with
           the current value for [b]. *)
        let v = if exact then v else Value.join v (find_or_top b state) in
        (* Store the new value unless it is top. In this case, drop it for
           canonicity. *)
        if Value.(equal v top)
        then remove b state
        else add b v state
      | Imprecise -> remove b state
    in
    try Addresses.Bits.fold_topset_ok aux_base loc.addr state
    with Abstract_interp.Error_Top -> empty

  let remove_variables vars state =
    let remove_variable state v = remove (Base.of_varinfo v) state in
    List.fold_left remove_variable state vars

  let remove loc state =
    let loc = Precise_locs.imprecise_location loc in
    Locations.(Addresses.Bits.fold_bases remove loc.addr state)

  let find loc typ state =
    let loc = Precise_locs.imprecise_location loc in
    let aux_base b o r =
      (* We degenerate to Top as soon as we find an imprecise location,
         or a base which is not bound in the map. *)
      match covers_base b o loc.size typ with
      | Precise -> Bottom.join Value.join r (`Value (find_or_top b state))
      | Imprecise -> `Value Value.top
    in
    try
      match Addresses.Bits.fold_topset_ok aux_base loc.addr `Bottom with
      | `Bottom -> Value.top (* does not happen if the location is not empty *)
      | `Value v -> v
    with Abstract_interp.Error_Top -> Value.top

end

module Make_Domain (Info: sig val name: string end) (Value: Value) = struct

  let table = Hashtbl.create 17
  let () =
    List.iter (fun (name, f) -> Hashtbl.replace table name f) Value.builtins

  let find_builtin name =
    try Some (Hashtbl.find table name)
    with Not_found -> None

  module M = Make_Memory (Info) (Value)
  include M

  include Domain_builder.Complete (M)

  type state = t
  type value = Value.t
  type location = Precise_locs.precise_location
  type origin

  let value_dependencies = Abstract_value.Leaf (module Value)
  let location_dependencies = Main_locations.ploc

  (* This function returns the information known about the location
     corresponding to [_lv], so that it may be used by the engine during
     evaluation. *)
  let extract_lval ~oracle:_ _context state lv loc =
    let v = find loc lv.Eva_ast.typ state in
    `Value (v, None), Alarmset.all

  let extract_expr ~oracle:_ _context _state _expr =
    `Value (Value.top, None), Alarmset.all

  let backward_location state lval loc _value =
    let new_value = find loc lval.Eva_ast.typ state in
    `Value (loc, new_value)

  (* This function binds [loc] to [v], of type [typ], in [state].
     [v] can be [`Bottom], which means that its contents are guaranteed
     to be indeterminate (e.g. uninitialized data). *)
  let bind_loc loc typ v state =
    match v with
    (* We are adding a "good" value. Store it in the state. *)
    | `Value v -> add loc typ v state
    (* Indeterminate value. Drop the information known for loc. *)
    | `Bottom -> remove loc state

  (* This function updates [state] with information for [expr], only possible
     when it is an lvalue. In this case, we can update the corresponding
     location with the result of the evaluation of [exp]. Both the value and
     the location are found in the [valuation]. *)
  let assume_exp valuation expr record state =
    match (expr : Eva_ast.exp).node with
    | Lval lv -> begin
        match valuation.Abstract_domain.find_loc lv with
        | `Top -> state
        | `Value {loc} ->
          if Precise_locs.cardinal_zero_or_one loc
          then bind_loc loc lv.typ record.value.v state
          else state
      end
    | _ -> state

  (* This function fills [state] according to the information available
     in [valuation]. This information is computed by Eva's engine for
     all the expressions involved in the current statement. *)
  let assume_valuation valuation state =
    valuation.Abstract_domain.fold (assume_exp valuation) state

  (* Abstraction of an assignment. *)
  let assign ~pos:_ lv _expr value valuation state =
    (* Update the state with the information obtained from evaluating
       [lv] and [e] *)
    let state = assume_valuation valuation state in
    (* Extract the abstract value *)
    let value = Eval.value_assigned value in
    (* Store the information [lv = e;] in the state *)
    let state = bind_loc lv.lloc lv.lval.typ value state in
    `Value state

  let update valuation state = `Value (assume_valuation valuation state)

  (* Abstraction of a conditional. All information inferred by the engine
     is present in the valuation, and must be stored in the memory
     abstraction of the domain itself. *)
  let assume ~pos:_ _expr _pos = update

  let start_recursive_call recursion state =
    let state = remove_variables recursion.withdrawal state in
    (* No collision should occur in the substitution. *)
    let decide _key _v1 _v2 = assert false in
    snd (replace_key ~decide recursion.base_substitution state)

  let start_call ~pos:_ call recursion _valuation state =
    let state =
      let start_recursive_call r = start_recursive_call r state in
      Option.fold ~some:start_recursive_call ~none:state recursion in
    let bind_argument state argument =
      let typ = argument.formal.vtype in
      let loc = Main_locations.PLoc.eval_varinfo argument.formal in
      let value = Eval.value_assigned argument.avalue in
      bind_loc loc typ value state
    in
    let state = List.fold_left bind_argument state call.arguments in
    `Value state

  let finalize_recursive_call ~pre recursion state =
    let inter = inter_with_shape recursion.base_withdrawal pre in
    let state = disjoint_union state inter in
    (* No collision should occur in the substitution. *)
    let decide _key _v1 _v2 = assert false in
    snd (replace_key ~decide recursion.base_substitution state)

  let finalize_call ~pos:_ call recursion ~pre ~post =
    let kf_name = Kernel_function.get_name call.kf in
    let finalize a = finalize_recursive_call ~pre a post in
    let post = Option.fold ~some:finalize ~none:post recursion in
    match find_builtin kf_name, call.return with
    | None, _ | _, None   -> `Value post
    | Some f, Some return ->
      let extract_value arg = Eval.value_assigned arg.avalue in
      let args = List.map extract_value call.arguments in
      if List.exists (function `Bottom -> true | `Value _ -> false) args
      then `Bottom
      else
        let args = List.map Bottom.non_bottom args in
        f args >>-: fun result ->
        let return_loc = Main_locations.PLoc.eval_varinfo return in
        bind_loc return_loc return.vtype (`Value result) post

  let show_expr valuation state fmt expr =
    match (expr : Eva_ast.exp).node with
    | Lval lval ->
      begin
        match valuation.Abstract_domain.find_loc lval with
        | `Top -> ()
        | `Value {loc} -> Value.pretty fmt (find loc lval.typ state)
      end
    | _ -> ()

  let enter_scope _kind _vars state = state
  let leave_scope _kf vars state = remove_variables vars state

  let logic_assign _assign location state = remove location state

  let empty () = top
  let initialize_variable _lval _location ~initialized:_ _value state = state
  let initialize_variable_using_type _kind _varinfo state = state

  let relate _bases _state = Base.SetLattice.empty

  let filter = M.inter_with_shape
  let project = filter

  let overwrite bases ~on ~by =
    let cache = Hptmap_sig.NoCache in
    let decide_both _key _v1 v2 = Some v2 in
    let decide_left key v1 =
      if Base.Hptset.mem key bases then None else Some v1
    in
    merge ~cache ~symmetric:false ~idempotent:true
      ~decide_both ~decide_left:(Traversing decide_left) ~decide_right:Neutral
      on by

  let reuse bases ~current_input ~previous_output =
    overwrite bases ~on:current_input ~by:previous_output

  let add = M.add
  let find = M.find
  let remove = M.remove
  let remove_variables = M.remove_variables
  let fold = M.fold
end
