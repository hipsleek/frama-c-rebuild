(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds

let are_available kf =
  Self.is_computed () &&
  match Function_calls.analysis_status kf with
  | Analyzed (Complete | Partial) -> true
  | SpecUsed | Builtin _ | Unreachable | Analyzed NoResults -> false

type 'a by_callstack = (Callstack.t * 'a) list

type context = {
  control_point : Domain_store.control_point;
  selector : Callstack.t list option;
  filter: (Callstack.t -> bool) list;
}

type request =
  | Point of context
  | Cvalue of Cvalue.Model.t

type error = Bottom | Top | DisabledDomain
type 'a result = ('a,error) Result.t

let string_of_error = function
  | Bottom -> "The computed state is bottom"
  | Top -> "The computed state is Top"
  | DisabledDomain -> "The required domain is disabled"

let pretty_error fmt error =
  Format.pp_print_string fmt (string_of_error error)
let pretty_result f fmt r =
  Result.fold ~ok:(f fmt) ~error:(pretty_error fmt) r


(* Building requests *)

let make control_point =
  Point { control_point; selector = None; filter = []; }

let before stmt = make (Before stmt)
let after stmt = make (After stmt)
let at_start_of kf = make (Start kf)
let at_end_of kf = make (After (Kernel_function.find_return kf))
let at_start = make Initial
let at_end () = at_end_of (fst (Globals.entry_point ()))

let before_kinstr = function
  | Cil_types.Kglobal -> at_start
  | Kstmt stmt -> before stmt

let change_context f = function
  | Point ctx -> Point (f ctx)
  | Cvalue _ as x -> x

let in_callstacks l =
  change_context (fun ctx -> { ctx with selector = Some l })

let in_callstack cs =
  change_context (fun ctx -> { ctx with selector = Some [cs] })

let filter_callstack f =
  change_context (fun ctx -> { ctx with filter = f :: ctx.filter })

let in_cvalue_state cvalue = Cvalue cvalue

let before_pos pos =
  match pos with
  | Position.RootCall { thread; entry_point } ->
    at_start_of entry_point
    |> in_callstack (Callstack.init ~thread ~entry_point)
  | Position.GlobalInit _ ->
    at_start
  | Position.Local local ->
    before (Position.Local.stmt local)
    |> in_callstack (Position.Local.callstack local)

let after_pos pos =
  match pos with
  | Position.RootCall { thread; entry_point } ->
    at_end_of entry_point
    |> in_callstack (Callstack.init ~thread ~entry_point)
  | Position.GlobalInit _ ->
    at_end ()
  | Position.Local local ->
    after (Position.Local.stmt local)
    |> in_callstack (Position.Local.callstack local)

(* Manipulating request results *)

type restricted_to_callstack = private [ `Restricted ]
type unrestricted_response = private [ `Unrestricted ]

module Response =
struct

  type ('a, 'callstack) t =
    | Consolidated : 'a -> ('a, unrestricted_response) t
    | ByCallstack  : 'a by_callstack -> ('a, 'callstack) t
    | Top : ('a, 'callstack) t
    | Bottom : ('a, 'callstack) t

  let coercion : ('a, restricted_to_callstack) t -> ('a, 'c) t = function
    | ByCallstack c -> ByCallstack c
    | Top -> Top
    | Bottom -> Bottom

  let is_bottom = function
    | ByCallstack [] | Bottom -> true
    | _ -> false

  (* Constructors *)

  let consolidated ~top =
    function
    | `Bottom -> Bottom
    | `Top -> Consolidated top
    | `Value state -> Consolidated state

  let by_callstack : context ->
    [< `Bottom | `Top | `Value of (Callstack.t * 'a) list ] ->
    ('a, restricted_to_callstack) t =
    fun req -> function
      | `Top -> Top
      | `Bottom -> Bottom
      | `Value table ->
        let keep (cs, _state) =
          List.for_all (fun filter -> filter cs) req.filter
          && Option.fold ~none:true ~some:(List.mem cs) req.selector
        in
        ByCallstack (List.filter keep table)

  (* Accessors *)

  let callstacks: ('a, restricted_to_callstack) t -> Callstack.t list = function
    | Top | Bottom -> [] (* What else to do when Top is given ? *)
    | ByCallstack l -> List.map fst l

  (* Fold *)

  let fold (f  : Callstack.t -> 'a -> 'b -> 'b) (acc : 'b) :
    ('a, restricted_to_callstack) t -> 'b =
    function
    | Top | Bottom -> acc (* What else to do when Top is given ? *)
    | ByCallstack l -> List.fold_left (fun acc (cs,x) -> f cs x acc) acc l

  (* Map *)

  let map : type c. ('a -> 'b) -> ('a, c) t -> ('b, c) t = fun f -> function
    | Consolidated v -> Consolidated (f v)
    | ByCallstack l -> ByCallstack (List.map (fun (cs,x) -> cs,f x) l)
    | Top -> Top
    | Bottom -> Bottom

  (* Reduction *)

  let map_reduce : type c. ([`Top | `Bottom] -> 'b) -> ('a -> 'b) ->
    ('b -> 'b -> 'b) -> ('a, c) t -> 'b =
    fun default map reduce -> function
      | Consolidated v -> map v
      | ByCallstack ((_,h) :: t) ->
        List.fold_left (fun acc (_,x) -> reduce acc (map x)) (map h) t
      | ByCallstack [] | Bottom -> default `Bottom
      | Top -> default `Top

  let default = function
    | `Bottom -> `Bottom
    | `Top -> `Top

  let map_join : type c.
    ('a -> 'b) -> ('b -> 'b -> 'b) -> ('a, c) t -> 'b or_top_bottom =
    fun map join ->
    let map' x = `Value (map x)
    and join' = TopBottom.join (fun x y -> `Value (join x y)) in
    map_reduce default map' join'

  let map_join' : type c. ('a -> [< 'b or_top_bottom]) -> ('b -> 'b -> 'b) ->
    ('a, c) t -> 'b or_top_bottom =
    fun map join ->
    let join' = TopBottom.join (fun x y -> `Value (join x y)) in
    map_reduce default map join'
end


(* Extracting states and values *)

type value = private [ `Value ]
type address = private [ `Address ]

module Make () =
struct
  open TopBottom.Operators

  module Engine = (val Engine.current ())

  module EvalTypes =
  struct
    type valuation = Engine.Eval.Valuation.t
    type exp = (valuation * Engine.Val.t) Eval.evaluated
    type lval = (valuation * Engine.Val.t Eval.flagged_value) Eval.evaluated
    type loc = (valuation * Engine.Loc.location) Eval.evaluated
  end

  type ('a,'c) evaluation =
    | LValue: (EvalTypes.lval, 'c) Response.t -> (value,'c) evaluation
    | Value: (EvalTypes.exp, 'c) Response.t -> (value,'c) evaluation
    | Address: (EvalTypes.loc, 'c) Response.t * Locations.access ->
        (address,'c) evaluation

  let get_from_cvalue cvalue =
    if Cvalue.Model.(equal cvalue bottom)
    then `Bottom
    else if Cvalue.Model.(equal cvalue top)
    then `Top
    else
      let state = cvalue, Locals_scoping.bottom () in
      `Value (Engine.Dom.set Cvalue_domain.State.key state Engine.Dom.top)

  let get_by_callstack (ctx : context) :
    (_, restricted_to_callstack) Response.t =
    let list =
      match ctx.selector with
      | Some [ callstack ] ->
        (* Optimization if only one callstack. *)
        let open Lattice_bounds.TopBottom.Operators in
        let+ state = Engine.get_state ~callstack ctx.control_point in
        [ callstack, state ]
      | _ ->
        Engine.get_state_by_callstack ctx.control_point
    in
    Response.by_callstack ctx list

  let get (req : request) : (_, unrestricted_response) Response.t =
    let open Response in
    match req with
    | Cvalue state -> consolidated ~top:Engine.Dom.top  (get_from_cvalue state)
    | Point req ->
      if req.filter <> [] || Option.is_some req.selector then
        Response.coercion @@ get_by_callstack req
      else
        let state = Engine.get_state req.control_point in
        consolidated ~top:Engine.Dom.top state

  let convert : 'a or_top_bottom -> 'a result = function
    | `Top -> Result.error Top
    | `Bottom -> Result.error Bottom
    | `Value v -> Result.ok v

  let callstacks = function
    | Point ctx -> get_by_callstack ctx |> Response.callstacks
    | Cvalue _ -> []

  let by_callstack = function
    | Cvalue _ -> assert false
    | Point ctx as req ->
      let f cs _res acc =
        (cs, in_callstack cs req) :: acc
      in
      get_by_callstack ctx |> Response.fold f []

  let is_reachable req =
    match get req with
    | Bottom -> false
    | _ -> true

  let equality_class exp req =
    let open Equality in
    match Engine.Dom.get Equality_domain.key with
    | None ->
      Result.error DisabledDomain
    | Some extract ->
      let hce = Hcexprs.HCE.of_exp (Eva_ast.translate_exp exp) in
      let extract' state =
        let equalities = Equality_domain.project (extract state) in
        try NonTrivial (Set.find hce equalities)
        with Not_found -> Trivial
      and reduce e1 e2 =
        match e1, e2 with
        | Trivial, _ | _, Trivial -> Trivial
        | NonTrivial e1, NonTrivial e2 -> Equality.inter e1 e2
      in
      let r = match Response.map_join extract' reduce (get req) with
        | (`Top | `Bottom) as r -> r
        | `Value Trivial -> `Top
        | `Value (NonTrivial e) ->
          let list = Equality.elements e in
          let to_cil hce = Hcexprs.HCE.to_exp hce |> Eva_ast.to_cil_exp in
          `Value (List.map to_cil list)
      in
      convert r

  let get_cvalue_model req =
    match Engine.Dom.get Cvalue_domain.State.key with
    | None ->
      Result.error DisabledDomain
    | Some extract ->
      let extract s = extract s |> fst in
      convert (Response.map_join extract Cvalue.Model.join (get req))

  let get_state req key join =
    match Engine.Dom.get key with
    | None -> Result.error DisabledDomain
    | Some extract -> convert (Response.map_join extract join (get req))

  let print_states ?filter req =
    let state = Response.map_join (fun x -> x) Engine.Dom.join (get req) in
    let print (type s)
        (key: s Abstract.Domain.key)
        (module Domain: Abstract_domain.S with type state = s)
        (state: s) acc =
      let name = Structure.Key_Domain.name key in
      let state =
        match filter with
        | None -> state
        | Some bases -> Domain.filter bases state
      in
      let str = Format.asprintf "%a" Domain.pretty state in
      (name, str) :: acc
    in
    let polymorphic_fold_fun = Engine.Dom.{ fold = print } in
    match state with
    | `Top | `Bottom -> []
    | `Value state -> Engine.Dom.fold polymorphic_fold_fun state []

  (* Evaluation *)

  let eval_lval lval req =
    let eval state = Engine.Eval.copy_lvalue state lval in
    LValue (Response.map eval (get req))

  let eval_exp exp req =
    let eval state = Engine.Eval.evaluate state exp in
    Value (Response.map eval (get req))

  let eval_address ~for_writing lval req =
    let eval state = Engine.Eval.lvaluate ~for_writing state lval in
    let access = if for_writing then Locations.Write else Read in
    Address (Response.map eval (get req), access)

  let eval_callee f req =
    let join = (@)
    and extract state =
      let r,_alarms = Engine.Eval.eval_function f state in
      r >>-: List.map fst
    in
    get req |> Response.map_join' extract join |> convert |>
    Result.map (List.sort_uniq Kernel_function.compare)


  (* Conversion *)

  let as_cvalue_or_uninitialized res =
    match Engine.Val.get Main_values.CVal.key with
    | None -> `Top
    | Some get ->
      let make_expr (x, _alarms) =
        x >>-: fun (_valuation, v) ->
        Cvalue.V_Or_Uninitialized.make ~initialized:true ~escaping:false (get v)
      in
      let make_lval (x, _alarms) =
        let+ _valuation, v = x in
        let Eval.{ v; initialized; escaping } = v in
        let v =
          match v with
          | `Bottom -> Cvalue.V.bottom
          | `Value v -> get v
        in
        Cvalue.V_Or_Uninitialized.make ~initialized ~escaping v
      in
      let join = Cvalue.V_Or_Uninitialized.join in
      match res with
      | LValue r -> Response.map_join' make_lval join r
      | Value r -> Response.map_join' make_expr join r

  let extract_value :
    type c. (value, c) evaluation -> (Engine.Val.t or_bottom, c) Response.t =
    let open Bottom.Operators in
    function
    | LValue r ->
      let extract (x, _alarms) = x >>- (fun (_valuation,fv) -> fv.Eval.v) in
      Response.map extract r
    | Value r ->
      let extract (x, _alarms) = x >>-: (fun (_valuation,v) -> v) in
      Response.map extract r

  let as_cvalue res =
    match Engine.Val.get Main_values.CVal.key with
    | None ->
      Result.error DisabledDomain
    | Some get ->
      let join = Main_values.CVal.join in
      let extract value =
        value >>-: get
      in
      extract_value res |> Response.map_join' extract join |> convert

  let extract_loc :
    type c. (address, c) evaluation ->
    (Engine.Loc.location or_bottom, c) Response.t * Locations.access =
    function
    | Address (r, access) ->
      let extract (x, _alarms) =
        let open Bottom.Operators in
        let+ _valuation,loc = x in loc
      in
      Response.map extract r, access

  let as_precise_loc res =
    match Engine.Loc.get Main_locations.PLoc.key with
    | None ->
      Result.error DisabledDomain
    | Some get ->
      let join ploc1 ploc2 =
        if Precise_locs.equal_loc ploc1 ploc2 then ploc1 else
          let loc1 = Precise_locs.imprecise_location ploc1
          and loc2 = Precise_locs.imprecise_location ploc2 in
          assert (Z_or_top.equal loc1.size loc2.size);
          let size = loc1.size in
          let addr_bit = Addresses.Bits.join loc1.addr loc2.addr in
          let paddr_bit = Precise_locs.inject_addr_bits addr_bit in
          Precise_locs.make_precise_loc paddr_bit ~size
      and extract loc =
        loc >>-: get
      in
      extract_loc res |> fst |> Response.map_join' extract join |> convert

  let as_location res =
    match Engine.Loc.get Main_locations.PLoc.key with
    | None ->
      Result.error DisabledDomain
    | Some get ->
      let join (loc1: Locations.t) (loc2: Locations.t) =
        let size = loc1.size
        and addr = Addresses.Bits.join loc1.addr loc2.addr in
        assert (Z_or_top.equal loc2.size size);
        Locations.make addr size
      and extract loc =
        loc >>-: get >>-: Precise_locs.imprecise_location
      in
      extract_loc res |> fst |> Response.map_join' extract join |> convert

  let as_zone res =
    match Engine.Loc.get Main_locations.PLoc.key with
    | None ->
      Result.error DisabledDomain
    | Some get ->
      let response_loc, access = extract_loc res in
      let join = Memory_zone.join
      and extract loc =
        loc >>-: get >>-:
        Precise_locs.enumerate_valid_bits access
      in
      response_loc |> Response.map_join' extract join |> convert

  let is_initialized : type c. (value,c) evaluation -> bool =
    function
    | LValue r ->
      let join = (&&)
      and extract (x, _alarms) =
        x >>-: (fun (_valuation,fv) -> fv.Eval.initialized)
      in
      begin match Response.map_join' extract join r with
        | `Bottom | `Top -> false
        | `Value v -> v
      end
    | Value _ -> true (* computed values are always initialized *)

  let alarms : type a c. (a,c) evaluation -> Alarms.t list =
    fun res ->
    let extract (_,v) = `Value v in
    let r = match res with
      | LValue r ->
        Response.map_join' extract Alarmset.union r
      | Value r ->
        Response.map_join' extract Alarmset.union r
      | Address (r, _lval) ->
        Response.map_join' extract Alarmset.union r
    in
    match r with
    | `Bottom | `Top -> []
    | `Value alarmset ->
      let add alarm status acc =
        if status <> Alarmset.True then alarm :: acc else acc
      in
      Alarmset.fold add [] alarmset

  let is_bottom : type a c. (a,c) evaluation -> bool = function
    | LValue r -> Response.is_bottom r
    | Value r -> Response.is_bottom r
    | Address (r, _lval) -> Response.is_bottom r
end


(* Working with callstacks *)

let callstacks req =
  let module E = Make () in
  E.callstacks req

let by_callstack req =
  let module E = Make () in
  E.by_callstack req


(* State requests *)

let equality_class exp req =
  let module E = Make () in
  E.equality_class exp req

let get_cvalue_model_result req =
  let module E = Make () in
  E.get_cvalue_model req

let get_cvalue_model req =
  match get_cvalue_model_result req with
  | Ok state -> state
  | Error Bottom -> Cvalue.Model.bottom
  | Error (Top | DisabledDomain) -> Cvalue.Model.top

let print_states ?filter req =
  let module E = Make () in
  E.print_states ?filter req


(* Evaluation *)

module type Lvaluation =
sig
  include module type of (Make ())
  val v : (address, unrestricted_response) evaluation
end

module type Evaluation =
sig
  include module type of (Make ())
  val v : (value, unrestricted_response) evaluation
end

type 'a evaluation =
  | Value: (module Evaluation) -> value evaluation
  | Address: (module Lvaluation) -> address evaluation

let build_eval_lval_and_exp () =
  let module M = Make () in
  let build v =
    (module struct
      include M
      let v = v
    end : Evaluation)
  in
  let eval_lval lval req = build @@ M.eval_lval lval req in
  let eval_exp exp req = build @@ M.eval_exp exp req in
  eval_lval, eval_exp

let eval_lval lval req =
  let lval = Eva_ast.translate_lval lval in
  Value ((fst @@ build_eval_lval_and_exp ()) lval req)

let eval_var vi req = eval_lval (Cil.var vi) req

let eval_exp exp req =
  let exp = Eva_ast.translate_exp exp in
  Value ((snd @@ build_eval_lval_and_exp ()) exp req)

let eval_address' ?(for_writing=false) lval req =
  let module M = Make () in
  let v = M.eval_address ~for_writing lval req in
  Address
    (module struct
      include M
      let v = v
    end : Lvaluation)

let eval_address ?(for_writing=false) lval req =
  let lval = Eva_ast.translate_lval lval in
  eval_address' ~for_writing lval req

let eval_callee f req =
  let module M = Make () in
  let f = Eva_ast.translate_host f in
  M.eval_callee f req

let callee stmt =
  let callee_lv =
    match stmt.Cil_types.skind with
    | Instr (Call (_lval, callee_lv, _args, _loc)) ->
      callee_lv
    | Instr (Local_init (_vi, ConsInit (f, _, _), _loc)) ->
      Var f
    | _ ->
      invalid_arg "Can only evaluate the callee on a statement which is a Call"
  in
  before stmt |> eval_callee callee_lv |> Result.value ~default:[]

(* Value conversion *)

let as_cvalue_or_uninitialized (Value evaluation) =
  let module E = (val evaluation : Evaluation) in
  match E.as_cvalue_or_uninitialized E.v with
  | `Value v -> v
  | `Bottom -> Cvalue.V_Or_Uninitialized.bottom
  | `Top -> Cvalue.V_Or_Uninitialized.top

let as_cvalue_result (Value evaluation) =
  let module E = (val evaluation : Evaluation) in
  E.as_cvalue E.v

let as_cvalue evaluation =
  match as_cvalue_result evaluation with
  | Ok v -> v
  | Error Bottom -> Cvalue.V.bottom
  | Error (Top | DisabledDomain) -> Cvalue.V.top

let as_ival evaluation =
  try
    Result.map Cvalue.V.project_ival (as_cvalue_result evaluation)
  with Cvalue.V.Not_based_on_null ->
    Result.error Top

let as_fval evaluation =
  let f ival =
    if Ival.is_float ival
    then Result.ok (Ival.project_float ival)
    else Result.error Top
  in
  Result.bind (as_ival evaluation) f

let as_float evaluation =
  try
    as_fval evaluation |>
    Result.map Fval.project_float |>
    Result.map Fval.F.to_float
  with Fval.Not_Singleton_Float ->
    Result.error Top

let as_integer evaluation =
  try
    Result.map Ival.project_int (as_ival evaluation)
  with Ival.Not_Singleton_Int ->
    Result.error Top

let as_int evaluation =
  try
    Result.map Z.to_int (as_integer evaluation)
  with Z.Overflow ->
    Result.error Top

let as_precise_loc_result (Address lvaluation) =
  let module E = (val lvaluation : Lvaluation) in
  E.as_precise_loc E.v

let as_precise_loc address =
  match as_precise_loc_result address with
  | Ok ploc -> ploc
  | Error Bottom -> Precise_locs.loc_bottom
  | Error (Top | DisabledDomain) -> Precise_locs.loc_top

let as_location_result (Address lvaluation) =
  let module E = (val lvaluation : Lvaluation) in
  E.as_location E.v

let as_location address =
  match as_location_result address with
  | Ok loc -> loc
  | Error Bottom -> Locations.bottom
  | Error (Top | DisabledDomain) -> Locations.top

let as_zone_result (Address lvaluation) =
  let module E = (val lvaluation : Lvaluation) in
  E.as_zone E.v

let as_zone address =
  match as_zone_result address with
  | Ok zone -> zone
  | Error Bottom -> Memory_zone.bottom
  | Error (Top | DisabledDomain) -> Memory_zone.top

(* Evaluation properties *)

let is_singleton: type a. a evaluation -> bool = function
  | Value _ as evaluation ->
    let cvalue = as_cvalue evaluation in
    Cvalue.V.cardinal_zero_or_one cvalue && not (Cvalue.V.is_bottom cvalue)
  | Address _ as lvaluation ->
    let loc = as_location lvaluation in
    Locations.cardinal_zero_or_one loc && not (Locations.is_bottom loc)

let is_initialized (Value evaluation) =
  let module E = (val evaluation : Evaluation) in
  E.is_initialized E.v

let alarms : type a. a evaluation -> Alarms.t list =
  function
  | Value evaluation ->
    let module E = (val evaluation : Evaluation) in
    E.alarms E.v
  | Address lvaluation ->
    let module L = (val lvaluation : Lvaluation) in
    L.alarms L.v

(* Dependencies *)

let expr_deps expr request =
  let lval_to_loc lv = eval_address' lv request |> as_precise_loc in
  Eva_ast.PreciseDepsOf.zone_of_exp lval_to_loc (Eva_ast.translate_exp expr)

let lval_deps lval request =
  let lval_to_loc lv = eval_address' lv request |> as_precise_loc in
  Eva_ast.PreciseDepsOf.zone_of_lval lval_to_loc Read
    (Eva_ast.translate_lval lval)

let address_deps lval request =
  let lval_to_loc lv = eval_address' lv request |> as_precise_loc in
  Eva_ast.PreciseDepsOf.indirect_zone_of_lval
    lval_to_loc (Eva_ast.translate_lval lval)

let expr_dependencies expr request =
  let lval_to_loc lv = eval_address' lv request |> as_precise_loc in
  Eva_ast.PreciseDepsOf.deps_of_exp lval_to_loc (Eva_ast.translate_exp expr)

(* Taint *)

type taint = Taint_domain.taint = Direct | Indirect | Untainted

let is_tainted ?names zone request =
  let module M = Make () in
  let state = M.get_state request Taint_domain.key Taint_domain.join in
  Result.map (fun state -> Taint_domain.is_tainted ?names state zone) state

type taint_names_by_kind = Taint_domain.taint_names_by_kind =
  { direct_taint_names: Datatype.String.Set.t;
    indirect_taint_names: Datatype.String.Set.t;
  }

let taint_names_by_kind zone request =
  let module M = Make () in
  let state = M.get_state request Taint_domain.key Taint_domain.join in
  Result.bind state (fun state ->
      match Taint_domain.taint_names_by_kind state zone with
      | `Top -> Error Top
      | `Value v -> Ok v)

(* Reachability *)

let is_empty rq =
  let module E = Make () in
  E.callstacks rq = []

let is_bottom : type a. a evaluation -> bool =
  function
  | Value evaluation ->
    let module E = (val evaluation : Evaluation) in
    E.is_bottom E.v
  | Address lvaluation ->
    let module L = (val lvaluation : Lvaluation) in
    L.is_bottom L.v

let is_reachable stmt =
  let module M = Make () in
  M.is_reachable (before stmt)

let is_reachable_pos pos =
  let module M = Make () in
  M.is_reachable (before_pos pos)

let is_reachable_kinstr kinstr =
  let module M = Make () in
  M.is_reachable (before_kinstr kinstr)

let condition_truth_value = Iterator.condition_truth_value

(* Callers / callsites *)

let is_called = Function_calls.is_called
let callers = Function_calls.callers
let callsites = Function_calls.callsites

(* Result conversion *)

let default default_value result =
  Result.value ~default:default_value result
