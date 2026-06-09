(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds
open Bottom.Operators

(* Helper for comparison functions *)
let (<?>) c lcmp =
  if c <> 0 then c else Lazy.force lcmp


(* --- Split monitors --- *)

type split_kind = Eva_annotations.split_kind = Static | Dynamic
[@@deriving eq,ord]

type split_term =
  | Expression of Eva_ast.Exp.t
  | Predicate of Cil_datatype.PredicateStructEq.t
[@@deriving eq, ord]

type split_monitor = {
  split_term : split_term;
  split_kind : split_kind;
  split_loc : Fileloc.t;
  split_limit : int;
  mutable split_values : Z.Set.t;
}
[@@deriving eq,ord]

let new_monitor
    ~(limit : int)
    ~(kind : split_kind)
    ~(term : split_term)
    ~(loc : Cil_types.location) =
  {
    split_term = term;
    split_kind = kind;
    split_loc = loc;
    split_limit = limit;
    split_values = Z.Set.empty;
  }

module SplitTerm = Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined

    module Exp = Eva_ast.Exp
    module Predicate = Cil_datatype.PredicateStructEq

    type t = split_term [@@deriving eq, ord]

    let name = "Partition.SplitTerm"

    let reprs =
      Stdlib.List.map (fun e -> Expression e) Exp.reprs @
      Stdlib.List.map (fun p -> Predicate p) Predicate.reprs

    let pretty fmt = function
      | Expression e -> Eva_ast.pp_exp fmt e
      | Predicate p -> Printer.pp_predicate fmt p

    let hash = function
      | Expression e -> Hashtbl.hash (1, Exp.hash e)
      | Predicate p -> Hashtbl.hash (2, Predicate.hash p)
  end)

module SplitMonitor = Datatype.Make_with_collections (
  struct
    include Datatype.Serializable_undefined
    module Values = Z.Set

    type t = split_monitor [@@deriving eq,ord]

    let name = "Partition.SplitMonitor"

    let reprs = [{
        split_term = Expression (List.hd Eva_ast.Exp.reprs);
        split_kind = Static;
        split_loc = Fileloc.unknown;
        split_limit = 0;
        split_values = Z.Set.empty
      }]

    let pretty fmt m =
      Format.fprintf fmt "%d/%d" (Values.cardinal m.split_values) m.split_limit

    let hash m =
      hash (
        SplitTerm.hash m.split_term,
        Fileloc.hash m.split_loc,
        Datatype.Int.hash m.split_limit,
        Values.hash m.split_values)

    let copy m =
      { m with split_values = m.split_values }
  end)


(* --- Stamp rationing --- *)

(* Stamps used to label states according to slevel.
   The second integer is used to keep separate the different states resulting
   from a transfer function producing a state list before a new stamping.  *)
type stamp = (int * int) option (* store stamp / transfer stamp *)

(* Stamp rationing according to the slevel. *)
type rationing =
  { current: int ref; (* last used stamp. *)
    limit: int;       (* limit of available stamps; after, stamps are [None]. *)
    merge: bool       (* on merge slevel annotations or -eva-merge-after-loop,
                         merge the incoming states with one unique stamp. *)
  }

let new_rationing ~limit ~merge = { current = ref 0; limit; merge }


(* --- Loops unrolling --- *)

module LoopUnrolling =
struct
  module Prototype =
  struct
    include Datatype.Serializable_undefined
    type t = {
      loop : Cil_datatype.Stmt.t;
      current : int;
      limit : int;
    }
    [@@deriving eq,ord]

    let name = "Partition.LoopUnrolling"

    let reprs =
      List.map
        (fun stmt -> {
             loop = stmt;
             current = 0;
             limit = 0;
           })
        Cil_datatype.Stmt.reprs

    let pretty fmt l =
      let pp_sid fmt stmt =
        Format.pp_print_int fmt stmt.Cil_types.sid
      in
      Format.fprintf fmt "s%a: %d/%d"
        pp_sid l.loop
        l.current
        l.limit

    let hash l =
      Hashtbl.hash (l.loop.sid, l.current, l.limit)
  end

  include Datatype.Make (Prototype)
  include Prototype

  let create ~loop ~limit =
    { loop=loop.Eva_automata.stmt ; current = 0; limit }

  let incr unrolling =
    if unrolling.current >= unrolling.limit then begin
      if unrolling.limit > 0 then
        Self.warning ~once:true ~current:true
          ~wkey:Self.wkey_loop_unroll_partial
          "loop not completely unrolled";
      unrolling
    end else begin
      let unrolling' = { unrolling with current = unrolling.current + 1 } in
      Statistics.(grow max_unrolling unrolling.loop unrolling'.current);
      unrolling'
    end
end


(* --- Keys --- *)

module Int = Datatype.Int
module SplitMap = SplitTerm.Map

type branch =
  | Branch of int
  | Builtin_result of Kernel_function.t * Cil_datatype.Kinstr.t * int
  | Spec_behavior of Kernel_function.t * Cil_datatype.Kinstr.t * int
  | Disjunction_case of Cil_datatype.Stmt.t * int
[@@deriving eq, ord]

module BranchDatatype = Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    type t = branch [@@deriving eq, ord]
    let name = "Partition.Branch"
    let reprs = [Branch 0]
    let pretty fmt = function
      | Branch id -> Format.fprintf fmt "%d" id
      | Builtin_result (kf, _, id) | Spec_behavior (kf, _, id) ->
        Format.fprintf fmt "%s#%d" (Kernel_function.get_name kf) id
      | Disjunction_case (stmt, id) -> Format.fprintf fmt "@%d#%d" stmt.sid id
    let hash = function
      | Branch id -> Hashtbl.hash (1, id)
      | Builtin_result (kf, kinstr, id) ->
        Hashtbl.hash
          (2, Kernel_function.hash kf, Cil_datatype.Kinstr.hash kinstr, id)
      | Spec_behavior (kf, kinstr, id) ->
        Hashtbl.hash
          (3, Kernel_function.hash kf, Cil_datatype.Kinstr.hash kinstr, id)
      | Disjunction_case (stmt, id) ->
        Hashtbl.hash (4, Cil_datatype.Stmt.hash stmt, id)
  end)

(* The key have several fields, one for each kind of partitioning:
   - Ration stamps: These modelize the legacy slevel. Each state is given
     a ration stamp (represented by two integers) until there is no slevel
     left. The first number is attributed by the store it comes from, the
     second one is attributed by the last transfer.
     It is an option type, when there is no more ration stamp, this field is
     set to None; each new state will not be distinguished by this field.
   - Branches: This field enumerate the last junctions points passed through.
     The partitioning may chose how the branches are identified, but it
     is a First-In-First-Out set.
   - Loops: This field stores the loop iterations needed to reach this state
     for each loop we are currently in. It is stored in reverse order
     (innermost loop first) It also stores the maximum number of unrolling ;
     this number varies from a state to another, as it is computed from
     an expression evaluated when we enter the loop.
   - Splits: track the splits applied to the state as a map from the term of
     the split to its value. Terms can be C expressions or ACSL predicates.
     Since the split creates states in which the expression evaluates to a
     singleton, the values of the map are integers.
     Static splits are only evaluated when the annotation is encountered
     whereas dynamic splits are reevaluated regularly; a list of active
     dynamic splits is also propagated in the key. *)
type key = {
  ration_stamp : stamp;
  branches : branch list;
  loops : LoopUnrolling.t list;
  splits : Z.t SplitMap.t; (* term -> value *)
  dynamic_splits : split_monitor SplitMap.t; (* term -> monitor *)
  syntactic_splits : int Int.Map.t; (* split vertex -> edge taken *)
}

type call_return_policy = {
  callee_splits: bool;
  callee_history: bool;
  caller_history: bool;
  history_size: int;
}

module Key =
struct
  module IntPair = Datatype.Pair (Int) (Int)
  module Stamp = Datatype.Option (IntPair)
  module BranchList = Datatype.List (BranchDatatype)
  module LoopList = Datatype.List (LoopUnrolling)
  module Splits = SplitMap.Make (Z)
  module DSplits = SplitMap.Make (SplitMonitor)
  module SSplits = Int.Map.Make (Int)

  (* Initial key, before any partitioning *)
  let empty = {
    ration_stamp = None;
    branches = [];
    loops = [];
    splits = SplitMap.empty;
    dynamic_splits = SplitMap.empty;
    syntactic_splits = Int.Map.empty;
  }

  let add_branch ?history_size b k =
    match history_size with
    | None -> { k with branches = b :: k.branches }
    | Some history_size ->
      if history_size > 0 then
        let trunc = List.take (history_size - 1) k.branches in
        { k with branches = b :: trunc }
      else if k.branches <> [] then
        { k with branches = [] }
      else
        k

  include Datatype.Make_with_collections (struct
      include Datatype.Serializable_undefined

      type t = key

      let name = "Partition.Key"

      let reprs = [ empty ]

      let compare k1 k2 =
        LoopList.compare k1.loops k2.loops
        <?> lazy (Splits.compare k1.splits k2.splits)
        (* Ignore monitors in comparison *)
        <?> lazy (SplitMap.compare (fun _ _ -> 0)
                    k1.dynamic_splits k2.dynamic_splits)
        <?> lazy (BranchList.compare k1.branches k2.branches)
        <?> lazy (Stdlib.Option.compare IntPair.compare
                    k1.ration_stamp k2.ration_stamp)
        <?> lazy (SSplits.compare k1.syntactic_splits k2.syntactic_splits)

      let equal = Datatype.from_compare

      let hash k =
        Stdlib.Hashtbl.hash (
          Stamp.hash k.ration_stamp,
          BranchList.hash k.branches,
          LoopList.hash k.loops,
          Splits.hash k.splits,
          DSplits.hash k.dynamic_splits, (* Monitors probably shouldn't be hashed *)
          SSplits.hash k.syntactic_splits)

      let pretty fmt key =
        begin match key.ration_stamp with
          | Some (n,_) -> Format.fprintf fmt "#%d" n
          | None -> ()
        end;
        Pretty_utils.pp_list ~pre:"[@[" ~sep:" ;@ " ~suf:"@]]"
          BranchDatatype.pretty
          fmt
          key.branches;
        Pretty_utils.pp_list ~pre:"(@[" ~sep:" ;@ " ~suf:"@])"
          (fun fmt { LoopUnrolling.current=i; _ } -> Format.pp_print_int fmt i)
          fmt
          key.loops;
        Pretty_utils.pp_list ~pre:"{@[" ~sep:" ;@ " ~suf:"@]}"
          (fun fmt (t, i) -> Format.fprintf fmt "%a:%a"
              SplitTerm.pretty t
              Z.pretty i)
          fmt
          (SplitMap.bindings key.splits)
    end)

  let exceed_rationing key = key.ration_stamp = None

  let combine ~policy ~caller ~callee =
    let combine_map merge_map get_map =
      let keep_second _ v1 v2 = if Option.is_some v2 then v2 else v1 in
      if policy.callee_splits
      then merge_map keep_second (get_map caller) (get_map callee)
      else get_map caller
    in
    (* There is no need to preserve the uniqueness of ration stamps here, as
       keys will always be given new stamps before the merge of identical keys.
       This invariant depends on the sequence of operations performed by
       the iterator and the trace_partitioning. *)
    {
      ration_stamp = None;
      branches =
        List.take policy.history_size (
          (if policy.callee_history then callee.branches else []) @
          (if policy.caller_history then caller.branches else [])
        );
      loops = caller.loops;
      splits = combine_map SplitMap.merge (fun t -> t.splits);
      dynamic_splits = combine_map SplitMap.merge (fun t -> t.dynamic_splits);
      syntactic_splits = combine_map Int.Map.merge (fun t -> t.syntactic_splits);
    }
end


(* --- Partitions --- *)

module KMap = Key.Map

type 'a partition = 'a KMap.t

let empty = KMap.empty
let find = KMap.find
let replace = KMap.add
let is_empty = KMap.is_empty
let size = KMap.cardinal
let iter = KMap.iter
let map = KMap.map
let filter = KMap.filter
let merge = KMap.merge

let to_list (p : 'a partition) : (key * 'a) list =
  KMap.bindings p


(* --- Partitioning actions --- *)

type unroll_limit =
  | ExpLimit of Cil_types.exp
  | IntLimit of int
  | AutoUnroll of Eva_automata.loop * int * int

type action =
  | Enter_loop of unroll_limit * Eva_automata.loop
  | Leave_loop
  | Incr_loop
  | Add_branch of int * int
  | Ration of rationing
  | Restrict of Eva_ast.exp * Z.t list
  | Split of split_monitor
  | Merge of split_term
  | SyntacticSplit of int * int
  | MergeSyntacticSplits
  | Update_dynamic_splits

exception InvalidAction

(* --- Flows --- *)

module MakeFlow (Abstract: Engine_abstractions_sig.S) =
struct
  type state = Abstract.Dom.t
  type t =  (key * state) list

  (* This module tries to keep lists of pairs (key, state) sorted in increasing
     order according to Key.compare. *)

  let empty = []

  let initial (p : 'a list) : t =
    List.map (fun state -> Key.empty, state) p

  let to_list (f : t) : (key * state) list = f

  let of_partition (p : state partition) : t =
    KMap.bindings p

  let to_partition (p : t) : state partition =
    let add p (k,x) =
      (* Join states with the same key *)
      let x' =
        try
          Abstract.Dom.join (KMap.find k p) x
        with Not_found -> x
      in
      KMap.add k x' p
    in
    List.fold_left add KMap.empty p

  let is_empty (p : t) =
    p = []

  let size (p : t) =
    List.length p

  let union (p1 : t) (p2 : t) : t =
    p1 @ p2

  (* --- Automatic loop unrolling ------------------------------------------- *)

  module AutoLoopUnroll = Auto_loop_unroll.Make (Abstract)

  (* --- Evaluation and split functions ------------------------------------- *)

  (* Evaluation with no reduction and no subdivision. *)
  let evaluate = Abstract.Eval.evaluate ~reduction:false ~subdivnb:0

  exception Operation_failed

  let fail ~source message =
    let warn_and_raise message =
      Self.warning ~source ~once:true "%s" message;
      raise Operation_failed
    in
    Format.kasprintf warn_and_raise message

  let evaluate_exp_to_ival ~source state exp =
    (* Evaluate the expression *)
    let valuation, value =
      match evaluate state exp with
      | `Value (valuation, value), alarms when Alarmset.is_empty alarms ->
        valuation, value
      | _ ->
        fail ~source "this partitioning parameter cannot be evaluated safely on \
                      all states"
    in
    (* Get the cvalue *)
    let cvalue = match Abstract.Val.get Main_values.CVal.key with
      | Some get_cvalue -> get_cvalue value
      | None -> fail ~source "partitioning is disabled when the CValue domain is \
                              not active"
    in
    (* Extract the ival *)
    let ival =
      try
        Cvalue.V.project_ival cvalue
      with Cvalue.V.Not_based_on_null ->
        fail ~source "this partitioning parameter must evaluate to an integer"
    in
    valuation, ival

  exception Split_limit of Z.t option

  let split_by_value ~monitor state exp =
    let source = fst monitor.split_loc in
    let module SplitValues = Z.Set in
    let valuation, ival = evaluate_exp_to_ival ~source state exp in
    (* Build a state with the lvalue set to a singleton *)
    let build i acc =
      let value = Abstract.Val.inject_int exp.typ i in
      let state =
        let* valuation = Abstract.Eval.assume ~valuation state exp value in
        (* Check the reduction *)
        Abstract.Dom.update (Abstract.Eval.to_domain_valuation valuation) state
      in
      match state with
      | `Value state ->
        let _,new_ival = evaluate_exp_to_ival ~source state exp in
        if not (Ival.is_singleton_int new_ival) then
          fail ~source "failing to learn perfectly from split" ;
        monitor.split_values <-
          SplitValues.add i monitor.split_values;
        (i, state) :: acc
      | `Bottom -> (* This value cannot be set in the state ; the evaluation of
                      expr was imprecise *)
        acc
    in
    try
      (* Check the size of the ival *)
      begin match Ival.cardinal ival with
        | None -> raise (Split_limit None)
        | Some c as count ->
          if Z.(gt c (of_int monitor.split_limit)) then
            raise (Split_limit count)
      end;
      (* For each integer of the ival, build a new state *)
      try
        let result = Ival.fold_int build ival [] in
        let c = SplitValues.cardinal monitor.split_values in
        if c > monitor.split_limit then
          raise (Split_limit (Some (Z.of_int c)));
        result
      with Abstract_interp.Error_Top -> (* The ival is float *)
        raise (Split_limit None)
    with
    | Split_limit count ->
      let pp_count fmt =
        match count with
        | None -> ()
        | Some c -> Format.fprintf fmt " (%a)" Z.pretty c
      in
      fail ~source "split on more than %d values%t prevented ; try to improve \
                    the analysis precision or look at the option -eva-split-limit \
                    to increase this limit."
        monitor.split_limit pp_count

  let eval_exp_to_int ~source state exp =
    let _valuation, ival = evaluate_exp_to_ival ~source state exp in
    try Z.to_int (Ival.project_int ival)
    with
    | Ival.Not_Singleton_Int ->
      fail ~source "this partitioning parameter must evaluate to a singleton"
    | Z.Overflow -> fail ~source "this partitioning parameter overflows an integer"

  let split_by_predicate state predicate =
    let env =
      let states = function _ -> Abstract.Dom.top in
      Abstract_domain.{ states; result = None }
    in
    match Abstract.Dom.evaluate_predicate env state predicate with
    | True -> [ Z.one, state ]
    | False -> [ Z.zero, state ]
    | Unknown ->
      let source = fst (predicate.Cil_types.pred_loc) in
      let aux positive =
        let+ state' =
          Abstract.Dom.reduce_by_predicate env state predicate positive in
        let x = Abstract.Dom.evaluate_predicate env state' predicate in
        if x == Unknown
        then
          Self.warning ~source ~once:true
            "failing to learn perfectly from split predicate";
        if Abstract.Dom.equal state' state then raise Operation_failed;
        let value = if positive then Z.one else Z.zero in
        value, state'
      in
      Bottom.list_values [ aux true; aux false ]

  (* --- Applying partitioning actions onto flows --------------------------- *)

  let stamp_by_value = match Abstract.Val.get Main_values.CVal.key with
    | None -> fun _ _ _ -> None
    | Some get -> fun expr expected_values state ->
      let typ = expr.Eva_ast.typ in
      let make stamp i = stamp, i, Abstract.Val.inject_int typ i in
      let expected_values = List.mapi make expected_values in
      match fst (evaluate state expr) with
      | `Bottom -> None
      | `Value (_cache, value) ->
        let is_included (_, _, v) = Abstract.Val.is_included v value in
        match List.find_opt is_included expected_values with
        | None -> None
        | Some (stamp, i, _) ->
          if Cvalue.V.cardinal_zero_or_one (get value)
          then Some (stamp, 0)
          else begin
            Self.result ~level:3 ~once:true ~current:true
              "cannot properly split on \\result == %a"
              Z.pretty i;
            None
          end

  let split_state ~monitor term (key, state) : (key * state) list =
    try
      let update_key (v, x) =
        { key with splits = SplitMap.add term v key.splits }, x
      in
      let states =
        match term with
        | Expression exp ->
          split_by_value ~monitor state exp
        | Predicate pred ->
          split_by_predicate state pred
      in
      List.map update_key states
    with Operation_failed -> [(key,state)]

  let split monitor (p : t) =
    let { split_term; split_kind } = monitor in
    let add_split (key, state) =
      let dynamic_splits =
        match split_kind with
        | Static -> SplitMap.remove split_term key.dynamic_splits
        | Dynamic -> SplitMap.add split_term monitor key.dynamic_splits
      in
      let key = { key with dynamic_splits } in
      split_state ~monitor split_term (key, state)
    in
    List.concat_map add_split p

  let update_dynamic_splits p =
    (* Update one state *)
    let update_state (key, state) =
      (* Split the states in the list l for the given exp *)
      let update_exp term monitor l =
        List.concat_map (split_state ~monitor term) l
      in
      (* Foreach exp in original state: split *)
      SplitMap.fold update_exp key.dynamic_splits [(key,state)]
    in
    List.concat_map update_state p

  let map_keys (f : key -> state -> key) (p : t) : t =
    List.map (fun (k,x) -> f k x, x) p

  let transfer_keys p = function
    | Split monitor ->
      split monitor p

    | Update_dynamic_splits ->
      update_dynamic_splits p

    | action -> (* Simple map transfer functions *)
      let transfer = match action with
        | Split _ | Update_dynamic_splits ->
          assert false (* Handled above *)

        | Enter_loop (limit_kind, loop) -> fun k x ->
          let limit = try match limit_kind with
            | ExpLimit cil_exp ->
              let exp = Eva_ast.translate_exp cil_exp
              and source = fst cil_exp.eloc in
              eval_exp_to_int ~source x exp
            | IntLimit i -> i
            | AutoUnroll (loop, min_unroll, max_unroll) ->
              match AutoLoopUnroll.compute ~max_unroll x loop with
              | None -> min_unroll
              | Some i ->
                Self.warning ~once:true ~current:true
                  ~wkey:Self.wkey_loop_unroll_auto
                  "Automatic loop unrolling.";
                i
            with
            | Operation_failed -> 0
          in
          let new_unrolling = LoopUnrolling.create ~loop ~limit in
          { k with loops = new_unrolling :: k.loops }

        | Leave_loop -> fun k _x ->
          begin match k.loops with
            | [] -> raise InvalidAction
            | _ :: tl -> { k with loops = tl }
          end

        | Incr_loop -> fun k _x ->
          begin match k.loops with
            | [] -> raise InvalidAction
            | unrolling :: tl ->
              { k with loops = LoopUnrolling.incr unrolling :: tl }
          end

        | Add_branch (b,max) -> fun k _x ->
          Key.add_branch ~history_size:max (Branch b) k

        | Ration { current; limit; merge } ->
          let length = List.length p in
          (* The incoming states exceed the rationing limit: no more stamps. *)
          if !current + length > limit then begin
            current := limit;
            fun k _ -> { k with ration_stamp = None }
          end
          (* If merge, a unique ration stamp for all incoming states. *)
          else if merge then begin
            current := !current + length;
            fun k _ -> { k with ration_stamp = Some (!current, 0) }
          end
          (* Default case: a different stamp for each incoming state. *)
          else
            let stamp () = incr current; Some (!current, 0) in
            fun k _ -> { k with ration_stamp = stamp () }

        | Restrict (expr, expected_values) -> fun k s ->
          { k with ration_stamp = stamp_by_value expr expected_values s }

        | Merge term -> fun k _x ->
          { k with
            splits = SplitMap.remove term k.splits;
            dynamic_splits = SplitMap.remove term k.dynamic_splits;
          }

        | SyntacticSplit (vertex, branch) -> fun k _x ->
          { k with
            syntactic_splits = Int.Map.add vertex branch k.syntactic_splits
          }

        | MergeSyntacticSplits -> fun k _x ->
          { k with syntactic_splits = Int.Map.empty }

      in
      map_keys transfer p

  let transfer (f : (key * state) -> (key * state) list): t -> t =
    let n = ref 0 in
    let restamp (key, state) =
      match key.ration_stamp with
      (* No ration stamp, just add the state to the list *)
      | None -> (key, state)
      (* There is a ration stamp, set the second part of the stamp to a
         unique transfer number *)
      | Some (s, _) ->
        let key' = { key with ration_stamp = Some (s, !n) } in
        incr n;
        (key', state)
    in
    List.concat_map (fun x -> List.map restamp (f x))

  let iter (f : key -> state -> unit) (p : t) : unit =
    List.iter (fun (k, x) -> f k x) p

  let join_duplicate_keys (p : t) : t =
    (* Function [aux] below reverses the list, so sort by decreasing order
       to keep a list sorted by increasing order. *)
    let cmp (k, _) (k', _) = Key.compare k' k in
    let p = List.fast_sort cmp p in
    let rec aux acc (key, state) = function
      | [] -> (key, state) :: acc
      | (key', state') :: tl ->
        if Key.compare key key' = 0
        then aux acc (key, Abstract.Dom.join state state') tl
        else aux ((key, state) :: acc) (key', state') tl
    in
    match p with
    | [] | [_] -> p
    | e :: tl -> aux [] e tl

  let filter_map (f: key -> state -> state option) : t -> t =
    List.filter_map (fun (key, s) -> Option.map (fun s' -> key, s') (f key s))
end
