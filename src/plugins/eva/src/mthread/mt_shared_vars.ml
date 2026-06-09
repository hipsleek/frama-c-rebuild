(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype
open Visitor
open Mt_cil
open Mt_memory.Types
open Mt_types
open Mt_shared_vars_types
open Mt_cfg_types
open Mt_thread

(* -------------------------------------------------------------------------- *)
(* --- Collecting accesses to variables                                   --- *)
(* -------------------------------------------------------------------------- *)

(* The beginning of this module is essentially a variation on an old version
   of Inputs/Outputs, in which we keep a bit more information (typically
   locations for writes, or the statements at which the operation took place
   for reads *)

let () = Ast_attributes.register ~ignore:true (AttrName false) "FRAMA_C_MODEL"
(* FIXME: Frama-C has removed the attribute FRAMA_C_MODEL from its libc. This
   should also probably be removed here. *)

(* Skip variables such as __fc_heap_status, __fc_random_counter, etc. *)
let is_model_base b =
  try
    let vi = Base.to_varinfo b in
    Ast_attributes.contains "FRAMA_C_MODEL" vi.vattr
  with Base.Not_a_C_variable -> false

(* Skip Mthread variables such as __fc_mthread_threads. *)
let is_frama_c_base = function
  | Base.Var (v, _) | Base.Allocated (v, _, _) ->
    String.starts_with ~prefix:"__fc_mthread" v.vname
  | _ -> false

let keep_base b =
  not (Base.is_any_formal_or_local b ||
       is_frama_c_base b ||
       is_model_base b ||
       (Mt_options.IgnoreNull.get () && Base.(equal b null))
      )


(* We are only interested in globals, and remove locals, formals, and special
   frama-c variables on the fly *)
let remove_uninteresting_variables_zone z =
  Memory_zone.filter_base keep_base z
let remove_uninteresting_variables_loc loc =
  Locations.filter_base keep_base loc

let error_io_whole_memory op =
  let source = fst (RW.loc op) in
  Mt_self.error ~source ~once:true
    "@[%a of the whole memory.@ Ignoring to allow Mthread to continue, \
     but the analysis will not be correct.@]"
    RW.pretty op

let filter_inout_access =
  let is_mthread_shared base =
    try
      (* Skip variable "__fc_mthread_shared", as it is only used to prevent
         Memexec from caching some functions *)
      let vi = Base.to_varinfo base in
      String.equal vi.vorig_name "__fc_mthread_shared"
    with Base.Not_a_C_variable ->
      false
  in
  let filter_base base =
    Base.is_global base && not (is_mthread_shared base)
  in
  Inout_access.mk_filter ~filter_base

let read_written_by_thread ?(watch_only=Memory_zone.top) sm th =
  let open Current_loc.Operators in

  let add stmt op zone acc =
    if Memory_zone.is_bottom zone then
      (* Do nothing *) acc
    else if Memory_zone.is_top zone then
      let () = error_io_whole_memory op in
      acc
    else
      let zone = remove_uninteresting_variables_zone zone in
      let zone = Memory_zone.narrow zone watch_only in
      let state = AccessesByZone.Map acc in
      let v = SetStmtIdAccess.inject_singleton (op, stmt, th) in
      match AccessesByZone.add_binding state ~exact:false zone v with
      | AccessesByZone.Bottom -> assert false (* state is not Bottom *)
      | AccessesByZone.Top -> assert false (* Top is checked above *)
      | AccessesByZone.Map m -> m
  in

  Inout_access.fold
    ~filter:filter_inout_access
    (fun pos memory acc ->
       match pos with
       | RootCall _ -> acc
       | GlobalInit _ -> acc (* never multithreaded. *)
       | Local (_, cs) when cs.thread <> Thread.id th  ->
         acc (* Ignore other threads *)
       | Local (stmt, _) ->
         let<> UpdatedCurrentLoc = Stmt.loc stmt in
         if sm stmt then
           acc
           |> add stmt (ReadPos pos) memory.read
           |> add stmt (WritePos pos) memory.write
         else
           acc)
    AccessesByZone.empty_map

(** In global mode, we do a rough analysis using the synthetic results of
    Value. In local mode, we supply precise states for each statement of the
    function. *)
type mode = VLocal | VGlobal

type collect_params = {
  stmt_multithread: stmt -> bool;
  thread: thread;
  mode: mode;
  iter_requests: stmt -> (Results.request -> unit) -> unit;
  watch_only: Memory_zone.t;
}

(* Visitor that collects all reads and assignments done by the functions.
   Strongly inspired by inout/{inputs,outputs}.ml. There are some
   differences, eg. Outputs.get_internal, which is accessed through Db in
   outputs.ml, is here in the class. Moreover we always use the same visitor:
   all results are accumulated in the variable [result], instead of being
   returned functionally *)
class do_it cp =
  object(self)
    inherit Visitor.frama_c_inplace as super

    val mutable result = AccessesByZone.empty_map
    method accesses = result

    (* Functions already visited. Used to avoid recursion and to prevent adding
       results multiple times in [result] *)
    val visited = Cil_datatype.Kf.Hashtbl.create 17

    method private mark_visited kf =
      Cil_datatype.Kf.Hashtbl.add visited kf ()

    method private already_visited kf =
      try  Cil_datatype.Kf.Hashtbl.find visited kf; true
      with Not_found -> false


    method private add_access op z =
      if not (Memory_zone.equal Memory_zone.top z) then (
        let stmt = self#cur_stmt in
        if cp.stmt_multithread stmt then
          let interesting = remove_uninteresting_variables_zone z in
          let concurrent = Memory_zone.narrow interesting cp.watch_only in
          let state = AccessesByZone.Map result in
          let v =
            SetStmtIdAccess.inject_singleton (op, stmt, cp.thread)
          in
          match AccessesByZone.add_binding state ~exact:false concurrent v with
          | AccessesByZone.Bottom -> assert false (* state is not Bottom *)
          | AccessesByZone.Top -> assert false (* Top is checked above *)
          | AccessesByZone.Map m -> result <- m
      ) else
        Mt_self.error ~current:true ~once:true
          "@[%a@ of@ the@ whole@ memory.@ Ignoring@ to@ allow@ Mthread@ to@ \
           continue,@ but@ the@ analysis@ will@ not@ be@ correct.@]"
          RW.pretty op

    method private cur_stmt =
      match super#current_stmt with
      | None -> Mt_self.abort "visiting without current statement"
      | Some s -> s

    method! vstmt_aux s =
      if cp.stmt_multithread s then
        match s.skind with
        | UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_,_,_) ->
               ignore(visitFramacStmt (self:>frama_c_visitor) stmt)) seq;
          Cil.SkipChildren
        | _ -> super#vstmt_aux s
      else
        Cil.DoChildren (* Not an interesting statement, we do not analyse it
                          deeply *)

    method! vlval lv =
      cp.iter_requests self#cur_stmt
        (fun req -> self#add_access Read Results.(lval_deps lv req));
      Cil.SkipChildren

    method private do_assign lv =
      cp.iter_requests self#cur_stmt
        (fun request ->
           let deps = Results.(address_deps lv request) in
           self#add_access Read deps;
           let loc = Results.(eval_address lv request |> as_location) in
           if Addresses.Bits.(equal loc.addr top) then
             Mt_self.warning ~current:true ~once:true
               "Problem with %a: its writing location is completely unknown."
               Printer.pp_lval lv;
           let loc = remove_uninteresting_variables_loc loc in
           let loc = Locations.(valid_part Write loc) in
           let bits_loc = Locations.(enumerate_valid_bits Write loc) in
           self#add_access (Write loc) bits_loc)

    method private do_init v i =
      let rec aux lv = function
        | SingleInit e ->
          self#do_assign lv;
          ignore (visitFramacExpr (self:>frama_c_visitor) e)
        | CompoundInit(ct,initl) ->
          let doinit o i _ () = aux (Cil.addOffsetLval o lv) i in
          Cil.foldLeftCompound ~implicit:true ~doinit ~ct ~initl ~acc:()
      in
      aux (Cil.var v) i

    method private do_call f =
      cp.iter_requests self#cur_stmt (fun request ->
          let deps = Results.address_deps (f,NoOffset) request in
          self#add_access Read deps;
          (* In global mode, we treat the recursive calls. In precise
             mode, they are done elsewhere in the construction of the cfg *)
          if cp.mode = VGlobal then
            let callees = Results.(eval_callee f request |> default []) in
            List.iter self#rw_fun callees
        )

    method! vinst i =
      let visit_expr e = ignore (visitFramacExpr (self:>frama_c_visitor) e) in
      if not (Results.is_reachable self#cur_stmt) then
        Cil.SkipChildren
      else
        match i with
        | Set (lv,exp,_) -> self#do_assign lv; visit_expr exp; Cil.SkipChildren
        | Local_init(v, AssignInit i, _) -> self#do_init v i; Cil.SkipChildren
        | Local_init(v, ConsInit (f, args, _), _) ->
          self#do_assign (Cil.var v);
          self#do_call (Var f);
          List.iter visit_expr args;
          Cil.SkipChildren
        | Call (lv_opt,lv,args,_) ->
          Option.iter self#do_assign lv_opt;
          self#do_call lv;
          List.iter visit_expr args;
          Cil.SkipChildren
        | _ -> Cil.DoChildren

    method! vexpr exp =
      match exp.enode with
      | AddrOf lv | StartOf lv ->
        cp.iter_requests self#cur_stmt
          (fun request ->
             let deps = Results.address_deps lv request in
             self#add_access Read deps;
          );
        Cil.SkipChildren
      | _ -> Cil.DoChildren

    method rw_stmt stmt =
      ignore (visitFramacStmt (self :> frama_c_visitor) stmt)

    (* Skip assigns to "__fc_mthread_shared" variable, as this variable is
       only used to prevent Memexec from caching some functions *)
    method private assigns_not_mthread = function
      | WritesAny -> WritesAny
      | Writes l ->
        let aux (t, _) = match t.it_content.term_node with
          | TLval (TVar { lv_name = name}, _) ->
            name <> "__fc_mthread_shared"
          | _ -> true
        in
        Writes (List.filter aux l)

    method private compute_for_funspec kf =
      let aux request =
        let state = Results.get_cvalue_model request in
        let behaviors = Logic_inout.valid_behaviors kf state in
        let assigns = Ast_info.merge_assigns behaviors in
        let assigns = self#assigns_not_mthread assigns in
        (* Compute the zones written by the assigns *)
        (match assigns with
         | WritesAny ->
           let top = Locations.make Addresses.Bits.top Z_or_top.top in
           self#add_access (Write top) Memory_zone.top;

         | Writes assigns' ->
           let aux l =
             try
               let env = Eval_terms.env_assigns ~pre:state in
               let alarm_mode = Eval_terms.Ignore in
               let loc = Eval_terms.eval_tlval_as_location ~alarm_mode env l in
               let loc = remove_uninteresting_variables_loc loc in
               let loc = Locations.(valid_part Write loc) in
               let z = Locations.(enumerate_valid_bits Write loc) in
               self#add_access (Write loc) z
             with Eval_terms.LogicEvalError _ ->
               Mt_self.warning ~once:true
                 "Unsupported assigns clause for function %a. Ignoring it."
                 Kernel_function.pretty kf;
           in
           List.iter
             (fun ({it_content = loc}, _) ->
                if not (Logic_utils.is_result loc) then aux loc
             ) assigns'
        );
        (* Compute the zone read by the assigns *)
        let read = Logic_inout.assigns_inputs_to_zone state assigns in
        self#add_access Read read
      in
      match cp.mode with
      | VGlobal ->
        let requests =
          Results.(at_start_of kf |> by_callstack |> List.map snd)
        in
        List.iter aux requests
      | VLocal ->
        cp.iter_requests self#cur_stmt aux


    method rw_fun kf =
      if not (self#already_visited kf) then (
        self#mark_visited kf;
        match Function_calls.use_spec_instead_of_definition kf, kf.fundec with
        | false, Definition (f,_) ->
          ignore (visitFramacFunction (self:>frama_c_visitor) f)

        | true, _ | _, Declaration _ -> self#compute_for_funspec kf
      )
  end

let aux_visitor sm th sa watch_only =
  let cp = {
    stmt_multithread = sm;
    thread = th;
    mode = (match sa with Global -> VGlobal | Local _ -> VLocal);
    iter_requests = iter_requests sa;
    watch_only = watch_only;
  } in
  new do_it cp

let read_written_by_function sm th sa ?(watch_only=Memory_zone.top) kf ki =
  let comp = aux_visitor sm th sa watch_only in
  (* We position the current statement for calls to leaf functions *)
  (match ki with
   | Kglobal -> ()
   | Kstmt s -> comp#push_stmt s
  );
  comp#rw_fun kf;
  comp#accesses

let is_multithreaded state_accessor stmt =
  let exception Stmt_is_multithreaded in
  let v = Mt_lib.var_thread_created () in
  let raise_if_non_zero request =
    let value = Results.(eval_var v request |> as_cvalue) in
    if Cvalue.V.contains_non_zero value then raise Stmt_is_multithreaded
  in
  try iter_requests state_accessor stmt raise_if_non_zero; false
  with Stmt_is_multithreaded -> true

(* Ad-hoc function that disregards accesses to variables that
   occurs before any thread is created. This simplifies the cfg of threads,
   and increases convergence speed *)
let stmt_is_multithreaded analysis state_accessor stmt =
  not (Thread.is_main analysis.curr_thread.th_eva_thread)
  || is_multithreaded state_accessor stmt


(* -------------------------------------------------------------------------- *)
(* --- Computation of variables accessed concurrently by two threads      --- *)
(* -------------------------------------------------------------------------- *)

module type Computer =
sig
  module Access : Datatype.S
  module Set: Lattice_type.Lattice_Set with type O.elt = Access.t
  module ZoneMap: Lmap_bitwise.Location_map_bitwise with type v = Set.t

  type list_accesses = (Memory_zone.t * Set.t) list

  val pretty_concurrent_accesses :
    ?f:Access.t Pretty_utils.formatter ->
    unit -> Format.formatter -> list_accesses -> unit

  val all_zones_accessed : list_accesses -> Memory_zone.t

  val concurrent_accesses_all_threads :
    Mt_thread.ThreadState.t list ->
    (list_accesses * list_accesses) * ZoneMap.map
end


(* All our computations are parameterized by the structure on which we
   act: either the information is at the level of the statement (obtained
   by the class [do_it] above), or at the level of the cfg node. In the
   second case, we use the dataflow information to determine when
   two threads are live simultaneously *)
module Aux(X:
           sig
             type info

             module Access: Datatype.S with type t = rw * info * Thread.t
             module Set: sig
               include Lattice_type.Lattice_Set with type O.elt = Access.t
               val pretty_aux: Access.t Pretty_utils.formatter -> t Pretty_utils.formatter
             end
             module ZoneMap: Lmap_bitwise.Location_map_bitwise with type v = Set.t

             val thread_data: thread_state -> ZoneMap.map

             val running_concurrently: thp:thread_state -> ths:thread_state -> infop:info -> bool
           end) =
struct
  include X

  (* YYY: this is not the good approach, as a write t[i] = foo with i imprecise
     will result in a huge location, instead of a unique location with many
     offsets. However, extracting the real location require changes at many
     places. Ideally, those locations should be stocked directly the RW
     constructor itself. This has been done for W, but not for R. *)
  let fold_location f m acc =
    let module H = Z.Hashtbl in
    let aux b itvs v acc =
      try
        let l = Int_Intervals.project_set itvs in
        let by_size = H.create 4 in
        let aux_itv (ib, ie) =
          let addr = Addresses.Bits.inject b (Ival.inject_singleton ib) in
          let size = Z.succ (Z.sub ie ib) in
          try
            let prev = H.find by_size size in
            let addr = Addresses.Bits.join prev addr in
            H.replace by_size size addr
          with Not_found -> H.add by_size size addr
        in
        List.iter aux_itv l;
        H.fold
          (fun size addr_bits acc ->
             let loc = Locations.make addr_bits (`Value size) in
             f loc v acc
          ) by_size acc
      with Abstract_interp.Error_Top ->
        let addr_bits = Addresses.Bits.inject b Ival.zero in
        let size = Z_or_top.top (* TODO : use validity *) in
        let loc = Locations.make addr_bits size in
        f loc v acc
    in
    X.ZoneMap.fold_base
      (fun base -> X.ZoneMap.LOffset.fold_fuse_same (aux base)) m acc


  (* Given two threads, return a function that tells if two possible concurrent
     accesses to a variable need to be considered (ie. if they are really
     concurrent wrt. the calling structure of the threads). *)
  let consider_vars_accesses th1 th2 =
    match ThreadState.one_creates_other th1 th2 with
    | `Unrelated ->
      (* The two threads are independent, so we have no better choice
         than to assume that all their variable accesses are concurrent *)
      (fun _ _ -> true)

    | `Creates (thp, ths) ->
      (* thp creates ths. We should only consider accesses of [thp] that
         can occur after [ths] is created, but we do not necessarily
         have this information available  *)
      let before info = X.running_concurrently ~thp ~ths ~infop:info in
      if ThreadState.equal thp th1 then
        (fun (_, info, _ : X.Access.t) _ -> before info)
      else
        (fun _ (_, info, _ : X.Access.t) -> before info)
  ;;


  (* Join two sets of accesses to a same location for two given threads.
     The [consider] function must return true when the accesses are possibly
     concurrent, ie when the two threads can be live. *)
  let concurrent_accesses_sets consider s1 s2 =
    (* We basically do a cartesian product, only removing accesses that
       are guaranteed to be non concurrent *)
    X.Set.fold
      (fun o1 acc -> X.Set.fold
          (fun o2 s ->
             Mt_self.debug ~level:2
               "@[<hov>Possible concurrent access@ %a@ and %a@]"
               X.Access.pretty o1 X.Access.pretty o2;
             let is_concurrent = consider o1 o2 in
             if is_concurrent then (
               Mt_self.debug ~level:2 "@[Above access is concurrent@]";
               X.Set.join s
                 (X.Set.join (X.Set.inject_singleton o1)
                    (X.Set.inject_singleton o2))
             ) else (
               Mt_self.debug ~level:2 "@[Above access is not concurrent@]";
               s)
          ) s2 acc
      ) s1 X.Set.bottom
  ;;

  (* Compute the concurrent accesses between two threads, by considering
     all accesses to the same variable by the two threads, and by
     removing those that are not really concurrent (using
     [concurrent_accesses_sets] above) *)
  let concurrent_accesses_two_threads th1 th2 =
    Mt_self.debug ~level:2 "Concurrent accesses in threads %a and %a"
      ThreadState.pretty th1 ThreadState.pretty th2;
    let consider = consider_vars_accesses th1 th2 in
    (* not a global cache: we have a dependency on [Thread.one_creates_other],
       which is not a pure function. *)
    let cache =
      Hptmap_sig.TemporaryCache "Mt_shared_vars.concurrent_accesses_two_threads"
    in
    (* NOT [empty_neutral]: this operation is akin to an intersection. *)
    let empty_neutral = false in
    (* NOT [idempotent]: two accesses at the same statement may fail to interact
       if one thread is not yet created. *)
    let idempotent = false in
    let symmetric = false in
    let decide_fast _ _ = X.ZoneMap.LOffset.Recurse in
    let map2 = X.ZoneMap.map2
        ~cache ~symmetric ~idempotent ~empty_neutral decide_fast in
    map2
      (fun s1 s2 -> concurrent_accesses_sets consider s1 s2)
      (X.thread_data th1) (X.thread_data th2)
  ;;

  (* Basic union of two sets accesses to the same variable. We simply
     merge the sets *)
  let basic_merge_events =
    let cache = Hptmap_sig.PersistentCache "Mt_shared_vars.basic_merge_events" in
    let empty_neutral = true in
    let idempotent = true in
    let symmetric = true in
    let decide_fast _ _ = X.ZoneMap.LOffset.Recurse in
    (* Partial application is important *)
    X.ZoneMap.map2
      ~cache ~symmetric ~idempotent ~empty_neutral decide_fast X.Set.join

  type list_accesses =
    (Memory_zone.t * X.Set.t) list

  (* Compute all the concurrent accesses to all the variables. For each
     thread, we consider its possible concurrent accesses with all
     the other threads. Algorithmically, there is no need to consider
     the accesses between (th1, th2) and (th2, th1), as the relation
     is symmetric. Hence we consider only half the cases. *)
  let concurrent_accesses_all_threads all_threads :
    (list_accesses * list_accesses) * _ =
    let rec aux acc = function
      | [] -> acc
      | th :: thq ->
        let rec aux' acc = function
          | [] -> acc
          | th' :: thq' ->
            let m = concurrent_accesses_two_threads th th' in
            aux' (basic_merge_events m acc) thq'
        in
        aux (aux' acc thq) thq
    in
    let all = aux X.ZoneMap.empty_map all_threads in
    (*  Gather possible data races into two different lists. At this write/write
        dataraces are separated from read/write dataraces.
    *)
    X.ZoneMap.fold_fuse_same
      (fun z s ((write_write_races, read_write_races) as acc) ->
         let read_access, write_access = X.Set.fold
             (fun (op, _, _) (read, write) ->
                match op with
                | Read -> (true, write)
                | Write _ -> (read, true)
                | ReadPos _ -> (true, write)
                | WritePos _ -> (read, true)
             ) s (false, false)
         in match read_access, write_access with
         | false, false -> acc (* no access at all, [s] is empty *)
         | true, false -> (* not a race condition *) acc
         | false, true ->
           (* write/write race *)
           if Mt_options.WriteWriteRaces.get ()
           then (z, s) :: write_write_races, read_write_races
           else acc
         | true, true -> (* read/write race *)
           write_write_races, (z, s) :: read_write_races
      ) all ([], []),
    all


  let pretty_concurrent_accesses ?(f=(fun _fmt _ -> ())) () fmt
      (l:list_accesses) =
    if l = [] then Format.fprintf fmt "none"
    else
      Format.fprintf fmt "@[<v 1>%a@]"
        (Pretty_utils.pp_list ~sep:"@ "
           (fun fmt (z, s) -> Format.fprintf fmt "@[<v 0>%a:@ @[<hov>%a@]@]"
               Memory_zone.pretty z (X.Set.pretty_aux f) s
           ))
        l

  let all_zones_accessed (l: list_accesses) =
    let aux acc (z, _) = Memory_zone.join z acc in
    List.fold_left aux Memory_zone.bottom l

end




module Global = Aux(
  struct
    type info = stmt
    let thread_data th = th.th_read_written
    module Access = StmtIdAccess
    module Set = SetStmtIdAccess
    module ZoneMap = AccessesByZone

    (* For this analysis, we do not try to find if the two threads run
       concurrently. This will be done later through the cfg *)
    let running_concurrently ~thp:_ ~ths:_ ~infop:_ = true
  end)


module Precise = struct
  include Aux(
    struct
      type info = node
      let thread_data th = th.th_read_written_cfg

      module Access = NodeIdAccess
      module Set = SetNodeIdAccess
      module ZoneMap = AccessesByZoneNode

      let running_concurrently ~thp:_ ~ths ~infop =
        let context = infop.cfgn_context in
        match ThreadPresence.find context.started_threads ths.th_eva_thread with
        | NotPresent -> false
        | MaybePresent | Present -> true

    end)

  (* validity should not be [Invalid] *)
  let default_offsetmap validity =
    let size = Cvalue.V_Offsetmap.size_from_validity validity in
    let size = Lattice_bounds.Bottom.non_bottom size in
    Cvalue.V_Offsetmap.create_isotropic ~size Cvalue.V_Or_Uninitialized.bottom

  let extract_shared_value node op (loc : Locations.t) state =
    match loc.size with
    | `Top ->
      Mt_self.warning ?source:(CfgNode.node_first_loc node)
        "Ignoring imprecise %a at %a"
        Mt_types.RW.pretty op Locations.pretty loc;
      []
    | `Value size ->
      Addresses.Bits.fold_topset_ok
        (fun base offs acc ->
           let validity = Base.validity base in
           if Base.Validity.equal Base.Invalid validity then
             acc
           else
             let default = default_offsetmap validity in
             let v = Cvalue.Model.find ~conflate_bottom:false state loc in
             let r = Cvalue.V_Offsetmap.update
                 ~validity:(Base.validity base)
                 ~exact:true ~offsets:offs ~size
                 (Cvalue.V_Or_Uninitialized.C_init_noesc v)
                 default
             in
             match r with
             | `Bottom -> acc
             | `Value offsm -> (base, offsm)::acc
        )
        loc.addr
        []

  let pp_stack fmt node =
    Format.fprintf fmt "@ // %a" CfgNode.pretty_stmts node;
    if Mt_options.DumpSharedVarsValues.get () > 1 then
      Format.fprintf fmt "@ %a" Callstack.pretty node.cfgn_stack

  let pp_access (op, node, th) base offsm =
    if Mt_options.DumpSharedVarsValues.get () > 0 then
      Mt_self.result ~once:true "@[%a %as @ @[%a%a@]@ %a@]"
        Thread.pretty th Mt_types.RW.pretty op Base.pretty base
        (Cvalue.V_Offsetmap.pretty_generic ?typ:(Base.typeof base) ()) offsm
        pp_stack node


  let display_shared_vars_value m =
    fold_location
      (fun loc s () ->
         SetNodeIdAccess.fold
           (fun (op, node, _thid as access) () ->
              match op with
              | ReadPos _ | WritePos _ ->
                Mt_self.not_yet_implemented ~current:true ~once:true
                  "MtSharedVars.Precise.display_shared_vars_value for ALoc"
              | Write _ -> ()
              | Read ->
                let state = node.cfgn_value_state.state_before in
                let shared =  extract_shared_value node op loc state in
                List.iter (fun (base, offsm) -> pp_access access base offsm) shared)
           s
           ())
      m
      ()

  module WriteSeen =
    Datatype.Triple_with_collections (CfgNode) (Thread) (Locations)

  let enumerate_written_vars_value m =
    let aux _b _itvs s acc =
      let aux_nodes (op, node, th as access) (seen, _wr as acc) =
        match op with
        | ReadPos _ | WritePos _ ->
          Mt_self.not_yet_implemented ~current:true ~once:true
            "MtSharedVars.Precise.enumerate_written_vars_value for ALoc"
        | Read -> acc
        | Write loc ->
          if not (WriteSeen.Set.mem (node, th, loc) seen) then
            let state = node.cfgn_value_state.state_after in
            let shared = extract_shared_value node op loc state in
            List.fold_left (fun (seen,wr) (base, offsm) ->
                pp_access access base offsm;
                let seen = WriteSeen.Set.add (node, th, loc) seen in
                (seen, (th, base, offsm) :: wr))
              acc
              shared
          else acc
      in
      SetNodeIdAccess.fold aux_nodes s acc
    in
    let _seen, wr =
      AccessesByZoneNode.fold_base
        (fun base -> AccessesByZoneNode.LOffset.fold_fuse_same (aux base))
        m (WriteSeen.Set.empty, [])
    in
    wr

  let join_shared_values l =
    let aux m (_id, base, offsm) =
      try
        let offsm' = Cvalue.Model.find_base base m in
        match offsm' with
        | `Top -> Mt_self.fatal "Top state"
        | `Bottom -> m (* base invalid. Probably impossible case *)
        | `Value offsm' ->
          let join = Cvalue.V_Offsetmap.join offsm offsm' in
          Cvalue.Model.add_base base join m
      with Not_found -> (* from Cvalue.Model.find_base *)
        Cvalue.Model.add_base base offsm m
    in
    List.fold_left aux Cvalue.Model.empty_map l


  (* Remove from the field [concur_accesses] of cfg nodes the zones
     that are not really concurrent. Then flag the node either as
     [NotReallySharedVar] or [SharedVarNonConcurrentAccess], depending
     on whether some zones remain. *)
  let remove_non_concur_zones_from_cfg all_zones cfg =
    let update_zones n =
      let filtered = SetZoneAccess.filter
          (fun (_, z) ->
             not (Memory_zone.equal Memory_zone.bottom
                    (Memory_zone.narrow all_zones z)))
          n.cfgn_var_access.concur_accesses
      in
      let kind =
        if SetZoneAccess.equal filtered SetZoneAccess.empty
        then NotReallySharedVar
        else SharedVarNonConcurrentAccess (* for now *)
      in
      n.cfgn_var_access <- { concur_accesses = filtered;
                             var_access_kind = kind }
    in
    CfgNode.iter ~f_before:update_zones cfg
  ;;

  (* Given a list of zone of accesses that are supposed to be the really
     concurrent ones (typically obtained by the function
     [concurrent_accesses_all_threads] of this module), mark all the relevant
     nodes as containing a really concurrent access *)
  let mark_concur_access_in_cfg l =
    let mark_useful (_z, s) =
      SetNodeIdAccess.iter
        (fun (_rw, n, _id) ->
           n.cfgn_var_access <-
             { n.cfgn_var_access with var_access_kind = ConcurrentAccess }
        ) s
    in
    List.iter mark_useful l

end


let register_concurrent_var_accesses analysis states =
  let kf = current_fun analysis in
  (* In the precise computation of shared vars, we prefer to have all accesses
     to a shared variable , even if the access itself is not concurrent. Hence
     we set [is_multithread] to a function that always return false *)
  let is_multithreaded = fun _ -> true in
  let ki = calling_ki analysis in
  let sa = match states with
    | `Final h -> h
    | `Leaf state ->
      match ki with
      | Kglobal -> assert false
      | Kstmt s ->
        let h = Stmt.Hashtbl.create 1 in
        Stmt.Hashtbl.add h s state;
        h
  in
  let accesses = read_written_by_function
      is_multithreaded analysis.curr_thread.th_eva_thread (Local sa)
      ~watch_only:analysis.concurrent_accesses kf ki
  in
  (* We transform the various accesses into mthread events *)
  AccessesByZone.fold
    (fun z set () ->
       SetStmtIdAccess.iter
         (fun (rw, stmt, _id) ->
            let top = Stack.access_to_var stmt in
            Mt_thread.register_event analysis ~top (VarAccess (rw, z))
         ) set
    ) accesses ()
