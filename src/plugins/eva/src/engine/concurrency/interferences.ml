(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds

module Pos = Position.Local
module PosMap = Pos.Map
module MutexesMap = Map.Make (Mutex.Set)

type thread_id = int

let dkey =
  Self.register_category "interferences"
    ~help:"debug messages about interferences from other threads \
           injected in Eva analysis with Mthread"

let pp_map iteri pp_key pp_val fmt map =
  let pp fmt k v =
    Format.fprintf fmt "@,@[<hov>%a@]:@;<1 2>@[<hov>%a@]" pp_key k pp_val v
  in
  iteri (pp fmt) map

(** An interference from on or several threads. *)
module Interference (Dom : Engine_abstractions_sig.Domain) =
struct
  type t = {
    (** The interference state. *)
    state : Dom.t or_top;

    (** The accesses that are the cause of the interference. *)
    access : Inout_access.t;
  }

  (** [is_included l r] returns [true] if the interference state of [l] is
      included in the interference state of [r].
      By construction, the accesses are included if the states are included. *)
  let is_included l r =
    Top.is_included Dom.is_included l.state r.state

  (** [join l r] joins the interfence. *)
  let join l r =
    let dom_join l r = `Value (Dom.join l r) in
    { state = Top.join dom_join l.state r.state;
      access = Inout_access.Access.join l.access r.access }

  (** [widen kf stmt l r] is the over-approximation of [join l r]. Assumes that
      [l] is included in [r] so the accesses are taken from [r] directly. *)
  let widen kf stmt l r =
    let dom_widen l r = `Value (Dom.widen kf stmt l r) in
    { r with state = Top.join dom_widen l.state r.state }

  (** [locked_mutexes interference] returns the set of mutexes locked in the
      interference state. *)
  let locked_mutexes { state; _ } =
    match state with
    | `Top -> Mutex.Set.empty
    | `Value state ->
      match Dom.get Mt_domain.Domain.key with
      | None -> Mutex.Set.empty
      | Some extract ->
        Mt_domain.Domain.mutexes (extract state)
        |> Mt_register.Mutex.locked_mutexes

  (** [equal ~bases l r] returns [true] if the state and accesses of [l] and [r]
      are equal. The states are [project]ed on [bases] before the comparison. *)
  let equal ~bases l r =
    let lstate = Top.map (Dom.project bases) l.state in
    let rstate = Top.map (Dom.project bases) r.state in
    Top.equal Dom.equal lstate rstate
    && Inout_access.Access.equal l.access r.access

  (** Pretty-printer for an interference. The state is projected on [bases]
      before printing. *)
  let pretty ~bases fmt { state; _ } =
    let state = Top.map (Dom.project bases) state in
    Top.pretty Dom.pretty fmt state

end

(* Set of interferences stored as a map from the set of mutexes surely
   locked to the corresponding interferences states. *)

module ByMutexes (Dom : Engine_abstractions_sig.Domain) =
struct
  module Interference = Interference (Dom)
  type t = Interference.t MutexesMap.t

  let pretty ~bases : Format.formatter -> t -> unit =
    let pp_key = Mutex.Set.pretty in
    pp_map MutexesMap.iter pp_key (Interference.pretty ~bases)

  let equal ~bases : t -> t -> bool =
    MutexesMap.equal (Interference.equal ~bases)
end


(* Set of interferences stored as a map from position to the
   interference generated at this control state. *)

module ByPosition (Dom : Engine_abstractions_sig.Domain) =
struct
  module Interference = Interference (Dom)

  type elt =
    {
      interference : Interference.t;
      widening_counter : int;
    }

  type t = elt PosMap.t

  let pretty ~bases fmt map =
    let pp_val fmt { interference; _ } = Interference.pretty ~bases fmt interference
    and pp_key = Pos.pretty_loc in
    pp_map PosMap.iter pp_key pp_val fmt map

  let empty : t = PosMap.empty

  let add_and_widen (pos : Pos.t) (interference : Interference.t) : t -> t =
    let update = function
      | None -> (* No previous entry *)
        let widening_delay = Parameters.WideningDelay.get () in
        Some { interference ; widening_counter = widening_delay - 1 }

      | Some previous -> (* Some previous entry *)
        if Interference.is_included interference previous.interference then
          Some previous
        else
          let interference =
            Interference.join previous.interference interference
          in
          let interference, widening_counter =
            if previous.widening_counter > 0 then
              (* No widening *)
              interference, previous.widening_counter
            else begin
              (* Widen the interferences between the previous and current
                 state. *)
              let widening_period = Parameters.WideningPeriod.get () in
              let stmt, cs = pos in
              let kf = Callstack.top_kf cs in
              Interference.widen kf stmt previous.interference interference,
              widening_period
            end
          in
          Some { interference ; widening_counter = widening_counter - 1 }
    in
    PosMap.update pos update

  let group_by_mutexes (map : t) : Interference.t MutexesMap.t =
    let add _pos { interference ; _ } acc_map =
      let locked_mutexes = Interference.locked_mutexes interference in
      let update = function
        | None -> Some interference
        | Some previous ->
          let interference = Interference.join previous interference in
          Some interference
      in
      MutexesMap.update locked_mutexes update acc_map
    in
    PosMap.fold add map MutexesMap.empty
end


(* Interferences Functor *)

module type Engine_Subset = sig
  include Engine_abstractions_sig.S
  include Engine_sig.Results with type state := Dom.state
                              and type value := Val.t
                              and type location := Loc.location
end

module Make (Engine : Engine_Subset) =
struct
  module Dom = Engine.Dom
  module ThreadTable = Thread.Hashtbl
  module ByPosition = ByPosition (Dom)
  module ByMutexes = ByMutexes (Dom)
  module Interference = Interference (Dom)

  type state = Dom.t

  type t = {
    interferences_by_pos : ByPosition.t ThreadTable.t;
    interferences_by_mutexes :  ByMutexes.t ThreadTable.t;
    mutable shared_bases : Base.Hptset.t;
  }

  let current = {
    interferences_by_pos = ThreadTable.create 13;
    interferences_by_mutexes = ThreadTable.create 13;
    shared_bases = Base.Hptset.empty;
  }

  let reset () =
    ThreadTable.reset current.interferences_by_pos;
    ThreadTable.reset current.interferences_by_mutexes;
    current.shared_bases <- Base.Hptset.empty

  let is_empty () =
    ThreadTable.length current.interferences_by_pos = 0

  (* Interference registration *)

  type add_result =
    | Updated
    | NoChanges

  let add_last_analysis thread concurrent_writes shared_bases =
    (* Retrieve the interferences  *)
    let default = ByPosition.empty in
    let old_interferences_by_pos =
      ThreadTable.find_default ~default current.interferences_by_pos thread
    in
    let new_interferences_by_pos =
      let add (stmt, callstack as pos) acc_map =
        let source = Pos.pos pos in
        let state = Engine.get_state ~callstack (After stmt) in
        match state with
        | `Bottom -> acc_map (* no interference to add *)
        | `Top | `Value _ as state ->
          let filter =
            Inout_access.mk_filter
              ~filter_base:(fun base -> Base.Hptset.mem base shared_bases)
          in
          let access = Inout_access.at ~filter (Position.of_local pos) in
          if Top.is_top state then
            Self.warning ~once:false ~source
              "Imprecise interference computed";
          let interference : Interference.t = { state; access } in
          ByPosition.add_and_widen pos interference acc_map
      in
      Pos.Set.fold add concurrent_writes old_interferences_by_pos
    in
    (* Check for changes *)
    let new_interferences_by_mutexes =
      ByPosition.group_by_mutexes new_interferences_by_pos
    and old_interferences_by_mutexes =
      ThreadTable.find_opt current.interferences_by_mutexes thread
    in
    let same_interferences = match old_interferences_by_mutexes with
      | None -> MutexesMap.is_empty new_interferences_by_mutexes
      | Some old ->
        (* Project interferences on current shared bases to check equality so
           that unshared memory do not contribute to the test. *)
        ByMutexes.equal ~bases:shared_bases new_interferences_by_mutexes old
    in
    let same_shared_bases =
      Base.Hptset.equal current.shared_bases shared_bases
    in
    (* Update the record *)
    ThreadTable.replace current.interferences_by_pos
      thread new_interferences_by_pos;
    ThreadTable.replace current.interferences_by_mutexes
      thread new_interferences_by_mutexes;
    current.shared_bases <- shared_bases;
    (* Debug printing *)
    let pp_write fmt (stmt, _cs as pos)  =
      Format.fprintf fmt "%a@ %a" Cil_datatype.Stmt.pretty stmt Pos.pretty pos
    in
    let pp_write_set fmt set =
      let pp fmt pos = Format.fprintf fmt "@,@[<hov 2>%a@]" pp_write pos in
      Pos.Set.iter (pp fmt) set
    in
    Self.debug ~dkey
      "@[<v 2>Interferences from thread %a@ \
       @[<v 2>concurrent writes:%a@]@ \
       @[<hov 2>shared bases:@ %a@]@ \
       @[<v 2>interferences by location:%a@]@ \
       @[<v 2>interferences by mutexes:%a@]@]"
      Thread.pretty thread
      pp_write_set concurrent_writes
      Base.Hptset.pretty shared_bases
      (ByPosition.pretty ~bases:shared_bases) new_interferences_by_pos
      (ByMutexes.pretty ~bases:shared_bases) new_interferences_by_mutexes;
    if not (same_interferences && same_shared_bases)
    then Updated
    else NoChanges

  (* Interference injection *)

  let applicable current_thread (state : state) : Interference.t or_bottom =
    let threads, mutexes = match Dom.get Mt_domain.Domain.key with
      (* Domain disabled, no information about threads and mutexes *)
      | None -> Mt_register.Thread.empty, Mutex.Set.empty
      (* Domain enabled *)
      | Some extract ->
        let mt_state = extract state in
        Mt_domain.Domain.threads mt_state,
        Mt_domain.Domain.mutexes mt_state |> Mt_register.Mutex.locked_mutexes
    in
    let add mutexes' interference' acc_interference =
      if Mutex.Set.disjoint mutexes mutexes' then
        (* No mutexes in common, this interference is applicable *)
        Bottom.join Interference.join acc_interference (`Value interference')
      else
        (* At least one mutex in common, this interfence cannot apply *)
        acc_interference
    in
    let add_thread thread interferences_map acc_interference =
      let is_current_thread = Thread.(equal thread current_thread) in
      let maybe_running =
        match Mt_register.Thread.find thread threads with
        (* Thread status is unknown, consider that the thread might be running*)
        | None -> true
        (* Thread status is known *)
        | Some status -> Mt_utils.Trilean.maybe_true status.running
      in
      let can_thread_interfere = maybe_running && not is_current_thread in
      if can_thread_interfere
      then MutexesMap.fold add interferences_map acc_interference
      else acc_interference
    in
    ThreadTable.fold
      add_thread
      current.interferences_by_mutexes
      `Bottom

  let inject current_thread state =
    match applicable current_thread state with
    | `Bottom -> state
    | `Value { state = `Top; _ } -> Dom.top
    | `Value { state = `Value interferences_state; access; _ } ->
      let written_shared_bases =
        let written_bases = Memory_zone.get_bases access.write in
        Base.SetLattice.(inject current.shared_bases
                         |> inter written_bases
                         |> project)
      in
      let result =
        Dom.overwrite written_shared_bases ~on:state ~by:interferences_state
      in
      Dom.join state result

  let inject_init_state th kf state =
    if is_empty () ||
       (Thread.is_main th &&
        (Thread.interrupt_handlers () = [])) then
      (* Identity if there are no interferences or if we are analyzing the main
         thread and there are no interrupt handlers declared. *)
      state
    else begin
      Self.debug ~dkey ~once:true
        "inject threads interferences at the start of %a for thread %a"
        Kernel_function.pretty kf
        Thread.pretty th;
      inject th state
    end

  let inject_after_change ~pos access state =
    let need_injection () =
      let access = Inout_access.keep_globals_only access in
      let zone = Memory_zone.join access.read access.write in
      match Memory_zone.get_bases zone with
      | Top ->
        (* Shared memory is Top, always inject *)
        Self.warning ~current:true ~once:true
          "imprecise memory footprint computed at this point";
        true
      | Set bases ->
        (* Inject only if the read/written memory intersects
           shared memory *)
        Base.Hptset.intersects bases current.shared_bases
    in
    if is_empty () || not (need_injection ())
    then
      (* Identity if there are no interferences or if we know that the last
         transfer function did not interact with shared memory. *)
      state
    else begin
      Self.debug ~dkey ~current:true ~once:true
        "inject threads interferences at this point";
      let current_thread = Thread.from_position pos in
      inject current_thread state
    end

end
