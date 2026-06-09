(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_cil
open Mt_memory.Types

(* -------------------------------------------------------------------------- *)
(* --- Variable access kind                                               --- *)
(* -------------------------------------------------------------------------- *)


type rw = Read | Write of Locations.t
        | ReadPos of Position.t | WritePos of Position.t

module RW = struct
  include Datatype.Make(
    struct
      include Datatype.Serializable_undefined
      type t = rw
      let name = "rw"
      let reprs = [Read]
      let equal rw1 rw2 = match rw1, rw2 with
        | Read, Read -> true
        | Write l1, Write l2 -> Locations.equal l1 l2
        | Read, Write _ | Write _, Read -> false
        | ReadPos pos1, ReadPos pos2
        | WritePos pos1, WritePos pos2 -> Position.equal pos1 pos2
        | ReadPos _, WritePos _ | WritePos _, ReadPos _ -> false
        | (Read | Write _), (ReadPos _ | WritePos _)
        | (ReadPos _ | WritePos _), (Read | Write _) -> false
      let compare rw1 rw2 = match rw1, rw2 with
        | Read, Read -> 0
        | Write l1, Write l2 -> Locations.compare l1 l2
        | Read, Write _ -> -1
        | Write _, Read -> 1
        | ReadPos pos1, ReadPos pos2
        | WritePos pos1, WritePos pos2 -> Position.compare pos1 pos2
        | ReadPos _, WritePos _ -> -1
        | WritePos _, ReadPos _ -> +1
        | (Read | Write _), (ReadPos _ | WritePos _) -> -2
        | (ReadPos _ | WritePos _), (Read | Write _) -> +2
      let hash = function
        | ReadPos pos -> 1 + Hashtbl.hash (1, Position.hash pos)
        | WritePos pos -> 1 + Hashtbl.hash (2, Position.hash pos)
        | Write l -> 1 + Hashtbl.hash (3, Locations.hash l)
        | Read -> 0
      let pretty fmt rw = Format.fprintf fmt "%s"
          (match rw with
           | Read -> "read"
           | Write _ -> "write"
           | ReadPos _ -> "read"
           | WritePos _ -> "write"
          )
    end)

  let loc op =
    match op with
    | Read | Write _ -> Fileloc.unknown
    | ReadPos pos | WritePos pos -> Position.loc pos

  let is_read op =
    match op with
    | Read -> true
    | Write _ -> false
    | ReadPos _ -> true
    | WritePos _ -> false

  let pretty_op fmt rw = pretty fmt rw

  let pretty_loc fmt rw =
    match rw with
    | Read -> Format.fprintf fmt "<noloc>"
    | Write l -> Locations.pretty fmt l
    | ReadPos pos | WritePos pos -> Position.pretty fmt pos
end

(* -------------------------------------------------------------------------- *)
(* --- Multi-threading events                                             --- *)
(* -------------------------------------------------------------------------- *)

type event =
  | CreateThread of Thread.t
  | StartThread of Thread.t
  | SuspendThread of Thread.t
  | CancelThread of Thread.t
  | ThreadExit of value
  | MutexLock of Mutex.t
  | MutexRelease of Mutex.t
  | CreateQueue of Mqueue.t * int option
  | SendMsg of Mqueue.t * (slice * int)
  | ReceiveMsg of Mqueue.t * pointer * int
  | VarAccess of rw * Memory_zone.t
  | Dummy of string * value list

module Event = struct
  type t = event

  (* Used for comparison. *)
  let rank = function
    | CreateThread _ -> 0
    | StartThread _ -> 1
    | SuspendThread _ -> 2
    | CancelThread _ -> 3
    | ThreadExit _ -> 4
    | MutexLock _ -> 5
    | MutexRelease _ -> 6
    | CreateQueue _ -> 7
    | SendMsg _ -> 8
    | ReceiveMsg _ -> 9
    | VarAccess _ -> 10
    | Dummy _ -> 11

  let pretty fmt = function
    | CreateThread th -> Format.fprintf fmt "Create thread %a" Thread.pretty th
    | StartThread th -> Format.fprintf fmt "Start thread %a" Thread.pretty th
    | SuspendThread th -> Format.fprintf fmt "Suspend thread %a" Thread.pretty th
    | CancelThread th -> Format.fprintf fmt "Cancel thread %a" Thread.pretty th
    | ThreadExit v -> Format.fprintf fmt "Thread exit, with code %a"
                        Cvalue.V.pretty v
    | MutexLock m -> Format.fprintf fmt "Lock %a" Mutex.pretty m
    | MutexRelease m -> Format.fprintf fmt "Release %a" Mutex.pretty m
    | CreateQueue (q, s) ->
      Format.fprintf fmt "Creating queue %a%a" Mqueue.pretty q
        (fun fmt -> function None -> ()
                           | Some s -> Format.fprintf fmt " (size %d)" s) s
    | SendMsg (q, (v, _s)) -> Format.fprintf fmt
                                "Sending@ message@ on %a,@ content@ %a"
                                Mqueue.pretty q Mt_memory.pretty_slice v
    | ReceiveMsg (q, loc, size) -> Format.fprintf fmt
                                     "Receiving@ message@ on %a,@ max size %d,@ stored in %a."
                                     Mqueue.pretty q size Pointer.pretty loc
    | VarAccess (rw, loc) ->
      Format.fprintf fmt "Var access@ %a@ of %a"
        RW.pretty rw Memory_zone.pretty loc
    | Dummy (s, l) ->
      Format.fprintf fmt "%s %a" s
        (Pretty_utils.pp_list ~sep:"@ " Cvalue.V.pretty) l

  let equal a1 a2 = match a1, a2 with
    | CreateThread th1, CreateThread th2
    | StartThread th1, StartThread th2
    | SuspendThread th1, SuspendThread th2
    | CancelThread th1, CancelThread th2 -> Thread.equal th1 th2
    | MutexLock m1, MutexLock m2
    | MutexRelease m1, MutexRelease m2 -> Mutex.equal m1 m2
    | CreateQueue (q1, s1), CreateQueue (q2, s2) ->
      Mqueue.equal q1 q2 && s1 = s2
    | SendMsg (q1, (v1, s1)), SendMsg (q2, (v2, s2)) ->
      Mqueue.equal q1 q2 && Cvalue.V_Offsetmap.equal v1 v2 && s1 = s2
    | ReceiveMsg (q1, l1, s1), ReceiveMsg (q2, l2, s2) ->
      s1 = s2 && Mqueue.equal q1 q2 && Pointer.equal l1 l2
    | VarAccess (rw1, z1), VarAccess (rw2, z2) ->
      RW.equal rw1 rw2 && Memory_zone.equal z1 z2
    | (CreateThread _ | StartThread _ | SuspendThread _ | CancelThread _
      | ThreadExit _
      | MutexLock _ | MutexRelease _ | CreateQueue _
      | SendMsg _ | ReceiveMsg _ | VarAccess _ | Dummy _), _ ->
      false


  let (<?>) c lcmp = if c <> 0 then c else Lazy.force lcmp

  let compare a1 a2 = match a1, a2 with
    | CreateThread th1, CreateThread th2
    | StartThread th1, StartThread th2
    | SuspendThread th1, SuspendThread th2
    | CancelThread th1, CancelThread th2 -> Thread.compare th1 th2
    | MutexLock m1, MutexLock m2
    | MutexRelease m1, MutexRelease m2 -> Mutex.compare m1 m2
    | ThreadExit v1, ThreadExit v2 -> Cvalue.V.compare v1 v2
    | CreateQueue (q1, s1), CreateQueue (q2, s2) ->
      compare s1 s2 <?> lazy (Mqueue.compare q1 q2)
    | SendMsg (q1, (v1, s1)), SendMsg (q2, (v2, s2)) ->
      Stdlib.compare s1 s2 <?>
      lazy (Cvalue.V_Offsetmap.compare v1 v2) <?>
      lazy (Mqueue.compare q1 q2)
    | ReceiveMsg (q1, l1, s1), ReceiveMsg (q2, l2, s2) ->
      Stdlib.compare s1 s2 <?>
      lazy (Pointer.compare l1 l2) <?>
      lazy (Mqueue.compare q1 q2)
    | VarAccess (rw1, z1), VarAccess (rw2, z2) ->
      RW.compare rw1 rw2 <?>
      lazy (Memory_zone.compare z1 z2)
    | Dummy (s1, l1), Dummy (s2, l2) ->
      String.compare s1 s2 <?>
      lazy ((List.compare Cvalue.V.compare) l1 l2)
    | (CreateThread _ | StartThread _ | SuspendThread _ | CancelThread _
      | ThreadExit _
      | MutexLock _ | MutexRelease _ | CreateQueue _
      | SendMsg _ | ReceiveMsg _ | VarAccess _ | Dummy _), _ ->
      rank a1 - rank a2

  let hash = function
    | CreateThread th -> Hashtbl.hash (Thread.hash th, 0)
    | CancelThread th -> Hashtbl.hash (Thread.hash th, 1)
    | MutexLock m -> Hashtbl.hash (Mutex.hash m, 2)
    | MutexRelease m -> Hashtbl.hash (Mutex.hash m, 3)
    | CreateQueue (q, s) -> Hashtbl.hash (Mqueue.hash q, s, 4)
    | SendMsg (q, (v, s)) ->
      Hashtbl.hash (Mqueue.hash q, Cvalue.V_Offsetmap.hash v, s, 5)
    | ReceiveMsg (q, l, size) ->
      Hashtbl.hash (Mqueue.hash q, Pointer.hash l, size, 6)
    | VarAccess (rw, z) -> Hashtbl.hash (RW.hash rw, Memory_zone.hash z, 7)
    | ThreadExit v -> Hashtbl.hash (Cvalue.V.hash v, 8)
    | Dummy (s, l) -> Hashtbl.hash (s, List.map Cvalue.V.hash l, 9)
    | StartThread th -> Hashtbl.hash (Thread.hash th, 10)
    | SuspendThread th -> Hashtbl.hash (Thread.hash th, 11)

end

module EventsSet = struct
  include Set.Make(Event)

  let threads_created s =
    fold (fun act l -> match act with
        | CreateThread id -> id :: l
        | _ -> l) s []

  let pretty ?(sep=("@ ": (_, _, _, _, _, _) format6)) () fmt =
    Pretty_utils.pp_iter ~pre:"" ~suf:"" ~sep iter Event.pretty fmt

end
type events_set = EventsSet.t


(* -------------------------------------------------------------------------- *)
(* --- Execution traces                                                   --- *)
(* -------------------------------------------------------------------------- *)

module Trace =
struct

  module TriesStacks = Trie.Make(Map.Make(StackElt))

  type data = {
    trace_events: events_set;
    trace_states: state Cil_datatype.Stmt.Map.t;
    trace_states_after: state Cil_datatype.Stmt.Map.t;
  }

  let join_data d1 d2 = {
    trace_events = EventsSet.union d1.trace_events d2.trace_events;
    trace_states = merge_map_functions_states d1.trace_states d2.trace_states;
    trace_states_after =
      merge_map_functions_states d1.trace_states_after d2.trace_states_after;
  }

  type t = data TriesStacks.t

  let empty = TriesStacks.empty

  let is_empty = TriesStacks.is_empty

  let default = {
    trace_events = EventsSet.empty;
    trace_states = Cil_datatype.Stmt.Map.empty;
    trace_states_after = Cil_datatype.Stmt.Map.empty;
  }

  let union = TriesStacks.union (fun _ d1 d2 -> Some (join_data d1 d2))

  let add_prefix = TriesStacks.add_prefix

  let add_aux f (trie: t) (stack : stack) =
    let cur =
      try TriesStacks.find stack trie
      with Not_found -> default
    in
    TriesStacks.add stack (f cur) trie

  let add_event t s evt =
    add_aux
      (fun d -> { d with trace_events = EventsSet.add evt d.trace_events}) t [s]

  let add_states t ~before ~after =
    add_aux
      (fun d -> { d with
                  trace_states = merge_map_non_map_functions_states d.trace_states before;
                  trace_states_after =
                    merge_map_non_map_functions_states d.trace_states_after after;
                })
      t []

  let subtrace_at_call trie call =
    try TriesStacks.select_prefix call trie
    with Not_found -> empty

  let no_deep_call trie =
    (* this is true if the trie only contains a singleton key of size 1 *)
    TriesStacks.prefixes_seq trie () = Seq.Nil


  let find_at_stmt trie stmt =
    TriesStacks.prefixes_seq trie
    |> Seq.filter
      (fun ((_,kinstr), _) ->
         match kinstr with
         | Kglobal -> false
         | Kstmt s -> Cil_datatype.Stmt.equal s stmt)
    |> List.of_seq


  let at_root trie =
    TriesStacks.find_opt [] trie

  let at_call trie call =
    try Some (TriesStacks.find [call] trie)
    with Not_found -> None

  let fold (trie : t) f =
    TriesStacks.fold
      (fun stack d -> EventsSet.fold (f stack) d.trace_events) trie

  let fold' t f = fold t (fun _ -> f)

  let iter (trie : t) f =
    TriesStacks.iter
      (fun stack d -> EventsSet.iter (f stack) d.trace_events) trie

  let iter' t f = iter t (fun _ -> f)

  let exists (trie : t) f =
    TriesStacks.exists
      (fun stack d -> EventsSet.exists (f stack) d.trace_events) trie

  let find_events f t =
    fold' t (fun evt acc -> if f evt then EventsSet.add evt acc else acc)
      EventsSet.empty


  let pretty fmt t =
    Format.fprintf fmt "@[<v>";
    TriesStacks.iter
      (fun stack d ->
         Format.fprintf fmt
           "stack:@ %a@ actions:@[%a@]@ @ "
           Stack.pretty stack (EventsSet.pretty ()) d.trace_events) t;
    Format.fprintf fmt "@]@.";
end


(* -------------------------------------------------------------------------- *)
(* --- Live threads/taken mutexes at a given point of execution           --- *)
(* -------------------------------------------------------------------------- *)

type presence_flag = NotPresent | Present | MaybePresent

module PresenceFlag = struct

  include Datatype.Make(
    struct
      include Datatype.Serializable_undefined
      type t = presence_flag
      let name = "Mt_types.presence_flag"
      let reprs = [NotPresent; Present; MaybePresent]
      let equal : t -> t -> _ = (=)
      let compare : t -> t -> int = Stdlib.compare
      let hash : t -> _ = Hashtbl.hash
    end)

  let combine p1 p2 = match p1, p2 with
    | Present, Present -> Present
    | NotPresent, NotPresent -> NotPresent
    | _ -> MaybePresent

  let fast_equal = equal

end


module type Presence = sig
  type key
  type t

  module KeySet: Datatype.Set with type elt = key

  val pretty: t Pretty_utils.formatter

  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int

  val empty: t
  val is_empty: t -> bool

  val find: t -> key -> presence_flag

  val add: key -> presence_flag -> t -> t

  val combine: t -> t -> t

  val only_present: t -> KeySet.t

  val all_present: t -> KeySet.t
end


module MakePresence (Key: Datatype.S_with_collections) = struct
  (* Implementation of maps on threads with hashing information. Invariant:
     we never store [NotPresent] inside the table, as it is the default
     value, and this introduces non-canonicity problems. (This is not
     disastrous per se, but this also implies that [equal m1 m2] does not
     imply [hash m1 = hash m2], a bad idea...) *)
  module M = Rangemap.Make(Key)(PresenceFlag)

  type t = M.t
  type key = Key.t

  module KeySet = Key.Set

  let pretty_with_flag fmt (th, p) = match p with
    | NotPresent -> ()
    | MaybePresent -> Format.fprintf fmt "(?)%a" Key.pretty th
    | Present -> Format.fprintf fmt "%a" Key.pretty th

  let pretty = Pretty_utils.pp_iter ~pre:"" ~suf:"" ~sep:"@ "
      (fun f -> M.iter (fun k v -> f (k, v))) pretty_with_flag

  let equal = M.equal
  let compare = M.compare
  let hash = M.hash

  let find (p : t) id =
    try M.find id p
    with Not_found -> NotPresent

  let add k v m =
    match v with
    | NotPresent -> M.remove k m
    | _ -> M.add k v m

  let conv = function
    | None -> NotPresent
    | Some v -> v

  let combine_aux f =
    let aux p1 p2 =
      match f (conv p1) (conv p2) with
      | NotPresent -> None (* Make sure not to store NotPresent *)
      | p -> Some p
    in
    M.merge (fun _ -> aux)


  let combine = combine_aux PresenceFlag.combine

  let empty = M.empty

  let is_empty = M.is_empty

  let only_present m =
    let aux id flag acc =
      if flag = Present then Key.Set.add id acc else acc
    in
    M.fold aux m KeySet.empty

  let all_present m =
    let aux id flag acc =
      match flag with
      | NotPresent -> (* Should not happen as NotPresent is not stored *) acc
      | MaybePresent | Present -> Key.Set.add id acc
    in
    M.fold aux m KeySet.empty
end


module ThreadPresence = MakePresence (Thread)
module MutexPresence = MakePresence (Mutex)
