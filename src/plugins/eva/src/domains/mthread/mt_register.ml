(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_utils

type value = Cvalue.V.t
type 'v result = 'v Mt_utils.Result.t

type errors =
  | AlreadyRegistered
  | NotRegistered
  | MayBeInState of (string * bool)

type update_check = Ok | Invalid of (string * bool)


module type Key_sig = sig
  include Hptmap.Id_Datatype
  val key_name : string
  val of_value : value -> t list result
  val to_value : t -> value
end


module type Status_sig = sig
  include Lattice_type.Join_Semi_Lattice
  val default : t
end


module Make (Key : Key_sig) (Status : Status_sig) = struct
  module Info = struct
    let initial_values = [ ]
    let dependencies = [ Ast.self ]
  end
  include Hptmap.Make (Key) (Status) (Info)
  let cache_name s = Hptmap_sig.PersistentCache (datatype_name ^ "." ^ s)
  let find key map = try Some (find key map) with Not_found -> None

  let warning key register = function
    | AlreadyRegistered ->
      Result.warning (register, Key.to_value key)
        "The %s %a is already registered."
        Key.key_name Key.pretty key
    | NotRegistered ->
      (* Temporary: do not emit warning when a key has not been registered.
         As Mthread does not inject the initial domain state at the start
         of a thread analysis, this is bound to happen (for now). *)
      if true
      then Result.ok (register, Value.of_int 1)
      else
        Result.warning (register, Value.of_int 1)
          "The %s %a is not registered."
          Key.key_name Key.pretty key
    | MayBeInState (state, sure) ->
      Result.warning (register, Value.of_int 2)
        "The %s %a %s already %s."
        Key.key_name Key.pretty key
        (if sure then "is" else "may be") state

  let fold_keys f keys register =
    let f' acc key =
      let open Result.Operators in
      let* (register, result) = acc in
      let+ register, result' = f key register in
      register, Value.join result' result
    in
    List.fold_left f' (Result.ok (register, Value.bottom)) keys

  let register keys register =
    let register_one key register =
      if not (mem key register)
      then Result.ok (add key Status.default register, Key.to_value key)
      else warning key register AlreadyRegistered
    in
    fold_keys register_one keys register

  let update new_status check keys_value register =
    let open Result.Operators in
    let update_one key register =
      match find key register with
      | None ->
        let+ (register, result) = warning key register NotRegistered in
        let register = add key (new_status Status.default) register in
        register, result
      | Some status ->
        let register = add key (new_status status) register in
        match check status with
        | Ok -> Result.ok (register, Value.zero)
        | Invalid reason -> warning key register (MayBeInState reason)
    in
    let* keys = Key.of_value keys_value in
    fold_keys update_one keys register

  (* If a key is not in the register, we consider that it may be unregistered
     from the point of view of the partial order. It means that the empty map is
     the top element. *)
  let top = empty

  let is_included =
    let cache = cache_name "is_included" in
    let decide_fst _b _l = true  (* r is top *) in
    let decide_snd _b _r = false (* l is top *) in
    let decide_both _ l r = Status.is_included l r in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  (* Over-approximation of the narrow of two registers. Keys registered on
     each sides are all kept. However, we are conservative on their status. *)
  let narrow =
    let cache = cache_name "narrow" in
    let decide _ x y = Status.join x y in
    join ~cache ~symmetric:true ~idempotent:true ~decide

  (* Join of two registers. It only keeps keys registered on both sides and
     their statuses are joined. *)
  let join =
    let cache = cache_name "join" in
    let decide _ x y = Some (Status.join x y) in
    inter ~cache ~symmetric:true ~idempotent:true ~decide
end


(* ----- Threads ------------------------------------------------------------ *)

module ThreadKey = struct
  include Thread
  let key_name = "thread"
  let of_value x =
    let open Result.Operators in
    let* l = Value.to_int_list x in
    let convert_one acc id =
      let* acc = acc in
      match find id with
      | None -> Result.error "Not a valid thread id '%d'." id
      | Some th -> Result.ok (th :: acc)
    in
    List.fold_left convert_one (Result.ok []) l
  let to_value th = Value.of_int (id th)
end

type thread_status =
  { running : Mt_utils.trilean ; canceled : Mt_utils.trilean }

module ThreadStatus = struct
  include Datatype.Make (struct
      type t = thread_status
      let name = "Mthread.thread.status"
      let reprs = [ { running = False ; canceled = False } ]
      let copy = Datatype.identity
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project

      let structural_descr =
        let running = Datatype.Bool.packed_descr in
        let canceled = Trilean.packed_descr in
        Structural_descr.t_record [| running ; canceled |]

      let pretty fmt { running ; canceled } =
        Format.fprintf fmt "Running : %a@.Canceled : %a@."
          Trilean.pretty running Trilean.pretty canceled

      let compare l r =
        Trilean.compare l.running r.running
        <?> lazy (Trilean.compare l.canceled r.canceled)

      let equal l r = compare l r = 0
      let hash t = Trilean.hash t.running + 3 * Trilean.hash t.canceled
    end)

  (* let top = { running = Unknown ; canceled = Unknown } *)

  let is_included l r =
    Trilean.is_included l.running r.running
    && Trilean.is_included l.canceled r.canceled

  let join l r =
    let running = Trilean.join l.running r.running in
    let canceled = Trilean.join l.canceled r.canceled in
    { running ; canceled }

  let default = { running = False ; canceled = False }
end

module Thread = struct
  include Make (ThreadKey) (ThreadStatus)

  let change_running running msg =
    let new_status status = { status with running } in
    update new_status @@ fun { running=previous } ->
    if Trilean.intersects running previous
    then Invalid (msg, Trilean.equal running previous)
    else Ok

  let start = change_running True "running"
  let suspend = change_running False "suspended"
  let cancel = update (fun s -> { s with canceled = True }) (fun _ -> Ok)
end


(* ----- Mutex -------------------------------------------------------------- *)

module MutexKey = struct
  include Mutex
  let key_name = "mutex"
  let of_value x =
    let open Result.Operators in
    let* l = Value.to_int_list x in
    let convert_one acc id =
      let* acc = acc in
      match find id with
      | None -> Result.error "Not a valid mutex id '%d'." id
      | Some th -> Result.ok (th :: acc)
    in
    List.fold_left convert_one (Result.ok []) l
  let to_value th = Value.of_int (id th)
end

type mutex_status = Locked | Unlocked

module MutexStatus = struct
  include Datatype.Make (struct
      include Datatype.Serializable_undefined
      type t = mutex_status
      let name = "Mthread.mutex.status"
      let reprs = [ Locked ; Unlocked ]
      let hash = function Locked -> 0 | Unlocked -> 1
      let compare x y = Datatype.Int.compare (hash x) (hash y)
      let equal x y = compare x y = 0
      let to_string = function Locked -> "locked" | Unlocked -> "unlocked"
      let pretty fmt status = Format.fprintf fmt "%s" (to_string status)
    end)

  (* There is a total order on statuses, that can be used as a partial order as
     it encodes the idea that we want to keep mutexes unlocked if we are not
     sure of their status. *)
  let is_included x y = compare x y <= 0
  let join x y = if compare x y <= 0 then y else x
  let default = Unlocked
end

(* A register of all the program's mutexes and their current status. A mutex is
   registered as locked if and only if we are absolutely sure that it is locked.
   It is indeed necessary to ensure soundness, as it will trigger more
   interferences as necessary. *)
module Mutex = struct
  include Make (MutexKey) (MutexStatus)
  let check bad msg st =
    if MutexStatus.equal bad st then Invalid (msg, true) else Ok
  let lock = update (fun _ -> Locked) (check Locked "locked")
  let unlock = update (fun _ -> Unlocked) (check Unlocked "unlocked")

  let locked_mutexes register =
    let add mutex status acc =
      match status with
      | Locked -> Mutex.Set.add mutex acc
      | Unlocked -> acc
    in
    fold add register Mutex.Set.empty
end
