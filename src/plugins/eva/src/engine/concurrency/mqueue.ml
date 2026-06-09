(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* --- Mqueues definition --- *)

module Prototype =
struct
  include Datatype.Serializable_undefined

  type t = {
    id: int;
    name: Concurrency.Name.t option
  }

  let name = "Eva.Queue"
  let reprs = [{ id = 0; name = None }]
  let equal q1 q2 = Int.equal q1.id q2.id
  let compare q1 q2 = Int.compare q1.id q2.id
  let hash q = q.id

  let pretty fmt q =
    match q.name with
    | Some name ->
      Concurrency.Name.pretty fmt name
    | None ->
      Format.fprintf fmt "#%i" q.id
end

module Queue = Datatype.Make_with_collections (Prototype)
include Prototype
include Queue

let id q = q.id
let label q = Pretty_utils.to_string pretty q


(* --- Queue registering --- *)

module MqueuesById = State_builder.Hashtbl (Datatype.Int.Hashtbl) (Queue)
    (struct
      let name = "Eva.Queue.MqueuesById"
      let dependencies = []
      let size = 13
    end)

let last_queue_id = ref 0

let create name =
  incr last_queue_id;
  let q = { id = !last_queue_id; name } in
  MqueuesById.add q.id q;
  q

let find id =
  MqueuesById.find_opt id


(* --- Queue identity --- *)

(* The identity of a queue is used to choose how to group mqueues creations
   during the analysis. This is especially useful if the queue is created inside
   a - potentially not unrolled - loop. *)

module Identity =
struct
  module Prototype =
  struct
    include Datatype.Serializable_undefined

    type t =
      | ByName of Concurrency.Name.t
      | ByCreationPoint of Position.Local.t
    [@@deriving eq, ord]

    let name = "Eva.Queue.Identity"

    let reprs =
      List.map (fun n -> ByName n) Concurrency.Name.reprs @
      List.map (fun al -> ByCreationPoint al) Position.Local.reprs

    let hash = function
      | ByName name ->
        Stdlib.Hashtbl.hash(1, Concurrency.Name.hash name)
      | ByCreationPoint al ->
        Stdlib.Hashtbl.hash(2, Position.Local.hash al)
  end

  include Prototype
  include Datatype.Make_with_collections (Prototype)
end

module Identities = State_builder.Hashtbl (Identity.Hashtbl) (Queue)
    (struct
      let name = "Eva.Queue.Identities"
      let dependencies = []
      let size = 13
    end)


(* Mqueues state *)

let create creation_point name =
  let identity = match name with
    | Some name -> Identity.Prototype.ByName name
    | None -> ByCreationPoint creation_point
  in
  Identities.memo (fun _ -> create name) identity

let reset_state () =
  last_queue_id := 0;
  MqueuesById.clear ();
  Identities.clear ()
