(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* --- Mutex definition --- *)

module Prototype =
struct
  include Datatype.Serializable_undefined

  type t = {
    id: int;
    name: Concurrency.Name.t option
  }

  let name = "Eva.Mutex"
  let reprs = [{ id = 0; name = None }]
  let equal m1 m2 = Int.equal m1.id m2.id
  let compare m1 m2 = Int.compare m1.id m2.id
  let hash m = m.id

  let pretty fmt m =
    match m.name with
    | Some name ->
      Concurrency.Name.pretty fmt name
    | None ->
      Format.fprintf fmt "#%i" m.id
end

module Mutex = Datatype.Make_with_collections (Prototype)
include Prototype
include Mutex

let id m = m.id
let label m = Pretty_utils.to_string pretty m


(* --- Mutex registering --- *)

module MutexesById = State_builder.Hashtbl (Datatype.Int.Hashtbl) (Mutex)
    (struct
      let name = "Eva.Mutex.MutexesById"
      let dependencies = []
      let size = 13
    end)

let last_mutex_id = ref 0

let create name =
  incr last_mutex_id;
  let m = { id = !last_mutex_id; name } in
  MutexesById.add m.id m;
  m

let find id =
  MutexesById.find_opt id


(* --- Mutex identity --- *)

(* The identity of a mutex is used to choose how to group mutex creations
   during the analysis. This is especially useful if the mutex is created inside
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

    let name = "Eva.Mutex.Identity"

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

module Identities = State_builder.Hashtbl (Identity.Hashtbl) (Mutex)
    (struct
      let name = "Eva.Mutex.Identities"
      let dependencies = []
      let size = 13
    end)


(* Mutexes state *)

let create creation_point name =
  let identity = match name with
    | Some name -> Identity.Prototype.ByName name
    | None -> ByCreationPoint creation_point
  in
  Identities.memo (fun _ -> create name) identity

let reset_state () =
  last_mutex_id := 0;
  MutexesById.clear ();
  Identities.clear ()
