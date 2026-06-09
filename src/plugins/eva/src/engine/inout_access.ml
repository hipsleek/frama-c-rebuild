(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Prototype = struct
  type t = {
    read : Memory_zone.t;
    write : Memory_zone.t;
  }
  [@@deriving eq,ord]
end
include Prototype

module Access = struct
  include Datatype.Make(struct
      include Datatype.Serializable_undefined
      include Prototype
      let name = "Eva.Inout_access.Access"
      let reprs =
        List.fold_left
          (fun acc read ->
             List.fold_left
               (fun acc write ->
                  { read ; write } :: acc)
               acc
               Memory_zone.reprs)
          []
          Memory_zone.reprs
      let pretty fmt access =
        Format.fprintf fmt "@[{ read: %a;@ write: %a; }@]"
          Memory_zone.pretty access.read
          Memory_zone.pretty access.write
    end)
  let bottom = { read = Memory_zone.bottom; write = Memory_zone.bottom }

  let is_bottom access =
    Memory_zone.is_bottom access.read &&
    Memory_zone.is_bottom access.write

  let is_included l r =
    Memory_zone.is_included l.read r.read
    && Memory_zone.is_included l.write r.write

  let join l r =
    { read = Memory_zone.join l.read r.read;
      write = Memory_zone.join l.write r.write }

  let make ?read ?write () =
    let default = Memory_zone.bottom in
    { read = Option.value ~default read;
      write = Option.value ~default write; }

  let add_read zone access =
    { access with read = Memory_zone.join access.read zone }

  let add_write zone access =
    { access with write = Memory_zone.join access.write zone }
end

module Cache : sig
  (** Get read/written memory zones for an analysis location. *)
  val get : Position.t -> t

  (** Change read/written memory zones for an analysis location. I.e. get the
      value, apply the given function then set the result. *)
  val change : Position.t -> (t -> t) -> unit

  (** Fold over all analysis locations and their read/written memory zones. *)
  val fold : (Position.t -> t -> 'acc -> 'acc) -> 'acc -> 'acc

  (** Dump the internal state regarding the read/written memory zones. Before
      dumping the memory zones are [filter]ed. *)
  val dump : filter:(t -> t) -> Format.formatter -> unit
end = struct
  (** State representing the read and written memory zones per analysis
      location. *)
  module State =
    State_builder.Hashtbl
      (Position.Hashtbl)
      (Access)
      (struct
        let name = "Eva.Inout_access.Cache.State"
        let size = 11
        let dependencies = [ Self.state ]
      end)

  let get (pos : Position.t) =
    try State.find pos
    with Not_found -> Access.bottom

  let change pos f =
    State.replace pos (f (get pos))

  let fold = State.fold

  let dump ~filter fmt =
    State.iter
      (fun pos access ->
         let access = filter access in
         if not @@ Access.is_bottom access then
           Format.fprintf fmt ">>> %a: %a"
             Position.pretty pos
             Access.pretty access)
end

let register_read pos zone =
  Cache.change pos (Access.add_read zone)

let register_write pos zone =
  Cache.change pos (Access.add_write zone)

let register pos access =
  Cache.change pos (Access.join access)

let mk_filter ~filter_base =
  let filter_zone = Memory_zone.filter_base filter_base in
  (fun access ->
     { read = filter_zone access.read;
       write = filter_zone access.write })
let keep_globals_only = mk_filter ~filter_base:Base.is_global

let at ?(filter=Fun.id) pos = Cache.get pos |> filter

let fold ?(filter=Fun.id) f init_acc =
  Cache.fold
    (fun pos access acc ->
       let access = filter access in
       if not (Access.is_bottom access) then
         f pos access acc
       else
         acc)
    init_acc

let iter ?(filter=Fun.id) f =
  fold ~filter
    (fun pos access () -> f pos access)
    ()

let dump ?(filter=Fun.id) fmt =
  Format.fprintf fmt "@.###### START OF DUMP OF VARIABLES #######@.";
  Cache.dump ~filter fmt;
  Format.fprintf fmt "@.####### END OF DUMP OF VARIABLES ########@.";
  ()
