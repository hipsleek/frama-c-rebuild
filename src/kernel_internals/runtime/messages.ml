(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module DatatypeMessages =
  Datatype.Make_with_collections
    (struct
      include Datatype.Serializable_undefined
      open Log
      type t = event
      let name = "message"
      let reprs =
        [ { evt_kind = Failure;
            evt_plugin = "";
            evt_category = None;
            evt_source = None;
            evt_message = Rich_text.empty } ]
      let mem_project = Datatype.never_any_project
      let hash (e: event)= Hashtbl.hash e
      let compare (e1: event) e2 = Extlib.compare_basic e1 e2
      let equal = Datatype.from_compare
    end)

module Messages =
  State_builder.Queue
    (DatatypeMessages)
    (struct
      let name = "Messages.message_table"
      let dependencies = [ Ast.self ]
    end)
let () = Ast.add_monotonic_state Messages.self

let hooks = ref []

let add_message m =
  let i = Messages.length () in
  Messages.add m;
  List.iter (fun fn -> fn (m, i)) !hooks

let nb_errors () =
  Messages.fold
    (fun n e ->
       match e.Log.evt_kind with
       | Log.Error -> succ n
       | _ -> n) 0

let nb_warnings () =
  Messages.fold
    (fun n e ->
       match e.Log.evt_kind with
       | Log.Warning -> succ n
       | _ -> n) 0

let nb_messages = Messages.length

let self = Messages.self

let iter = Messages.iter
let fold = Messages.fold
let dump_messages () = iter Log.echo

let () = Log.add_listener add_message

module OnceTable =
  State_builder.Hashtbl
    (DatatypeMessages.Hashtbl)
    (Datatype.Unit)
    (struct
      let size = 37
      let dependencies = [ Ast.self ]
      let name = "Messages.OnceTable"
    end)

let check_not_yet evt =
  if OnceTable.mem evt then false
  else begin
    OnceTable.add evt ();
    true
  end

let () = Log.check_not_yet := check_not_yet

let reset_once_flag () = OnceTable.clear ()

let add_hook fn = hooks := !hooks @ [fn]
