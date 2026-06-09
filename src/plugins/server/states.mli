(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Synchronized values between Server and Client *)

open Package

type 'a callback = ('a -> unit) -> unit

(** Connect a hook registry to a signal. As soon as the signal is being
    traced, a hook to emit the signal is registered. *)
val register_hook : Request.signal -> 'a callback -> unit

(** Register a (projectified) value and generates the associated signal and
    request:
    - Signal [<name>.sig] is emitted on value updates;
    - GET Request [<name>.get] returns the current value.

    If provided, the [~add_hook] option is used to register a hook to notify the
    server of value updates. The hook will be installed only once the client
    starts to listen for value updates.

    Inside {b Ivette} you can use the [States.useSyncValue(id)] hook to
    synchronize with this value.
*)
val register_value :
  package:package ->
  name:string ->
  descr:Markdown.text ->
  output:'a Request.output ->
  get:(unit -> 'a) ->
  ?add_hook:('b callback) ->
  unit -> Request.signal

(** Sub-signature of [State_builder.Ref] for [register_framac_value]. *)
module type Value = sig
  type data
  val get: unit -> data
  val add_hook_on_change: (data -> unit) -> unit
  module Datatype: Datatype.S
  val add_hook_on_update: (Datatype.t -> unit) -> unit
end

(** Same as [register_value] but takes a [State_builder.Ref] module as
    parameter. *)
val register_framac_value :
  package:package ->
  name:string ->
  descr:Markdown.text ->
  output:'a Request.output ->
  (module Value with type data = 'a) ->
  Request.signal

(** Register a (projectified) state and generates the associated signal and
    requests:
    - Signal [<name>.sig] is emitted on value updates;
    - GET Request [<name>.get] returns the current value;
    - SET Request [<name>.set] modifies the server value.

    If provided, the [~add_hook] option is used to register a hook to notify the
    server of value updates. The hook will be installed only once the client
    starts to listen for value updates.

    Inside {b Ivette} you can use the [States.useSyncState(id)] hook to
    synchronize with this state.
*)
val register_state :
  package:package ->
  name:string ->
  descr:Markdown.text ->
  data:'a Data.data ->
  get:(unit -> 'a) ->
  set:('a -> unit) ->
  ?add_hook:('b callback) ->
  unit -> Request.signal

(** Sub-signature of [State_builder.Ref] for [register_framac_state]. *)
module type State = sig
  type data
  val set: data -> unit
  val get: unit -> data
  val add_hook_on_change: (data -> unit) -> unit
  module Datatype: Datatype.S
  val add_hook_on_update: (Datatype.t -> unit) -> unit
end

(** Same as [register_state] but takes a [State_builder.Ref] module as
    parameter. *)
val register_framac_state :
  package:package ->
  name:string ->
  descr:Markdown.text ->
  data:'a Data.data ->
  (module State with type data = 'a) ->
  Request.signal

type 'a model (** Columns array model *)

(** Creates an empty array model. *)
val model : unit -> 'a model

(** Populate an array model with a new field.
    If a [~default] value is given, the field becomes optional and
    the field is omitted when equal to the default value (compared with [=]).
*)
val column :
  name:string ->
  descr:Markdown.text ->
  data:('b Request.output) ->
  get:('a -> 'b) ->
  ?default:'b ->
  'a model -> unit

(** Populate an array model with a new optional field. *)
val option :
  name:string ->
  descr:Markdown.text ->
  data:('b Request.output) ->
  get:('a -> 'b option) ->
  'a model -> unit

(** Synchronized array state. *)
type 'a array

(** Mark the array to be fully reloaded. *)
val reload : 'a array -> unit

(** Mark an array entry as updated. *)
val update : 'a array -> 'a -> unit

(** Mark an array entry as removed. *)
val remove : 'a array -> 'a -> unit

(** Get the signal associated with the array. *)
val signal : 'a array -> Request.signal

(** Register everything necessary to synchronize an array with
    the client:
    - Signal [<name>.sig] is emitted on array updates;
    - GET Request [<name>.fetch] is registered to get updates;
    - GET Request [<name>.reload] is registered to trigger a full reload.

    The [~key] parameter is used to identify array entries, and used to fill
    the reserved column ["id"] of entries.

    Columns added to the model after registration are {i not} taken into
    account.

    The optional [~preload] function will be called just before
    every column getters. Notice that column getters are called in
    registration order.

    If provided, the [~add_xxx_hook] options are used to register hooks
    to notify the server of corresponding array updates.
    Each hook will be installed only once the client starts to listen for array
    updates.

    Inside {b Ivette} you can obtain the entries in sync by using the
    [States.useSyncArray()] hook.
*)
val register_array :
  package:package ->
  name:string ->
  descr:Markdown.text ->
  key:('a -> string) ->
  ?keyName:string ->
  ?keyType:jtype ->
  iter:('a callback) ->
  ?preload:('a -> unit) ->
  ?add_update_hook:('a callback) ->
  ?add_remove_hook:('a callback) ->
  ?add_reload_hook:(unit callback) ->
  'a model -> 'a array

(** Sub-signature of [State_builder.Hashtbl] for [register_framac_array]. *)
module type TableState = sig
  type key
  type data
  val iter: (key -> data -> unit) -> unit
  val add_hook_on_change:
    ((key, data) State_builder.hashtbl_event -> unit) -> unit
  module Datatype: Datatype.S
  val add_hook_on_update: (Datatype.t -> unit) -> unit
end

(** Same as [register_array] but takes a [State_builder.Hashtbl] module as
    parameter. *)
val register_framac_array :
  package:package ->
  name:string ->
  descr:Markdown.text ->
  key:('k -> string) ->
  ?keyName:string ->
  ?keyType:jtype ->
  ('k * 'd) model ->
  (module TableState with type key = 'k and type data = 'd) ->
  ('k * 'd) array

(* -------------------------------------------------------------------------- *)
