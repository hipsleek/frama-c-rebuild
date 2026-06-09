(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let get ~plugin name typ ~fallback =
  try Dynamic.get ~plugin name typ
  with Failure _ | Dynamic.(Unbound_value _ | Incompatible_type _) -> fallback

module Inout = struct
  let plugin = "Inout"

  let register_call_hook f =
    let fallback _ = () in
    let typ = Datatype.(func (func Inout_type.ty unit) unit) in
    get ~plugin "register_call_hook" typ ~fallback f

  let kf_outputs kf =
    let fallback _ = Memory_zone.top in
    let typ arg = Datatype.func arg Memory_zone.ty in
    get ~plugin "kf_outputs" (typ Kernel_function.ty) ~fallback kf
end

module Callgraph = struct
  let plugin = "Callgraph"

  let iter_in_rev_order f =
    let fallback = Globals.Functions.iter in
    (* callgraph is too slow on programs with too many callsites. *)
    if Function_calls.nb_callsites () > 20000
    then fallback f
    else
      let typ = Datatype.(func (func Kernel_function.ty unit) unit) in
      get ~plugin "iter_in_rev_order" typ ~fallback f
end

module Scope = struct
  let plugin = "Scope"

  let rm_asserts () =
    let fallback () =
      Self.warning
        "The scope plugin is missing: cannot remove redundant alarms."
    in
    let typ = Datatype.(func unit unit) in
    get ~plugin "rm_asserts" typ ~fallback ()
end
