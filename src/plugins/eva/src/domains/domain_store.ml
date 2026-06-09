(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

type control_point =
  | Initial
  | Start of Kernel_function.t
  | Before of Cil_datatype.Stmt.t
  | After of Cil_datatype.Stmt.t
[@@deriving eq, ord]

module ControlPointPrototype = struct
  include Datatype.Serializable_undefined
  type t = control_point [@@deriving eq, ord]
  let name = "Domain_store.ControlPoint"
  let reprs = [ Initial ]

  let hash = function
    | Initial -> Hashtbl.hash 0
    | Start kf -> Hashtbl.hash (1, Kernel_function.hash kf)
    | Before stmt -> Hashtbl.hash (2, Cil_datatype.Stmt.hash stmt)
    | After stmt -> Hashtbl.hash (3, Cil_datatype.Stmt.hash stmt)

  let pretty fmt = function
    | Initial -> Format.fprintf fmt "Initial"
    | Start kf -> Format.fprintf fmt "Start of %a" Kernel_function.pretty kf
    | Before stmt -> Format.fprintf fmt "Before %a" Printer.pp_stmt stmt
    | After stmt -> Format.fprintf fmt "After %a" Printer.pp_stmt stmt
end

module ControlPoint = Datatype.Make_with_collections (ControlPointPrototype)


module type InputDomain = sig
  include Datatype.S
  val name: string
  val top: t
end

module type S = sig
  type t
  val set_state: ?callstack:Callstack.t -> control_point -> t -> unit
  val get_state: ?callstack:Callstack.t -> control_point -> t or_top_bottom
  val callstacks: control_point -> Callstack.t list or_top
  val is_computed: unit -> bool
end

module Make (Domain: InputDomain) = struct

  let info name : (module State_builder.Info_with_size) =
    (module struct
      let name = Format.asprintf "Eva.Domain_store.Make(%s).%s" Domain.name name
      let size = 17
      let dependencies = [ Self.state ]
    end)

  (* Are states of this domain saved? *)
  module Save = State_builder.Option_ref (Datatype.Bool) (val info "Save")

  (* If the domain is unmarshable, its states cannot be saved on the disk,
     so this boolean should never be true when reloading a session. Set it to
     None to not prevent saving states in future analyses. *)
  let () =
    if Descr.is_unmarshable Domain.datatype_descr
    then Save.howto_marshal (fun _ -> ()) (fun () -> ref None)

  module Table =
    State_builder.Hashtbl (ControlPoint.Hashtbl) (Domain) (val info "Table")

  module States_by_callstack = Callstack.Hashtbl.Make (Domain)

  module TableByCallstack =
    State_builder.Hashtbl
      (ControlPoint.Hashtbl) (States_by_callstack) (val info "TableByCallstack")

  let save () =
    Parameters.(ResultsAll.get () && not (NoResultsDomain.mem Domain.name))

  let set_state ?callstack control_point state =
    if Save.memo save then
      match callstack with
      | None -> Table.replace control_point state
      | Some callstack ->
        let create _key = Callstack.Hashtbl.create 7 in
        let by_callstack = TableByCallstack.memo create control_point in
        Callstack.Hashtbl.replace by_callstack callstack state

  let is_computed () = Save.get_option () |> Option.value ~default:false

  let get_state ?callstack control_point =
    if is_computed ()
    then
      try
        match callstack with
        | None -> `Value (Table.find control_point)
        | Some callstack ->
          let cs_tbl = TableByCallstack.find control_point in
          `Value (Callstack.Hashtbl.find cs_tbl callstack)
      with Not_found -> `Bottom
    else `Top

  let get_callstacks key =
    TableByCallstack.find key |> Callstack.Hashtbl.to_seq_keys |> List.of_seq

  let callstacks control_point =
    if is_computed () then
      try `Value (get_callstacks control_point)
      with Not_found -> `Value []
    else `Top
end
