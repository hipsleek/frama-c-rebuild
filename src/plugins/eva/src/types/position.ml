(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype

module type S = sig
  include Datatype.S_with_collections
  val loc : t -> Cil_types.location
  val pos : t -> Filepos.t
  val kinstr : t -> Cil_types.kinstr
  val pretty_loc : Format.formatter -> t -> unit
end

module Local =
struct
  module Prototype =
  struct
    include Datatype.Serializable_undefined

    type t = Stmt.t * Callstack.t [@@deriving eq, ord]

    let name = "Position.Local"
    let reprs =
      List.concat_map
        (fun stmt -> List.map (fun cs -> (stmt,cs)) Callstack.reprs)
        Stmt.reprs
    let hash (stmt, cs) =
      Hashtbl.hash (Stmt.hash stmt, Callstack.hash cs)
    let pretty fmt (stmt,cs) =
      Format.fprintf fmt "%a <-@ %a"
        Fileloc.pretty (Stmt.loc stmt)
        Callstack.pretty cs
  end

  include Datatype.Make_with_collections (Prototype)

  let loc (stmt, _cs) =
    Cil_datatype.Stmt.loc stmt

  let pos lpos =
    fst (loc lpos)

  let kinstr lpos =
    Cil_types.Kstmt (fst lpos)

  let pretty_loc fmt lpos =
    Fileloc.pretty fmt (loc lpos)

  let kf (_stmt, cs) =
    Callstack.top_kf cs

  let stmt (stmt, _cs) =
    stmt

  let callstack (_stmt, cs) =
    cs
end

type local = Local.t

(* Datatype for Position.t *)
module Prototype = struct
  include Datatype.Serializable_undefined

  type t =
    | RootCall of { thread: int; entry_point: Kernel_function.t }
    | GlobalInit of Varinfo.t
    | Local of Local.t
  [@@deriving eq, ord]

  let name = "Position"
  let reprs =
    List.map
      (fun kf -> RootCall { thread=0; entry_point=kf; })
      Kernel_function.reprs @
    List.map (fun vi -> GlobalInit vi) Varinfo.reprs @
    List.map (fun local -> Local local) Local.reprs
  let hash = function
    | RootCall { thread; entry_point } ->
      Hashtbl.hash (1, thread, Kernel_function.hash entry_point)
    | GlobalInit vi -> Hashtbl.hash (2, Varinfo.hash vi)
    | Local l -> Hashtbl.hash (3, Local.hash l)
  let pretty fmt = function
    | RootCall { entry_point; _ } ->
      Format.pp_print_string fmt (Kernel_function.get_name entry_point)
    | GlobalInit vi -> Format.pp_print_string fmt vi.vname
    | Local l -> Local.pretty fmt l
end
include Datatype.Make_with_collections (Prototype)
include Prototype

let local stmt callstack =
  Local (stmt, callstack)

let root_call ~thread ~entry_point =
  RootCall { thread; entry_point }

let global_init vi =
  GlobalInit vi

let is_local = function
  | RootCall _ | GlobalInit _ -> false
  | Local _ -> true

let loc pos =
  match pos with
  | RootCall { entry_point: Kernel_function.t; _ } ->
    Kernel_function.get_location entry_point
  | GlobalInit vi -> vi.vdecl
  | Local l -> Local.loc l

let pos pos =
  loc pos |> fst

let kinstr pos =
  match pos with
  | RootCall _ | GlobalInit _ -> Cil_types.Kglobal
  | Local l -> Local.kinstr l

let stmt pos =
  match pos with
  | RootCall _ | GlobalInit _ -> None
  | Local (stmt,_cs) -> Some stmt

let kf pos =
  match pos with
  | RootCall { entry_point } -> Some entry_point
  | GlobalInit _ -> None
  | Local lpos -> Some (Local.kf lpos)

let callstack pos =
  match pos with
  | RootCall { thread; entry_point } ->
    Some (Callstack.init ~thread ~entry_point)
  | GlobalInit _vi -> None
  | Local lpos -> Some (Local.callstack lpos)

let pretty_loc fmt pos =
  Fileloc.pretty fmt (loc pos)

let of_kinstr kinstr callstack =
  match kinstr with
  | Cil_types.Kstmt stmt ->
    Local (stmt, callstack)
  | Kglobal ->
    match Callstack.pop_call callstack with
    | entry_point, None ->
      RootCall { thread=callstack.thread; entry_point }
    | _kf, Some (stmt, callstack) ->
      Local (stmt, callstack)

let of_local lpos =
  Local lpos

let set_stmt stmt pos =
  let is_in_kf stmt kf =
    Kernel_function.equal
      (Kernel_function.find_englobing_kf stmt)
      kf
  in
  let open Option.Operators in
  let* kf = kf pos
  and* cs = callstack pos in
  if is_in_kf stmt kf then
    Some (local stmt cs)
  else
    None

let push_kf kf pos =
  match pos with
  | GlobalInit _ | RootCall _ ->
    None
  | Local (stmt, cs) ->
    try
      let cs = Callstack.push kf stmt cs in
      let stmt = Kernel_function.find_first_stmt kf in
      Some (local stmt cs)
    with Kernel_function.No_Statement ->
      None

