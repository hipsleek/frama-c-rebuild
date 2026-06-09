(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Name = struct

  module Prototype =
  struct
    open Cil_datatype

    include Datatype.Serializable_undefined

    type t =
      | String of string
      | Integer of Z.t
      | Pointer of Varinfo.t * OffsetStructEq.t
      | RawPointer of Varinfo.t * Z.t (* Offset in bits *)
    [@@deriving eq, ord]

    let name = "Eva.Concurrency.Name"
    let reprs = [Integer Z.zero]

    let pretty fmt = function
      | String s -> Unicode.pp_string fmt s
      | Integer i -> Z.pretty fmt i
      | Pointer (v, o) ->
        let last_offset = Cil.lastOffset o in
        let o =
          match last_offset with
          | Field ({ forig_name ; fattr; _ }, NoOffset)
            when forig_name = "_fc"
              && Ast_attributes.(contains fc_stdlib_internal fattr) ->
            (* The pthreads library in Frama-C's stdlib models pthreads types
               with a struct with a single field `_fc`. Mthread uses that field
               to identify threads and mutexes so it ends up here as concurrency
               name. We can omit that field when printing the name. *)
            fst (Cil.removeOffset o)
          | _ ->
            o
        in
        Format.fprintf fmt "%a%a" Varinfo.pretty v OffsetStructEq.pretty o
      | RawPointer (v, o) ->
        Format.fprintf fmt "&%a + %a" Varinfo.pretty v Z.pretty o

    let hash = function
      | String s -> Hashtbl.hash (1, s)
      | Integer i -> Hashtbl.hash (2, Z.hash i)
      | Pointer (v, o) -> Hashtbl.hash (3, Varinfo.hash v, OffsetStructEq.hash o)
      | RawPointer (v, o) -> Hashtbl.hash (4, Varinfo.hash v, Z.hash o)
  end

  include Prototype
  include Datatype.Make_with_collections (Prototype)

  let of_string s = String s

  let to_string n =
    Pretty_utils.to_string pretty n

  let of_address base i =
    match base with
    | Base.Null ->
      Some (Integer i)
    | Base.Var (vi, _) when Ast_info.is_string_literal vi ->
      begin
        match Globals.Vars.get_string_literal vi with
        | Str s -> Some (String s)
        | Wstr s -> Some (String (Escape.escape_wstring s))
      end
    | Base.Var (vi, _) | Base.Allocated (vi, _, _) ->
      begin try
          let offset, _typ =
            Bit_utils.find_offset vi.vtype ~offset:i Bit_utils.MatchLast
          in
          Some (Pointer (vi, offset))
        with Bit_utils.NoMatchingOffset ->
          Some (RawPointer (vi, i))
      end
    | _ -> None

  let of_cvalue cvalue =
    try
      let base, ival = Addresses.Bytes.find_lonely_binding cvalue in
      let byte_offset = Ival.project_int ival in
      let bits_offset = Z.(mul byte_offset (of_int 8)) in
      of_address base bits_offset
    with Not_found | Ival.Not_Singleton_Int -> None
end
