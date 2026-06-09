(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Table = Datatype.String.Hashtbl

type t =
  {
    globals: (varinfo * init_or_str option) Table.t;
    functions: varinfo Table.t;
    typedefs: typeinfo Table.t;
    structs: compinfo Table.t;
    unions: compinfo Table.t;
    enums: enuminfo Table.t;
  }

let empty () : t =
  {
    globals = Table.create 17;
    functions = Table.create 17;
    typedefs = Table.create 17;
    structs = Table.create 17;
    unions = Table.create 17;
    enums = Table.create 17;
  }

let add_global (env : t) (vi : varinfo) (init: init_or_str option) : unit  =
  let v = Table.find_opt env.globals vi.vname in
  let init = match v with
    | None -> init
    | Some v -> snd v
  in
  Table.replace env.globals vi.vname (vi,init)

let add_function (env : t) (vi : varinfo) : unit  =
  Table.add env.functions vi.vname vi

let add_typeinfo (env : t) (typeinfo : typeinfo) : unit =
  Table.add env.typedefs typeinfo.torig_name typeinfo

let add_compinfo (env : t) (compinfo : compinfo) : unit  =
  let table = if compinfo.cstruct then env.structs else env.unions in
  Table.add table compinfo.corig_name compinfo

let add_enuminfo (env : t) (enuminfo : enuminfo) : unit  =
  Table.add env.enums enuminfo.eorig_name enuminfo

let find_global (env : t) (vname : string) : varinfo  =
  fst (Table.find env.globals vname)

let find_global_init env vname =
  snd (Table.find env.globals vname)

let find_function (env : t) (vname : string) : varinfo =
  Table.find env.functions vname

let find_typedef (env : t) (tname : string) : typeinfo=
  Table.find env.typedefs tname

let find_struct (env : t) (tname : string) : compinfo =
  Table.find env.structs tname

let find_union (env : t) (tname : string) : compinfo =
  Table.find env.unions tname

let find_enum (env : t) (tname : string) : enuminfo =
  Table.find env.enums tname

let find_type (env : t) (namespace : Logic_typing.type_namespace)
    (tname : string) : typ =
  match namespace with
  | Logic_typing.Typedef ->
    Cil_const.mk_tnamed (find_typedef env tname)
  | Logic_typing.Struct ->
    Cil_const.mk_tcomp  (find_struct env tname)
  | Logic_typing.Union ->
    Cil_const.mk_tcomp  (find_union env tname)
  | Logic_typing.Enum ->
    Cil_const.mk_tenum  (find_enum env tname)

let mem_global (env : t) (vname : string) : bool =
  Table.mem env.globals vname

let mem_function (env : t) (vname : string) : bool =
  Table.mem env.functions vname

let from_file (file : file) : t =
  let env = empty () in
  let v = object inherit Cil.nopCilVisitor
    method! vglob glob =
      begin match glob with
        | GFunDecl(_,vi,_) | GFun ({svar = vi}, _) ->
          add_function env vi
        | GVarDecl (vi,_) -> add_global env vi None
        | GVar (vi, init, _) ->
          add_global env vi init.init
        | GType (typeinfo,_) ->
          add_typeinfo env typeinfo
        | GCompTag (compinfo,_) ->
          add_compinfo env compinfo
        | GEnumTag (enuminfo,_) ->
          add_enuminfo env enuminfo
        | _ -> ()
      end;
      Cil.SkipChildren
  end in
  Cil.visitCilFile v file;
  env
