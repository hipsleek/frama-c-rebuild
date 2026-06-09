(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype
open Server
module Md = Markdown

let package = Package.package ~plugin:"region" ~title:"Region Analysis" ()

(* -------------------------------------------------------------------------- *)
(* --- Server Data                                                        --- *)
(* -------------------------------------------------------------------------- *)

module Node : Data.S with type t = Memory.node =
struct
  type t = Memory.node
  let jtype = Data.declare ~package ~name:"node" (Jindex "node")
  let to_json n = Json.of_int @@ Memory.id n
  let of_json _js = failwith "The of_json function should never be called."
end

module NodeOpt = Data.Joption(Node)
module NodeList = Data.Jlist(Node)

module Cvar : Data.S with type t = Memory.cvar =
struct
  type t = Memory.cvar
  let jtype = Data.declare ~package ~name:"cvar" @@
    Jrecord [
      "name", Jstring ;
      "label", Jstring ;
      "title", Jstring ;
      "cells", Jnumber ;
    ]

  let title (Memory.Cvar r) =
    Format.asprintf "%a (%db)%t"
      Typ.pretty r.cvar.vtype
      (Fields.bitsSizeOf r.cvar.vtype)
      (fun fmt ->
         if r.cells > 1 then Format.fprintf fmt " (%d cells)" r.cells)

  let to_json (Memory.Cvar r as cvar) =
    Json.of_fields [
      "name", Json.of_string r.cvar.vname ;
      "label", Json.of_string r.label ;
      "title", Json.of_string (title cvar) ;
      "cells", Json.of_int r.cells ;
    ]
  let of_json _ = failwith "Region.Cvar.of_json"
end

module Root : Data.S with type t = Memory.root =
struct
  type t = Memory.root
  let jtype = Data.declare ~package ~name:"root" @@
    Jrecord [
      "range", Jstring ;
      "typeof", Jstring ;
      "attrs", Jarray Jstring ;
      "marker", Kernel_ast.Marker.jtype ;
    ]

  let typeof (Memory.Root r) =
    Format.asprintf "%a[..]" Printer.pp_typ r.typ

  let range (Memory.Root r) =
    Format.asprintf "%a%a[%a..%a]"
      Spec.pp_named r.named
      Printer.pp_term r.ptr
      Printer.pp_term r.inf
      Printer.pp_term r.sup

  let attributes (Memory.Root r) : Json.t =
    let pool : Json.t list ref = ref [] in
    Attr.iter
      (fun a ->
         pool := `String (Format.asprintf "%a" Attr.pp_attr a) :: !pool)
      r.flags ;
    `List (List.rev !pool)

  let to_json (Memory.Root r as root) =
    Json.of_fields [
      "range", Json.of_string (range root) ;
      "typeof", Json.of_string (typeof root) ;
      "attrs", attributes root ;
      "marker", Kernel_ast.Marker.to_json (PIP r.ip) ;
    ]
  let of_json _ = failwith "Region.Cvar.of_json"
end

module Range : Data.S with type t = Memory.range =
struct
  type t = Memory.range
  let jtype = Data.declare ~package ~name:"range" @@
    Jrecord [
      "label", Jstring ;
      "offset", Jnumber ;
      "length", Jnumber ;
      "cells", Jnumber ;
      "data", Node.jtype ;
    ]

  let to_json (Memory.Range rg) =
    Json.of_fields [
      "label", Json.of_string rg.label ;
      "offset", Json.of_int rg.offset ;
      "length", Json.of_int rg.length ;
      "cells", Json.of_int rg.cells ;
      "data", Node.to_json rg.data ;
    ]
  let of_json _ = failwith "Region.Range.of_json"
end

module ACCESS: Data.S with type t = Access.acs =
struct
  type t = Access.acs
  let jtype = Data.declare ~package ~name:"access" @@
    Jrecord [
      "rank", Jnumber ;
      "access", Jstring ;
      "source", Jstring ;
      "typeof", Jstring ;
      "marker", Kernel_ast.Marker.jtype ;
    ]

  let to_json acs =
    let to_string pp = Format.asprintf "%a" pp in
    `Assoc [
      "rank", `Int (Access.rank acs) ;
      "access", `String (to_string Access.pp_access acs) ;
      "source", `String (to_string Access.pp_source acs) ;
      "typeof", `String (to_string Printer.pp_typ @@ Access.typeof acs) ;
      "marker", Kernel_ast.Marker.to_json @@ Access.marker acs ;
    ]
  let of_json _ = failwith "Region.Access.of_json"
end

module Cvars = Data.Jlist(Cvar)
module Roots = Data.Jlist(Root)
module Ranges = Data.Jlist(Range)
module ACS = Data.Jlist(ACCESS)

module Region: Data.S with type t = Memory.region =
struct
  type t = Memory.region

  let labels_to_json ls =
    Json.of_list @@ List.map Json.of_string ls

  let ikind_to_char (ikind : Cil_types.ikind) =
    match ikind with
    | IBool | IUChar -> 'b'
    | IChar | ISChar -> 'c'
    | IInt -> 'i' | IUInt -> 'u'
    | IShort | IUShort -> 's'
    | ILong | ILongLong -> 'l'
    | IULong | IULongLong -> 'w'
    | IInt128 | IUInt128 -> 'q'

  let fkind_to_char (fkind : Cil_types.fkind) =
    match fkind with
    | FFloat  | FFloat32 -> 'f'
    | FDouble | FFloat64 | FLongDouble -> 'd'

  let typ_to_char (ty: Cil_types.typ) =
    match ty.tnode with
    | TPtr _ -> 'p'
    | TInt ik -> ikind_to_char ik
    | TFloat fk -> fkind_to_char fk
    | TComp { cstruct } -> if cstruct then 'S' else 'U'
    | TArray _ -> 'A'
    | TNamed _ -> 'T'
    | TEnum _ -> 'E'
    | TFun _ -> 'F'
    | TVoid | TBuiltin_va_list -> 'x'

  let typs_to_char (typs : Cil_types.typ list) =
    match typs with
    | [] -> '-'
    | [ty] -> typ_to_char ty
    | _ -> 'x'

  let label (m: Memory.region) =
    let buffer = Buffer.create 4 in
    (* if m.singleton then Buffer.add_string buffer "!" ; *)
    if m.inits <> [] then Buffer.add_char buffer 'I' ;
    if m.reads <> [] then Buffer.add_char buffer 'R' ;
    if m.writes <> [] then Buffer.add_char buffer 'W' ;
    if m.pointed <> None then Buffer.add_string buffer "(*)"
    else if m.inits <> [] || m.reads <> [] || m.writes <> [] then
      begin
        Buffer.add_char buffer '(' ;
        Buffer.add_char buffer @@ typs_to_char m.types ;
        Buffer.add_char buffer ')' ;
      end ;
    if Buffer.length buffer > 0 then Buffer.contents buffer else "…"

  let pp_typ_layout s0 fmt ty =
    let s = Fields.bitsSizeOf ty in
    if s <> s0 then
      Format.fprintf fmt "(%a)%%%db" Typ.pretty ty s
    else
      Typ.pretty fmt ty

  let title (m: Memory.region) =
    Format.asprintf "%t (%db)%t"
      begin fun fmt ->
        match m.types with
        | [] -> Format.pp_print_string fmt "(no access)"
        | [ty] -> pp_typ_layout m.sizeof fmt ty ;
        | ty::ts ->
          pp_typ_layout 0 fmt ty ;
          List.iter (Format.fprintf fmt ", %a" (pp_typ_layout 0)) ts ;
      end
      m.sizeof
      begin fun fmt ->
        if m.types <> [] && m.singleton then Format.pp_print_string fmt " (singleton)" ;
        Attr.iter (Format.fprintf fmt " (%a)" Attr.pp_attr) m.flags ;
      end

  let labels (r: Memory.region) =
    List.filter
      (fun l ->
         List.for_all
           (function Memory.Root r -> r.named <> l)
           r.roots
      ) r.labels

  let jtype = Data.declare ~package ~name:"region" @@
    Jrecord [
      "node", Node.jtype ;
      "result", Jboolean ;
      "cvars", Cvars.jtype ;
      "roots", Roots.jtype ;
      "labels", Jarray Jalpha ;
      "parents", NodeList.jtype ;
      "sizeof", Jnumber ;
      "ranges", Ranges.jtype ;
      "pointed", NodeOpt.jtype ;
      "reads", ACS.jtype ;
      "writes", ACS.jtype ;
      "inits", ACS.jtype ;
      "typed", Jboolean ;
      "singleton", Jboolean ;
      "label", Jstring ;
      "title", Jstring ;
    ]

  let to_json (m: Memory.region) =
    Json.of_fields [
      "node", Node.to_json m.node ;
      "result", Json.of_bool m.cresult ;
      "cvars", Cvars.to_json m.cvars ;
      "roots", Roots.to_json m.roots ;
      "labels", labels_to_json @@ labels m ;
      "parents", NodeList.to_json m.parents ;
      "sizeof", Json.of_int @@ m.sizeof ;
      "ranges", Ranges.to_json @@ m.ranges ;
      "pointed", NodeOpt.to_json @@ m.pointed ;
      "reads", ACS.to_json m.reads ;
      "writes", ACS.to_json m.writes ;
      "inits", ACS.to_json m.inits ;
      "typed", Json.of_bool (m.typed <> None) ;
      "singleton", Json.of_bool m.singleton ;
      "label", Json.of_string @@ label m ;
      "title", Json.of_string @@ title m ;
    ]

  let of_json _ = failwith "Region.Layout.of_json"
end

module Regions = Data.Jlist(Region)

(* -------------------------------------------------------------------------- *)
(* --- Server API                                                         --- *)
(* -------------------------------------------------------------------------- *)

let map_of_localizable (loc : Printer_tag.localizable) =
  let open Printer_tag in
  match kf_of_localizable loc with
  | None -> raise Not_found
  | Some kf -> Analysis.find kf

let region_of_localizable (m: Memory.map) (loc: Printer_tag.localizable) =
  try
    match loc with
    | PExp(_,_,e) -> Memory.exp m e
    | PLval(_,_,lv) -> Some (Memory.lval m lv)
    | PVDecl(_,_,x) -> Some (Memory.lval m (Var x,NoOffset))
    | PStmt _ | PStmtStart _
    | PTermLval _ | PGlobal _ | PIP _ | PType _ -> None
  with Not_found -> None

let map_of_declaration (decl : Printer_tag.declaration) =
  match decl with
  | SFunction kf -> Analysis.find kf
  | _ -> raise Not_found

let signal = Request.signal ~package ~name:"updated"
    ~descr:(Md.plain "Region Analysis Updated")

let () = Analysis.add_hook (fun () -> Request.emit signal)

let () =
  Request.register
    ~package ~kind:`EXEC ~name:"compute"
    ~descr:(Md.plain "Compute regions for the given declaration")
    ~input:(module Kernel_ast.Decl)
    ~output:(module Data.Junit)
    (function SFunction kf -> Analysis.compute kf | _ -> ())

let () =
  Request.register
    ~package ~kind:`GET ~name:"regions"
    ~descr:(Md.plain "Returns computed regions for the given declaration")
    ~input:(module Kernel_ast.Decl)
    ~output:(module Regions)
    ~signals:[signal]
    begin fun decl ->
      try Memory.regions @@ map_of_declaration decl
      with Not_found -> []
    end

let () =
  Request.register
    ~package ~kind:`GET ~name:"regionsAt"
    ~descr:(Md.plain "Compute regions at the given marker program point")
    ~input:(module Kernel_ast.Marker)
    ~output:(module Regions)
    ~signals:[signal]
    begin fun loc ->
      try Memory.regions @@ map_of_localizable loc
      with Not_found -> []
    end

let () =
  Request.register
    ~package ~kind:`GET ~name:"localize"
    ~descr:(Md.plain "Localize the marker in its map")
    ~input:(module Kernel_ast.Marker)
    ~output:(module NodeOpt)
    ~signals:[signal]
    begin fun loc ->
      try
        let map = map_of_localizable loc in
        region_of_localizable map loc
      with Not_found -> None
    end

(* -------------------------------------------------------------------------- *)
