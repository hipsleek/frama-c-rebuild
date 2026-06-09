(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* --- Type definition --- *)

module Prototype = struct
  type t = {
    hash : int ;
    path_name : string ;
    base_name : string ; (* Filename.basename *)
    dir : t option ; (* path whose path_name is Filename.dirname *)
    mutable symbolic_name : string option ; (* Symbolic name *)
  }
  let hash p = p.hash
  let equal p q = p.path_name = q.path_name
end

include Prototype

module Table = Hashtbl.Make (Prototype)


(* --- Construction --- *)

let empty = {
  path_name = "";
  hash = 0;
  base_name = ".";
  dir = None;
  symbolic_name = None
}

(* re_drive and re_root match drive expressions to deal with non-Cygwin
   Windows-like paths (e.g. with MinGW) *)
let re_drive = Str.regexp "[A-Za-z]:"
let re_path = Str.regexp "[/\\\\]"
let re_root = Str.regexp "/\\|\\([A-Za-z]:\\\\\\)\\|\\([A-Za-z]:/\\)"

(* Can not use Weak, since the internal [t] representation is temporary.
   Can not use a weak-cache because each minor GC
   may empty the cache (see #191). *)

module Hcons : sig
  val find : t -> t
  val merge : t -> t
end =
struct
  let table = Table.create 128
  let find = Table.find table
  let merge p =
    try
      Table.find table p
    with Not_found ->
      Table.add table p p ; p
end


let cache = Array.make 256 None
let dir path =
  match path.dir with
  | None -> empty (* the parent of the root directory is itself *)
  | Some d -> d

let root path_name =
  Hcons.merge { empty with path_name ; hash = Hashtbl.hash path_name }

let make dir base_name =
  let path_name = Printf.sprintf "%s/%s" dir.path_name base_name in
  let hash = Hashtbl.hash path_name in
  Hcons.merge
    { empty with
      path_name;
      hash;
      base_name = base_name;
      dir = Some dir
    }

let rec norm path = function
  | [] -> path
  | ".." :: ps -> norm (dir path) ps
  | "." :: ps -> norm path ps
  | p :: ps -> norm (make path p) ps

let insert ~base path_name =
  let full_path_name =
    (* if a <base> is provided with a <file> which is already absolute
       (and thus matches [re_root]) then the <base> is not taken
       into account *)
    if Str.string_match re_root path_name 0
    then path_name
    else base.path_name ^ "/" ^ path_name in
  let hash = Hashtbl.hash full_path_name in
  match Array.get cache (hash land 255) with
  | Some (pn, p) when full_path_name = pn -> p
  | _ ->
    let p = { empty with path_name = full_path_name; hash } in
    try Hcons.find p
    with Not_found ->
      let base =
        (* if a <base> is provided while a <file> is already absolute
           (and thus matches [re_root]) then the <base> is not taken
           into account *)
        if Str.string_match re_root path_name 0
        then root (String.sub path_name 0 (Str.group_end 0 - 1))
        else base in
      let name_parts = Str.split re_path path_name in
      (* Windows paths may start with '<drive>:'. If so, remove it *)
      let parts = if List.length name_parts > 0 &&
                     Str.string_match re_drive (List.nth name_parts 0) 0 then
          List.tl name_parts
        else name_parts
      in
      let path = norm base parts in
      Array.set cache (hash land 255) (Some (path_name, path));
      path

let cwd =
  Unix.(realpath (getcwd ()))
  |> insert ~base:empty


(* --- Conversion --- *)

let of_string ?base path_name =
  let base = Option.fold ~none:cwd ~some:(insert ~base:cwd) base in
  insert ~base path_name

let to_string path =
  path.path_name

type base = Absolute | Cwd | Name of string * t

let to_uri path =
  let buffer = Buffer.create 80 in
  let rec add_component path =
    match path.symbolic_name with
    | Some sn -> Name (sn, path)
    | None when path == cwd (* hconsed *) -> Cwd
    | None ->
      match path.dir with
      | None -> (* root *)
        Buffer.add_string buffer path.path_name;
        Absolute
      | Some parent ->
        let base = add_component parent in
        if Buffer.length buffer > 0 || base = Absolute then
          Buffer.add_char buffer '/';
        Buffer.add_string buffer path.base_name;
        base
  in
  let base = add_component path in
  let uri = Buffer.contents buffer in
  base, uri


(* -------------------------------------------------------------------------- *)
(* --- Symbolic Names                                                     --- *)
(* -------------------------------------------------------------------------- *)

(* Note: Symbolic directories are not currently projectified *)

module Names = struct
  let table : string Table.t = Table.create 3

  let add path name =
    Table.replace table path name;
    path.symbolic_name <- Some name

  let remove path =
    Table.remove table path;
    path.symbolic_name <- None

  let all () =
    let compare (p1, n1) (p2, n2) =
      let c = String.compare n1 n2 in
      if c <> 0 then c
      else String.compare p1.path_name p2.path_name
    in
    Table.to_seq table
    |> List.of_seq
    |> List.sort compare
end
