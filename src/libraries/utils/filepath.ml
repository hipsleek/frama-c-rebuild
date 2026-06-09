(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


(* -------------------------------------------------------------------------- *)
(* --- Current Working Directory                                          --- *)
(* -------------------------------------------------------------------------- *)

(* Note: the call to Unix.realpath prevents some issues with symbolic links
   in directory names. If you have problems with this, please contact us.
   For the same reason, Sys.getcwd should _not_ be called directly, but only
   via this function, to avoid conflicting results in case the user forgot
   to call Unix.realpath.
*)
let pwd () = Unix.(realpath (getcwd ()))


(* -------------------------------------------------------------------------- *)
(* --- Conversion from string                                             --- *)
(* -------------------------------------------------------------------------- *)

type existence =
  | Must_exist
  | Must_not_exist
  | Indifferent

exception No_file
exception File_exists

let sanitize_filename s =
  (* Invalid characters for different OSes taken from
     <https://stackoverflow.com/a/31976060> *)
  let is_invalid = function
    (* Unix limitations *)
    | '/' -> true
    | c when Char.code c = 0 -> true
    (* Additional MacOS limitations *)
    | ':' -> true
    (* Additional Windows limitations *)
    | '<' | '>' | '"' | '\\' | '|' | '?' | '*' -> true
    | c when Char.code c > 0 && Char.code c <= 31 -> true
    | _ -> false
  in
  String.map (fun c -> if is_invalid c then '_' else c) s

let normalize ?base s =
  if s = ""
  then ""
  else
    let norm_path_name = Hpath.(of_string ?base s |> to_string) in
    if norm_path_name = ""
    then "/"
    else norm_path_name

let check_existence ~existence p =
  match existence with
  | Must_exist when not (Sys.file_exists p) ->
    raise No_file
  | Must_not_exist when Sys.file_exists p ->
    raise File_exists
  | Indifferent | Must_exist | Must_not_exist -> ()

let of_string ?(existence=Indifferent) ?base s =
  let p = normalize ?base s in
  check_existence ~existence p;
  p

let of_format ?existence ?dir format =
  let to_filepath s =
    sanitize_filename s
    |> of_string ?existence ?base:dir
  in
  Format.kasprintf to_filepath format

(* -------------------------------------------------------------------------- *)
(* --- Datatype                                                           --- *)
(* -------------------------------------------------------------------------- *)

(** Avoid using {!of_string} here because {!Hpath.of_string} prefixes the string
    with the current working directory. We need to make sure the path is the
    same for all executions of Frama-C because {!dummy} is used in the reprs
    of the datatype and having different dummies can break loads/saves.
*)
let dummy = "@dummy_filepath@"

type t = string [@@deriving show]

include (
  Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    type nonrec t = t
    let name = "Filepath"
    let reprs = [ dummy ]
    let equal = String.equal
    let compare = String.compare
    let hash = Hashtbl.hash (* String.hash only introduced in OCaml 5.0 *)
    let copy = Fun.id
  end) : Datatype.S_with_collections with type t := t)


(* -------------------------------------------------------------------------- *)
(* --- Constant paths                                                     --- *)
(* -------------------------------------------------------------------------- *)

let empty = of_string ""
let is_empty fp = equal fp empty
let special_stdout = of_string "-"
let is_special_stdout fp = equal fp special_stdout


(* -------------------------------------------------------------------------- *)
(* --- Pretty printing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let to_base_uri name =
  Hpath.(of_string name |> to_uri)

let to_string p =
  if is_special_stdout p then
    "<stdout>"
  else if is_empty p then
    "<unknown>"
  else
    match to_base_uri p with
    | Absolute, uri -> uri
    | (Cwd | Name (".",_)), uri -> if uri = "" then "." else uri
    | Name (name,_), uri -> if uri = "" then name else name ^ "/" ^ uri

let pretty fmt p =
  Format.pp_print_string fmt (to_string p)

let compare_pretty ?(case_sensitive=false) s1 s2 =
  let s1 = to_string s1 in
  let s2 = to_string s2 in
  if case_sensitive then String.compare s1 s2
  else
    String.compare
      (String.lowercase_ascii s1)
      (String.lowercase_ascii s2)

let to_string_abs ?(quoted=false) s =
  if quoted
  then Filename.quote s
  else s

let pretty_abs fmt p =
  Format.fprintf fmt "%s" p

let to_string_list l = l


(* -------------------------------------------------------------------------- *)
(* --- Path manipulation                                                  --- *)
(* -------------------------------------------------------------------------- *)

let basename p = Filename.basename p

let dirname p = Filename.dirname p

let extension p = Filename.extension p

let extend ?existence t ext = of_string ?existence (t ^ ext)

let concat ?existence t s = of_string ?existence (t ^ "/" ^ s)

let (/) = concat ~existence:Indifferent

let concats ?existence t sl =
  let s' = List.fold_left (fun acc s -> acc ^ "/" ^ s) "" sl in
  of_string ?existence (t ^ s')

let has_suffix p suffix = Filename.check_suffix p suffix

let chop_suffix p suffix = Filename.chop_suffix p suffix


(* -------------------------------------------------------------------------- *)
(* --- Relative Paths                                                     --- *)
(* -------------------------------------------------------------------------- *)

let to_string_rel ?(quoted=false) ?(base=Hpath.(cwd |> to_string)) p =
  let r =
    if base = p then "."
    else
      let base = base ^ Filename.dir_sep in
      if String.starts_with ~prefix:base p then
        let n = String.length base in
        let p = String.sub p n (String.length p - n) in
        if p = "" then "." else p
      else p
  in
  if quoted then Filename.quote r else r

let pretty_rel fmt p =
  Format.pp_print_string fmt (to_string_rel p)

let is_relative ?(base=Hpath.(cwd |> to_string)) p =
  String.equal base p || String.starts_with ~prefix:(base ^ Filename.dir_sep) p

(* -------------------------------------------------------------------------- *)
(* --- Symboling Names                                                    --- *)
(* -------------------------------------------------------------------------- *)

let add_symbolic_dir name p =
  Hpath.(Names.add (of_string p) name)

let add_symbolic_dir_list name l =
  List.iter (fun p -> Hpath.(Names.add (of_string p)) name) l

let remove_symbolic_dir p =
  Hpath.Names.remove (Hpath.of_string p)

let all_symbolic_dirs () =
  Hpath.Names.all ()
  |> List.map (fun (path, name) -> (name, Hpath.to_string path))


(* -------------------------------------------------------------------------- *)
(* --- Position in source file                                            --- *)
(* -------------------------------------------------------------------------- *)

type position = {
  pos_path : t;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

let empty_pos = {
  pos_path = empty;
  pos_lnum = 0;
  pos_bol = 0;
  pos_cnum = -1;
}

let pp_pos fmt pos =
  let path = pos.pos_path in
  if is_empty path || is_special_stdout path then
    Format.fprintf fmt "%a" pretty path
  else
    Format.fprintf fmt "%a:%d" pretty path pos.pos_lnum

let is_empty_pos pos = pos == empty_pos


(* -------------------------------------------------------------------------- *)
(* --- Tests                                                              --- *)
(* -------------------------------------------------------------------------- *)

let%test _ = of_string "/" = "/"
let%test _ = of_string "/.." = "/"
let%test _ = of_string "/../../." = "/"
let%test _ = of_string "///" = "//"
let%test _ = of_string "//tmp//" = "//tmp/"
let%test _ = of_string "/../tmp/../.." = "/"
let%test _ = of_string "/tmp/inexistent_directory/.." = "/tmp"
let%test _ = of_string "" = ""
let%test _ = to_string_rel (of_string ".") = "."
let%test _ = to_string_rel (of_string "./tests/..") = "."
let%test _ =
  to_string_rel ~base:(of_string "/a/b/") (of_string "/a/bc/d") = "/a/bc/d"
let%test _ =
  add_symbolic_dir "SYMB" (of_string "/tmp/symb/");
  to_string (of_string "/tmp/symb/file.c") = "SYMB/file.c"
