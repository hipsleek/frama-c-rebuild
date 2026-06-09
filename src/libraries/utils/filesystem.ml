(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Filepath

(* -------------------------------------------------------------------------- *)
(* --- Error handling                                                     --- *)
(* -------------------------------------------------------------------------- *)

(* Filesystem Exceptions *)

let error_message : exn -> string = function
  | Sys_error msg -> msg
  | Unix.Unix_error (code, _, _) -> Unix.error_message code
  | _ -> assert false

(* Convert Unix_error to Sys_error *)
let convert_exception : exn -> exn = function
  | Unix.Unix_error _ as exn -> Sys_error (error_message exn)
  | exn -> exn

let raise_exception format =
  Format.kasprintf (fun msg -> raise (Sys_error msg)) format

(* Errors *)

type error = string * Filepath.t
type nonrec 'a result = ('a,error) result

let convert_error (p : t) (exn : exn) : 'a result =
  Error (error_message exn, p)

(* Invalid arguments *)

let check_nonempty p =
  if is_empty p then
    invalid_arg "path should not be empty"


(* -------------------------------------------------------------------------- *)
(* --- File system                                                        --- *)
(* -------------------------------------------------------------------------- *)

type file_kind =
  | File
  | Directory
  | CharacterDevice
  | BlockDevice
  | SymbolicLink
  | NamedPipe
  | Socket

let convert_file_kind : Unix.file_kind -> file_kind = function
  | S_REG -> File
  | S_DIR -> Directory
  | S_CHR -> CharacterDevice
  | S_BLK -> BlockDevice
  | S_LNK -> SymbolicLink
  | S_FIFO -> NamedPipe
  | S_SOCK -> Socket

let file_kind (p : t) =
  check_nonempty p;
  try
    let stats = Unix.stat (Filepath.to_string_abs p) in
    Ok (convert_file_kind stats.st_kind)
  with Unix.Unix_error _ as exn ->
    convert_error p exn

let exists (p : t) =
  file_kind p |> Result.is_ok

let file_exists (p : t) =
  file_kind p = Ok (File)

let dir_exists (p : t) =
  file_kind p = Ok (Directory)

let read_dir (p : t) =
  check_nonempty p;
  Sys.readdir (Filepath.to_string_abs p)

let list_dir (p : t) =
  read_dir p |> Array.to_list

let iter_dir (f : string -> unit) (p : t) : unit =
  read_dir p |> Array.iter (fun s -> f s)

let fold_dir (f : string -> 'a -> 'a) (p : t) (acc : 'a) : 'a =
  read_dir p |> Array.fold_left (fun acc s ->  f s acc) acc

let remove_file (p : t) =
  try
    Unix.unlink (Filepath.to_string_abs p)
  with Unix.Unix_error _ -> ()

let rec remove_dir (p : t) =
  try
    iter_dir
      (fun s ->
         let f = p / s in
         if dir_exists f then remove_dir f else remove_file f
      ) p;
    Unix.rmdir (Filepath.to_string_abs p)
  with Unix.Unix_error _ -> ()

let rename (s : t) (t : t) =
  check_nonempty s;
  check_nonempty t;
  Sys.rename (Filepath.to_string_abs s) (Filepath.to_string_abs t)

let rec make_dir ?(parents=true) ?(perm=0o755) (p: t) =
  check_nonempty p;
  try
    Unix.mkdir (Filepath.to_string_abs p) perm
  with
  | Unix.Unix_error (Unix.ENOENT,_,_) when parents ->
    let parent = Filepath.dirname p in
    if p <> parent then (* Prevent infinite recursion; can it ever happen ? *)
      make_dir ~parents ~perm parent;
    make_dir ~parents:false ~perm p
  | Unix.Unix_error (Unix.EEXIST,_,_) ->
    if not (dir_exists p) then
      raise_exception "%a exists but is not a directory" Filepath.pretty p
  | exn ->
    raise (convert_exception exn)


(* -------------------------------------------------------------------------- *)
(* --- Temporary files                                                    --- *)
(* -------------------------------------------------------------------------- *)

let temp_file ~prefix ~suffix =
  Filename.temp_file prefix suffix |> Filepath.of_string

let temp_dir ~prefix ~suffix =
  (* temp_dir is introduced in Ocaml 5.1 *)
  let rec one_try limit =
    try
      let dir = Filename.temp_file prefix suffix in
      Unix.unlink dir;
      Unix.mkdir dir 0o700;
      Filepath.of_string dir
    with
    | Unix.Unix_error _ when limit >= 0 ->
      one_try (pred limit)
    | exn ->
      raise (convert_exception exn)
  in
  one_try 10


(* -------------------------------------------------------------------------- *)
(* --- File comparison                                                    --- *)
(* -------------------------------------------------------------------------- *)

let digest_raw (p : t) =
  check_nonempty p;
  try
    Digest.file (Filepath.to_string_abs p)
  with exn ->
    raise (convert_exception exn)

let digest (p : t) =
  digest_raw p |> Digest.to_hex

let same_digest (p1 : t) (p2 : t) =
  String.equal (digest_raw p1) (digest_raw p2)


(* -------------------------------------------------------------------------- *)
(* --- Low level Input/Output                                            --- *)
(* -------------------------------------------------------------------------- *)

type action_if_missing = Create of int | DoNotCreate
type action_if_exists = Error | Append | Truncate

type ('ch,'a) safe_processor = ('ch -> 'a) -> 'a result
type ('ch,'a) exn_processor = ('ch -> 'a) -> 'a

let flags_and_perm ?if_exists ~if_missing ~binary ~blocking default =
  let l =
    default ::
    (if binary then [Open_binary] else [Open_text]) @
    (if blocking then [] else [Open_nonblock]) @
    match if_exists with
    | Some Error -> [Open_excl]
    | Some Append ->  [Open_append]
    | Some Truncate -> [Open_trunc]
    | None -> []
  in
  match if_missing with
  | DoNotCreate -> l, 0 (* perm is ignored when Open_creat is not set *)
  | Create mode -> Open_creat :: l, mode

(* We don't directly use Fun.protect as it catches exceptions in [finally]
   and reraise them as [Finally_raised exn]. However, a [Sys_error] can be
   raised by {!close_out} (and {!close_in} but it should not happen).
*)
let protect_file_op ~(close: 'ch -> unit) (job: 'ch -> 'a) (channel: 'ch) =
  let r =
    try job channel with
    | exn ->
      try
        close channel;
        raise exn
      with
      | Sys_error _ ->
        raise exn (* re-raise the first exception, do not erase it *)
  in
  close channel;
  r

let with_open_in_exn
    ?(if_missing=DoNotCreate)
    ?(binary=false)
    ?(blocking=true)
    (p: t)
    (job: in_channel -> 'a): 'a =
  check_nonempty p;
  let flags, perm =
    flags_and_perm ~if_missing ~binary ~blocking Open_rdonly
  in
  open_in_gen flags perm (Filepath.to_string_abs p)
  |> protect_file_op ~close:close_in job

let with_open_in ?if_missing ?binary ?blocking p job =
  try Ok (with_open_in_exn ?if_missing ?binary ?blocking p job)
  with Sys_error m -> Error (m,p)

let with_open_out_exn
    ?(if_missing=Create 0o666)
    ?(if_exists=Truncate)
    ?(binary=false)
    ?(blocking=true)
    (p: t)
    (job: out_channel -> 'a): 'a =
  check_nonempty p;
  let flags, perm =
    flags_and_perm ~if_exists ~if_missing ~binary ~blocking Open_wronly
  in
  open_out_gen flags perm (Filepath.to_string_abs p)
  |> protect_file_op ~close:close_out job

let with_open_out ?if_missing ?if_exists ?binary ?blocking p job =
  try Ok (with_open_out_exn ?if_missing ?if_exists ?binary ?blocking p job)
  with Sys_error m -> Error (m,p)

module Compressed : sig
  val with_open_in_exn :
    Filepath.t ->
    (Channel.input, 'a) exn_processor
  val with_open_out_exn :
    ?compress:bool ->
    Filepath.t ->
    (Channel.output, 'a) exn_processor
end = struct
  let with_open_in_exn (p : t) job =
    check_nonempty p;
    Channel.open_in_bin (Filepath.to_string_abs p)
    |> protect_file_op ~close:Channel.close_in job

  let with_open_out_exn ?compress (p : t) job =
    check_nonempty p;
    Channel.open_out_bin ?compress (Filepath.to_string_abs p)
    |> protect_file_op ~close:Channel.close_out job
end

module Operators =
struct
  let (let+) with_open job = with_open job
  let (let*) with_open job = with_open job |> Result.join
  let (let$) with_open job = with_open job
end


(* -------------------------------------------------------------------------- *)
(* --- High level Input/Output                                            --- *)
(* -------------------------------------------------------------------------- *)

open Operators

let with_formatter_exn p job =
  let$ out_channel = with_open_out_exn p in
  let fmt = Format.formatter_of_out_channel out_channel in
  let finally = Format.pp_print_flush fmt in
  Fun.protect ~finally (fun () -> job fmt)

let with_formatter p job =
  try Ok (with_formatter_exn p job)
  with Sys_error m -> Error (m, p)

let rec bincopy buffer in_channel out_channel =
  let s = Bytes.length buffer in
  let n = input in_channel buffer 0 s in
  if n > 0 then
    ( output out_channel buffer 0 n ; bincopy buffer in_channel out_channel )
  else
    ( flush out_channel )

let copy_file src tgt =
  let$ in_channel = with_open_in_exn src in
  let$ out_channel = with_open_out_exn tgt in
  bincopy (Bytes.create 2048) in_channel out_channel

let iter_lines p job =
  let$ in_channel = with_open_in_exn p in
  try
    while true do
      job (input_line in_channel) ;
    done
  with End_of_file -> ()

let iteri_lines p job =
  let i = ref 0 in
  iter_lines p (fun line -> incr i; job !i line)

let iter_line_range p first_line last_line job =
  let job' i line =
    if i >= first_line then begin
      job i line;
      if i >= last_line then raise Exit
    end
  in
  try iteri_lines p job'  with Exit -> ()


(* -------------------------------------------------------------------------- *)
(* --- Tests                                                              --- *)
(* -------------------------------------------------------------------------- *)

let _test_file () =
  let filepath = temp_file ~prefix:"" ~suffix:"" in
  Extlib.safe_at_exit (fun () -> remove_file filepath);
  filepath

let _test_dir () =
  let filepath = temp_dir ~prefix:"" ~suffix:"" in
  Extlib.safe_at_exit (fun () -> remove_dir filepath);
  filepath

let _test_filename () =
  let filepath = temp_file ~prefix:"" ~suffix:"" in
  remove_file filepath;
  filepath

let%test _ =
  try ignore (file_exists Filepath.empty); false
  with Invalid_argument _ -> true
let%test _ = file_exists (_test_file ())
let%test _ = not (file_exists (_test_dir ()))
let%test _ = not (dir_exists (_test_file ()))
let%test _ = dir_exists (_test_dir ())
let%test _ =
  try make_dir (_test_file ()); false (* path exists and is not a directory *)
  with Sys_error _ -> true
let%test _ =
  let p = _test_filename () in
  make_dir p;
  dir_exists p
let%test _ =
  let p = _test_filename () in
  make_dir (p / "subdir");
  dir_exists (p / "subdir")
