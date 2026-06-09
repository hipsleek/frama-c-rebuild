(* Performs syntactic checks related to parseability of Frama-C libc headers *)

let run_once = ref false

let header_re = Str.regexp ".*\\.h$"

let is_header f = Str.string_match header_re f 0

(* Files which are *not* supposed to be parsed *)
let blacklist libc_dir =
  List.map (Filepath.concat libc_dir) ["tgmath.h"; "complex.h"]

(* only goes down one level, which is enough for the libc *)
let collect_headers () =
  let libc_dir = Kernel.Share.get_dir "libc" in
  let libc_dir_files = Filesystem.list_dir libc_dir in
  let contents =
    List.map (Filename.concat (Filepath.to_string_abs libc_dir)) libc_dir_files
  in
  let subdirs = List.filter Sys.is_directory contents in
  let base_headers = List.filter is_header contents in
  let all_headers = List.fold_left (fun acc dir ->
      let contents = Array.to_list (Sys.readdir dir) in
      let headers =
        List.filter_map
          (fun c -> if is_header c then Some (Filename.concat dir c) else None)
          contents
      in
      acc @ headers
    ) base_headers subdirs
  in
  let all_headers = List.sort String.compare_ignore_case all_headers in
  let to_skip = blacklist libc_dir in
  List.iter (fun header ->
      let header_path = Filepath.of_string header in
      if List.mem header_path to_skip then
        Format.printf "skipping %a@." Filepath.pretty header_path
      else begin
        Kernel.Files.clear ();
        Dynamic.Parameter.String.set "" header;
        Ast.compute ();
      end
    ) all_headers

let () =
  Boot.Main.extend collect_headers
