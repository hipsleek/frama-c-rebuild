(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module C = Configurator.V1

module Temp = struct (* Almost copied from configurator *)
  let (^/) a b = a ^ "/" ^ b

  let rec rm_rf dir =
    Array.iter (fun fn ->
        let fn = dir ^/ fn in
        if Sys.is_directory fn then rm_rf fn else Unix.unlink fn)
      (Sys.readdir dir);
    Unix.rmdir dir

  let prng = lazy (Random.State.make_self_init ())

  let gen_name ~dir ~prefix ~suffix =
    let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
    dir ^/ Printf.sprintf "%s%06x%s" prefix rnd suffix

  let create_dir () =
    let dir =
      gen_name ~dir:(Filename.get_temp_dir_name ()) ~prefix:"" ~suffix:"" in
    Unix.mkdir dir 0o700 ;
    at_exit (fun () -> rm_rf dir) ;
    dir

  let create ?(dir=create_dir ()) ?(prefix="") ?(suffix="") mk =
    let rec try_name counter =
      let name = gen_name ~dir ~prefix ~suffix in
      match mk name with
      | () -> name
      | exception Unix.Unix_error _ when counter < 1000 -> try_name (counter + 1)
    in
    try_name 0
end

module C_preprocessor = struct (* This could be put in Dune? *)
  type t =
    { preprocessor: string
    ; pp_opt: string option
    }

  let stdout_contains out str =
    let re = Str.regexp_string str in
    try ignore (Str.search_forward re out 0); true
    with Not_found -> false

  let find_preprocessor configurator =
    let cc_env = try Sys.getenv "CPP" with Not_found -> "" in
    if cc_env <> "" then (cc_env, None) (* assume default CPP needs no args *)
    else
      let finder (command, _pp_opt) =
        C.which configurator command |> Option.is_some
      in
      (* Note: We could add 'cl.exe' to the list, but since it requires
         '/<opt>' and not '-<opt>' for its options, it will fail in every
         check anyway. So the user may manually specify it if they want it,
         but having it here brings no benefit.
         Note: 'cpp' is NOT the POSIX way to call the preprocessor, and it
         behaves VERY badly on macOS (as if using '-traditional', see
         https://stackoverflow.com/questions/9508159).
         Therefore, we try `gcc -E` and `cc -E`, but not 'cpp'.
      *)
      try List.find finder [("gcc", Some "-E"); ("cc", Some "-E");
                            ("x86_64-w64-mingw32-gcc", Some "-E")]
      with Not_found -> C.die "Could not find a C preprocessor"

  let write_file name code =
    let out = open_out name in
    Printf.fprintf out "%s" code ;
    close_out out

  let call configurator preprocessor options code =
    let dir = Temp.create_dir () in
    let file =
      Temp.create ~dir ~suffix:".c" (fun name -> write_file name code) in
    C.Process.run configurator ~dir preprocessor (options @ [ file ])

  let get configurator =
    let preprocessor, pp_opt = find_preprocessor configurator in
    { preprocessor ; pp_opt }

  let preprocess configurator t options code =
    call configurator t.preprocessor (Option.to_list t.pp_opt @ options) code
end

(* Frama-C specific part *)

module Cpp = struct
  module GnuLike = struct
    let code = {|
#define foo 0
/* foo */
int main(){}
|}

    let check configurator preprocessor =
      let options = ["-dD" ; "-nostdinc"] in
      (C_preprocessor.preprocess configurator preprocessor options code).exit_code = 0
  end

  module KeepComments = struct
    let code =
      {|/* Check whether comments are kept in output */|}

    let keep_comments_option = "-C"

    let check configurator preprocessor =
      let result = C_preprocessor.preprocess configurator preprocessor [keep_comments_option] code in
      result.exit_code = 0 && C_preprocessor.stdout_contains result.stdout "kept"
  end

  module Archs = struct
    let opt_m_code value =
      Format.asprintf {|/* Check if preprocessor supports option -m%s */|} value

    let check configurator preprocessor arch =
      let code = opt_m_code arch in
      let options = [ Format.asprintf "-m%s" arch ] in
      if (C_preprocessor.preprocess configurator preprocessor options code).exit_code = 0
      then Some arch else None

    let supported_archs configurator preprocessor archs =
      let check = check configurator preprocessor in
      List.map (fun s -> "-m" ^ s) @@ List.filter_map check archs
  end

  module C2x = struct
    let code = {|int main(){}|}

    let check configurator preprocessor =
      let options = ["-std=c2x"] in
      (C_preprocessor.preprocess configurator preprocessor options code).exit_code = 0
  end

  module C2y = struct
    let code = {|int main(){}|}

    let check configurator preprocessor =
      let options = ["-std=c2y"] in
      (C_preprocessor.preprocess configurator preprocessor options code).exit_code = 0
  end

  type t =
    { preprocessor : C_preprocessor.t
    ; default_args : string list
    ; is_gnu_like : bool
    ; keep_comments : bool
    ; supported_archs_opts : string list
    ; has_c2x : bool
    ; has_c2y : bool
    }

  let get configurator =
    let preprocessor = C_preprocessor.get configurator in
    let default_args = Option.to_list preprocessor.pp_opt @ [ "-C" ; "-I." ] in
    let is_gnu_like = GnuLike.check configurator preprocessor in
    let keep_comments = KeepComments.check configurator preprocessor in
    let supported_archs_opts =
      Archs.supported_archs configurator preprocessor [ "16" ; "32" ; "64" ] in
    let has_c2x = C2x.check configurator preprocessor in
    let has_c2y = C2y.check configurator preprocessor in
    { preprocessor
    ; default_args
    ; is_gnu_like
    ; keep_comments
    ; supported_archs_opts
    ; has_c2x
    ; has_c2y
    }

  let pp_flags fmt =
    let pp_sep fmt () = Format.fprintf fmt " " in
    Format.pp_print_list ~pp_sep Format.pp_print_string fmt

  let pp_default_cpp fmt cpp =
    Format.fprintf fmt "%s %a"
      cpp.preprocessor.preprocessor
      pp_flags cpp.default_args

  let pp_archs fmt cpp =
    let pp_arch fmt arch = Format.fprintf fmt "\"%s\"" arch in
    let pp_sep fmt () = Format.fprintf fmt "; " in
    Format.fprintf fmt
      "%a" (Format.pp_print_list ~pp_sep pp_arch) cpp.supported_archs_opts

  let add_macros tbl cpp =
    Hashtbl.replace tbl "@FRAMAC_DEFAULT_CPP@"
      (Format.asprintf "%a" pp_default_cpp cpp);
    Hashtbl.replace tbl "@FRAMAC_DEFAULT_CPP_ARGS@"
      (Format.asprintf "%a" pp_flags cpp.default_args);
    Hashtbl.replace tbl "@FRAMAC_GNU_CPP@" (string_of_bool cpp.is_gnu_like);
    Hashtbl.replace tbl "@DEFAULT_CPP_KEEP_COMMENTS@"
      (string_of_bool cpp.keep_comments);
    Hashtbl.replace tbl "@DEFAULT_CPP_KEEP_COMMENTS@"
      (string_of_bool cpp.keep_comments);
    Hashtbl.replace tbl "@DEFAULT_CPP_SUPPORTED_ARCH_OPTS@"
      (Format.asprintf "%a" pp_archs cpp);
end

module Fc_version = struct
  type t =
    { major: string
    ; minor: string
    ; ext: string
    ; name: string
    }

  let head1 file = (* output first line from file, without '\n' *)
    try
      let ic = open_in file in
      let s = input_line ic in
      close_in ic;
      s
    with _exc ->
      C.die "Can't read %s." file

  let get () =
    let out_VERSION = head1 "VERSION" in
    let re_version =
      Str.regexp {|\([1-9][0-9]\)\(\.\([0-9]\)\|~dev\)\(.*\)|}
    in
    if not (Str.string_match re_version out_VERSION 0) then
      C.die "Invalid VERSION contents.";
    let major = Str.matched_group 1 out_VERSION in
    let rest = Str.matched_group 2 out_VERSION in
    let minor, ext =
      if rest.[0] = '~' then "0", "~~dev"
      else
        Str.matched_group 3 out_VERSION,
        try Str.matched_group 4 out_VERSION with Not_found -> ""
    in
    let name = head1 "VERSION_CODENAME" in
    { major; minor; ext; name }

  let add_macros tbl version =
    Hashtbl.replace tbl "@VERSION@"
      (Format.asprintf "%s.%s%s" version.major version.minor version.ext);
    Hashtbl.replace tbl "@VERSION_CODENAME@" version.name;
    Hashtbl.replace tbl "@MAJOR_VERSION@" version.major;
    Hashtbl.replace tbl "@MINOR_VERSION@" version.minor
end

module OS = struct
  type t = Windows | MacOS | OtherUnix

  let get configurator =
    match C.ocaml_config_var_exn configurator "os_type" with
    | "Win32" -> Windows
    | _ ->
      match C.ocaml_config_var_exn configurator "system" with
      | "macosx" -> MacOS
      | _ -> OtherUnix

  let add_macros tbl t =
    let module_dirs = match t with
      | Windows   -> "Win_dirs"
      | MacOS     -> "Macos_dirs"
      | OtherUnix -> "Unix_dirs"
    in
    Hashtbl.replace tbl "@FRAMAC_TARGET_SYSTEM_CONFIG_INCLUDE@" module_dirs
end

let python_available configurator =
  let result = C.Process.run configurator "python3" ["--version"] in
  if result.exit_code <> 0 then false
  else
    let out = result.stdout in
    let re_version =
      Str.regexp {|.* \([0-9]\)\.\([0-9]+\).[0-9]+|}
    in
    try
      let maj, med =
        if Str.string_match re_version out 0 then
          int_of_string @@ Str.matched_group 1 out,
          int_of_string @@ Str.matched_group 2 out
        else raise Not_found
      in
      if maj <> 3 || med < 10 then raise Not_found ;
      true
    with Not_found | Failure _ -> false

(* macOS finds it clever pretending that gcc is clang. Check that
   gcc --version actually contains the 'gcc' string.
*)
let gcc_is_genuine configurator =
  let result = C.Process.run configurator "gcc" ["--version"] in
  if result.exit_code <> 0 then false
  else
    let out = result.stdout in
    let re_name = Str.regexp {|.*gcc|} in
    Str.string_match re_name out 0

let re_macro = Str.regexp "@[A-Za-z0-9_]+@"
let expand macros line =
  let line = ref line in
  try
    while true do
      ignore (Str.search_forward re_macro !line 0);
      let key = Str.matched_string !line in
      match Hashtbl.find_opt macros key with
      | None -> C.die "Unknown macro: %s" key
      | Some v ->
        line := Str.global_replace (Str.regexp_string key) v !line
    done;
    assert false
  with Not_found ->
    !line

let configure configurator =
  let version = Fc_version.get () in
  let cpp = Cpp.get configurator in
  let os = OS.get configurator in
  let macros = Hashtbl.create 10 in
  Fc_version.add_macros macros version;
  Cpp.add_macros macros cpp;
  OS.add_macros macros os;
  let ic = open_in "src/kernel_internals/runtime/system_config.ml.in" in
  let oc = open_out "system_config.ml" in
  try
    while true; do
      let line = expand macros (input_line ic) in
      output_string oc line;
      output_char oc '\n';
    done;
  with End_of_file ->
    close_in ic;
    close_out oc;
    let python = open_out "python-3.10-available" in
    Printf.fprintf python "%B" (python_available configurator);
    close_out python;
    let gcc = open_out "gcc-is-genuine" in
    Printf.fprintf gcc "%B" (gcc_is_genuine configurator);
    close_out gcc;
    let c2x = open_out "has-c2x-option" in
    Printf.fprintf c2x "%B" cpp.has_c2x;
    close_out c2x;
    let c2y = open_out "has-c2y-option" in
    Printf.fprintf c2y "%B" cpp.has_c2y;
    close_out c2y

let () =
  C.main ~name:"frama_c_config" configure
