(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module C = Configurator.V1

(** {2 Helper functions} *)

type test_kind =
  | Bin of string * string list (** Arguments for [bin_available] *)
  | Dep of string (** Arguments for [dep_available] *)

let cache = Hashtbl.create 7
let memo k f =
  try Hashtbl.find cache k
  with Not_found ->
    let v = f () in
    Hashtbl.add cache k v;
    v

(** [bin_available configurator (prog, opts)] tries to run the given program
    [prog] with the given arguments [args] and returns [true] if the exit code
    is zero *)
let bin_available configurator (prog, opts) =
  memo (Bin (prog, opts)) @@ fun () ->
  let result = C.Process.run configurator prog opts in
  result.exit_code = 0

(** [dep_available file] reads the content of the given file and tries to
    interpret it as a boolean, returns the value of that boolean, [false] if it
    cannot read the file. *)
let dep_available file =
  memo (Dep file) @@ fun () ->
  let ic = open_in file in
  let result =
    try input_line ic
      (* Only take the first word of the line *)
      |> String.split_on_char ' ' |> List.hd
      (* Try to parse a boolean *)
      |> bool_of_string
    with _ -> false
  in
  close_in_noerr ic;
  result

(** [test_aux filename configurator l] executes all tests in [l] and returns the
    conjunction of the results. In addition, the result is saved in file
    [filename]. *)
let test_aux filename configurator l =
  let result =
    List.for_all
      (fun test ->
         match test with
         | Bin (exec, opts) -> bin_available configurator (exec, opts)
         | Dep file -> dep_available file)
      l
  in
  let out = open_out filename in
  Printf.fprintf out "%B%s"
    result
    (if result then "" else " (Some tests are disabled)");
  close_out_noerr out;
  result

(** Helper to build [bin_available] arguments *)
let bin exec args = Bin (exec, args)

(** Helper to build [dep_available] arguments *)
let dep file = Dep file

(** {2 Checking availability of dependencies for test folders} *)

(** {3 Kernel tests}  *)

let tests_fc_scripts_deps_available configurator =
  let python = dep "python-3.10-available" in
  let clang = bin "clang" ["--version"] in
  let yq = bin "yq" ["--version"] in
  test_aux "tests-fc_scripts-deps-available" configurator
    [python; clang; yq]

let tests_jcdb_deps_available configurator =
  let python = dep "python-3.10-available" in
  test_aux  "tests-jcdb-deps-available" configurator
    [python]

let tests_libc_deps_available configurator =
  let gcc = bin "gcc" ["--version"] in
  let python = dep "python-3.10-available" in
  test_aux "tests-libc-deps-available" configurator
    [gcc; python]

let tests_metrics_deps_available configurator =
  let python = dep "python-3.10-available" in
  test_aux "tests-metrics-deps-available" configurator
    [python]

let tests_misc_deps_available configurator =
  let gcc = bin "gcc" ["--version"] in
  let socat = bin "socat" ["-V"] in
  test_aux "tests-misc-deps-available" configurator
    [gcc; socat]

let tests_mopsa_deps_available configurator =
  let mopsa_build = bin "mopsa-build" [] in
  let mopsa_db = bin "mopsa-db" [] in
  let cmake = bin "cmake" ["--version"] in
  test_aux "tests-mopsa-deps-available" configurator
    [mopsa_build; mopsa_db; cmake]

let tests_spec_deps_available configurator =
  let gcc = bin "gcc" ["--version"] in
  let unix2dos = bin "unix2dos" ["--version"] in
  test_aux "tests-spec-deps-available" configurator
    [gcc; unix2dos]

let tests_syntax_deps_available configurator =
  let clang = bin "clang" ["--version"] in
  let gcc = bin "gcc" ["--version"] in
  let genuine_gcc = dep "gcc-is-genuine" in
  let python = dep "python-3.10-available" in
  let has_c2x_option = dep "has-c2x-option" in
  let has_c2y_option = dep "has-c2y-option" in
  test_aux "tests-syntax-deps-available" configurator
    [clang; gcc; genuine_gcc; python; has_c2x_option; has_c2y_option]

(** {3 Plug-ins tests}  *)

let tests_eva_deps_available configurator =
  let dot = bin "dot" ["--version"] in
  let gcc = bin "gcc" ["--version"] in
  test_aux "tests-eva-deps-available" configurator
    [dot; gcc]

let tests_markdown_report_deps_available configurator =
  let check_jsonschema = bin "check-jsonschema" ["--version"] in
  let jq = bin "jq" ["--version"] in
  test_aux "tests-markdown-report-deps-available" configurator
    [check_jsonschema; jq]

let tests_server_deps_available configurator =
  (* No dependencies for now *)
  test_aux "tests-server-deps-available" configurator
    [ ]

let tests_deps_available configurator =
  let tests = [
    (* Kernel *)
    tests_fc_scripts_deps_available
  ; tests_jcdb_deps_available
  ; tests_libc_deps_available
  ; tests_metrics_deps_available
  ; tests_misc_deps_available
  ; tests_mopsa_deps_available
  ; tests_spec_deps_available
  ; tests_syntax_deps_available
  (* Plug-ins *)
  ; tests_eva_deps_available
  ; tests_markdown_report_deps_available
  ; tests_server_deps_available
  ] in
  (* Use fold instead of for_all so that every function in tests is called and
     *-deps-available files are generated *)
  List.fold_left
    (fun acc tests_deps -> tests_deps configurator && acc)
    true
    tests

let configure configurator =
  let dependencies_available = tests_deps_available configurator in
  let tests_deps = open_out "tests-dependencies-available" in
  Printf.fprintf tests_deps "%B" dependencies_available;
  close_out tests_deps;
  let tests_deps_comment = open_out "tests-dependencies-comment" in
  Printf.fprintf tests_deps_comment "%s"
    (if dependencies_available then "" else "(Some tests are disabled)");
  close_out_noerr tests_deps_comment

let () =
  C.main ~name:"frama_c_tests_config" configure

