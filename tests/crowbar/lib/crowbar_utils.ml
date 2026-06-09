let () = Kernel.AutoLoadPlugins.off ()
let () = Dynamic.load_module "frama-c-eva"

let dirname = Filename.dirname Sys.executable_name
let cases_dir = "failed_cases"
let fcases_dir = Filepath.of_string cases_dir

let () =
  Filesystem.make_dir ~perm:0o755 fcases_dir

let () =
  match
    Filesystem.with_open_out (Filepath.concat fcases_dir ".empty") ignore
  with
  | Ok () -> ()
  | Error (msg,_) ->
    Format.printf "Error creating file in failed_cases directory: %s@." msg

let filepath =
  let count = ref 0 in
  fun name ->
    incr count;
    let id = string_of_int !count in
    let name =
      dirname ^ "/"
      ^ cases_dir ^ "/"
      ^ name ^ id ^ ".i"
    in
    Filepath.of_string name

let generate_cil_file name =
  let path = filepath name in
  Cil_types.{
    fileName = path;
    globals = [];
    globinit = None;
    globinitcalled = false
  }

let generate_file file =
  let open Filesystem.Operators in
  let filepath = file.Cil_types.fileName in
  Kernel.add_debug_keys Kernel.dkey_print_attrs;
  let result =
    let+ channel = Filesystem.with_open_out filepath in
    match file.globals with
    | [] -> ()
    | _ ->
      let fmt = Format.formatter_of_out_channel channel in
      Format.fprintf fmt "%a@." Printer.pp_file file
  in
  match result with
  | Ok () -> ()
  | Error (msg, file) ->
    Format.printf "error writing to file %a: %s"
      Filepath.pretty file
      msg

let run s f =
  Format.printf "Running Crowbar tests on %s@." s;
  f ()
