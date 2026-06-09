(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Retres =
  Kernel_function.Make_Table
    (Datatype.Option(Cil_datatype.Varinfo))
    (struct
      let name = "Value.Library_functions.Retres"
      let size = 9
      let dependencies = [Ast.self]
    end)
let () = Ast.add_monotonic_state Retres.self

let () =
  State_dependency_graph.add_dependencies ~from:Retres.self [ Self.state ]

let get_retres_vi = Retres.memo
    (fun kf ->
       let vi = Kernel_function.get_vi kf in
       let typ = Cil.getReturnType vi.vtype in
       if Ast_types.is_void typ then
         None
       else
         try
           ignore (Cil.bitsSizeOf typ);
           let name = Format.asprintf "\\result<%a>" Kernel_function.pretty kf in
           Some (Cil.makeVarinfo false false name typ)
         with Cil.SizeOfError _ ->
           Self.abort ~current:true
             "function %a returns a value of unknown size. Aborting"
             Kernel_function.pretty kf
    )

let returned_value kf =
  let return_type = Ast_types.unroll (Kernel_function.get_return_type kf) in
  match return_type.tnode with
  | TComp _ when Cil.is_fully_arithmetic return_type -> Cvalue.V.top_int
  | TPtr _ | TComp _ -> Cvalue.V.inject Base.null Ival.zero
  | TInt _ | TEnum _ ->  Cvalue.V.top_int
  | TFloat (FFloat  | FFloat32) -> Cvalue.V.top_single_precision_float
  | TFloat (FDouble | FFloat64 | FLongDouble) -> Cvalue.V.top_float
  | TBuiltin_va_list ->
    Self.error ~current:true ~once:true ~stacktrace:true
      "functions returning variadic arguments must be stubbed";
    Cvalue.V.top_int
  | TVoid -> Cvalue.V.top (* this value will never be used *)
  | TFun _ | TNamed _ | TArray _ -> assert false


let unsupported_specifications =
  [
    "asprintf", "stdio.c";
    "canonicalize_path_name", "stdlib.c";
    "fmemopen", "stdio.c";
    "getaddrinfo", "netdb.c";
    "getenv", "stdlib.c";
    "getline", "stdio.c";
    "getpwnam_r", "pwd.c";
    "getpwuid_r", "pwd.c";
    "glob", "glob.c";
    "globfree", "glob.c";
    "posix_memalign", "stdlib.c";
    "putenv", "stdlib.c";
    "realpath", "stdlib.c";
    "setenv", "stdlib.c";
    "strdup", "string.c";
    "strerror", "string.c";
    "strndup", "string.c";
    "unsetenv", "stdlib.c";
    "vasprintf", "stdio.c";
    "vfscanf", "stdio.c";
    "vfwscanf", "wchar.c";
    "vscanf", "stdio.c";
    "vwscanf", "wchar.c";
    "wcsdup", "wchar.c";
    "raise", "signal.c";
    "rawmemchr", "string.c";
  ]

let unsupported_specs_tbl =
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun (name, file) -> Hashtbl.replace tbl name file)
    unsupported_specifications;
  tbl

let warn_unsupported_spec name =
  try
    let header = Hashtbl.find unsupported_specs_tbl name in
    Self.warning ~once:true ~current:true
      ~wkey:Self.wkey_libc_unsupported_spec
      "@[The specification of function '%a' is currently not supported by Eva.@ \
       Consider adding '%a'@ to the analyzed source files.@]"
      Printer.pp_varname name Filepath.pretty
      Filepath.(System_config.Share.libc / header)
  with Not_found -> ()
