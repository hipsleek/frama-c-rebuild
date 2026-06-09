(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let kf_of_varinfo (vi: Cil_types.varinfo) =
  if vi.vglob
  then fst (Globals.entry_point ())
  else fst (Globals.FileIndex.kernel_function_of_local_var_or_param_varinfo vi)

let aux find_zone vi =
  let kf = kf_of_varinfo vi in
  Memory_zone.mem_base (Base.of_varinfo vi) (find_zone kf)

let is_read = aux Inputs.get_internal
let is_written = aux Outputs.get_internal

let () =
  Server.Kernel_ast.register_var_filter "read"
    ~labels:("variables read (according to Eva analysis)",
             "variables never read (according to Eva analysis)")
    ~enable:Eva.Analysis.is_computed
    ~add_hook:(fun f -> Eva.Analysis.register_computation_hook (fun _ -> f ()))
    is_read

let () =
  Server.Kernel_ast.register_var_filter "written"
    ~labels:("variables written (according to Eva analysis)",
             "variables never written (according to Eva analysis)")
    ~enable:Eva.Analysis.is_computed
    ~add_hook:(fun f -> Eva.Analysis.register_computation_hook (fun _ -> f ()))
    is_written
