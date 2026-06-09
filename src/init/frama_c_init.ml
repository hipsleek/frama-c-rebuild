(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let () =
  let gc_params = Gc.get () in
  Gc.set
    { gc_params with
      Gc.minor_heap_size = 1 lsl 18 ;
      major_heap_increment = 1 lsl 22;
      (* space_overhead = 40 ; max_overhead = 100 *)
    }

let () = Printexc.record_backtrace true
