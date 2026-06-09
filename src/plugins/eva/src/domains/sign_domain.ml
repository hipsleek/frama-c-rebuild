(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Sign_Value = struct
  include Sign_value

  (* In this domain, we only track integer variables. *)
  let track_variable vi = Ast_types.is_integral vi.vtype

  (* The base lattice is finite, we can use join to perform widening *)
  let widen _ = join

  let builtins = []
end

module Name = struct let name = "sign" end
module Domain = Simple_memory.Make_Domain (Name) (Sign_Value)
include Domain

let registered =
  let name = "sign"
  and descr = "Infers the sign of program variables." in
  Abstractions.Domain.register ~name ~descr ~priority:4 (module Domain)
