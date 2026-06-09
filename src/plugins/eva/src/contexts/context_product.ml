(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Make (L : Abstract_context.S) (R : Abstract_context.S) = struct
  type t = L.t * R.t
  let top = (L.top, R.top)
  let narrow (l, r) (l', r') =
    Lattice_bounds.Bottom.product (L.narrow l l') (R.narrow r r')
end
