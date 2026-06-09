(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Callgraph_api.S

module Graphviz_attributes: Graph.Graphviz.GraphWithDotAttrs
  with type t = G.t
   and type V.t = Kernel_function.t
   and type E.t = G.E.t
