(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types

module Lhost : Datatype.S_with_collections with type t = lhost
module Offset : Datatype.S_with_collections with type t = offset
module Lval : Datatype.S_with_collections with type t = lval
module Exp : Datatype.S_with_collections with type t = exp
module Constant : Datatype.S_with_collections with type t = constant
