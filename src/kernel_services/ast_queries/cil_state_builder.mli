(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Functors for building computations which use kernel datatypes.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

module Stmt_set_ref(_: State_builder.Info) :
  State_builder.Set_ref with type elt = Cil_types.stmt

module Kinstr_hashtbl(Data:Datatype.S)(_: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.kinstr and type data = Data.t

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Stmt_hashtbl(Data:Datatype.S)(_: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.stmt and type data = Data.t

module Varinfo_hashtbl(Data:Datatype.S)(_: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.varinfo
                         and type data = Data.t

module Exp_hashtbl(Data:Datatype.S)(_: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.exp
                         and type data = Data.t

module Lval_hashtbl(Data:Datatype.S)(_: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.lval
                         and type data = Data.t

module Kernel_function_hashtbl
    (Data:Datatype.S)(_: State_builder.Info_with_size):
  State_builder.Hashtbl with type key = Cil_types.kernel_function
                         and type data = Data.t

(*
module Code_annotation_hashtbl
  (Data:Project.Datatype.S)(Info:State_builder.Info_with_size) :
  State_builder.Hashtbl
  with type key = Cil_types.code_annotation and type data = Data.t
 *)
