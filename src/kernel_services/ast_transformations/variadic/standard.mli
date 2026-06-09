(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

exception Translate_call_exn of Cil_types.varinfo

val fallback_fun_call :
  builder:Builder.t -> callee:Cil_types.varinfo ->
  Environment.t ->
  Va_types.variadic_function -> Cil_types.exp list -> unit

val aggregator_call :
  builder:Builder.t ->
  Va_types.aggregator ->
  Va_types.variadic_function -> Cil_types.exp list -> unit

val overloaded_call :
  builder:Builder.t ->
  Va_types.overload ->
  Va_types.variadic_function -> Cil_types.exp list -> unit

val format_fun_call :
  builder:Builder.t ->
  Environment.t ->
  Va_types.format_fun ->
  Va_types.variadic_function -> Cil_types.exp list -> unit
