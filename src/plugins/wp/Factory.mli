(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Model Factory                                                      --- *)
(* -------------------------------------------------------------------------- *)

type mheap = Hoare | ZeroAlias | Eva | Bytes | Region | Typed of MemTyped.pointer
type mvar = Raw | Var | Ref | Caveat

type setup = {
  mvar : mvar ;
  mheap : mheap ;
  cint : Cint.model ;
  cfloat : Cfloat.model ;
}

type driver = LogicBuiltins.driver

val ident : setup -> string
val descr : setup -> string
val compiler : mheap -> mvar -> (module Memory.Compiler)
val configure_driver : setup -> driver -> unit -> WpContext.rollback
val instance : setup -> driver -> WpContext.model
val default : setup (** ["Var,Typed,Nat,Real"] memory model. *)

val parse :
  ?default:setup ->
  ?warning:(string -> unit) ->
  string list -> setup
(**
   Apply specifications to default setup.
   Default setup is [Factory.default].
   Default warning is [Wp_parameters.abort]. *)

(* -------------------------------------------------------------------------- *)
