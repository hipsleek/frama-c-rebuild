(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(* cabsvisit.mli *)
(* interface for cabsvisit.ml *)

type nameKind =
    NVar                                (** Variable or function prototype
                                            name *)
  | NFun                                (** Function definition name *)
  | NField                              (** The name of a field *)
  | NType                               (** The name of a type *)


(* All visit methods are called in preorder! (but you can use
   ChangeDoChildrenPost to change the order) *)
class type cabsVisitor = object
  method vexpr: Cabs.expression -> Cabs.expression Cil.visitAction   (* expressions *)
  method vinitexpr: Cabs.init_expression -> Cabs.init_expression Cil.visitAction
  method vstmt: Cabs.statement -> Cabs.statement list Cil.visitAction
  method vblock: Cabs.block -> Cabs.block Cil.visitAction
  method vvar: string -> string                  (* use of a variable names *)
  method vdef: Cabs.definition -> Cabs.definition list Cil.visitAction
  method vtypespec: Cabs.typeSpecifier -> Cabs.typeSpecifier Cil.visitAction
  method vdecltype: Cabs.decl_type -> Cabs.decl_type Cil.visitAction

  (* For each declaration we call vname *)
  method vname: nameKind -> Cabs.specifier -> Cabs.name -> Cabs.name Cil.visitAction
  method vspec: Cabs.specifier -> Cabs.specifier Cil.visitAction     (* specifier *)
  method vattr: Cabs.attribute -> Cabs.attribute list Cil.visitAction


  method vEnterScope: unit -> unit
  method vExitScope: unit -> unit
end


class nopCabsVisitor: cabsVisitor


val visitCabsTypeSpecifier: cabsVisitor ->
  Cabs.typeSpecifier -> Cabs.typeSpecifier
val visitCabsSpecifier: cabsVisitor -> Cabs.specifier -> Cabs.specifier

(** Visits a decl_type. The bool argument is saying whether we are in a
    function definition and thus the scope in a PROTO should extend until the
    end of the function *)
val visitCabsDeclType: cabsVisitor -> bool -> Cabs.decl_type -> Cabs.decl_type
val visitCabsDefinition: cabsVisitor -> Cabs.definition -> Cabs.definition list
val visitCabsBlock: cabsVisitor -> Cabs.block -> Cabs.block
val visitCabsStatement: cabsVisitor -> Cabs.statement -> Cabs.statement list
val visitCabsExpression: cabsVisitor -> Cabs.expression -> Cabs.expression
val visitCabsAttributes: cabsVisitor -> Cabs.attribute list
  -> Cabs.attribute list
val visitCabsName: cabsVisitor -> nameKind
  -> Cabs.specifier -> Cabs.name -> Cabs.name
val visitCabsFile: cabsVisitor -> Cabs.file -> Cabs.file


(*
(** Set by the visitor to the current location *)
val visitorLocation: Cabs.cabsloc ref
*)
