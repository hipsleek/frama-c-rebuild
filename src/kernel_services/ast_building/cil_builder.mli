(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module is meant to build C or ACSL expressions in a unified way.
    Compared to "classic" Cil functions it also avoids the necessity to provide
    a location everywhere.

    Some naming conventions:
    - "prime" types (suffixed with `'`) are a "low-level" representation, "hiding" constructors
      so that the corresponding unprimed types can be used directly, improving readability;
    - `harden` functions perform an "unboxing" of the polymorphic variant constructor.
*)

module Type :
sig
  exception NotACType

  type empty
  type ('value,'shape) typ

  (* Logic types *)
  val of_ltyp : Cil_types.logic_type -> (unit,unit) typ
  val integer : (unit,unit) typ
  val real : (unit,unit) typ

  (* C base types *)
  val of_ctyp : Cil_types.typ -> ('v,'v) typ
  val void : ('v,'v) typ
  val bool : ('v,'v) typ
  val char : ('v,'v) typ
  val schar : ('v,'v) typ
  val uchar : ('v,'v) typ
  val int : ('v,'v) typ
  val uint : ('v,'v) typ
  val short : ('v,'v) typ
  val ushort : ('v,'v) typ
  val long : ('v,'v) typ
  val ulong : ('v,'v) typ
  val longlong : ('v,'v) typ
  val ulonglong : ('v,'v) typ
  val float : ('v,'v) typ
  val double : ('v,'v) typ
  val longdouble : ('v,'v) typ

  val ptr : ('v,'s) typ -> ('v,'v) typ
  val array : ?size:int -> ('v,'s) typ -> ('v,'s list) typ
  val structure :
    Cil_types.compinfo -> (Cil_types.fieldinfo -> 'a -> 'v) -> ('v, 'a) typ

  module ArgList : sig
    type t =
      | [] : t
      | Variadic : t
      | (::) : (string * ('v,'s) typ * Cil_types.attributes) * t -> t
  end

  val proto: ('vr, 'sr) typ -> ArgList.t -> (empty, empty) typ

  (* Attributes *)
  val attribute : ('v,'s) typ -> string -> Cil_types.attrparam list
    -> ('v,'s) typ
  val const : ('v,'s) typ -> ('v,'s) typ
  val stdlib_generated : ('v,'s) typ -> ('v,'s) typ

  (* Conversion *)
  val cil_typ : ('v,'s) typ -> Cil_types.typ
  val cil_logic_type : ('v,'s) typ -> Cil_types.logic_type
end


(* --- C & Logic expressions builder --- *)

module Exp :
sig
  include module type of Type

  type const'
  type var'
  type lhost'
  type lval'
  type exp'
  type init'

  type label
  type const = [ `const of const' ]
  type var = [ `var of var' ]
  type lhost = [ var | `lhost of lhost' ]
  type lval = [ lhost | `lval of lval' ]
  type exp = [ const | lval | `exp of exp' ]
  type init = [ exp | `init of init']

  val pretty : Format.formatter -> [init | `none] -> unit

  val none : [> `none]

  (* Labels *)

  val here : label
  val old : label
  val pre : label
  val post : label
  val loop_entry : label
  val loop_current : label
  val program_init : label

  (* Constants *)

  (** Implicitly converted to type [int] when converted into C constant *)
  val of_int : int -> [> const]

  (** Implicitly converted to type [int] when converted into C constant *)
  val of_integer : Z.t -> [> const]

  (** Default kind is [int]. Value is truncated if necessary. *)
  val of_cint :
    ?kind:Cil_types.ikind -> Z.t -> [> const]

  (** Default kind is [double].
      Value is rounded to simple precision if necessary. *)
  val of_cfloat :
    ?kind:Cil_types.fkind -> float -> [> const]

  val of_constant : Cil_types.constant -> [> const]
  val zero : [> const]
  val one : [> const]

  (* Lvalues *)

  val var : Cil_types.varinfo -> [> var]
  val of_lhost: Cil_types.lhost -> [> lhost]
  val of_lval : Cil_types.lval -> [> lval]

  (* Expressions *)

  exception EmptyList

  val of_exp : Cil_types.exp -> [> exp]
  val of_exp_copy : Cil_types.exp -> [> exp]
  val of_exp_list : Cil_types.exp list -> [> exp] list
  val unop : Cil_types.unop -> [< exp] -> [> exp]
  val neg : [< exp] -> [> exp]
  val lognot : [< exp] -> [> exp]
  val bwnot : [< exp] -> [> exp]
  val succ : [< exp] -> [> exp] (* e + 1 *)
  val add_int : [< exp] -> int -> [> exp] (* e + i *)
  val binop : Cil_types.binop -> [< exp] -> [< exp] -> [> exp]
  val add : [< exp] -> [< exp] -> [> exp]
  val sub : [< exp] -> [< exp] -> [> exp]
  val mul : [< exp] -> [< exp] -> [> exp]
  val div : [< exp] -> [< exp] -> [> exp]
  val modulo : [< exp] -> [< exp] -> [> exp]
  val shiftl : [< exp] -> [< exp] -> [> exp]
  val shiftr : [< exp] -> [< exp] -> [> exp]
  val lt : [< exp] -> [< exp] -> [> exp]
  val gt : [< exp] -> [< exp] -> [> exp]
  val le : [< exp] -> [< exp] -> [> exp]
  val ge : [< exp] -> [< exp] -> [> exp]
  val eq : [< exp] -> [< exp] -> [> exp]
  val ne : [< exp] -> [< exp] -> [> exp]
  val logor : [< exp] -> [< exp] -> [> exp]
  val logand : [< exp] -> [< exp] -> [> exp]
  val logor_list : [< exp] list -> exp
  val logand_list : [< exp] list -> exp
  val bwand : [< exp] -> [< exp] -> [> exp]
  val bwor : [< exp] -> [< exp] -> [> exp]
  val bwxor : [< exp] -> [< exp] -> [> exp]
  val cast : ('v,'s) typ -> [< exp] -> [> exp]
  val cast' : Cil_types.typ -> [< exp] -> [> exp]
  val addr : [< lval] -> [> exp]
  val mem : [< exp] -> [> lval]
  val index : [< lval] -> [< exp] -> [> lval]
  val field : [< lval] -> Cil_types.fieldinfo -> [> lval]
  val fieldnamed : [< lval] -> string -> [> lval]

  val result : [> lval]
  val term : Cil_types.term -> [> exp]
  val range :  [< exp | `none] -> [< exp | `none] -> [> exp]
  val whole : [> exp] (* Whole range, i.e. .. *)
  val whole_right : [> exp] (* Whole range right side, i.e. 0.. *)
  val app : Cil_types.logic_info -> label list -> [< exp] list -> [> exp]

  val object_pointer : ?at:label -> [< exp] -> [> exp]
  val valid : ?at:label -> [< exp] -> [> exp]
  val valid_read : ?at:label -> [< exp] -> [> exp]
  val initialized : ?at:label -> [< exp] -> [> exp]
  val dangling : ?at:label -> [< exp] -> [> exp]
  val allocable : ?at:label -> [< exp] -> [> exp]
  val freeable : ?at:label -> [< exp] -> [> exp]
  val fresh : label -> label -> [< exp] -> [< exp] -> [> exp]

  val of_init : Cil_types.init -> [> init]
  val compound : Cil_types.typ -> init list -> [> init]
  val values : (init,'values) typ -> 'values -> init

  (* Redefined operators *)

  val (+) : [< exp] -> [< exp] -> [> exp]
  val (-) : [< exp] -> [< exp] -> [> exp]
  val ( * ) : [< exp] -> [< exp] -> [> exp]
  val (/) : [< exp] -> [< exp] -> [> exp]
  val (%) : [< exp] -> [< exp] -> [> exp]
  val (<<) : [< exp] -> [< exp] -> [> exp]
  val (>>) : [< exp] -> [< exp] -> [> exp]
  val (<) : [< exp] -> [< exp] -> [> exp]
  val (>) : [< exp] -> [< exp] -> [> exp]
  val (<=) : [< exp] -> [< exp] -> [> exp]
  val (>=) : [< exp] -> [< exp] -> [> exp]
  val (==) : [< exp] -> [< exp] -> [> exp]
  val (!=) : [< exp] -> [< exp] -> [> exp]
  val (--) : [< exp] -> [< exp] -> [> exp]
  val (.@[]) : [< lval] -> [< exp] -> [> lval] (* C index operator [] *)

  (* Export CIL objects from built expressions *)

  exception LogicInC of exp
  exception CInLogic of exp
  exception NotATerm of exp
  exception NotAPredicate of exp
  exception NotAFunction of Cil_types.logic_info
  exception Typing_error of string
  exception OutOfScope of string

  val cil_logic_label : label -> Cil_types.logic_label
  val cil_constant : [< const] -> Cil_types.constant
  val cil_varinfo : [< var] -> Cil_types.varinfo
  val cil_lhost: loc:Cil_types.location -> [< lhost] -> Cil_types.lhost
  val cil_lval : loc:Cil_types.location -> [< lval] -> Cil_types.lval
  val cil_exp : loc:Cil_types.location -> [< exp] -> Cil_types.exp
  val cil_term_lval : loc:Cil_types.location -> ?restyp:Cil_types.typ ->
    [< lval] -> Cil_types.term_lval
  val cil_term_lhost: loc:Cil_types.location -> ?restyp: Cil_types.typ ->
    [< lhost] -> Cil_types.term_lhost
  val cil_term : loc:Cil_types.location -> ?restyp:Cil_types.typ ->
    [< exp] -> Cil_types.term
  val cil_iterm : loc:Cil_types.location -> ?restyp:Cil_types.typ ->
    [< exp] -> Cil_types.identified_term
  val cil_pred : loc:Cil_types.location -> ?restyp:Cil_types.typ ->
    [< exp] -> Cil_types.predicate
  val cil_ipred : loc:Cil_types.location -> ?restyp:Cil_types.typ ->
    [< exp] -> Cil_types.identified_predicate
  val cil_init : loc:Cil_types.location -> [< init] -> Cil_types.init

  val cil_typeof : [< var] -> Cil_types.typ
end


(* --- Pure builder --- *)

module Pure :
sig
  include module type of Exp
    with type ('v,'s) typ = ('v,'s) Type.typ
     and type const' = Exp.const'
     and type var' = Exp.var'
     and type lhost' = Exp.lhost'
     and type lval' = Exp.lval'
     and type exp' = Exp.exp'
     and type init' = Exp.init'
     and type label = Exp.label

  type call'
  type instr'
  type stmt'

  type call = [ `call of call' ]
  type instr = [ call | `instr of instr' ]
  type stmt = [ instr | `stmt of stmt' ]

  (* Instructions *)
  val of_instr : Cil_types.instr -> [> instr]
  val skip : [> instr]
  val assign : [< lval] -> [< exp | call] -> [> instr]
  val incr : [< lval] -> [> instr]
  val call : Cil_types.kernel_function -> [< exp] list -> [> call]
  val call_ptr : [< lhost] -> [< exp] list -> [> call]

  val local : ?ghost:bool -> ?init:'v -> (init,'v) typ -> string ->
    [> var] * [> instr]
  val local' : ?ghost:bool -> ?init:init -> Cil_types.typ -> string ->
    [> var] * [> instr]
  val local_copy : ?ghost:bool -> ?suffix:string -> [< var] ->
    [> var] * [> instr]

  (* Statements *)
  val of_stmtkind : Cil_types.stmtkind -> [> stmt]
  val of_stmt : Cil_types.stmt -> [> stmt]
  val of_stmts : Cil_types.stmt list -> [> stmt]
  val block : [< stmt] list -> [> stmt]
  val sequence : [< stmt] list -> [> stmt] (* does not generate block *)
  val ghost : [< stmt] -> [> stmt]
  val if_ : ?ghost_else:bool ->
    [< exp] -> then_:[< stmt] list -> else_:[< stmt] list -> [> stmt]

  (* Conversion to Cil *)

  (* for the three following function into is mandatory if the built ast
     contains locals declarations *)
  val cil_instr : ?into:Cil_types.fundec ->
    loc:Cil_types.location -> instr -> Cil_types.instr
  val cil_stmtkind : ?into:Cil_types.fundec ->
    loc:Cil_types.location -> stmt -> Cil_types.stmtkind
  val cil_stmt : ?into:Cil_types.fundec ->
    loc:Cil_types.location -> stmt -> Cil_types.stmt

  (* Operators *)
  val (let+) : var * stmt -> (var -> stmt list) -> stmt list
  val (and+) : var -> var -> var * var

  val (:=) : [< lval] -> [< exp | call] -> [> instr] (* assign *)
  val (+=) : [< lval] -> [< exp] -> [> instr]
  val (-=) : [< lval] -> [< exp] -> [> instr]
end


(* --- Stateful builder --- *)

exception BuildError of string

module Stateful () :
sig
  include module type of Exp
    with type ('v,'s) typ = ('v,'s) Type.typ
     and type const' = Exp.const'
     and type var' = Exp.var'
     and type lhost' = Exp.lhost'
     and type lval' = Exp.lval'
     and type exp' = Exp.exp'
     and type init' = Exp.init'
     and type label = Exp.label

  (* Functions *)
  val open_function :
    ?loc:Cil_types.location -> ?ghost:bool -> ?vorig_name:string ->
    string -> [> var]
  val set_return_type : ('s,'v) typ -> unit
  val set_return_type' : Cil_types.typ -> unit
  val add_attribute : Cil_types.attribute -> unit
  val add_new_attribute : string -> Cil_types.attrparam list -> unit
  val add_stdlib_generated : unit -> unit
  val finish_function : ?register:bool -> unit -> Cil_types.global
  val finish_declaration : ?register:bool -> unit -> Cil_types.global

  (* Behaviors *)
  type source = [exp | `indirect of exp]
  val indirect: [< source] -> [> source]
  val assigns: [< exp] list -> [< exp | `indirect of [< exp]] list -> unit
  val infer_assigns: unit -> unit (* Uses Infer_assigns.from_prototype *)
  val requires: [< exp] -> unit
  val ensures: [< exp] -> unit

  (* Statements *)
  val of_stmtkind : Cil_types.stmtkind -> unit
  val of_stmt : Cil_types.stmt -> unit
  val of_stmts : Cil_types.stmt list -> unit
  val open_block :
    ?loc:Cil_types.location -> ?into:Cil_types.fundec -> ?ghost:bool ->
    unit -> unit
  val open_ghost :
    ?loc:Cil_types.location -> ?into:Cil_types.fundec ->
    unit -> unit
  val open_switch :
    ?loc:Cil_types.location -> ?into:Cil_types.fundec ->
    [< exp] -> unit
  val open_if :
    ?loc:Cil_types.location -> ?into:Cil_types.fundec ->
    [< exp] -> unit
  val open_else : ?ghost:bool -> unit -> unit
  val close : unit -> unit
  val finish_block : unit -> Cil_types.block
  val finish_instr_list : ?scope:Cil_types.block -> unit -> Cil_types.instr list
  val finish_stmt : unit -> Cil_types.stmt
  val case : [< exp] -> unit
  val break : unit -> unit
  val return : [< exp | `none] -> unit

  (* Variables *)
  val local : ?ghost:bool -> ?init:'v -> (init,'v) typ -> string -> [> var]
  val local' : ?ghost:bool -> ?init:init -> Cil_types.typ -> string -> [> var]
  val local_copy : ?ghost:bool -> ?suffix:string -> [< var] -> [> var]
  val parameter : ?ghost:bool -> ?attributes:Cil_types.attributes ->
    Cil_types.typ -> string -> [> var]

  (* Instructions *)
  val of_instr : Cil_types.instr -> unit
  val skip : unit -> unit
  val assign : [< lval] -> [< exp] -> unit
  val incr : [< lval] -> unit
  val call : [< lval | `none] -> Cil_types.kernel_function -> [< exp] list ->
    unit
  val call_ptr : [< lval | `none] -> [< lhost] -> [< exp] list -> unit
  val pure : [< exp ] -> unit

  (* Operators *)
  val (:=) : [< lval] -> [< exp] -> unit (* assign *)
  val (+=) : [< lval] -> [< exp] -> unit
  val (-=) : [< lval] -> [< exp] -> unit
end
