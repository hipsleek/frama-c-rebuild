(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Side Conditions Generator                                         --- *)
(* -------------------------------------------------------------------------- *)

open Memory
open Cil_types
open Condition

type addr = LV of lval | TLV of term_lval | ADDR of exp | TADDR of term
type value = E of exp | T of term
type guard =
  | Bounds of value * Z.t
  | Non_null of addr
  | Valid of addr
  | Valid_read of addr
  | Valid_pointer of addr
  | Valid_region of node * addr
  | Initialized of addr
  | Aligned of addr * typ

type condition =
  | Forall of quantifiers * condition
  | Hyp of predicate * condition
  | Let of logic_info * condition
  | At of condition * logic_label
  | Guard of guard

val pp_addr  : Format.formatter -> addr  -> unit
val pp_value : Format.formatter -> value -> unit
val pp_guard : Format.formatter -> guard -> unit
val pp_condition : Format.formatter -> condition -> unit

val of_value : value -> term
val of_addr  : ?loc:location -> addr -> term
val of_guard : ?loc:location -> ?names:string list -> guard -> predicate
val of_condition : ?loc:location -> ?names:string list -> condition -> predicate

val kind : addr -> lkind
val pointed : addr -> typ

val guards : kernel_function -> map ->
  (names:string list -> invalid:bool -> condition -> unit) ->
  stmt -> unit

val add_annotation :
  ?kf:kernel_function ->
  ?emitter:Emitter.t ->
  ?names:string list ->
  ?invalid:bool ->
  ?hyps:Property.t list ->
  stmt -> condition -> unit

val is_annotated : kernel_function -> bool
val set_annotated : kernel_function -> unit
val annotate : kernel_function -> unit
