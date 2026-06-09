(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Pretty printers of unicode symbols.
    Each function in this module prints a single unicode symbol, or an
    ASCII-based replacement if -no-unicode option is set.
    @since 32.0-Germanium *)

(** This function can be used to turn on or off the use of unicode UTF-8
    characters in messages.
*)
val use_unicode : bool -> unit

type printer = Format.formatter -> unit

(** Set operations. *)

val pp_in_set : printer (** ∈ *)
val pp_empty_set : printer (** ∅ *)
val pp_top : printer (** ⊤ *)
val pp_bottom : printer (** ⊥ *)
val pp_union : printer (** ∪ *)

(* Relations. *)

val pp_le : printer (** ≤ *)
val pp_ge : printer (** ≥ *)
val pp_eq : printer (** ≡ *)
val pp_neq : printer (** ≢ *)

(* Logic operators. *)

val pp_not : printer (** ¬ *)
val pp_and : printer (** ∧ *)
val pp_or : printer (** ∨ *)
val pp_xor : printer (** ⊻ *)

val pp_implies : printer (** ⇒ *)
val pp_iff : printer (** ⇔ *)

val pp_in_acsl : printer (** ∈ *)
val pp_forall : printer (** ∀ *)
val pp_exists : printer (** ∃ *)

(* Logic types. *)

val pp_boolean : printer (** 𝔹 *)
val pp_integer : printer (** ℤ *)
val pp_real : printer (** ℝ *)

(* Greek letters. *)

val pp_alpha : printer (** α *)
val pp_pi : printer (** π *)
val pp_lambda : printer (** λ *)
val pp_mu : printer (** µ *)
val pp_theta : printer (** θ *)

module Capital : sig
  val pp_theta : printer (** Θ *)
end

(* Superscript/subscript *)

val pp_super_int : Format.formatter -> int -> unit
val pp_sub_int : Format.formatter -> int -> unit

(* Other symbols. *)

val pp_right_arrow : printer (** → *)
val pp_maps_to : printer (** ↦ @since 33.0-Arsenic *)

val pp_plus_minus : printer (** ± *)
val pp_times : printer (** × *)
val pp_multiplication_dot : printer (** ⋅ *)

val pp_ellipsis : printer (** … *)

val pp_floor : 'a Pretty_utils.formatter -> 'a Pretty_utils.formatter (** ⌊elt⌋ *)
val pp_ceil : 'a Pretty_utils.formatter -> 'a Pretty_utils.formatter (** ⌈elt⌉ *)

(* Complete strings *)

(** [pp_string fmt s] pretty prints a string [s] as {!Format.pp_print_string},
    but each unicode character is counted as one character for Format line
    splitting policies. This avoids differences with OCaml < 5.4. *)
val pp_string : Format.formatter -> string -> unit
