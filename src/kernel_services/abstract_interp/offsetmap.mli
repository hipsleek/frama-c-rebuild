(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Maps from intervals to values. *)

module type Parameters = sig
  (** Should offsetmaps emit feedback messages when approximating the write of
      large memory locations? *)
  val approximation_feedback: bool
end

(** Maps from intervals to values. The documentation of the returned
    maps is in module {!Offsetmap_sig}. *)
module Make (V : Offsetmap_lattice_with_isotropy.S) (_: Parameters) :
  Offsetmap_sig.S with type v = V.t
                   and type widen_hint = V.widen_hint

(**/**)
(* Exported as Int_Intervals, do not use this module directly *)
module Int_Intervals: Int_Intervals_sig.S
(**/**)


(** Maps from intervals to simple values. The documentation of the returned
    maps is in module {!Offsetmap_bitwise_sig}. *)
module Make_bitwise(V: sig
    include Lattice_type.Join_Semi_Lattice
    include Lattice_type.With_Top with type t := t
  end) :
  Offsetmap_bitwise_sig.S
  with type v = V.t
   and type intervals = Int_Intervals.t


(**/**)

(* This is automatically set by the Value plugin. Do not modify. *)
val set_plevel: int -> unit
val get_plevel: unit -> int
