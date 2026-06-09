(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extension of OCaml's {!Stdlib.Maps} module.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since 33.0-Arsenic
*)

include module type of Stdlib.Map

(** Extension of {!Stdlib.Map.S}. *)
module type S = sig
  include S

  (** Pretty prints a map given a printer for the keys and one for the values.
      @param format defaults to "{{ %t }}"
      @param item defaults to "%a -> %a"
      @param sep defaults to ";@ "
      @param last defaults to [sep]
      @param empty defaults to "{{}}" *)
  val pretty :
    ?format:(Pretty.tformatter -> unit) Pretty.format ->
    ?item:(key Pretty.aformatter -> key -> 'a Pretty.aformatter -> 'a -> unit)
        Pretty.format ->
    ?sep:unit Pretty.format ->
    ?last:unit Pretty.format ->
    ?empty:unit Pretty.format ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit

  (** Same as [union f] but when [f] always returns [Some]. *)
  val closed_union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

module Make (Ord: OrderedType) : S with type key = Ord.t
