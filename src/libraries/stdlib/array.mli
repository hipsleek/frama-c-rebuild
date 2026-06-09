(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extension of OCaml's {!Stdlib.Array} module.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since 33.0-Arsenic *)

include module type of Stdlib.Array

(** Same as {!Stdlib.Array.equal} but made available here until the
    minimal supported version is OCaml 5.4. *)
val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Same as {!Stdlib.Array.compare} but made available here until the
    minimal supported version is OCaml 5.4. *)
val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int

(** Compute a hash for the set given a hash for the elements. *)
val hash : ('a -> int) -> 'a t -> int

(** Pretty prints an array given a printer for the elements.
    @param format defaults to "[| %t |]"
    @param item defaults to "%a"
    @param sep defaults to ";@ "
    @param last defaults to [sep]
    @param empty defaults to "[||]" *)
val pretty :
  ?format:(Pretty.tformatter -> unit) Pretty.format ->
  ?item:('a Pretty.aformatter -> 'a -> unit) Pretty.format ->
  ?sep:unit Pretty.format ->
  ?last:unit Pretty.format ->
  ?empty:unit Pretty.format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit
