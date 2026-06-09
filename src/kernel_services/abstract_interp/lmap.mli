(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Maps from bases to memory maps. The memory maps are those of the
    [Offsetmap] module.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(** Contents of a variable when it is not present in the state.
    See function [default_contents] in the signature below *)
type 'a default_contents =
  | Bottom
  | Top of 'a
  | Constant of 'a
  | Other

module type Default_offsetmap = sig
  type v
  type offsetmap

  val name: string
  (** Used to create different datatypes each time the functor is applied *)

  val default_offsetmap : Base.t -> offsetmap Lattice_bounds.or_bottom
  (** Value returned when a map is queried, and the base is not present.
      [`Bottom] indicates that the base is never bound in such a map. *)

  val default_contents: v default_contents
  (** This function is used to optimize functions that add keys in a map,
      in particular when maintaining canonicity w.r.t. default contents.
      It describes the contents [c] of the offsetmap
      resulting from [default_offsetmap b]. The possible values are:
      - [Bottom] means that [c] is [V.bottom] everywhere, and furthermore
        that [V.bottom] has an empty concretization. We deduce from this
        fact that unmapped keys do not contribute to a join, and that
        [join c v] is never [c] as soon as [v] is not itself [v].
      - [Top] means that [c] is [V.top] everywhere. Thus unmapped keys
        have a default value more general than the one in a map where the
        key is bound.
      - [`Constant v] means that [c] is an offsetmap with a single interval
        containing [v] everywhere. [v] must be isotropic (in the sense
        of {!V.is_isotropic}).
      - [`Other] means that [default_offsetmap] returns something arbitrary.

      This function is only used on keys that change values. Thus it is
      safe to have [default_offsetmap] return something that do not
      match [default_contents] on constant keys.
  *)
end

module Make_LOffset
    (V: sig
       include Offsetmap_lattice_with_isotropy.S
       include Lattice_type.With_Top_Opt with type t := t
     end)
    (Offsetmap: Offsetmap_sig.S with type v = V.t
                                 and type widen_hint = V.widen_hint)
    (_: Default_offsetmap with type v := V.t
                           and type offsetmap := Offsetmap.t):
  Lmap_sig.S
  with type v = V.t
   and type widen_hint_base = V.widen_hint
   and type offsetmap = Offsetmap.t
