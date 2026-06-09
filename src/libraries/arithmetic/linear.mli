(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Nat
open Finite



(** Definition of a linear space over a field. *)

module Space (Field : Field.S) : sig

  (** The type of scalars in the field 𝕂. *)
  type scalar = Field.scalar

  (** The type of matrices in 𝕂ⁿˣᵐ *)
  type ('n, 'm) matrix

  (** Type representing a column vector of 𝕂ⁿ. One can use {!Matrix.transpose}
      if one needs a row vector, for example when printing it. *)
  type 'n vector = ('n, zero succ) matrix



  module Vector : sig

    val pretty : Format.formatter -> 'n succ vector -> unit

    (** The call [zero n] returns the 0 vector in 𝕂ⁿ. *)
    val zero   : 'n succ nat -> 'n succ vector

    (** Build a vector from an array. Raise out of bounds
        exceptions if the array is not well formed. *)
    val of_array : 'n succ nat -> string array -> 'n succ vector

    (** The call [repeat x n] returns a vector in 𝕂ⁿ which each dimension
        containing the scalar x. *)
    val repeat : scalar -> 'n succ nat -> 'n succ vector

    (** The call [base i n] returns the i-th base vector in the orthonormal
        space of 𝕂ⁿ. In other words, the returned vector contains zero except
        for the i-th coordinate, which contains one. *)
    val base   : 'n succ finite -> 'n succ nat -> 'n succ vector

    (** The call [get i m] returns the i-th coefficient. *)
    val get : 'n finite -> 'n vector -> scalar

    (** The call [set i x v] returns a new vector of the same linear space as
        [v], and with the same coordinates, except for the i-th one, which is
        set to the scalar [x]. *)
    val set    : 'n finite -> scalar -> 'n vector -> 'n vector

    (** The call [size v] for [v] a vector of 𝕂ⁿ returns n. *)
    val size   : 'n vector -> 'n nat

    (** The call [norm v] computes the ∞-norm of [v], i.e the maximum of the
        absolute values of its coordinates. *)
    val norm   : 'n vector -> scalar

    (** The call [max l r] returns a vector for which each coordinate is the
        maximum between the corresponding coordinates of [l] and [r]. *)
    val max : 'n vector -> 'n vector -> 'n vector

  end



  module Matrix : sig

    val pretty : Format.formatter -> ('n succ, 'm succ) matrix -> unit

    (** The call [id n] returns the identity matrix in 𝕂ⁿˣⁿ. *)
    val id : 'n succ nat -> ('n succ, 'n succ) matrix

    (** The call [zero n m] returns the 0 matrix in 𝕂ⁿˣᵐ. *)
    val zero : 'n succ nat -> 'm succ nat -> ('n succ, 'm succ) matrix

    (** Build a matrix from a 2 dimensional array of strings. Strings are
        used here to ensure that no rounding is performed prior of the
        ones that may be introduced by the underlying field.
        Raise out of bounds exceptions if the array is not well formed. *)
    val of_array : 'n succ nat -> 'm succ nat -> string array array -> ('n succ, 'm succ) matrix

    (** The call [get i j m] returns the coefficient of the i-th row and
        the j-th column. *)
    val get : 'n finite -> 'm finite -> ('n, 'm) matrix -> scalar

    (** The call [set i j x m] returns a new matrix of the same linear space as
        [m], and with the same coefficients, except for the one of the i-th row
        and the j-th column, which is set to the scalar [x]. *)
    val set : 'n finite -> 'm finite -> scalar -> ('n, 'm) matrix -> ('n, 'm) matrix

    (** The call [norm_inf m] computes the ∞-norm of [m], i.e the maximum of the
        absolute sums of the rows of [m]. *)
    val norm_inf : ('n, 'm) matrix -> scalar

    (** The call [norm_one m] computes the 1-norm of [m], i.e the maximum of the
        absolute sums of the columns of [m]. *)
    val norm_one : ('n, 'm) matrix -> scalar

    (** The call [transpose m] for m in 𝕂ⁿˣᵐ returns a new matrix in 𝕂ᵐˣⁿ with
        all the coefficients transposed. *)
    val transpose : ('n, 'm) matrix -> ('m, 'n) matrix

    (** The call [dimensions m] for m in 𝕂ⁿˣᵐ returns the pair (n, m). *)
    val dimensions : ('m, 'n) matrix -> 'm nat * 'n nat

    (** Matrices addition. The dimensions compatibility is statically ensured. *)
    val ( + ) : ('n, 'm) matrix -> ('n, 'm) matrix -> ('n, 'm) matrix

    (** Matrices subtraction. As for the addition, the dimensions compatibility
        is statically ensured. *)
    val ( - ) : ('n, 'm) matrix -> ('n, 'm) matrix -> ('n, 'm) matrix

    (** Matrices multiplication. The dimensions compatibility is statically
        ensured. *)
    val ( * ) : ('n, 'm) matrix -> ('m, 'p) matrix -> ('n, 'p) matrix

    (** Componentwise division. *)
    val ( / ) : ('n, 'm) matrix -> ('n, 'm) matrix -> ('n, 'm) matrix

    (** Scalar multiplication. *)
    val scale : scalar -> ('n, 'm) matrix  -> ('n, 'm) matrix

    (** Matrix inverse. Returns None if the input matrix is singular. *)
    val inverse : ('n succ, 'n succ) matrix -> ('n succ, 'n succ) matrix option

    (** The call [abs m] returns a matrix for which each coordinate is the
        absolute value of the corresponding coordinate of [m]. *)
    val abs : ('n, 'm) matrix -> ('n, 'm) matrix

    (** The call [all_components_lower_than l r] return true if and only if
        each components of [l] are lower or equal to their counterpart in [r],
        i.e for all i and j, [get i j l] is lower or equal to [get i j r]. *)
    val all_components_lower_than : ('n, 'm) matrix -> ('n, 'm) matrix -> bool

  end

end
