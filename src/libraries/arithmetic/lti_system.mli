(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module aims to provide overapproximations of the behaviors of
    linear time-invariant systems, for both the transition and
    the permanent phases.

    A LTI system corresponds to the following recursive equation :
      {math X[t + 1] = AX[t] + Bε[t] + S}
    where :
    - {m 𝕂} is a field ;
    - {m n} is the system's state dimension, or order ;
    - {m m} is the system's input space dimension ;
    - {m X[t] ∈ 𝕂^n} is the system's state vector at iteration {m t} ;
    - {m μ[t] ∈ 𝕂^m} is an input vector at iteration {m t} ;
    - {m A ∈ 𝕂^{n × n}} is the state matrix ;
    - {m B ∈ 𝕂^{n × m}} is the input matrix ;
    - {m S ∈ 𝕂^n} is the system's shift.

    Several notes here :
    - The only hypothesis on {m A} is that its eigenvalues are all lower
      than one in absolute value. It is a sufficient condition for the
      filter to converge. Conversely, there is no hypothesis on {m B}.
      If the procedure cannot prove easily that this hypothesis is
      satisfied, it will simply return [None].
    - All input vectors are supposed belonging to a box in {m 𝕂^m}.
    - Most presentations of LTI systems describe them using two equations,
      a recursive one equivalent to the one presented here and focused on
      the hidden state vector, and an output non recursive equation focused
      on transforming the hidden state vector into a usable output. However,
      as the two equations can be easily combined into one, it is not
      considered in this module.
    - Usually, the shift is not present, as it makes the system kind of
      affine instead of linear. However, the theory underlying this module
      can easily take it into account, and thus make it more general.

    A complete documentation on the underlying theory will be added in a
    near future. For an example using this module, one can check its tests,
    located in {!test/lti_system}. *)

module Make (K : Field.S) : sig

  (** The linear space in which the systems are defined. *)
  module Linear : module type of Linear.Space (K)
  module Box : module type of Box.Make (K)
  open Linear
  open Nat

  type 'n box = 'n Box.t

  (** A LTI system full specification. The fields are as follows:
      - [state_matrix]: the system's state matrix {m A};
      - [input_matrix]: the system's input matrix {m B};
      - [input_space]: the box containing all input vectors;
      - [shift]: the system's shift vector {m S};
      - [initial_state]: the system's initial state {m X[0]}. *)
  type ('n, 'm) system =
    { state_matrix  : ('n, 'n) matrix
    ; input_matrix  : ('n, 'm) matrix
    ; input_space   : 'm box
    ; shift         : 'n vector
    ; initial_state : 'n vector
    }

  (** Representation of a LTI system's behavior. The fields are as follows:
      - [transition] represents the transition phase as a list of boxes,
        one for each iteration that cannot be proven contained in the
        permanent phase. The length of the list, i.e the number of unrolled
        iterations, depends on the system's parameters and on the precision
        of the permanent phase's abstraction.
      - [permanent] represents the permanent phase as a unique box, which
        is an invariant for the filter for all iterations after the ones
        unrolled through the transition phase. *)
  type 'n behavior = { transition : 'n box list ; permanent : 'n box }

  (** Behavior computation. See module-level documentation for a general
      overview and a link to the underlying theory. The optional parameters
      are as follows:
      - [timeout] specifies the maximum analysis duration. It is expressed
        in seconds, and its default value is one second.
      - [completion_target] specifies the relative completion of the permanent
        phase that must be achieved, i.e how much of the permanent box is
        proven to be a valid and reachable state of the system.
        It is expressed as a ratio between 0 and 1. *)
  val behavior :
    ?timeout : float ->
    completion_target: float ->
    ('n succ, 'm succ) system ->
    'n succ behavior option

  (** Pretty print a behavior. Used for test and debug purposes. *)
  val pretty_behavior : 'n behavior option Pretty_utils.formatter

end
