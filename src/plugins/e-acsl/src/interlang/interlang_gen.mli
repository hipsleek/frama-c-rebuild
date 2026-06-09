(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Interlang

(** The compilation of E-ACSL to Cil is implemented as a two-stage process,
    where E-ACSL is first translated into an intermediate language {!Interlang}
    and only then into Cil. This module defines a monad {!M} for specifying
    computations that generate {!Interlang} expressions, and is thus used for
    the first stage.

    It is an instance of the RWS monad ({!Monad_rws}), operating on
    the following background data. *)

(** The Reader variable of {!M}. See {!Monad_rws.Conf.env}.

    Computations within the [Interlang_gen.M] monad are initiated from within a
    larger compilation context with its own environment of type {!Env.t}.
    Moreover in that context the current [kernel_function] and current location
    are known. These data will not change during the computation, and are
    simply made available (as a sort of global variable) through this Reader
    variable {!field-env}.

    Additionally the {!rte} field specifies whether RTEs are to be generated.
    This value might be locally shadowed during the computation. *)
type env =
  {kf : Cil_types.kernel_function;
   loc : Cil_types.location;
   vars : exp Cil_datatype.Term.Map.t;
   env : Env.t;
   rte : bool}

(** The State variable of {!M} includes a map of local variables (initially
    empty) which is enriched with newly generated variables. This is done such
    that at the end of a block these variables can be cleaned. (The cleaning
    mechanism is not yet implemented in the new compilation scheme, and
    effectuated by the direct-to-Cil compilation scheme.) *)
type state = exp Cil_datatype.Term.Map.t

type out = unit

(** This exception is raised when the language element to be translated is
    not yet supported by the intermediate language compilation scheme. In that
    case the old direct-to-Cil compilation scheme is used.
    The preferred method of raising this exception is using {!M.not_covered}. *)
exception Not_covered

(** The intermediate language generation monad. It is used for translating
    E-ACSL predicates to expressions of the E-ACSL intermediate language (see
    {!Interlang}). *)
module M : sig
  include Monad_rws.S
    with type env = env
     and type state = state
     and type out = out

  (** The preferred method of raising the {!Not_covered} exception.
      The format parameter should print the unsupported language element
      encountered. The [?pre] parameter allows for some additional information
      to be printed alongside. *)
  val not_covered :
    ?pre:string -> (Format.formatter -> 'a -> unit) -> 'a -> 'b t

  (** A convenience function to obtain the logic environment from the current
      {!Env.t}-portion of the Reader variable. *)
  val read_logic_env : Analyses_datatype.Logic_env.t t
end

type 'a m = 'a M.t (** an abbreviation for the monad type *)

val of_binop: Cil_types.binop -> Interlang.binop
val of_relation: Cil_types.relation -> Interlang.binop
