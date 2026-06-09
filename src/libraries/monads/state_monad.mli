(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** The State monad represents computations relying on a global mutable
    state but implemented in a functional way.
    @since 31.0-Gallium *)

module Make (Env : Datatype.S_with_collections) : sig
  include Monad.S
  type env = Env.t
  val get_environment : env t
  val set_environment : env -> unit t
  val resolve : 'a t -> env -> 'a * env
end
