(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Conf = sig
  type env (* Reader *)
  type out (* Writer *)
  type state (* State *)

  val empty_out : unit -> out
  val merge_out : out -> out -> out
end

module type S = sig
  type env (* Reader *)
  type out (* Writer *)
  type state (* State *)

  include Monad.S

  module Option : Monad.Iterators
    with type 'a iterable = 'a option
     and type 'a monad = 'a t

  module List : Monad.Iterators
    with type 'a iterable = 'a list
     and type 'a monad = 'a t

  val run : env:env -> state:state -> 'a t -> 'a * out * state

  (* Reader *)
  val read : env t
  val with_env : (env -> env) -> 'a t -> 'a t

  (* Writer *)
  val write : out -> unit t
  val update : out -> 'a t -> 'a t
  val flush : 'a t -> ('a * out) t

  (* State *)
  val get : state t
  val set : state -> unit t
  val modify : (state -> state) -> unit t

end

module Make (C : Conf)
  : S with type env = C.env
       and type state = C.state
       and type out = C.out
= struct
  type env = C.env
  type state = C.state
  type out = C.out

  module M = Monad.Make_based_on_bind (struct
      type 'a t = C.env -> C.state -> 'a * C.out * C.state
      let return x = fun _env state -> x, C.empty_out (), state
      let bind f m =
        fun env state ->
        let x, m_out, state = m env state in
        let y, f_out, state = f x env state in
        y, C.merge_out m_out f_out, state
    end)

  include M

  let run ~env ~state f = f env state

  (* reader *)
  let read = fun env state -> env, C.empty_out (), state
  let with_env f m = fun env state -> m (f env) state

  (* writer *)
  let write out = fun _env state -> (), out, state
  let update out m = fun env state ->
    let x, out', state = m env state in
    x, C.merge_out out out', state
  let flush m = fun env state ->
    let x, out, state = m env state in
    (x, out), C.empty_out (), state

  (* state *)
  let get = fun _env state -> state, C.empty_out (), state
  let set state = fun _env _state -> (), C.empty_out (), state
  let modify f = fun _env state -> (), C.empty_out (), f state

  module Option = Option.Make_monadic_iterators (M)
  module List = List.Make_monadic_iterators (M)
end
