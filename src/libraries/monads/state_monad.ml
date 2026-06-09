(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Make (Env : Datatype.S_with_collections) = struct

  module Cache = Env.Hashtbl
  type 'a cache = 'a Cache.t
  type env = Env.t

  module Minimal = struct

    type 'a t = ('a * env) cache option * (env -> 'a * env)
    let return (x : 'a) : 'a t = (None, fun env -> x, env)

    let compute ((cache, make) : 'a t) (env : env) : 'a * env =
      match cache with
      | None -> make env
      | Some cache when Cache.mem cache env -> Cache.find cache env
      | Some cache -> let r = make env in Cache.add cache env r ; r

    let bind (f : 'a -> 'b t) (m : 'a t) : 'b t =
      let make env = let a, env = compute m env in compute (f a) env in
      (Some (Cache.create 13), make)

  end

  include Monad.Make_based_on_bind (Minimal)
  let get_environment : env t = None, fun env -> env, env
  let set_environment (env : env) : unit t = None, fun _ -> (), env
  let resolve (m : 'a t) (env : env) : 'a * env = Minimal.compute m env

end
