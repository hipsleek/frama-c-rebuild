(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Local Context                                                      --- *)
(* -------------------------------------------------------------------------- *)

type 'a value = {
  name : string ; (* Descriptive *)
  mutable current : 'a option ;
}

let create ?default name = { name = name ; current = default }
let name s = s.name

let defined env = match env.current with None -> false | Some _ -> true

let get env =
  match env.current with
  | Some e -> e
  | None -> Wp_parameters.fatal "Context '%s' non-initialized." env.name

let get_opt env = env.current

let set env s =
  env.current <- Some s

let clear env =
  env.current <- None

let update env f =
  match env.current with
  | Some e -> env.current <- Some (f e)
  | None -> Wp_parameters.fatal "Context '%s' non-initialized." env.name

let bind_with env w f e =
  let tmp = env.current in env.current <- w ;
  try let e = f e in env.current <- tmp ; e
  with error -> env.current <- tmp ; raise error

let bind env s f e = bind_with env (Some s) f e
let free env f e = bind_with env None f e

let push env x = let old = env.current in env.current <- Some x ; old
let pop env old = env.current <- old

let demon = ref []

let register f = demon := !demon @ [f]

let configure =
  let closure,state =
    State_builder.apply_once "Wp.Context.configure" [ Ast.self ]
      (fun () -> List.iter (fun f -> f ()) !demon)
  in
  ignore state ; closure

(* -------------------------------------------------------------------------- *)
