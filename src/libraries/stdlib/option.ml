(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Minimal = struct
  type 'a t = 'a option
  let return v = Some v
  let bind f m = Stdlib.Option.bind m f
  let product l r = match l, r with Some l, Some r -> Some (l, r) | _ -> None
end

include Monad.Make_based_on_bind_with_product (Minimal)

include Stdlib.Option

let bind = Minimal.bind

let ( <? ) opt default = value ~default opt

let filter f = function
  | None -> None
  | (Some x) as o -> if f x then o else None

let get ?(exn=Invalid_argument "option is None") = function
  | None -> raise exn
  | Some x -> x

let value_or_else ~none = function
  | None -> none ()
  | Some v -> v

let hash hash v = match v with
  | None -> 31179
  | Some v -> hash v

let merge f x y = match x, y with
  | x, None | None, x -> x
  | Some x, Some y -> Some (f x y)

let map2 f x y = match x, y with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (f x y)

let map_no_copy f o =
  match o with
  | None -> o
  | Some x ->
    let x' = f x in
    if x' != x then Some x' else o

module Make_monadic_iterators (M : Monad.S) = struct
  type 'a iterable = 'a option
  type 'a monad = 'a M.t

  let fold f acc = function
    | None -> M.return acc
    | Some x -> f acc x

  let iter f = function
    | None -> M.return ()
    | Some x -> f x

  let map f = function
    | None -> M.return None
    | Some x -> M.map (fun x -> Some x) (f x)

end
