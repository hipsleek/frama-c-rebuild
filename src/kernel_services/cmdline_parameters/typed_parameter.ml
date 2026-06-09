(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) gen_accessor =
  { get: unit -> 'a;
    set: 'a -> unit;
    add_set_hook: ('b -> 'b -> unit) -> unit;
    add_update_hook: ('b -> 'b -> unit) -> unit }

type 'a accessor = ('a, 'a) gen_accessor

type typed_accessor =
  | Bool of bool accessor * string option (** the negative option, if any *)
  | Int of int accessor * (unit -> int * int) (** getting range *)
  | Float of float accessor * (unit -> float * float) (** getting range *)
  | String of string accessor * (unit -> string list) (** possible values *)

type parameter =
  { name: string;
    help: string;
    accessor: typed_accessor;
    visible: bool;
    safe: bool;
    reconfigurable: bool;
    is_set: unit -> bool }

include
  Datatype.Make_with_collections
    (struct
      type t = parameter
      let name = "Parameter.t"
      let rehash = Datatype.identity
      let structural_descr = Structural_descr.t_unknown
      let reprs =
        [ { name = "bool_opt";
            help = "dummy bool option";
            accessor =
              Bool
                ({ get = (fun () -> false);
                   set = (fun _ -> ());
                   add_set_hook = (fun _ -> ());
                   add_update_hook = (fun _ -> ()) },
                 None);
            visible = false ;
            safe = false;
            reconfigurable = false ;
            is_set = fun () -> false }
        ]
      let equal = (==)
      let compare x y = if x == y then 0 else String.compare x.name y.name
      let hash x = Datatype.String.hash x.name
      let copy x = x (* The representation of the parameter is immutable *)
      let pretty fmt x = Format.pp_print_string fmt x.name
      let mem_project = Datatype.never_any_project
    end)

let parameters = Datatype.String.Hashtbl.create 97

let create ~name ~help ~accessor ~visible ~safe ~reconfigurable ~is_set =
  let p = { name; help; accessor; visible; safe; reconfigurable; is_set } in
  (* parameter name unicity already checks in [Plugin]. *)
  assert (not (Datatype.String.Hashtbl.mem parameters name));
  Datatype.String.Hashtbl.add parameters name p;
  p

let get = Datatype.String.Hashtbl.find parameters

let pretty_value fmt p = match p.accessor with
  | Bool(a, _) -> Format.fprintf fmt "%b" (a.get ())
  | Int(a, _) -> Format.fprintf fmt "%d" (a.get ())
  | Float (a, _) -> Format.fprintf fmt "%f" (a.get ())
  | String(a, _) -> Format.fprintf fmt "%s" (a.get ())

let get_value p = Format.asprintf "%a" pretty_value p
