(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

exception Error_Top
exception Error_Bottom
exception Not_less_than
exception Can_not_subdiv

let dkey = Kernel.dkey_approximation

let feedback_approximation format =
  Kernel.feedback ~dkey ~current:true ~once:true format

type truth = True | False | Unknown

let inv_truth = function
  | Unknown -> Unknown
  | True -> False
  | False -> True

module Comp = struct
  type t = Lt | Gt | Le | Ge | Eq | Ne

  type result = truth = True | False | Unknown

  let inv = function
    | Gt -> Le
    | Lt -> Ge
    | Le -> Gt
    | Ge -> Lt
    | Eq -> Ne
    | Ne -> Eq

  let sym = function
    | Gt -> Lt
    | Lt -> Gt
    | Le -> Ge
    | Ge -> Le
    | Eq -> Eq
    | Ne -> Ne

  let pretty_comp fmt = function
    | Gt -> Format.pp_print_string fmt ">"
    | Lt -> Format.pp_print_string fmt "<"
    | Le -> Format.pp_print_string fmt "<="
    | Ge -> Format.pp_print_string fmt ">="
    | Eq -> Format.pp_print_string fmt "=="
    | Ne -> Format.pp_print_string fmt "!="


end


module Make_Generic_Lattice_Set
    (V: Datatype.S)
    (Set: Lattice_type.Hptset with type elt = V.t)
= struct

  type t = Set of Set.t | Top
  type set = t

  let bottom = Set Set.empty
  let top = Top

  let hash = function
    | Top -> 12373
    | Set s ->
      let f v acc = 67 * acc + (V.hash v) in
      Set.fold f s 17

  let equal e1 e2 =
    e1 == e2 ||
    match e1, e2 with
    | Top, Top -> true
    | Set e1, Set e2 -> Set.equal e1 e2
    | Top, Set _ | Set _, Top -> false

  let compare e1 e2 =
    if e1 == e2 then 0
    else match e1, e2 with
      | Top, _ -> 1
      | _, Top -> -1
      | Set e1, Set e2 -> Set.compare e1 e2

  (** This is exact *)
  let join e1 e2 =
    if e1 == e2 then e1
    else match e1, e2 with
      | Top, _ | _, Top -> Top
      | Set s1 , Set s2 -> Set (Set.union s1 s2)

  (** This is exact *)
  let link = join

  (** This is exact *)
  let meet v1 v2 =
    if v1 == v2 then v1
    else match v1,v2 with
      | Top, v | v, Top -> v
      | Set s1 , Set s2 -> Set (Set.inter s1 s2)

  (** This is exact *)
  let narrow = meet

  let is_included e1 e2 =
    (e1 == e2) ||
    match e1,e2 with
    | _, Top -> true
    | Top, _ -> false
    | Set s1, Set s2 -> Set.subset s1 s2

  let intersects e1 e2 =
    match e1, e2 with
    | _, Top | Top, _ -> true
    | Set s1 , Set s2 -> Set.exists (fun e -> Set.mem e s2) s1

  let inter e1 e2 =
    match e1, e2 with
    | Top, Top -> Top
    | Top, Set s | Set s, Top -> Set s
    | Set s1, Set s2 -> Set (Set.inter s1 s2)

  let cardinal_less_than s n =
    match s with
    | Top -> raise Not_less_than
    | Set s ->
      let c = Set.cardinal s in
      if c > n then raise Not_less_than;
      c

  let cardinal_zero_or_one s =
    try ignore (cardinal_less_than s 1) ; true
    with Not_less_than -> false

  let inject s = Set s
  let inject_singleton e = inject (Set.singleton e)
  let empty = inject Set.empty

  let project = function
    | Top -> raise Error_Top
    | Set s -> s

  let filter f = function
    | Top -> Top
    | Set s -> Set (Set.filter f s)

  let transform f e1 e2 =
    match e1, e2 with
    | Top, _ | _, Top -> Top
    | Set s1 , Set s2 -> Set (f s1 s2)

  let map_set f s = Set.fold (fun v -> Set.add (f v)) s Set.empty

  let fold f e init = match e with
    | Top -> raise Error_Top
    | Set s -> Set.fold f s init

  let iter f = function
    | Top -> raise Error_Top
    | Set s -> Set.iter f s

  let exists f = function
    | Top -> true
    | Set s -> Set.exists f s

  let for_all f = function
    | Top -> false
    | Set s -> Set.for_all f s

  let mem v = function
    | Top -> true
    | Set s -> Set.mem v s

  let apply2 f s1 s2 =
    let distribute_on_elements f s1 s2 =
      Set.fold (fun v -> Set.union (map_set (f v) s2)) s1 Set.empty
    in
    transform (distribute_on_elements f) s1 s2

  let apply1 f = function
    | Top -> top
    | Set s -> Set (map_set f s)

  let pretty fmt = function
    | Top -> Format.fprintf fmt "TopSet"
    | Set s ->
      if Set.is_empty s then Format.fprintf fmt "BottomSet"
      else
        Pretty_utils.pp_iter
          ~pre:"@[<hov 1>{"
          ~suf:"}@]"
          ~sep:";@ "
          Set.iter
          (fun fmt v -> Format.fprintf fmt "@[%a@]" V.pretty v)
          fmt s

  include
    (Datatype.Make
       (struct
         type t = set
         let name = V.datatype_name ^ " lattice_set"
         let structural_descr =
           Structural_descr.t_sum [| [| Set.packed_descr |] |]
         let reprs = Top :: List.map (fun o -> Set o) Set.reprs
         let equal = equal
         let compare = compare
         let hash = hash
         let rehash = Datatype.identity
         let copy = Datatype.undefined
         let pretty = pretty
         let mem_project = Datatype.never_any_project
       end) :
       Datatype.S with type t := t)
end

module Make_Lattice_Set
    (V: Datatype.S)
    (Set: Lattice_type.Hptset with type elt = V.t)
  : Lattice_type.Lattice_Set with module O = Set
= struct
  module O = Set
  include Make_Generic_Lattice_Set (V) (Set)
end


module Make_Hashconsed_Lattice_Set
    (V: Hptmap.Id_Datatype)
    (Set: Hptset.S with type elt = V.t)
  : Lattice_type.Lattice_Set with module O = Set
= struct

  module O = Set

  include Make_Generic_Lattice_Set (V) (Set)

  let intersects e1 e2 = match e1, e2 with
    | _, Top | Top, _ -> true
    | Set s1 , Set s2 -> Set.intersects s1 s2

end

module Int = struct

  include (Integer: module type of Integer with type t = Integer.t)
  [@@alert "-deprecated"]

  include (Datatype.Integer: Datatype.S_with_collections with type t := t)
  [@@alert "-deprecated"]

  (** execute [f] on [inf], [inf + step], ... *)
  let fold f ~inf ~sup ~step acc =
    (*    Format.printf "Int.fold: inf:%a sup:%a step:%a@\n"
           pretty inf pretty sup pretty step; *)
    let nb_loop = Z.ediv (Z.sub sup inf) step in
    let rec fold_incr ~counter ~inf acc =
      if equal counter 1000z then
        feedback_approximation "enumerating %a integers" pretty nb_loop;
      if Z.leq inf sup then begin
        (*          Format.printf "Int.fold: %a@\n" pretty inf; *)
        fold_incr ~counter:(Z.succ counter) ~inf:(Z.add step inf) (f inf acc)
      end else acc
    in
    let rec fold_decr ~counter ~sup acc =
      if equal counter 1000z then
        feedback_approximation "enumerating %a integers" pretty nb_loop;
      if Z.leq inf sup then begin
        (*          Format.printf "Int.fold: %a@\n" pretty inf; *)
        fold_decr ~counter:(Z.succ counter) ~sup:(Z.add step sup) (f sup acc)
      end else acc
    in
    if Z.leq Z.zero step
    then fold_incr ~counter:Z.zero ~inf acc
    else fold_decr ~counter:Z.zero ~sup acc

end


(* Typing constraints are enforced directly in the .mli *)
module Rel = struct
  include Z

  let e_rem = erem

  let check ~rem ~modu =
    zero <= rem && rem < modu

  let add_abs = add
  let sub_abs = sub
end
