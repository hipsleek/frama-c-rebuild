(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp

(* Represents the interval between [min] and [max], congruent to [rem] modulo
   [modulo]. A value of [None] for [min] (resp. [max]) represents -infinity
   (resp. +infinity). [modulo] is > 0, and [0 <= rem < modulo]. *)
type itv = { min: Z.t option;
             max: Z.t option;
             rem: Z.t;
             modu: Z.t; }

let top = { min = None; max = None; rem = Z.zero; modu = Z.one }

(* ------------------------------ Datatype ---------------------------------- *)

let bound_compare x y =
  match x, y with
  | None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some x, Some y -> Z.compare x y

let compare t1 t2 =
  let r1 = bound_compare t1.min t2.min in
  if r1 <> 0 then r1
  else
    let r2 = bound_compare t1.max t2.max in
    if r2 <> 0 then r2
    else
      let r3 = Z.compare t1.rem t2.rem in
      if r3 <> 0 then r3
      else Z.compare t1.modu t2.modu

let equal t1 t2 = compare t1 t2 = 0

let hash_v_option = function
  | None -> 97
  | Some v -> Z.hash v

let hash t =
  hash_v_option t.min + 5501 * (hash_v_option t.max) +
  59 * (Z.hash t.rem) + 13031 * (Z.hash t.modu)

let pretty fmt t =
  let print_bound fmt = function
    | None -> Format.fprintf fmt "--"
    | Some v -> Z.pretty fmt v
  in
  Format.fprintf fmt "[%a..%a]%t"
    print_bound t.min
    print_bound t.max
    (fun fmt ->
       if Z.is_zero t.rem && Z.is_one t.modu then Format.fprintf fmt ""
       else Format.fprintf fmt ",%a%%%a" Z.pretty t.rem Z.pretty t.modu)

include Datatype.Make_with_collections
    (struct
      type t = itv
      let name = "int_interval"
      open Structural_descr
      let structural_descr =
        let s_int = Descr.str Z.datatype_descr in
        t_record [| pack (t_option s_int);
                    pack (t_option s_int);
                    Z.packed_descr;
                    Z.packed_descr |]
      let reprs = [ top ]
      let equal = equal
      let compare = compare
      let hash = hash
      let pretty = pretty
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
      let copy = Datatype.undefined
    end)

(* ------------------------------ Building ---------------------------------- *)

let fail min max r modu =
  let bound fmt = function
    | None -> Format.fprintf fmt "--"
    | Some(x) -> Z.pretty fmt x
  in
  Kernel.fatal "Ival: broken interval, min=%a max=%a r=%a modu=%a"
    bound min bound max Z.pretty r Z.pretty modu

let is_safe_modulo r modu =
  (Z.geq r Z.zero ) && (Z.geq modu Z.one) && (Z.lt r modu)

let is_safe_bound bound r modu = match bound with
  | None -> true
  | Some m -> Z.equal (Z.erem m modu) r

(* Sanity check. *)
let check ~min ~max ~rem ~modu =
  if not (is_safe_modulo rem modu
          && is_safe_bound min rem modu
          && is_safe_bound max rem modu)
  then fail min max rem modu

let make ~min ~max ~rem ~modu =
  check ~min ~max ~rem ~modu;
  { min; max; rem; modu }

let inject_singleton e =
  { min = Some e; max = Some e; rem = Z.zero; modu = Z.one }

let inject_range min max =
  { min; max; rem = Z.zero; modu = Z.one }

let build_interval ~min ~max ~rem:r ~modu =
  assert (is_safe_modulo r modu);
  let fix_bound fix bound = match bound with
    | None -> None
    | Some b -> Some (if Z.equal b (Z.erem r modu) then b else fix b)
  in
  let min = fix_bound (fun min -> Z.round_up_to_r ~min ~r ~modu) min
  and max = fix_bound (fun max -> Z.round_down_to_r ~max ~r ~modu) max in
  make ~min ~max ~rem:r ~modu

let make_or_bottom ~min ~max ~rem ~modu =
  match min, max with
  | Some min, Some max when Z.gt min max -> `Bottom
  | _, _ -> `Value (make ~min ~max ~rem ~modu)

(* ---------------------------------- Utils --------------------------------- *)

let min_le_elt min elt =
  match min with
  | None -> true
  | Some m -> Z.leq m elt

let max_ge_elt max elt =
  match max with
  | None -> true
  | Some m -> Z.geq m elt

let all_positives t =
  match t.min with
  | None -> false
  | Some m -> Z.geq m Z.zero

let all_negatives t =
  match t.max with
  | None -> false
  | Some m -> Z.leq m Z.zero

let min_and_max t = t.min, t.max

let min_max_rem_modu t = t.min, t.max, t.rem, t.modu

let mem i t =
  Z.equal (Z.erem i t.modu) t.rem
  && min_le_elt t.min i && max_ge_elt t.max i

let cardinal t =
  match t.min, t.max with
  | None, _ | _, None -> None
  | Some mn, Some mx -> Some Z.(succ ((ediv (sub mx mn) t.modu)))

(* --------------------------------- Lattice -------------------------------- *)

(** [min_is_lower mn1 mn2] is true iff mn1 is a lower min than mn2 *)
let min_is_lower min1 min2 =
  match min1, min2 with
  | None, _ -> true
  | _, None -> false
  | Some m1, Some m2 -> Z.leq m1 m2

(** [max_is_greater mx1 mx2] is true iff mx1 is a greater max than mx2 *)
let max_is_greater max1 max2 =
  match max1, max2 with
  | None, _ -> true
  | _, None -> false
  | Some m1, Some m2 -> Z.geq m1 m2

let rem_is_included r1 m1 r2 m2 =
  (Z.is_zero (Z.erem m1 m2)) && (Z.equal (Z.erem r1 m2) r2)

let is_included t1 t2 =
  (min_is_lower t2.min t1.min) &&
  (max_is_greater t2.max t1.max) &&
  rem_is_included t1.rem t1.modu t2.rem t2.modu

let min_min x y =
  match x, y with
  | None,_ | _, None -> None
  | Some x, Some y -> Some (Z.min x y)

let max_max x y =
  match x, y with
  | None,_ | _, None -> None
  | Some x, Some y -> Some (Z.max x y)

let join t1 t2 =
  let modu = Z.(gcd (gcd t1.modu t2.modu) (abs (sub t1.rem t2.rem))) in
  let rem = Z.erem t1.rem modu in
  let min = min_min t1.min t2.min in
  let max = max_max t1.max t2.max in
  make ~min ~max ~rem ~modu

let link t1 t2 =
  if Z.equal t1.rem t2.rem && Z.equal t1.modu t2.modu
  then
    let min = match t1.min, t2.min with
      | Some a, Some b -> Some (Z.min a b)
      | _ -> None in
    let max = match t1.max, t2.max with
      | Some a, Some b -> Some (Z.max a b)
      | _ -> None in
    make ~min ~max ~rem:t1.rem ~modu:t1.modu
  else t1 (* No best abstraction anyway. *)

(* [extended_euclidian_algorithm a b] returns x,y,gcd such that
   a*x+b*y=gcd(x,y). *)
let extended_euclidian_algorithm a b =
  assert (Z.gt a Z.zero);
  assert (Z.gt b Z.zero);
  let a = ref a and b = ref b in
  let x = ref Z.zero and lastx = ref Z.one in
  let y = ref Z.one and lasty = ref Z.zero in
  while not (Z.is_zero !b) do
    let (q,r) = Z.ediv_rem !a !b in
    a := !b;
    b := r;
    let tmpx = !x in
    (x:= Z.sub !lastx (Z.mul q !x); lastx := tmpx);
    let tmpy = !y in
    (y:= Z.sub !lasty (Z.mul q !y); lasty := tmpy);
  done;
  (!lastx, !lasty, !a)

(* This function provides solutions to the Chinese remainder theorem,
   i.e. it finds the solutions x such that:
   x == r1 mod m1 && x == r2 mod m2.
   If no such solution exists, it raises Error_Bottom; else it returns
   (r,m) such that all solutions x are such that x == r mod m. *)
let compute_r_common r1 m1 r2 m2 =
  (* (E1) x == r1 mod m1 && x == r2 mod m2
     <=> \E k1,k2: x = r1 + k1*m1 && x = r2 + k2*m2
     <=> \E k1,k2: x = r1 + k1*m1 && k1*m1 - k2*m2 = r2 - r1

     Let r = r2 - r1. The equation (E2): k1*m1 - k2*m2 = r is
     diophantine; there are solutions x to (E1) iff there are
     solutions (k1,k2) to (E2).

     Let d = gcd(m1,m2). There are solutions to (E2) only if d
     divides r (because d divides k1*m1 - k2*m2). Else we raise
     [Error_Bottom]. *)
  let (x1,_,gcd) = extended_euclidian_algorithm m1 m2 in
  let r = Z.sub r2 r1 in
  let (r_div, r_rem) = Z.ediv_rem r gcd in
  if not (Z.is_zero r_rem)
  then raise Error_Bottom
  (* The extended euclidian algorithm has provided solutions x1,x2 to
     the Bezout identity x1*m1 + x2*m2 = d.

     x1*m1 + x2*m2 = d ==> x1*(r/d)*m1 + x2*(r/d)*m2 = d*(r/d).

     Thus, k1 = x1*(r/d), k2=-x2*(r/d) are solutions to (E2)
     Thus, x = r1 + x1*(r/d)*m1 is a particular solution to (E1). *)
  else
    let k1 = Z.mul x1 r_div in
    let x = Z.add r1 (Z.mul k1 m1) in
    (* If two solutions x and y exist, they are equal modulo lcm(m1,m2).
       We have x == r1 mod m1 && y == r1 mod m1 ==> \E k1: x - y = k1*m1
       x == r2 mod m2 && y == r2 mod m2 ==> \E k2: x - y = k2*m2

       Thus k1*m1 = k2*m2 is a multiple of m1 and m2, i.e. is a multiple
       of lcm(m1,m2). Thus x = y mod lcm(m1,m2). *)
    let lcm = Z.lcm m1 m2 in
    (* x may be bigger than the lcm, we normalize it. *)
    (Z.erem x lcm, lcm)

let compute_first_common mn1 mn2 r modu =
  if mn1 = None && mn2 = None
  then None
  else
    let m =
      match mn1, mn2 with
      | Some m, None | None, Some m -> m
      | Some m1, Some m2 -> Z.max m1 m2
      | None, None -> assert false (* already tested above *)
    in
    Some (Z.round_up_to_r ~min:m ~r ~modu)

let compute_last_common mx1 mx2 r modu =
  if mx1 = None && mx2 = None
  then None
  else
    let m =
      match mx1, mx2 with
      | Some m, None | None, Some m -> m
      | Some m1, Some m2 -> Z.min m1 m2
      | None, None -> assert false (* already tested above *)
    in
    Some (Z.round_down_to_r ~max:m ~r ~modu)

let meet t1 t2 =
  try
    let rem, modu = compute_r_common t1.rem t1.modu t2.rem t2.modu in
    let min = compute_first_common t1.min t2.min rem modu in
    let max = compute_last_common t1.max t2.max rem modu in
    make_or_bottom ~min ~max ~rem ~modu
  with Error_Bottom -> `Bottom

let narrow = meet

type widen_hint = Z.Set.t

let widen ?(size=Z.zero) ?(hint = Z.Set.empty) t1 t2 =
  if equal t1 t2 then t2
  else
    (* Add possible interval limits deducted from the bitsize *)
    let thresholds =
      (* If bitsize > 128, the values do not correspond to a scalar type.
         This can (rarely) happen on structures or arrays that have been
         reinterpreted as one value by the offsetmaps. In this case, do not
         use limits, and do not create arbitrarily large integers. *)
      if Z.gt size 128z
      then Z.Set.empty
      else if Z.is_zero size
      then hint
      else
        (* Integer includes a function size so we rename it here to avoid
           shadowing it. *)
        let ssize = size in
        let limits =
          Z.[ neg (two_power (pred ssize));
              pred (two_power (pred ssize));
              pred (two_power ssize); ]
        in
        Z.Set.(union hint (of_list limits))
    in
    let modu = Z.(gcd (gcd t1.modu t2.modu) (abs (sub t1.rem t2.rem))) in
    let rem = Z.erem t1.rem modu in
    let min =
      if bound_compare t1.min t2.min = 0 then t2.min else
        match t2.min with
        | None -> None
        | Some min2 ->
          try
            let v = Z.Set.nearest_elt_le min2 thresholds
            in Some (Z.round_up_to_r ~r:rem ~modu ~min:v)
          with Not_found -> None
    in
    let max =
      if bound_compare t1.max t2.max = 0 then t2.max else
        match t2.max with
        | None -> None
        | Some max2 ->
          try
            let v = Z.Set.nearest_elt_ge max2 thresholds
            in Some (Z.round_down_to_r ~r:rem ~modu ~max:v)
          with Not_found -> None
    in
    make ~min ~max ~rem ~modu

let intersects v1 v2 =
  v1 == v2 || not (meet v1 v2 = `Bottom) (* YYY: slightly inefficient *)

let cardinal_less_than t n =
  let c = match t.min, t.max with
    | None, _ | _, None -> raise Not_less_than
    | Some min, Some max -> Z.succ ((Z.ediv (Z.sub max min) t.modu))
  in
  if Z.leq c (Z.of_int n)
  then Z.to_int c (* This is smaller than the original [n] *)
  else raise Not_less_than

let cardinal_zero_or_one t =
  match t.min, t.max with
  | None, _ | _, None -> false
  | Some min, Some max -> Z.equal min max

(* TODO? *)
let diff v _ = `Value v
let diff_if_one v _ = `Value v

let complement_under ~min ~max t =
  let inject_range min max =
    if Z.leq min max
    then `Value (inject_range (Some min) (Some max))
    else `Bottom
  in
  match t.min, t.max with
  | None, None -> `Bottom
  | Some b, None -> inject_range min (Z.pred b)
  | None, Some e -> inject_range (Z.succ e) max
  | Some b, Some e ->
    let delta_min = Z.sub b min in
    let delta_max = Z.sub max e in
    if Z.leq delta_min delta_max
    then inject_range (Z.succ e) max
    else inject_range min (Z.pred b)

(** execute [f] on [inf], [inf + step], ... *)
let fold_aux f ~inf ~sup ~step acc =
  let open Z in
  (*    Format.printf "fold_aux: inf:%a sup:%a step:%a@\n"
         pretty inf pretty sup pretty step; *)
  let nb_loop = ediv (sub sup inf) step in
  let rec fold_incr ~counter ~inf acc =
    if equal counter 1000z then
      feedback_approximation "enumerating %a integers" pretty nb_loop;
    if leq inf sup then begin
      (*          Format.printf "fold_aux: %a@\n" pretty inf; *)
      fold_incr ~counter:(succ counter) ~inf:(add step inf) (f inf acc)
    end else acc
  in
  let rec fold_decr ~counter ~sup acc =
    if equal counter 1000z then
      feedback_approximation "enumerating %a integers" pretty nb_loop;
    if leq inf sup then begin
      (*          Format.printf "fold_aux: %a@\n" pretty inf; *)
      fold_decr ~counter:(succ counter) ~sup:(add step sup) (f sup acc)
    end else acc
  in
  if leq zero step
  then fold_incr ~counter:zero ~inf acc
  else fold_decr ~counter:zero ~sup acc

let fold_int ?(increasing=true) f t acc =
  match t.min, t.max with
  | None, _ | _, None -> raise Error_Top
  | Some inf, Some sup ->
    let step = if increasing then t.modu else Z.neg t.modu in
    fold_aux f ~inf ~sup ~step acc

let fold_enum f v acc =
  fold_int (fun x acc -> f (inject_singleton x) acc) v acc

let to_seq ?(increasing=true) t =
  let start, is_before_last =
    match t.min, t.max with
    | Some l, Some u -> if increasing then l, Z.geq u else u, Z.leq l
    | Some l, None when increasing -> l, fun _ -> true (* Infinite sequence *)
    | None, Some u when not increasing -> u, fun _ -> true (* Infinite sequence *)
    | _ -> raise Error_Top
  in
  let next = if increasing then Z.add else Z.sub in
  let rec aux i () =
    if is_before_last i
    then Seq.Cons (i, aux (next i t.modu))
    else Seq.Nil
  in
  aux start


(* ------------------------------ Arithmetics ------------------------------- *)

let opt_map2 f m1 m2 =
  match m1, m2 with
  | None, _ | _, None -> None
  | Some m1, Some m2 -> Some (f m1 m2)

let add_singleton_int i t =
  let incr = Z.add i in
  let min = Option.map incr t.min in
  let max = Option.map incr t.max in
  let rem = Z.erem (incr t.rem) t.modu in
  make ~min ~max ~rem ~modu:t.modu

let add t1 t2 =
  let modu = Z.gcd t1.modu t2.modu in
  let rem = Z.erem (Z.add t1.rem t2.rem) modu in
  let min =
    opt_map2
      (fun min1 min2 -> Z.round_up_to_r ~min:(Z.add min1 min2) ~r:rem ~modu)
      t1.min t2.min
  in
  let max =
    opt_map2
      (fun m1 m2 -> Z.round_down_to_r ~max:(Z.add m1 m2) ~r:rem ~modu)
      t1.max t2.max
  in
  make ~min ~max ~rem ~modu

let add_under t1 t2 =
  if Z.equal t1.modu t2.modu
  then
    (* Note: min1+min2 % modu = max1 + max2 % modu = r1 + r2 % modu;
       no need to trim the bounds here.  *)
    let rem = Z.erem (Z.add t1.rem t2.rem) t1.modu in
    let min = opt_map2 Z.add t1.min t2.min
    and max = opt_map2 Z.add t1.max t2.max in
    `Value (make ~min ~max ~rem ~modu:t1.modu)
  else
    (* In many cases, there is no best abstraction; for instance when
       modu1 divides modu2, a part of the resulting interval is
       congruent to modu1, and a larger part is congruent to modu2.  In
       general, one can take the intersection. In any case, this code
       should be rarely called. *)
    `Bottom

let neg t =
  make
    ~min:(Option.map Z.neg t.max)
    ~max:(Option.map Z.neg t.min)
    ~rem:(Z.erem (Z.neg t.rem) t.modu)
    ~modu:t.modu

let abs t =
  match t.min, t.max with
  | Some mn, _ when Z.(geq mn zero) -> t
  | _, Some mx when Z.(leq mx zero) -> neg t
  | _, _ ->
    let max =
      match t.min, t.max with
      | Some mn, Some mx -> Some (Z.(max (neg mn) mx))
      | _, _ -> None
    in
    let modu = Z.(gcd t.modu (add t.rem t.rem)) in
    let rem = Z.erem t.rem modu in
    build_interval ~min:(Some Z.zero) ~max ~rem ~modu

type ext_value = Ninf | Pinf | Val of Z.t
let inject_min = function None -> Ninf | Some m -> Val m
let inject_max = function None -> Pinf | Some m -> Val m
let ext_neg = function Ninf -> Pinf | Pinf -> Ninf | Val v -> Val (Z.neg v)
let ext_mul x y =
  match x, y with
  | Ninf, Ninf | Pinf, Pinf -> Pinf
  | Ninf, Pinf | Pinf, Ninf -> Ninf
  | Val v1, Val v2 -> Val (Z.mul v1 v2)
  | (Ninf | Pinf as x), Val v | Val v, (Ninf | Pinf as x) ->
    if Z.gt v Z.zero then x
    else if Z.lt v Z.zero then ext_neg x
    else Val Z.zero

let ext_min x y =
  match x,y with
    Ninf, _ | _, Ninf -> Ninf
  | Pinf, x | x, Pinf -> x
  | Val x, Val y -> Val(Z.min x y)

let ext_max x y =
  match x,y with
    Pinf, _ | _, Pinf -> Pinf
  | Ninf, x | x, Ninf -> x
  | Val x, Val y -> Val(Z.max x y)

let ext_proj = function Val x -> Some x | _ -> None

let scale f t =
  let incr = Z.mul f in
  if Z.gt f Z.zero
  then
    let modu = incr t.modu in
    make
      ~min:(Option.map incr t.min) ~max:(Option.map incr t.max)
      ~rem:(Z.erem (incr t.rem) modu) ~modu
  else
    let modu = Z.neg (incr t.modu) in
    make
      ~min:(Option.map incr t.max) ~max:(Option.map incr t.min)
      ~rem:(Z.erem (incr t.rem) modu) ~modu

let scale_div_common ~pos f t degenerate =
  assert (not (Z.is_zero f));
  let div_f =
    if pos
    then fun a -> Z.ediv a f
    else fun a -> Z.div a f
  in
  let rem, modu =
    let negative = max_is_greater (Some Z.zero) t.max in
    if (negative                                (* all negative *)
        || pos                                  (* good div *)
        || (min_is_lower (Some Z.zero) t.min) (* all positive *)
        ||    (Z.is_zero (Z.erem t.rem f)))  (* exact *)
    && (Z.is_zero (Z.erem t.modu f))
    then
      let modu = Z.abs (div_f t.modu) in
      let r = if negative then Z.sub t.rem t.modu else t.rem in
      (Z.erem (div_f r) modu), modu
    else (* degeneration*)
      degenerate t.rem t.modu
  in
  let divf_mn1 = Option.map div_f t.min in
  let divf_mx1 = Option.map div_f t.max in
  let min, max =
    if Z.gt f Z.zero
    then divf_mn1, divf_mx1
    else divf_mx1, divf_mn1
  in
  make ~min ~max ~rem ~modu

let scale_div ~pos f v =
  scale_div_common ~pos f v (fun _ _ -> Z.zero, Z.one)

let scale_div_under ~pos f v =
  try
    (* TODO: a more precise result could be obtained by transforming
       Top(min,max,r,m) into Top(min,max,r/f,m/gcd(m,f)). But this is
       more complex to implement when pos or f is negative. *)
    `Value (scale_div_common ~pos f v (fun _r _m -> raise Exit))
  with Exit -> `Bottom

let scale_rem ~pos f t =
  assert (not (Z.is_zero f));
  let f = if Z.lt f Z.zero then Z.neg f else f in
  let rem_f a = if pos then Z.erem a f else Z.rem a f in
  let modu = Z.gcd f t.modu in
  let rr = Z.erem t.rem modu in
  let binf, bsup =
    if pos
    then Z.round_up_to_r ~min:Z.zero ~r:rr ~modu,
         Z.round_down_to_r ~max:(Z.pred f) ~r:rr ~modu
    else
      let min = if all_positives t then Z.zero else Z.neg (Z.pred f) in
      let max = if all_negatives t then Z.zero else Z.pred f in
      Z.round_up_to_r ~min ~r:rr ~modu,
      Z.round_down_to_r ~max ~r:rr ~modu
  in
  let mn_rem, mx_rem =
    match t.min, t.max with
    | Some mn, Some mx ->
      let div_f a = if pos then Z.ediv a f else Z.div a f in
      (* See if [mn..mx] is included in [k*f..(k+1)*f] for some [k]. In
         this case, [%] is monotonic and [mn%f .. mx%f] is a more precise
         result. *)
      if Z.equal (div_f mn) (div_f mx)
      then rem_f mn, rem_f mx
      else binf,bsup
    | _ -> binf,bsup
  in
  make ~min:(Some mn_rem) ~max:(Some mx_rem) ~rem:rr ~modu

let c_rem t1 t2 =
  match t2.min, t2.max with
  | None, _ | _, None -> `Value top
  | Some min2, Some max2 ->
    if Z.is_zero max2
    then `Bottom (* completely undefined. *)
    else
      (* Result is of the sign of x. Also, compute |x| to bound the result *)
      let neg, pos, max_x =
        min_le_elt t1.min Z.minus_one,
        max_ge_elt t1.max Z.one,
        (match t1.min, t1.max with
         | Some mn, Some mx -> Some (Z.max (Z.abs mn) (Z.abs mx))
         | _ -> None)
      in
      (* Bound the result: no more than |x|, and no more than |y|-1 *)
      let pos_rem = Z.max (Z.abs min2) (Z.abs max2) in
      let bound = Z.pred pos_rem in
      let bound = Option.fold ~some:(Z.min bound) ~none:bound max_x in
      (* Compute result bounds using sign information *)
      let min = if neg then Some (Z.neg bound) else Some Z.zero in
      let max = if pos then Some bound else Some Z.zero in
      (* TODO: inject_range_or_bottom? *)
      `Value (inject_range min max)

let mul t1 t2 =
  let min1 = inject_min t1.min in
  let max1 = inject_max t1.max in
  let min2 = inject_min t2.min in
  let max2 = inject_max t2.max in
  let a = ext_mul min1 min2 in
  let b = ext_mul min1 max2 in
  let c = ext_mul max1 min2 in
  let d = ext_mul max1 max2 in
  let min = ext_min (ext_min a b) (ext_min c d) in
  let max = ext_max (ext_max a b) (ext_max c d) in
  (* let multipl1 = Z.gcd m1 r1 in
     let multipl2 = Z.gcd m2 r2 in
     let modu1 = Z.gcd m1 m2 in
     let modu2 = Z.mul multipl1 multipl2 in
     let modu = Z.lcm modu1 modu2 in    *)
  let modu =
    Z.(gcd
         (gcd (mul t1.modu t2.modu) (mul t1.rem t2.modu))
         (mul t2.rem t1.modu))
  in
  let rem = Z.erem (Z.mul t1.rem t2.rem) modu in
  make ~min:(ext_proj min) ~max:(ext_proj max) ~rem ~modu

(* ymin and ymax must be the same sign *)
let div_range x ymn ymx =
  match min_and_max x with
  | Some xmn, Some xmx ->
    let c1 = Z.div xmn ymn in
    let c2 = Z.div xmx ymn in
    let c3 = Z.div xmn ymx in
    let c4 = Z.div xmx ymx in
    let min = Z.min (Z.min c1 c2) (Z.min c3 c4) in
    let max = Z.max (Z.max c1 c2) (Z.max c3 c4) in
    inject_range (Some min) (Some max)
  | _ ->
    top

let div x y =
  match y.min, y.max with
  | None, _ | _, None -> `Value top
  | Some min, Some max ->
    let result_pos =
      if Z.gt max Z.zero
      then
        let lpos =
          if Z.gt min Z.zero
          then min
          else Z.round_up_to_r ~min:Z.one ~r:y.rem ~modu:y.modu
        in
        `Value (div_range x lpos max)
      else `Bottom
    in
    let result_neg =
      if Z.lt min Z.zero
      then
        let gneg =
          if Z.lt max Z.zero
          then max
          else Z.round_down_to_r ~max:Z.minus_one ~r:y.rem ~modu:y.modu
        in
        `Value (div_range x min gneg)
      else `Bottom
    in
    Lattice_bounds.Bottom.join join result_neg result_pos

(* ----------------------------------- Misc --------------------------------- *)

let cast ~size ~signed t =
  let factor = Z.two_power size
  and mask = Z.two_power (Z.pred size) in
  let best_effort () =
    let modu = Z.gcd factor t.modu in
    let rem = Z.erem t.rem modu in
    let min =
      if signed
      then Z.round_up_to_r ~min:(Z.neg mask) ~r:rem ~modu
      else Z.round_up_to_r ~min:Z.zero ~r:rem ~modu
    and max =
      if signed
      then Z.round_down_to_r ~max:(Z.pred mask) ~r:rem ~modu
      else Z.round_down_to_r ~max:(Z.pred factor) ~r:rem ~modu
    in
    make ~min:(Some min) ~max:(Some max) ~rem ~modu
  in
  match t.min, t.max with
  | Some min, Some max ->
    let not_p_factor = Z.neg factor in
    let highbits_mn, highbits_mx =
      if signed then
        Z.logand (Z.add min mask) not_p_factor,
        Z.logand (Z.add max mask) not_p_factor
      else
        Z.logand min not_p_factor,
        Z.logand max not_p_factor
    in
    if Z.equal highbits_mn highbits_mx
    then
      if Z.is_zero highbits_mn
      then t
      else
        let rem_f value = Z.cast ~size ~signed ~value in
        let min, max = rem_f min, rem_f max in
        let rem = Z.erem min t.modu in
        make ~min:(Some min) ~max:(Some max) ~rem ~modu:t.modu
    else best_effort ()
  | _, _ -> best_effort ()

let subdivide t =
  match t.min, t.max with
  | Some min, Some max ->
    let mean = Z.ediv (Z.add min max) 2z in
    let succmean = Z.succ mean in
    build_interval ~min:(Some min) ~max:(Some mean) ~rem:t.rem ~modu:t.modu,
    build_interval ~min:(Some succmean) ~max:(Some max) ~rem:t.rem ~modu:t.modu
  | _, _ -> raise Can_not_subdiv

let reduce_sign t b =
  let r, modu = t.rem, t.modu in
  if b
  then
    if all_positives t
    then `Value t
    else
      let max = Some Z.(round_down_to_r ~max:minus_one ~r ~modu) in
      make_or_bottom ~min:t.min ~max ~rem:r ~modu
  else
  if all_negatives t
  then `Value t
  else
    let min = Some Z.(round_up_to_r ~min:zero ~r ~modu) in
    make_or_bottom ~min ~max:t.max ~rem:r ~modu

let reduce_bit i t b =
  let r, modu = t.rem, t.modu in
  let power = Z.two_power_of_int i in (* 001000 *)
  let mask = Z.pred (Z.two_power_of_int (i+1)) in (* 001111 *)
  (* Reduce bounds to the nearest satisfying bound *)
  let min = match t.min with
    | Some l when Z.testbit l i <> b ->
      let min =
        if b
        then Z.(logor (logand l (lognot mask)) power) (* ll1000 *)
        else Z.(succ (logor l mask)) (* ll1111 + 1 *)
      in
      Some (Z.round_up_to_r ~min ~r ~modu)
    | l -> l
  and max = match t.max with
    | Some u when Z.testbit u i <> b ->
      let max =
        if b
        then Z.(pred (logand u (lognot mask))) (* uu0000 - 1 *)
        else Z.(logand (logor u mask) (lognot power)) (* uu0111 *)
      in
      Some (Z.round_down_to_r ~max ~r ~modu)
    | u -> u
  in
  make_or_bottom ~min ~max ~rem:r ~modu

(* No check in the exported [make] function: callers are expected to perform
   the checks when needed. Thus the function [check] is also exported. *)
let make ~min ~max ~rem ~modu = { min; max; rem; modu }
