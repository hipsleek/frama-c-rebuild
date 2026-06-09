(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let compute_cache_size i = 1 lsl (8 + i)
let default_value = 2

(* Should not be used to create cache before the value from -cache-size
   has been set. *)
let cache_size = ref (compute_cache_size default_value)

(* Set by the kernel, according to the -cache-size parameter. *)
let set_cache_size i = cache_size := compute_cache_size i

(** The caches of this module are lazy, for two reasons:

    - some caches are never used, because the function that created them is
      never called. This typically happens for functors implementing generic
      datastructures, where not all functions are used in every module
      (but every function with a static cache creates its cache nevertheless)

    - Caches must be cleared as soon as some states change, in order to remain
      coherent (for example, when the current project changes). When setting
      multiple command-line options, the caches may be cleared after each option.
      When caches are big, this becomes very time-consuming. To avoid this,
      the functions [clear] do nothing when the caches have not been forced yet.
      (This is not perfect: once a lazy cache has been forced, each 'clear'
      operation becomes costly again.)
*)

module type Cacheable =
sig
  type t
  val hash : t -> int
  val sentinel : t
  val equal : t -> t -> bool
end

module type Result =
sig
  type t
  val sentinel : t
end

(** The Array_k modules (k = 2, 3, 4) below provide a small interface to arrays
    of k-tuples. However, for performance reasons, they are implemented as
    k-tuples of arrays of the same size. (The difference can be up to 10% on
    some benchmarks.)

    Note that there used to be an even faster implementation (about 2% on the
    same benchmarks) as a flattened array of k times the size. It relied on a
    precise understanding of the low-level OCaml memory model (and the Obj
    module). However, that made some maintainers worry on the rare occasions
    they had to look at this file.
*)

module Array = struct
  include Stdlib.Array
  let clear : 'a t -> 'a -> unit
    = fun t a -> fill t 0 (length t) a
end

module Array_2 =
struct
  type ('a, 'b) t = 'a array * 'b array

  let (clear : ('a, 'b) t -> 'a -> 'b -> unit)
    = fun (ta, tb) a b -> Array.(clear ta a; clear tb b)

  let (make : int -> 'a -> 'b -> ('a, 'b) t)
    = fun size a b -> Array.(make size a, make size b)

  let (set : ('a, 'b) t -> int -> 'a -> 'b -> unit)
    = fun (ta, tb) i a b -> Array.(set ta i a; set tb i b)

  let (get0 : ('a, 'b) t -> int -> 'a)
    = fun (ta, _) i -> Array.get ta i

  let (get1 : ('a, 'b) t -> int -> 'b)
    = fun (_, tb) i -> Array.get tb i
end

module Array_3 =
struct
  type ('a, 'b, 'c) t = 'a array * 'b array * 'c array

  let (clear : ('a, 'b, 'c) t -> 'a -> 'b -> 'c -> unit)
    = fun (ta, tb, tc) a b c -> Array.(clear ta a; clear tb b; clear tc c)

  let (make : int -> 'a -> 'b -> 'c -> ('a, 'b, 'c) t)
    = fun size a b c -> Array.(make size a, make size b, make size c)

  let (set : ('a, 'b, 'c) t -> int -> 'a -> 'b -> 'c -> unit)
    = fun (ta, tb, tc) i a b c -> Array.(set ta i a; set tb i b; set tc i c)

  let (get0 : ('a, 'b, 'c) t -> int -> 'a)
    = fun (ta, _, _) i -> Array.get ta i

  let (get1 : ('a, 'b, 'c) t -> int -> 'b)
    = fun (_, tb, _) i -> Array.get tb i

  let (get2 : ('a, 'b, 'c) t -> int -> 'c)
    = fun (_, _, tc) i -> Array.get tc i
end

module Array_4 =
struct
  type ('a, 'b, 'c, 'd) t = 'a array * 'b array * 'c array * 'd array

  let (clear : ('a , 'b , 'c , 'd) t -> 'a -> 'b -> 'c -> 'd -> unit)
    = fun (ta, tb, tc, td) a b c d ->
      Array.(clear ta a; clear tb b; clear tc c; clear td d)

  let (make : int -> 'a -> 'b -> 'c -> 'd -> ('a , 'b , 'c , 'd) t)
    = fun size a b c d ->
      Array.(make size a, make size b, make size c, make size d)

  let (set : ('a, 'b, 'c, 'd) t -> int -> 'a -> 'b -> 'c -> 'd -> unit)
    = fun (ta, tb, tc, td) i a b c d ->
      Array.(set ta i a; set tb i b; set tc i c; set td i d)

  let (get0 : ('a, 'b, 'c, 'd) t -> int -> 'a)
    = fun (ta, _, _, _) i -> Array.get ta i

  let (get1 : ('a, 'b, 'c, 'd) t -> int -> 'b)
    = fun (_, tb, _, _) i -> Array.get tb i

  let (get2 : ('a, 'b, 'c, 'd) t -> int -> 'c)
    = fun (_, _, tc, _) i -> Array.get tc i

  let (get3 : ('a, 'b, 'c, 'd) t -> int -> 'd)
    = fun (_, _, _, td) i -> Array.get td i
end

module Symmetric_Binary (H: Cacheable) (R: Result) =
struct

  let create () =
    let size = !cache_size in
    let array = Array_3.make size H.sentinel H.sentinel R.sentinel in
    array, pred size

  let cache = lazy (create ())

  let clear () =
    if Lazy.is_val cache then
      let array, _ = Lazy.force cache in
      Array_3.clear array H.sentinel H.sentinel R.sentinel

  let hash = H.hash

  let merge f a0 a1 =
    let array, mask = Lazy.force cache in
    let a0', a1', h0, h1 =
      let h0 = hash a0 in
      let h1 = hash a1 in
      if h0 < h1
      then a0, a1, h0, h1
      else a1, a0, h1, h0
    in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in
    if H.equal (Array_3.get0 array has) a0'
    && H.equal (Array_3.get1 array has) a1'
    then
      Array_3.get2 array has
    else
      let result = f a0 a1 in
      Array_3.set array has a0' a1' result;
      result
end

module Arity_One (H: Cacheable) (R: Result) =
struct

  (* Creates the array used for the cache, and the mask to be used on keys
     according to the array size. *)
  let create () =
    let size = !cache_size in
    let array = Array_2.make size H.sentinel R.sentinel in
    array, pred size

  let cache = lazy (create ())

  let clear () =
    if Lazy.is_val cache then
      let array, _ = Lazy.force cache in
      Array_2.clear array H.sentinel R.sentinel

  let merge f a0 =
    let array, mask = Lazy.force cache in
    let h0 = H.hash a0 in
    let has = h0 land mask in
    if H.equal (Array_2.get0 array has) a0
    then
      Array_2.get1 array has
    else
      let result = f a0 in
      Array_2.set array has a0 result;
      result
end

module Arity_Two (H0: Cacheable) (H1: Cacheable) (R: Result) =
struct

  (* Creates the array used for the cache, and the mask to be used on keys
     according to the array size. *)
  let create () =
    let size = !cache_size in
    let array = Array_3.make size H0.sentinel H1.sentinel R.sentinel in
    array, pred size

  let cache = lazy (create ())

  let clear () =
    if Lazy.is_val cache then
      let array, _ = Lazy.force cache in
      Array_3.clear array H0.sentinel H1.sentinel R.sentinel

  let merge f a0 a1 =
    let array, mask = Lazy.force cache in
    let h0 = H0.hash a0 in
    let h1 = H1.hash a1 in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in
    if H0.equal (Array_3.get0 array has) a0
    && H1.equal (Array_3.get1 array has) a1
    then
      Array_3.get2 array has
    else
      let result = f a0 a1 in
      Array_3.set array has a0 a1 result;
      result
end

module Arity_Three (H0: Cacheable) (H1: Cacheable) (H2: Cacheable) (R: Result) =
struct

  (* Creates the array used for the cache, and the mask to be used on keys
     according to the array size. *)
  let create () =
    let size = !cache_size in
    let array =
      Array_4.make size H0.sentinel H1.sentinel H2.sentinel R.sentinel
    in
    array, pred size

  let cache = lazy (create ())

  let clear () =
    if Lazy.is_val cache then
      let array, _ = Lazy.force cache in
      Array_4.clear array H0.sentinel H1.sentinel H2.sentinel R.sentinel

  let merge f a0 a1 a2 =
    let array, mask = Lazy.force cache in
    let h0 = H0.hash a0 in
    let h1 = H1.hash a1 in
    let h2 = H2.hash a2 in
    let has = h0 + 117 * h1 + 2375 * h2 in
    let has = has land mask in
    if H0.equal (Array_4.get0 array has) a0
    && H1.equal (Array_4.get1 array has) a1
    && H2.equal (Array_4.get2 array has) a2
    then
      Array_4.get3 array has
    else
      let result = f a0 a1 a2 in
      Array_4.set array has a0 a1 a2 result;
      result
end


module Array_Bit =
struct
  let make size =
    let size = (size + 7) lsr 3 in
    Bytes.make size (char_of_int 0)

  let get s i =
    let c = i lsr 3 in
    let b = 1 lsl (i land 7) in
    (Char.code (Bytes.get s c)) land b <> 0

  let set s i v =
    let c = i lsr 3 in
    let b = 1 lsl (i land 7) in
    let oldcontents = Char.code (Bytes.get s c) in
    let newcontents =
      if v
      then b lor oldcontents
      else
        let mask = lnot b in
        oldcontents land mask
    in
    Bytes.set s c (Char.chr newcontents)

  let clear s =
    let zero = char_of_int 0 in
    Bytes.fill s 0 (Bytes.length s) zero
end

module Binary_Predicate (H0: Cacheable) (H1: Cacheable) =
struct

  (* Creates two arrays (one for the arguments, one for the boolean result),
     and the mask to be used on keys according to the array size. *)
  let create () =
    let size = !cache_size in
    let array_args = Array_2.make size H0.sentinel H1.sentinel in
    let array_result = Array_Bit.make size in
    array_args, array_result, pred size

  let cache = lazy (create ())

  let clear () =
    if Lazy.is_val cache then
      let array_args, array_result, _ = Lazy.force cache in
      Array_2.clear array_args H0.sentinel H1.sentinel;
      Array_Bit.clear array_result

  let merge f a0 a1 =
    let array_args, array_result, mask = Lazy.force cache in
    let has =
      let h0 = H0.hash a0 in
      let h1 = H1.hash a1 in
      599 * h0 + h1
    in
    let has = has land mask in
    if H0.equal (Array_2.get0 array_args has) a0
    && H1.equal (Array_2.get1 array_args has) a1
    then
      Array_Bit.get array_result has
    else
      let r = f a0 a1 in
      Array_2.set array_args has a0 a1;
      Array_Bit.set array_result has r;
      r
end

module Symmetric_Binary_Predicate (H0: Cacheable) =
struct

  (* Creates two arrays (one for the arguments, one for the boolean result),
     and the mask to be used on keys according to the array size. *)
  let create () =
    let size = !cache_size in
    let array_args = Array_2.make size H0.sentinel H0.sentinel in
    let array_result = Array_Bit.make size in
    array_args, array_result, pred size

  let cache = lazy (create ())

  let clear () =
    if Lazy.is_val cache then
      let array_args, array_result, _ = Lazy.force cache in
      Array_2.clear array_args H0.sentinel H0.sentinel;
      Array_Bit.clear array_result

  let hash = H0.hash

  let merge f a0 a1 =
    let array_args, array_result, mask = Lazy.force cache in
    let a0, a1, h0, h1 =
      let h0 = hash a0 in
      let h1 = hash a1 in
      if h0 < h1
      then a0, a1, h0, h1
      else a1, a0, h1, h0
    in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in
    if H0.equal (Array_2.get0 array_args has) a0
    && H0.equal (Array_2.get1 array_args has) a1
    then
      Array_Bit.get array_result has
    else
      let r = f a0 a1 in
      Array_2.set array_args has a0 a1;
      Array_Bit.set array_result has r;
      r
end
