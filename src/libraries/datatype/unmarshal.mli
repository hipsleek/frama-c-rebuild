(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier BSD-3-Clause                                      *)
(*  Copyright (C)                                                             *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  All rights reserved.                                                      *)
(*                                                                            *)
(******************************************************************************)

(* caml_unmarshal by Ineffable Casters *)

(* Version 3.11.2.0 *)

(** Provides a function [input_val], similar in
    functionality to the standard library function [Marshal.from_channel].
    The main difference with [Marshal.from_channel] is that
    [input_val] is able to apply transformation functions on the
    values on the fly as they are read from the input channel.

    Because it has an abstract representation of the type,
    [input_val] is able to catch some inconsistencies that
    [Marshal.from_channel] cannot. It is therefore "more" type-safe,
    but only if it is always used in conditions where the static type
    attributed to the result by the type-checker agrees with the
    representation of the type passed as second argument to [input_val].
    No such verification is done by this module (this would require
    changes to the compiler).

    The sanity checks are not the primary purpose of [input_val],
    and it is possible for a bug where the representation of a value of
    the wrong type is passed to [input_val] to go undetected, just
    as this can happen with [Marshal.from_channel].
*)

type t =
  | Abstract
  | Structure of structure
  | Transform of t * (Obj.t -> Obj.t)
  | Return of t * (unit -> Obj.t)
  | Dynamic of (unit -> t)

and structure =
  | Sum of t array array
  | Dependent_pair of t * (Obj.t -> t)
  | Array of t

(** Type [t] is used to describe the type of the data to be read and
    the transformations to be applied to the data.

    [Abstract] is used to input a value without any checking or
    transformation (as [Marshal.from_channel] does).  In this case,
    you don't need to provide a precise description of the
    representation of the data.

    [Structure a] is used to provide a description of the representation
    of the data, along with optional transformation functions for
    parts of the data.

    [a] can be:
    - [Array(t)], indicating that
      the data is an array of values of the same type, each value being
      described by [t].
    - [Sum(c)] for describing a non-array type where [c] is
      an array describing the non-constant constructors of
      the type being described (in the order of their
      declarations in that type).  Each element of this latter array is an
      array of [t] that describes (in order) the fields of the
      corresponding constructor.
    - [Dependent_pair(e,f)] for instructing the unmarshaler to
      reconstruct the first component of a pair first, using [e] as
      its description, and to apply function [f] to this value in order
      to get the description of the pair's second component.

    The shape of [a] must match the shape
    of the representation of the type of the data being imported, or
    [input_val] may report an error when the data doesn't match the
    description.

    [Transform (u, f)] is used to specify a transformation function on
    the data described by [u].  [input_val] will read and rebuild the
    data as described by [u], then call [f] on that data and return
    the result returned by [f].

    [Return (u, f)] is the same as [Transform], except that the data
    is not rebuilt, and [()] is passed to [f] instead of the data.
    This is to be used when the transformation functions of [u] rebuild
    the data by side effects and the version rebuilt by [input_val] is
    irrelevant.

    [Dynamic f] is used to build a new description on the fly when a
    new data of the current type is encountered.
*)

val input_val : Channel.input -> t -> 'a
(** [input_val c t]
    Read a value from the input channel [c], applying the transformations
    described by [t].
*)

val null : Obj.t
(** recursive values cannot be completely formed at the time
    they are passed to their transformation function.  When traversing
    a recursive value, the transformation function must check the
    fields for physical equality to [null] (with the function [==])
    and avoid using any field that is equal to [null].
*)

val id : Obj.t -> Obj.t
(** Use this function when you don't want to change the value
    unmarshaled by input_val.  You can also use your own identity
    function, but using this one is more efficient. *)

(** {2 Convenience functions for describing transformations.} *)

val t_unit : t
val t_int : t
val t_string : t
val t_float : t
val t_bool : t
val t_int32 : t
val t_int64 : t
val t_nativeint : t

val t_record : t array -> t
val t_tuple : t array -> t
val t_list : t -> t
val t_ref : t -> t
val t_option : t -> t
val t_array : t -> t

val t_queue: t -> t

val t_hashtbl_unchangedhashs :t -> t -> t
val t_hashtbl_changedhashs :
  (int -> 'table) -> ('table -> 'key -> 'value -> unit) -> t -> t -> t

val t_set_unchangedcompares : t -> t
val t_map_unchangedcompares : t -> t -> t

(** {2 Functions for writing deserializers.} *)

val register_custom : string -> (Channel.input -> Obj.t) -> unit

val arch_sixtyfour : bool
val arch_bigendian : bool

val getword : Channel.input -> Int32.t
val read8s : Channel.input -> int
val read16s : Channel.input -> int
val read32s : Channel.input -> int
val read64s : Channel.input -> int
val read8u : Channel.input -> int
val read16u : Channel.input -> int
val read32u : Channel.input -> int
val read64u : Channel.input -> int
val readblock : Channel.input -> Obj.t -> int -> int -> unit
val readblock_rev : Channel.input -> Obj.t -> int -> int -> unit
