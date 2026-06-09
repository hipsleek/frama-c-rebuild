(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Useful operations.
    This module does not depend of any of frama-c module. *)

val nop: 'a -> unit
[@@deprecated "Use ignore instead."]
[@@migrate { repl = ignore }]
(** Do nothing. *)

val adapt_filename: string -> string
(** Ensure that the given filename has the extension "cmo" in bytecode
    and "cmxs" in native *)

val max_cpt: int -> int -> int
(** [max_cpt t1 t2] returns the maximum of [t1] and [t2] wrt the total ordering
    induced by tags creation. This ordering is defined as follows:
    forall tags t1 t2, t1 <= t2 iff t1 is before t2 in the finite sequence
    [0; 1; ..; max_int; min_int; min_int-1; -1] *)

val number_to_color: int -> int

(* ************************************************************************* *)
(** {2 Function builders} *)
(* ************************************************************************* *)

exception Unregistered_function of string
(** Never catch it yourself: let the kernel do the job.
    @since Oxygen-20120901 *)

val mk_labeled_fun: string -> 'a
(** To be used to initialized a reference over a labeled function.
    @since Oxygen-20120901
    @raise Unregistered_function when not properly initialized *)

val mk_fun: string -> ('a -> 'b) ref
(** Build a reference to an uninitialized function
    @raise Unregistered_function when not properly initialized *)

(* ************************************************************************* *)
(** {2 Function combinators} *)
(* ************************************************************************* *)

val ($) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
[@@deprecated "Use Fun.Operators.($) or Fun.compose instead."]
[@@migrate { repl = Fun.compose } ]
(** Composition. *)

val uncurry: ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
[@@deprecated "Use Fun.uncurry2 instead."]
[@@migrate { repl = Fun.uncurry2 } ]

val iter_uncurry2:
  (('a -> 'b -> unit) -> 'c -> unit) ->
  (('a * 'b -> unit) -> 'c -> unit)
[@@deprecated "This function will be removed after the next release."]
[@@migrate { repl = (fun iter f v -> iter (fun a b -> f (a, b)) v)}]

(* ************************************************************************* *)
(** {2 Tuples} *)
(* ************************************************************************* *)

val nest: 'b -> 'a * 'c -> ('a * 'b) * 'c
(** Nest the first argument with the first element of the pair given as second
    argument. *)

val flatten: ('a * 'b) * 'c -> 'a * 'b * 'c
(** Flatten the pairs into a triplet. *)

(* ************************************************************************* *)
(** {2 Lists} *)
(* ************************************************************************* *)

val as_singleton: 'a list -> 'a
[@@deprecated "Use List.as_singleton instead."]
[@@migrate { repl = List.as_singleton } ]
(** returns the unique element of a singleton list.
    @raise Invalid_argument on a non singleton list. *)

val last: 'a list -> 'a
[@@deprecated "Use List.last instead."]
[@@migrate { repl = List.last } ]
(** returns the last element of a list.
    @raise Invalid_argument on an empty list
    @since Nitrogen-20111001 *)

val replace: ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
[@@deprecated "Use List.replace instead."]
[@@migrate { repl = List.replace } ]
(** [replace cmp x l] replaces the first element [y] of [l] such that
    [cmp x y] is true by [x]. If no such element exists, [x] is added
    at the tail of [l].
    @since Neon-20140301
*)

val product_fold: ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
[@@deprecated "Use List.product_fold instead."]
[@@migrate { repl = List.product_fold } ]
(** [product f acc l1 l2] is similar to [fold_left f acc l12] with l12 the
    list of all pairs of an elt of [l1] and an elt of [l2]
*)

val product: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
[@@deprecated "Use List.product_map instead."]
[@@migrate { repl = List.product_map } ]
(** [product f l1 l2] applies [f] to all the pairs of an elt of [l1] and
    an element of [l2].
*)

val find_index: ('a -> bool) -> 'a list -> int
[@@deprecated "Use List.find_index instead."]
[@@migrate { repl = (fun f l ->
    List.find_index f l |> Option.get ~exn:Not_found) } ]
(** returns the index (starting at 0) of the first element verifying the
    condition
    @raise Not_found if no element in the list matches the condition
*)

val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
[@@deprecated "Use List.compare instead."]
[@@migrate { repl = List.compare } ]
(** Generic list comparison function, where the elements are compared
    with the specified function
    @since Boron-20100401 *)

val opt_of_list: 'a list -> 'a option
[@@deprecated "Use List.to_option instead."]
[@@migrate { repl = List.to_option } ]
(** converts a list with 0 or 1 element into an option.
    @raise Invalid_argument on lists with more than one argument
    @since Oxygen-20120901 *)

val subsets: int -> 'a list -> 'a list list
[@@deprecated "Use List.combinations instead."]
[@@migrate { repl = List.combinations } ]
(** [subsets k l] computes the combinations of [k] elements from list [l].
    E.g. subsets 2 [1;2;3;4] = [[1;2];[1;3];[1;4];[2;3];[2;4];[3;4]].
    This function preserves the order of the elements in [l] when
    computing the sublists. [l] should not contain duplicates.
    @since Aluminium-20160501 *)

val list_first_n : int -> 'a list -> 'a list
[@@deprecated "Use List.take instead."]
[@@migrate { repl = List.take } ]
(** [list_first_n n l] returns the first [n] elements of the list. Tail
    recursive.
    It returns an empty list if [n] is nonpositive and the whole list if [n] is
    greater than [List.length l].
    It is equivalent to [list_slice ~last:n l]. *)

val list_slice: ?first:int -> ?last:int -> 'a list -> 'a list
[@@deprecated "Use List.slice instead."]
[@@migrate { repl = List.slice } ]
(** [list_slice ?first ?last l] is equivalent to Python's slice operator
    (l[first:last]): returns the range of the list between [first] (inclusive)
    and [last] (exclusive), starting from 0.
    If omitted, [first] defaults to 0 and [last] to [List.length l].
    Negative indices are allowed, and count from the end of the list.
    [list_slice] never raises exceptions: out-of-bounds arguments are clipped,
    and inverted ranges result in empty lists.
    @since 18.0-Argon *)

val map_no_copy: ('a -> 'a) -> 'a list -> 'a list
[@@deprecated "Use List.map_no_copy instead."]
[@@migrate { repl = List.map_no_copy } ]
(** Like map but try not to make a copy of the list
    @since 30.0-Zinc *)

val map_no_copy_list: ('a -> 'a list) -> 'a list -> 'a list
[@@deprecated "Use List.concat_map_no_copy instead."]
[@@migrate { repl = List.concat_map_no_copy } ]
(** Like map but each call can return a list. Try not to make a copy of the list
    @since 30.0-Zinc *)

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

(** [merge f k a b]  returns
    - [None] if both [a] and [b] are [None]
    - [Some a'] (resp. [b'] if [b] (resp [a]) is [None]
      and [a] (resp. [b]) is [Some]
    - [f k a' b'] if both [a] and [b] are [Some]

    It is mainly intended to be used with Map.merge

    @since Oxygen-20120901
*)
val merge_opt:
  ('a -> 'b -> 'b -> 'b) -> 'a -> 'b option -> 'b option -> 'b option
[@@deprecated "Use Option.merge or replace map merges by closed_union from Map."]
[@@migrate { repl = (fun f k -> Option.merge (f k)) } ]

val opt_filter: ('a -> bool) -> 'a option -> 'a option
[@@deprecated "Use Option.filter instead."]
[@@migrate { repl = Option.filter } ]

val the: exn:exn -> 'a option -> 'a
[@@deprecated "Use Option.the instead."]
[@@migrate { repl = Option.get } ]
(** @raise Exn if the value is [None] and [exn] is specified.
    @raise Invalid_argument if the value is [None] and [exn] is not specified.
    @return v if the value is [Some v].
    @before 23.0-Vanadium [exn] was an optional argument.
*)

val opt_hash: ('a -> int) -> 'a option -> int
[@@deprecated "Use Option.hash instead."]
[@@migrate { repl = Option.hash } ]
(** @since Sodium-20150201 *)

val opt_map2: ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
[@@deprecated "Use Option.map2 instead."]
[@@migrate { repl = Option.map2 } ]
(** @return [f a b] if arguments are [Some a] and [Some b], orelse return
    [None].
    @since 24.0-Chromium *)

val opt_map_no_copy: ('a -> 'a) -> 'a option -> 'a option
[@@deprecated "Use Option.map_no_copy instead."]
[@@migrate { repl = Option.map_no_copy } ]
(** same as map_no_copy for options.
    @since 30.0-Zinc *)

(* ************************************************************************* *)
(** {2 Strings} *)
(* ************************************************************************* *)

val string_del_prefix: ?strict:bool -> string -> string -> string option
[@@deprecated "Use String.remove_prefix instead."]
[@@migrate { repl = String.remove_prefix } ]
(** [string_del_prefix ~strict p s] returns [None] if [p] is not a prefix of
    [s] and Some [s1] iff [s=p^s1].
    @since Oxygen-20120901 *)

val string_del_suffix: ?strict:bool -> string -> string -> string option
[@@deprecated "Use String.remove_suffix instead."]
[@@migrate { repl = String.remove_suffix } ]
(** [string_del_suffix ~strict suf s] returns [Some s1] when [s = s1 ^ suf]
    and None of [suf] is not a suffix of [s].
    @since Aluminium-20160501
*)

val make_unique_name:
  (string -> bool) -> ?sep:string -> ?start:int -> string -> int*string
(** [make_unique_name mem s] returns [(0, s)] when [(mem s)=false]
    otherwise returns [(n,new_string)] such that [new_string] is
    derived from [(s,sep,start)] and [(mem new_string)=false] and [n<>0]
    @since Oxygen-20120901 *)

val strip_underscore: string -> string
[@@deprecated "Use String.trim_underscore instead."]
[@@migrate { repl = String.trim_underscore } ]
(** remove underscores at the beginning and end of a string. If a string
    is composed solely of underscores, return the empty string

    @since 18.0-Argon
*)

(** Same as [String.escaped], but avoid escaping UTF8 characters encoded on
    several chars.
    @since 32.0-Germanium *)
val escape_non_utf8: string -> string
[@@deprecated "Use String.utf8_escaped instead."]
[@@migrate { repl = String.utf8_escaped } ]

(** Escape string for use in HTML tag. *)
val html_escape: string -> string
[@@deprecated "Use String.html_escape instead."]
[@@migrate { repl = String.html_escape } ]

(** [percent_encode s] returns the string [s] encoded so that it can be used
    as a path component in a HTML URL. All characters not on the list of
    unreserved characters in RFC3986 are percent-encoded. For instance the space
    character is converted to [%20].

    Cf. {{:https://datatracker.ietf.org/doc/html/rfc3986#section-2.3}} for the
    list of unreserved characters.

    @since 32.0-Germanium *)
val percent_encode: string -> string
[@@deprecated "Use String.percent_encode instead."]
[@@migrate { repl = String.percent_encode } ]

(** [format_string_of_stag stag] returns the string corresponding to [stag],
    or raises an exception if the tag extension is unsupported.

    @since 22.0-Titanium
*)
val format_string_of_stag: Format.stag -> string

(* ************************************************************************* *)
(** {2 Performance} *)
(* ************************************************************************* *)

external address_of_value: 'a -> int = "address_of_value" [@@noalloc]

(* ************************************************************************* *)
(** {2 System commands} *)
(* ************************************************************************* *)

val safe_at_exit : (unit -> unit) -> unit
(** Register function to call with [Stdlib.at_exit], but only
    for non-child process (fork). The order of execution is preserved
    {i wrt} ordinary calls to [Stdlib.at_exit]. *)

(* ************************************************************************* *)
(** {2 Comparison functions} *)
(* ************************************************************************* *)

(** Use this function instead of [Stdlib.compare], as this makes
    it easier to find incorrect uses of the latter *)
external compare_basic: 'a -> 'a -> int = "%compare"

(** Case-insensitive string comparison. Only ISO-8859-1 accents are handled.
    @since Silicon-20161101 *)
val compare_ignore_case: string -> string -> int
[@@deprecated "Use String.compare_ignore_case instead."]
[@@migrate { repl = String.compare_ignore_case } ]
