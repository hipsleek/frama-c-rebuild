(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let nop _ = ()

let adapt_filename f =
  let change_suffix ext =
    try Filename.chop_extension f ^ ext
    with Invalid_argument _ -> f ^ ext
  in
  change_suffix (if Dynlink.is_native then ".cmxs" else ".cmo")

(* [max_cpt t1 t2] returns the maximum of [t1] and [t2] wrt the total ordering
   induced by tags creation. This ordering is defined as follows:
   forall tags t1 t2,
   t1 <= t2 iff
   t1 is before t2 in the finite sequence
   [0; 1; ..; max_int; min_int; min_int-1; -1] *)
let max_cpt c1 c2 = max (c1 + min_int) (c2 + min_int) - min_int

let number_to_color n =
  let color = ref 0 in
  let number = ref n in
  for _i = 0 to 7 do
    color := (!color lsl 1) +
             (if !number land 1 <> 0 then 1 else 0) +
             (if !number land 2 <> 0 then 256 else 0) +
             (if !number land 4 <> 0 then 65536 else 0);
    number := !number lsr 3
  done;
  !color

(* ************************************************************************* *)
(** {2 Function builders} *)
(* ************************************************************************* *)

exception Unregistered_function of string

let mk_labeled_fun s =
  raise
    (Unregistered_function
       (Printf.sprintf "Function '%s' not registered yet" s))

let mk_fun s = ref (fun _ -> mk_labeled_fun s)

(* ************************************************************************* *)
(** {2 Function combinators} *)
(* ************************************************************************* *)

let ($) f g x = f (g x)

let uncurry f x = f (fst x) (snd x)

let iter_uncurry2 iter f v =
  iter (fun a b -> f (a, b)) v

(* ************************************************************************* *)
(** {2 Tuples} *)
(* ************************************************************************* *)

let nest b (a, c) = (a, b), c

let flatten ((a, b), c) = a, b, c

(* ************************************************************************* *)
(** {2 Lists} *)
(* ************************************************************************* *)

let as_singleton = List.as_singleton
let last = List.last
let replace = List.replace
let product_fold = List.product_fold
let product = List.product_map
let find_index f l = List.find_index f l |> Option.get ~exn:Not_found
let list_compare = List.compare
let opt_of_list = List.to_option
let subsets = List.combinations
let list_first_n = List.take
let list_slice = List.slice
let map_no_copy = List.map_no_copy
let map_no_copy_list = List.concat_map_no_copy

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

let merge_opt f k = Option.merge (f k)
let opt_filter = Option.filter
let the ~exn = Option.get ~exn
let opt_hash = Option.hash
let opt_map2 = Option.map2
let opt_map_no_copy = Option.map_no_copy

(* ************************************************************************* *)
(** {2 Performance} *)
(* ************************************************************************* *)

external address_of_value: 'a -> int = "address_of_value" [@@noalloc]

(* ************************************************************************* *)
(** System commands *)
(* ************************************************************************* *)

(*[LC] due to Unix.exec calls, at_exit might be cloned into child process
  and executed when they are canceled early.

  The alternative, such as registering an daemon that raises an exception,
  hence interrupting the process, might not work: child processes still need to
  run some daemons, such as [flush_all] which is registered by default. *)

let pid = Unix.getpid ()
let safe_at_exit f =
  at_exit
    begin fun () ->
      let child = Unix.getpid () in
      if child = pid then f ()
    end


(* ************************************************************************* *)
(** Strings *)
(* ************************************************************************* *)

let string_del_prefix = String.remove_prefix
let string_del_suffix = String.remove_suffix
let strip_underscore = String.trim_underscores
let escape_non_utf8 = String.utf8_escaped
let html_escape = String.html_escape
let percent_encode = String.percent_encode

let make_unique_name mem ?(sep=" ") ?(start=2) from =
  let rec build base id =
    let fullname = base ^ sep ^ string_of_int id in
    if mem fullname then build base (succ id) else id,fullname
  in
  if mem from then build from start else (0,from)

let format_string_of_stag = function
  | Format.String_tag tag -> tag
  | _ -> raise (Invalid_argument "unsupported tag extension")

(* ************************************************************************* *)
(** Comparison functions *)
(* ************************************************************************* *)

external compare_basic: 'a -> 'a -> int = "%compare"

let compare_ignore_case = String.compare_ignore_case
