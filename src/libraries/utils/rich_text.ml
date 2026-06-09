(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Text enriched with semantic tags                                   --- *)
(* -------------------------------------------------------------------------- *)

type tag = {
  p : int ; (* first position *)
  q : int ; (* last position (excluded) *)
  tag : Format.stag ;
  children : tag list ;
}

type t = {
  plain : string;
  tags : tag list;
}

let empty = { plain = "" ; tags = [] }
let is_empty text = text.plain = "" && text.tags = []
let of_string plain =  { empty with plain }
let plain text = text.plain
let size text = String.length text.plain
let index text c = String.index text.plain c

let contains text c =
  String.contains text.plain c

let rec offset_tag ~limit ~offset tag =
  assert (offset <= limit);
  if tag.q < offset || tag.p >= limit
  then None (* Remove tags out of bounds *)
  else Some
      { p = max 0 (tag.p - offset);
        q = min tag.q limit - offset;
        tag = tag.tag;
        children = offset_tags ~limit ~offset tag.children;
      }
and offset_tags ~limit ~offset tags =
  List.filter_map (offset_tag ~limit ~offset) tags

let sub ?(start_pos=0) ?(end_pos) text =
  let n = size text in
  let end_pos = Option.value ~default:n end_pos |> min n in
  { plain = String.sub text.plain start_pos (end_pos - start_pos);
    tags = offset_tags ~limit:end_pos ~offset:start_pos text.tags;
  }

type truncation = [`None | `Left of int | `Middle of int | `Right of int]

let need_truncation ?(truncate : truncation = `None) text =
  let length = String.length text.plain in
  match truncate with
  | `None -> false
  | `Left size | `Middle size | `Right size -> size < length

let pretty ?(truncate : truncation = `None) ?(ellipsis="[...]") fmt text =
  let length = String.length text.plain in
  (* Truncate the text if requested
     truncate_start, truncate_end : position of the truncation ;
     length of truncation L = truncate_end - truncate_start ;
     total    size = (length - L) + ellipsis_length ;
     hence       L = length - size + ellipsis_length *)
  let truncate_start, truncate_end =
    match truncate with
    | `Left size | `Middle size | `Right size
      when size <= String.length ellipsis -> (0, length)

    | `Middle size when size < length ->
      let ellipsis_length = String.length ellipsis in
      let size_left = (size - ellipsis_length) / 2 in
      let size_right = ((size - ellipsis_length) - size_left) in
      (size_left, length - size_right)
    (* L = length - size_right - size_left *)
    (* L = length - ((size - ellipsis_length) - size_left) - size_left *)
    (* L = length - (size - ellipsis_length) + size_left - size_left *)
    (* L = length - size + ellipsis_length *)

    | `Right size when size < length ->
      let ellipsis_length = String.length ellipsis in
      (size - ellipsis_length, length)
    (* L = length - (size - ellipsis_length) *)
    (* L = length - size + ellipsis_length *)

    | `Left size when size < length ->
      let ellipsis_length = String.length ellipsis in
      (0, length - size + ellipsis_length)
    (* L = length - size + ellipsis_length *)

    | _ ->
      max_int, max_int (* Do not truncate until max_int, hopefully never *)
  in
  (* Output of a substring of the text from p (included) to q (excluded) *)
  (* Do replace by Format.pp_print_substring_as as soon as OCaml 5.1 is
     the minimal version supported by Frama-C *)
  let output_sub p q =
    if p < q then
      let s = String.sub text.plain p (q - p) in
      Format.pp_print_string fmt s
  in
  (* Output of a substring of the text, but with truncated contents if
     required. *)
  let output_truncated ~force_ellipsis p q =
    (* Is there no untersection between [p..q[ and
       [truncate_start..truncate_end[ ? *)
    if p >= truncate_end || q <= truncate_start then
      output_sub p q
    else begin
      output_sub p truncate_start;
      if force_ellipsis || p <= truncate_start && q >= truncate_end then
        Format.pp_print_string fmt ellipsis;
      output_sub truncate_end q;
    end
  in
  (* Iteration over the semantic tags of the text *)
  (* [with_ellipsis] tells whether to output ellpsis when truncating *)
  let rec aux ~force_ellipsis p q =
    function
    | [] -> output_truncated ~force_ellipsis p q
    | { tag ; p=tp ; q=tq ; children } :: tags ->
      if tp >= truncate_start && tq <= truncate_end then
        aux ~force_ellipsis p q tags
      else if q < tp then
        output_truncated ~force_ellipsis p q
      else if tq < p then
        aux ~force_ellipsis p q tags
      else begin
        output_truncated ~force_ellipsis p tp;
        Format.pp_open_stag fmt tag;
        aux ~force_ellipsis:false tp tq children;
        Format.pp_close_stag fmt ();
        let force_ellipsis =
          force_ellipsis || p <= truncate_start && q >= truncate_end
        in
        aux ~force_ellipsis tq q tags;
      end
  in
  aux ~force_ellipsis:true 0 (String.length text.plain) text.tags

let to_string ?prefix ?suffix ?(truncate:truncation = `None) ?ellipsis text =
  let length = match truncate with
    | `None -> size text | `Right size | `Middle size | `Left size -> size in
  let string_buffer = Buffer.create length in
  let fmt = Format.formatter_of_buffer string_buffer in
  Option.iter (fun f -> f fmt) prefix;
  pretty ~truncate ?ellipsis fmt text;
  Option.iter (fun f -> f fmt) suffix;
  Format.pp_print_flush fmt ();
  Buffer.contents string_buffer

(* -------------------------------------------------------------------------- *)
(* --- Buffers for building rich text                                     --- *)
(* -------------------------------------------------------------------------- *)

type buffer = {
  formatter : Format.formatter ; (* formatter on self (recursive) *)
  content : Buffer.t ;
  mutable revtags : tag list ; (* in reverse order *)
  mutable stack : (int * Format.stag * tag list) list ; (* opened tag positions *)
}

module Buffer =
struct
  let min_buffer = 128    (* initial size of buffer *)

  let is_blank = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

  let trim_begin buffer =
    let rec lookup_fwd text k n =
      if k < n && is_blank (Buffer.nth text k) then
        lookup_fwd text (succ k) n else k
    in lookup_fwd buffer.content 0 (Buffer.length buffer.content)

  let trim_end buffer =
    let rec lookup_bwd text k =
      if k > 0 && is_blank (Buffer.nth text (pred k)) then
        lookup_bwd text (pred k) else k
    in lookup_bwd buffer.content (Buffer.length buffer.content)

  let push_tag buffer tag =
    let p = Buffer.length buffer.content in
    buffer.stack <- ( p , tag, buffer.revtags ) :: buffer.stack ;
    buffer.revtags <- []

  let pop_tag buffer tag =
    match buffer.stack with
    | [] -> ()
    | (p,tag',tags)::stack ->
      assert (tag = tag');
      let q = Buffer.length buffer.content in
      buffer.stack <- stack ;
      let children = List.rev buffer.revtags in
      buffer.revtags <- { p ; q ; tag ; children } :: tags

  let rec pop_all buffer =
    match buffer.stack with
    | [] -> ()
    | (_,tag,_) :: _ ->
      pop_tag buffer tag;
      pop_all buffer


  (* --- External API --- *)

  let create ?indent ?margin () =
    let content = Buffer.create min_buffer in
    let fmt = Format.formatter_of_buffer content in
    let buffer = { formatter=fmt; content; revtags = [] ; stack = [] ; } in
    begin match indent , margin with
      | None , None -> ()
      | Some k , None ->
        let m = Format.pp_get_margin fmt () in
        Format.pp_set_max_indent fmt (max 0 (min k m))
      | None , Some m ->
        Format.pp_set_margin fmt (max 0 m) ;
        let k = Format.pp_get_max_indent fmt () in
        if k < m-10 then Format.pp_set_max_indent fmt (max 0 (m-10))
      | Some k , Some m ->
        Format.pp_set_margin fmt (max 0 m) ;
        Format.pp_set_max_indent fmt (max 0 (min k (m-10)))
    end ;
    Format.pp_set_formatter_stag_functions fmt Format.{
        print_open_stag = ignore ;
        print_close_stag = ignore ;
        mark_open_stag = (fun stag -> push_tag buffer stag; "") ;
        mark_close_stag = (fun stag -> pop_tag buffer stag; "") ;
      } ;
    Format.pp_set_mark_tags fmt true ;
    buffer

  let reset buffer =
    Buffer.reset buffer.content;
    buffer.revtags <- [];
    buffer.stack <- []

  let contents ?(trim=true) buffer =
    Format.pp_print_flush buffer.formatter ();
    (* The following lines requires that the formatter have been flushed *)
    pop_all buffer;
    if trim then
      let p = trim_begin buffer in
      let q = trim_end buffer in
      let plain =
        if p < q
        then Buffer.sub buffer.content p (q - p)
        else ""
      in
      let tags = List.rev buffer.revtags |> offset_tags ~limit:q ~offset:p in
      { plain ; tags }
    else
      let plain = Buffer.contents buffer.content
      and tags = List.rev buffer.revtags in
      { plain ; tags }

  let add_char buffer c =
    Format.pp_print_char buffer.formatter c

  let add_string buffer s =
    Format.pp_print_string buffer.formatter s

  let add_substring buffer s k n =
    Format.pp_print_string buffer.formatter (String.sub s k n)

  let bprintf buffer format =
    Format.fprintf buffer.formatter format

  let kbprintf kjob buffer format =
    Format.kfprintf kjob buffer.formatter format
end

let kmprintf ?indent ?margin ?trim kjob format =
  let buffer = Buffer.create ?indent ?margin () in
  let to_text _fmt =
    kjob (Buffer.contents ?trim buffer)
  in
  Buffer.kbprintf to_text buffer format

let mprintf ?indent ?margin ?trim format =
  kmprintf ?indent ?margin ?trim (Fun.id) format

let sprintf ?(indent=20) ?(margin=40) ?trim ?truncate ?ellipsis format =
  let to_string text =
    to_string ?truncate ?ellipsis text
  in
  kmprintf ~indent ~margin ?trim to_string format


(* -------------------------------------------------------------------------- *)
(* --- Tests                                                              --- *)
(* -------------------------------------------------------------------------- *)

let test_pretty ?(truncate=`Middle 12) format output =
  let prefix fmt = Format.pp_set_mark_tags fmt true in
  let text = mprintf format in
  let result = to_string ~prefix ~truncate text in
  let success = result = output in
  if not success then
    Format.eprintf "wrong output: '%s' given, '%s' expected@."
      result output;
  success

(* Test empty format *)
let%test _ = test_pretty "" ""

(* Basic test *)
let%test _ = test_pretty "01234" "01234"

(* Truncate size < ellipsis length *)
let%test _ = test_pretty ~truncate:(`Middle 2) "0123456789" "[...]"

(* truncation basic test *)
let%test _ = test_pretty "01234567890123456789" "012[...]6789"

(* Blank string *)
let%test _ = test_pretty " \t\r\n " ""

(* Basic trim *)
let%test _ = test_pretty "   01234  " "01234"

(* Basic trim and truncation *)
let%test _ = test_pretty "   01234567890123456789  " "012[...]6789"

(* Basic stag usage *)
let%test _ = test_pretty "0@{<a>12345678@}9" "0<a>12345678</a>9"

(* Missing closing stag *)
let%test _ = test_pretty "0@{<a>123456789" "0<a>123456789</a>"

(* Truncation with stags *)
let%test _ = test_pretty "0@{<a>123456789012345678@}9" "0<a>12[...]678</a>9"
let%test _ =
  test_pretty "0@{<a>123456@{<b>7890@}12345678@}9" "0<a>12[...]678</a>9"
let%test _ =
  test_pretty "012345@{<a>6@{<b>7890@}1@}23456789" "012[...]6789"
let%test _ =
  test_pretty "0@{<a>123456@}78901@{<b>2345678@}9" "0<a>12</a>[...]<b>678</b>9"
let%test _ =
  test_pretty ~truncate:(`Middle 17)
    "0@{<a>1@{<b>2@{<c>3@{<d>4@}5@}6@}789012@{<e>3@{<f>4@{<g>5@}6@}7@}8@}9"
    "0<a>1<b>2<c>3<d>4</d>5</c></b>[...]<e><f>4<g>5</g>6</f>7</e>8</a>9"
let%test _ =
  test_pretty ~truncate:(`Middle 17)
    "0@{<a>1@{<b>2@{<c>3@{<d>4@}5@}6@}789012@{<e>@{<f>@{<g>345@}6@}7@}8@}9"
    "0<a>1<b>2<c>3<d>4</d>5</c></b>[...]<e><f><g>45</g>6</f>7</e>8</a>9"
let%test _ =
  test_pretty ~truncate:(`Middle 17)
    "0@{<a>1@{<b>2@{<c>3@{<d>456@}@}@}789012@{<e>3@{<f>4@{<g>5@}6@}7@}8@}9"
    "0<a>1<b>2<c>3<d>45</d></c></b>[...]<e><f>4<g>5</g>6</f>7</e>8</a>9"

(* Trim with stags *)
let%test _ = test_pretty "  0@{<a>12345678@}9   " "0<a>12345678</a>9"

(* Trim and truncate with stags *)
let%test _ = test_pretty "0@{<a>123456789012345678@}9" "0<a>12[...]678</a>9"

(* Truncation on Middle *)
let%test _ =
  test_pretty ~truncate:(`Middle 12) "0123456789xxx9876543210" "012[...]3210"

(* Truncation on Left *)
let%test _ =
  test_pretty ~truncate:(`Left 12)   "0123456789xxx9876543210" "[...]6543210"

(* Truncation on Right *)
let%test _ =
  test_pretty ~truncate:(`Right 12)  "0123456789xxx9876543210" "0123456[...]"
