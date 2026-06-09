(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Supported styles *)

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Orange

type style =
  | Bold
  | Faint
  | Italic
  | Underline
  | Blink
  | Strike
  | Foreground of (color [@compare fun _ _ -> 0]) (* Ignore colors comparison *)
  | Background of (color [@compare fun _ _ -> 0])
[@@deriving ord]

type Format.stag += Style_tag of style


(* Encoding *)

let encode_color = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
  | Orange -> 208

let encode_style_start = function
  | Bold -> "1"
  | Faint -> "2"
  | Italic -> "3"
  | Underline -> "4"
  | Blink -> "5"
  | Strike -> "9"
  | Foreground c ->
    let i = encode_color c in
    if i < 8
    then string_of_int (i + 30)
    else "38;5;" ^ string_of_int i
  | Background c ->
    let i = encode_color c in
    if i < 8
    then string_of_int (i + 40)
    else "48;5;" ^ string_of_int i

let encode_style_end = function
  | Bold | Faint -> "22"
  | Italic -> "23"
  | Underline -> "24"
  | Blink -> "25"
  | Strike -> "29"
  | Foreground _ -> "39"
  | Background _ -> "49"

let escape_sequence = Printf.sprintf "\x1B[%sm"

let reset_sequence = escape_sequence "0"

(* Current state of style *)

module StyleSet = Set.Make (struct
    type t = style
    let compare = compare_style
  end)

module State =
struct
  type state = StyleSet.t
  type t = state Stack.t

  let init = Stack.create
  let reset = Stack.clear

  let push (stack : t) (style : style) : unit =
    let prev = Stack.top_opt stack |> Option.value ~default:StyleSet.empty in
    let current = StyleSet.add style prev in
    Stack.push current stack

  (* If a style must be maintained after pop, it is returned.
     This functions relies on StyleSet.compare = compare_style, which must
     ignore the color attribute on Foreground and Background. This way, when
     looking for a foreground (resp. background) color in the previous state,
     we find the original foreground (resp. background) style, even if the
     color does not match; the found color is the original color to be restored.
  *)
  let pop (stack : t) (style : style) : style option =
    ignore @@ Stack.pop_opt stack; (* In particular, ignore empty stacks *)
    let previous = Stack.top_opt stack in
    Option.bind (StyleSet.find_opt style) previous
end

let open_style state style =
  State.push state style;
  style |> encode_style_start |> escape_sequence

let close_style state style =
  match State.pop state style with
  | Some new_style ->
    new_style |> encode_style_start |> escape_sequence
  | None ->
    style |> encode_style_end |> escape_sequence

(* Format semantic tags *)

let stylemap = Hashtbl.create 24
let add_style a sty = Hashtbl.add stylemap a sty
let find_style = Hashtbl.find stylemap
let remove_style = Hashtbl.remove stylemap

let styles = [
  "bold", Bold ;
  "faint", Faint ;
  "italic", Italic ;
  "underline", Underline ;
  "blink", Blink ;
  "strike", Strike ;
]

let colors = [
  "black", Black ;
  "red", Red ;
  "green", Green ;
  "yellow", Yellow ;
  "blue", Blue ;
  "magenta", Magenta ;
  "cyan", Cyan ;
  "white", White ;
  "orange", Orange ;
]

let populate () =
  begin
    List.iter (fun (a,sty) -> Hashtbl.add stylemap a sty) styles ;
    List.iter
      (fun (a,color) ->
         let fg = Foreground color in
         let bg = Background color in
         Hashtbl.add stylemap a fg ;
         Hashtbl.add stylemap ("fg:" ^ a) fg ;
         Hashtbl.add stylemap ("bg:" ^ a) bg ;
      ) colors ;
  end

let reset_styles () =
  begin
    Hashtbl.clear stylemap ;
    populate () ;
  end

let styles_of_stag = function
  | Format.String_tag s ->
    String.lowercase_ascii s
    |> String.split_on_char ','
    |> List.map find_style
  | Style_tag style -> [style]
  | _ -> raise Not_found

let mark_open_stag state fallback stag =
  try
    styles_of_stag stag
    |> List.map (open_style state)
    |> String.concat ""
  with Not_found -> fallback stag

let mark_close_stag state fallback stag =
  try
    styles_of_stag stag
    |> List.rev (* states must be popped in reverse order *)
    |> List.map (close_style state)
    |> String.concat ""
  with Not_found -> fallback stag

let is_supported () =
  match Sys.getenv "TERM" with
  | exception Not_found | "dumb" | "" -> false
  | _  -> true

let enable_on ?(fallback=false) formatter =
  let state = State.init () in
  let reset () =
    State.reset state;
    Format.pp_print_string formatter reset_sequence
  in
  populate () ;
  Format.pp_set_mark_tags formatter true ;
  let old = Format.pp_get_formatter_stag_functions formatter () in
  let dopen,dclose =
    if fallback then old.mark_open_stag, old.mark_close_stag
    else let notag _ = "" in notag,notag in
  Format.pp_set_formatter_stag_functions formatter
    { old with
      mark_open_stag = mark_open_stag state dopen ;
      mark_close_stag = mark_close_stag state dclose } ;
  reset

(* Tests *)

let run_test format output =
  let buffer = Buffer.create 42 in
  let fmt = Format.formatter_of_buffer buffer in
  let reset = enable_on fmt in
  Format.fprintf fmt format;
  reset ();
  Format.pp_print_flush fmt ();
  let result = Buffer.contents buffer in
  let success = result = output in
  if not success then
    Format.eprintf "wrong output: %S given, %S expected@."
      result output;
  success

let%test _ = run_test
    "a@{<red>b@}c" "a\x1B[31mb\x1B[39mc\x1B[0m"

let%test _ = run_test (* unterminated stag *)
    "a@{<red>bc" "a\x1B[31mbc\x1B[0m"

let%test _ = run_test (* combined stags *)
    "a@{<red,bold>b@}c" "a\x1B[31m\x1B[1mb\x1B[22m\x1B[39mc\x1B[0m"
