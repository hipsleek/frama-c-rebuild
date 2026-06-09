(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let _test_pretty pretty x expected =
  let fmt = Format.str_formatter in
  let previous_margin = Format.pp_get_margin fmt () in
  Format.pp_set_margin fmt 20;
  Format.fprintf fmt "@[<hov 2>%a@]" pretty x;
  let result = Format.flush_str_formatter () in
  Format.pp_set_margin fmt previous_margin;
  let b = (result = expected) in
  if not b then
    Format.eprintf "Test failed.@.Given:@.%s@.Expected@.%s@." result expected;
  b

let%test "string list" =
  let x = [ "Lorem"; "ipsum"; "dolor" ] in
  let pretty = List.pretty Format.pp_print_string in
  _test_pretty pretty x
    "[ Lorem; ipsum;\n\
    \  dolor ]"

let%test "string array" =
  let x = [| "Lorem"; "ipsum"; "dolor" |] in
  let pretty = Array.pretty Format.pp_print_string in
  _test_pretty pretty x
    "[| Lorem; ipsum;\n\
    \  dolor |]"

let%test "string set" =
  let open Set.Make (String) in
  let x = empty |> add "Lorem" |> add "ipsum" |> add "dolor" in
  let pretty = pretty Format.pp_print_string in
  _test_pretty pretty x
    "{ Lorem; dolor;\n\
    \  ipsum }"

let%test "int,string map" =
  let open Map.Make (Int) in
  let x = empty |> add 1 "Lorem" |> add 2 "ipsum" |> add 3 "dolor" in
  let pretty = pretty Format.pp_print_int Format.pp_print_string in
  _test_pretty pretty x
    "{{ 1 ↦ Lorem; 2 ↦\n\
    \  ipsum; 3 ↦\n\
    \  dolor }}"

let%test "map without unicode" =
  let open Map.Make (Int) in
  let x = empty |> add 1 "Lorem" |> add 2 "ipsum" |> add 3 "dolor" in
  let pretty = pretty Format.pp_print_int Format.pp_print_string in
  Unicode.use_unicode false;
  Fun.protect ~finally:(fun () -> Unicode.use_unicode true) @@
  fun () -> _test_pretty pretty x
    "{{ 1 -> Lorem; 2 ->\n\
    \  ipsum; 3 ->\n\
    \  dolor }}"
