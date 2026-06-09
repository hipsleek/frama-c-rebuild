(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let file = open_in "../logic_parser.mly"

module Strings = Set.Make(String)

let tokens = ref Strings.empty

let wildcards = ref Strings.empty

type state = Throw | Wildcard | Comment

let remove_comment in_comm s =
  let b = Buffer.create 5 in
  let token (in_comm, prev) c =
    if in_comm then begin
      match prev, c with
      | Some '*', '/' -> false, None
      | _ -> true, Some c
    end else begin
      match prev, c with
      | Some '/', '*' -> true, None
      | Some c1, c2 ->
        Buffer.add_char b c1; false, Some c2
      | _ -> false, Some c
    end
  in
  let (in_comm, last) = Seq.fold_left token (in_comm, None) (String.to_seq s) in
  if not in_comm then begin
    match last with
    | Some c -> Buffer.add_char b c
    | None -> ()
  end;
  in_comm, Buffer.contents b

let is_token_line s = String.length s >= 6 && String.sub s 0 6 = "%token"

let add_tokens s =
  let rec add_token s1 =
    Scanf.sscanf s1 " %[A-Za-z0-9_] %s@$"
      (fun kw tl ->
         if kw <> "" then begin
           tokens:=Strings.add kw !tokens;
           add_token tl
         end)
  in
  let s = String.sub s 7 (String.length s - 7) in
  let s =
    if String.contains s '>' then begin
      let idx = String.index s '>' in
      String.sub s (idx+1) (String.length s - idx - 1)
    end else s
  in add_token s

let wildcard_rules =
  [ "acsl_c_keyword";
    "any_identifier";
    "bs_keyword";
    "c_keyword";
    "identifier";
    "identifier_or_typename";
    "is_acsl_decl_or_code_annot";
    "is_acsl_other";
    "is_acsl_spec";
    "is_acsl_typename";
    "is_ext_spec";
    "keyword";
    "non_logic_keyword";
    "post_cond";
    "wildcard";
  ]

let find_rule_name s =
  let l = String.index s ':' in
  String.sub s 0 l

let is_wildcard_rule s =
  if String.contains s ':' then begin
    let rule = find_rule_name s in
    let res =
      List.mem rule wildcard_rules in
    res
  end else false

let is_other_rule s =
  if String.contains s ':' then begin
    let rule = find_rule_name s in
    not (List.mem rule wildcard_rules)
  end else false

let add_wildcards s =
  let s =
    if String.contains s ':' then begin
      let l = String.index s ':' in
      String.sub s (l+1) (String.length s - l - 1)
    end else s
  in
  let rec add_wildcard s =
    Scanf.sscanf s " | %s { %_s@} %s"
      (fun kw tl ->
         wildcards := Strings.add kw !wildcards;
         if tl <> "" then add_wildcard tl)
  in
  if s <> "" then
    try
      add_wildcard s
    with Scanf.Scan_failure _ | End_of_file -> ()

let () =
  try
    let state = Stack.create () in
    Stack.push Throw state;
    while true do
      let s = input_line file in
      let in_comm, s = remove_comment (Stack.top state = Comment) s in
      (* if we exit from comment, return to previous state and analyze line *)
      if not in_comm && Stack.top state = Comment then begin
        ignore (Stack.pop state);
      end;
      if is_token_line s then add_tokens s
      else if Stack.top state = Throw then begin
        if is_wildcard_rule s then begin
          Stack.clear state;
          Stack.push Wildcard state;
          add_wildcards s;
        end
      end
      else if Stack.top state = Wildcard then begin
        if is_other_rule s then begin
          Stack.clear state;
          Stack.push Throw state;
        end
        else begin
          add_wildcards s;
        end;
      end;
      (* if we have started an unfinished comment,
         pass in comment mode for the next line. *)
      if in_comm && Stack.top state <> Comment then begin
        Stack.push Comment state;
      end
    done
  with End_of_file -> ()

let whitelist =
  List.fold_right
    Strings.add
    [ "EOF" ]
    Strings.empty

let () =
  let diff = Strings.diff (Strings.diff !tokens whitelist) !wildcards in
  if not (Strings.is_empty diff) then begin
    prerr_endline
      "Some tokens are not captured by wildcard rules. This will cause issue \
       if those tokens appear in a contract. Please add the following tokens \
       in the appropriate rule:";
    Strings.iter prerr_endline diff;
    exit 2
  end
