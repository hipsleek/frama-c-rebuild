(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(* Copied and modified from [cil/src/errormsg.ml] *)

(***** Handling parsing errors ********)
type parseinfo = {
  lexbuf : Lexing.lexbuf;
  menhir_pos: (Lexing.position * Lexing.position) MenhirLib.ErrorReports.buffer;
  mutable current_working_directory : Filepath.t option;
  mutable current_origin : Filepos.t option;
  origins : Filepos.t Datatype.Int.Hashtbl.t;
}

let current = ref None

let startParsing fname lexer =
  (* We only support one open file at a time *)
  match !current with
  | Some { lexbuf } ->
    Kernel.fatal
      "[Errorloc.startParsing] supports only one open file: \
       You want to open %S and %S is still open"
      fname (Lexing.lexeme_start_p lexbuf).Lexing.pos_fname
  | None ->
    let scan_references = Kernel.EagerLoadSources.get () in
    let path = Filepath.of_string fname in
    match Parse_env.open_source ~scan_references path with
    | Error msg -> Kernel.fatal "%s" msg
    | Ok in_str ->
      let lexbuf = Lexing.from_string in_str in
      let menhir_pos, lexer = MenhirLib.ErrorReports.wrap lexer in
      (* Initialize lexer buffer. *)
      lexbuf.Lexing.lex_curr_p <-
        { Lexing.pos_fname = Filepath.to_string_abs path;
          Lexing.pos_lnum  = 1;
          Lexing.pos_bol   = 0;
          Lexing.pos_cnum  = 0
        };
      current := Some
          { lexbuf;
            menhir_pos;
            current_working_directory = None;
            current_origin = None;
            origins = Datatype.Int.Hashtbl.create 100;
          };
      lexbuf, lexer

let finishParsing () =
  match !current with
  | None -> Kernel.fatal "Parsing called while lexbuf is empty"
  | Some _ -> current := None

let find_origin current line =
  Datatype.Int.Hashtbl.find_opt current.origins line

let set_current_origin current origin =
  let line = current.lexbuf.lex_curr_p.pos_lnum in
  current.current_origin <- Some origin;
  Datatype.Int.Hashtbl.replace current.origins line origin

(* Call this function to announce a new line *)
let newline () =
  let current = Option.get !current in
  Lexing.new_line current.lexbuf;
  let new_origin = Option.map Filepos.incr_line current.current_origin in
  Option.iter (set_current_origin current) new_origin

let setCurrentWorkingDirectory fp =
  let current = Option.get !current in
  current.current_working_directory <- Some fp

(* preprocessors tend to use '<xxx>' filenames in line directives to
   denote special locations, e.g. builtin or command-line-defined macros.
   Worse, this can get localized. We are thus a bit liberal in what we
   consider special filenames.
*)
let is_special_file n =
  let len = String.length n in
  (* not sure an empty string can realistically happen here,
     but it can't hurt to check. *)
  len = 0 || n.[0] = '<' && n.[len-1] = '>'

let setCurrentLine ?(filename: string option) (line: int) =
  let current = Option.get !current in
  match filename with
  | Some filename ->
    let base = current.current_working_directory in
    let path = Filepath.of_string ?base filename in
    if not (is_special_file filename) && not (Filesystem.exists path) then
      (* do not change line number if directive refers to non-existing file *)
      Kernel.warning ~wkey:Kernel.wkey_line_directive ~once:true
        "ignoring non-existing file '%a', referenced in a line directive"
        Filepath.pretty path
    else
      let new_origin =
        (* The line directive have been processed, we already are on the next
           line. Subtract 1 to have the actual inclusion line. *)
        let inclusion_line = current.lexbuf.lex_curr_p.pos_lnum - 1 in
        match find_origin current inclusion_line with
        | Some origin -> Filepos.update_line ~path ~line origin
        | None -> Filepos.make ~path ~line ()
      in
      set_current_origin current new_origin
  | None ->
    match current.current_origin with
    | None ->
      (* Some test use #line directive to reduce changes in non-regression
         tests. We update the current line to support this behavior, but the
         position is likely meaningless. *)
      current.lexbuf.lex_curr_p <-
        { current.lexbuf.lex_curr_p with
          pos_lnum = line;
          pos_bol = current.lexbuf.lex_curr_p.pos_cnum;
        }
    | Some origin ->
      let new_origin = Filepos.update_line ~line origin in
      set_current_origin current new_origin

let convert_pos pos =
  let open Option.Operators in
  let origin =
    let* current = !current in
    let+ origin = find_origin current (pos.Lexing.pos_lnum) in
    Filepos.Preprocessed origin
  in
  Filepos.of_lexing_pos ?origin pos

let convert_loc (pos_start, pos_end) =
  convert_pos pos_start, convert_pos pos_end

(* Precise error locations points to the output of the preprocessing phase.
   Some work needs to be done to find the matching location in the original
   source. It is not always possible however, as when macro are used in the
   original source file, there may be no obvious location in the original file
   matching a location in the preprocessed file. *)

(* Get the matching column in [dst_line] from a column [col] in [src_line]
   while ignoring space differences. Return None if there are differences other
   than spaces. *)
let matching_column src_line dst_line col =
  (* is_blank is to be replaced with {!Char.Ascii.is_blank} when the minimal
     supported OCaml is 5.4 *)
  let is_blank c =
    c = ' ' || c = '\t'
  in
  let rec aux i j =
    if i = col
    then Some j (* We found the matching column *)
    else if i >= String.length src_line || j >= String.length dst_line
    then None  (* Column is outside of line *)
    else
      match src_line.[i], dst_line.[j] with
      | s, d when is_blank s && is_blank d ->
        (* Both characters are blank, try to eat more blanks before continuing *)
        if i + 1 < String.length src_line && is_blank src_line.[i + 1]
        then aux (i + 1) j
        else if j + 1 < String.length dst_line && is_blank dst_line.[j + 1]
        then aux i (j + 1)
        else aux (i + 1) (j + 1)
      | s, d when s = d ->
        (* Matching characters, continue *)
        aux (i + 1) (j + 1)
      | _, _ ->
        (* Unmatching characters, probably a C macro *)
        None
  in
  aux 0 0

exception LineNotFound of { path: Filepath.t; line: int }

let matching_pos ~orig_lines ~ppc_lines pos =
  let find_line pos lines =
    let line = Filepos.input_line pos in
    try List.assoc line lines
    with Not_found ->
      let path = Filepos.input_path pos in
      raise (LineNotFound {path; line})
  in
  let orig_pos = Filepos.original pos in
  let ppc_column = Filepos.input_column pos in
  let orig_string = find_line orig_pos orig_lines
  and ppc_string = find_line pos ppc_lines in
  let open Option.Operators in
  let+ column = matching_column ppc_string orig_string ppc_column in
  Filepos.update_column ~column orig_pos

let matching_loc ~orig_lines ~ppc_lines loc =
  let start_pos, end_pos = loc in
  let open Option.Operators in
  let+ start_col = matching_pos ~orig_lines ~ppc_lines start_pos
  and+ end_col = matching_pos ~orig_lines ~ppc_lines end_pos in
  start_col, end_col

(* Replace non-blanks characters with spaces. Keep tabulations. *)
let replace_chars_with_blanks s =
  String.map (function '\t' -> '\t' | _ -> ' ') s

let min_and_max x y =
  if x <= y then x, y else y, x

let get_context_from_file ~ctx loc =
  let start_path = Filepos.input_path (fst loc)
  and end_path = Filepos.input_path (snd loc)
  and start_line = Filepos.input_line (fst loc)
  and end_line = Filepos.input_line (snd loc) in
  assert (Filepath.equal start_path end_path);
  (* Reorder if necessary *)
  let start_line, end_line = min_and_max start_line end_line in
  (* Extract lines *)
  let start_ctx = max 0 (start_line - ctx)
  and end_ctx = end_line + ctx in
  let lines = ref [] in
  let add_line i line = lines := (i, line) :: !lines in
  Filesystem.iter_line_range start_path start_ctx end_ctx add_line;
  (* Return the result *)
  List.rev !lines

let pp_context fmt (loc,lines) =
  let numbering = not (Filepos.is_preprocessed (fst loc)) in
  let pp_line fmt i =
    if numbering
    then Format.fprintf fmt "%-6d" i
    else Format.fprintf fmt "%6s" ""
  and pp_line_range fmt (i,j) =
    if numbering
    then Format.fprintf fmt "%d-%d" i j
    else Format.fprintf fmt "%6s" ""
  in
  let start_pos, end_pos = loc in
  (* The difference between the first and last error lines can be very
      large; in this case, we print only the first and last [inner_context]
      lines, with "..." between them. *)
  let inner_context = 4 in
  let start_line = Filepos.input_line start_pos
  and end_line = Filepos.input_line end_pos in
  let start_line, end_line = min_and_max start_line end_line in (* reorder *)
  let pp_line (i,line) =
    (* If more than one line of error, print blank line after the error loc *)
    if start_line <> end_line && i = end_line + 1 then
      Format.fprintf fmt "\n";
    (* Print or omit the line *)
    if i < start_line + inner_context || i > end_line - inner_context then
      Format.fprintf fmt "%a%s\n" pp_line i line
    else if i = start_line + inner_context then
      Format.fprintf fmt "%a [... omitted ...]\n"
        pp_line_range (start_line + inner_context, end_line - inner_context);
    (* If more than one line of error, print blank line after the error loc *)
    if start_line <> end_line && i = start_line - 1 then
      Format.fprintf fmt "\n";
    (* If exactly one line of error, print upward arrow *)
    if start_line = end_line && i = end_line then
      let start_col = Filepos.input_column start_pos - 1
      and end_col = Filepos.input_column end_pos - 1 in
      let arrows =
        if start_col < 0 || end_col < 0 then (* columns are not available *)
          String.make (String.length line) '^'
        else
          let start_col, end_col = min_and_max start_col end_col in
          let start_col = min start_col (String.length line) in
          replace_chars_with_blanks (String.sub line 0 start_col) ^
          String.make (max 1 (end_col - start_col)) '^'
      in
      Format.fprintf fmt "%6s%s\n" "" arrows
  in
  List.iter pp_line lines

(* Prints the line(s) between start_pos and pos,
   plus up to [ctx] lines before and after (if they exist),
   similar to 'grep -C<ctx>'.
   Most exceptions are silently caught and printing is stopped if they occur. *)
let pp_context_from_file ?(ctx=2) fmt ppc_loc =
  let (ppc_start, ppc_end) = ppc_loc in
  try
    match Filepos.origin ppc_start, Filepos.origin ppc_end with
    | (Unknown | Generated _), _ | _, (Unknown | Generated _) ->
      (* Do not print context for unknown locations *)
      ()

    | Preprocessed orig_start, Preprocessed orig_end
      when Filepath.equal (Filepos.path orig_start) (Filepos.path orig_end) ->
      (* [loc] is a location in a preprocessed file *)
      let orig_loc = orig_start, orig_end in
      let orig_lines = get_context_from_file ~ctx orig_loc in
      if Filepos.line (fst orig_loc) <> Filepos.line (snd orig_loc) then
        (* The original location is on several lines, only print the original
           code. *)
        pp_context fmt (orig_loc, orig_lines)
      else begin
        (* The location is on a line coming from preprocessed code.
           Remark: if, for any reason, the location in the preprocessed file is
           not on one line, the matching algorithm will likely return None. *)
        let ppc_lines = get_context_from_file ~ctx:0 ppc_loc in
        match matching_loc ~orig_lines ~ppc_lines ppc_loc with
        | Some orig_loc ->
          (* Found a match for the columns in the original file: only show
             the original file. *)
          pp_context fmt (orig_loc,orig_lines)
        | None ->
          (* Failed to match either the start or end column in the original
             file: show both the preprocesssed then the original file. *)
          pp_context fmt (ppc_loc, ppc_lines);
          Format.fprintf fmt "in code pre-processed from the original source\n";
          pp_context fmt (orig_loc, orig_lines)
        | exception LineNotFound { path; line } ->
          (* Line not found in one of the files: show the original file *)
          pp_context fmt (orig_loc, orig_lines);
          Kernel.warning "end of file %a reached before line %d"
            Filepath.pretty path line;
      end

    | _ ->
      (* The code is not preprocessed or the location is overlapping two
         files: show the input/preprocessed file. *)
      let ppc_loc =
        (* In the rare case of different file for start and end of location,
           only keep the end of the location. *)
        if Filepath.equal
            (Filepos.input_path ppc_start) (Filepos.input_path ppc_end)
        then ppc_start, ppc_end
        else ppc_end, ppc_end
      in
      let ppc_lines = get_context_from_file ~ctx ppc_loc in
      pp_context fmt (ppc_loc, ppc_lines)
  with
  | Sys_error msg ->
    Kernel.warning "%s" msg

let parse_error ?loc msg =
  let current = Option.get !current in
  (* there are cases when we are called before menhir has requested at
     least two tokens, ending up in an assertion failure. Unfortunately,
     ErrorReports API does not allow us to check whether the buffer is
     empty or not.
  *)
  let all_pos = Stack.create() in
  let () =
    (* this is absolutely not a hack and used MenhirLib exactly as intended. *)
    try
      let pp loc = Stack.push loc all_pos; "" in
      ignore (MenhirLib.ErrorReports.show pp current.menhir_pos)
    with _ -> ()
  in
  let loc =
    match loc with
    | Some loc -> loc
    | None ->
      if Stack.is_empty all_pos then
        convert_loc (current.lexbuf.lex_start_p, current.lexbuf.lex_curr_p)
      else
        let _,start_pos = Stack.pop all_pos in
        let last_pos =
          if Stack.is_empty all_pos then
            current.lexbuf.Lexing.lex_curr_p
          else
            fst (Stack.pop all_pos)
        in
        convert_loc (start_pos, last_pos)
  in
  let pretty_token fmt token =
    (* prints more detailed information around the erroneous token;
       due to the fact that some tokens are normalized (e.g. single-line ACSL
       comments), we blacklist them to avoid confusing the user *)
    let blacklist = ["*/"] in
    if List.mem token blacklist then ()
    else
      Format.fprintf fmt ",@ before or at token: %s" token
  in
  Format.kasprintf (fun str ->
      Kernel.feedback ~source:(fst loc) "%s:@." str
        ~append:(fun fmt ->
            Format.fprintf fmt "Location: @[<hv>%a%a@]\n"
              Fileloc.pretty_long_with_inclusions loc
              pretty_token (Lexing.lexeme current.lexbuf);
            Format.fprintf fmt "%a@."
              (pp_context_from_file ~ctx:2) loc);
      raise (Log.AbortError "kernel"))
    msg


(* More parsing support functions: line, file, char count *)
let currentLoc () =
  let current = Option.get !current in
  let start_pos = Lexing.lexeme_start_p current.lexbuf
  and end_pos = Lexing.lexeme_end_p current.lexbuf in
  convert_loc (start_pos, end_pos)


(** Handling of errors during parsing *)

let abort_context ?(loc=Current_loc.get ()) msg =
  let append fmt =
    Format.pp_print_newline fmt ();
    pp_context_from_file fmt loc
  in
  Kernel.abort ~source:(fst loc) ~append msg

let hadErrors = ref false
let had_errors () = !hadErrors
let clear_errors () = hadErrors := false

let set_error (_:Log.event) = hadErrors := true

let () =
  Kernel.register Log.Error set_error;
  Kernel.register Log.Failure set_error

(* Deprecated *)
let pp_location = Fileloc.pretty_long
