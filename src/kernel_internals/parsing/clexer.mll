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

(* FrontC -- lexical analyzer
**
** 1.0	3.22.99	Hugues Cassé	First version.
** 2.0  George Necula 12/12/00: Many extensions
*)
{
open Cparser
module H = Hashtbl
module E = Errorloc

let currentLoc () = E.currentLoc ()

let parse_error ?(loc=currentLoc()) msg =
  E.parse_error ~loc msg

(* Convert char into Int64 *)
let int64_of_char c = Int64.of_int (Char.code c)

let one_line_ghost = ref false
let is_oneline_ghost () = !one_line_ghost
let enter_oneline_ghost () = one_line_ghost := true
let exit_oneline_ghost () = one_line_ghost := false

let ghost_code = ref false
let is_ghost_code () = !ghost_code
let enter_ghost_code () = ghost_code := true
let exit_ghost_code () = ghost_code := false

let ghost_annot = ref false
let ghost_annot_start = ref Fileloc.unknown
let is_ghost_annot () = !ghost_annot
let get_ghost_annot_start () = !ghost_annot_start
let enter_ghost_annot () =
  ghost_annot := true;
  ghost_annot_start:= currentLoc()
let exit_ghost_annot () =
  ghost_annot := false;
  ghost_annot_start := Fileloc.unknown

let add_comment c = Cabshelper.Comments.add (currentLoc()) c




(*
** Keyword hashtable
*)

let lexicon = H.create 211
let add key builder = H.add lexicon key builder
let remove key = H.remove lexicon key
let is_c_keyword s = Hashtbl.mem lexicon s

(* Search for a keyword, default to variable name, as opposed to type *)
let scan_ident id =
  let here = currentLoc () in
  try (H.find lexicon id) here
  with Not_found -> IDENT id

let valid key builder =
  add key builder

let unsupported key =
  let msg f k = Format.fprintf f "%s is currently unsupported by Frama-C." k in
  add key (fun loc -> Kernel.abort ~source:(fst loc) "%a" msg key)

let compiler (name, is_machdep) key builder =
  let wkey = Kernel.wkey_conditional_feature in
  let warning loc =
    Kernel.warning ~wkey ~source:(fst loc)
      "%s is a %s extension, use a %s-based machdep to enable it."
        key name name ;
    IDENT key
  in
  add key (fun loc -> if is_machdep then builder loc else warning loc)

let gcc key builder = compiler ("GCC", Machine.gccMode ()) key builder
let msvc key builder = compiler ("MSVC", Machine.msvcMode ()) key builder

let c23_or_later key builder =
  if compare (Kernel.CStd.get()) Kernel.C23 >= 0
  then add key builder

let filename_keyword () =
  let convert acc c = int64_of_char c :: acc in
  let path (loc : Cil_types.location) = Filepos.path (fst loc) in
  let filename loc = Filepath.to_string (path loc) in
  let ints loc = List.rev (String.fold_left convert [] (filename loc)) in
  add "__FC_FILENAME__" (fun loc -> CST_STRING (ints loc, loc))

let init_lexicon () =
  H.clear lexicon ;
  Logic_env.reset_typenames () ;
  Logic_env.builtin_types_as_typenames () ;
  valid "auto" (fun loc -> AUTO loc) ;
  valid "const" (fun loc -> CONST loc) ;
  valid "__const" (fun loc -> CONST loc) ;
  valid "__const__" (fun loc -> CONST loc) ;
  valid "static" (fun loc -> STATIC loc) ;
  valid "extern" (fun loc -> EXTERN loc) ;
  valid "long" (fun loc -> LONG loc) ;
  valid "short" (fun loc -> SHORT loc) ;
  valid "register" (fun loc -> REGISTER loc) ;
  valid "signed" (fun loc -> SIGNED loc) ;
  valid "__signed" (fun loc -> SIGNED loc) ;
  valid "unsigned" (fun loc -> UNSIGNED loc) ;
  valid "volatile" (fun loc -> VOLATILE loc) ;
  valid "__volatile" (fun loc -> VOLATILE loc) ;
  (* WW: see /usr/include/sys/cdefs.h for why __signed and __volatile
   * are accepted GCC-isms *)
  valid "char" (fun loc -> CHAR loc) ;
  c23_or_later "bool" (fun loc -> BOOL loc) ;
  c23_or_later "true" (fun loc -> TRUE loc) ;
  c23_or_later "false" (fun loc -> FALSE loc) ;
  valid "_Bool" (fun loc -> BOOL loc) ;
  valid "int" (fun loc -> INT loc) ;
  valid "float" (fun loc -> FLOAT loc) ;
  valid "_Float32" (fun loc -> FLOAT32 loc) ;
  valid "double" (fun loc -> DOUBLE loc) ;
  valid "_Float64" (fun loc -> FLOAT64 loc) ;
  valid "void" (fun loc -> VOID loc) ;
  valid "enum" (fun loc -> ENUM loc) ;
  valid "struct" (fun loc -> STRUCT loc) ;
  valid "typedef" (fun loc -> TYPEDEF loc) ;
  valid "union" (fun loc -> UNION loc) ;
  valid "break" (fun loc -> BREAK loc) ;
  valid "continue" (fun loc -> CONTINUE loc) ;
  valid "goto" (fun loc -> GOTO loc) ;
  valid "return" (fun loc -> RETURN loc) ;
  valid "switch" (fun loc -> SWITCH loc) ;
  valid "case" (fun loc -> CASE loc) ;
  valid "default" (fun loc -> DEFAULT loc) ;
  valid "while" (fun loc -> WHILE loc) ;
  valid "do" (fun loc -> DO loc) ;
  valid "for" (fun loc -> FOR loc) ;
  valid "if" (fun loc -> IF loc) ;
  valid "else" (fun _ -> ELSE) ;
  c23_or_later "alignas" (fun loc -> ALIGNAS loc) ;
  valid "_Alignas" (fun loc -> ALIGNAS loc) ;
  c23_or_later "alignof" (fun loc -> ALIGNOF loc) ;
  valid "_Alignof" (fun loc -> ALIGNOF loc) ;
  valid "_Generic" (fun loc -> GENERIC loc) ;
  (*** Implementation specific keywords ***)
  valid "__signed__" (fun loc -> SIGNED loc) ;
  valid "__inline__" (fun loc -> INLINE loc) ;
  valid "inline" (fun loc -> INLINE loc) ;
  valid "__inline" (fun loc -> INLINE loc) ;
  valid "_Noreturn" (fun loc -> NORETURN loc) ;
  valid "_Static_assert" (fun loc -> STATIC_ASSERT loc) ;
  valid "__attribute__" (fun loc -> ATTRIBUTE loc) ;
  valid "__attribute" (fun loc -> ATTRIBUTE loc) ;
  valid "_Nullable" (fun loc -> NOP_ATTRIBUTE loc) ;
  valid "__blockattribute__" (fun _ -> BLOCKATTRIBUTE) ;
  valid "__blockattribute" (fun _ -> BLOCKATTRIBUTE) ;
  valid "__asm__" (fun loc -> ASM loc) ;
  valid "asm" (fun loc -> ASM loc) ;
  valid "__typeof__" (fun loc -> TYPEOF loc) ;
  valid "__typeof" (fun loc -> TYPEOF loc) ;
  valid "typeof" (fun loc -> TYPEOF loc) ;
  valid "__volatile__" (fun loc -> VOLATILE loc) ;
  valid "__volatile" (fun loc -> VOLATILE loc) ;
  valid "__FUNCTION__" (fun loc -> FUNCTION__ loc) ;
  valid "__func__" (fun loc -> FUNCTION__ loc) ; (* ISO 6.4.2.2 *)
  valid "__PRETTY_FUNCTION__" (fun loc -> PRETTY_FUNCTION__ loc) ;
  valid "__label__" (fun _ -> LABEL__) ;
  valid "__restrict" (fun loc -> RESTRICT loc) ;
  valid "restrict" (fun loc -> RESTRICT loc) ;
  valid "_Thread_local" (fun loc -> THREAD_LOCAL loc) ;
  (*** GCC ***)
  gcc   "__alignof__" (fun loc -> GCC_ALIGNOF loc) ;
  gcc   "__thread" (fun loc -> THREAD loc) ;
  gcc   "__int128" (fun loc -> INT128 loc) ;
  (**** MSVC ***)
  valid "__int64" (fun _ -> INT64 (currentLoc ())) ;
  valid "__int32" (fun loc -> INT loc) ;
  valid "_cdecl" ( fun _ -> MSATTR ("_cdecl", currentLoc ())) ;
  valid "__cdecl" (fun _ -> MSATTR ("__cdecl", currentLoc ())) ;
  valid "_stdcall" (fun _ -> MSATTR ("_stdcall", currentLoc ())) ;
  valid "__stdcall" (fun _ -> MSATTR ("__stdcall", currentLoc ())) ;
  valid "_fastcall" (fun _ -> MSATTR ("_fastcall", currentLoc ())) ;
  valid "__fastcall" (fun _ -> MSATTR ("__fastcall", currentLoc ())) ;
  valid "__w64" (fun _ -> MSATTR("__w64", currentLoc ())) ;
  valid "__declspec" (fun loc -> DECLSPEC loc) ;
  msvc  "__alignof" (fun loc -> ALIGNOF loc) ;
  (* !! we turn forceinline into inline *)
  valid "__forceinline" (fun loc -> INLINE loc) ;
  (* Some files produced by 'GCC -E' expect this type to be defined *)
  valid "__builtin_va_list" (fun _ -> NAMED_TYPE "__builtin_va_list") ;
  valid "__builtin_va_arg" (fun loc -> BUILTIN_VA_ARG loc) ;
  valid "__builtin_types_compatible_p" (fun loc -> BUILTIN_TYPES_COMPAT loc) ;
  valid "__builtin_offsetof" (fun loc -> BUILTIN_OFFSETOF loc) ;
  filename_keyword () ;
  (* The following C11/GNU extension tokens are not yet supported, so we
   provide some helpful error messages. Usage of 'fatal' instead of 'error'
   below prevents duplicate error messages due to parsing errors. *)
  unsupported "_Complex" ;
  unsupported "_Decimal32" ;
  unsupported "_Decimal64" ;
  unsupported "_Imaginary"



(* Mark an identifier as a type name. The old mapping is preserved and will
 * be reinstated when we exit this context *)
let add_type name =
  add name (fun _ -> NAMED_TYPE name) ;
  Logic_env.add_typename name

let remove_type name =
  remove name ; Logic_env.remove_typename name



let context : string list list ref = ref [ [] ]

let push_context _ = context := [] :: !context

let pop_context _ =
  match !context with
  | [] -> Kernel.fatal "Empty context stack"
  | con :: sub -> context := sub ; List.iter remove_type con

(* Mark an identifier as a variable name. The old mapping is preserved and
 * will be reinstated when we exit this context  *)
let add_identifier name =
  match !context with
  | [] -> Kernel.fatal "Empty context stack"
  | con :: sub ->
    context := (name :: con) :: sub ;
    add name (fun _ -> IDENT name) ;
    Logic_env.hide_typename name



let typedef_decl = ref false
let is_typedef () = !typedef_decl
let set_typedef () = typedef_decl := true
let reset_typedef () = typedef_decl := false



(*
** Buffer processor
*)

let init ~filename lexer =
  init_lexicon () ; E.startParsing filename lexer

let finish () =
  E.finishParsing () ; Logic_env.reset_typenames ()


(*** escape character management ***)
let scan_escape = function
  | 'n' -> int64_of_char '\n'
  | 'r' -> int64_of_char '\r'
  | 't' -> int64_of_char '\t'
  | 'b' -> int64_of_char '\b'
  | 'f' -> int64_of_char '\012'  (* ASCII code 12 *)
  | 'v' -> int64_of_char '\011'  (* ASCII code 11 *)
  | 'a' -> int64_of_char '\007'  (* ASCII code 7 *)
  | 'e' | 'E' -> int64_of_char '\027'  (* ASCII code 27. This is a GCC extension *)
  | '\'' -> int64_of_char '\''
  | '"' -> int64_of_char '"'     (* '"' *)
  | '?' -> int64_of_char '?'
  | '(' -> int64_of_char '('
  | '{' -> int64_of_char '{'
  | '[' -> int64_of_char '['
  | '%' -> int64_of_char '%'
  | '\\' -> int64_of_char '\\'
  | other -> parse_error "Unrecognized escape sequence: \\%c" other

let scan_hex_escape str =
  let radix = Int64.of_int 16 in
  let the_value = ref Int64.zero in
  (* start at character 2 to skip the \x *)
  for i = 2 to (String.length str) - 1 do
    let thisDigit = Cabshelper.valueOfDigit (String.get str i) in
    (* the_value := !the_value * 16 + thisDigit *)
    the_value := Int64.add (Int64.mul !the_value radix) thisDigit
  done ;
  !the_value

let scan_oct_escape str =
  let radix = Int64.of_int 8 in
  let the_value = ref Int64.zero in
  (* start at character 1 to skip the \x *)
  for i = 1 to (String.length str) - 1 do
    let thisDigit = Cabshelper.valueOfDigit (String.get str i) in
    (* the_value := !the_value * 8 + thisDigit *)
    the_value := Int64.add (Int64.mul !the_value radix) thisDigit
  done ;
  !the_value

let lex_hex_escape remainder lexbuf =
  let prefix = scan_hex_escape (Lexing.lexeme lexbuf) in
  prefix :: remainder lexbuf

let lex_oct_escape remainder lexbuf =
  let prefix = scan_oct_escape (Lexing.lexeme lexbuf) in
  prefix :: remainder lexbuf

let lex_simple_escape remainder lexbuf =
  let lexchar = Lexing.lexeme_char lexbuf 1 in
  let prefix = scan_escape lexchar in
  prefix :: remainder lexbuf

let lex_unescaped remainder lexbuf =
  let prefix = int64_of_char (Lexing.lexeme_char lexbuf 0) in
  prefix :: remainder lexbuf

let lex_comment remainder buffer lexbuf =
  let s = Lexing.lexeme lexbuf in
  if s = "\n" then E.newline() ;
  Option.iter (fun b -> Buffer.add_string b s) buffer ;
  remainder buffer lexbuf

(* We ignore the last line if it does not contain "\n" as it means that
  it directly precedes the "else" and thus contains only whitespaces *)
let rec iter_comments ends_with_n f = function
  | [] -> ()
  | [ _ ] when not ends_with_n -> ()
  | x :: l -> f x ; iter_comments ends_with_n f l

let do_ghost_else_comments register comments =
  let last = String.length comments in
  (* note that comments contains at least a blank *)
  let ends_with_n = '\n' = String.get comments (last - 1) in
  let comments = String.split_on_char '\n' comments in
  let comments = List.map String.trim comments in
  let print_comments = Kernel.PrintComments.get () in
  let do_comment s = register && print_comments && not (String.equal "" s) in
  let process_line s = E.newline () ; if do_comment s then add_comment s in
  iter_comments ends_with_n process_line comments

let do_lex_comment ?(first_string="") remainder lexbuf =
  if Kernel.PrintComments.get () then
    let b = Buffer.create 80 in
    let () = Buffer.add_string b first_string in
    let () = remainder (Some b) lexbuf in
    add_comment (Buffer.contents b)
  else remainder None lexbuf

let do_oneline_ghost ~first_string comment lexbuf initial =
  do_lex_comment ~first_string comment lexbuf ;
  E.newline () ;
  if is_oneline_ghost ()
  then (exit_oneline_ghost () ; RGHOST)
  else initial lexbuf



(* Pragmas get explicit end-of-line tokens.
 * Elsewhere they are silently discarded as whitespace. *)
let pragmaLine = ref false

let annot_char = ref '@'

(* prevent the C lexer interpretation of comments *)
let prevent _ x = annot_char := if x then '@' else '\000'
let () = Kernel.ReadAnnot.add_set_hook prevent

let annot_start_pos = ref Cabshelper.cabslu
let buf = Buffer.create 1024

let save_current_pos () =
  annot_start_pos := currentLoc ()

let annot_lex initial rule lexbuf =
  save_current_pos () ;
  Buffer.clear buf ;
  try rule lexbuf
  with Parsing.Parse_error ->
    let start = Lexing.lexeme_start_p lexbuf in
    let source = E.convert_pos start in
    Kernel.warning ~wkey:Kernel.wkey_annot_error ~source "skipping annotation" ;
    initial lexbuf

let make_annot ~one_line default lexbuf s =
  let start = snd !annot_start_pos in
  match Logic_lexer.annot (start, s) with
  (* error occurred and annotation is discarded. Find a normal token. *)
  | None -> default lexbuf
  | Some (stop, token) ->
    lexbuf.Lexing.lex_curr_p <- Filepos.to_lexing_pos stop ;
    if one_line then E.newline () ;
    match token with
    | Logic_ptree.Adecl d -> DECL d
    | Logic_ptree.Aspec -> SPEC (start, s)
    (* At this point, we only have identified a function spec. Complete
       parsing of the annotation will only occur in the cparser.mly rule. *)
    | Logic_ptree.Acode_annot (loc,a) -> CODE_ANNOT (a, loc)
    | Logic_ptree.Aloop_annot (loc,a) -> LOOP_ANNOT (a,loc)

(* Initialize the pointer in Errormsg *)
let () =
  Lexerhack.add_type := add_type ;
  Lexerhack.push_context := push_context ;
  Lexerhack.pop_context := pop_context ;
  Lexerhack.add_identifier := add_identifier ;
  Lexerhack.is_typedef := is_typedef ;
  Lexerhack.set_typedef := set_typedef ;
  Lexerhack.reset_typedef := reset_typedef
}



let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let binarydigit = ['0' '1']
let letter = ['a'- 'z' 'A'-'Z']
let usuffix = ['u' 'U']
let lsuffix = "l" | "L" | "ll" | "LL"

let intsuffix =
  lsuffix | usuffix | usuffix lsuffix | lsuffix usuffix | usuffix ? "i64"

let hexprefix = '0' ['x' 'X']
let binaryprefix = '0' ['b' 'B']

let intnum = decdigit+ intsuffix?
let octnum = '0' octdigit+ intsuffix?
let hexnum = hexprefix hexdigit+ intsuffix?
let binarynum = binaryprefix binarydigit+ intsuffix?

let exponent = ['e' 'E']['+' '-']? decdigit+
let fraction  = '.' decdigit+
let decfloat =
    (intnum? fraction)
  | (intnum exponent)
  | (intnum? fraction exponent)
  | (intnum '.')
  | (intnum '.' exponent)

let hexfraction = hexdigit* '.' hexdigit+ | hexdigit+ '.'
let binexponent = ['p' 'P'] ['+' '-']? decdigit+
let hexfloat =
    hexprefix hexfraction binexponent
  | hexprefix hexdigit+   binexponent

let floatsuffix = ['f' 'F' 'l' 'L' 'd' 'D'] | "f32" | "F32" | "f64" | "F64"
let floatnum = (decfloat | hexfloat) floatsuffix?

let ident = (letter|'_')(letter|decdigit|'_'|'$')*
(* \026 is the dos EOF character *)
let blank = [' ' '\t' '\012' '\r' '\026']+
let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit+
let oct_escape = '\\' octdigit octdigit? octdigit?

(* Solaris-style pragmas *)
let solaris_pragmas =
    "ident" | "section" | "option" | "asm" | "use_section" | "weak"
  | "redefine_extname" | "TCS_align"

(* Embedded world *)
let embedded_pragmas = "global_register" | "location"

(* Pragmas that are not parsed by CIL. We lex them as PRAGMA_LINE tokens *)
let no_parse_pragma = "warning" | "GCC" | "omp" | solaris_pragmas | embedded_pragmas

let ghost_comments = "//\n" | ("//" [^'\n''@'] ([^'\n']*("\\\n")?)* '\n')



rule initial = parse

  (* Skip special doxygen comments or similar cases, e.g. /*@**...*/.
      Use of '@' instead of '!annot_char' is intentional *)
  | "/*" ("" | "@{" | "@}" | "@*" as suf)
    {
    do_lex_comment ~first_string:suf comment lexbuf ;
    initial lexbuf
    }

  | "/*" ([^ '*' '\n'] as c)
    {
    let first_string = String.make 1 c in
    if c = !annot_char
    then annot_lex initial annot_first_token lexbuf
    else (do_lex_comment ~first_string comment lexbuf ; initial lexbuf)
    }

  (* See comment for "/*@{" above *)
  | "//" ("" | "@{" | "@}" | "@*" as suf)
    {
    do_oneline_ghost ~first_string:suf onelinecomment lexbuf initial
    }

  | "//" ([^ '\n'] as c)
    {
    let first_string = String.make 1 c in
    if c = !annot_char
    then annot_lex initial annot_one_line lexbuf
    else do_oneline_ghost ~first_string onelinecomment lexbuf initial
    }

  | "\\ghost"
    {
    if is_ghost_code () || is_oneline_ghost ()
    then GHOST (currentLoc())
    else parse_error "Use of \\ghost out of ghost code"
    }

  | blank {initial lexbuf} | '\n'
    {
    E.newline () ;
    if !pragmaLine then (pragmaLine := false ; PRAGMA_EOL)
    else if is_oneline_ghost () then (exit_oneline_ghost () ; RGHOST)
    else initial lexbuf
    }

  | '\\' '\r' * '\n'
    {
    E.newline () ;
    initial lexbuf
    }

  | '#'  { hash lexbuf }
  | "%:" { hash lexbuf }
  | "_Pragma" { PRAGMA (currentLoc ()) }

  | '\''
    {
    let start = Lexing.lexeme_start_p lexbuf in
    let content = chr lexbuf in
    let last = Lexing.lexeme_end_p lexbuf in
    CST_CHAR (content, E.convert_loc (start,last))
    }

  | "L'"
    {
    let start = Lexing.lexeme_start_p lexbuf in
    let content = chr lexbuf in
    let last = Lexing.lexeme_end_p lexbuf in
    CST_WCHAR (content, E.convert_loc (start,last))
    }

  | '"'
    {
    let start = Lexing.lexeme_start_p lexbuf in
    let content = str lexbuf in
    let last = Lexing.lexeme_end_p lexbuf in
    CST_STRING (content, E.convert_loc (start,last))
    }

  | "L\""
    {
    let start = Lexing.lexeme_start_p lexbuf in
    let content = str lexbuf in
    let last = Lexing.lexeme_end_p lexbuf in
    CST_WSTRING(content, E.convert_loc (start,last))
    }

  | floatnum
    {
    let loc = currentLoc () in
    let content = Lexing.lexeme lexbuf in
    let len = String.length content in
    let last = content.[len - 1] in
    if (last = 'd' || last = 'D') && not (Machine.gccMode ()) then begin
       Kernel.warning ~wkey:Kernel.wkey_conditional_feature
          ~source:(fst loc) ~once:true
          "'d' suffix in a floating literal (%s) is a GCC extension, \
           use a GCC-based machdep to enable it" content;
       (* drop the suffix *)
       CST_FLOAT (String.sub content 0 (len - 1), loc)
    end else
       CST_FLOAT (content, loc)
    }

  (* GCC Extension for binary numbers *)
  | binarynum { CST_INT (Lexing.lexeme lexbuf, currentLoc ()) }

  | hexnum    { CST_INT (Lexing.lexeme lexbuf, currentLoc ()) }
  | octnum    { CST_INT (Lexing.lexeme lexbuf, currentLoc ()) }
  | intnum    { CST_INT (Lexing.lexeme lexbuf, currentLoc ()) }
  | "!quit!"  { EOF }
  | "..."     { ELLIPSIS }
  | "+="      { PLUS_EQ }
  | "-="      { MINUS_EQ }
  | "*="      { STAR_EQ }
  | "/="      { SLASH_EQ }
  | "%="      { PERCENT_EQ }
  | "|="      { PIPE_EQ }
  | "&="      { AND_EQ }
  | "^="      { CIRC_EQ }
  | "<<="     { INF_INF_EQ }
  | ">>="     { SUP_SUP_EQ }
  | "<<"      { INF_INF }
  | ">>"      { SUP_SUP }
  | "=="      { EQ_EQ }
  | "!="      { EXCLAM_EQ }
  | "<="      { INF_EQ }
  | ">="      { SUP_EQ }
  | "="       { EQ }
  | "<"       { INF }
  | ">"       { SUP }
  | "++"      { PLUS_PLUS (currentLoc ()) }
  | "--"      { MINUS_MINUS (currentLoc ()) }
  | "->"      { ARROW }
  | '+'       { PLUS (currentLoc ()) }
  | '-'       { MINUS (currentLoc ()) }

  | '*'
    {
    if is_ghost_code ()
    then might_end_ghost lexbuf
    else STAR (currentLoc ())
    }

  | "/" ([^ '\n'] as c)
    {
    if c = !annot_char then
      if is_ghost_code () || is_oneline_ghost ()
      then (enter_ghost_annot () ; annot_lex initial annot_first_token lexbuf)
      else parse_error "This kind of annotation is valid only inside ghost code"
    else (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1 ; SLASH)
    }

  | '/'   { SLASH }
  | '%'   { PERCENT }
  | '!'   { EXCLAM (currentLoc ()) }
  | "&&"  { AND_AND (currentLoc ()) }
  | "||"  { PIPE_PIPE }
  | '&'   { AND (currentLoc ()) }
  | '|'   { PIPE }
  | '^'   { CIRC }
  | '?'   { QUEST }

  | ':'
    {
    if Cabshelper.is_attr_test ()
    then (Cabshelper.pop_attr_test () ; COLON2)
    else COLON
    }

  | '~'       { TILDE (currentLoc ()) }
  | '{'       { LBRACE (currentLoc ()) }
  | '}'       { RBRACE (currentLoc ()) }
  | "<%"      { LBRACE (currentLoc ()) }
  | "%>"      { RBRACE (currentLoc ()) }
  | '['       { LBRACKET }
  | ']'       { RBRACKET }
  | "<:"      { LBRACKET }
  | ":>"      { RBRACKET }
  | '('       { LPAREN (currentLoc ()) }
  | ')'       { RPAREN }
  | ';'       { SEMICOLON (currentLoc ()) }
  | ','       { COMMA }
  | '.'       { DOT }
  | "sizeof"  { SIZEOF (currentLoc ()) }
  | "__asm"   { ASM (currentLoc ()) }

  (* If we see __pragma we consume it and the matching parentheses as well *)
  | "__pragma" { let _ = matchingpars 0 lexbuf in initial lexbuf }

  (* __extension__ is a black. The parser runs into some conflicts if we let it
     pass *)
  | "__extension__" { initial lexbuf }

  | ident { scan_ident (Lexing.lexeme lexbuf) }

  | eof
    {
    if is_oneline_ghost()
    then (exit_oneline_ghost () ; RGHOST)
    else EOF
    }

  | _ as c
    {
    if is_ghost_code() && c = '@' then initial lexbuf
    else parse_error "Invalid symbol"
    }


and might_end_ghost = parse
  | '/' { exit_ghost_code() ; RGHOST }
  | ""  { STAR (currentLoc()) }


and comment buffer = parse
  | "*/" {  }
  | eof  { parse_error "Unterminated C comment" }
  | _    { lex_comment comment buffer lexbuf }


and onelinecomment buffer = parse
  | "*/"
    {
    (* end of multiline comment *)
    if is_ghost_code ()
    then lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 2
    else lex_comment onelinecomment buffer lexbuf
    }
  | '\n' | eof {  }
  | _          { lex_comment onelinecomment buffer lexbuf }


and matchingpars parsopen = parse
  | '\n'  { E.newline (); matchingpars parsopen lexbuf }
  | blank { matchingpars parsopen lexbuf }
  | '('   { matchingpars (parsopen + 1) lexbuf }
  | ')'   { if parsopen > 1 then matchingpars (parsopen - 1) lexbuf }
  | "/*"  { do_lex_comment comment lexbuf ; matchingpars parsopen lexbuf }
  | '"'   { let _ = str lexbuf in matchingpars parsopen lexbuf }
  | _     { matchingpars parsopen lexbuf }


(* # <line number> <file name> ... *)
and hash = parse
  | '\n' { E.newline (); initial lexbuf}
  | blank { hash lexbuf}

  (* #line directive, the "line" before the number can be omitted. *)
  | ("line" blank +)? (intnum as s)
    { 
    let msg () = Kernel.warning "Bad line number in preprocessed file: %s" s in
    let lineno = try int_of_string s with Failure _ -> msg () ; 0 in
    (* A file name may follow *)
    let filename = file lexbuf in
    E.setCurrentLine ?filename lineno;
    initial lexbuf
    }

  (* For pragmas with irregular syntax, like #pragma warning, we parse them as a
     whole line. *)
  | "pragma" blank (no_parse_pragma as pragmaName)
    {
    let here = currentLoc () in
    PRAGMA_LINE (pragmaName ^ pragma lexbuf, here)
    }

  | "pragma"  { pragmaLine := true ; PRAGMA (currentLoc ()) }
  | _         { endline lexbuf }

and ignore_line_end = parse 
  | '\n' { E.newline () }
  | eof { }
  | _ { ignore_line_end lexbuf }

and file = parse
  | '\n'  { E.newline (); None }
  | blank { file lexbuf }

  (* The //-ending file directive is a GCC extension that provides the CWD of
     the preprocessor when the file was preprocessed. *)
  | '"' ([^ '\012' '\t' '"']* as d) "//\""
    {
    E.setCurrentWorkingDirectory (Filepath.of_string d);
    ignore_line_end lexbuf;
    None
    }

  | '"' (([^ '\012' '\t' '"']|"\\\"")* as f) '"'
    {
    let unescape = Str.regexp_string "\\\"" in
    let filename = Str.global_replace unescape "\"" f in
    ignore_line_end lexbuf;
    Some filename
    }

  | _ { ignore_line_end lexbuf; None }


and endline = parse
  | '\n'  { E.newline () ; initial lexbuf }
  | eof   { EOF }
  | _     { endline lexbuf }


and pragma = parse
  | '\n'  { E.newline () ; "" }
  | _     { let cur = Lexing.lexeme lexbuf in cur ^ (pragma lexbuf) }


and str = parse
  (* no nul termination in CST_STRING '"' *)
  | '"'         { [] }
  | hex_escape  { lex_hex_escape str lexbuf }
  | oct_escape  { lex_oct_escape str lexbuf }
  | escape      { lex_simple_escape str lexbuf }
  | eof         { parse_error "unterminated string" }
  | _           { lex_unescaped str lexbuf }


and chr =  parse
  | '\''        { [] }
  | hex_escape  { lex_hex_escape chr lexbuf }
  | oct_escape  { lex_oct_escape chr lexbuf }
  | escape      { lex_simple_escape chr lexbuf }
  | eof         { parse_error "unterminated char" }
  | _           { lex_unescaped chr lexbuf }


and annot_first_token = parse
  | "ghost" ((blank| '\\'?'\n' | ghost_comments)* as comments) "else"
    {
    if is_oneline_ghost () then parse_error "nested ghost code" ;
    Buffer.clear buf ;
    let loc = currentLoc () in
    do_ghost_else_comments true comments ;
    enter_ghost_code () ;
    LGHOST_ELSE (loc)
    }

  | "ghost"
    {
    if is_oneline_ghost () then parse_error "nested ghost code" ;
    Buffer.clear buf ;
    enter_ghost_code () ;
    LGHOST
    }

  | ' '|'@'|'\t'|'\r' as c { Buffer.add_char buf c ; annot_first_token lexbuf }
  | '\n' { E.newline() ; Buffer.add_char buf '\n' ; annot_first_token lexbuf }
  | ""   { annot_token lexbuf }


and annot_token = parse
  | "*/"
    {
    if is_ghost_annot ()
    then begin
      let loc = (fst (get_ghost_annot_start()), snd (currentLoc())) in
      parse_error ~loc "Ghost multi-line annotation not terminated"
    end;
    let s = Buffer.contents buf in
    make_annot ~one_line:false initial lexbuf s
    }

  | eof  { parse_error "Unterminated annotation" }
  | '\n' { E.newline() ; Buffer.add_char buf '\n' ; annot_token lexbuf }
  | _ as c
    {
    if is_ghost_annot () && c = !annot_char
    then might_end_ghost_annot lexbuf
    else (Buffer.add_char buf c ; annot_token lexbuf)
    }


and might_end_ghost_annot = parse
  | '/' { exit_ghost_annot ();
          let s = Buffer.contents buf in
          make_annot ~one_line:false initial lexbuf s }
  | "" { Buffer.add_char buf !annot_char; annot_token lexbuf }


and annot_one_line = parse
  | "ghost" ((blank|"\\\n")+ as comments) "else"
    {
    do_ghost_else_comments false comments ;
    if is_oneline_ghost () then parse_error "nested ghost code" ;
    enter_oneline_ghost () ;
    LGHOST_ELSE (currentLoc ())
    }

  | "ghost"
    {
    if is_oneline_ghost () then parse_error "nested ghost code" ;
    enter_oneline_ghost () ;
    LGHOST
    }

  | ' '|'@'|'\t'|'\r' as c { Buffer.add_char buf c ; annot_one_line lexbuf }
  | "" { annot_one_line_logic lexbuf }


and annot_one_line_logic = parse
  | '\n'    { make_annot ~one_line:true initial lexbuf (Buffer.contents buf) }
  | eof     { parse_error "Invalid C file: should end with a newline" }
  | _ as c  { Buffer.add_char buf c ; annot_one_line_logic lexbuf }



{
  (* Catch the exceptions raised by the lexer itself *)
  let initial lexbuf =
    try initial lexbuf
    with Failure _ -> raise Parsing.Parse_error
}
