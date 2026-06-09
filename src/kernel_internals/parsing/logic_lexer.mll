(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

{

  open Logic_parser
  open Lexing

  type state = Normal | Test

  let state_stack = Stack.create ()

  let () = Stack.push Normal state_stack

  let get_state () = try Stack.top state_stack with Stack.Empty -> Normal

  let pop_state () = try ignore (Stack.pop state_stack) with Stack.Empty -> ()

  exception Error of (int * int) * string

  let ext_acsl_spec = ref false

  let loc lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let lex_error lexbuf s =
    raise (Error (loc lexbuf, "lexical error, " ^ s))

  let find_utf8 =
    let h = Hashtbl.create 97 in
    List.iter (fun (i,t) -> Hashtbl.add h i t)
      [ Utf8_logic.forall, FORALL;
        Utf8_logic.exists, EXISTS;
        Utf8_logic.eq, EQ;
        Utf8_logic.neq, NE;
        Utf8_logic.le, LE;
        Utf8_logic.ge, GE;
        Utf8_logic.implies,IMPLIES;
        Utf8_logic.iff, IFF;
        Utf8_logic.conj, AND;
        Utf8_logic.disj, OR;
        Utf8_logic.neg, NOT;
        Utf8_logic.x_or, HATHAT;
        Utf8_logic.minus, MINUS;
        Utf8_logic.boolean, BOOLEAN;
        Utf8_logic.integer, INTEGER;
        Utf8_logic.real, REAL;
        Utf8_logic.inset, IN;
        Utf8_logic.pi, PI;
      ];

    fun s -> try Hashtbl.find h s
    with Not_found -> IDENTIFIER s

  let all_digits s =
    let is_digit =
      function
      | '0'..'9' | 'a'..'f' | 'A'..'F' -> ()
      | _ -> raise Exit
    in
    try String.iter is_digit s; true with Exit -> false

  let is_ucn s =
    if String.length s <= 2 || s.[0] <> '\\' then false else begin
      match s.[1] with
      | 'U' -> String.length s = 10 && all_digits (String.sub s 2 8)
      | 'u' -> String.length s = 6 && all_digits (String.sub s 2 4)
      | _ -> false
    end

  let int_of_digit chr =
    match chr with
    | '0'..'9' -> (Char.code chr) - (Char.code '0')
    | 'a'..'f' -> (Char.code chr) - (Char.code 'a') + 10
    | 'A'..'F' -> (Char.code chr) - (Char.code 'A') + 10
    | _ -> assert false

  (* assumes is_ucn s *)
  let unicode_char s =
    let code = ref 0 in
    let add_digit c = code := 16 * !code + int_of_digit c in
    String.iter add_digit (String.sub s 2 (String.length s - 2));
    let uchar = Uchar.of_int !code in
    let buffer = Buffer.create (Uchar.utf_8_byte_length uchar) in
    Buffer.add_utf_8_uchar buffer uchar;
    find_utf8 (Buffer.contents buffer)

  let extension ~plugin name =
    try
      (* extension_from will raise Not_found or fatal, if extension does not
         exist or if there are ambiguities between extension names and the
         plugin is not provided. *)
      let plugin = Logic_env.extension_from ?plugin name in
      match Logic_env.extension_category ~plugin name with
      | Cil_types.Ext_contract -> Some (EXT_CONTRACT (name, plugin))
      | Cil_types.Ext_global ->
          if Logic_env.is_extension_block ~plugin name
          then Some (EXT_GLOBAL_BLOCK (name, plugin))
          else Some (EXT_GLOBAL (name, plugin))
      | Cil_types.Ext_code_annot _ -> Some (EXT_CODE_ANNOT (name, plugin))
    with Not_found ->
      (* We need to distinguish here which token was parsed (with or without
         plugin) to help the parser (cf. ext_loader rule). *)
      match plugin with
      | None ->
        begin
          try
            (* importer_from will raise Not_found or fatal, if extension does not
              exist or if there are ambiguities between extension names and the
              plugin is not provided. *)
            let plugin = Logic_env.importer_from name in
            Some (EXT_LOADER (name, plugin))
          with Not_found -> None
        end
      | Some plugin ->
        if Logic_env.is_importer ~plugin name then
          Some (EXT_LOADER_PLUGIN (name, plugin))
        else None
  [@@alert "-acsl_extension_from"]

  let identifier, is_acsl_keyword =
    let all_kw = Hashtbl.create 37 in
    let type_kw = Hashtbl.create 3 in
    let ext_acsl_kw kw s _ = if !ext_acsl_spec then kw else IDENTIFIER s in
    List.iter
      (fun (i,t) -> Hashtbl.add all_kw i t;)
      [
        "admit", (fun _ -> ADMIT);
        "alignof", (fun _ -> ALIGNOF);
        "_Alignof", (fun _ -> ALIGNOF);
        "allocates", (fun _ -> ALLOCATES);
        "assert", (fun _ -> ASSERT);
        "assigns", (fun _ -> ASSIGNS);
        "assumes", (fun _ -> ASSUMES);
        "at", ext_acsl_kw EXT_SPEC_AT "at";
        "axiom", (fun _ -> AXIOM);
        "axiomatic", (fun _ -> AXIOMATIC);
        "behavior", (fun _ -> BEHAVIOR);
        "behaviors", (fun _ -> BEHAVIORS);
        "_Bool", (fun _ -> BOOL);
        "breaks", (fun _ -> BREAKS);
        "case", (fun _ -> CASE);
        "char", (fun _ -> CHAR);
        "check", (fun _ -> CHECK);
        "complete", (fun _ -> COMPLETE);
        "const", (fun _ -> CONST);
        "continues", (fun _ -> CONTINUES);
        "contract", ext_acsl_kw EXT_SPEC_CONTRACT "contract";
        "decreases", (fun _ -> DECREASES);
        "disjoint", (fun _ -> DISJOINT);
        "double", (fun _ -> DOUBLE);
        "else", (fun _ -> ELSE);
        "ensures", (fun _ -> ENSURES);
        "enum", (fun _ -> ENUM);
        "exits", (fun _ -> EXITS);
        "frees", (fun _ -> FREES);
        "function", ext_acsl_kw EXT_SPEC_FUNCTION "function";
        "float", (fun _ -> FLOAT);
        "_Float32", (fun _ -> FLOAT32);
        "_Float64", (fun _ -> FLOAT64);
        "for", (fun _ -> FOR);
        "global", (fun _ -> GLOBAL);
        "if", (fun _ -> IF);
        "import", (fun _ -> IMPORT);
        "inductive", (fun _ -> INDUCTIVE);
        "include", ext_acsl_kw EXT_SPEC_INCLUDE "include";
        "int", (fun _ -> INT);
        "invariant", (fun _ -> INVARIANT);
        "label", (fun _ -> LABEL);
        "lemma", (fun _ -> LEMMA);
        "let", ext_acsl_kw EXT_SPEC_LET "let";
        (* ACSL extension for external spec file *)
        "logic", (fun _ -> LOGIC);
        "long", (fun _ -> LONG);
        "loop", (fun _ -> LOOP);
        "model", (fun _ -> MODEL);
        (* ACSL extension for model fields *)
        "module", (fun _ -> if !ext_acsl_spec then EXT_SPEC_MODULE else MODULE);
        (* ACSL extension for external spec file *)
        "predicate", (fun _ -> PREDICATE);
        "reads", (fun _ -> READS);
        "requires", (fun _ -> REQUIRES);
        "returns", (fun _ -> RETURNS);
        "short", (fun _ -> SHORT);
        "signed", (fun _ -> SIGNED);
        "sizeof", (fun _ -> SIZEOF);
        "struct", (fun _ -> STRUCT);
        "terminates", (fun _ -> TERMINATES);
        "type", (fun _ -> TYPE);
        "union", (fun _ -> UNION);
        "unsigned", (fun _ -> UNSIGNED);
        "variant", (fun _ -> VARIANT);
        "void", (fun _ -> VOID);
        "volatile", (fun _ -> VOLATILE);
        "writes", (fun _ -> WRITES);
        "__FC_FILENAME__",
        (fun loc ->
           let filename = Fileloc.path loc |> Filepath.to_string in
           STRING_LITERAL (false,filename));
      ];
    List.iter (fun (x, y) -> Hashtbl.add type_kw x y)
      ["integer", INTEGER; "real", REAL; "boolean", BOOLEAN; ];
    (fun ~plugin s loc ->
      try
        (Hashtbl.find all_kw s)
        loc
      with Not_found ->
        match extension ~plugin s with
        | None ->
          if Logic_env.typename_status s then TYPENAME s
          else
            (try Hashtbl.find type_kw s
             with Not_found -> IDENTIFIER s)
        | Some lex -> lex
    ),
    (fun s -> Hashtbl.mem all_kw s || Hashtbl.mem type_kw s)

  let bs_identifier =
    let h = Hashtbl.create 97 in
    List.iter (fun (i,t) -> Hashtbl.add h i t)
      [
        "\\allocation", ALLOCATION;
        "\\allocable", ALLOCABLE;
        "\\automatic", AUTOMATIC;
        "\\as", AS;
        "\\at", AT;
        "\\base_addr", BASE_ADDR;
        "\\block_length", BLOCK_LENGTH;
        "\\dynamic", DYNAMIC;
        "\\empty", EMPTY;
        "\\exists", EXISTS;
        "\\false", FALSE;
        "\\forall", FORALL;
        "\\freeable", FREEABLE;
        "\\fresh", FRESH;
        "\\from", FROM;
        "\\ghost", GHOST;
        "\\initialized", INITIALIZED;
        "\\dangling", DANGLING;
        "\\aligned", ALIGNED;
        "\\in", IN;
        "\\inter", INTER;
        "\\lambda", LAMBDA;
        "\\let", LET;
        "\\nothing", NOTHING;
        "\\null", NULL;
        "\\offset", OFFSET;
        "\\old", OLD;
        "\\pi", PI;
        "\\register", REGISTER;
        "\\result", RESULT;
        "\\separated", SEPARATED;
        "\\static", STATIC;
        "\\true", TRUE;
        "\\type", BSTYPE;
        "\\typeof", TYPEOF;
        "\\unallocated", UNALLOCATED;
        "\\union", BSUNION;
        "\\object_pointer", OBJECT_POINTER;
        "\\valid", VALID;
        "\\valid_read", VALID_READ;
        "\\valid_index", VALID_INDEX;
        "\\valid_range", VALID_RANGE;
        "\\valid_function", VALID_FUNCTION;
        "\\with", WITH;
      ];
    fun lexbuf ->
      let s = lexeme lexbuf in
      if is_ucn s then unicode_char s else begin
        try Hashtbl.find h s with Not_found ->
          if Logic_env.typename_status s then TYPENAME s
          else IDENTIFIER s
      end

  let longident lexbuf =
    let s = lexeme lexbuf in LONGIDENT s

  (* Update lexer buffer. *)
  let update_line_pos lexbuf line =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
	Lexing.pos_lnum = line;
	Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

  let update_file_pos lexbuf file =
   let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_fname = file }

  let hack_merge_tokens current next =
    match (current,next) with
    | CHECK, REQUIRES -> true, CHECK_REQUIRES
    | CHECK, ENSURES -> true, CHECK_ENSURES
    | CHECK, EXITS -> true, CHECK_EXITS
    | CHECK, RETURNS -> true, CHECK_RETURNS
    | CHECK, BREAKS -> true, CHECK_BREAKS
    | CHECK, CONTINUES -> true, CHECK_CONTINUES
    | CHECK, LOOP -> true, CHECK_LOOP
    | CHECK, INVARIANT -> true, CHECK_INVARIANT
    | CHECK, LEMMA -> true, CHECK_LEMMA
    | ADMIT, REQUIRES -> true, ADMIT_REQUIRES
    | ADMIT, ENSURES -> true, ADMIT_ENSURES
    | ADMIT, EXITS -> true, ADMIT_EXITS
    | ADMIT, RETURNS -> true, ADMIT_RETURNS
    | ADMIT, BREAKS -> true, ADMIT_BREAKS
    | ADMIT, CONTINUES -> true, ADMIT_CONTINUES
    | ADMIT, LOOP -> true, ADMIT_LOOP
    | ADMIT, INVARIANT -> true, ADMIT_INVARIANT
    | ADMIT, LEMMA -> true, ADMIT_LEMMA
    | _ -> false, current

  let check_ext_plugin source plugin tok =
    match tok with
    | IDENTIFIER s ->
      if Plugin.is_present plugin then
        Kernel.warning ~once:true ~wkey:Kernel.wkey_extension_unknown ~source
          "Ignoring unregistered extension '%s' of plug-in %s" s plugin
      else
        Kernel.warning ~once:true ~wkey:Kernel.wkey_plugin_not_loaded ~source
          "Ignoring extension '%s' for unloaded plug-in %s" s plugin;
      IDENTIFIER_EXT s
    | EXT_CODE_ANNOT (s, _)
    | EXT_GLOBAL (s, _)
    | EXT_GLOBAL_BLOCK (s, _)
    | EXT_CONTRACT (s, _) ->
      if String.equal plugin "kernel" then
        Kernel.abort ~source
          "Extension '%s' from frama-c's kernel should not be used with the \
           syntax \\kernel::%s" s s;
      tok
    | _ -> raise Parsing.Parse_error

  let check_ext_importer source plugin tok =
    match tok with
    | IDENTIFIER s ->
      if Plugin.is_present plugin then
        Kernel.warning ~once:true ~wkey:Kernel.wkey_extension_unknown ~source
          "Ignoring unregistered module importer extension '%s' of plug-in %s"
          s plugin
      else
        Kernel.warning ~once:true ~wkey:Kernel.wkey_plugin_not_loaded ~source
          "Ignoring module importer extension '%s' for unloaded plug-in %s" s plugin;
      IDENTIFIER_LOADER s
    | EXT_LOADER_PLUGIN (s, _) ->
      if String.equal plugin "kernel" then
        Kernel.abort ~source
          "Module importer extension '%s' from frama-c's kernel should not be \
           used with the syntax \\kernel::%s" s s;
      tok
    | _ -> raise Parsing.Parse_error
}

let space = [' ' '\t' '\012' '\r' '@' ]

let rB = ['0' '1']
let rD = ['0'-'9']
let rO = ['0'-'7']
let rL = ['a'-'z' 'A'-'Z' '_']
let rH = ['a'-'f' 'A'-'F' '0'-'9']
let rE = ['E''e']['+''-']? rD+
let rP = ['P''p']['+''-']? rD+
let rFS	= ('f'|'F'|'l'|'L'|'d'|'D'|"f32"|"F32"|"f64"|"F64")
let rIS = ('u'|'U'|'l'|'L')*
let rOP = [
  '=' '<' '>' '~'
  '+' '-' '*' '/' '\\' '%'
  '!' '$' '&' '?' '@' '^' '.' ':' '|' '#'
  '_' '\''
  'a'-'z' 'A'-'Z' '0'-'9'
  '[' ']' '{' '}'
]
let comment_line = "//" [^'\n']*
let rIdentifier = rL (rL | rD)*
let xIdentifier = rL (rL | rD | "'")*
let opIdentifier = (rL | rD | rOP)+

(* Do not forget to update also the corresponding chr rule if you add
   a supported escape sequence here. *)
let escape = '\\'
   ('\'' | '"' | '?' | '\\' | 'a' | 'b' | 'f' | 'n' | 'r'
   | 't' | 'v')

let hex_escape = '\\' ['x' 'X'] rH+
let oct_escape = '\\' rO rO? rO?

let utf8_char = ['\128'-'\254']+

rule token = parse
  | space+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | comment_line '\n' { Lexing.new_line lexbuf; token lexbuf }
  | comment_line eof { token lexbuf }
  | "*/" { lex_error lexbuf "unexpected block-comment closing" }
  | "/*" { if !ext_acsl_spec
           then comment lexbuf
           else lex_error lexbuf "unexpected block-comment opening"
         }
  | '\\' (rIdentifier as plugin) "::" (rIdentifier as name) ":" {
     let loc = Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
     let cabsloc = Errorloc.convert_loc loc in
     let tok = identifier ~plugin:(Some plugin) name cabsloc in
     check_ext_importer (fst cabsloc) plugin tok
     }
  | '\\' (rIdentifier as plugin) "::" (rIdentifier as name) {
     let loc = Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
     let cabsloc = Errorloc.convert_loc loc in
     let tok = identifier ~plugin:(Some plugin) name cabsloc in
     check_ext_plugin (fst cabsloc) plugin tok
     }
  | '\\' rIdentifier { bs_identifier lexbuf }
  | ( rIdentifier "::")+     xIdentifier      { longident lexbuf }
  | ( rIdentifier "::")+ "(" opIdentifier ")" { longident lexbuf }
  | rIdentifier       {
      let loc = Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
      let cabsloc = Errorloc.convert_loc loc in
      let s = lexeme lexbuf in
      let curr_tok = identifier ~plugin:None s cabsloc in
      if curr_tok = CHECK || curr_tok = ADMIT then begin
        let next_tok =
          token { lexbuf with refill_buff = lexbuf.refill_buff }
        in
        let (eat_next, tok) = hack_merge_tokens curr_tok next_tok in
        if eat_next then ignore (token lexbuf);
        tok
      end else curr_tok
    }

  | '0'['x''X'] rH+ rIS?    { INT_CONSTANT (lexeme lexbuf) }
  | '0'['b''B'] rB+ rIS?    { INT_CONSTANT (lexeme lexbuf) }
  | '0' rD+ rIS?            { INT_CONSTANT (lexeme lexbuf) }
  | rD+                     { INT_CONSTANT (lexeme lexbuf) }
  | rD+ rIS                 { INT_CONSTANT (lexeme lexbuf) }
  | ('L'? "'" as prelude) (([^ '\\' '\'' '\n']|("\\"[^ '\n']))+ as content) "'"
      {
        let b = Buffer.create 5 in
        Buffer.add_string b prelude;
        let lbf = Lexing.from_string content in
        INT_CONSTANT (chr b lbf ^ "'")
      }
(* floating-point literals, both decimal and hexadecimal *)
  | rD+ rE rFS?
  | rD* "." rD+ (rE)? rFS?
  | rD+ "." rD* (rE)? rFS?
  | '0'['x''X'] rH+ '.' rH* rP rFS?
  | '0'['x''X'] rH* '.' rH+ rP rFS?
  | '0'['x''X'] rH+ rP rFS?
      { FLOAT_CONSTANT (lexeme lexbuf) }

 (* hack to lex 0..3 as 0 .. 3 and not as 0. .3 *)
  | (rD+ as n) ".."         { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 2;
                              INT_CONSTANT n }

  | 'L'? '"' as prelude (([^ '\\' '"' '\n']|("\\"[^ '\n']))* as content) '"'
      { STRING_LITERAL (prelude.[0] = 'L',content) }
  | '#'                     { hash lexbuf }
  | "==>"                   { IMPLIES }
  | "<==>"                  { IFF }
  | "-->"                   { BIMPLIES }
  | "<-->"                  { BIFF }
  | "&&"                    { AND }
  | "||"                    { OR }
  | "!"                     { NOT }
  | "$"                     { DOLLAR }
  | ","                     { COMMA }
  | "->"                    { ARROW }
  | "?"                     { Stack.push Test state_stack; QUESTION }
  | ";"                     { SEMICOLON }
  | ":"                     { match get_state() with
                                  Normal  -> COLON
                                | Test -> pop_state(); COLON2
                            }
  | "::"                    { COLONCOLON }
  | "."                     { DOT }
  | ".."                    { DOTDOT }
  | "..."                   { DOTDOTDOT }
  | "-"                     { MINUS }
  | "+"                     { PLUS }
  | "*"                     { STAR }
  | "*^"                    { STARHAT }
  | "&"                     { AMP }
  | "^^"                    { HATHAT }
  | "^"                     { HAT }
  | "|"                     { PIPE }
  | "~"                     { TILDE }
  | "/"                     { SLASH }
  | "%"                     { PERCENT }
  | "<"                     { LT }
  | ">"                     { GT }
  | "<="                    { LE }
  | ">="                    { GE }
  | "=="                    { EQ }
  | "="                     { EQUAL }
  | "!="                    { NE }
  | "("                     { Stack.push Normal state_stack; LPAR }
  | ")"                     { pop_state(); RPAR }
  | "{"                     { Stack.push Normal state_stack; LBRACE }
  | "}"                     { pop_state(); RBRACE }
  | "["                     { Stack.push Normal state_stack; LSQUARE }
  | "]"                     { pop_state(); RSQUARE }
  | "[|"                    { Stack.push Normal state_stack; LSQUAREPIPE }
  | "|]"                    { pop_state(); RSQUAREPIPE }
  | "<<"                    { LTLT }
  | ">>"                    { GTGT }
  | utf8_char as c          { find_utf8 c }
  | eof                     { EOF }
  | _   { lex_error lexbuf ("illegal character " ^ lexeme lexbuf) }

and chr buffer = parse
  | hex_escape
      { let s = lexeme lexbuf in
        let real_s = String.sub s 2 (String.length s - 2) in
        let rec add_one_char s =
          let l = String.length s in
          if l = 0 then ()
          else
          let h = int_of_digit s.[0] in
          let c,s =
            if l = 1 then (h,"")
            else
              (16*h + int_of_digit s.[1],
               String.sub s 2 (String.length s - 2))
          in
          Buffer.add_char buffer (Char.chr c); add_one_char s
        in add_one_char real_s; chr buffer lexbuf
      }
  | oct_escape
      { let s = lexeme lexbuf in
        let real_s = String.sub s 1 (String.length s - 1) in
        let rec value i s =
          if s = "" then i
          else value (8*i+int_of_digit s.[0])
            (String.sub s 1 (String.length s -1))
        in let c = value 0 real_s in
        Buffer.add_char buffer (Char.chr c); chr buffer lexbuf
      }
  | escape
      { Buffer.add_char buffer
          (match (lexeme lexbuf).[1] with
               'a' -> '\007'
             | 'b' -> '\b'
             | 'f' -> '\012'
             | 'n' -> '\n'
             | 'r' -> '\r'
             | 't' -> '\t'
             | 'v' -> '\011' (* no '\v' in OCaml 😞 *)
             | '\'' -> '\''
             | '"' -> '"'
             | '?' -> '?'
             | '\\' -> '\\'
             | _ -> (* escape regex does not allow anything else *) assert false
          ); chr buffer lexbuf}
  | eof { Buffer.contents buffer }
  | _  { Buffer.add_string buffer (lexeme lexbuf); chr buffer lexbuf }

and hash = parse
  '\n'		{ Lexing.new_line lexbuf; token lexbuf}
| [' ''\t']		{ hash lexbuf}
| rD+	        { (* We are seeing a line number. This is the number for the
                   * next line *)
                 let s = Lexing.lexeme lexbuf in
                 let lineno =
                   try
                     int_of_string s
                   with Failure _ ->
                     (* the int is too big. *)
                     Kernel.warning
                       ~source:(Errorloc.convert_pos lexbuf.lex_start_p)
                       "Bad line number in preprocessed file: %s"  s;
                     (-1)
                 in
                 update_line_pos lexbuf (lineno - 1);
                  (* A file name may follow *)
		  file lexbuf }
| "line"        { hash lexbuf } (* MSVC line number info *)
| _	        { endline lexbuf}

and file =  parse
        '\n'		        { Lexing.new_line lexbuf; token lexbuf}
|	[' ''\t''\r']			{ file lexbuf}
|	'"' ([^ '\012' '\t' '"']|"\\\"")* '"' {
    let n = Lexing.lexeme lexbuf in
    let n1 = String.sub n 1
        ((String.length n) - 2) in
    let unescape = Str.regexp_string "\\\"" in
    let n1 = Str.global_replace unescape "\"" n1 in
    update_file_pos lexbuf n1;
    endline lexbuf
  }

|	_			{ endline lexbuf}

and endline = parse
  '\n' { Lexing.new_line lexbuf; token lexbuf}
| eof  { EOF }
| _    { endline lexbuf}

and comment = parse
    '\n' { Lexing.new_line lexbuf; comment lexbuf}
  | "*/" { token lexbuf}
  | eof  { lex_error lexbuf "non-terminating block-comment" }
  | _    { comment lexbuf}

{
  (* When Ocaml 4.11+ becomes mandatory, we can probably replace this with
     Lexing.set_position. *)
  let set_initial_position dest_lexbuf src_pos =
    dest_lexbuf.Lexing.lex_curr_p <- src_pos;
    dest_lexbuf.lex_abs_pos <- src_pos.pos_cnum

  let parse_from_position f (pos, s : Filepos.t * string) =
    let open Current_loc.Operators in
    let lb = from_string s in
    let get_loc () = Errorloc.convert_pos lb.Lexing.lex_curr_p in
    let<> UpdatedCurrentLoc = (pos, pos) in
    let warn = Kernel.warning ~wkey:Kernel.wkey_annot_error in
    set_initial_position lb (Filepos.to_lexing_pos pos);
    try
      let res = f token lb in
      Some (get_loc (), res)
    with
      | Failure s -> (* raised by the lexer itself, through [f] *)
          warn ~source:(get_loc ()) "lexing error: %s" s; None
      | Parsing.Parse_error ->
        warn ~source:(get_loc ()) "unexpected token '%s'" (Lexing.lexeme lb);
        None
      | Error (_, m) -> warn ~source:(get_loc ()) "%s" m; None
      | Logic_utils.Not_well_formed (loc, m) ->
        warn ~source:(fst loc) "%s" m; None
      | Logic_utils.Unknown_ext -> None
      | Log.FeatureRequest(source,_,msg) ->
        let source = Option.value ~default:(get_loc ()) source in
        warn ~source "unimplemented ACSL feature: %s" msg; None
      | Log.(AbortError _ | AbortFatal _) as exn -> raise exn
      | exn ->
        Kernel.fatal ~source:(get_loc ()) "Unknown error (%s)"
          (Printexc.to_string exn)

  let lexpr = parse_from_position Logic_parser.lexpr_eof

  let annot = parse_from_position Logic_parser.annot

  let spec = parse_from_position Logic_parser.spec

  let ext_spec lexbuf =
    ext_acsl_spec:=true;
    let finally() = ext_acsl_spec:=false in
    let work () = Logic_parser.ext_spec token lexbuf in
    Fun.protect ~finally work

  type 'a parse = Filepos.t * string -> (Filepos.t * 'a) option

  let chr lexbuf =
    let buf = Buffer.create 16 in
    chr buf lexbuf
}
