(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

(* File yalexer.mll *)
{
    open Yaparser
}

let num    = ['0'-'9']
let alpha  = ['a'-'z' 'A'-'Z']
let ident  = alpha (num | alpha | '_')*
let string = ([^ '"' '\\']|'\\'_)*

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let binarydigit = ['0' '1']
let letter = ['a'- 'z' 'A'-'Z']

let usuffix = ['u' 'U']
let lsuffix = "l"|"L"|"ll"|"LL"
let intsuffix =
  lsuffix | usuffix | usuffix lsuffix | lsuffix usuffix
| usuffix ? "i64"

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

let floatsuffix = ['f' 'F' 'l' 'L']
let floatnum = (decfloat | hexfloat) floatsuffix?

rule token = parse
    [' ' '\t' ]       { token lexbuf }     (* skip blanks *)
  | '\n'              { Utils_parser.newline lexbuf; token lexbuf }
  | "/*"              { comment lexbuf ; token lexbuf }
  | "//"              { onelinecomment lexbuf ; token lexbuf }
  | floatnum as lxm   { FLOAT(lxm) }
  |	hexnum as lxm     { INT(lxm) }
  |	octnum as lxm     { INT(lxm) }
  | binarynum as lxm  { INT(lxm) }
  |	intnum as lxm     { INT(lxm) }
  | "CALL"            { CALL_OF }
  | "RETURN"          { RETURN_OF }
  | "COR"             { CALLORRETURN_OF }
  | "other"           { OTHERWISE }
  | "true"            { TRUE }
  | "false"           { FALSE }
  | "\\result" as lxm { IDENTIFIER(lxm) }
  | ident as lxm      { IDENTIFIER(lxm) }
  | '$' (ident as lxm){ METAVAR(lxm) }
  | ','               { COMMA }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { STAR }
  | '/'               { SLASH }
  | '%'               { PERCENT }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '['               { LSQUARE }
  | ']'               { RSQUARE }
  | '{'               { LCURLY }
  | '}'               { RCURLY }
  | "{{"              { LBRACELBRACE }
  | "}}"              { RBRACERBRACE }
  | '.'               { DOT }
  | "->"              { RARROW }
  | '&'               { AMP }
  | '|'               { PIPE }
  | "&&"              { AND }
  | "||"              { OR }
  | '!'               { NOT }
  | "<"               { LT }
  | ">"               { GT }
  | "<="              { LE }
  | ">="              { GE }
  | "=="              { EQ }
  | "!="              { NEQ }
  | ';'               { SEMI_COLON }
  | ':'               { COLON }
  | "::"              { COLUMNCOLUMN }
  | '^'               { CARET }
  | '?'               { QUESTION }
  | eof               { EOF }
  | ":="              { AFF }
  | _                 { Utils_parser.unknown_token lexbuf }

and comment = parse
  |  "*/"       {  }
  | eof         { Utils_parser.unterminated_comment lexbuf }
  | _           { comment lexbuf }

and onelinecomment = parse
  | '\n'|eof    {  }
  | _           { onelinecomment lexbuf }
