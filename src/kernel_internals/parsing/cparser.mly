/***************************************************************************/
/*                                                                         */
/*  SPDX-License-Identifier BSD-3-Clause                                   */
/*  Copyright (C) 2001-2003                                                */
/*  George C. Necula    <necula@cs.berkeley.edu>                           */
/*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          */
/*  Wes Weimer          <weimer@cs.berkeley.edu>                           */
/*  Ben Liblit          <liblit@cs.berkeley.edu>                           */
/*  All rights reserved.                                                   */
/*  File modified by                                                       */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*  INRIA (Institut National de Recherche en Informatique et Automatique)  */
/*                                                                         */
/***************************************************************************/

/*  3.22.99 Hugues Cass<E9> First version.
    2.0  George Necula 12/12/00: Practically complete rewrite.
*/


%{

open Cabs
open Cabshelper

(*
** Expression building
*)
let smooth_expression lst =
  match lst with
  | [] -> Kernel.fatal "empty COMMA expression, should not happen"
  | [expr] -> expr
  | hd :: _ ->
    let beg_loc = fst hd.expr_loc in
    let end_loc = snd (List.last lst).expr_loc in
    { expr_loc = (beg_loc,end_loc); expr_node = COMMA (lst) }

let merge_string (c1,(b1,_)) (l2,(_,e2)) = c1 :: l2, (b1,e2)

(* To be called only inside a grammar rule. *)
let make_expr start_end_pos expr_node =
  let expr_loc = Errorloc.convert_loc start_end_pos in
  { expr_loc; expr_node }

let currentFunctionName = ref "<outside any function>"

exception NoProto

(* Go through all the parameter names and mark them as identifiers *)
let rec findProto = function
    PROTO (d, args, _,_) when isJUSTBASE d ->
    List.iter (fun (_, (an, _, _, _)) -> !Lexerhack.add_identifier an) args
  | PROTO (d, _,_, _) -> findProto d
  | PARENTYPE (_, d, _) -> findProto d
  | PTR (_, d) -> findProto d
  | ARRAY (d, _, _) -> findProto d
  | _ -> raise NoProto

and isJUSTBASE = function
    JUSTBASE -> true
  | PARENTYPE (_, d, _) -> isJUSTBASE d
  | _ -> false

let announceFunctionName ((n, decl, _, loc):name) =
  !Lexerhack.add_identifier n;
  (* Start a context that includes the parameter names and the whole body.
   * Will pop when we finish parsing the function body *)
  !Lexerhack.push_context ();
  (try findProto decl
   with NoProto ->
     Errorloc.parse_error
       ~loc "Cannot find the prototype in a function definition");
  currentFunctionName := n

let check_funspec_abrupt_clauses fname spec =
  List.iter (fun bhv ->
      List.iter (function
          | (Cil_types.Normal | Cil_types.Exits),_ -> ()
          | (Cil_types.Breaks | Cil_types.Continues | Cil_types.Returns),
            { Logic_ptree.tp_statement = { lexpr_loc = loc}} ->
            Errorloc.parse_error ~loc
              "Specification of function %s can only contain ensures or \
               exits post-conditions" fname)
        bhv.Logic_ptree.b_post_cond)
    spec.Logic_ptree.spec_behavior

let applyPointer (ptspecs: attribute list list) (dt: decl_type)
  : decl_type =
  (* Outer specification first *)
  let rec loop = function
      [] -> dt
    | attrs :: rest -> PTR(attrs, loop rest)
  in
  loop ptspecs

let register_symbol_class nl =
  let add =
    if !Lexerhack.is_typedef() then !Lexerhack.add_type
    else !Lexerhack.add_identifier
  in
  List.iter (fun ((n, _, _, _), _) -> add n) nl

let doDeclaration logic_spec (loc: cabsloc) (specs: spec_elem list) (nl: init_name list)  =
  if isTypedef specs then begin
    TYPEDEF ((specs, List.map (fun (n, _) -> n) nl), loc)
  end else
  if nl = [] then
    ONLYTYPEDEF (specs, loc)
  else begin
    !Lexerhack.push_context ();
    List.iter
      (fun ((_,t,_,_),_) ->
         try findProto t with NoProto -> ())
      nl;
    let logic_spec =
      match logic_spec with
      | None -> None
      | Some (loc, _ as ls) -> begin
          Option.map
            (fun (loc', spec) ->
               let name =
                 match nl with
                 | [ (n,_,_,_),_ ] -> n
                 | _ -> "unknown function"
               in
               check_funspec_abrupt_clauses name spec;
               (spec, (loc, loc')))
            (Logic_lexer.spec ls)
        end
    in
    !Lexerhack.pop_context ();
    DECDEF (logic_spec, (specs, nl), loc)
  end

let in_ghost =
  let ghost_me = object
    inherit Cabsvisit.nopCabsVisitor
    method! vstmt s =
      s.stmt_ghost <- true;
      Cil.DoChildren
  end
  in
  List.map
    (fun s -> ignore (Cabsvisit.visitCabsStatement ghost_me s); s)

let ghost_global = ref false

let doFunctionDef spec (loc: cabsloc)
    (lend: cabsloc)
    (specs: spec_elem list)
    (n: name)
    (b: block) : definition =
  let fname = (specs, n) in
  let name = match n with (n,_,_,_) -> n in
  Option.iter (fun (spec, _) -> check_funspec_abrupt_clauses name spec) spec;
  let b = if !ghost_global then { b with bstmts = in_ghost b.bstmts } else b in
  FUNDEF (spec, fname, b, loc, lend)

let doOldParDecl floc (names: string list)
    ((pardefs: name_group list), (isva: bool)) : single_name list * bool =
  let findOneName n =
    (* Search in pardefs for the definition for this parameter *)
    let rec loopGroups = function
      | [] -> ([SpecType Tint], (n, JUSTBASE, [], floc))
      | (specs, names) :: restgroups ->
        let rec loopNames = function
          | [] -> loopGroups restgroups
          | ((n',_, _, _) as sn) :: _ when n' = n -> (specs, sn)
          | _ :: restnames -> loopNames restnames
        in
        loopNames names
    in
    loopGroups pardefs
  in
  let args = List.map findOneName names in
  (args, isva)

let int64_to_char ~loc value =
  if (Int64.compare value (Int64.of_int 255) > 0) ||
     (Int64.compare value Int64.zero < 0) then
    Errorloc.parse_error ~loc "integral literal 0x%Lx too big" value
  else
    Char.chr (Int64.to_int value)

(* takes a not-nul-terminated list, and converts it to a string. *)
let intlist_to_string ~loc str =
  let buffer = Buffer.create 64 in
  let add_char c =
    Buffer.add_char buffer (int64_to_char ~loc c)
  in
  let add_char_list l = List.iter add_char l in
  List.iter add_char_list str ;
  Buffer.contents buffer

let fst3 (result, _, _) = result
let trd3 (_, _, result) = result

let fourth4 (_,_,_,result) = result

let sizeofType () =
  let findSpecifier name =
    let convert_one_specifier s =
      if s = "int" then Cabs.Tint
      else if s = "unsigned" then Cabs.Tunsigned
      else if s = "long" then Cabs.Tlong
      else if s = "short" then Cabs.Tshort
      else if s = "char" then Cabs.Tchar
      else
        Kernel.fatal
          ~current:true
          "Cparser.sizeofType: cannot find the right specifier for type %s" name
    in
    let add_one_specifier s acc =
      (Cabs.SpecType (convert_one_specifier s)) :: acc
    in
    let specs = Str.split (Str.regexp " +") name in
    List.fold_right add_one_specifier specs []
  in
  findSpecifier (Machine.size_t ())


(*
   transform:  offsetof(type, member)
   into     :  (size_t) (&(type * ) 0)->member
 *)

let transformOffsetOf (speclist, dtype) member =
  let mk_expr e = { expr_loc = member.expr_loc; expr_node = e } in
  let rec addPointer = function
    | JUSTBASE ->
      PTR([], JUSTBASE)
    | PARENTYPE (attrs1, dtype, attrs2) ->
      PARENTYPE (attrs1, addPointer dtype, attrs2)
    | ARRAY (dtype, attrs, expr) ->
      ARRAY (addPointer dtype, attrs, expr)
    | PTR (attrs, dtype) ->
      PTR (attrs, addPointer dtype)
    | PROTO (dtype, names, gnames, variadic) ->
      PROTO (addPointer dtype, names, gnames,variadic)
  in
  let nullType = (speclist, addPointer dtype) in
  let nullExpr = mk_expr (CONSTANT (CONST_INT "0")) in
  let castExpr = mk_expr (CAST (nullType, SINGLE_INIT nullExpr)) in

  let rec replaceBase e =
    let node = match e.expr_node with
      | VARIABLE field ->
        MEMBEROFPTR (castExpr, field)
      | MEMBEROF (base, field) ->
        MEMBEROF (replaceBase base, field)
      | INDEX (base, index) ->
        INDEX (replaceBase base, index)
      | _ ->
        Errorloc.parse_error ~loc:e.expr_loc
          "malformed offset expression in offsetof macro"
    in { e with expr_node = node }
  in
  let memberExpr = replaceBase member in
  let addrExpr = { memberExpr with expr_node = UNARY (ADDROF, memberExpr)} in
  let sizeofType = sizeofType(), JUSTBASE in
  { addrExpr with expr_node = CAST (sizeofType, SINGLE_INIT addrExpr)}

let no_ghost_stmt s = {stmt_ghost = false ; stmt_node = s}
let ghost_stmt s = {stmt_ghost = true ; stmt_node = s}

let no_ghost = List.map no_ghost_stmt

let in_block sloc l =
  let loc = Errorloc.convert_loc sloc in
  match l with
  (* in_block should always called on parsed statement, which cannot return an
      empty list. *)
  | [] ->
    Errorloc.parse_error ~loc "empty list of statement, should not happen"
  | [s] -> s
  | _::_ ->
    no_ghost_stmt (BLOCK ({ blabels = []; battrs = []; bstmts = l},
                          get_statementloc (List.hd l),
                          get_statementloc (List.last l)))

let in_ghost_block ?(battrs=[]) l =
  let l = in_ghost l in
  ghost_stmt (BLOCK ({ blabels = []; battrs ; bstmts = l},
                     get_statementloc (List.hd l),
                     get_statementloc (List.last l)))

let type_to_expr_for_builtin ~loc ~builtin specifier decl_type =
  (* Prefix the builtin name with __fc_ to avoid collisions with real
     world code. *)
  let builtin = "__fc_" ^ builtin in
  (* Build an expression representing the given type with a marker to indicate
     the builtin for which this translation was made. *)
  let info = SINGLE_INIT { expr_loc = loc; expr_node = VARIABLE builtin } in
  { expr_loc = loc; expr_node = CAST ((specifier, decl_type), info) }

%}

%token <Filepos.t * string> SPEC
%token <Logic_ptree.decl list> DECL
%token <Logic_ptree.code_annot * Cabs.cabsloc> CODE_ANNOT
%token <Logic_ptree.code_annot list * Cabs.cabsloc> LOOP_ANNOT

%token <string> IDENT
%token <int64 list * Cabs.cabsloc> CST_CHAR
%token <int64 list * Cabs.cabsloc> CST_WCHAR
%token <string * Cabs.cabsloc> CST_INT
%token <string * Cabs.cabsloc> CST_FLOAT
%token <string> NAMED_TYPE

/* Each character is its own list element, and the terminating nul is not
   included in this list. */
%token <int64 list * Cabs.cabsloc> CST_STRING
%token <int64 list * Cabs.cabsloc> CST_WSTRING

%token EOF
%token<Cabs.cabsloc> BOOL TRUE FALSE CHAR INT DOUBLE FLOAT VOID
%token<Cabs.cabsloc> INT64 INT128
%token<Cabs.cabsloc> FLOAT32 FLOAT64 ENUM STRUCT TYPEDEF UNION
%token<Cabs.cabsloc> SIGNED UNSIGNED LONG SHORT
%token<Cabs.cabsloc> VOLATILE EXTERN STATIC CONST RESTRICT AUTO REGISTER
%token<Cabs.cabsloc> THREAD THREAD_LOCAL
%token<Cabs.cabsloc> GHOST

%token<Cabs.cabsloc> SIZEOF ALIGNOF ALIGNAS GENERIC
%token<Cabs.cabsloc> GCC_ALIGNOF

%token EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
%token AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%token ARROW DOT

%token EQ_EQ EXCLAM_EQ INF SUP INF_EQ SUP_EQ
%token<Cabs.cabsloc> PLUS MINUS STAR
%token SLASH PERCENT
%token<Cabs.cabsloc> TILDE AND
%token PIPE CIRC
%token<Cabs.cabsloc> EXCLAM AND_AND
%token PIPE_PIPE
%token INF_INF SUP_SUP
%token<Cabs.cabsloc> PLUS_PLUS MINUS_MINUS

%token RPAREN
%token<Cabs.cabsloc> LPAREN RBRACE
%token<Cabs.cabsloc> LBRACE
%token LBRACKET RBRACKET
%token COLON COLON2
%token<Cabs.cabsloc> SEMICOLON
%token COMMA ELLIPSIS QUEST

%token<Cabs.cabsloc> BREAK CONTINUE GOTO RETURN
%token<Cabs.cabsloc> SWITCH CASE DEFAULT
%token<Cabs.cabsloc> WHILE DO FOR
%token<Cabs.cabsloc> IF
%token ELSE

%token<Cabs.cabsloc> NOP_ATTRIBUTE ATTRIBUTE INLINE NORETURN STATIC_ASSERT ASM TYPEOF FUNCTION__ PRETTY_FUNCTION__
%token LABEL__
%token<Cabs.cabsloc> BUILTIN_VA_ARG
%token BLOCKATTRIBUTE
%token<Cabs.cabsloc> BUILTIN_TYPES_COMPAT BUILTIN_OFFSETOF
%token<Cabs.cabsloc> DECLSPEC
%token<string * Cabs.cabsloc> MSATTR
%token<string * Cabs.cabsloc> PRAGMA_LINE
%token<Cabs.cabsloc> PRAGMA
%token PRAGMA_EOL

/*Frama-C: ghost bracketing */
%token LGHOST RGHOST
%token<Cabs.cabsloc> LGHOST_ELSE

/* operator precedence */
%nonassoc   if_no_else
%nonassoc   ghost_else_no_else
%nonassoc   ELSE LGHOST_ELSE

%right  NAMED_TYPE /* We'll use this to handle redefinitions of NAMED_TYPE as variables */
%left   IDENT

/* Non-terminals information */
%start interpret file
%type <(bool*Cabs.definition) list> file interpret globals

%type <Cabs.definition> global


%type <Cabs.attribute list> attributes attributes_with_asm asmattr
%type <Cabs.constant * cabsloc> constant
%type <string * cabsloc> string_constant
%type <Cabs.expression> expression
%type <Cabs.expression> opt_expression
%type <Cabs.init_expression> init_expression
%type <Cabs.expression list> comma_expression
%type <Cabs.expression list> paren_comma_expression
%type <Cabs.expression list> arguments
%type <Cabs.expression list> bracket_comma_expression

%type <Cabs.initwhat * Cabs.init_expression> initializer_single
%type <(Cabs.initwhat * Cabs.init_expression) list> initializer_list
%type <Cabs.initwhat> init_designators init_designators_opt

%type <spec_elem list * cabsloc> decl_spec_list
%type <typeSpecifier * cabsloc> type_spec
%type <Cabs.field_group list> struct_decl_list


%type <Cabs.name> old_proto_decl
%type <Cabs.single_name> parameter_decl
%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <cabsloc * spec_elem list * name> function_def_start
%type <Cabs.spec_elem list * Cabs.decl_type> type_name
%type <Cabs.block * cabsloc * cabsloc> block
%type <Cabs.statement list> block_element_list
%type <string list> local_labels local_label_names
%type <string list> old_parameter_list_ne

%type <Cabs.init_name> init_declarator
%type <Cabs.init_name list> init_declarator_list
%type <Cabs.name> declarator
%type <Cabs.name * expression option> field_decl
%type <(Cabs.name * expression option) list> field_decl_list
%type <string * Cabs.decl_type * cabsloc> direct_decl
%type <Cabs.decl_type> abs_direct_decl abs_direct_decl_opt
%type <Cabs.decl_type * Cabs.attribute list> abstract_decl

 /* (* Each element is a "* <type_quals_opt>". *) */
%type <attribute list list> pointer pointer_opt
%type <Cabs.spec_elem * cabsloc> cvspec
%type <(Cabs.spec_elem list * Cabs.decl_type) option * expression> generic_association
%type <((Cabs.spec_elem list * Cabs.decl_type) option * expression) list> generic_association_list
%%

interpret: file { $1 }

file: globals EOF {$1}

globals:
| /* empty */                            { [] }
| global globals                         { (false,$1) :: $2 }
| ghost_glob_begin ghost_globals globals { $2 @ $3 }
| SEMICOLON globals                      { $2 }
;

ghost_glob_begin:
| LGHOST { ghost_global:=true }
;

/* Rules for global ghosts: TODO keep the ghost status! */
ghost_globals:
| declaration ghost_globals  { (true,$1)::$2 }
| function_def ghost_globals { (true,$1)::$2 }
| RGHOST                     { ghost_global:=false; [] }
;

/*** Global Definition ***/
global:
| DECL         { GLOBANNOT $1 }
| declaration  { $1 }
| function_def { $1 }

/*(* Some C header files are shared with the C++ compiler and have linkage
   * specification *)*/
| EXTERN string_constant declaration
  { LINKAGE (fst $2, (*handleLoc*) (snd $2), [ $3 ]) }
| EXTERN string_constant LBRACE globals RBRACE {
    LINKAGE (
      fst $2, (*handleLoc*) (snd $2),
      List.map
        (fun (x,y) ->
          if x then
            let loc = Cabshelper.get_definitionloc y in
            Errorloc.parse_error ~loc
              "invalid ghost in extern linkage specification"
          else y)
      $4
    )
  }
| ASM LPAREN string_constant RPAREN SEMICOLON
  { GLOBASM (fst $3, (*handleLoc*) $1) }
| pragma { $1 }
/* (* Old-style function prototype. This should be somewhere else, like in
    * "declaration". For now we keep it at global scope only because in local
    * scope it looks too much like a function call  *) */
| f=IDENT LPAREN prms=old_parameter_list_ne RPAREN decls=old_pardef_list SEMICOLON {
    let dloc = Errorloc.convert_loc $loc in
    let floc = Errorloc.convert_loc $loc(f) in
    (* Convert pardecl to new style *)
    let pardecl, isva = doOldParDecl floc prms decls in
    (* Make the function declarator *)
    doDeclaration None dloc []
      [((f, PROTO(JUSTBASE, pardecl,[],isva),
        [Ast_attributes.fc_oldstyleproto,[]], floc), NO_INIT)]
  }
| f=IDENT LPAREN RPAREN SEMICOLON {
    let dloc = Errorloc.convert_loc $loc in
    let floc = Errorloc.convert_loc $loc(f) in
    doDeclaration None dloc []
      [((f, PROTO(JUSTBASE,[],[],false),
        [Ast_attributes.fc_oldstyleproto,[]], floc), NO_INIT)]
  }
;

id_or_typename_as_id:
| IDENT      { $1 }
| NAMED_TYPE { $1 }
;

id_or_typename: id_or_typename_as_id { $1 }

maybecomma:
| /* empty */ { () }
| COMMA       { () }
;

/* *** Expressions *** */

primary_expression:                     /*(* 6.5.1. *)*/
| IDENT                  { make_expr $sloc (VARIABLE $1) }
| constant {
    let (v,expr_loc) = $1 in
    { expr_loc; expr_node = CONSTANT v }
  }
| paren_comma_expression { make_expr $sloc (PAREN (smooth_expression $1)) }
| LPAREN block RPAREN    { make_expr $sloc (GNU_BODY (fst3 $2)) }
| generic_selection      { make_expr $sloc (GENERIC $1) }
;

postfix_expression:                     /*(* 6.5.2 *)*/
| primary_expression { $1 }
| postfix_expression bracket_comma_expression
    { make_expr $sloc (INDEX ($1, smooth_expression $2))}
| postfix_expression LPAREN arguments RPAREN ghost_arguments_opt
    { make_expr $sloc (CALL ($1, $3, $5))}
| BUILTIN_VA_ARG LPAREN expression COMMA type_name RPAREN {
    let builtin = "__builtin_va_arg" in
    let b, d = $5 in
    let loc = Errorloc.convert_loc $loc($5) in
    let loc_f = Errorloc.convert_loc $loc($1) in
    let type_as_expr = type_to_expr_for_builtin ~loc ~builtin b d in
    make_expr $sloc
      (CALL
          ({ expr_loc = loc_f;
            expr_node = VARIABLE builtin},
          [$3; type_as_expr],[]))
  }
| BUILTIN_TYPES_COMPAT LPAREN type_name COMMA type_name RPAREN {
    let builtin = "__builtin_types_compatible_p" in
    let b1,d1 = $3 in
    let b2,d2 = $5 in
    let loc_f = Errorloc.convert_loc $loc($1) in
    let loc1 = Errorloc.convert_loc $loc($3) in
    let loc2 = Errorloc.convert_loc $loc($5) in
    let type_as_expr1 = type_to_expr_for_builtin ~loc:loc1 ~builtin b1 d1 in
    let type_as_expr2 = type_to_expr_for_builtin ~loc:loc2 ~builtin b2 d2 in
    make_expr $sloc
      (CALL
          ({expr_loc = loc_f;
            expr_node = VARIABLE builtin},
          [ type_as_expr1; type_as_expr2],[]))
  }
| BUILTIN_OFFSETOF LPAREN type_name COMMA offsetof_member_designator RPAREN {
    let loc_f = Errorloc.convert_loc $loc($1) in
    let arg = transformOffsetOf $3 $5 in
    let builtin = { expr_loc = loc_f;
                    expr_node = VARIABLE "__builtin_offsetof" }
    in
    make_expr $sloc (CALL (builtin, [ arg ], []))
  }
| postfix_expression DOT id_or_typename { make_expr $sloc (MEMBEROF ($1, $3)) }
| postfix_expression ARROW id_or_typename
    { make_expr $sloc (MEMBEROFPTR ($1, $3)) }
| postfix_expression PLUS_PLUS { make_expr $sloc (UNARY (POSINCR, $1)) }
| postfix_expression MINUS_MINUS { make_expr $sloc (UNARY (POSDECR, $1)) }
/* (* We handle GCC constructor expressions *) */
| LPAREN type_name RPAREN LBRACE initializer_list_opt RBRACE
    { make_expr $sloc (CAST($2, COMPOUND_INIT $5)) }
;

offsetof_member_designator: /* GCC extension for __builtin_offsetof */
| id_or_typename { make_expr $sloc (VARIABLE $1) }
| offsetof_member_designator DOT IDENT { make_expr $sloc (MEMBEROF ($1, $3)) }
| offsetof_member_designator bracket_comma_expression
    { make_expr $sloc (INDEX ($1, smooth_expression $2)) }
;

unary_expression:   /*(* 6.5.3 *)*/
| postfix_expression           { $1 }
| PLUS_PLUS unary_expression   { make_expr $sloc (UNARY (PREINCR, $2)) }
| MINUS_MINUS unary_expression { make_expr $sloc (UNARY (PREDECR, $2)) }
| SIZEOF unary_expression      { make_expr $sloc (EXPR_SIZEOF $2) }
| SIZEOF LPAREN type_name RPAREN
    { let b, d = $3 in make_expr $sloc (TYPE_SIZEOF (b, d)) }
| ALIGNOF unary_expression     { make_expr $sloc (EXPR_ALIGNOF($2, `Standard)) }
| ALIGNOF LPAREN type_name RPAREN
    {let b, d = $3 in make_expr $sloc (TYPE_ALIGNOF (b, d, `Standard)) }
| GCC_ALIGNOF unary_expression     { make_expr $sloc (EXPR_ALIGNOF($2, `GCC)) }
| GCC_ALIGNOF LPAREN type_name RPAREN
    {let b, d = $3 in make_expr $sloc (TYPE_ALIGNOF (b, d, `GCC)) }

| PLUS cast_expression         { make_expr $sloc (UNARY (PLUS, $2)) }
| MINUS cast_expression        { make_expr $sloc (UNARY (MINUS, $2)) }
| STAR cast_expression         { make_expr $sloc (UNARY (MEMOF, $2)) }
| AND cast_expression          { make_expr $sloc (UNARY (ADDROF, $2)) }
| EXCLAM cast_expression       { make_expr $sloc (UNARY (NOT, $2)) }
| TILDE cast_expression        { make_expr $sloc (UNARY (BNOT, $2)) }
/* (* GCC allows to take address of a label (see COMPGOTO statement) *) */
| AND_AND id_or_typename_as_id { make_expr $sloc (LABELADDR $2) }
;

cast_expression:   /*(* 6.5.4 *)*/
| unary_expression { $1 }
| LPAREN type_name RPAREN cast_expression
    { make_expr $sloc (CAST($2, SINGLE_INIT $4)) }
;

multiplicative_expression:  /*(* 6.5.5 *)*/
| cast_expression { $1 }
| multiplicative_expression STAR cast_expression
    { make_expr $sloc (BINARY(MUL, $1, $3)) }
| multiplicative_expression SLASH cast_expression
    { make_expr $sloc (BINARY(DIV, $1, $3)) }
| multiplicative_expression PERCENT cast_expression
    { make_expr $sloc (BINARY(MOD, $1, $3)) }
;

additive_expression:  /*(* 6.5.6 *)*/
| multiplicative_expression { $1 }
| additive_expression PLUS multiplicative_expression
    { make_expr $sloc (BINARY(ADD, $1, $3)) }
| additive_expression MINUS multiplicative_expression
    { make_expr $sloc (BINARY(SUB, $1, $3)) }
;

shift_expression:      /*(* 6.5.7 *)*/
| additive_expression { $1 }
| shift_expression  INF_INF additive_expression
    { make_expr $sloc (BINARY(SHL, $1, $3)) }
| shift_expression  SUP_SUP additive_expression
    { make_expr $sloc (BINARY(SHR, $1, $3)) }
;

relational_expression:   /*(* 6.5.8 *)*/
| shift_expression { $1 }
| relational_expression INF shift_expression
    { make_expr $sloc (BINARY(LT, $1, $3)) }
| relational_expression SUP shift_expression
    { make_expr $sloc (BINARY(GT, $1, $3)) }
| relational_expression INF_EQ shift_expression
    { make_expr $sloc (BINARY(LE, $1, $3)) }
| relational_expression SUP_EQ shift_expression
    { make_expr $sloc (BINARY(GE, $1, $3)) }
;

equality_expression:   /*(* 6.5.9 *)*/
| relational_expression { $1 }
| equality_expression EQ_EQ relational_expression
    { make_expr $sloc (BINARY(EQ, $1, $3)) }
| equality_expression EXCLAM_EQ relational_expression
    { make_expr $sloc (BINARY(NE, $1, $3)) }
;

bitwise_and_expression:   /*(* 6.5.10 *)*/
| equality_expression { $1 }
| bitwise_and_expression AND equality_expression
    { make_expr $sloc (BINARY(BAND, $1, $3)) }
;

bitwise_xor_expression:   /*(* 6.5.11 *)*/
| bitwise_and_expression { $1 }
| bitwise_xor_expression CIRC bitwise_and_expression
    { make_expr $sloc (BINARY(XOR, $1, $3)) }
;

bitwise_or_expression:   /*(* 6.5.12 *)*/
| bitwise_xor_expression { $1 }
| bitwise_or_expression PIPE bitwise_xor_expression
    { make_expr $sloc (BINARY(BOR, $1, $3)) }
;

logical_and_expression:   /*(* 6.5.13 *)*/
| bitwise_or_expression { $1 }
| logical_and_expression AND_AND bitwise_or_expression
    { make_expr $sloc (BINARY(AND, $1, $3)) }
;

logical_or_expression:   /*(* 6.5.14 *)*/
| logical_and_expression { $1 }
| logical_or_expression PIPE_PIPE logical_and_expression
    { make_expr $sloc (BINARY(OR, $1, $3)) }
;

conditional_expression:    /*(* 6.5.15 *)*/
| logical_or_expression { $1 }
| logical_or_expression QUEST opt_expression COLON conditional_expression
    { make_expr $sloc (QUESTION ($1, $3, $5)) }
;

/*(* The C spec says that left-hand sides of assignment expressions are unary
 * expressions. GCC allows cast expressions in there ! *)*/

assignment_expression:     /*(* 6.5.16 *)*/
| conditional_expression { $1 }
| cast_expression EQ assignment_expression
    { make_expr $sloc (BINARY(ASSIGN, $1, $3)) }
| cast_expression PLUS_EQ assignment_expression
    { make_expr $sloc (BINARY(ADD_ASSIGN, $1, $3)) }
| cast_expression MINUS_EQ assignment_expression
    { make_expr $sloc (BINARY(SUB_ASSIGN, $1, $3)) }
| cast_expression STAR_EQ assignment_expression
    { make_expr $sloc (BINARY(MUL_ASSIGN, $1, $3)) }
| cast_expression SLASH_EQ assignment_expression
    { make_expr $sloc (BINARY(DIV_ASSIGN, $1, $3)) }
| cast_expression PERCENT_EQ assignment_expression
    { make_expr $sloc (BINARY(MOD_ASSIGN, $1, $3)) }
| cast_expression AND_EQ assignment_expression
    { make_expr $sloc (BINARY(BAND_ASSIGN, $1, $3)) }
| cast_expression PIPE_EQ assignment_expression
    { make_expr $sloc (BINARY(BOR_ASSIGN, $1, $3)) }
| cast_expression CIRC_EQ assignment_expression
    { make_expr $sloc (BINARY(XOR_ASSIGN, $1, $3)) }
| cast_expression INF_INF_EQ assignment_expression
    { make_expr $sloc (BINARY(SHL_ASSIGN, $1, $3)) }
| cast_expression SUP_SUP_EQ assignment_expression
    { make_expr $sloc (BINARY(SHR_ASSIGN, $1, $3))}
;

/*(* 6.5.17 *)*/
expression: assignment_expression { $1 }

constant:
| TRUE            {CONST_BOOL (true), $1}
| FALSE           {CONST_BOOL (false), $1}
| CST_INT         {CONST_INT (fst $1), snd $1}
| CST_FLOAT       {CONST_FLOAT (fst $1), snd $1}
| CST_CHAR        {CONST_CHAR (fst $1), snd $1}
| CST_WCHAR       {CONST_WCHAR (fst $1), snd $1}
| string_constant {CONST_STRING (fst $1), snd $1}
| wstring_list    {CONST_WSTRING (List.concat (fst $1)), snd $1}
;

string_constant:
/* Now that we know this constant isn't part of a wstring, convert it
   back to a string for easy viewing. */
| string_list {
    let loc = Errorloc.convert_loc $loc in
    intlist_to_string ~loc (fst $1), snd $1
  }
;

string_list:
| one_string             { [fst $1], snd $1 }
| one_string string_list { merge_string $1 $2 }
;

wstring_list:
| CST_WSTRING              { [fst $1], snd $1 }
| one_string wstring_list  { merge_string $1 $2 }
| CST_WSTRING wstring_list { merge_string $1 $2 }
| CST_WSTRING string_list  { merge_string $1 $2 }
/* If a wstring is present anywhere in the list, the whole is a wstring */

one_string:
| CST_STRING {$1}
| FUNCTION__ {(Cabshelper.explodeStringToInts !currentFunctionName), $1}
| PRETTY_FUNCTION__ {(Cabshelper.explodeStringToInts !currentFunctionName), $1}
;

init_expression:
| expression                         { SINGLE_INIT $1 }
| LBRACE initializer_list_opt RBRACE { COMPOUND_INIT $2}

initializer_list:    /* ISO 6.7.8. Allow a trailing COMMA */
| initializer_single                             { [$1] }
| initializer_single COMMA initializer_list_opt  { $1 :: $3 }
;

initializer_list_opt:
| /* empty */      { [] }
| initializer_list { $1 }
;

initializer_single:
| init_designators eq_opt init_expression { ($1, $3) }
| gcc_init_designators init_expression    { ($1, $2) }
| init_expression                         { (NEXT_INIT, $1) }
;

eq_opt:
| EQ          { () }
/*(* GCC allows missing = *)*/
| /* empty */ { () }
;

init_designators:
| DOT id_or_typename init_designators_opt           { INFIELD_INIT($2, $3) }
| LBRACKET expression RBRACKET init_designators_opt { ATINDEX_INIT($2, $4) }
| LBRACKET expression ELLIPSIS expression RBRACKET
    { ATINDEXRANGE_INIT($2, $4) }
;

init_designators_opt:
| /* empty */      { NEXT_INIT }
| init_designators { $1 }
;

gcc_init_designators: /* GCC supports these strange things */
| id_or_typename COLON { INFIELD_INIT($1, NEXT_INIT) }
;

ghost_arguments_opt:
| /* empty */     { [] }
| ghost_arguments { $1 }

ghost_arguments:
| LGHOST LPAREN arguments RPAREN RGHOST { $3 }

arguments:
| /* empty */         { [] }
| comma_expression    { $1 }
;

opt_expression:
| /* empty */      {make_expr $sloc NOTHING}
| comma_expression {smooth_expression $1 }
;

comma_expression:
| expression                        { [$1] }
| expression COMMA comma_expression { $1 :: $3 }
;

comma_expression_opt:
| /* empty */      { make_expr $sloc NOTHING }
| comma_expression { smooth_expression $1 }
;

paren_comma_expression:
| LPAREN comma_expression RPAREN { $2 }
;

bracket_comma_expression:
| LBRACKET comma_expression RBRACKET { $2 }
;

/*** statements ***/
generic_block(content):
| block_begin local_labels block_attrs content RBRACE
    { { blabels = $2; battrs = $3; bstmts = $4 }, $1, $5 }
;

block: /* ISO 6.8.2 */
| generic_block(block_content) { $1 }

main_block:
| generic_block(main_block_content) { $1 }

block_begin:
| LBRACE { !Lexerhack.push_context (); $1 }
;

block_attrs:
| /* empty */                       { [] }
| BLOCKATTRIBUTE paren_attr_list_ne { [("__blockattribute__", $2)] }
;

block_content: block_element_list { !Lexerhack.pop_context(); $1 }

/* for the main block of a function, we must pop
   _two_ contexts: the one of the block itself, and the one
   introduced for the definition itself (which includes the
   parameters and the function name)
*/
main_block_content: block_element_list {
    !Lexerhack.pop_context();
    !Lexerhack.pop_context();
    $1
}

/* statements and declarations in a block, in any order (for C99 support) */
block_element_list:
| annot_list_opt                              { $1 }
| annot_list_opt declaration block_element_list
    { $1 @ no_ghost_stmt (DEFINITION($2)) :: $3 }
| annot_list_opt statement block_element_list { $1 @ $2 @ $3 }
| annot_list_opt pragma block_element_list    { $1 @ $3 }
;

annot_list_opt:
| /* empty */ { [] }
| annot_list  { $1 }
;

annot_list:
| CODE_ANNOT annot_list_opt { no_ghost [Cabs.CODE_ANNOT $1] @ $2}
| LGHOST block_element_list RGHOST annot_list_opt { (in_ghost $2) @ $4 }
;

local_labels:
| /* empty */                                       { [] }
| LABEL__ local_label_names SEMICOLON local_labels  { $2 @ $4 }
;

local_label_names:
| id_or_typename_as_id                         { [ $1 ] }
| id_or_typename_as_id COMMA local_label_names { $1 :: $3 }
;

annotated_statement:
| statement { $1 }
| annot_list statement { $1 @ $2 }
;

else_part:
| /* empty */ {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost_stmt (NOP (None, loc))
  }
  %prec if_no_else /* To attach the next else to the current if */
| ELSE annotated_statement { in_block $loc($2) $2 }
| LGHOST_ELSE annotated_statement RGHOST
    { in_ghost_block ~battrs:[ (Ast_attributes.frama_c_ghost_else , []) ] $2 }
    %prec ghost_else_no_else /* To force the non ghost else to be attached to the current if */
| LGHOST_ELSE annotated_statement RGHOST ELSE annotated_statement {
    let loc = Errorloc.convert_loc $sloc in
    Kernel.warning ~wkey:Kernel.wkey_ghost_bad_use ~source:(fst loc)
      "Invalid ghost else ignored" ;
    in_block $loc($5) $5
  }

statement:
/* GCC's Label and Statement Attributes can only happen on empty statement.
   Due to conflicts with declaration, for now we only accept a single attribute
   per statement, whereas GCC allows an attribute specifier list. */
| attr=attribute_nocv? loc=SEMICOLON {
    let attr = Option.map fst attr in
    no_ghost [NOP (attr, loc)]
  }
| SPEC annotated_statement {
    let bs = $2 in
    match Logic_lexer.spec $1 with
    | Some (loc',spec) ->
      let spec = no_ghost [Cabs.CODE_SPEC (spec, (fst $1, loc'))] in
      spec @ $2
    | None -> bs
  }
| comma_expression SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [COMPUTATION (smooth_expression $1,loc)]
  }
| block { let (x,y,z) = $1 in no_ghost [BLOCK (x, y, z)] }
| IF paren_comma_expression annotated_statement else_part {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [IF (smooth_expression $2, in_block $loc($3) $3, $4, loc)]
  }
| SWITCH paren_comma_expression annotated_statement {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [SWITCH (smooth_expression $2, in_block $loc($3) $3, loc)]
  }
| opt_loop_annotations WHILE paren_comma_expression annotated_statement {
    let first = Errorloc.convert_pos $startpos($2) in
    let last = Errorloc.convert_pos $endpos in
    no_ghost [WHILE ($1, smooth_expression $3, in_block $loc($4) $4, (first,last))]
  }
| opt_loop_annotations DO annotated_statement WHILE paren_comma_expression SEMICOLON {
    let first = Errorloc.convert_pos $startpos($2) in
    let last = Errorloc.convert_pos $endpos in
    no_ghost [DOWHILE ($1, smooth_expression $5, in_block $loc($3) $3, (first,last))]
  }
| opt_loop_annotations FOR LPAREN for_clause opt_expression SEMICOLON
  opt_expression RPAREN annotated_statement {
    let first = Errorloc.convert_pos $startpos($2) in
    let last = Errorloc.convert_pos $endpos in
    no_ghost [FOR ($1, $4, $5, $7, in_block $loc($9) $9, (first,last))]
  }
| id = id_or_typename_as_id COLON s = annotated_statement {
    let loc = Errorloc.convert_loc $loc(id) in
    match s with
    | [] -> (* should not happen if grammar is written correctly *)
      Errorloc.parse_error ~loc "empty statement after label"
    | s :: others -> no_ghost [LABEL(id,s,loc)] @ others
  }
| CASE expression COLON annotated_statement {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [CASE ($2, in_block $loc($4) $4, loc)]
  }
| CASE expression ELLIPSIS expression COLON annotated_statement {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [CASERANGE ($2, $4, in_block $loc($6) $6, loc)]
  }
| DEFAULT COLON annotated_statement {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [DEFAULT (in_block $loc($3) $3, loc)]
  }
| RETURN SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [RETURN ({ expr_loc = loc; expr_node = NOTHING}, loc)]
  }
| RETURN comma_expression SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [RETURN (smooth_expression $2, loc)]
  }
| BREAK SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [BREAK loc]
  }
| CONTINUE SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [CONTINUE loc]
  }
| GOTO id_or_typename_as_id SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [GOTO ($2, loc)]
  }
| GOTO STAR comma_expression SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [COMPGOTO (smooth_expression $3, loc) ]
  }
| ASM GOTO asmattr LPAREN asmtemplate asmoutputs RPAREN SEMICOLON {
    let loc = Errorloc.convert_loc $loc in
    no_ghost [ASM ($3, mk_asm_templates $5, $6, loc)]
  }
| ASM asmattr LPAREN asmtemplate asmoutputs RPAREN SEMICOLON {
    let loc = Errorloc.convert_loc $sloc in
    no_ghost [ASM ($2, mk_asm_templates $4, $5, loc)]
  }
;

opt_loop_annotations:
| /* empty */      { [] }
| loop_annotations { $1 }
;

loop_annotations: loop_annotation { $1 }
/* Not in ACSL Grammar
| loop_annotation loop_annotations
    { { Cil_types.invariant = $1.Cil_types.invariant @ $2.Cil_types.invariant;
        Cil_types.loop_assigns = $1.Cil_types.loop_assigns @ $2.Cil_types.loop_assigns ;
        Cil_types.variant = $1.Cil_types.variant @ $2.Cil_types.variant;
        Cil_types.pragma = $1.Cil_types.pragma @ $2.Cil_types.pragma }
    }
*/
;

loop_annotation:
| LOOP_ANNOT { fst $1 }
;

for_clause:
| opt_expression SEMICOLON { FC_EXP $1 }
| declaration              { FC_DECL $1 }
;

ghost_parameter_opt:
| /* empty */     {[]}
| ghost_parameter {$1}
;

ghost_parameter:
| LGHOST parameter_list_startscope rest_par_list RPAREN RGHOST
    { let (l, _) = $3 in l }
;

declaration:                                /* ISO 6.7.*/
| spec=ioption(SPEC) specif=decl_spec_list decls=init_declarator_list? SEMICOLON {
    let loc = Errorloc.convert_loc ($symbolstartpos,$endpos) in
    let decls = Option.value ~default:[] decls in
    if !Lexerhack.is_typedef () && decls = [] then begin
      let source = fst loc in
      let wkey = Kernel.wkey_unnamed_typedef in
      Kernel.warning ~source ~wkey "typedef without a name"
    end;
    !Lexerhack.reset_typedef();
    doDeclaration spec loc (fst specif) decls
  }
| static_assert_declaration SEMICOLON
    { let (e, m, loc) = $1 in STATIC_ASSERT (e, m, loc) }
;

static_assert_declaration:
| STATIC_ASSERT LPAREN expression RPAREN                       { ($3, "", $1) }
| STATIC_ASSERT LPAREN expression COMMA string_constant RPAREN { ($3, fst $5, $1) }
;

generic_selection: /* ISO C11 6.5.1.1 */
| GENERIC LPAREN assignment_expression COMMA generic_association_list RPAREN
    { ($3, $5) }
| GENERIC LPAREN assignment_expression RPAREN {
    let loc = Errorloc.convert_loc $loc in
    Errorloc.parse_error ~loc
      "_Generic requires at least one generic association";
  }

generic_association_list:
| generic_association { [ $1 ] }
| generic_association COMMA generic_association_list { $1 :: $3 }

generic_association:
| type_name COLON assignment_expression { let s, d = $1 in (Some (s, d), $3) }
| DEFAULT COLON assignment_expression   { (None, $3) }
;

init_declarator_list:
| decl_and_init_decl_attr_list { register_symbol_class $1; $1 }
;

decl_and_init_decl_attr_list:
| init_declarator                                 { [$1] }
| init_declarator COMMA init_declarator_attr_list { $1 :: $3 }
;

init_declarator_attr_list:
| init_declarator_attr                                 { [ $1 ] }
| init_declarator_attr COMMA init_declarator_attr_list { $1 :: $3 }
;

init_declarator_attr:
| attribute_nocv_list init_declarator {
    let ((name, decl, attrs, loc), init) = $2 in
    ((name, PARENTYPE ($1,decl,[]), attrs, loc), init)
  }
;

init_declarator: /* ISO 6.7 */
| declarator                    { ($1, NO_INIT) }
| declarator EQ init_expression { ($1, $3) }
;

// C standard specifiers without type specifiers
decl_spec_wo_type_nor_attr: /* ISO 6.7 */
// ISO 6.7.1
| TYPEDEF  { !Lexerhack.set_typedef(); SpecTypedef, $1 }
| EXTERN   { SpecStorage EXTERN, $1 }
| STATIC   { SpecStorage STATIC, $1 }
| AUTO     { SpecStorage AUTO, $1 }
| REGISTER { SpecStorage REGISTER, $1}
// ISO 6.7.4
| INLINE   { SpecInline, $1 }
| NORETURN { SpecAttr (("noreturn",[])), $1 }
| ALIGNAS LPAREN conditional_expression RPAREN {
    SpecAlignas($3), $1
  }
| ALIGNAS LPAREN type_name RPAREN {
    let b, d = $3 in
    SpecAlignas(make_expr $sloc (TYPE_ALIGNOF(b, d, `Standard))), $1
  }

// ISO 6.7.3
| cvspec   { $1 }
;

// GCC allows attributes in decl specifiers
decl_spec_attribute:
| attribute_nocv { SpecAttr (fst $1), snd $1 }
;

// All possible decl specifiers
any_decl_spec:
| decl_spec_wo_type_nor_attr { fst $1 }
| decl_spec_attribute        { fst $1 }
| type_spec                  { SpecType(fst $1) }

/* The specifier list of decls cannot contain only GCC attribute specifiers. It
   must contain at least one type specifier or any other C standard specifier.
   We loop in decl_spec_list until we get one of those, then we leave the loop
   and allow any specifier or empty. */
decl_spec_list:
| decl_spec_wo_type_nor_attr decl_spec_list_opt { fst $1 :: $2, snd $1 }
| decl_spec_attribute decl_spec_list            { fst $1 :: fst $2, snd $1 }
| type_spec pragma* (* pragma accepted by GCC *) decl_spec_list_opt_no_named {
    let pragmas = $2 in
    if pragmas != [] then begin
      let loc = Cabshelper.get_definitionloc (List.hd pragmas) in
      Kernel.warning ~wkey:Kernel.wkey_parser_unsupported_pragma
        ~once:true ~source:(fst loc)
        "Discarding _Pragma's in function declaration"
    end;
    SpecType(fst $1) :: $3, snd $1
  }

/* Here we can match any specifier, meaning we already saw at least one type or
   C standard specifier. */
decl_spec_list_no_restriction:
| decl_spec_wo_type_nor_attr decl_spec_list_opt { fst $1 :: $2, snd $1 }
| decl_spec_attribute decl_spec_list_opt        { fst $1 :: $2, snd $1 }
| type_spec pragma* (* pragma accepted by GCC *) decl_spec_list_opt_no_named {
    let pragmas = $2 in
    if pragmas != [] then begin
      let loc = Cabshelper.get_definitionloc (List.hd pragmas) in
      Kernel.warning ~wkey:Kernel.wkey_parser_unsupported_pragma
        ~once:true ~source:(fst loc)
        "Discarding _Pragma's in function declaration"
    end;
    SpecType(fst $1) :: $3, snd $1
  }

/* (* In most cases if we see a NAMED_TYPE we must shift it. Thus we declare
    * NAMED_TYPE to have right associativity *) */
decl_spec_list_opt:
| /* empty */                   { [] } %prec NAMED_TYPE
| decl_spec_list_no_restriction { fst $1 }
;

/* (* We add this separate rule to handle the special case when an appearance
    * of NAMED_TYPE should not be considered as part of the specifiers but as
    * part of the declarator. IDENT has higher precedence than NAMED_TYPE *)
 */
decl_spec_list_opt_no_named:
| /* empty */                               { [] } %prec IDENT
| any_decl_spec decl_spec_list_opt_no_named { $1 :: $2 }
;

type_spec:   /* ISO 6.7.2 */
| VOID     { Tvoid, $1}
| CHAR     { Tchar, $1 }
| BOOL     { Tbool, $1 }
| SHORT    { Tshort, $1 }
| INT      { Tint, $1 }
| LONG     { Tlong, $1 }
| INT64    { Tint64, $1 }
| INT128   { Tint128, $1 }
| FLOAT    { Tfloat, $1 }
| DOUBLE   { Tdouble, $1 }
| FLOAT32  { Tfloat32, $1 }
| FLOAT64  { Tfloat64, $1 }
| SIGNED   { Tsigned, $1 }
| UNSIGNED { Tunsigned, $1 }
| STRUCT                 id_or_typename
                                                   { Tstruct ($2, None,    []), $1 }
| STRUCT just_attributes id_or_typename
                                                   { Tstruct ($3, None,    $2), $1 }
| STRUCT                 id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tstruct ($2, Some $4, []), $1 }
| STRUCT                                LBRACE struct_decl_list RBRACE
                                                   { Tstruct ("", Some $3, []), $1 }
| STRUCT just_attributes id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tstruct ($3, Some $5, $2), $1 }
| STRUCT just_attributes                LBRACE struct_decl_list RBRACE
                                                   { Tstruct ("", Some $4, $2), $1 }
| UNION                  id_or_typename
                                                   { Tunion  ($2, None,    []), $1 }
| UNION                  id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tunion  ($2, Some $4, []), $1 }
| UNION                                 LBRACE struct_decl_list RBRACE
                                                   { Tunion  ("", Some $3, []), $1 }
| UNION  just_attributes id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tunion  ($3, Some $5, $2), $1 }
| UNION  just_attributes                LBRACE struct_decl_list RBRACE
                                                   { Tunion  ("", Some $4, $2), $1 }
| ENUM                   id_or_typename
                                                   { Tenum   ($2, None,    []), $1 }
| ENUM                   id_or_typename LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ($2, Some $4, []), $1 }
| ENUM                                  LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ("", Some $3, []), $1 }
| ENUM   just_attributes id_or_typename LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ($3, Some $5, $2), $1 }
| ENUM   just_attributes                LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ("", Some $4, $2), $1 }
| NAMED_TYPE      {
      (Tnamed $1, Errorloc.convert_loc $sloc)
      }
| TYPEOF LPAREN expression RPAREN     { TtypeofE $3, $1 }
| TYPEOF LPAREN type_name RPAREN      { let s, d = $3 in
                                          TtypeofT (s, d), $1 }
;

/* (* ISO 6.7.2. Except that we allow empty structs. We
    * also allow missing field names. *) */
struct_decl_list:
| /* empty */ { [] }
| decl_spec_list SEMICOLON struct_decl_list
    {
      let loc = Errorloc.convert_loc $sloc in
      FIELD (fst $1, [(missingFieldDecl loc, None)]) :: $3
    }
/*(* GCC allows extra semicolons *)*/
| SEMICOLON struct_decl_list { $2 }
| decl_spec_list field_decl_list SEMICOLON struct_decl_list
    { FIELD (fst $1, $2) :: $4 }
/*(* MSVC allows pragmas in strange places *)*/
| pragma struct_decl_list { $2 }
/*(* C11 allows static_assert-declaration *)*/
| static_assert_declaration {
    let (e, m, loc) = $1 in
    [ STATIC_ASSERT_FG (e, m, loc) ]
  }
| static_assert_declaration SEMICOLON struct_decl_list {
    let (e, m, loc) = $1 in
    STATIC_ASSERT_FG (e, m, loc) :: $3
  }

;
field_decl_list: /* (* ISO 6.7.2 *) */
| field_decl                       { [$1] }
| field_decl COMMA field_decl_list { $1 :: $3 }
;

field_decl: /* (* ISO 6.7.2. Except that we allow unnamed fields. *) */
| declarator                      { ($1, None) }
| declarator COLON expression attributes {
    let (n,decl,al,loc) = $1 in
    let al' = al @ $4 in
    ((n,decl,al',loc), Some $3)
  }
| COLON expression {
      let loc = Errorloc.convert_loc $sloc in
      (missingFieldDecl loc, Some $2)
    }
;

enum_list: /* (* ISO 6.7.2.2 *) */
| enumerator {[$1]}
| enum_list COMMA enumerator {$1 @ [$3]}
;

enumerator:
| IDENT {
    let loc = Errorloc.convert_loc $sloc in
    ($1, { expr_node = NOTHING; expr_loc = loc }, loc)
  }
| IDENT just_attributes {
    let attrs = $2 in
    let loc = Errorloc.convert_loc $sloc in
    Kernel.warning ~wkey:Kernel.wkey_parser_unsupported_attributes
      ~source:(fst loc)
      "Discarding attributes in enumerator (unsupported feature): %a"
      Cprint.print_attributes attrs;
    ($1, { expr_node = NOTHING; expr_loc = loc }, loc)
  }
| IDENT EQ expression { ($1, $3, Errorloc.convert_loc $sloc) }
| IDENT just_attributes EQ expression {
    let attrs = $2 in
    let loc = Errorloc.convert_loc $sloc in
    Kernel.warning ~wkey:Kernel.wkey_parser_unsupported_attributes
      ~source:(fst loc)
      "Discarding attributes in enumerator (unsupported feature): %a"
      Cprint.print_attributes attrs;
    ($1, $4, loc)
  }
;


declarator: /* (* ISO 6.7.5. Plus Microsoft declarators.*) */
| pointer_opt direct_decl attributes_with_asm {
    let (n, decl, loc) = $2 in
    (n, applyPointer $1 decl, $3, loc)
  }
;


attributes_or_static: /* 6.7.5.2/3 */
| attributes comma_expression_opt { $1,$2 }
| attribute attributes STATIC comma_expression
    { fst $1::$2 @ ["static",[]], smooth_expression $4 }
| STATIC attributes comma_expression
    { ("static",[]) :: $2, smooth_expression $3 }
;

direct_decl: /* (* ISO 6.7.5 *) */
/* (* We want to be able to redefine named types as variable names *) */
| id_or_typename { ($1, JUSTBASE, Errorloc.convert_loc $sloc) }

| LPAREN attributes declarator RPAREN {
    let (n,decl,al,loc) = $3 in
    (n, PARENTYPE($2,decl,al), loc)
  }
| direct_decl LBRACKET attributes_or_static RBRACKET {
    let (n, decl, loc) = $1 in
    let (attrs, size) = $3 in
    (n, ARRAY(decl, attrs, size), loc)
  }
| direct_decl LPAREN RPAREN ghost_parameter_opt {
    let (n,decl,loc) = $1 in (n, PROTO(decl,[],$4,false), loc)
  }
| direct_decl parameter_list_startscope rest_par_list RPAREN ghost_parameter_opt {
    let (n, decl, loc) = $1 in
    let (params, isva) = $3 in
    let ghost = $5 in
    !Lexerhack.pop_context ();
    (n, PROTO(decl, params, ghost, isva), loc)
  }
;

parameter_list_startscope:
| LPAREN { !Lexerhack.push_context () }
;

rest_par_list:
| parameter_decl rest_par_list1 {
    let (params, isva) = $2 in
    ($1 :: params, isva)
  }
;

rest_par_list1:
| /* empty */                         { ([], false) }
| COMMA ELLIPSIS                      { ([], true) }
| COMMA parameter_decl rest_par_list1 {
    let (params, isva) = $3 in
    ($2 :: params, isva)
  }
;


parameter_decl: /* (* ISO 6.7.5 *) */
| decl_spec_list declarator    { (fst $1, $2) }
| decl_spec_list abstract_decl {
    let d, a = $2 in
    let loc = Errorloc.convert_loc $sloc in
    (fst $1, ("", d, a, loc))
  }
| decl_spec_list {
    let loc = Errorloc.convert_loc $sloc in
    (fst $1, ("", JUSTBASE, [], loc))
  }
| LPAREN parameter_decl RPAREN { $2 }
;

/* (* Old style prototypes. Like a declarator *) */
old_proto_decl:
| pointer_opt direct_old_proto_decl {
    let (n, decl, a, loc) = $2 in
    (n, applyPointer $1 decl, a, loc)
  }
;

direct_old_proto_decl:
| direct_decl LPAREN old_parameter_list_ne RPAREN old_pardef_list {
    let n, decl, floc = $1 in
    let par_decl, isva = doOldParDecl floc $3 $5 in
    (n, PROTO(decl, par_decl, [],isva), [Ast_attributes.fc_oldstyleproto,[]], floc)
  }
/* (* appears sometimesm but generates a shift-reduce conflict. *)
| LPAREN STAR direct_decl LPAREN old_parameter_list_ne RPAREN RPAREN
    LPAREN RPAREN old_pardef_list {
      let par_decl, isva = doOldParDecl $5 $10 in
      let n, decl = $3 in (n, PROTO(decl, par_decl,[], isva), [])
    }
*/
;

old_parameter_list_ne:
| IDENT                             { [$1] }
| IDENT COMMA old_parameter_list_ne { $1::$3 }
;

old_pardef_list:
| /* empty */                                   { ([], false) }
| decl_spec_list old_pardef SEMICOLON ELLIPSIS  { ([(fst $1, $2)], true) }
| decl_spec_list old_pardef SEMICOLON old_pardef_list {
    let rest, isva = $4 in
    ((fst $1, $2) :: rest, isva)
  }
;

old_pardef:
| declarator                  { [$1] }
| declarator COMMA old_pardef { $1 :: $3 }
;

pointer: /* (* ISO 6.7.5 *) */
| STAR attributes pointer_opt { $2 :: $3 }
;

pointer_opt:
| /* empty */ { [] }
| pointer     { $1 }
;

type_name: /* (* ISO 6.7.6 *) */
| specif=decl_spec_list name=abstract_decl {
    let d, a = name in
    if a <> [] then begin
      let loc = Errorloc.convert_loc ($loc(name)) in
      Errorloc.parse_error ~loc "attributes in type name"
    end;
    (fst specif, d)
  }
| decl_spec_list { (fst $1, JUSTBASE) }
;

abstract_decl: /* (* ISO 6.7.6. *) */
| pointer_opt abs_direct_decl attributes { applyPointer $1 $2, $3 }
| pointer                                { applyPointer $1 JUSTBASE, [] }
;

abs_direct_decl: /* (* ISO 6.7.6. We do not support optional declarator for
                     * functions. Plus Microsoft attributes. See the
                     * discussion for declarator. *) */
| LPAREN attributes abstract_decl RPAREN {
    let d, a = $3 in
    PARENTYPE ($2, d, a)
  }
| abs_direct_decl_opt LBRACKET comma_expression_opt RBRACKET
    { ARRAY($1, [], $3) }
/*(* The next should be abs_direct_decl_opt but we get conflicts *)*/
| abs_direct_decl parameter_list_startscope rest_par_list RPAREN {
    let (params, isva) = $3 in
    !Lexerhack.pop_context ();
    PROTO ($1, params,[], isva)
  }
| abs_direct_decl LPAREN RPAREN { PROTO ($1, [],[], false) }
;

abs_direct_decl_opt:
| abs_direct_decl { $1 }
| /* empty */     { JUSTBASE }
;

function_def: /* (* ISO 6.9.1 *) */
| s=ioption(SPEC) fn=function_def_start b=main_block {
    let (loc, specs, decl) = fn in
    let spec_loc =
      Option.bind
        (fun s ->
            let loc = fst s in
            Option.map
              (fun (loc', spec) -> spec, (loc, loc'))
              (Logic_lexer.spec s))
        s
    in
    currentFunctionName := "<__FUNCTION__ used outside any functions>";
    doFunctionDef spec_loc loc (trd3 b) specs decl (fst3 b)
  }
;

function_def_start: /* (* ISO 6.9.1 *) */
| decl_spec_list declarator {
    announceFunctionName $2;
    (fourth4 $2, fst $1, $2)
  }
/* (* Old-style function prototype *) */
| decl_spec_list old_proto_decl {
    announceFunctionName $2;
    (snd $1, fst $1, $2)
  }
/* (* New-style function that does not have a return type *) */
| IDENT parameter_list_startscope rest_par_list RPAREN ghost_parameter_opt{
    let (params, isva) = $3 in
    let ghost = $5 in
    let floc = Errorloc.convert_loc $loc($1) in
    let fdec = ($1, PROTO(JUSTBASE, params, ghost, isva), [], floc) in
    announceFunctionName fdec;
    (* Default is int type *)
    let defSpec = [SpecType Tint] in (floc, defSpec, fdec)
  }

/* (* No return type and old-style parameter list *) */
| IDENT LPAREN old_parameter_list_ne RPAREN old_pardef_list {
    (* Convert pardecl to new style *)
    let floc = Errorloc.convert_loc $loc($1) in
    let pardecl, isva = doOldParDecl floc $3 $5 in
    (* Make the function declarator *)
    let fdec = ($1, PROTO(JUSTBASE, pardecl,[],isva), [], floc) in
    announceFunctionName fdec;
    (* Default is int type *)
    (floc, [SpecType Tint], fdec)
  }
| IDENT LPAREN RPAREN ghost_parameter_opt {
    let floc = Errorloc.convert_loc $loc($1) in
    let fdec = ($1, PROTO(JUSTBASE,[],$4,false),[],floc) in
    announceFunctionName fdec;
    (floc, [SpecType Tint], fdec)
  }
;

/* const/volatile as type specifier elements */
cvspec:
| CONST           { SpecCV(CV_CONST), $1 }
| VOLATILE        { SpecCV(CV_VOLATILE), $1 }
| RESTRICT        { SpecCV(CV_RESTRICT), $1 }
| GHOST           { SpecCV(CV_GHOST), $1 }
;

/*** GCC attributes ***/
attributes:
| /* empty */          { [] }
| attribute attributes { fst $1 :: $2 }
;

/* (* In some contexts we can have an inline assembly to specify the name to
    * be used for a global. We treat this as a name attribute *) */
attributes_with_asm:
| /* empty */                   { [] }
| attribute attributes_with_asm { fst $1 :: $2 }
| ASM LPAREN string_constant RPAREN attributes {
    let loc = Errorloc.convert_loc $loc($3) in
    ("__asm__",
      [{ expr_node = CONSTANT(CONST_STRING (fst $3)); expr_loc = loc}])
    :: $5
  }
;

/* things like __attribute__, but no const/volatile */
attribute_nocv:
| ATTRIBUTE LPAREN paren_attr_list RPAREN { ("__attribute__", $3), $1 }
| NOP_ATTRIBUTE                           { ("__attribute__", []), $1 }
| DECLSPEC paren_attr_list_ne             { ("__declspec", $2), $1 }
| MSATTR                                  { (fst $1, []), snd $1 }
// ISO 6.7.3
| THREAD                                  { ("__thread", []), $1 }
| THREAD_LOCAL { ("__thread", [make_expr $sloc (VARIABLE "c11")]), $1 }
;

attribute_nocv_list:
| /* empty */                        { [] }
| attribute_nocv attribute_nocv_list { fst $1 :: $2 }
;

/* __attribute__ plus const/volatile */
attribute:
| attribute_nocv  { $1 }
| CONST           { ("const", []), $1 }
| RESTRICT        { ("restrict",[]), $1 }
| VOLATILE        { ("volatile",[]), $1 }
| GHOST           { ("ghost",[]), $1 }
;

/* (* sm: I need something that just includes __attribute__ and nothing more,
 * to support them appearing between the 'struct' keyword and the type name.
 * Actually, a declspec can appear there as well (on MSVC) *) */
just_attribute:
| ATTRIBUTE LPAREN paren_attr_list RPAREN { ("__attribute__", $3) }
| DECLSPEC paren_attr_list_ne             { ("__declspec", $2) }
;

/* this can't be empty, b/c I folded that possibility into the calling
 * productions to avoid some S/R conflicts */
just_attributes:
| just_attribute                 { [$1] }
| just_attribute just_attributes { $1 :: $2 }
;

/** (* PRAGMAS and ATTRIBUTES *) ***/
pragma:
| PRAGMA PRAGMA_EOL      { PRAGMA (make_expr $sloc (VARIABLE ("")), $1) }
| PRAGMA attr PRAGMA_EOL { PRAGMA ($2, $1) }
| PRAGMA attr SEMICOLON PRAGMA_EOL { PRAGMA ($2, $1) }
| PRAGMA_LINE { PRAGMA (make_expr $sloc (VARIABLE (fst $1)), snd $1) }
;

/* (* We want to allow certain strange things that occur in pragmas, so we
    * cannot use directly the language of expressions *) */
var_attr:
| IDENT                 { make_expr $sloc (VARIABLE $1) }
| NAMED_TYPE            { make_expr $sloc (VARIABLE $1) }
| DEFAULT COLON CST_INT { make_expr $sloc (VARIABLE ("default:" ^ fst $3)) }
/* Const when it appears in attribute lists, is translated to aconst */
| CONST                 { make_expr $sloc (VARIABLE "aconst") }
/*(** GCC allows this as an attribute for functions, synonym for noreturn **)*/
| VOLATILE              { make_expr $sloc (VARIABLE ("__noreturn__")) }
| CST_INT COLON CST_INT { make_expr $sloc (VARIABLE (fst $1 ^ ":" ^ fst $3)) }
;

basic_attr:
| CST_INT   { make_expr $sloc (CONSTANT(CONST_INT (fst $1))) }
| CST_FLOAT { make_expr $sloc (CONSTANT(CONST_FLOAT(fst $1))) }
| var_attr  { $1 }
;

basic_attr_list_ne:
| basic_attr                    { [$1] }
| basic_attr basic_attr_list_ne { $1::$2 }
;

parameter_attr_list_ne:
| basic_attr_list_ne { $1 }
| basic_attr_list_ne string_constant {
    $1 @ [make_expr $sloc (CONSTANT(CONST_STRING (fst $2)))]
  }
| basic_attr_list_ne string_constant parameter_attr_list_ne {
    $1 @ ([make_expr $sloc (CONSTANT(CONST_STRING (fst $2)))] @ $3)
  }
;

param_attr_list_ne:
| parameter_attr_list_ne { $1 }
| string_constant        { [make_expr $sloc (CONSTANT(CONST_STRING (fst $1)))] }
;

primary_attr:
| basic_attr         { $1 }
| LPAREN attr RPAREN { $2 }
| string_constant    { make_expr $sloc (CONSTANT(CONST_STRING (fst $1))) }
;

postfix_attr:
| primary_attr { $1 }
| id_or_typename_as_id paren_attr_list_ne {
    let loc = Errorloc.convert_loc $loc($1) in
    make_expr $sloc (CALL({ expr_loc = loc; expr_node = VARIABLE $1}, $2,[]))
  }
/* (* use a VARIABLE "" so that the parentheses are printed *) */
| id_or_typename_as_id LPAREN RPAREN {
    let loc1 = Errorloc.convert_loc $loc($1) in
    let loc2 =
      Errorloc.convert_pos $startpos($2),
      Errorloc.convert_pos $endpos($3)
    in
    let f = { expr_node = VARIABLE $1; expr_loc = loc1 } in
    let arg = { expr_node = VARIABLE ""; expr_loc = loc2 } in
    make_expr $sloc (CALL(f, [arg],[]))
  }
/* (* use a VARIABLE "" so that the parameters are printed without
    * parentheses nor comma *) */
| basic_attr param_attr_list_ne {
    let loc = Errorloc.convert_loc $loc($1) in
    make_expr $sloc (CALL({ expr_node = VARIABLE ""; expr_loc = loc}, $1::$2,[]))
  }
| postfix_attr ARROW id_or_typename   { make_expr $sloc (MEMBEROFPTR ($1, $3))}
| postfix_attr DOT id_or_typename     { make_expr $sloc (MEMBEROF ($1, $3)) }
| postfix_attr LBRACKET attr RBRACKET { make_expr $sloc (INDEX ($1, $3)) }
;

/*(* Since in attributes we use both IDENT and NAMED_TYPE as identifiers,
 * that leads to conflicts for SIZEOF and ALIGNOF. In those cases we require
 * that their arguments be expressions, not attributes *)*/
unary_attr:
| postfix_attr                   { $1 }
| SIZEOF unary_expression        { make_expr $sloc (EXPR_SIZEOF $2) }
| SIZEOF LPAREN type_name RPAREN {
    let b, d = $3 in
    make_expr $sloc (TYPE_SIZEOF (b, d))
  }
| ALIGNOF unary_expression        {
  make_expr $sloc (EXPR_ALIGNOF($2, `Standard))
  }
| ALIGNOF LPAREN type_name RPAREN {
    let b, d = $3 in
    make_expr $sloc (TYPE_ALIGNOF (b, d, `Standard))
  }
| GCC_ALIGNOF unary_expression        {
  make_expr $sloc (EXPR_ALIGNOF($2, `GCC))
  }
| GCC_ALIGNOF LPAREN type_name RPAREN {
    let b, d = $3 in
    make_expr $sloc (TYPE_ALIGNOF (b, d, `GCC))
  }

| PLUS cast_attr   { make_expr $sloc (UNARY (PLUS, $2)) }
| MINUS cast_attr  { make_expr $sloc (UNARY (MINUS, $2)) }
| STAR cast_attr   { make_expr $sloc (UNARY (MEMOF, $2)) }
| AND cast_attr    { make_expr $sloc (UNARY (ADDROF, $2)) }
| EXCLAM cast_attr { make_expr $sloc (UNARY (NOT, $2)) }
| TILDE cast_attr  { make_expr $sloc (UNARY (BNOT, $2)) }
;

cast_attr: unary_attr { $1 }

multiplicative_attr:
| cast_attr                              { $1 }
| multiplicative_attr STAR cast_attr    {make_expr $sloc (BINARY(MUL ,$1 , $3))}
| multiplicative_attr SLASH cast_attr   {make_expr $sloc (BINARY(DIV ,$1 , $3))}
| multiplicative_attr PERCENT cast_attr {make_expr $sloc (BINARY(MOD ,$1 , $3))}
;

additive_attr:
| multiplicative_attr                     { $1 }
| additive_attr PLUS multiplicative_attr  {make_expr $sloc (BINARY(ADD ,$1 , $3))}
| additive_attr MINUS multiplicative_attr {make_expr $sloc (BINARY(SUB ,$1 , $3))}
;

shift_attr:
| additive_attr                    { $1 }
| shift_attr INF_INF additive_attr {make_expr $sloc (BINARY(SHL ,$1 , $3))}
| shift_attr SUP_SUP additive_attr {make_expr $sloc (BINARY(SHR ,$1 , $3))}
;

relational_attr:
| shift_attr                        { $1 }
| relational_attr INF shift_attr    {make_expr $sloc (BINARY(LT ,$1 , $3))}
| relational_attr SUP shift_attr    {make_expr $sloc (BINARY(GT ,$1 , $3))}
| relational_attr INF_EQ shift_attr {make_expr $sloc (BINARY(LE ,$1 , $3))}
| relational_attr SUP_EQ shift_attr {make_expr $sloc (BINARY(GE ,$1 , $3))}
;

equality_attr:
| relational_attr                         { $1 }
| equality_attr EQ_EQ relational_attr     {make_expr $sloc (BINARY(EQ ,$1 , $3))}
| equality_attr EXCLAM_EQ relational_attr {make_expr $sloc (BINARY(NE ,$1 , $3))}
;


bitwise_and_attr:
| equality_attr                      { $1 }
| bitwise_and_attr AND equality_attr {make_expr $sloc (BINARY(BAND ,$1 , $3))}
;

bitwise_xor_attr:
| bitwise_and_attr                       { $1 }
| bitwise_xor_attr CIRC bitwise_and_attr {make_expr $sloc (BINARY(XOR ,$1 , $3))}
;

bitwise_or_attr:
| bitwise_xor_attr                      { $1 }
| bitwise_or_attr PIPE bitwise_xor_attr {make_expr $sloc (BINARY(BOR ,$1 , $3))}
;

logical_and_attr:
| bitwise_or_attr                          { $1 }
| logical_and_attr AND_AND bitwise_or_attr {make_expr $sloc (BINARY(AND ,$1 , $3))}
;

logical_or_attr:
| logical_and_attr                           { $1 }
| logical_or_attr PIPE_PIPE logical_and_attr {make_expr $sloc (BINARY(OR ,$1 , $3))}
;

conditional_attr:
| logical_or_attr                        { $1 }
| logical_or_attr QUEST attr_test conditional_attr COLON2 conditional_attr
    { make_expr $sloc (QUESTION($1, $4, $6)) }

assign_attr:
| conditional_attr                     { $1 }
| conditional_attr EQ conditional_attr { make_expr $sloc (BINARY(ASSIGN,$1,$3)) }

/* hack to avoid shift reduce conflict in attribute parsing. */
attr_test:
| /* empty */ { Cabshelper.push_attr_test () }

attr: assign_attr { $1 }

attr_list_ne:
| attr                    { [$1] }
| attr COMMA attr_list_ne { $1 :: $3 }
;

attr_list:
| /* empty */  { [] }
| attr_list_ne { $1 }
;

paren_attr_list_ne:
| LPAREN attr_list_ne RPAREN { $2 }
;

paren_attr_list:
| LPAREN attr_list RPAREN { $2 }
;

/*** GCC ASM instructions ***/
asmattr:
| /* empty */      { [] }
| VOLATILE asmattr { ("volatile", []) :: $2 }
| CONST asmattr    { ("const", []) :: $2 }
;

asmtemplate:
| line=one_string {
    let loc = Errorloc.convert_loc $loc in
    [intlist_to_string ~loc [fst line]]
  }
| line=one_string rest=asmtemplate {
    let loc = Errorloc.convert_loc $loc(line) in
    intlist_to_string ~loc [fst line] :: rest
  }
;

asmoutputs:
| /* empty */                 { None }
| COLON asmoperands asminputs {
    let (ins, (clobs,labels)) = $3 in
    Some {aoutputs = $2; ainputs = ins; aclobbers = clobs; alabels = labels }
  }
;

asmoperands:
| /* empty */   { [] }
| asmoperandsne { List.rev $1 }
;

asmoperandsne:
| asmoperand                     { [$1] }
| asmoperandsne COMMA asmoperand { $3 :: $1 }
;

asmoperand:
| asmopname string_constant LPAREN expression RPAREN { ($1, fst $2, $4) }
;

asminputs:
| /* empty */                  { ([], ([],[])) }
| COLON asmoperands asmclobber { ($2, $3) }
;

asmopname:
| /* empty */             { None }
| LBRACKET IDENT RBRACKET { Some $2 }
;

asmclobber:
| /* empty */                     { [],[] }
| COLON asmlabels                 { [],$2 }
| COLON asmcloberlst_ne asmlabels { $2,$3 }
;
asmcloberlst_ne:
| string_constant                       { [fst $1] }
| string_constant COMMA asmcloberlst_ne { fst $1 :: $3 }
;

asmlabels:
| /* empty */             { [] }
| COLON local_label_names { $2 }
%%
