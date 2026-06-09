/******************************************************************************/
/*                                                                            */
/*  SPDX-License-Identifier LGPL-2.1                                          */
/*  Copyright (C)                                                             */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      */
/*  INRIA (Institut National de Recherche en Informatique et en Automatique)  */
/*                                                                            */
/******************************************************************************/

/* Grammar for C annotations */

%{

  open Cil_types
  open Logic_ptree
  open Logic_utils

  let loc (start_pos, end_pos) =
    Errorloc.convert_loc (start_pos, end_pos)

  let pos pos =
    Errorloc.convert_pos pos

  let loc_info lexpr_loc x = { lexpr_node = x; lexpr_loc }
  let loc_start x = fst x.lexpr_loc
  let loc_end x = snd x.lexpr_loc

  let info start_end x = loc_info (loc start_end) x

  (* Normalize p1 && (p2 && p3) into (p1 && p2) && p3 *)
  let rec pland p1 p2 =
    match p2.lexpr_node with
      | PLand (p3,p4) ->
        let loc = (loc_start p1, loc_end p3) in
        PLand(loc_info loc (pland p1 p3),p4)
      | _ -> PLand(p1,p2)

  let rec plor p1 p2 =
    match p2.lexpr_node with
      | PLor(p3,p4) ->
        let loc = (loc_start p1, loc_end p3) in
        PLor(loc_info loc (plor p1 p3),p4)
      | _ -> PLor(p1,p2)

  let clause_order start_end name1 name2 =
    raise
      (Not_well_formed
         (loc start_end,
          "wrong order of clause in contract: "
          ^ name1 ^ " after " ^ name2 ^ "."))

  let missing start_end token next_token =
    raise
      (Not_well_formed
         (loc start_end,
          Format.asprintf "expecting '%s' before %s" token next_token))

  type sense_of_relation = Unknown | Disequal | Less | Greater

  let check_empty (loc,msg) l =
    match l with
        [] -> ()
      | _ -> raise (Not_well_formed (loc,msg))

  let relation_sense rel sense =
    match rel, sense with
      | Eq, (Unknown|Greater|Less) -> sense, true
      | Neq, Unknown -> Disequal, false (* No chain of disequality for now*)
      | (Gt|Ge), (Unknown|Greater) -> Greater, true
      | (Lt|Le), (Unknown|Less) -> Less, true
      | _ -> sense, false

  let module_types : string list ref Stack.t = Stack.create ()

  let push_typename t =
    Logic_env.add_typename t ;
    try
      let r = Stack.top module_types in r := t :: !r
    with Stack.Empty -> ()

  let push_module_types () =
    Stack.push (ref []) module_types

  let pop_module_types () =
    let r = Stack.pop module_types in
    List.iter Logic_env.remove_typename !r

  let type_variables_stack = Stack.create ()

  let enter_type_variables_scope l =
    List.iter Logic_env.add_typename l;
    Stack.push l type_variables_stack

  let exit_type_variables_scope () =
    let l = Stack.pop type_variables_stack in
    List.iter Logic_env.remove_typename l

  let loc_decl start_end d = { decl_node = d; decl_loc = loc start_end }

  let loc_ext start_end d = { extended_node = d; extended_loc = loc start_end }

  let filter_from l = function
    | FromAny ->
      l, FromAny
    | From ds ->
      let f ds d = if List.exists (is_same_lexpr d) ds then ds else d :: ds in
      l, From(List.(rev (fold_left f [] ds)))

  let concat_froms cura newa =
    let compare_pair (curb,_) (newb,_) = is_same_lexpr curb newb in
    (* NB: the following has an horrible complexity, but the order of
       clauses in the input is preserved. *)
    let concat_one acc (newloc, newf)  =
      let (newloc, newf) as p = filter_from newloc newf in
      try
        let (curloc,curf) = List.find (compare_pair p) acc
        in
        match (curf, newf) with
          | _,FromAny ->
            (* the new fundeps does not give more information than the one
               which is already present. Just ignore it.
             *)
           acc
          | FromAny, _ ->
              (* the new fundeps is strictly more precise than the old one.
                 We replace the old dependency by the new one, but keep
                 the location at its old place in the list. This ensures
                 that we get the exact same clause if we try to
                 link the original contract with its pretty-printed version. *)
              List.replace compare_pair p acc
          | From curl, From newl ->
            let incl l lin =
              List.(for_all (fun e -> exists (is_same_lexpr e) lin) l)
            in
            let drop d k =
              Kernel.warning ~current:false ~wkey:Kernel.wkey_multi_from
                "Drop '%a' \\from at %a for more precise one at %a"
                Logic_print.print_lexpr curloc
                Fileloc.pretty d.lexpr_loc
                Fileloc.pretty k.lexpr_loc
            in
            if incl curl newl then begin
              if not (incl newl curl) then drop newloc curloc;
              acc
            end
            else if incl newl curl then begin
              drop curloc newloc;
              List.replace compare_pair p acc
            end
            else acc @ [p]
      with Not_found -> acc @ [p]
    in List.fold_left concat_one cura newa

  let concat_allocation fa1 fa2 =
    match fa1,fa2 with
      | FreeAllocAny,_ -> fa2
      | _,FreeAllocAny -> fa1
      | FreeAlloc(f1,a1),FreeAlloc(f2,a2) -> FreeAlloc(f2@f1,a2@a1)

  (* a1 represents the assigns _after_ the current clause a2. *)
  let concat_assigns start_pos a1 a2 =
    match a1,a2 with
        WritesAny,a -> Writes (concat_froms [] a)
      | Writes [], [] -> a1
      | Writes [], _  | Writes _, [] ->
        raise (
          Not_well_formed (loc start_pos,"Mixing \\nothing and a real location"))
      | Writes a1, a2 -> Writes (concat_froms (concat_froms [] a2) a1)

  let concat_loop_assigns_allocation start_pos annots bhvs2 a2 fa2=
    (* NB: this is supposed to merge assigns related to named behaviors, in
       case of annotation like
       for a,b: assigns x,y;
       for b,c: assigns z,t;
       DO NOT CALL this function for loop assigns not attached to specific
       behaviors.
     *)
    assert (bhvs2 <> []);
    if fa2 == FreeAllocAny && a2 == WritesAny
    then annots
    else
    let split l1 l2 =
      let treat_one (only1,both,only2) x =
        if List.mem x l1 then
          (List.filter (fun y -> x <> y) only1,x::both,only2)
        else (only1,both,x::only2)
      in List.fold_left treat_one (l1,[],[]) l2
    in
    let treat_one ca (bhvs2,acc) =
      match ca,a2,fa2 with
          (AAssigns(bhvs1,a1)),(Writes a2),_ ->
            let (only1,both,only2) = split bhvs1 bhvs2 in
            (match both with
              | [] -> bhvs2, ca::acc
              | _ ->
                let common_annot = AAssigns(both,concat_assigns start_pos a1 a2) in
                let annots =
                  match only1 with
                    | [] -> common_annot :: acc
                    | _ -> AAssigns(only1,a1) :: common_annot :: acc
                in only2,annots)
        | (AAllocation(bhvs1,fa1)),_,(FreeAlloc _) ->
           let (only1,both,only2) = split bhvs1 bhvs2 in
            (match both with
              | [] -> bhvs2, ca::acc
              | _ ->
                let common_annot =
                  AAllocation(both,concat_allocation fa1 fa2)
                in
                let annots =
                  match only1 with
                    | [] -> common_annot :: acc
                    | _ -> AAllocation(only1,fa1) :: common_annot :: acc
                in only2,annots)
         | _,_,_ -> bhvs2,ca::acc
    in
    let (bhvs2, annots) = List.fold_right treat_one annots (bhvs2,[]) in
    match bhvs2 with
      | [] -> annots (* Already considered all cases. *)
      | _ ->
	  let annots = if a2 <> WritesAny
	    then AAssigns (bhvs2,a2) :: annots
            else annots
	  in
	  if fa2 <> FreeAllocAny
	    then AAllocation (bhvs2,fa2) :: annots
            else annots

  let obsolete name ~source ~now =
    Kernel.warning ~source
      "parsing obsolete ACSL construct '%s'. '%s' should be used instead."
      name now

  let escape =
    let regex1 = Str.regexp "\\(\\(\\\\\\\\\\)*[^\\]\\)\\(['\"]\\)" in
    let regex2 = Str.regexp "\\(\\\\\\\\\\)*\\\\$" in
    fun str ->
      let str = Str.global_replace regex1 "\\1\\\\3" str in
      Str.global_replace regex2 "\\1\\\\" str

  let cv_const = ("const", [])
  let cv_volatile = ("volatile", [])
  let cv_ghost = ("ghost", [])

  let toplevel_pred tp_kind tp_statement = { tp_kind; tp_statement }

  let extension ext_name ext_plugin ext_content =
    {ext_name; ext_plugin; ext_content}

  let global_extension gext_name gext_plugin gext_kind gext_content =
    Ext_extension {gext_name; gext_plugin; gext_kind; gext_content}

  let import loader module_name module_alias =
    let import_loader =
      match loader with
      | None -> None
      | Some (loader_name, loader_plugin) ->
          Some {loader_name;loader_plugin}
    in
    LDimport { import_loader; module_name; module_alias}


%}

/*****************************************************************************/
/* IMPORTANT NOTE: When you add a new token, be sure that it will be         */
/* recognized by the any: rule at the end of this file.                      */
/* Otherwise, the token will not be usable inside a contract.                */
/*****************************************************************************/

%token EXT_SPEC_MODULE EXT_SPEC_FUNCTION EXT_SPEC_CONTRACT EXT_SPEC_INCLUDE
%token EXT_SPEC_AT EXT_SPEC_LET
%token <string> LONGIDENT IDENTIFIER TYPENAME IDENTIFIER_EXT IDENTIFIER_LOADER
%token <bool*string> STRING_LITERAL
%token <string> INT_CONSTANT
%token <string> FLOAT_CONSTANT
%token <string> STRING_CONSTANT
%token <string> WSTRING_CONSTANT
%token LPAR RPAR IF ELSE COLON COLON2 COLONCOLON DOT DOTDOT DOTDOTDOT
%token INT INTEGER REAL BOOLEAN BOOL FLOAT FLOAT32 FLOAT64 LT GT LE GE EQ NE COMMA ARROW EQUAL
%token FORALL EXISTS IFF IMPLIES AND OR NOT SEPARATED
%token TRUE FALSE OLD AS AT RESULT
%token BLOCK_LENGTH BASE_ADDR OFFSET VALID VALID_READ VALID_INDEX VALID_RANGE
%token OBJECT_POINTER VALID_FUNCTION
%token ALLOCATION STATIC REGISTER AUTOMATIC DYNAMIC UNALLOCATED
%token ALLOCABLE FREEABLE FRESH
%token DOLLAR QUESTION MINUS PLUS STAR AMP SLASH PERCENT LSQUARE RSQUARE EOF
%token GLOBAL INVARIANT VARIANT DECREASES FOR LABEL ASSERT CHECK ADMIT SEMICOLON NULL EMPTY
%token REQUIRES ENSURES ALLOCATES FREES ASSIGNS LOOP NOTHING FROM
%token CHECK_REQUIRES CHECK_LOOP CHECK_INVARIANT CHECK_LEMMA
%token CHECK_ENSURES CHECK_EXITS CHECK_CONTINUES CHECK_BREAKS CHECK_RETURNS
%token ADMIT_REQUIRES ADMIT_LOOP ADMIT_INVARIANT ADMIT_LEMMA
%token ADMIT_ENSURES ADMIT_EXITS ADMIT_CONTINUES ADMIT_BREAKS ADMIT_RETURNS
%token <string * string> EXT_CODE_ANNOT EXT_GLOBAL EXT_GLOBAL_BLOCK EXT_CONTRACT EXT_LOADER EXT_LOADER_PLUGIN
%token EXITS BREAKS CONTINUES RETURNS
%token VOLATILE READS WRITES
%token LOGIC PREDICATE INDUCTIVE AXIOM LEMMA LBRACE RBRACE
%token AXIOMATIC MODULE IMPORT
%token MODEL CASE
%token VOID CHAR SIGNED UNSIGNED SHORT LONG DOUBLE STRUCT ENUM UNION
%token BSUNION INTER
%token TYPE BEHAVIOR BEHAVIORS ASSUMES COMPLETE DISJOINT
%token TERMINATES
%token BIFF BIMPLIES STARHAT HAT HATHAT PIPE TILDE GTGT LTLT
%token ALIGNOF SIZEOF LAMBDA LET
%token TYPEOF BSTYPE
%token WITH CONST GHOST
%token INITIALIZED DANGLING ALIGNED
%token LSQUAREPIPE RSQUAREPIPE
%token IN
%token PI

%right prec_named
%nonassoc prec_forall prec_exists prec_lambda LET
%right QUESTION prec_question
%left IFF
%right IMPLIES
%left OR
%left HATHAT
%left AND
%left BIFF
%right BIMPLIES
%left PIPE
%left HAT
%left STARHAT
%left AMP
%nonassoc IN
%left LTLT GTGT
%left PLUS MINUS
%left STAR SLASH PERCENT
%right prec_cast TILDE NOT prec_unary_op
%left DOT ARROW LSQUARE

%type <Logic_ptree.lexpr> lexpr_eof
%start lexpr_eof

%type <Logic_ptree.lexpr list> lexpr_list_eof
%start lexpr_list_eof

%type <Logic_ptree.annot> annot
%start annot

%type <Logic_ptree.spec> spec
%start spec

%type <Logic_ptree.ext_spec> ext_spec
%start ext_spec

%%

/*** predicates and terms ***/

lexpr_list_eof:
| lexpr_list EOF { $1 }

lexpr_list:
| /* epsilon */ { [] }
| ne_lexpr_list  { $1 }
;

ne_lexpr_list:
| lexpr                    { [$1] }
| lexpr COMMA ne_lexpr_list { $1 :: $3 }
;

lexpr_eof:
| lexpr EOF { $1 }
;

lexpr_option:
| /* epsilon */ { None }
| lexpr         { Some $1 }
;

lexpr:
  /* predicates */
| lexpr IMPLIES lexpr { info $sloc (PLimplies ($1, $3)) }
| lexpr IFF lexpr { info $sloc (PLiff ($1, $3)) }
| lexpr OR lexpr     { info $sloc (plor $1 $3) }
| lexpr AND lexpr    { info $sloc (pland $1 $3) }
| lexpr HATHAT lexpr    { info $sloc (PLxor ($1, $3)) }
/* terms */
| lexpr AMP lexpr { info $sloc (PLbinop ($1, Bbw_and, $3)) }
| lexpr PIPE lexpr { info $sloc (PLbinop ($1, Bbw_or, $3)) }
| lexpr HAT lexpr { info $sloc (PLbinop ($1, Bbw_xor, $3)) }
| lexpr BIMPLIES lexpr { info $sloc (PLbinop (info $sloc (PLunop (Ubw_not, $1)), Bbw_or, $3)) }
| lexpr BIFF lexpr { info $sloc (PLbinop (info $sloc (PLunop (Ubw_not, $1)), Bbw_xor, $3)) }
| lexpr IN lexpr { info $sloc (PLapp ("\\subset", [], [info $sloc (PLset [$1]);$3])) }
| lexpr QUESTION lexpr COLON2 lexpr %prec prec_question
    { info $sloc (PLif ($1, $3, $5)) }
/* both terms and predicates */
| any_identifier COLON lexpr %prec prec_named { info $sloc (PLnamed ($1, $3)) }
| string COLON lexpr %prec prec_named
      { let (iswide,str) = $1 in
        if iswide then begin
           let l = loc $sloc in
           raise (Not_well_formed(l, "Wide strings are not allowed as labels."))
         end;
        let str = escape str in
         info $sloc (PLnamed (str, $3))
       }
| lexpr_rel { $1 }
;

lexpr_rel:
| lexpr_end_rel  { $1 }
| lexpr_inner rel_list
      { let rel, rhs, _, oth_rel = $2 in
        let loc = loc_start $1, loc_end rhs in
        let relation = loc_info loc (PLrel($1,rel,rhs)) in
        match oth_rel with
            None -> relation
          | Some oth_relation -> info $sloc (pland relation oth_relation)
      }
;

lexpr_binder:
| LET bounded_var EQUAL lexpr SEMICOLON lexpr %prec LET
      { info $sloc (PLlet($2,$4,$6))}
| FORALL binders SEMICOLON lexpr  %prec prec_forall
      { info $sloc (PLforall ($2, $4)) }
| EXISTS binders SEMICOLON lexpr  %prec prec_exists
      { info $sloc (PLexists ($2, $4)) }
| LAMBDA binders SEMICOLON lexpr  %prec prec_lambda
      { info $sloc (PLlambda ($2,$4)) }
;

lexpr_end_rel:
  lexpr_inner  { $1 }
| lexpr_binder { $1 }
| NOT lexpr_binder { info $sloc (PLnot $2) }
;

rel_list:
| relation lexpr_end_rel
  { $1, $2, fst(relation_sense $1 Unknown), None }
| relation lexpr_inner rel_list
  {
    let next_rel, rhs, sense, oth_rel = $3 in
    let (sense, correct) = relation_sense $1 sense
    in
    if correct then
      let loc = loc_start $2, loc_end rhs in
      let my_rel = loc_info loc (PLrel($2,next_rel,rhs)) in
      let oth_rel = match oth_rel with
          None -> my_rel
        | Some rel ->
	    let loc = loc_start $2, loc_end rel in
	    loc_info loc (pland my_rel rel)
      in
      $1,$2,sense,Some oth_rel
    else begin
      let loc = loc $sloc in
      raise (Not_well_formed(loc,"Inconsistent relation chain."));
    end
  }
;

relation:
| LT    { Lt }
| GT    { Gt }
| LE    { Le }
| GE    { Ge }
| EQ    { Eq }
| NE    { Neq }
    /* C. Marche: added to produce better error messages */
| EQUAL {
      let l = loc $sloc in
      raise
        (Not_well_formed(l,
                         "Assignment operators not allowed in annotations."))
    }
;

lexpr_inner:
| string {
      let (is_wide,content) = $1 in
      let cst = if is_wide then
        WStringConstant content
      else
        StringConstant content
      in
      info $sloc (PLconstant cst)
    }
| NOT lexpr_inner { info $sloc (PLnot $2) }
| TRUE { info $sloc PLtrue }
| FALSE { info $sloc PLfalse }
| OBJECT_POINTER opt_label_1 LPAR lexpr RPAR { info $sloc (PLobject_pointer ($2,$4)) }
| VALID opt_label_1 LPAR lexpr RPAR { info $sloc (PLvalid ($2,$4)) }
| VALID_READ opt_label_1 LPAR lexpr RPAR { info $sloc (PLvalid_read ($2,$4)) }
| VALID_FUNCTION LPAR lexpr RPAR { info $sloc (PLvalid_function $3) }
| VALID_INDEX opt_label_1 LPAR lexpr COMMA lexpr RPAR {
  let source = pos $symbolstartpos in
  obsolete ~source "\\valid_index(addr,idx)" ~now:"\\valid(addr+idx)";
  info $sloc (PLvalid ($2,info $sloc (PLbinop ($4, Badd, $6)))) }
| VALID_RANGE opt_label_1 LPAR lexpr COMMA lexpr COMMA lexpr RPAR {
  let source = pos $symbolstartpos in
  obsolete "\\valid_range(addr,min,max)"
    ~source ~now:"\\valid(addr+(min..max))";
  info $sloc (PLvalid
          ($2,info $sloc (PLbinop ($4, Badd, (info $sloc (PLrange((Some $6),Some $8)))))))
}
| INITIALIZED opt_label_1 LPAR lexpr RPAR { info $sloc (PLinitialized ($2,$4)) }
| DANGLING opt_label_1 LPAR lexpr RPAR { info $sloc (PLdangling ($2,$4)) }
| ALIGNED LPAR lexpr COMMA lexpr RPAR { info $sloc (PLaligned ($3,$5)) }
| FRESH opt_label_2 LPAR lexpr COMMA lexpr RPAR { info $sloc (PLfresh ($2,$4, $6)) }
| BASE_ADDR opt_label_1 LPAR lexpr RPAR { info $sloc (PLbase_addr ($2,$4)) }
| BLOCK_LENGTH opt_label_1 LPAR lexpr RPAR { info $sloc (PLblock_length ($2,$4)) }
| OFFSET opt_label_1 LPAR lexpr RPAR { info $sloc (PLoffset ($2,$4)) }
| ALLOCABLE opt_label_1 LPAR lexpr RPAR { info $sloc (PLallocable ($2,$4)) }
| FREEABLE opt_label_1 LPAR lexpr RPAR { info $sloc (PLfreeable ($2,$4)) }
| ALLOCATION opt_label_1 LPAR lexpr RPAR
  { let source = pos $symbolstartpos in
    Kernel.not_yet_implemented ~source "\\allocation" }
| AUTOMATIC {
  let source = pos $symbolstartpos in
  Kernel.not_yet_implemented ~source "\\automatic" }
| DYNAMIC {
  let source = pos $symbolstartpos in
  Kernel.not_yet_implemented ~source "\\dynamic" }
| REGISTER {
  let source = pos $symbolstartpos in
  Kernel.not_yet_implemented ~source "\\register" }
| STATIC {
  let source = pos $symbolstartpos in
  Kernel.not_yet_implemented ~source "\\static" }
| UNALLOCATED {
  let source = pos $symbolstartpos in
  Kernel.not_yet_implemented ~source "\\unallocated" }
| NULL { info $sloc PLnull }
| constant { info $sloc (PLconstant $1) }
| lexpr_inner PLUS lexpr_inner { info $sloc (PLbinop ($1, Badd, $3)) }
| lexpr_inner MINUS lexpr_inner { info $sloc (PLbinop ($1, Bsub, $3)) }
| lexpr_inner STAR lexpr_inner { info $sloc (PLbinop ($1, Bmul, $3)) }
| lexpr_inner SLASH lexpr_inner { info $sloc (PLbinop ($1, Bdiv, $3)) }
| lexpr_inner PERCENT lexpr_inner { info $sloc (PLbinop ($1, Bmod, $3)) }
| lexpr_inner STARHAT lexpr_inner  { info $sloc (PLrepeat ($1, $3)) }
| lexpr_inner ARROW identifier_or_typename_full { info $sloc (PLarrow ($1, $3)) }
| lexpr_inner DOT identifier_or_typename_full { info $sloc (PLdot ($1, $3)) }
| lexpr_inner LSQUARE range RSQUARE { info $sloc (PLarrget ($1, $3)) }
| lexpr_inner LSQUARE lexpr RSQUARE { info $sloc (PLarrget ($1, $3)) }
| LSQUAREPIPE lexpr_list RSQUAREPIPE { info $sloc (PLlist $2) }
| MINUS lexpr_inner %prec prec_unary_op { info $sloc (PLunop (Uminus, $2)) }
| PLUS  lexpr_inner %prec prec_unary_op { $2 }
| TILDE lexpr_inner { info $sloc (PLunop (Ubw_not, $2)) }
| STAR  lexpr_inner %prec prec_unary_op { info $sloc (PLunop (Ustar, $2)) }
| AMP   lexpr_inner %prec prec_unary_op { info $sloc (PLunop (Uamp, $2)) }
| SIZEOF LPAR lexpr RPAR { info $sloc (PLsizeofE $3) }
| SIZEOF LPAR cast_logic_type RPAR { info $sloc (PLsizeof $3) }
| ALIGNOF LPAR cast_logic_type RPAR { info $sloc (PLalignof $3) }
| OLD LPAR lexpr RPAR { info $sloc (PLold $3) }
| AT LPAR lexpr COMMA label_name RPAR { info $sloc (PLat ($3, $5)) }
| RESULT { info $sloc PLresult }
| SEPARATED LPAR ne_lexpr_list RPAR
      { info $sloc (PLseparated $3) }
| symbol_identifier LPAR ne_lexpr_list RPAR
      { info $sloc (PLapp ($1, [], $3)) }
| symbol_identifier LBRACE ne_label_args RBRACE LPAR ne_lexpr_list RPAR
      { info $sloc (PLapp ($1, $3, $6)) }
| symbol_identifier LBRACE ne_label_args RBRACE
      { info $sloc (PLapp ($1, $3, [])) }
| symbol_identifier  { info $sloc (PLvar $1) }
| PI  { info $sloc (PLvar "\\pi") }
| lexpr_inner GTGT lexpr_inner { info $sloc (PLbinop ($1, Brshift, $3))}
| lexpr_inner LTLT lexpr_inner { info $sloc (PLbinop ($1, Blshift, $3))}
| LPAR lexpr RPAR { info $sloc $2.lexpr_node }
| LPAR range RPAR { info $sloc $2.lexpr_node }
| LPAR cast_logic_type RPAR lexpr_inner %prec prec_cast
      { info $sloc (PLcast ($2, $4)) }
| TYPEOF LPAR lexpr RPAR { info $sloc (PLtypeof $3) }
| BSTYPE LPAR type_spec(typename) RPAR { info $sloc (PLtype $3) }
| BSTYPE LPAR type_spec(typename) stars RPAR { info $sloc (PLtype ($4 $3)) }
    /* tsets */
| EMPTY { info $sloc PLempty }
| BSUNION LPAR lexpr_list RPAR { info $sloc (PLunion $3) }
| INTER LPAR lexpr_list RPAR { info $sloc (PLinter $3) }
| LBRACE RBRACE
      { info $sloc (PLset []) }
/* because LONGIDENT can be both a type name or a plain identifier,
   we can't have a full lexpr here, as there would be an ambiguity
   in { x | a::b * ...: should a::b be considered as a type (hence
   we are parsing a comprehension with a binder), or an identifier
   (hence, we are still parsing an lexpr).
*/
| LBRACE lexpr_inner RBRACE
      { info $sloc (PLset [$2]) }
| LBRACE lexpr_inner COMMA lexpr_list RBRACE
      { info $sloc (PLset ($2 :: $4)) }
| LBRACE lexpr_inner PIPE binders RBRACE
      { info $sloc (PLcomprehension ($2,$4,None)) }
| LBRACE lexpr_inner PIPE binders SEMICOLON lexpr RBRACE
      { info $sloc (PLcomprehension ($2,$4,Some $6)) }
    /* Aggregated object initialization */
| LBRACE field_init RBRACE
      { info $sloc (PLinitField($2)) }
| LBRACE array_init RBRACE
      { info $sloc (PLinitIndex($2)) }
| LBRACE lexpr_inner WITH update RBRACE
      { List.fold_left
	  (fun a (path,upd_val) -> info $sloc (PLupdate(a,path,upd_val))) $2 $4 }
/*
| LET bounded_var EQUAL lexpr SEMICOLON lexpr %prec LET { info $sloc (PLlet($2,$4,$6))}*/
;

ne_label_args:
| identifier_or_typename_full { [ $1 ] }
| identifier_or_typename_full COMMA ne_label_args { $1 :: $3 }

string:
| STRING_LITERAL { $1 }
| string STRING_LITERAL {
      let (is_wide,prefix) = $1 in
      let (is_wide2,suffix) = $2 in
      (is_wide || is_wide2, prefix ^ suffix)
    }
;

range:
| lexpr_option DOTDOT lexpr_option { info $sloc (PLrange($1,$3)) }
;

/*** Aggregated object initialization ***/

field_path_elt:
| DOT identifier_or_typename_full { $2 }
;
field_init_elt:
| field_path_elt EQUAL lexpr { ($1, $3) }
;

field_init:
| field_init_elt                  { [$1] }
| field_init_elt COMMA field_init { $1::$3 }
;

array_path_elt:
| LSQUARE lexpr RSQUARE      { $2 }
| LSQUARE range RSQUARE      { $2 }
;

array_init_elt:
| array_path_elt EQUAL lexpr { ($1, $3) }


array_init:
| array_init_elt                  { [$1] }
| array_init_elt COMMA array_init { $1::$3 }
;

/*** Functional update ***/
update:
| update_elt                  { [$1] }
| update_elt COMMA update { $1::$3 }
;

update_elt:
| path EQUAL lexpr                { $1, PLupdateTerm $3 }
| path EQUAL LBRACE WITH update RBRACE { $1, PLupdateCont $5 }
;

path:
| path_elt      { [$1] }
| path_elt path { $1::$2 }
;

path_elt:
| field_path_elt { PLpathField $1 }
| array_path_elt { PLpathIndex $1 }
;

/*** binders ***/

binders:
| binders_reentrance { let (_lt, vars) = $1 in vars }
;

binders_reentrance:
| decl_spec { let (lt, var) = $1 in (lt, [var]) }
| binders_reentrance COMMA decl_spec
    { let _, vars = $1 in
      let (lt, var) = $3 in
        (lt, vars @ [ var ])
    }
| binders_reentrance COMMA var_spec
    { let last_type_spec, vars = $1 in
        (last_type_spec, vars @ [ let (modif, name) = $3 in (modif last_type_spec, name)])
    }
;

decl_spec:
| type_spec(typesymbol) var_spec { ($1, let (modif, name) = $2 in (modif $1, name))  }
;

var_spec:
|       var_spec_bis { let (outer, inner,name) = $1 in
                       ((fun x -> outer (inner x)), name)}
| stars var_spec_bis
  { let (outer, inner, name) = $2 in
      ((fun x -> outer (inner ($1 x))), name) }
;

constant:
| INT_CONSTANT   { IntConstant $1 }
| FLOAT_CONSTANT { FloatConstant $1 }
| STRING_CONSTANT { StringConstant $1 }
| WSTRING_CONSTANT { WStringConstant $1 }
;

array_size:
| lexpr {Some $1}
| /* empty */ { None }
;

var_spec_bis:
| full_identifier     { ((fun x -> x),(fun x -> x), $1) }
| var_spec_bis LSQUARE array_size RSQUARE
      { let (outer, inner, name) = $1 in
          (outer, (fun x -> inner (LTarray (x,$3))), name)
      }
| LPAR var_spec RPAR { let (modif, name) = $2 in (modif, (fun x -> x), name) }
| var_spec_bis LPAR abs_param_type_list RPAR
      { let (outer, inner,name) = $1 in
        let params = $3 in
        (outer, (fun x -> inner (LTarrow (params,x))), name)
      }
;

abs_param_type_list:
| /* empty */    { [ ] }
| abs_param_list { $1 }
| abs_param_list COMMA DOTDOTDOT {
  let source = pos $symbolstartpos in
  Kernel.not_yet_implemented ~source "variadic C function types"
  }
;

abs_param_list:
| abs_param { [ $1 ] }
| abs_param_list COMMA abs_param { $1 @ [ $3 ] }
;

/* TODO: abs_param should be less restrictive than parameter
since its name can be omitted
*/
abs_param:
| type_spec(identifier_or_typename) abs_spec_option { $2 $1 }
| parameter { fst $1 }
;

ne_parameters:
| parameter { [$1] }
| parameter COMMA ne_parameters { $1 :: $3 }
;

parameter:
| type_spec(identifier_or_typename) var_spec
  { let (modif, name) = $2 in (modif $1, name)}
;


/*** type expressions ***/

logic_type_gen(tname):
| type_spec(tname) abs_spec_option { $2 $1 }
;

typename:
| name = TYPENAME { name }
;

typesymbol:
| name = TYPENAME { name }
| name = LONGIDENT { name }
/* TODO treat the case of an ACSL keyword that is also a typedef */
;

logic_type: logic_type_gen(typesymbol) { $1 }

cv:
  CONST { cv_const }
| VOLATILE { cv_volatile }
| GHOST { cv_ghost }
;

type_spec_cv:
     type_spec(TYPENAME) cv_after { $2 $1 }
|    cv type_spec_cv { LTattribute ($2, $1) }

cv_after:
  /* empty */ { fun t -> t }
| cv cv_after { fun t -> $2 (LTattribute (t,$1)) }

cast_logic_type:
 | type_spec_cv abs_spec_cv_option { $2 $1 }
;

logic_rt_type:
| logic_type_gen(identifier_or_typename) { $1 }
;

abs_spec_option:
| /* empty */ { fun t -> t }
| abs_spec    { $1 }
;

abs_spec_cv_option:
| /* empty */   { fun t -> t }
| abs_spec_cv { $1 }
;

abs_spec:
|                    tabs { $1 }
| stars                   { $1 }
| stars              tabs { fun t -> $2 ($1 t) }
| stars abs_spec_bis      { fun t -> $2 ($1 t) }
| stars abs_spec_bis tabs { fun t -> $2 ($3 ($1 t)) }
|       abs_spec_bis tabs { fun t -> $1 ($2 t) }
|       abs_spec_bis      { $1 }
;

abs_spec_cv:
|                         tabs { $1 }
| stars_cv                       { $1 }
| stars_cv                 tabs                { fun t -> $2 ($1 t) }
| stars_cv abs_spec_bis_cv       { fun t -> $2 ($1 t) }
| stars_cv abs_spec_bis_cv tabs                { fun t -> $2 ($3 ($1 t)) }
|          abs_spec_bis_cv tabs                { fun t -> $1 ($2 t) }
|          abs_spec_bis_cv       { $1 }
;

abs_spec_bis:
| LPAR abs_spec RPAR { $2 }
| abs_spec_bis LPAR abs_param_type_list RPAR { fun t -> $1 (LTarrow($3,t)) };
;

abs_spec_bis_cv:
| LPAR abs_spec_cv RPAR { $2 }
| abs_spec_bis_cv LPAR abs_param_type_list RPAR { fun t -> $1 (LTarrow($3,t)) };
;

stars:
| STAR          { fun t -> LTpointer t }
| stars STAR    { fun t -> (LTpointer ($1 t)) }
;

stars_cv:
| STAR          { fun t -> LTpointer t }
| STAR cv       { fun t -> LTattribute ((LTpointer t), $2) }
| stars_cv STAR    { fun t -> (LTpointer ($1 t)) }
| stars_cv STAR cv { fun t -> (LTattribute ((LTpointer ($1 t)), $3)) }
;

tabs:
| LSQUARE array_size RSQUARE
    {
      fun t -> LTarray (t,$2)
    }
| LSQUARE array_size RSQUARE tabs
    {
      fun t -> (LTarray ($4 t,$2))
    }
;

type_spec(tname):
| INTEGER        { LTinteger }
| REAL           { LTreal }
| BOOLEAN        { LTboolean }
| VOID           { LTvoid }
| BOOL           { LTint IBool }
| CHAR           { LTint IChar }       /** [char] */
| SIGNED CHAR    { LTint ISChar }      /** [signed char] */
| UNSIGNED CHAR  { LTint IUChar }      /** [unsigned char] */
| INT            { LTint IInt }        /** [int] */
| SIGNED INT     { LTint IInt }        /** [int] */
| UNSIGNED INT   { LTint IUInt }       /** [unsigned int] */
| UNSIGNED       { LTint IUInt }
| SHORT          { LTint IShort }      /** [short] */
| SIGNED SHORT   { LTint IShort }      /** [short] */
| UNSIGNED SHORT { LTint IUShort }     /** [unsigned short] */
| SHORT INT          { LTint IShort }      /** [short] */
| SIGNED SHORT INT   { LTint IShort }      /** [short] */
| UNSIGNED SHORT INT { LTint IUShort }     /** [unsigned short] */
| LONG           { LTint ILong }       /** [long] */
| SIGNED LONG    { LTint ILong }       /** [long] */
| UNSIGNED LONG  { LTint IULong }      /** [unsigned long] */
| SIGNED LONG INT{ LTint ILong }       /** [long] */
| LONG  INT      { LTint ILong }       /** [long] */
| UNSIGNED LONG INT { LTint IULong }      /** [unsigned long] */
| LONG LONG      { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| SIGNED LONG LONG   { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| UNSIGNED LONG LONG { LTint IULongLong }  /** [unsigned long long]
                                (or [unsigned _int64] on Microsoft Visual C) */
| LONG LONG INT     { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| SIGNED LONG LONG INT  { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| UNSIGNED LONG LONG INT { LTint IULongLong }  /** [unsigned long long]
                                (or [unsigned _int64] on Microsoft Visual C) */
| FLOAT             { LTfloat FFloat }
| FLOAT32           { LTfloat FFloat32 }
| FLOAT64           { LTfloat FFloat64 }
| DOUBLE            { LTfloat FDouble }
| LONG DOUBLE       { LTfloat FLongDouble }
| STRUCT id = identifier_or_typename_full { LTstruct id }
| ENUM id = identifier_or_typename_full { LTenum id }
| UNION id = identifier_or_typename_full  { LTunion id }
| name = tname          { LTnamed (name,[]) }
| name = tname LT prms = ne_logic_type_list(tname) GT { LTnamed(name,prms) }
;

ne_logic_type_list(tname):
| l = separated_nonempty_list(COMMA,logic_type_gen(tname)) { l }
;

symbol_identifier:
| id = full_identifier { id }
| name = LONGIDENT { name }
;

full_identifier:
| id = identifier { id }
| ADMIT { "admit" }
| ALLOCATES { "allocates" }
| ASSERT { "assert" }
| ASSIGNS { "assigns" }
| ASSUMES { "assumes" }
| AXIOM { "axiom" }
| AXIOMATIC { "axiomatic" }
| BEHAVIOR { "behavior" }
| BREAKS { "breaks" }
| CHECK { "check" }
| COMPLETE { "complete" }
| CONTINUES { "continues" }
| DECREASES { "decreases" }
| DISJOINT { "disjoint" }
| ENSURES { "ensures" }
| EXITS { "exits" }
| FREES { "frees" }
| GLOBAL { "global" }
| IMPORT { "import" }
| INDUCTIVE { "inductive" }
| INVARIANT { "invariant" }
| LEMMA { "lemma" }
| LOGIC { "logic" }
| LOOP { "loop" }
| MODEL { "model" }
| MODULE { "module" }
| PREDICATE { "predicate" }
| REQUIRES { "requires" }
| RETURNS { "returns" }
| TERMINATES { "terminates" }
| TYPE { "type" }
| VARIANT { "variant" }
| EXT_SPEC_MODULE { "module" }
| EXT_SPEC_FUNCTION { "function" }
| EXT_SPEC_CONTRACT { "contract" }
| EXT_SPEC_INCLUDE { "include" }
| EXT_SPEC_AT { "at" }
| EXT_SPEC_LET { "let" }
| id = EXT_CODE_ANNOT { fst id }
| id = EXT_CONTRACT { fst id }
| id = EXT_GLOBAL { fst id }
| id = EXT_GLOBAL_BLOCK { fst id }
| id = EXT_LOADER { fst id }
| id = EXT_LOADER_PLUGIN { fst id }
| id = IDENTIFIER_EXT { id }
| id = IDENTIFIER_LOADER { id }
;

%inline unknown_extension:
| id=IDENTIFIER_EXT content=extension_content { (id,content) }


/*** ACSL extension for external spec file ***/

ext_spec:
 | ext_global_clauses_opt ext_module_specs_opt ext_global_specs_opt EOF { (None,$1,$2)::$3 }
;

ext_global_clauses_opt:
 | /* empty */         { [] }
 | ext_global_clauses  { $1 }
;

ext_global_clauses:
| ext_global_clause                    { [$1] }
| ext_global_clause ext_global_clauses { $1::$2 }
;

ext_global_clause:
| decl  { Ext_decl (loc_decl $sloc $1) }
| GLOBAL
  EXT_SPEC_LET any_identifier EQUAL lexpr SEMICOLON { Ext_macro (true, $3, $5) }
| EXT_SPEC_LET any_identifier EQUAL lexpr SEMICOLON { Ext_macro (false, $2, $4) }
| EXT_SPEC_INCLUDE string SEMICOLON { let b,s = $2 in Ext_include(b,s, loc $sloc) }
;

ext_global_specs_opt:
 | /* empty */       { [] }
 | ext_global_specs  { $1 }
;

ext_global_specs:
| ext_global_spec                  { [$1] }
| ext_global_spec ext_global_specs { $1::$2 }
;

ext_global_spec:
| ext_module_markup ext_global_clauses_opt ext_module_specs
    { (Some $1),$2,$3 }
| ext_module_markup ext_global_clauses_opt
    { (Some $1),$2,[] }
;

ext_module_specs_opt:
 | /* empty */      { [] }
 | ext_module_specs { $1 }
 | ext_fun_specs { [None, $1] }
 | ext_fun_specs ext_module_specs { (None, $1)::$2 }
;

ext_module_specs:
| ext_module_spec                  { [$1] }
| ext_module_spec ext_module_specs { $1::$2 }
;

ext_module_spec:
| ext_function_markup ext_function_specs_opt { (Some $1),$2 }
;

ext_function_specs_opt:
| /* empty */         { [] }
| ext_function_specs  { $1 }
;

ext_function_specs:
| ext_at_stmt_markup  { []}
| ext_function_spec   { [$1] }
| ext_function_spec ext_function_specs { $1::$2 }
;

ext_function_spec:
| ext_global_clause { Ext_glob $1 }
| ext_fun_spec      { $1 }
;

ext_fun_specs:
| ext_fun_spec               { [$1] }
| ext_fun_spec ext_fun_specs { $1::$2 }
;

ext_fun_spec:
| ext_at_stmt_markup ext_stmt_loop_spec
    { Ext_stmt($1,$2,loc $sloc) }
| ext_contract_markup contract
    { let s,pos = $2 in Ext_spec (s,pos) }
;

ext_stmt_loop_spec:
| annotation { $1 }
| ext_contract_markup contract { let s, pos = $2 in Acode_annot (pos, AStmtSpec ([],s)) }
;

ext_identifier_opt:
| /* empty*/     { "" }
| ext_identifier { $1 }
;

ext_identifier:
| any_identifier { $1 }
;

ext_module_markup:
| EXT_SPEC_MODULE ext_identifier COLON { $2 }
;

ext_function_markup:
| EXT_SPEC_FUNCTION ext_identifier COLON { $2, loc $sloc }
;

ext_contract_markup:
| EXT_SPEC_CONTRACT ext_identifier_opt COLON { $2 }
;

stmt_markup:
| any_identifier { $1 }
| INT_CONSTANT { $1 }
;

stmt_markup_attr:
| stmt_markup                      { [$1] }
| stmt_markup stmt_markup_attr { $1 :: $2 }
;

ext_at_stmt_markup:
| EXT_SPEC_AT stmt_markup_attr COLON { $2 }
;

/*** function and statement contracts ***/

spec:
| contract EOF { fst $1 }
;

contract:
| requires terminates decreases simple_clauses behaviors complete_or_disjoint
    { let requires=$1 in
      let (empty,(allocation,assigns,post_cond,extended)) = $4 in
      let behaviors = $5 in
      let (completes,disjoints) = $6 in
      let behaviors =
        if requires <> [] || post_cond <> [] ||
	   allocation <> FreeAllocAny ||
           assigns <> WritesAny || extended <> []
        then
          (Cabshelper.mk_behavior
             ~requires ~post_cond ~assigns ~allocation ~extended ())
          :: behaviors
        else if $2<>None || $3<>None ||
                behaviors<>[] || completes<>[] ||disjoints<>[]
        then behaviors
        else
          if empty then
            raise (Not_well_formed (loc $sloc,"Empty annotation is not allowed"))
          else
            raise Unknown_ext
      in
        { spec_terminates = $2;
          spec_variant = $3;
          spec_behavior = behaviors;
          spec_complete_behaviors = completes;
          spec_disjoint_behaviors = disjoints;
        }, loc $sloc
    }
| requires ne_terminates REQUIRES { clause_order $loc($3) "requires" "terminates" }
| requires terminates ne_decreases REQUIRES
      { clause_order $loc($4) "requires" "decreases" }
| requires terminates ne_decreases TERMINATES
      { clause_order $loc($4) "terminates" "decreases" }
| requires terminates decreases ne_simple_clauses REQUIRES
      { clause_order $loc($5) "requires" "post-condition, assigns or allocates" }
| requires terminates decreases ne_simple_clauses TERMINATES
      { clause_order $loc($5) "terminates" "post-condition, assigns or allocates" }
| requires terminates decreases ne_simple_clauses DECREASES
      { clause_order $loc($5) "decreases" "post-condition, assigns or allocates" }
| requires terminates decreases simple_clauses ne_behaviors TERMINATES
      { clause_order $loc($6) "terminates" "behavior" }
| requires terminates decreases simple_clauses ne_behaviors DECREASES
      { clause_order $loc($6) "decreases" "behavior" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  REQUIRES
      { clause_order $loc($7) "requires" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  TERMINATES
      { clause_order $loc($7) "terminates" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  DECREASES
      { clause_order $loc($7) "decreases" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  BEHAVIOR
      { clause_order $loc($7) "behavior" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  ASSIGNS
      { clause_order $loc($7) "assigns" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  ALLOCATES
      { clause_order $loc($7) "allocates" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  FREES
      { clause_order $loc($7) "frees" "complete or disjoint" }
| requires terminates decreases simple_clauses behaviors ne_complete_or_disjoint
  post_cond_kind
      { clause_order $loc($7) "post-condition" "complete or disjoint" }
;

// use that to detect potentially missing ';' at end of clause
clause_kw:
| ADMIT_REQUIRES { "admit requires" }
| ADMIT_INVARIANT { "admit invariant" }
| ADMIT_LEMMA { "admit lemma" }
| ADMIT_LOOP { "admit loop" }
| CHECK_REQUIRES { "check requires" }
| CHECK_INVARIANT { "check invariant" }
| CHECK_LEMMA { "check lemma" }
| CHECK_LOOP { "check loop" }
| REQUIRES { "requires" }
| ASSUMES {"assumes"}
| ASSIGNS { "assigns" }
| post_cond { snd $1 }
| DECREASES { "decreases"}
| BEHAVIOR { "behavior"}
| ALLOCATES {"allocates"}
| FREES {"frees"}
| COMPLETE {"complete"}
| DISJOINT {"disjoint"}
| EXT_CONTRACT { fst $1 }
| id=IDENTIFIER_EXT { id }
| EOF { "end of annotation" }
;

requires:
| /* epsilon */ { [] }
| ne_requires { $1 }
;

ne_requires:
| REQUIRES lexpr SEMICOLON requires { toplevel_pred Assert $2::$4 }
| CHECK_REQUIRES lexpr SEMICOLON requires { toplevel_pred Check $2 :: $4 }
| ADMIT_REQUIRES lexpr SEMICOLON requires { toplevel_pred Admit $2 :: $4 }
| REQUIRES lexpr clause_kw { missing $loc($2) ";" $3 }
| CHECK_REQUIRES lexpr clause_kw { missing $loc($2) ";" $3 }
| ADMIT_REQUIRES lexpr clause_kw { missing $loc($2) ";" $3 }
;

terminates:
| /* epsilon */ { None }
| ne_terminates { Some $1 }
;

ne_terminates:
| TERMINATES lexpr SEMICOLON { $2 }
| TERMINATES lexpr clause_kw { missing $loc($2) ";" $3 }
;

decreases:
| /* epsilon */   { None }
| ne_decreases { Some $1 }
;

ne_decreases:
| DECREASES variant SEMICOLON { $2 }
| DECREASES variant clause_kw { missing $loc($2) ";" $3 }
;

variant:
| lexpr FOR full_identifier { ($1, Some $3) }
| lexpr                     { ($1, None) }
;

simple_clauses:
| /* epsilon */ { true,(FreeAllocAny,WritesAny,[],[]) }
| ne_simple_clauses { false,$1 }
;

allocation:
| ALLOCATES zones { FreeAlloc([],$2) }
| FREES zones { FreeAlloc($2,[]) }

ne_simple_clauses:
| post_cond_kind lexpr SEMICOLON simple_clauses
    { let tp_kind, kind = $1 in
      let allocation,assigns,post_cond,extended = snd $4 in
      allocation,assigns,
      ((kind,toplevel_pred tp_kind $2)::post_cond),extended }
| allocation SEMICOLON simple_clauses
    { let allocation,assigns,post_cond,extended = snd $3 in
      let a = concat_allocation allocation $1 in
      a,assigns,post_cond,extended
    }
| ASSIGNS assigns SEMICOLON simple_clauses
    { let allocation,assigns,post_cond,extended = snd $4 in
      let a = concat_assigns $sloc assigns $2
      in allocation,a,post_cond,extended
    }
| unknown_extension SEMICOLON simple_clauses { snd $3 }
| EXT_CONTRACT extension_content SEMICOLON simple_clauses
    { let allocation,assigns,post_cond,extended = snd $4 in
      let name, plugin = $1 in
      let processed = Logic_env.preprocess_extension ~plugin name $2 in
      let ext = extension name plugin processed in
      allocation,assigns,post_cond,ext::extended
    }
| post_cond_kind lexpr clause_kw { missing $loc($2) ";" $3 }
| allocation clause_kw { missing $loc($1) ";" $2 }
| ASSIGNS assigns clause_kw { missing $loc($2) ";" $3 }
| EXT_CONTRACT ne_grammar_extension clause_kw { missing $loc($1) ";" $3 }
;

ne_grammar_extension:
| zones { $1 }
;

/* possibly empty list of terms, for ACSL extensions registered by plugins. */
extension_content:
| /* epsilon */ { [] }
| zones { $1 }
;

post_cond_kind:
| post_cond { fst $1 }
;

behaviors:
| /* epsilon */ { [] }
| ne_behaviors { $1 }

ne_behaviors:
| BEHAVIOR behavior_name COLON behavior_body behaviors
      { let (assumes,requires,(allocation,assigns,post_cond,extended)) = $4 in
        let behaviors = $5 in
        let b =
          Cabshelper.mk_behavior
            ~name:$2
            ~assumes ~requires ~post_cond ~assigns ~allocation ~extended ()
        in b::behaviors
      }
;

behavior_body:
| assumes requires simple_clauses { $1,$2,snd $3 }
| assumes ne_requires ASSUMES
      { clause_order $loc($3) "assumes" "requires" }
| assumes requires ne_simple_clauses ASSUMES
      { clause_order $loc($4) "assumes" "assigns or post-condition" }
| assumes requires ne_simple_clauses REQUIRES
      { clause_order $loc($4) "requires" "assigns or post-condition" }
;

assumes:
| /* epsilon */ { [] }
| ASSUMES lexpr SEMICOLON assumes { $2::$4 }
| ASSUMES lexpr clause_kw { missing $loc($2) ";" $3 }
;

complete_or_disjoint:
| /* epsilon */ { [],[] }
| ne_complete_or_disjoint { $1 }

ne_complete_or_disjoint:
| COMPLETE BEHAVIORS behavior_name_list SEMICOLON
    complete_or_disjoint
      { let complete,disjoint = $5 in $3::complete, disjoint }
| DISJOINT BEHAVIORS behavior_name_list SEMICOLON
          complete_or_disjoint
      { let complete,disjoint = $5 in complete,$3::disjoint }
/* complete behaviors decreases; is valid (provided there's a behavior
   named decreases)
*/
| COMPLETE BEHAVIORS ne_behavior_name_list clause_kw { missing $loc($3) ";" $4 }
| DISJOINT BEHAVIORS ne_behavior_name_list clause_kw { missing $loc($3) ";" $4 }
;

/*** assigns and tsets ***/

assigns:
| zones { List.map (fun x -> (x,FromAny)) $1 }
| ne_zones FROM zones {List.map (fun x -> (x, From $3)) $1}
;

zones:
| ne_zones { $1 }
| NOTHING  { [] }
;

ne_zones:
| ne_lexpr_list { $1 }
;

/*** annotations ***/

annot:
| annotation EOF  { $1 }
| is_acsl_spec any EOF { Aspec }
| decl_list EOF   { Adecl ($1) }
;

// Not supported anymore
attribute_annot:
| identifier { $1 }
| GHOST { "\\ghost" }

annotation:
| loop_annotations
      { let (b,v,p) = $1 in
        (* TODO: do better, do not lose the structure ! *)
        let l = b@v@p in
        Aloop_annot (loc $sloc, l) }
| FOR ne_behavior_name_list COLON contract_or_code_annotation
      { $4 $2 }
| code_annotation { Acode_annot (loc $sloc,$1 []) }
| code_annotation beg_code_annotation
      { raise
          (Not_well_formed (loc $sloc,
                            "Only one code annotation is allowed per comment"))
      }
| attribute_annot {
    raise (Not_well_formed (loc $sloc,
      "Attribute annotation '"^ $1 ^"' are not supported anymore, use regular \
       C attributes instead."))
  }
| unknown_extension SEMICOLON { raise Unknown_ext }
;

contract_or_code_annotation:
| contract
      { fun bhvs -> let s, pos = $1 in Acode_annot (pos, AStmtSpec (bhvs,s)) }
| code_annotation { fun bhvs -> Acode_annot (loc $sloc, ($1 bhvs)) }
;

/*** loop annotations ***/

loop_annotations:
| loop_annot_stack
    { let (i,fa,a,b,v,p, e) = $1 in
      let invs = List.map (fun i -> AInvariant([],true,i)) i in
      let ext = List.map (fun x -> AExtended([],true, x)) e in
      let oth = match a with
        | WritesAny -> b
        | Writes _ ->
            (* by definition all existing AAssigns are tied to at least
               one behavior. No need to merge against them. *)
            AAssigns ([],a)::b
      in
      let oth = match fa with
        | FreeAllocAny -> oth
        | _ -> AAllocation ([],fa)::oth
      in
	(invs@oth@ext,v,p)
    }
;

/* TODO: gather loop assigns that are related to the same behavior */
loop_annot_stack:
| loop_invariant loop_annot_opt
    { let (i,fa,a,b,v,p,e) = $2 in ($1::i,fa,a,b,v,p,e) }
| loop_effects loop_annot_opt
    { let (i,fa,a,b,v,p,e) = $2 in (i,fa,concat_assigns $sloc a $1,b,v,p,e) }
| loop_allocation loop_annot_opt
    { let (i,fa,a,b,v,p,e) = $2 in (i,concat_allocation fa $1,a,b,v,p,e) }
| FOR ne_behavior_name_list COLON loop_annot_stack
    { let (i,fa,a,b,v,p,e) = $4 in
      let behav = $2 in
      let invs = List.map (fun i -> AInvariant(behav,true,i)) i in
      let ext = List.map (fun x -> AExtended(behav,true,x)) e in
      let oth = concat_loop_assigns_allocation $sloc b behav a fa in
      ([],FreeAllocAny,WritesAny,invs@ext@oth,v,p,[])
    }
| loop_variant loop_annot_opt
    { let pos,loop_variant = $1 in
      let (i,fa,a,b,v,p,e) = $2 in
      check_empty
        (pos,"loop invariant is not allowed after loop variant.") i ;
      check_empty
        (pos, "loop extension is not allowed after loop variant.") e;
      (match fa with
        | FreeAlloc(f,a) ->
	    check_empty
              (pos,"loop frees is not allowed after loop variant.") f ;
	    check_empty
              (pos,"loop allocates is not allowed after loop variant.") a
        | FreeAllocAny -> ());
      (match a with
          WritesAny -> ()
        | Writes _ ->
          raise
            (Not_well_formed
               (pos,"loop assigns is not allowed after loop variant.")));
      check_empty
        (pos,"loop behavior is not allowed after loop variant.") b ;
      check_empty
        (pos,"loop annotations can have at most one variant.") v ;
      (i,fa,a,b,AVariant loop_variant::v,p,e) }
| loop_grammar_extension loop_annot_opt {
    let (i,fa,a,b,v,p,e) = $2 in
    (i,fa,a,b,v,p, $1::e)
  }
| LOOP unknown_extension SEMICOLON loop_annot_opt { $4 }
;

loop_annot_opt:
| /* epsilon */
    { ([], FreeAllocAny, WritesAny, [], [], [], []) }
| loop_annot_stack
    { $1 }
;

loop_effects:
| LOOP ASSIGNS assigns SEMICOLON { $3 }
;

loop_allocation:
| LOOP allocation SEMICOLON { $2 }
;

loop_invariant:
| LOOP INVARIANT lexpr SEMICOLON { toplevel_pred Assert $3 }
| CHECK_LOOP INVARIANT lexpr SEMICOLON { toplevel_pred Check $3 }
| ADMIT_LOOP INVARIANT lexpr SEMICOLON { toplevel_pred Admit $3 }
;

loop_variant:
| LOOP VARIANT variant SEMICOLON { loc $sloc,$3 }
;

/* Grammar Extensibility for plugins */
loop_grammar_extension:
| LOOP EXT_CODE_ANNOT extension_content SEMICOLON {
  let open Cil_types in
  let name, plugin = $2 in
  try
    begin match Logic_env.extension_category ~plugin name with
      | Ext_code_annot (Ext_next_loop | Ext_next_both) ->
        let processed = Logic_env.preprocess_extension ~plugin name $3 in
        {ext_name = name; ext_plugin = plugin; ext_content = processed}
      | Ext_code_annot (Ext_here | Ext_next_stmt) ->
        raise
          (Not_well_formed
            (loc $loc($2), name ^ " is not a loop annotation extension"))
      | _ -> raise Not_found
    end
  with Not_found ->
    Kernel.fatal ~source:(pos $startpos($2))
      "%s is not a code annotation extension. Parser got wrong lexeme." name
}
;


/*** code annotations ***/
beg_code_annotation:
| FOR {}
| ASSERT {}
| CHECK {}
| ADMIT {}
| INVARIANT {}
| CHECK_INVARIANT {}
| ADMIT_INVARIANT {}
| CHECK_LOOP {}
| ADMIT_LOOP {}
| EXT_CODE_ANNOT {}
;

code_annotation:
| ASSERT lexpr SEMICOLON
  { fun bhvs -> AAssert (bhvs,toplevel_pred Assert $2) }
| CHECK lexpr SEMICOLON
  { fun bhvs -> AAssert (bhvs,toplevel_pred Check $2) }
| ADMIT lexpr SEMICOLON
  { fun bhvs -> AAssert (bhvs,toplevel_pred Admit $2) }
| INVARIANT lexpr SEMICOLON
  { fun bhvs -> AInvariant (bhvs,false,toplevel_pred Assert $2) }
| CHECK_INVARIANT lexpr SEMICOLON
  { fun bhvs -> AInvariant (bhvs,false,toplevel_pred Check $2) }
| ADMIT_INVARIANT lexpr SEMICOLON
  { fun bhvs -> AInvariant (bhvs,false,toplevel_pred Admit $2) }
| EXT_CODE_ANNOT extension_content SEMICOLON
  { fun bhvs ->
    let open Cil_types in
    let name, plugin = $1 in
    try
      begin match Logic_env.extension_category ~plugin name with
        | Ext_code_annot (Ext_here | Ext_next_stmt | Ext_next_both) ->
          let processed = Logic_env.preprocess_extension ~plugin name $2 in
          let ext = extension name plugin processed in
          Logic_ptree.AExtended(bhvs,false,ext)
        | Ext_code_annot Ext_next_loop ->
          raise
            (Not_well_formed
               (loc $loc($1),
                Printf.sprintf
                  "%s is a loop annotation extension. It can't be used as a \
                   plain code annotation extension. Did you mean 'loop %s'?"
                  name name))
        | _ -> raise Not_found
      end
    with Not_found ->
      Kernel.fatal ~source:(pos $startpos($1))
        "%s is not a code annotation extension. Parser got wrong lexeme" name
  }
;

/*** declarations and logical definitions ***/

decl_list:
| decl            { [loc_decl $sloc $1] }
| decl decl_list  { (loc_decl $sloc $1) :: $2 }
;

decl:
| GLOBAL INVARIANT any_identifier COLON lexpr SEMICOLON
    { LDinvariant ($3, $5) }
| VOLATILE ne_zones volatile_opt SEMICOLON { LDvolatile ($2, $3) }
| type_annot {LDtype_annot $1}
| model_annot {LDmodel_annot $1}
| logic_decl { $1 }
| ext_decl { LDextended $1 }
;

ext_decl:
| EXT_GLOBAL extension_content SEMICOLON {
    let name, plugin = $1 in
     let processed = Logic_env.preprocess_extension ~plugin name $2 in
     let ext = extension name plugin processed in
     Ext_lexpr ext
   }
| EXT_GLOBAL_BLOCK any_identifier LBRACE ext_decls RBRACE {
    let name, plugin = $1 in
    let processed_id,processed_block =
       Logic_env.preprocess_extension_block ~plugin name ($2,$4)
    in
    global_extension name plugin processed_id processed_block
   }
;

ext_decls:
| /* epsilon */
    { [] }
| unknown_extension SEMICOLON ext_decls { $3 }
| ext_decl_loc ext_decls
    { $1::$2 }
;

ext_decl_loc:
| ext_decl { loc_ext $sloc $1 }
;

volatile_opt:
| /* empty */ { None, None }
| READS any_identifier volatile_opt
              { let read,write=$3 in
                  if read = None then
		    (Some $2),write
		  else
                    (Format.eprintf "Warning: read %s ignored@." $2; $3)
	      }
| WRITES any_identifier volatile_opt
              { let read,write=$3 in
                  if write = None then
		    read,(Some $2)
		  else
                    (Format.eprintf "Warning: write %s ignored@." $2; $3)
	      }
;

type_annot:
| TYPE INVARIANT any_identifier LPAR parameter RPAR EQUAL
    lexpr SEMICOLON
  { let typ,name = $5 in{ inv_name = $3; this_name = name; this_type = typ; inv = $8; } }
;

opt_semicolon:
| /* epsilon */ { }
| SEMICOLON { }
;

model_annot:
| MODEL type_spec(typename) LBRACE parameter opt_semicolon RBRACE SEMICOLON
  { let typ,name = $4 in
    { model_for_type = $2; model_name = name; model_type = typ; }
  }
;

poly_id_type:
| full_identifier
    { enter_type_variables_scope []; ($1,[]) }
| full_identifier LT ne_tvar_list GT
    { enter_type_variables_scope $3; ($1,$3) }
;

/* we need to recognize the typename as soon as it has been declared,
  so that it can be used in data constructors in the type definition itself
*/
poly_id_type_add_typename:
| poly_id_type { push_typename (fst $1) ; $1 }
;

poly_id:
| poly_id_type { let (id,tvar) = $1 in (id,[],tvar) }
| full_identifier LBRACE ne_label_list RBRACE
      { enter_type_variables_scope []; ($1,$3,[]) }
| full_identifier LBRACE ne_label_list RBRACE LT ne_tvar_list GT
      { enter_type_variables_scope $6; $1,$3,$6 }
;

opt_parameters:
| /*epsilon*/ { [] }
| parameters { $1 }
;

parameters:
| LPAR ne_parameters RPAR { $2 }
;

logic_def:
/* logic function definition */
| LOGIC logic_rt_type poly_id opt_parameters EQUAL lexpr SEMICOLON
    { let (id, labels, tvars) = $3 in
      exit_type_variables_scope ();
      LDlogic_def (id, labels, tvars, $2, $4, $6) }
/* predicate definition */
| PREDICATE poly_id opt_parameters EQUAL lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDpredicate_def (id, labels, tvars, $3, $5) }
/* inductive predicate definition */
| INDUCTIVE pred = poly_id params = parameters
    midrule({exit_type_variables_scope()})
    LBRACE cases = indcases RBRACE
    { let (id,labels,tvars) = pred in
      LDinductive_def(id, labels, tvars, params, cases) }
| LEMMA poly_id COLON lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, labels, tvars, toplevel_pred Assert $4) }
| CHECK_LEMMA poly_id COLON lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, labels, tvars, toplevel_pred Check $4) }
| ADMIT_LEMMA poly_id COLON lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, labels, tvars, toplevel_pred Admit $4) }
| AXIOMATIC any_identifier LBRACE logic_decls RBRACE
    { LDaxiomatic($2,$4) }
| MODULE push_module_name LBRACE logic_decls RBRACE
    { pop_module_types () ; LDmodule($2,$4) }
| IMPORT mId = module_name SEMICOLON
    { import None mId None }
| IMPORT mId = module_name AS id = IDENTIFIER SEMICOLON
    { import None mId (Some id) }
| IMPORT loader = ext_loader mId = module_name SEMICOLON
    { import (Some loader) mId None }
| IMPORT loader = ext_loader mId = module_name AS id = IDENTIFIER SEMICOLON
    { import (Some loader) mId (Some id) }
| TYPE poly_id_type_add_typename EQUAL typedef SEMICOLON
        { let (id,tvars) = $2 in
          exit_type_variables_scope ();
          LDtype(id,tvars,Some $4)
        }
;

module_name:
| IDENTIFIER { $1 }
| LONGIDENT  { $1 }
;

push_module_name:
| module_name { push_module_types () ; $1 }
;

ext_loader:
| IDENTIFIER COLON {
    Kernel.warning ~once:true ~wkey:Kernel.wkey_extension_unknown
      ~source:(fst (loc $sloc))
      "Ignoring unregistered module importer extension '%s'" $1;
    raise Unknown_ext
  }
| EXT_LOADER COLON  { $1 }
| EXT_LOADER_PLUGIN { $1 }
| IDENTIFIER_LOADER { raise Unknown_ext }


logic_decls:
| /* epsilon */
    { [] }
| logic_decl_loc logic_decls
    { $1::$2 }
;

logic_decl:
| logic_def  { $1 }
/* logic function declaration */
| LOGIC logic_rt_type poly_id opt_parameters reads_clause SEMICOLON
    { let (id, labels, tvars) = $3 in
      exit_type_variables_scope ();
      LDlogic_reads (id, labels, tvars, $2, $4, $5) }
/* predicate declaration */
| PREDICATE poly_id opt_parameters reads_clause SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDpredicate_reads (id, labels, tvars, $3, $4) }
/* type declaration */
| TYPE poly_id_type SEMICOLON
    { let (id,tvars) = $2 in
      exit_type_variables_scope ();
      push_typename id;
      LDtype(id,tvars,None) }
/* axiom */
| AXIOM poly_id COLON lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, labels, tvars, toplevel_pred Admit $4) }
;

logic_decl_loc:
| logic_decl { loc_decl $sloc $1 }
;


reads_clause:
| /* epsilon */ { None }
| READS zones { Some $2 }
;

typedef:
| ne_datacons_list { TDsum $1 }
| logic_type { TDsyn $1 }
;

datacons_list:
| /* epsilon */ { [] }
| PIPE datacons datacons_list { $2 :: $3 }
;

ne_datacons_list:
| datacons datacons_list { $1 :: $2 }
| PIPE datacons datacons_list { $2 :: $3 }
;

datacons:
| full_identifier { ($1,[]) }
| full_identifier LPAR ne_type_list RPAR { ($1,$3) }
;

ne_type_list:
| logic_type { [$1] }
| logic_type COMMA ne_type_list { $1::$3 }

indcases:
| /* epsilon */
    { [] }
| CASE poly_id COLON lexpr SEMICOLON indcases
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      (id,labels,tvars,$4)::$6 }
;


ne_tvar_list:
| full_identifier                    { [$1] }
| full_identifier COMMA ne_tvar_list { $1 :: $3 }
;

ne_label_list:
| label_name                     { [$1] }
| label_name COMMA ne_label_list { $1 :: $3 }
;

opt_label_1:
| opt_label_list { match $1 with
		     | [] -> None
		     | l::[] -> Some l
		     | _ -> raise (Not_well_formed (loc $sloc,"Only one label is allowed")) }
;

opt_label_2:
| opt_label_list { match $1 with
		     | [] -> None
		     | l1::l2::[] -> Some (l1,l2)
		     | _::[] -> raise (Not_well_formed (loc $sloc,"One label is missing"))
		     | _ -> raise (Not_well_formed (loc $sloc,"Only two labels are allowed")) }
;

opt_label_list:
| /* epsilon */               { [] }
| LBRACE ne_label_list RBRACE { $2 }
;

/* names */
label_name:
| any_identifier { $1 }
;

behavior_name_list:
| /* epsilon */         { [] }
| ne_behavior_name_list { $1 }
;

ne_behavior_name_list:
| behavior_name                             { [$1] }
| behavior_name COMMA ne_behavior_name_list {$1 :: $3}
;

behavior_name:
| any_identifier { $1 }
;

any_identifier:
| identifier { $1 }
| is_acsl_typename { $1 }
| TYPENAME { $1 }
| keyword { $1 }
;

identifier_or_typename:
| TYPENAME { $1 }
| LONGIDENT { $1 }
| full_identifier { $1 }
;

identifier_or_typename_full: /* allowed as C field names */
| is_acsl_typename  { $1 }
| TYPENAME { $1 }
| full_identifier { $1 }
;

identifier: /* part included into 'identifier_or_typename', but duplicated to avoid parsing conflicts */
| IDENTIFIER { $1 }
/* token list used inside acsl clauses: */
| BEHAVIORS  { "behaviors" }
| LABEL      { "label" }
| READS      { "reads" }
| WRITES     { "writes" }
;

bounded_var:
| full_identifier { $1 }
| is_acsl_typename /* Since TYPENAME cannot be accepted by lexpr rule */
    { raise
	(Not_well_formed(loc $sloc,
			 "Type names are not allowed as binding variable"))
    }
| TYPENAME  /* Since TYPENAME cannot be accepted by lexpr rule */
    { raise
	(Not_well_formed(loc $sloc,
			 "Type names are not allowed as binding variable"))
    }
;

c_keyword:
| ALIGNOF  { "alignof" }
| CHAR     { "char" }
| BOOLEAN  { "boolean" }
| BOOL     { "_Bool" }
| CONST    { "const" }
| DOUBLE   { "double" }
| ENUM     { "enum" }
| ELSE     { "else" }
| FLOAT    { "float" }
| FLOAT32  { "_Float32" }
| FLOAT64  { "_Float64" }
| IF       { "if" }
| INT      { "int" }
| LONG     { "long" }
| SHORT    { "short" }
| SIGNED   { "signed" }
| SIZEOF   { "sizeof" }
| STATIC   { "static" }
| STRUCT   { "struct" }
| UNION    { "union" }
| UNSIGNED { "unsigned" }
| VOID     { "void" }
;

acsl_c_keyword:
| CASE     { "case" }
| FOR      { "for" }
| VOLATILE { "volatile" }
;

post_cond:
| ENSURES { (Assert,Normal), "ensures" }
| EXITS   { (Assert,Exits), "exits" }
| BREAKS  { (Assert,Breaks), "breaks" }
| CONTINUES { (Assert,Continues), "continues" }
| RETURNS { (Assert,Returns), "returns" }
| CHECK_ENSURES { (Check,Normal), "check ensures" }
| CHECK_EXITS   { (Check,Exits), "check exits" }
| CHECK_BREAKS  { (Check,Breaks), "check breaks" }
| CHECK_CONTINUES { (Check,Continues), "check continues" }
| CHECK_RETURNS { (Check,Returns), "check returns" }
| ADMIT_ENSURES { (Admit,Normal), "admit ensures" }
| ADMIT_EXITS   { (Admit,Exits), "admit exits" }
| ADMIT_BREAKS  { (Admit,Breaks), "admit breaks" }
| ADMIT_CONTINUES { (Admit,Continues), "admit continues" }
| ADMIT_RETURNS { (Admit,Returns), "admit returns" }
;

is_acsl_spec:
| post_cond  { snd $1 }
| EXT_CONTRACT { fst $1 }
| ASSIGNS    { "assigns" }
| ALLOCATES  { "allocates" }
| FREES      { "frees" }
| BEHAVIOR   { "behavior" }
| REQUIRES   { "requires" }
| CHECK_REQUIRES { "check requires" }
| ADMIT_REQUIRES { "admit requires" }
| TERMINATES { "terminates" }
| COMPLETE   { "complete" }
| DECREASES  { "decreases" }
| DISJOINT   { "disjoint" }
;

is_acsl_decl_or_code_annot:
| EXT_CODE_ANNOT { fst $1 }
| EXT_GLOBAL { fst $1 }
| EXT_GLOBAL_BLOCK { fst $1 }
| EXT_LOADER { fst $1 }
| EXT_LOADER_PLUGIN { fst $1 }
| IDENTIFIER_EXT { $1 }
| IDENTIFIER_LOADER { $1 }
| ASSUMES   { "assumes" }
| ASSERT    { "assert" }
| CHECK     { "check" }
| ADMIT     { "admit" }
| GLOBAL    { "global" }
| INDUCTIVE { "inductive" }
| INVARIANT { "invariant" }
| ADMIT_INVARIANT { "admit invariant" }
| CHECK_INVARIANT { "check invariant" }
| LEMMA     { "lemma" }
| ADMIT_LEMMA { "admit lemma" }
| CHECK_LEMMA { "check lemma" }
| LOOP      { "loop" }
| ADMIT_LOOP { "admit loop" }
| CHECK_LOOP { "check loop" }
| PREDICATE { "predicate" }
| TYPE      { "type" }
| MODEL     { "model" }
| AXIOM     { "axiom" }
| VARIANT   { "variant" }
| AXIOMATIC { "axiomatic" }
| MODULE    { "module" }
| IMPORT    { "import" }
;

is_acsl_typename:
| INTEGER  { "integer" (* token that can be used in C fields *) }
| REAL     { "real" (* token that can be used in C fields *) }
;

is_ext_spec:
| EXT_SPEC_MODULE   { "module" }
| EXT_SPEC_FUNCTION { "function" }
| EXT_SPEC_CONTRACT { "contract" }
| EXT_SPEC_INCLUDE  { "include" }
| EXT_SPEC_AT       { "at" }
| EXT_SPEC_LET      { "let" }
;

keyword:
| LOGIC   { "logic" }
| non_logic_keyword { $1 }
;

non_logic_keyword:
| c_keyword      { $1 }
| acsl_c_keyword { $1 }
| is_ext_spec    { $1 }
| is_acsl_spec   { $1 }
| is_acsl_decl_or_code_annot { $1 }
;

bs_keyword:
| ALLOCABLE { () }
| ALLOCATION { () }
| AUTOMATIC { () }
| AT { () }
| AS { () }
| BASE_ADDR { () }
| BLOCK_LENGTH { () }
| DYNAMIC { () }
| EMPTY { () }
| FALSE { () }
| FORALL { () }
| FREEABLE { () }
| FRESH { () }
| FROM { () }
| GHOST { () }
| INTER { () }
| LAMBDA { () }
| LET { () }
| NOTHING { () }
| NULL { () }
| OLD { () }
| OFFSET { () }
| REGISTER { () }
| RESULT { () }
| SEPARATED { () }
| TRUE { () }
| BSTYPE { () }
| TYPEOF { () }
| BSUNION { () }
| UNALLOCATED { () }
| OBJECT_POINTER { () }
| VALID { () }
| VALID_INDEX { () }
| VALID_RANGE { () }
| VALID_READ { () }
| VALID_FUNCTION { () }
| INITIALIZED { () }
| DANGLING { () }
| ALIGNED { () }
| WITH { () }
;

wildcard:
| any_identifier { () }
| LONGIDENT { () }
| bs_keyword { () }
| AMP { () }
| AND { () }
| ARROW { () }
| BIFF { () }
| BIMPLIES { () }
| COLON { () }
| COLON2 { () }
| COLONCOLON { () }
| COMMA { () }
| INT_CONSTANT { () }
| FLOAT_CONSTANT { () }
| STRING_CONSTANT { () }
| WSTRING_CONSTANT { () }
| DOLLAR { () }
| DOT { () }
| DOTDOT { () }
| DOTDOTDOT { () }
| EQ { () }
| EQUAL { () }
| EXISTS { () }
| GE { () }
| GT { () }
| GTGT { () }
| HAT { () }
| HATHAT { () }
| IFF { () }
| IMPLIES { () }
| LBRACE { () }
| LE { () }
| LPAR { () }
| LSQUARE { () }
| LSQUAREPIPE { () }
| LT { () }
| LTLT { () }
| MINUS { () }
| NE { () }
| NOT { () }
| OR { () }
| PERCENT { () }
| PI { () }
| PIPE { () }
| PLUS { () }
| QUESTION { () }
| RBRACE { () }
| RPAR { () }
| RSQUARE { () }
| RSQUAREPIPE { () }
| SEMICOLON { () }
| SLASH { () }
| STAR { () }
| STARHAT { () }
| STRING_LITERAL { () }
| TILDE { () }
| IN { () }
;

any:
| wildcard { () }
| wildcard any { () }
;

%%
