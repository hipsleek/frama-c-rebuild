(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Modified by TrustInSoft *)

open Cil_types
open Cil_datatype
open Printer_api
open Format

let string_of_assert = function
  | Assert -> "assert"
  | Check -> "check"
  | Admit -> "admit"

let name_of_assert = function
  | Assert -> "assertion"
  | Check -> "check"
  | Admit -> "admit"

let string_of_lemma = function
  | Assert -> "lemma"
  | Check -> "check lemma"
  | Admit -> "axiom"

let ident_of_lemma = function
  | Assert -> "lemma"
  | Check -> "check_lemma"
  | Admit -> "axiom"

let string_of_predicate ~kw = function
  | Assert -> kw
  | Check -> "check " ^ kw
  | Admit -> "admit " ^ kw

let ident_of_predicate ~kw = function
  | Assert -> kw
  | Check -> "check_" ^ kw
  | Admit -> "admit_" ^ kw

let pp_assert_kind fmt kd = Format.pp_print_string fmt (string_of_assert kd)
let pp_lemma_kind fmt kd = Format.pp_print_string fmt (string_of_lemma kd)
let pp_predicate_kind ~kw fmt kd =
  match kd with
  | Assert -> Format.pp_print_string fmt kw
  | Check ->
    begin
      Format.pp_print_string fmt "check " ;
      Format.pp_print_string fmt kw ;
    end
  | Admit ->
    begin
      Format.pp_print_string fmt "admit " ;
      Format.pp_print_string fmt kw ;
    end


module Extensions = struct
  let initialized = ref false
  let ref_print = ref (fun ~plugin:_ _ _ _ _ -> assert false)
  let ref_short_print = ref (fun ~plugin:_ _ _ _ _ -> assert false)

  let set_handler ~print ~short_print =
    assert (not !initialized) ;
    ref_print := print ;
    ref_short_print := short_print ;
    initialized := true ;
    ()

  let pp printer fmt {ext_name; ext_kind; ext_plugin} =
    !ref_print ~plugin:ext_plugin ext_name printer fmt ext_kind

  let pp_short printer fmt {ext_name; ext_kind; ext_plugin} =
    !ref_short_print ~plugin:ext_plugin ext_name printer fmt ext_kind

end
let set_extension_handler = Extensions.set_handler

(* for specific builtin functions that act as placeholder for C macros.
   For each name f below, pretty-printer will replace f and &f with the
   corresponding name. Be sure to keep the list in sync with share/libc.
*)
let rename_builtins = Datatype.String.Hashtbl.create 17

let () =
  List.iter (fun (x,y) -> Datatype.String.Hashtbl.add rename_builtins x y)
    [
      "__fc_sig_dfl", "SIG_DFL";
      "__fc_sig_ign", "SIG_IGN";
      "__fc_sig_err", "SIG_ERR";
    ]

let filter_printing_attributes l =
  if Kernel.(is_debug_key_enabled dkey_print_attrs) then l
  else List.filter (fun (name, _) -> Ast_attributes.should_print name) l

let needs_quote =
  let regex = Str.regexp "^[A-Za-z0-9_]+$" in
  fun s -> not (Str.string_match regex s 0)

let print_as_source source =
  Kernel.Debug.get () = 0
  &&
  (Kernel.BigIntsHex.is_default ()
   || not (Str.string_match (Str.regexp "^-?[0-9]+$") source 0))

let print_global g =
  (* This function decides whether to hide functions in Frama-C's libc. *)
  let attrs = Cil_datatype.Global.attr g in
  let printable =
    not (Ast_attributes.(contains fc_stdlib attrs))
    || Kernel.PrintLibc.get()
  in
  let print_var v =
    not (Cil_builtins.is_unused_builtin v) ||
    Kernel.is_debug_key_enabled Kernel.dkey_print_builtins
  in
  match g with
  | GVar (vi,_,_) | GFun ({svar = vi},_) | GVarDecl (vi,_) | GFunDecl (_,vi,_)->
    print_var vi && printable
  | _ -> printable

let print_std_includes fmt globs =
  if not (Kernel.PrintLibc.get ()) then begin
    let extract_file acc = function
      | AStr s ->
        (* These strings are filepaths, so we normalize and prettify them *)
        let s = Filepath.(to_string (of_string s)) in
        Datatype.String.Set.add s acc
      | _ -> Kernel.warning "Unexpected attribute parameter for fc_stdlib"; acc
    in
    let add_file acc g =
      let attrs = Cil_datatype.Global.attr g in
      Ast_attributes.(find_params fc_stdlib attrs)
      |> List.fold_left extract_file acc
    in
    let includes = List.fold_left add_file Datatype.String.Set.empty globs in
    let print_one_include s = Format.fprintf fmt "#include \"%s\"@." s in
    Datatype.String.Set.iter print_one_include includes
  end

let pretty_C_constant suffix k fmt i =
  let nb_signed_bits =
    Z.pred (Z.of_int (8 * (Cil.bytesSizeOfInt k)))
  in
  let max_strict_signed = Z.two_power nb_signed_bits in
  let most_neg = Z.neg max_strict_signed in
  if Z.equal most_neg i then
    (* sm: quirk here: if you print -2147483648 then this is two
       tokens in C, and the second one is too large to represent in
       a signed int..
       so we do what's done in limits.h, and print (-2147483467-1); *)
    (* in gcc this avoids a warning, but it might avoid a real
       problem on another compiler or a 64-bit architecture *)
    Format.fprintf fmt "(-%a%s-1%s)"
      Z.pretty (Z.pred max_strict_signed) suffix suffix
  else
    Format.fprintf fmt "%a%s" Z.pretty i suffix

let pred_body = function
  | LBpred a -> a
  | LBnone
  | LBreads _
  | LBinductive _
  | LBterm _ -> Kernel.fatal "definition expected in Cil.pred_body"

let state =
  { line_directive_style = None;
    print_cil_input = false;
    print_cil_as_is = false;
    line_length = 80;
    warn_truncate = true }

(* Parentheses/precedence level. An expression "a op b" is printed
   parenthesized if its parentheses level is >= that that of its context.
   Identifiers have the lowest level and weakly binding operators (e.g. |)
   have the largest level. The correctness criterion is that a smaller level
   MUST correspond to a stronger precedence!
   These levels must be coherent with the precedence used in file
   [src/kernel_internals/parsing/logic_parser.mly].
*)
module Precedence = struct

  let upperLevel = 110          (* Occurrence order in [logic_parser.mly]:
                                   1 [%right prec_named]
                                   2 [%nonassoc TYPENAME] *)

  let binderLevel = 100          (* 3 [%nonassoc prec_forall prec_exists
                                                prec_lambda LET] *)

  let questionLevel = 90 (* 4 [%right QUESTION prec_question] *)

  let iff_level = 89            (* 5 [%left IFF] *)
  let implies_level = 87 (* and +1 for positive side
                                   6 [%right IMPLIES] *)

  (* Be careful if you change the relative order of these 3 levels *)
  let or_level = 85             (* 7 [%left OR] *)
  let xor_level = 84            (* 8 [%left HATHAT] *)
  let and_level = 83            (* 9 [%left AND] *)

  let assoc_connector_level x =
    and_level <= x && x <= or_level

  let bitwiseLevel = 75        (* 10 [%left BIFF]
                                  11 [%right BIMPLIES]
                                  12 [%left PIPE]
                                  13 [%left HAT]
                                  14 [%left STARHAT] (related to \repeat)
                                  15 [%left AMP] *)

  let belongLevel = 72         (* 16 [%nonassoc IN] *)

  let comparativeLevel = 70    (* 17 [%left LT] *)
  let additiveLevel = 60       (* 18 [%left LTLT GTGT]
                                  19 [%left PLUS MINUS] *)
  let multiplicativeLevel = 40 (* 20 [%left STAR SLASH PERCENT] *)

  let unaryLevel = 30          (* 21 [%right prec_cast TILDE NOT prec_unary_op] *)
  let addrOfLevel = 30         (* 21 [%right prec_cast TILDE NOT prec_unary_op] *)
  let memOffset_level = 20     (* 22 [%left DOT ARROW LSQUARE] *)
  let derefStarLevel = 20      (* 22 [%left DOT ARROW LSQUARE] *)
  let indexLevel = 20          (* 22 [%left DOT ARROW LSQUARE] *)
  let arrowLevel = 20          (* 22 [%left DOT ARROW LSQUARE] *)
  let sizeOfLevel = 20         (* -  [%token] *)
  let alignOfLevel = 20        (* -  [%token] *)

  let applicationLevel = 10    (* -  [%token] *)

  (* is this predicate the encoding of [\in]? If so, return its arguments. *)
  let subset_is_backslash_in p = match p with
    | Papp ({l_var_info = {lv_name = "\\subset"}},
            [],
            [{term_node = Tunion [tl]}; tr]) when not state.print_cil_as_is ->
      Some (tl, tr)
    | _ -> None

  let getParenthLevelPred = function
    | Pfalse
    | Ptrue
    | Pallocable _
    | Pfreeable _
    | Pvalid _
    | Pvalid_read _
    | Pobject_pointer _
    | Pvalid_function _
    | Pinitialized _
    | Pdangling _
    | Paligned _
    | Pseparated _
    | Pat _
    | Pfresh _ -> 0
    | Plet _
    | Pforall _
    | Pexists _ -> binderLevel
    | Pif _ -> questionLevel
    | Piff _ -> iff_level
    | Pimplies _ -> implies_level (* and +1 for positive side *)
    | Por _ -> or_level
    | Pxor _ -> xor_level
    | Pand _ -> and_level

    | Papp _ as p ->
      if subset_is_backslash_in p = None then
        applicationLevel
      else belongLevel
    | Prel _ -> comparativeLevel
    | Pnot _ -> unaryLevel

  let compareLevel x y =
    if assoc_connector_level x && assoc_connector_level y then 0
    else compare x y

  let needParens thisLevel contextprec =
    let c = compareLevel thisLevel contextprec in
    if c != 0
    then c > 0
    else
      not (thisLevel == binderLevel ||
           thisLevel == iff_level (* Piff *) ||
           (assoc_connector_level thisLevel && thisLevel == contextprec
            && not state.print_cil_as_is))

  let getParenthLevel e = match e.enode with
    | BinOp(LAnd, _,_,_) -> and_level
    | BinOp(LOr, _,_,_) -> or_level
    (* Bit operations. *)
    | BinOp((BOr|BXor|BAnd),_,_,_) -> bitwiseLevel
    (* Comparisons *)
    | BinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_) -> comparativeLevel
    (* Additive. Shifts can have higher level than + or - but I want parentheses
       around them *)
    | BinOp((MinusA|MinusPP|MinusPI|PlusA|
             PlusPI|Shiftlt|Shiftrt),_,_,_) -> additiveLevel
    (* Multiplicative *)
    | BinOp((Div|Mod|Mult),_,_,_) -> multiplicativeLevel
    (* Unary *)
    | StartOf((Var v,_)) when Ast_info.is_string_literal v -> 0
    | CastE(_,_)
    | AddrOf(_)
    | StartOf(_)
    | UnOp((Neg|BNot|LNot),_,_) -> unaryLevel
    (* Lvals *)
    | Lval(Mem _ , _) -> derefStarLevel
    | Lval(Var _, (Field _|Index _)) -> indexLevel
    | SizeOf _ | SizeOfE _ -> sizeOfLevel
    | AlignOf _ | AlignOfE _ -> alignOfLevel
    | Lval(Var _, NoOffset) -> 0        (* Plain variables *)
    | Const _ -> 0                        (* Constants *)

  let rec getParenthLevelLogic = function
    | Tlambda _ | Tlet _ -> binderLevel
    | Tif (_, _, _)  -> questionLevel
    | TBinOp(LOr, _,_) -> or_level
    | TBinOp(LAnd, _,_) -> and_level
    (* Bit operations. *)
    | TBinOp((BOr|BXor|BAnd),_,_) -> bitwiseLevel
    | Tapp({ l_var_info },[],[_;_])
      when l_var_info.lv_name = "\\repeat" || l_var_info.lv_name = "\\concat" ->
      bitwiseLevel
    (* Comparisons *)
    | TBinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_) -> comparativeLevel
    (* Additive. Shifts can have higher level than + or - but I want parentheses
       around them *)
    | TBinOp((MinusA|MinusPP|MinusPI|PlusA|
              PlusPI|Shiftlt|Shiftrt),_,_) -> additiveLevel
    (* Multiplicative *)
    | TBinOp((Div|Mod|Mult),_,_) -> multiplicativeLevel
    (* Unary *)
    | TCast(false,_,_)
    | TAddrOf(_)
    | TStartOf(_)
    | TUnOp((Neg|BNot|LNot),_) -> unaryLevel
    (* Lvals *)
    | TLval(TMem _ , _) -> derefStarLevel
    | TLval(TVar _, (TField _|TIndex _|TModel _)) -> indexLevel
    | TLval(TResult _,(TField _|TIndex _|TModel _)) -> indexLevel
    | TSizeOf _ | TSizeOfE _ -> sizeOfLevel
    | TAlignOf _ | TAlignOfE _ -> alignOfLevel
    (* VP: I'm not sure I understand why sizeof(x) and f(x) should
       have a separated treatment wrt parentheses. *)
    (* application and applications-like constructions *)
    | Tapp (_, _,_)|TDataCons _
    | Tblock_length _ | Tbase_addr _ | Toffset _ | Tat (_, _)
    | Tunion _ | Tinter _
    | TUpdate _ | Ttypeof _ | Ttype _ -> applicationLevel
    | TCast (true,_,e) -> (getParenthLevelLogic e.term_node) + 1
    | TLval(TVar _, TNoOffset) -> 0        (* Plain variables *)
    (* Trange always requires parenthesis, except inside indexes. These cases
       are handled in [term_node] and [term_offset] functions. *)
    | Trange _ -> 0
    (* Constructions that do not require parentheses *)
    | TConst _
    | Tnull | TLval (TResult _,TNoOffset) | Tcomprehension _  | Tempty_set -> 0

  (* Create an expression of the same shape, and use {!getParenthLevel} *)
  let getParenthLevelAttrParam = function
    | ACons ("__fc_assign", [_;_]) -> upperLevel
    | ACons ("__fc_float", [_]) -> 0
    | AInt _ | AStr _ | ACons _ -> 0
    | ASizeOf _ | ASizeOfE _ -> sizeOfLevel
    | AAlignOf _ | AAlignOfE _ -> alignOfLevel
    | AUnOp (uo, _) ->
      getParenthLevel
        (Cil.dummy_exp
           (UnOp(uo, Cil.zero ~loc:Fileloc.unknown, Cil_const.intType)))
    | ABinOp (bo, _, _) ->
      getParenthLevel
        (Cil.dummy_exp(BinOp(bo,
                             Cil.zero ~loc:Fileloc.unknown,
                             Cil.zero ~loc:Fileloc.unknown,
                             Cil_const.intType)))
    | AAddrOf _ -> addrOfLevel
    | ADot _ | AIndex _ | AStar _ -> memOffset_level
    | AQuestion _ -> questionLevel

  let needIndent current pred fmt =
    let nextLevel = getParenthLevelPred pred.pred_content in
    let need = not (current == binderLevel && nextLevel == binderLevel) in
    if need then begin
      pp_open_box fmt 2;
      kfprintf (fun fmt -> pp_close_box fmt ()) fmt
    end
    else
      fprintf fmt


end

let get_termination_kind_name = function
  | Normal -> "ensures" | Exits -> "exits" | Breaks -> "breaks"
  | Continues -> "continues" | Returns -> "returns"

let rec get_pand_list pred l =
  match pred.pred_content with
  | Pand(p1,p2) -> get_pand_list p1 (p2::l)
  | _ -> pred::l

let rec get_tand_list term l =
  match term.term_node with
  | TBinOp(LAnd,t1,t2) -> get_tand_list t1 (t2::l)
  | _ -> term::l

let is_compatible_rel_binop op1 op2 =
  match op1, op2 with
  | (Lt | Le | Eq), (Lt | Le | Eq) -> true
  | (Gt | Ge | Eq), (Gt | Ge | Eq) -> true
  | _ -> false

let is_compatible_relation op1 op2 =
  match op1, op2 with
  | (Rlt | Rle | Req), (Rlt | Rle | Req) -> true
  | (Rgt | Rge | Req), (Rgt | Rge | Req) -> true
  | _ -> false

type direction = Nothing | Less | Greater | Both

let update_direction_binop dir op =
  match dir, op with
  | _, Eq -> dir
  | (Both | Less), (Lt | Le) -> Less
  | (Both | Greater), (Gt | Ge) -> Greater
  | _ -> Nothing

let update_direction_rel dir op =
  match dir, op with
  | _, Req -> dir
  | (Both | Less), (Rlt | Rle) -> Less
  | (Both | Greater), (Rgt | Rge) -> Greater
  | _ -> Nothing

let is_same_direction_binop dir op =
  update_direction_binop dir op <> Nothing

let is_same_direction_rel dir op =
  update_direction_rel dir op <> Nothing

let remove_no_op_coerce t =
  match t.term_node with
  | TCast (true, ty,t) when Cil.no_op_coerce ty t -> t
  | _ -> t

let rec is_singleton t =
  match t.term_node with
  | TCast (true, Ltype ({ lt_name = "set"},_), t') -> is_singleton t'
  | _ -> not (Logic_const.is_set_type t.term_type)

(* when pretty-printing relation chains, a < b && b' < c, it can happen that
   b has a coercion and b' hasn't or vice-versa (bc c is an integer and a and
   b are ints for instance). We nevertheless want to
   pretty-print that as a < b < c. For that, we compare b and b' after having
   removed any existing head coercion.
*)
let equal_mod_coercion t1 t2 =
  Cil_datatype.Term.equal (remove_no_op_coerce t1) (remove_no_op_coerce t2)

(* Grab one of the labels of a statement *)
let rec pickLabel = function
  | [] -> None
  | Label (lbl, _, _) :: _ -> Some lbl
  | _ :: rest -> pickLabel rest

let extract_acsl_list t =
  let rec aux acc t =
    match t.term_node with
    | TDataCons ({ ctor_name }, []) when ctor_name = "\\Nil" ->
      Some (List.rev acc)
    | TDataCons ({ ctor_name }, [hd; tl]) when ctor_name = "\\Cons" ->
      aux (hd :: acc) tl
    | _ -> None
  in
  aux [] t

let is_cfg_block =
  function Stmt_block _ -> false | Then_with_else | Other | Body -> true

let rec has_unprotected_local_init s =
  match s.skind with
  | Instr (Local_init _) -> true
  | UnspecifiedSequence((s,_,_,_,_) :: _) -> has_unprotected_local_init s
  | Block { bscoping = false; bstmts = s :: _ } -> has_unprotected_local_init s
  | _ -> false

(** Annotation context in which the Printer is *)
type annot_ctxt =
  | Not_in_annot     (** not in annotation *)
  | In_simple_annot  (** in /*@ ... */ annotation *)
  | In_nested_annot  (** in /@ ... @/ annotation *)

let pp_wstring fmt s =
  Format.fprintf fmt "L\"%s\"" (Escape.escape_wstring s)

class cil_printer () = object (self)

  val mutable logic_printer_enabled = true

  val mutable verbose = false
  (* Do not add a value that depends on a
     non-constant variable of the kernel here (e.g. [Kernel.Debug.get ()]). Due
     to the way the pretty-printing class is instantiated, this value would be
     evaluated too soon. Override the [reset] method instead. *)

  method reset () =
    verbose <- Kernel.debug_atleast 1;
    state.print_cil_as_is <- Kernel.debug_atleast 1 || Kernel.PrintAsIs.get()

  method pp_keyword fmt s = pp_print_string fmt s
  method pp_acsl_keyword = self#pp_keyword

  val mutable annot_ctxt = Not_in_annot

  method pp_open_annotation ?(block=true) ?(pre=format_of_string "/*@@") fmt =
    let pre = match annot_ctxt with
      | Not_in_annot -> annot_ctxt <- In_simple_annot ; pre
      | In_simple_annot -> annot_ctxt <- In_nested_annot ; format_of_string "/@@"
      | _ -> assert false (* cannot enter an annotation in a internal annotation *)
    in
    (if block then Pretty_utils.pp_open_block else Format.fprintf)
      fmt "%(%)" pre
  method pp_close_annotation ?(block=true) ?(suf=format_of_string "*/") fmt =
    let suf = match annot_ctxt with
      | In_nested_annot -> annot_ctxt <- In_simple_annot ; format_of_string "@@/"
      | In_simple_annot -> annot_ctxt <- Not_in_annot ; suf
      | _ -> assert false (* we should not have to close an annotation that is not open *)
    in
    (if block then Pretty_utils.pp_close_block else Format.fprintf)
      fmt "%(%)" suf

  (* indicates whether we are printing ghost elements *)
  val mutable is_ghost = false
  method private display_comment () = not is_ghost || verbose

  method private in_ghost_if_needed fmt ghost_flag
      ?(break_ghost=true) ~post_fmt ?block do_it =
    let display_ghost = ghost_flag && not is_ghost in
    if display_ghost then begin
      is_ghost <- true ;
      Format.fprintf fmt "%t " (fun fmt -> self#pp_open_annotation ?block fmt) ;
      Format.fprintf fmt
        (if break_ghost then "%a@ " else "%a ") self#pp_acsl_keyword "ghost"
    end ;
    do_it () ;
    if display_ghost then begin
      is_ghost <- false;
      Format.fprintf fmt post_fmt
        (fun fmt -> self#pp_close_annotation ?block fmt)
    end

  method without_annot:
    'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit =
    fun f fmt x ->
    let tmp = logic_printer_enabled in
    logic_printer_enabled <- false;
    let finally () = logic_printer_enabled <- tmp in
    Fun.protect ~finally (fun () -> f fmt x);

  val mutable force_brace = false

  method force_brace:
    'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit =
    fun f fmt x ->
    let tmp = force_brace in
    force_brace <- true;
    let finally () = force_brace <- tmp in
    Fun.protect ~finally (fun () -> f fmt x);

  val current_stmt = Stack.create ()

  val mutable current_function = None
  method private current_function = current_function

  method private in_current_function vi =
    assert (current_function = None);
    current_function <- Some vi

  method private out_current_function =
    assert (current_function <> None);
    current_function <- None

  val mutable current_behavior = None
  method private current_behavior = current_behavior

  method private set_current_behavior b =
    assert (current_behavior = None);
    current_behavior <- Some b

  method private reset_current_behavior () =
    assert (current_behavior <> None);
    current_behavior <- None

  val mutable has_annot = false
  method private has_annot = has_annot && logic_printer_enabled

  method private stmt_has_annot _ = false

  method private push_stmt s = Stack.push s current_stmt
  method private pop_stmt s =
    ignore (Stack.pop current_stmt);
    has_annot <- false;
    s

  method private current_stmt =
    try Some (Stack.top current_stmt) with Stack.Empty -> None

  method private may_be_skipped s = s.labels = [] && s.sattr = []

  (* constant *)
  method constant fmt = function
    | CInt64(_, _, Some s) when print_as_source s ->
      fprintf fmt "%s" s (* Always print the text if there is one, unless
                            we want to print it as hexa *)
    | CInt64(i, ik, _) ->
      (*fprintf fmt "/* %Lx */" i;*)
      (* We must make sure to capture the type of the constant. For some
          constants this is done with a suffix, for others with a cast
          prefix.*)
      let suffix = match ik with
        | IUInt -> "U"
        | ILong -> "L"
        | IULong -> "UL"
        | ILongLong | IInt128 -> if Machine.msvcMode () then "L" else "LL"
        | IULongLong | IUInt128 -> if Machine.msvcMode () then "UL" else "ULL"
        | IInt | IBool | IShort | IUShort | IChar | ISChar | IUChar -> ""
      in
      let prefix =
        if suffix <> "" then ""
        else if ik = IInt then ""
        else Format.asprintf "(%a)" self#ikind ik
      in
      fprintf fmt "%s%a" prefix (pretty_C_constant suffix ik) i

    | CChr(c) -> fprintf fmt "'%s'" (Escape.escape_char c)
    | CReal(_, _, Some s) -> fprintf fmt "%s" s
    | CReal(f, fsize, None) ->
      fprintf fmt "%a%s"
        Floating_point.pretty f
        (match fsize with
         | FFloat   -> "f"
         | FFloat32 -> "f32"
         | FFloat64 -> "f64"
         | FDouble  -> ""
         | FLongDouble -> "L")
    | CEnum {einame = s} -> self#varname fmt s

  (*** VARIABLES ***)
  method varname fmt v =
    let v =
      if not state.print_cil_as_is &&
         Datatype.String.Hashtbl.mem rename_builtins v
      then
        Datatype.String.Hashtbl.find rename_builtins v
      else v
    in
    pp_print_string fmt v

  (* variable use *)
  method varinfo fmt v =
    if Kernel.is_debug_key_enabled Kernel.dkey_print_vid then begin
      Format.fprintf fmt "/* vid:%d" v.vid;
      (match v.vlogic_var_assoc with
         None -> ()
       | Some v -> Format.fprintf fmt ", lvid:%d" v.lv_id
      );
      Format.fprintf fmt " */"
    end;
    self#varname fmt v.vname

  method private no_ghost_at_first_level t =
    match t.tnode with
    | TArray (bt, e) ->
      let bt' = Ast_types.remove_attributes [ "ghost" ] bt in
      let tattr = Ast_attributes.drop "ghost" t.tattr in
      Cil_const.mk_tarray ~tattr bt' e
    | _ -> Ast_types.remove_attributes [ "ghost" ] t

  (* variable declaration *)
  method vdecl fmt (v:varinfo) =
    let stom, rest = Ast_attributes.split_storage_modifiers v.vattr in
    (* Small hack to keep printing noreturn attribute before function type. *)
    let noreturn_attrs =
      Ast_attributes.filter "noreturn" v.vtype.tattr
    in
    let stom_noreturn = stom @ noreturn_attrs in
    let vtype_no_noreturn = Ast_types.remove_attributes ["noreturn"] v.vtype in
    let fundecl = if Ast_types.is_fun v.vtype then Some v else None in
    let v = { v with vtype = self#no_ghost_at_first_level vtype_no_noreturn } in
    let v =
      if v.vformal && not state.print_cil_as_is then begin
        let tattr = v.vtype.tattr in
        match v.vtype.tnode with
        | TPtr t when Ast_attributes.contains "arraylen" tattr ->
          (* We make sure qualifiers are not pushed to its elements here. *)
          let t' = Cil_const.mk_tarray ~push_qualifiers:false ~tattr t None in
          { v with vtype = t'}
        | _ -> v
      end
      else v
    in
    let name =
      if Ast_attributes.(contains anonymous_attribute_name v.vattr)
      && not v.vreferenced && not state.print_cil_as_is
      then
        None
      else Some (fun fmt -> self#varinfo fmt v)
    in
    (* First the storage modifiers *)
    fprintf fmt "%s%a%a%s%a%a"
      (if v.vinline then "__inline " else "")
      self#storage v.vstorage
      self#attributes stom_noreturn
      (if stom_noreturn = [] then "" else " ")
      (self#typ ?fundecl ?alignas:v.valignas name) v.vtype
      self#attributes rest

  method lhost fmt = function
    | Var vi -> self#varinfo fmt vi
    | Mem e ->
      fprintf fmt "*%a" (self#exp_prec Precedence.derefStarLevel) e

  (*** L-VALUES ***)
  method lval fmt (lv:lval) =  (* lval (base is 1st field)  *)
    match lv with
      Var vi, o -> fprintf fmt "%a%a" self#varinfo vi self#offset o
    | Mem e, Field(fi, o) ->
      fprintf fmt "%a->%a%a"
        (self#exp_prec Precedence.arrowLevel)  e
        self#varname fi.fname
        self#offset o
    | Mem e, NoOffset ->
      fprintf fmt "*%a"
        (self#exp_prec Precedence.derefStarLevel) e
    | Mem e, o ->
      fprintf fmt "(*%a)%a"
        (self#exp_prec Precedence.derefStarLevel) e
        self#offset o

  (** Offsets **)

  method field fmt fi = self#varname fmt fi.fname

  method offset fmt = function
    | NoOffset -> ()
    | Field (fi, o) ->
      fprintf fmt ".%a%a"
        self#field fi
        self#offset o
    | Index (e, o) ->
      fprintf fmt "[%a]%a"
        self#exp e
        self#offset o

  method private lval_prec (contextprec: int) fmt lv =
    if Precedence.getParenthLevel (Cil.dummy_exp(Lval(lv))) >= contextprec then
      fprintf fmt "(%a)" self#lval lv
    else
      self#lval fmt lv

  (* used to check whether StartOf x can be printed as x
     or must be rendered as &x[0]. *)
  val mutable parent_non_decay = false

  (*** EXPRESSIONS ***)
  method exp fmt (e: exp) =
    let non_decay = parent_non_decay in
    parent_non_decay <- false;
    let level = Precedence.getParenthLevel e in
    (* fprintf fmt "/* eid:%d */" e.eid; *)
    let alignof_kw = function
      | `Standard -> "_Alignof"
      | `GCC -> "__alignof__"
    in
    let debug = Kernel.is_debug_key_enabled Kernel.dkey_print_c_types in
    if debug then fprintf fmt "/*(type:%a */" (self#typ None) (Cil.typeOf e);
    let () =
      match e.enode with
      | Const(c) -> self#constant fmt c
      | Lval(l) -> self#lval fmt l

      | UnOp(u,e1,_) ->
        (match u, e1 with
         | Neg, {enode = Const (CInt64 (v, _, _))}
           when Z.geq v Z.zero ->
           fprintf fmt "-%a" (self#exp_prec level) e1
         | _ ->
           fprintf fmt "%a %a" self#unop u (self#exp_prec level) e1)

      | BinOp(b,e1,e2,_) ->
        fprintf fmt "@[%a %a %a@]"
          (self#exp_prec level) e1
          self#binop b
          (self#exp_prec level) e2

      | CastE(t,e) ->
        fprintf fmt "(%a)%a" (self#typ None) t (self#exp_prec level) e
      | SizeOf t ->
        fprintf fmt "%a(%a)"
          self#pp_keyword "sizeof" (self#typ None) t
      | SizeOfE e ->
        fprintf fmt "%a(%a)"
          self#pp_keyword "sizeof" self#exp_non_decay e
      | AlignOf (t, i) ->
        fprintf fmt "%a(%a)" (self#pp_keyword) (alignof_kw i) (self#typ None) t
      | AlignOfE (e, i) ->
        fprintf fmt "%a(%a)" (self#pp_keyword) (alignof_kw i) self#exp_non_decay e
      | AddrOf ((Var v, NoOffset))
        when Datatype.String.Hashtbl.mem rename_builtins v.vname ->
        self#varinfo fmt v
      | AddrOf lv -> fprintf fmt "& %a" (self#lval_prec Precedence.addrOfLevel) lv
      | StartOf(lv) ->
        if state.print_cil_as_is || non_decay then
          fprintf fmt "&(%a[0])" self#lval lv
        else self#lval fmt lv
    in
    if debug then fprintf fmt "/*)*/"

  method private exp_non_decay fmt e = parent_non_decay <- true; self#exp fmt e

  method unop fmt u =
    fprintf fmt "%s"
      (match u with
       | Neg -> "-"
       | BNot -> "~"
       | LNot -> "!")

  method binop fmt b =
    fprintf fmt "%s"
      (match b with
       | PlusA | PlusPI -> "+"
       | MinusA | MinusPP | MinusPI -> "-"
       | Mult -> "*"
       | Div -> "/"
       | Mod -> "%"
       | Shiftlt -> "<<"
       | Shiftrt -> ">>"
       | Lt -> "<"
       | Gt -> ">"
       | Le -> "<="
       | Ge -> ">="
       | Eq -> "=="
       | Ne -> "!="
       | BAnd -> "&"
       | BXor -> "^"
       | BOr -> "|"
       | LAnd -> "&&"
       | LOr -> "||")

  (* Print an expression, given the precedence of the context in which it
   * appears. *)
  method private exp_prec (contextprec: int) fmt (e: exp) =
    let thisLevel = Precedence.getParenthLevel e in
    let needParens =
      if thisLevel >= contextprec then
        true
      else if contextprec == Precedence.bitwiseLevel then
        (* quiet down some GCC warnings *)
        thisLevel == Precedence.additiveLevel
        || thisLevel == Precedence.comparativeLevel
      else
        false
    in
    if needParens then fprintf fmt "(%a)" self#exp e else self#exp fmt e

  method init fmt = function
    | SingleInit e -> self#exp fmt e
    | CompoundInit (t, initl) ->
      (* We do not print the type of the Compound *)
    (*
      let dinit e = d_init () e in
      dprintf "{@[%a@]}"
      (docList ~sep:(chr ',' ++ break) dinit) initl
     *)
      let designated_init fmt = function
        | Field(f, NoOffset), i ->
          fprintf fmt ".%a = " self#varname f.fname;
          self#init fmt i
        | Index(e, NoOffset), i ->
          fprintf fmt "[%a] = " self#exp e;
          self#init fmt i
        | _ -> Kernel.fatal "Trying to print malformed initializer"
      in
      if not (Ast_types.is_array t) then
        Pretty_utils.pp_list ~pre:"{@[<hv>" ~sep:",@ " ~suf:"@]}" ~empty:"{}"
          designated_init fmt initl
      else begin
        let print_index prev_index (designator,init as di) =
          let curr_index =
            match designator with
            | Index(e,NoOffset) -> Cil.constFoldToInt ~machdep:false e
            | _ -> None
          in
          let designator_needed =
            match prev_index, curr_index with
            | None, _ | _, None -> true
            | Some p, Some c -> not (Z.equal (Z.succ p) c)
          in
          if designator_needed then designated_init fmt di
          else self#init fmt init;
          curr_index
        in
        let print_next_index prev_index di =
          Format.fprintf fmt ",@ "; print_index prev_index di
        in
        Format.fprintf fmt "{@[<hv>";
        (match initl with
         | [] -> ()
         | i::tl ->
           let curr_index = print_index (Some Z.minus_one) i in
           ignore (List.fold_left print_next_index curr_index tl));
        Format.fprintf fmt "@]}"
      end

  method str_literal fmt = function
    | Str s -> Format.fprintf fmt "\"%s\"" (Escape.escape_string s)
    | Wstr l -> pp_wstring fmt l

  method init_or_str fmt i =
    match i with
    | CInit i -> self#init fmt i
    | StrInit lit -> self#str_literal fmt lit

  (** What terminator to print after an instruction. sometimes we want to
      print sequences of instructions separated by comma *)
  val mutable instr_terminator = ";"

  method private set_instr_terminator (term : string) =
    instr_terminator <- term

  method private get_instr_terminator () = instr_terminator

  (*** INSTRUCTIONS ****)
  method instr fmt (i:instr) = (* imperative instruction *)
    fprintf fmt "%a"
      (self#line_directive ~forcefile:false) (Cil_datatype.Instr.loc i);
    let pp_call dest f fmt args =
      (match dest with
       | None -> ()
       | Some lv ->
         fprintf fmt "%a = " self#lval lv;
         (* Maybe we need to print a cast *)
         (let destt = Cil.typeOfLval lv in
          match Ast_types.unroll_node (Cil.typeOfLhost f) with
          | TFun(rt, _, _) when (Cil.need_cast rt destt) ->
            fprintf fmt "(%a)" (self#typ None) destt
          | _ -> ()));
      (* Now the function name. Build an lvalue and print it using self#lval.
         This is necessary for Printer_tag to create a localizable (a marker
         in the server) for the function called, allowing users to interact
         with it in the GUI (there is no localizable/marker for lhost only). *)
      let f_lval = f, NoOffset in
      (match f with
       | Var _ -> self#lval fmt f_lval
       | Mem _ -> fprintf fmt "(%a)"  self#lval f_lval);

      let (_, param_ts, _, _) = Cil.splitFunctionType (Cil.typeOfLhost f) in
      let _, g_params_ts = Cil.argsToPairOfLists param_ts in

      let rec break n l =
        if n = 0 then [], l
        else match l with
          | [] -> assert false
          | x :: l' -> let (f, s) = break (n-1) l' in x :: f, s
      in
      let args, ghosts = if is_ghost then
          args, []
        else
          let n = List.length args - List.length g_params_ts in
          break n args
      in

      (* Now the arguments *)
      Format.fprintf fmt "@[" ;
      Pretty_utils.pp_flowlist ~left:"(" ~sep:"," ~right:")" self#exp fmt args;
      (* Now the ghost arguments *)
      begin match ghosts with
        | [] -> ()
        | _ -> Pretty_utils.pp_flowlist
                 ~left:"@;/*@@ ghost (" ~sep:"," ~right:") */"
                 self#exp fmt ghosts
      end ;
      (* Now the terminator *)
      fprintf fmt "@]%s" instr_terminator
    in
    match i with
    | Skip _ -> fprintf fmt ";"
    | Set(lv,e,_) -> begin
        (* Be nice to some special cases *)
        match e.enode with
          BinOp((PlusA|PlusPI),
                {enode = Lval(lv')},
                {enode=Const(CInt64(one,_,_))},_)
          when LvalStructEq.equal lv lv' && Z.is_one one
               && not state.print_cil_as_is ->
          fprintf fmt "%a ++%s"
            (self#lval_prec Precedence.indexLevel) lv
            instr_terminator
        | BinOp((MinusA|MinusPI),
                {enode = Lval(lv')},
                {enode=Const(CInt64(one,_,_))}, _)
          when LvalStructEq.equal lv lv' && Z.is_one one
               && not state.print_cil_as_is ->
          fprintf fmt "%a --%s"
            (self#lval_prec Precedence.indexLevel) lv
            instr_terminator

        | BinOp((PlusA|PlusPI),
                {enode = Lval(lv')},
                {enode = Const(CInt64(mone,_,_))},_)
          when LvalStructEq.equal lv lv' && Z.equal mone Z.minus_one
               && not state.print_cil_as_is ->
          fprintf fmt "%a --%s"
            (self#lval_prec Precedence.indexLevel) lv
            instr_terminator

        | BinOp((PlusA|PlusPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
                 Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
                {enode = Lval(lv')},e,_) when LvalStructEq.equal lv lv' ->
          fprintf fmt "%a %a= %a%s"
            self#lval  lv
            self#binop bop
            self#exp e
            instr_terminator

        | _ ->
          fprintf fmt "%a = %a%s"
            self#lval lv
            self#exp e
            instr_terminator

      end
    | Local_init(vi, AssignInit i, _) ->
      Format.fprintf fmt "@[<2>%a =@ %a%s@]"
        self#vdecl vi self#init i instr_terminator
    | Local_init(vi, ConsInit(f, args, Constructor), _) ->
      let args = Cil.mkAddrOfVi vi :: args in
      Format.fprintf fmt "@[<2>%a%s@]@\n" self#vdecl vi instr_terminator;
      pp_call None (Var f) fmt args
    | Local_init(vi, ConsInit(f, args, Plain_func), _) ->
      Format.fprintf fmt "@[<2>%a =@ %a@]" self#vdecl vi
        (pp_call None (Var f)) args;
      (* In cabs2cil we have turned the call to builtin_va_arg into a
         three-argument call: the last argument is the address of the
         destination *)
    | Call(None, Var vi,
           [dest; {enode = SizeOf t}; adest], (l,_))
      when vi.vname = "__builtin_va_arg"
        && not state.print_cil_as_is ->
      let destlv = match (Cil.stripCasts adest).enode with
          AddrOf destlv -> destlv
        (* If this fails, it's likely that an extension interfered
           with the AddrOf *)
        | _ ->
          Kernel.fatal ~source:l
            "Encountered unexpected call to %s with dest %a"
            vi.vname self#exp adest
      in
      fprintf fmt "%a = __builtin_va_arg (@[%a,@ %a@])%s"
        self#lval destlv
        (* Now the arguments *)
        self#exp dest
        (self#typ None)  t
        instr_terminator

    (* In cabs2cil we have dropped the last argument in the call to
       __builtin_va_start, __builtin_stdarg_start, __builtin_c23_va_start. *)
    | Call(None, Var vi, [marker], l)
      when ((vi.vname = "__builtin_stdarg_start" ||
             vi.vname = "__builtin_va_start" ||
             vi.vname = "__builtin_c23_va_start")
            && not state.print_cil_as_is) ->
      let last = self#getLastNamedArgument () in
      self#instr fmt (Call(None, Var vi, [marker; last], l))

    (* In cabs2cil we have dropped the last argument in the call to
       __builtin_next_arg. *)
    | Call(res, Var vi, [ ], l)
      when vi.vname = "__builtin_next_arg"
        && not state.print_cil_as_is ->
      let last = self#getLastNamedArgument () in
      self#instr fmt (Call(res, Var vi, [last], l))

    (* In cabs2cil we have turned the call to
       __builtin_types_compatible_p(t1, t2) into
       __builtin_types_compatible_p(sizeof t1, sizeof t2), so that we can
       represent the types as expressions.
       Remove the sizeofs when printing. *)
    | Call(dest, Var vi,
           [{enode = SizeOf t1}; {enode = SizeOf t2}], _)
      when vi.vname = "__builtin_types_compatible_p"
        && not state.print_cil_as_is ->
      (* Print the destination *)
      (match dest with
         None -> ()
       | Some lv -> fprintf fmt "%a = " self#lval lv );
      (* Now the call itself *)
      fprintf fmt "%a(%a, %a)%s"
        self#varname vi.vname
        (self#typ None) t1
        (self#typ None) t2
        instr_terminator
    | Call(_, Var vi, _, (l,_))
      when vi.vname = "__builtin_types_compatible_p"
        && not state.print_cil_as_is ->
      Kernel.fatal ~source:l
        "__builtin_types_compatible_p: cabs2cil should have added sizeof to \
         the arguments."

    | Call(dest, Var vi, [ arg ], (l, _))
      when vi.vname = "__builtin_offsetof"
        && not state.print_cil_as_is ->
      begin
        match arg.enode with
        | CastE (_, { enode = AddrOf (host, offset) }) ->
          (* Print the destination *)
          Option.iter (fprintf fmt "%a = " self#lval) dest;
          (* Now the call itself *)
          fprintf fmt "%a(%a, %a)%s"
            self#varname "offsetof"
            (self#typ None) (Cil.typeOfLhost host)
            self#offset offset
            instr_terminator
        | _ -> Kernel.fatal ~source:l "__builtin_offsetof: invalid argument."
      end

    | Call(dest,e,args,_) -> pp_call dest e fmt args

    | Asm(attrs, tmpls, ext_asm, l) ->
      self#line_directive fmt l;
      let goto =
        match ext_asm with Some { asm_gotos = _::_ } -> " goto" | _ -> ""
      in
      if Machine.msvcMode () then
        fprintf fmt "__asm%s {@[%a@]}%s"
          goto
          (Pretty_utils.pp_list ~sep:"@\n"
             (fun fmt s -> fprintf fmt "%s" s)) tmpls
          instr_terminator
      else begin
        fprintf fmt "@[<hv 2>__asm__%s%a (@,@[%a@]"
          goto
          self#attributes attrs
          (Pretty_utils.pp_list ~sep:"@\n" (fun fmt x -> fprintf fmt "%S" x))
          tmpls;
        (match ext_asm with
         | None -> ()
         | Some { asm_outputs; asm_inputs; asm_clobbers; asm_gotos } ->
           let lab_nil = asm_gotos = [] in
           let lab_clob_nil = asm_clobbers = [] && lab_nil in
           let lab_clob_in_nil = asm_inputs = [] && lab_clob_nil in
           (* We must output at least one column to distinguish basic asm
              from extended asm whose components are all empty. Other
              columns will be pretty-printed only if needed, i.e. if there
              is at least one non-empty list still to be printed.
           *)
           Format.fprintf fmt "@;@[: %a@]"
             (Pretty_utils.pp_list ~sep:",@ "
                (fun fmt (idopt, c, lv) ->
                   fprintf fmt "%s%S (%a)"
                     (match idopt with
                      | None -> ""
                      | Some id -> "[" ^ id ^ "] ")
                     c
                     self#lval lv))
             asm_outputs;
           if not lab_clob_in_nil then
             Format.fprintf fmt "@;@[: %a@]"
               (Pretty_utils.pp_list ~sep:",@ "
                  (fun fmt (idopt, c, e) ->
                     fprintf fmt "%s%S (%a)"
                       (match idopt with
                        | None -> ""
                        | Some id -> "[" ^ id ^ "] ")
                       c
                       self#exp e))
               asm_inputs;
           if not lab_clob_nil then
             Format.fprintf fmt "@;@[: %a@]"
               (Pretty_utils.pp_list ~sep:",@ "
                  (fun fmt c -> fprintf fmt "%S" c))
               asm_clobbers;
           (* No need to output a final colon if there's no label. *)
           if not lab_nil then
             Pretty_utils.pp_list ~pre:"@;@[:" ~suf:"@]" ~sep:",@ "
               (fun fmt r ->
                  match pickLabel !r.labels with
                  | Some label -> Format.pp_print_string  fmt label
                  | None ->
                    Kernel.error "Cannot find label for target of asm goto: %a"
                      (self#without_annot self#stmt) !r;
                    Format.pp_print_string fmt "__invalid_label")
               fmt
               asm_gotos);
        fprintf fmt "@,@])%s" instr_terminator
      end
    | Code_annot (annot, l) ->
      has_annot <- true;
      if logic_printer_enabled then begin
        self#line_directive ~forcefile:false fmt l;
        Format.fprintf fmt "%t " (fun fmt -> self#pp_open_annotation fmt);
        self#code_annotation fmt annot ;
        Format.fprintf fmt "@ %t" (fun fmt -> self#pp_close_annotation fmt);
      end

  (** For variadic calls *)
  method private getLastNamedArgument () =
    match self#current_function with
    | None ->
      Kernel.error ~current:true "Current stmt not positioned";
      Cil_datatype.Exp.dummy
    | Some vi ->
      let formals = Cil.getFormalsDecl vi in
      match List.rev formals with
      | [] ->
        (* Typing error, this function should
           have at least one named argument *)
        Kernel.abort ~current:true
          "%s should have at least one named argument"
          vi.vname
      | f :: _ -> Cil.new_exp ~loc:f.vdecl (Lval (Cil.var f))

  (**** STATEMENTS ****)
  method stmt fmt (s:stmt) =        (* control-flow statement *)
    self#push_stmt s;
    self#pop_stmt (self#next_stmt Cil.invalidStmt fmt s)

  method next_stmt (next: stmt) fmt (s: stmt) =
    self#push_stmt s;
    self#pop_stmt (self#annotated_stmt next fmt s)

  method stmt_labels fmt (s:stmt) =
    let suf =
      if has_unprotected_local_init s then format_of_string ";@]@ "
      else format_of_string "@]@ "
    in
    if s.labels <> [] then
      Pretty_utils.pp_list
        ~pre:"@[<hov>" ~sep:"@ " ~suf self#label fmt s.labels

  method label fmt = function
    | Label (s, _, b) when b || not verbose -> fprintf fmt "@[%s:@]" s
    | Label (s, _, _) -> fprintf fmt "@[%s: /* internal */@]" s
    | Case (e, _) -> fprintf fmt "@[%a %a:@]" self#pp_keyword "case" self#exp e
    | Default _ -> fprintf fmt "@[%a:@]" self#pp_keyword "default"

  method builtin_logic_info fmt bli =
    fprintf fmt "%a" self#varname bli.bl_name

  method logic_type_info fmt s = self#logic_name fmt s.lt_name

  method logic_ctor_info fmt ci = self#logic_name fmt ci.ctor_name

  method initinfo fmt io =
    match io.init with
    | None -> fprintf fmt "{}"
    | Some i -> fprintf fmt "%a" self#init_or_str i

  method fundec fmt fd =  fprintf fmt "%a" self#varinfo fd.svar

  method annotated_stmt (next: stmt) fmt (s: stmt) =
    pp_open_hvbox fmt 0;
    if Kernel.is_debug_key_enabled Kernel.dkey_print_sid then begin
      Format.fprintf fmt "/* sid:%d */@\n" s.sid;
    end;
    (* print the statement. *)
    if Cil.is_skip s.skind && not s.ghost && s.sattr = [] then begin
      if verbose || s.labels <> [] then begin
        self#stmt_labels fmt s;
        fprintf fmt ";"
      end
    end else begin
      self#in_ghost_if_needed fmt s.ghost ~post_fmt:"%t"
        (fun () ->
           self#stmt_labels fmt s; self#stmtkind s.sattr next fmt s.skind)
    end;
    pp_close_box fmt ()

  method private require_braces ctxt blk =
    force_brace
    || verbose || Kernel.is_debug_key_enabled Kernel.dkey_print_sid
    (* If one the of condition above is true, /* sid:... */ will be printed
       on its own line before s. Braces are needed *)
    || ctxt = Body (* function body is always between braces. *)
    ||
    (let attrs = filter_printing_attributes blk.battrs in
     match blk.bstmts, attrs, blk.blocals, ctxt with
     | _, _, _ :: _,_ | _, _ :: _, _, _ -> true
     | _::_::_,[],[],Stmt_block s ->
       not (Cil.has_extern_local_init blk) &&
       (self#stmt_has_annot s || s.labels <> [])
     (* Do not put braces around a Local_init statement if we are not
        in the appropriate block. This trumps the presence of a binding
        annotation (or a label at block level, in case of something like:
        { /* start of scoping block */
         //@ slicing pragma stmt;
         /* { */ /* start of non-scoping block
           int x = 42;
           x++;
           ...
         /* } */ /* end of non-scoping block
         x++;
        } /* end of scoping block */
        In such case, the pretty-printer can't satisfy the scope of the
        annotation and the scope of x at the same time. We favor x, which
        gives us at least a correct, compilable, C code.
     *)
     | _::_::_,[],[],_ -> is_cfg_block ctxt
     | [s],[],[], (Then_with_else | Other)
       when (not is_ghost) && s.ghost -> true
     | [ { skind = Block b } as s' ], [], [], Stmt_block s ->
       (b.bscoping ||
        (not (Cil.has_extern_local_init b) && self#stmt_has_annot s))
       && self#require_braces ctxt b
       && not (self#require_braces (Stmt_block s') b)
     (* If b wants braces in current context but not in subcontext, put
        braces directly there. Otherwise, wait for children to do it. *)
     | [ { skind = Block b } ], [], [], _ -> self#require_braces ctxt b
     | [ { skind = UnspecifiedSequence s } ], [], [], _ ->
       self#require_braces ctxt (Cil.block_from_unspecified_sequence s)
     | [_],[],[], Then_with_else -> self#block_has_dangling_else blk
     | [ _ ], [], [], _ -> false
     | [],[],[],_ -> false)

  method private inline_block ctxt blk = match blk.bstmts with
    | [] | [ { skind = (Instr _ | Return _ | Goto _ | Break _ | Continue _ ) } ]
      ->
      not (self#require_braces ctxt blk)
    | [ { skind = Block blk } ] -> self#inline_block ctxt blk
    | _ -> false

  method private block_is_function blk = match blk.bstmts with
    | [ { skind = Instr (Call _) } ] -> true
    | [ { skind = Instr (Local_init (_, ConsInit _, _)) } ] -> true
    (* NB: a block consisting solely of an initializer is pretty useless,
       but who knows? *)
    | [ { skind = Block blk } ] -> self#block_is_function blk
    | _ -> false

  method private block_has_dangling_else blk = match blk.bstmts with
    | [ { skind = If(_, _, e, _) }] when Cil.is_ghost_else e -> true
    | [ { skind = If(_, { bstmts=[]; battrs=[] }, _, _)
                | If(_, {bstmts=[{skind=Goto _; labels=[]}]; battrs=[]}, _, _)
                | If(_, _, { bstmts=[]; battrs=[] }, _)
                | If(_, _, {bstmts=[{skind=Goto _; labels=[]}]; battrs=[]}, _) } ]
      -> true
    | [ { skind = Block blk | If(_, _, blk, _) } ] ->
      self#block_has_dangling_else blk
    | _ -> false

  method private vdecl_complete fmt v =
    Format.fprintf fmt "@[<hov 0>" ;
    self#in_ghost_if_needed fmt v.vghost ~post_fmt:"@ %t" ~block:false
      (fun () -> Format.fprintf fmt "%a;" self#vdecl v) ;
    Format.fprintf fmt "@]"

  (* no box around the block *)
  method private unboxed_block
      ?(cut=true) ctxt fmt blk =
    let braces = self#require_braces ctxt blk in
    let inline = not braces && self#inline_block ctxt blk in
    if braces then Format.pp_print_char fmt '{';
    if braces && not inline then pp_print_space fmt ();
    if blk.blocals <> [] && verbose then
      fprintf fmt "@[/* Locals: %a */@]@ "
        (Pretty_utils.pp_list ~sep:",@ " self#varinfo) blk.blocals;
    if verbose && not blk.bscoping then fprintf fmt "/* non-scoping */@\n";
    if blk.battrs <> [] then
      (* [JS 2012/12/07] could directly call self#attributesGen whenever we are
         sure than it puts its printing material inside a box *)
      fprintf fmt "@[%a@]" (self#attributesGen true) blk.battrs;
    let locals_decl = List.filter (fun v -> not v.vdefined) blk.blocals in
    if locals_decl <> [] then
      Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@;" ~suf:"@]@ "
        self#vdecl_complete fmt locals_decl;
    let rec iterblock ~cut fmt = function
      | [] -> ()
      | [ s ] ->
        fprintf fmt "";
        if cut && not inline && not braces then pp_print_cut fmt ();
        self#next_stmt Cil.invalidStmt fmt s
      | s_cur :: (s_next :: _ as tail) ->
        Format.fprintf fmt "%a@ "
          (self#next_stmt s_next) s_cur;
        (iterblock[@tailcall]) ~cut:false fmt tail
    in
    let stmts = blk.bstmts in
    if stmts = [] && not braces then fprintf fmt ";"
    else fprintf fmt "%a" (iterblock ~cut) stmts;
    if braces then Format.fprintf fmt "@;<1 -2>}"

  (* wrapper for unboxed_block. Mainly for keeping a method per type in
     Cil_types. All internal calls are directed to unboxed_block.
  *)
  method block fmt (blk: block) =
    fprintf fmt "@[<v 2>%a@]" (self#unboxed_block Other) blk

  (* Store here the name of the last file printed in a line number. This is
     private to the object *)
  val mutable lastFileName = Filepath.empty
  val mutable lastLineNumber = -1

  (* Make sure that you only call self#line_directive on an empty line *)
  method line_directive ?(forcefile=false) fmt l =
    match state.line_directive_style with
    | None -> ()
    | Some _ when Fileloc.line l <= 0 -> ()

    (* Do not print lineComment if the same line as above *)
    | Some Line_comment_sparse when Fileloc.line l = lastLineNumber ->
      ()

    | Some style  ->
      let directive = match style with
        | Line_comment | Line_comment_sparse -> "//#line"
        | Line_preprocessor_output when not (Machine.msvcMode ()) -> "#"
        | Line_preprocessor_output | Line_preprocessor_input -> "#line"
      in
      let path = Fileloc.path l in
      lastLineNumber <- Fileloc.line l;
      let filename =
        if forcefile || not (Filepath.equal path lastFileName) then begin
          lastFileName <- path;
          Format.asprintf " \"%a\"" Filepath.pretty path
        end else
          ""
      in
      fprintf fmt "@[@<0>\n@<0>%s@<0> @<0>%d@<0> @<0>%s@]@\n"
        directive (Fileloc.line l) filename

  (* Print a recovered while condition from Loop *)
  method pp_while_head fmt cond =
    fprintf fmt "%a (%a)"
      self#pp_keyword "while"
      self#exp cond

  method private pp_while_head_stacked ~stmt ~cond fmt =
    begin
      Stack.push stmt current_stmt;
      self#pp_while_head fmt cond;
      ignore (Stack.pop current_stmt);
    end

  method stmtkind sattr (next: stmt) fmt = function
    | UnspecifiedSequence seq ->
      let ctxt =
        match self#current_stmt with None -> Other | Some s -> Stmt_block s
      in
      let as_block = Cil.block_from_unspecified_sequence seq in
      let require_braces = self#require_braces ctxt as_block in
      let inline_block = self#inline_block ctxt as_block in
      let print_stmt pstmt fmt (stmt, modifies, writes, reads,_) =
        pstmt fmt stmt;
        if verbose ||
           Kernel.is_debug_key_enabled Kernel.dkey_print_unspecified
        then
          Format.fprintf fmt "@ /*effects: @[(%a) %a@ <-@ %a@]*/"
            (Pretty_utils.pp_list ~sep:",@ " self#lval) modifies
            (Pretty_utils.pp_list ~sep:",@ " self#lval) writes
            (Pretty_utils.pp_list ~sep:",@ " self#lval) reads
      in
      let rec iterblock fmt = function
        | [] -> ()
        | [ srw ] ->
          print_stmt (self#next_stmt Cil.invalidStmt) fmt srw
        | srw_first :: ((s_next,_,_,_,_) :: _ as tail) ->
          print_stmt (self#next_stmt s_next) fmt srw_first ;
          pp_print_space fmt ();
          iterblock fmt tail
      in
      fprintf fmt "%t%a%t"
        (fun fmt ->
           if require_braces then
             fprintf fmt "@[<v 0>@[<v 2>{ /* sequence */@;"
           else if inline_block then fprintf fmt "@[<hv 0>"
           else fprintf fmt "@[<v 0>")
        iterblock seq
        (if require_braces then fun fmt -> fprintf fmt "@]@;}@]"
         else fun fmt -> pp_close_box fmt ())

    | Return(None, l) ->
      fprintf fmt "@[%a%a;@]"
        (fun fmt -> self#line_directive fmt) l
        self#pp_keyword "return"

    | Return(Some e, l) ->
      fprintf fmt "@[%a@[<hv 2>%a@ %a;@]@]"
        (fun fmt -> self#line_directive fmt) l
        self#pp_keyword "return"
        self#exp e

    | Goto (sref, l) -> begin
        match pickLabel !sref.labels with
        | Some lbl ->
          fprintf fmt "@[%a%a %s;@]"
            (fun fmt -> self#line_directive fmt) l
            self#pp_keyword "goto"
            lbl
        | None ->
          Kernel.error "Cannot find label for target of goto: %a"
            (self#without_annot self#stmt) !sref;
          fprintf fmt "@[%a@ __invalid_label;@]" self#pp_keyword "goto"
      end

    | Break l ->
      fprintf fmt "@[%a%a;@]"
        (fun fmt -> self#line_directive fmt) l
        self#pp_keyword "break"

    | Continue l ->
      fprintf fmt "@[%a%a;@]"
        (fun fmt -> self#line_directive fmt) l
        self#pp_keyword "continue"

    | Instr (Skip loc) ->
      fprintf fmt "@[%a%a;@]"
        (fun fmt -> self#line_directive fmt) loc
        self#attributes sattr

    | Instr i ->
      self#instr fmt i

    | If(be,t,{bstmts=[];battrs=[]},l)
      when not state.print_cil_as_is ->
      fprintf fmt "@[<hv>%a@[<v 2>%a (%a) %a@]@]"
        (fun fmt -> self#line_directive ~forcefile:false fmt) l
        self#pp_keyword "if"
        self#exp be
        (self#unboxed_block Other) t

    | If(be,t,{bstmts=[{skind=Goto(gref,_);labels=[]}]; battrs=[]},l)
      when !gref == next && not state.print_cil_as_is ->
      fprintf fmt "@[<hv>%a@[<v 2>%a (%a) %a@]@]"
        (fun fmt -> self#line_directive ~forcefile:false fmt) l
        self#pp_keyword "if"
        self#exp be
        (self#unboxed_block Other) t

    | If(be,{bstmts=[];battrs=[]},e,l)
      when not (Cil.is_ghost_else e) && not state.print_cil_as_is ->
      fprintf fmt "@[<hv>%a@[<v 2>%a (%a) %a@]@]"
        (fun fmt -> self#line_directive ~forcefile:false fmt) l
        self#pp_keyword "if"
        self#exp (Cil.dummy_exp(UnOp(LNot,be,Cil_const.intType)))
        (self#unboxed_block Other) e

    | If(be,{bstmts=[{skind=Goto(gref,_);labels=[]}]; battrs=[]},e,l)
      when not (Cil.is_ghost_else e) && !gref == next && not state.print_cil_as_is ->
      fprintf fmt "@[<hv>%a@[<v 2>%a (%a) %a@]@]"
        (fun fmt -> self#line_directive ~forcefile:false fmt) l
        self#pp_keyword "if"
        self#exp (Cil.dummy_exp(UnOp(LNot,be,Cil_const.intType)))
        (self#unboxed_block Other) e;

    | If(be,t,e,l) ->
      pp_open_hvbox fmt 0;
      self#line_directive fmt l;
      let else_at_newline =
        (self#require_braces Then_with_else t)
        || not (self#inline_block Then_with_else t)
        || not (self#inline_block Other e)
        || (* call to a function in both branches (for GUI' status bullets) *)
        (force_brace && self#block_is_function t && self#block_is_function e)
      in
      fprintf fmt "@[<v 2>%a (%a) %a@]"
        self#pp_keyword "if"
        self#exp be
        (self#unboxed_block Then_with_else) t;
      if else_at_newline then fprintf fmt "@\n" else fprintf fmt "@ ";
      let do_print () =
        fprintf fmt "%a %a"
          self#pp_keyword "else"
          (self#unboxed_block Other) e;
      in
      fprintf fmt "@[<v 2>" ;
      self#in_ghost_if_needed
        fmt (Cil.is_ghost_else e) ~block:false
        ~break_ghost:false ~post_fmt:"%t" do_print ;
      fprintf fmt "@]" ;
      pp_close_box fmt ()

    | Switch(e,b,_,l) ->
      fprintf fmt "@[%a@[<v 2>%a (%a) %a@]@]"
        (fun fmt -> self#line_directive ~forcefile:false fmt) l
        self#pp_keyword "switch"
        self#exp e
        (self#unboxed_block Other) b

    | Loop(a, b, l, _, _) ->
      Format.pp_open_hvbox fmt 0;
      if logic_printer_enabled && a <> [] then begin
        Format.fprintf fmt "%t " (fun fmt -> self#pp_open_annotation fmt);
        Pretty_utils.pp_list ~sep:"@\n" self#code_annotation fmt a;
        Format.fprintf fmt "@ %t" (fun fmt -> self#pp_close_annotation fmt);
      end;
      let pp_sattr fmt =
        if sattr <> [] && Kernel.is_debug_key_enabled Kernel.dkey_print_attrs
        then Format.fprintf fmt "@[/*%a */ @]" self#attributes sattr
        else Format.ifprintf fmt ""
      in
      ((* Maybe the first thing is a conditional. Turn it into a WHILE *)
        try
          let rec skipEmpty = function
            | [] -> []
            | { skind = Instr (Skip _) } as h :: rest
              when self#may_be_skipped h-> skipEmpty rest
            | x -> x
          in
          let stmt, cond, bodystmts =
            (* Bill McCloskey: Do not remove the If if it has labels *)
            match skipEmpty b.bstmts with
            | { skind = If(e,tb,fb,_) } as to_skip :: rest
              when not state.print_cil_as_is
                && self#may_be_skipped to_skip ->
              (match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
               | [], [ { skind = Break _ } as s ] when self#may_be_skipped s ->
                 to_skip, e, rest
               | [], [ { skind = Goto(sref, _) } as s ]
                 when self#may_be_skipped s
                   && Cil_datatype.Stmt.equal !sref next ->
                 to_skip, e, rest
               | [ { skind = Break _ } as s ], [] when self#may_be_skipped s ->
                 to_skip, Cil.dummy_exp (UnOp(LNot, e, Cil_const.intType)), rest
               | [ { skind = Goto(sref, _) } as s ], []
                 when self#may_be_skipped s
                   && Cil_datatype.Stmt.equal !sref next ->
                 to_skip, Cil.dummy_exp (UnOp(LNot, e, Cil_const.intType)), rest
               | _ -> raise Not_found)
            | _ -> raise Not_found
          in
          let b = match skipEmpty bodystmts with
              [{ skind=Block b} as s ] when self#may_be_skipped s -> b
            | _ -> { b with bstmts = bodystmts }
          in
          Format.fprintf fmt "%a@[<v 2>%t %t%a@]"
            (fun fmt -> self#line_directive fmt) l
            (self#pp_while_head_stacked ~stmt ~cond)
            pp_sattr
            (self#unboxed_block Other) b;
        with Not_found ->
          Format.fprintf fmt "%a@[<v 2>%a (1) %t%a@]"
            (fun fmt -> self#line_directive fmt) l
            self#pp_keyword "while"
            pp_sattr
            (self#unboxed_block Other) b);
      Format.pp_close_box fmt ()

    | Block b ->
      let ctxt =
        match self#current_stmt with None -> Other | Some s -> Stmt_block s
      in
      (* We do not want to put extra braces in presence of blocks included in
         another block (that's often the case). So the following line
         specifically limits the number of braces in that case. But that
         assumes that the required braces have already been put before by the
         callers *)
      let braces = self#require_braces ctxt b in
      let open_box =
        if self#inline_block ctxt b then pp_open_hvbox else pp_open_vbox
      in
      open_box fmt (if braces then 2 else 0);
      if verbose then Pretty_utils.pp_open_block fmt "/*block:begin*/@ ";
      self#unboxed_block ~cut:false ctxt fmt b;
      if verbose then Pretty_utils.pp_close_block fmt "/*block:end*/";
      pp_close_box fmt ()

    | TryFinally (b, h, l) ->
      fprintf fmt "@[%a@[<v 2>__try@ %a@]@ @[<v 2>__finally@ %a@]@]"
        (fun fmt -> self#line_directive fmt) l
        (self#unboxed_block Other) b
        (self#unboxed_block Other) h

    | TryExcept (b, (il, e), h, l) ->
      fprintf fmt "@[%a@[<v 2>__try@ %a@]@ @[<v 2>__except(@\n@["
        (fun fmt -> self#line_directive fmt) l
        (self#unboxed_block Other) b;
      (* Print the instructions but with a comma at the end, instead of
       * semicolon *)
      instr_terminator <- ",";
      Pretty_utils.pp_list ~sep:"@\n" self#instr fmt il;
      instr_terminator <- ";";
      fprintf fmt "%a) @]@ %a@]" self#exp e (self#unboxed_block Other) h

    | Throw (e,_) ->
      let print_expr fmt (e,_) = self#exp fmt e in
      fprintf fmt "@[<hov 2>%a@ %a;@]"
        self#pp_keyword "throw"
        (Pretty_utils.pp_opt ~pre:"(" ~suf:")" print_expr) e
    | TryCatch(body,catch,_) ->
      let print_var_catch_all fmt v =
        match v with
        | Catch_all -> pp_print_string fmt "..."
        | Catch_exn(v,l) ->
          fprintf fmt "@[<v 2>@[%a@]%a@]"
            self#vdecl v
            (Pretty_utils.pp_list ~pre:"@;" ~sep:"@;"
               (fun fmt (v,_) -> self#vdecl fmt v)) l
      in
      let print_one_catch fmt (v,b) =
        fprintf fmt "@[<v 2>@[%a (@;%a@;)@] %a@]"
          self#pp_keyword "catch"
          print_var_catch_all v
          (self#unboxed_block Other) b
      in
      fprintf fmt "@[<v 0>@[<v 2>%a %a@]@;@[<v 2>%a@]@]"
        self#pp_keyword "try"
        (self#unboxed_block Other) body
        (Pretty_utils.pp_list
           ~pre:"" ~sep:"@;" ~suf:"" print_one_catch) catch

  (*** GLOBALS ***)

  method typeref :
    'a. typ -> (formatter -> 'a -> unit) -> formatter -> 'a -> unit =
    fun _t pp fmt x -> pp fmt x

  method typedef :
    'a. global -> (formatter -> 'a -> unit) -> formatter -> 'a -> unit =
    fun _g pp fmt x -> pp fmt x

  method global fmt (g:global) =
    if print_global g then
      match g with
      | GFun (fundec, l) ->
        self#in_current_function fundec.svar;
        self#in_ghost_if_needed fmt fundec.svar.vghost ~post_fmt:"%t@\n"
          (fun () ->
             (* If the function has attributes then print a prototype because
              * GCC cannot accept function attributes in a definition *)
             let oldattr = fundec.svar.vattr in
             let oldattr' = filter_printing_attributes oldattr in
             (* Always print the file name before function declarations *)
             (* Prototype first *)
             if oldattr' <> [] then
               (self#line_directive fmt l;
                fprintf fmt "%a@\n"
                  self#vdecl_complete fundec.svar);
             (* Temporarily remove the function attributes *)
             fundec.svar.vattr <- [];
             (* Body now *)
             self#line_directive ~forcefile:true fmt l;
             self#fundecl fmt fundec;
             (* Restore the list of attributes *)
             fundec.svar.vattr <- oldattr) ;
        fprintf fmt "@\n";
        self#out_current_function

      | GType (typ, l) ->
        self#line_directive ~forcefile:true fmt l;
        let pp_typename fmt = self#typedef g self#varname fmt typ.tname in
        fprintf fmt "%a %a;@\n"
          self#pp_keyword "typedef"
          (self#typ (Some pp_typename)) typ.ttype

      | GEnumTag (enum, l) ->
        self#line_directive fmt l;
        if verbose then
          fprintf fmt "/* Following enum is equivalent to %a */@\n"
            (self#typ None)
            (Cil_const.mk_tint enum.ekind);
        fprintf fmt "%a@[ %a {@\n%a@]@\n}%a;@\n"
          self#pp_keyword "enum"
          (self#typedef g self#enumname) enum
          (Pretty_utils.pp_list ~sep:",@\n"
             (fun fmt item ->
                fprintf fmt "%a = %a"
                  self#varname item.einame
                  self#exp item.eival))
          enum.eitems
          self#attributes enum.eattr

      | GEnumTagDecl (enum, l) -> (* This is a declaration of a tag *)
        self#line_directive fmt l;
        fprintf fmt "%a %a;@\n"
          self#pp_keyword "enum"
          (self#typeref (Cil_const.mk_tenum enum) self#enumname) enum

      | GCompTag (comp, l) -> (* This is a definition of a tag *)
        let sto_mod, rest_attr = Ast_attributes.split_storage_modifiers comp.cattr in
        self#line_directive ~forcefile:true fmt l;
        fprintf fmt "@[<3>%a%a %a {@\n%a@]@\n}%a;@\n"
          self#compkind comp
          self#attributes sto_mod
          (self#typedef g self#compname) comp
          (Pretty_utils.pp_list ~sep:"@\n" self#fieldinfo)
          (Option.value ~default:[] comp.cfields)
          self#attributes rest_attr

      | GCompTagDecl (comp, l) -> (* This is a declaration of a tag *)
        self#line_directive fmt l;
        fprintf fmt "%a %a;@\n"
          self#compkind comp
          (self#typeref (Cil_const.mk_tcomp comp) self#compname) comp

      | GVar (vi, io, l) ->
        self#line_directive ~forcefile:true fmt l;
        Format.fprintf fmt "@[<hov 2>";
        self#in_ghost_if_needed fmt vi.vghost ~post_fmt:"@ %t" ~block:false
          (fun () ->
             self#vdecl fmt vi;
             (match io.init with
              | None -> ()
              | Some i ->
                fprintf fmt " =@ ";
                self#init_or_str fmt i;
             );
             fprintf fmt ";") ;
        fprintf fmt "@]@\n";

        (* print global variable 'extern' declarations *)
      | GVarDecl (vi, l) ->
        self#line_directive fmt l;
        fprintf fmt "%a@\n@\n" self#vdecl_complete vi

      (* print function prototypes *)
      | GFunDecl (funspec, vi, l) ->
        self#in_current_function vi;
        self#opt_funspec fmt funspec;
        if not state.print_cil_as_is &&
           Cil_builtins.Builtin_functions.mem vi.vname
        then begin
          (* Compiler builtins need no prototypes. Just print them in
             comments. *)
          fprintf fmt "/* compiler builtin: @\n   %a;   */@\n"
            self#vdecl vi
        end else begin
          self#line_directive fmt l;
          fprintf fmt "%a@\n@\n" self#vdecl_complete vi
        end;
        self#out_current_function

      | GAsm (s, l) ->
        self#line_directive fmt l;
        fprintf fmt "__asm__(\"%s\");@\n" (Escape.escape_string s)

      | GPragma ((an, args), l) ->
        (* sm: suppress printing pragmas that gcc does not understand *)
        (* assume anything starting with "ccured" is ours *)
        (* also don't print the 'combiner' pragma *)
        (* nor 'cilnoremove' *)
        let suppress =
          not state.print_cil_input
          && not (Machine.msvcMode ())
          && (String.starts_with ~prefix:"box" an
              || String.starts_with ~prefix:"ccured" an
              || an = "merger"
              || an = "cilnoremove")
        in
        self#line_directive fmt l;
        if suppress then fprintf fmt "/* ";
        pp_print_string fmt "#pragma ";
        begin
          match an, args with
          | _, [] ->
            fprintf fmt "%s" an
          | "weak", [ACons (varinfo, [])] ->
            fprintf fmt "weak %s" varinfo
          | "",_ ->
            fprintf fmt "%a"
              (Pretty_utils.pp_list ~sep:" " self#attrparam) args
          | _ ->
            fprintf fmt "%s(%a)"
              an
              (Pretty_utils.pp_list ~sep:"," self#attrparam) args
        end;
        if suppress then  fprintf fmt " */@\n" else fprintf fmt "@\n"

      | GAnnot (decl,l) ->
        (* attributes are purely internal. *)
        self#line_directive fmt l;
        fprintf fmt "%t@ %a@ %t@\n"
          (fun fmt -> self#pp_open_annotation ~block:false fmt)
          self#global_annotation decl
          (fun fmt -> self#pp_close_annotation ~block:false fmt)

      | GText s  ->
        if s <> "//" then
          fprintf fmt "%s@\n" s

  method fieldinfo fmt fi =
    fprintf fmt "%a%a %s%a;"
      (self#typ
         (Some (fun fmt ->
              if fi.fname <> Cil.missingFieldName then
                self#varname fmt fi.fname)))
      fi.ftype
      (self#alignas ~space:true) fi.falignas
      (match fi.fbitfield with
       | None -> ""
       | Some i -> ": " ^ string_of_int i ^ " ")
      self#attributes fi.fattr;
    if Kernel.(is_debug_key_enabled dkey_print_field_offsets) then
      try
        let (offset, size) = Cil.fieldBitsOffset fi in
        let first = offset in
        let last = if size > 0 then Some (offset + size - 1) else None in
        let pp_opt fmt = Pretty_utils.pp_opt ~none:"" Format.pp_print_int fmt in
        fprintf fmt " /* bits %d .. %a */" first pp_opt last
      with Cil.SizeOfError _ -> ()

  method private opt_funspec fmt funspec =
    if logic_printer_enabled && not (Cil.is_empty_funspec funspec) then
      (fprintf fmt "@[<hv 1>";
       fprintf fmt "%t %a@ %t"
         (fun fmt -> self#pp_open_annotation ~block:false fmt)
         self#funspec funspec
         (fun fmt -> self#pp_close_annotation ~block:false fmt);
       fprintf fmt "@]@\n")

  method private fundecl fmt f =
    (* declaration. *)
    fprintf fmt "@[";
    self#in_ghost_if_needed fmt f.svar.vghost ~post_fmt:"@ %t" ~block:false
      (fun () ->
         fprintf fmt "%a@\n@[<v 2>" self#vdecl f.svar;
         self#unboxed_block Body fmt f.sbody;
         fprintf fmt "@]") ;
    fprintf fmt "@]@."

  (***** PRINTING DECLARATIONS and TYPES ****)

  method storage fmt = function
    | NoStorage -> fprintf fmt ""
    | Static -> fprintf fmt "%a " self#pp_keyword "static"
    | Extern -> fprintf fmt "%a " self#pp_keyword "extern"
    | Register -> fprintf fmt "%a " self#pp_keyword "register"

  method private alignas ~space fmt = function
    | None -> fprintf fmt ""
    | Some e -> fprintf fmt "%s_Alignas(%a)" (if space then " " else "") self#exp e

  method fkind fmt = function
    | FFloat   -> fprintf fmt "float"
    | FFloat32 -> fprintf fmt "_Float32"
    | FFloat64 -> fprintf fmt "_Float64"
    | FDouble  -> fprintf fmt "double"
    | FLongDouble -> fprintf fmt "long double"

  method ikind fmt c =
    fprintf fmt "%s"
      (match c with
       | IChar -> "char"
       | IBool ->
         if let std = Kernel.CStd.get () in std = C11 || std = C17
         then "_Bool"
         else "bool"
       | ISChar -> "signed char"
       | IUChar -> "unsigned char"
       | IInt -> "int"
       | IUInt -> "unsigned int"
       | IShort -> "short"
       | IUShort -> "unsigned short"
       | ILong -> "long"
       | IULong -> "unsigned long"
       | ILongLong ->
         if Machine.msvcMode ()
         then "__int64"
         else "long long"
       | IULongLong ->
         if Machine.msvcMode ()
         then "unsigned __int64"
         else "unsigned long long"
       | IInt128 -> "__int128"
       | IUInt128 -> "unsigned __int128"
      )

  method compkind fmt ci =
    self#pp_keyword fmt (if ci.cstruct then "struct" else "union")

  method compname fmt ci =
    self#varname fmt ci.cname

  method compinfo fmt ci =
    fprintf fmt "%a %a" self#compkind ci self#compname ci

  method enumname fmt enum =
    self#varname fmt enum.ename

  method typename fmt tinfo =
    self#varname fmt tinfo.tname

  method typ ?fundecl ?alignas nameOpt
      fmt (t:typ) =
    let pname fmt space = match nameOpt with
      | None -> ()
      | Some pp -> if space then pp_print_char fmt ' '; pp fmt in
    let palignas fmt space = match alignas with
      | None -> ()
      | alignas -> self#alignas fmt ~space alignas in
    let printAttributes fmt (a: attributes) =
      match nameOpt with
      | None when not state.print_cil_input && not (Machine.msvcMode ()) -> ()
      (* Cannot print the attributes in this case because gcc does not like them
         here, except if we are printing for CIL, or for MSVC.  In fact, for
         MSVC we MUST print attributes such as __stdcall *)
      (* if pa = nil then nil else text "/*" ++ pa ++ text "*/"*)
      | _ ->  self#attributes fmt a
    in
    match t.tnode with
    | TVoid ->
      fprintf fmt "void%a%a%a" self#attributes t.tattr palignas true pname true

    | TInt ikind ->
      fprintf fmt "%a%a%a%a"
        (self#typeref t self#ikind) ikind
        self#attributes t.tattr
        palignas true
        pname true

    | TFloat fkind ->
      fprintf fmt "%a%a%a%a"
        (self#typeref t self#fkind) fkind
        self#attributes t.tattr
        palignas true
        pname true

    | TComp comp -> (* A reference to a struct *)
      fprintf fmt "%a %a%a%a%a"
        self#compkind comp
        (self#typeref t self#compname) comp
        self#attributes t.tattr
        palignas true
        pname true

    | TEnum enum ->
      fprintf fmt "%a %a%a%a%a"
        self#pp_keyword "enum"
        (self#typeref t self#enumname) enum
        self#attributes t.tattr
        palignas true
        pname true

    | TPtr bt ->
      (* Parenthesize the ( * attr name) if a pointer to a function or an
       * array. However, on MSVC the __stdcall modifier must appear right
       * before the pointer constructor "(__stdcall *f)". We push them into
       * the parenthesis. *)
      let (paren: (formatter -> unit) option), (bt': typ) =
        match bt.tnode with
        | TFun(rt, args, isva) when Machine.msvcMode () ->
          let an, af', at =
            Ast_attributes.(partition ~default:AttrType bt.tattr)
          in
          (* We take the af' and we put them into the parentheses *)
          Some
            (fun fmt ->
               fprintf fmt
                 "(%a"
                 printAttributes af'),
          Cil_const.mk_tfun ~tattr:(Ast_attributes.add_list an at) rt args isva
        | TFun _ | TArray _ -> (Some (fun fmt -> fprintf fmt "(")), bt
        | _ -> None, bt
      in
      let name' =
        fun fmt ->
        let attr = t.tattr <> [] in
        fprintf fmt "*%a%a%a"
          printAttributes t.tattr
          palignas attr
          pname (attr || alignas <> None)
      in
      let name'' =
        fun fmt ->
        (* Put the parenthesis *)
        match paren with
        | Some p -> fprintf fmt "%t%t)" p name'
        | None -> fprintf fmt "%t" name'
      in
      self#typ (Some name'') fmt bt'

    | TArray (elemt, lo) ->
      let atts_elem, a = Ast_attributes.split_array_attributes t.tattr in
      let size_info,a =
        List.partition (fun a ->
            List.mem (Ast_attributes.get_name a) ["arraylen"; "static"]
          ) a
      in
      (* qualifiers attributes are not supposed to be on the TArray,
         but on the base type, except in the case of a formal declaration. *)
      if atts_elem <> [] && size_info = [] then
        Kernel.failure ~current:true
          "Found some incorrect attributes for array (%a). Please report."
          self#attributes atts_elem;
      let sep fmt = if atts_elem <> [] then Format.pp_print_space fmt () in
      let print_size_info fmt =
        match size_info with
        | [] -> printAttributes fmt a
        | [("arraylen",[s])]->
          Format.fprintf fmt "%a%t%a"
            printAttributes atts_elem sep self#attrparam s
        | [("static",[]); ("arraylen",[s])]
        | [("arraylen", [s]); ("static", [])] ->
          Format.fprintf fmt "static%a@ %a"
            printAttributes atts_elem self#attrparam s
        | _ -> ()
      in
      let name' fmt =
        if filter_printing_attributes a = [] then
          fprintf fmt "%a%a" palignas true pname false
        else if nameOpt = None then
          fprintf fmt "%a%a" printAttributes a palignas true
        else
          fprintf fmt "(%a%a%a)" printAttributes a palignas true pname true
      in
      self#typ
        (Some (fun fmt ->
             fprintf fmt "%t[%t]"
               name'
               (fun fmt ->
                  match lo with
                  | None -> print_size_info fmt
                  | Some e -> self#exp fmt e)
           ))
        fmt
        elemt

    | TFun (restyp, args, isvararg) ->
      (* Note: no printing of _Alignas here, since it does not make sense. *)
      let name' fmt =
        if filter_printing_attributes t.tattr = [] then pname fmt false
        else if nameOpt = None then printAttributes fmt t.tattr
        else fprintf fmt "(%a%a)" printAttributes t.tattr pname true
      in
      let partition_ghosts ghost_arg args =
        match args with
        | None -> None, []
        | Some l ->
          let ghost_args, args = if is_ghost then
              [], l
            else
              List.partition ghost_arg l
          in
          Some args, ghost_args
      in
      let pp_params fmt (args, ghost_args) pp_args =
        fprintf fmt "%t@[<hv>(@[%t@])%t@]" name'
          (fun fmt ->
             match args with
             | (None | Some []) when isvararg -> fprintf fmt "..."
             | None -> ()
             | Some [] -> fprintf fmt "void"
             | Some args ->
               Pretty_utils.pp_list ~sep:",@ " pp_args fmt args;
               if isvararg then fprintf fmt "@ , ...")
          (fun fmt ->
             Pretty_utils.pp_list ~pre:"@;/*@@ ghost (@[" ~suf:"@]) */" ~sep:",@ "
               pp_args fmt ghost_args)
      in
      let pp_params fmt = match fundecl with
        | None ->
          let pp_args fmt (aname,atype,aattr) =
            (* The storage modifiers come first *)
            let atype = self#no_ghost_at_first_level atype in
            let stom, rest = Ast_attributes.split_storage_modifiers aattr in
            fprintf fmt "%a%a%a"
              self#attributes stom
              (self#typ (Some (fun fmt -> fprintf fmt "%s" aname))) atype
              self#attributes rest
          in
          let p = partition_ghosts Cil.isGhostFormalVarDecl args in
          pp_params fmt p pp_args
        | Some fundecl ->
          let args =
            try Some (Cil.getFormalsDecl fundecl) with Not_found -> None
          in
          let p = partition_ghosts Cil.isGhostFormalVarinfo args in
          pp_params fmt p self#vdecl
      in
      self#typ (Some pp_params) fmt restyp

    | TNamed ti ->
      fprintf fmt "%a%a%a%a"
        (self#typeref t self#typename) ti
        self#attributes t.tattr
        palignas true
        pname true

    | TBuiltin_va_list ->
      (* Note: no printing of _Alignas here, since it does not make sense. *)
      fprintf fmt "__builtin_va_list%a%a"
        self#attributes t.tattr
        pname true

  (**** PRINTING ATTRIBUTES *********)
  method attributes fmt a = self#attributesGen false fmt a

  (* Print one attribute. Return also an indication whether this attribute
     should be printed inside the __attribute__ list *)
  method attribute fmt (an, args) =
    (* Recognize and take care of some known cases *)
    match an, args with
    | "const", [] -> self#pp_keyword fmt "const"; false
    (* Put the aconst inside the attribute list *)
    | "aconst", [] when not (Machine.msvcMode ()) -> fprintf fmt "__const__"; true
    | "thread", [ ACons ("c11",[]) ]
      when not state.print_cil_as_is ->
      fprintf fmt "_Thread_local"; false
    | "thread", [] when not (Machine.msvcMode ()) -> fprintf fmt "__thread"; false
    | "volatile", [] -> self#pp_keyword fmt "volatile"; false
    | "ghost", [] -> self#pp_keyword fmt "\\ghost"; false
    | "restrict", [] ->
      if Machine.msvcMode () then
        fprintf fmt "__restrict"
      else
        self#pp_keyword fmt "restrict";
      false
    | "missingproto", [] ->
      if self#display_comment () then fprintf fmt "/* missing proto */";
      false
    | "cdecl", [] when Machine.msvcMode () ->
      fprintf fmt "__cdecl"; false
    | "stdcall", [] when Machine.msvcMode () ->
      fprintf fmt "__stdcall"; false
    | "fastcall", [] when Machine.msvcMode () ->
      fprintf fmt "__fastcall"; false
    | "declspec", args when Machine.msvcMode () ->
      fprintf fmt "__declspec(%a)"
        (Pretty_utils.pp_list ~sep:"" self#attrparam) args;
      false
    | "w64", [] when Machine.msvcMode () ->
      fprintf fmt "__w64"; false
    | "asm", args ->
      fprintf fmt "__asm__(%a)"
        (Pretty_utils.pp_list ~sep:"" self#attrparam) args;
      false

    (* sm: also suppress "format" because we seem to print it in
       a way gcc does not like *)
    | "format", _ ->
      if self#display_comment () then fprintf fmt "/* format attribute */";
      false

    | "hidden", _ -> (* hidden attribute list *)
      false
    (* sm: here's another one I don't want to see gcc warnings about.. *)
    | "mayPointToStack", _ when not state.print_cil_input ->
      (* [matth: may be inside another comment.]
         -> text "/*mayPointToStack*/", false *)
      false

    | "arraylen", [a] ->
      if self#display_comment () then fprintf fmt "/*[%a]*/" self#attrparam a;
      false
    | "static",_ ->
      if self#display_comment () then fprintf fmt "/* static */"; false
    | "", _ ->
      fprintf fmt "%a "
        (Pretty_utils.pp_list ~sep:" " self#attrparam) args;
      true
    | s, _ when
        s = Ast_attributes.bitfield_attribute_name &&
        not state.print_cil_as_is &&
        not (Kernel.is_debug_key_enabled Kernel.dkey_print_bitfields) ->
      false
    | _ -> (* This is the default case *)
      (* Add underscores to the name *)
      let an' =
        if Machine.msvcMode () then "__" ^ an else "__" ^ an ^ "__"
      in
      (match args with
       | [] -> fprintf fmt "%s" an'
       | _ :: _ ->
         fprintf fmt "%s(%a)"
           an'
           (Pretty_utils.pp_list ~sep:"," self#attrparam) args);
      true

  method private attribute_prec (contextprec: int) fmt (a: attrparam) =
    let thisLevel = Precedence.getParenthLevelAttrParam a in
    let needParens =
      if thisLevel >= contextprec then
        true
      else if contextprec == Precedence.bitwiseLevel then
        (* quiet down some GCC warnings *)
        thisLevel == Precedence.additiveLevel
        || thisLevel == Precedence.comparativeLevel
      else
        false
    in
    if needParens then fprintf fmt "(%a)" self#attrparam a
    else self#attrparam fmt a

  method attrparam fmt a =
    let level = Precedence.getParenthLevelAttrParam a in
    let alignof_kw = function
      | `Standard -> "_Alignof"
      | `GCC -> "__alignof__"
    in
    match a with
    | AInt n -> fprintf fmt "%a" Z.pretty n
    | AStr s -> fprintf fmt "\"%s\"" (Escape.escape_string s)
    | ACons(s, []) -> fprintf fmt "%s" s
    | ACons("__fc_assign", [a1; a2]) ->
      fprintf fmt "%a=%a"
        (self#attribute_prec level) a1
        (self#attribute_prec level) a2
    | ACons("__fc_float", [AStr s]) -> pp_print_string fmt s
    | ACons(s,al) ->
      fprintf fmt "%s(%a)"
        s
        (Pretty_utils.pp_list ~sep:"" self#attrparam) al
    | ASizeOfE a ->
      fprintf fmt "%a(%a)"
        self#pp_keyword "sizeof"
        self#attrparam a
    | ASizeOf t ->
      fprintf fmt "%a(%a)"
        self#pp_keyword "sizeof"
        (self#typ None) t
    | AAlignOfE (a, i) ->
      fprintf fmt "%a(%a)" self#pp_keyword (alignof_kw i) self#attrparam a
    | AAlignOf (t, i) ->
      fprintf fmt "%a(%a)" self#pp_keyword (alignof_kw i) (self#typ None) t
    | AUnOp(u,a1) ->
      fprintf fmt "%a %a" self#unop u (self#attribute_prec level) a1
    | ABinOp(b,a1,a2) ->
      fprintf fmt "@[(%a)%a@  (%a) @]"
        (self#attribute_prec level) a1
        self#binop b
        (self#attribute_prec level) a2
    | ADot (ap, s) ->
      fprintf fmt "%a.%s" self#attrparam ap s
    | AStar a1 ->
      fprintf fmt "(*%a)" (self#attribute_prec Precedence.derefStarLevel) a1
    | AAddrOf a1 ->
      fprintf fmt "& %a" (self#attribute_prec Precedence.addrOfLevel) a1
    | AIndex (a1, a2) ->
      fprintf fmt "%a[%a]" self#attrparam a1 self#attrparam a2
    | AQuestion (a1, a2, a3) ->
      fprintf fmt "%a ? %a : %a"
        self#attrparam a1
        self#attrparam a2
        self#attrparam a3

  (* A general way of printing lists of attributes *)
  method private attributesGen (block: bool) fmt (a: attributes) =
    (* Scan all the attributes and separate those that must be printed inside
       the __attribute__ list *)
    let rec loop (in__attr__: string list) = function
      | [] ->
        if in__attr__ <> [] then
          begin
            (* __blockattribute__ is not standard, and is used only when
               we are outputting something destined to Cil. Otherwise we
               output everything between comments. *)
            let for_cil s = if state.print_cil_input then "" else s in
            if block
            then fprintf fmt " %s __blockattribute__(" (for_cil "/*")
            else fprintf fmt " __attribute__((";
            Pretty_utils.pp_list ~sep:",@ "
              Format.pp_print_string fmt in__attr__;
            if not block (* reversed so that highlighting matches *)
            then fprintf fmt "))"
            else fprintf fmt ") %s@\n" (for_cil "*/")
          end
      | x :: rest ->
        let buff = Buffer.create 17 in
        let local_fmt = formatter_of_buffer buff in
        let ina = self#attribute local_fmt x in
        pp_print_flush local_fmt ();
        let dx = Buffer.contents buff in
        if ina then
          loop (dx :: in__attr__) rest
        else begin
          if dx <> "" then fprintf fmt " %s" dx;
          loop in__attr__ rest
        end
    in
    loop [] (filter_printing_attributes a);

    (* ******************************************************************* *)
    (* Logic annotations printer *)
    (* ******************************************************************* *)

  method logic_constant fmt = function
    | Boolean true -> self#pp_acsl_keyword fmt "\\true"
    | Boolean false -> self#pp_acsl_keyword fmt "\\false"
    | Integer(_, Some s) when print_as_source s ->
      fprintf fmt "%s" s (* Always print the text if there is one, unless
                            we want to print it as hexa *)
    | Integer(i, _) ->  Z.pretty fmt i
    | LStr(s) -> fprintf fmt "\"%s\"" (Escape.escape_string s)
    | LWStr(s) ->
      (* text ("L\"" ^ escape_string s ^ "\"")  *)
      fprintf fmt "L";
      List.iter
        (fun elt ->
           if (elt >= Int64.zero &&
               elt <= (Int64.of_int 255)) then
             fprintf fmt "%S"
               (Escape.escape_char (Char.chr (Int64.to_int elt)))
           else
             fprintf fmt "\"\\x%LX\"" elt;
           fprintf fmt "@ ")
        s;
      (* we cannot print L"\xabcd" "feedme" as L"\xabcdfeedme" -- the former
         has 7 wide characters and the later has 3. *)
    | LChr(c) -> fprintf fmt "'%s'" (Escape.escape_char c)
    | LReal(r) -> fprintf fmt "%s" r.r_literal
    | LEnum e -> self#varname fmt e.einame

  method logic_type name fmt =
    let pname = match name with
      | Some d -> (fun fmt -> Format.fprintf fmt "@ %t" d)
      | None -> fun _ -> ()
    in
    function
    | Ctype typ -> self#typ name fmt typ
    | Lboolean ->
      Format.fprintf fmt "%t%t" Unicode.pp_boolean pname
    | Linteger ->
      Format.fprintf fmt "%t%t" Unicode.pp_integer pname
    | Lreal ->
      Format.fprintf fmt "%t%t" Unicode.pp_real pname
    | Ltype (s,l) ->
      fprintf fmt "%a%a%t" self#logic_type_info s
        ((* the space avoids the issue of list<list<int>> where the double >
            would be read as a shift. It could be optimized away in most of
            the cases. *)
          Pretty_utils.pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@]>@ "
            (self#logic_type None)) l pname
    | Larrow (args,rt) ->
      fprintf fmt "@[@[<2>{@ %a@]}@]%a%t"
        (Pretty_utils.pp_list ~sep:",@ " (self#logic_type None)) args
        (self#logic_type None) rt pname
    | Lvar s -> fprintf fmt "%s%t" s pname

  method private name fmt s =
    if needs_quote s then Format.fprintf fmt "\"%s\"" s
    else Format.pp_print_string fmt s

  method private term_prec contextprec fmt e =
    let thisLevel = Precedence.getParenthLevelLogic e.term_node in
    if Precedence.needParens thisLevel contextprec then
      fprintf fmt "@[<hov 2>(%a)@]" self#term e
    else self#term fmt e

  method identified_term fmt t = self#term fmt t.it_content

  method term fmt t =
    let debug = Kernel.is_debug_key_enabled Kernel.dkey_print_logic_types in
    if debug then
      fprintf fmt "/*(type:%a */" (self#logic_type None) t.term_type;
    begin match t.term_name with
      | [] -> self#term_node fmt t
      | _ :: _ ->
        fprintf fmt "(@[<2>%a:@ %a@])"
          (Pretty_utils.pp_list ~sep:":@ " self#name) t.term_name
          self#term_node t
    end;
    if debug then fprintf fmt "/*)*/"

  (* This instance variable recalls the current program point in order to
     avoid pretty-printing useless {L} labels. Default to Here. Global
     annotations must take care of setting it to their declared label if
     needed. *)
  val mutable current_label = Logic_const.here_label

  method term_binop fmt = function
    | PlusA | PlusPI -> Format.pp_print_string fmt "+"
    | MinusA | MinusPP | MinusPI -> Format.pp_print_string fmt "-"
    | Mult -> Format.pp_print_string fmt "*"
    | Div -> Format.pp_print_string fmt "/"
    | Mod -> Format.pp_print_string fmt "%"
    | Shiftlt -> Format.pp_print_string fmt "<<"
    | Shiftrt -> Format.pp_print_string fmt ">>"
    | Lt -> Format.pp_print_string fmt "<"
    | Gt -> Format.pp_print_string fmt ">"
    | Le -> Unicode.pp_le fmt
    | Ge -> Unicode.pp_ge fmt
    | Eq -> Unicode.pp_eq fmt
    | Ne -> Unicode.pp_neq fmt
    | BAnd -> Format.pp_print_string fmt "&"
    | BXor -> Format.pp_print_string fmt "^"
    | BOr -> Format.pp_print_string fmt "|"
    | LAnd -> Unicode.pp_and fmt
    | LOr -> Unicode.pp_or fmt

  method relation fmt = function
    | Rlt -> Format.pp_print_string fmt "<"
    | Rgt -> Format.pp_print_string fmt ">"
    | Rle -> Unicode.pp_le fmt
    | Rge -> Unicode.pp_ge fmt
    | Req -> Unicode.pp_eq fmt
    | Rneq -> Unicode.pp_neq fmt

  method private tand_list fmt l =
    match l with
    | [] -> ()
    | [ t ] -> self#term_prec Precedence.and_level fmt t
    | { term_node = TBinOp(op1,low,mid1) } ::
      { term_node = TBinOp(op2,mid2,up) } :: l
      when is_compatible_rel_binop op1 op2
        && equal_mod_coercion mid1 mid2 ->
      fprintf fmt "@[%a %a@ %a %a@ %a"
        (self#term_prec Precedence.comparativeLevel) low
        self#term_binop op1
        (self#term_prec Precedence.comparativeLevel) mid1
        self#term_binop op2
        (self#term_prec Precedence.comparativeLevel) up;
      let dir =
        update_direction_binop (update_direction_binop Both op1) op2
      in
      let rec rel_list dir t =
        function
        | [] -> fprintf fmt "@]"
        | { term_node = TBinOp(op,t',up) } :: l
          when is_same_direction_binop dir op
            && equal_mod_coercion t t' ->
          fprintf fmt " %a@ %a"
            self#term_binop op
            (self#term_prec Precedence.comparativeLevel) up;
          rel_list (update_direction_binop dir op) up l
        | l ->
          fprintf fmt "@] %a@ %a" self#term_binop LAnd self#tand_list l
      in
      rel_list dir up l
    | t :: l ->
      fprintf fmt "%a %a@ %a"
        (self#term_prec Precedence.and_level) t
        self#term_binop LAnd
        self#tand_list l

  method private range fmt (low, high) =
    let pp_opt = Pretty_utils.pp_opt ~pre:"" ~suf:"" in
    fprintf fmt "@[%a..%a@]"
      (pp_opt
         (fun fmt v -> Format.fprintf fmt "%a " (self#term_prec Precedence.upperLevel) v)) low
      (pp_opt
         (fun fmt v -> Format.fprintf fmt "@ %a" (self#term_prec Precedence.upperLevel) v)) high;

  method term_node fmt t =
    let current_level = Precedence.getParenthLevelLogic t.term_node in
    let term = self#term_prec current_level in
    match t.term_node with
    | TConst s -> fprintf fmt "%a" self#logic_constant s
    | TDataCons(ci,args) ->
      (match extract_acsl_list t with
       | Some l when not state.print_cil_as_is ->
         fprintf fmt "@,[|@;%a|]"
           (Pretty_utils.pp_list
              ~sep:",@ " ~pre:"@[<hov>" ~suf:"@]@;" self#term_node) l
       | _ ->
         fprintf fmt "%a%a" self#logic_name ci.ctor_name
           (Pretty_utils.pp_list ~pre:"(@[" ~suf:"@])" ~sep:",@ " self#term)
           args)
    | TLval lv -> fprintf fmt "%a" (self#term_lval_prec current_level) lv
    | TSizeOf t ->
      fprintf fmt "%a(%a)" self#pp_acsl_keyword "sizeof" (self#typ None) t
    | TSizeOfE e ->
      fprintf fmt "%a(%a)" self#pp_acsl_keyword "sizeof" self#term e
    | TAlignOf e ->
      fprintf fmt "%a(%a)" self#pp_acsl_keyword "alignof" (self#typ None) e
    | TAlignOfE e ->
      fprintf fmt "%a(%a)" self#pp_acsl_keyword "alignof" self#term e
    | TUnOp (op,e) -> fprintf fmt "%a%a" self#unop op term e
    | TBinOp (LAnd, l, r) when not state.print_cil_as_is ->
      fprintf fmt "@[%a@]" self#tand_list (get_tand_list l [r])
    | TBinOp (op,l,r) ->
      fprintf fmt "@[%a@ %a@ %a@]" term l self#term_binop op term r
    | TCast (false, Ctype ty,t) ->
      begin match ty.tnode, t.term_node with
        | TFloat fk , TConst(LReal r as cst) when
            not Kernel.(is_debug_key_enabled dkey_print_logic_coercions) &&
            Floating_point.has_suffix fk r.r_literal ->
          self#logic_constant fmt cst
        | _ ->
          fprintf fmt "(%a)%a" (self#typ None) ty term t
      end
    | TCast(false,_,_) ->
      Kernel.fatal "Found a TCast to ctype without a Ctype associated"
    | TAddrOf lv ->
      fprintf fmt "&%a" (self#term_lval_prec Precedence.addrOfLevel) lv
    | TStartOf lv ->
      fprintf fmt "&(%a[0])" self#term_lval lv
    (* A few built-ins for lists have special syntax. Use it when we're not
       in print-as-is mode. *)
    | Tapp ({ l_var_info },[],[e1; e2])
      when l_var_info.lv_name = "\\concat" && not state.print_cil_as_is ->
      fprintf fmt "%a ^ %a" term e1 term e2
    | Tapp ({ l_var_info },[],[e1;e2])
      when l_var_info.lv_name = "\\repeat" && not state.print_cil_as_is ->
      fprintf fmt "%a *^ %a" term e1 term e2
    | Tapp (f, labels, tl) ->
      fprintf fmt "%a%a%a"
        self#logic_info f
        self#labels labels
        (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]" ~sep:",@ " self#term) tl
    | Tif (cond,th,el) ->
      fprintf fmt "@[<2>@[%a@]@ ?@ @[%a@]@ :@ @[%a@]@]"
        self#pred_prec_named (current_level, cond)
        term th
        term el
    | Tat (t,lab) ->
      let old_label = current_label in
      current_label <- lab;
      if Cil_datatype.Logic_label.equal lab Logic_const.old_label then
        fprintf fmt "@[%a(@[%a@])@]" self#pp_acsl_keyword "\\old" self#term t
      else
        fprintf fmt "@[%a(@[@[%a@],@,@[%a@]@])@]"
          self#pp_acsl_keyword "\\at" self#term t self#logic_label lab;
      current_label <- old_label
    | Toffset (l,t) ->
      fprintf fmt "%a%a(%a)" self#pp_acsl_keyword "\\offset"
        self#labels [l] self#term t
    | Tbase_addr (l,t) ->
      fprintf fmt "%a%a(%a)" self#pp_acsl_keyword "\\base_addr"
        self#labels [l] self#term t
    | Tblock_length (l,t) ->
      fprintf fmt "%a%a(%a)" self#pp_acsl_keyword "\\block_length"
        self#labels [l] self#term t
    | Tnull -> self#pp_acsl_keyword fmt "\\null"
    | TUpdate (t,toff,v) ->
      fprintf fmt "{%a %a %a = %a}"
        self#term t
        self#pp_acsl_keyword "\\with"
        self#term_offset toff
        self#term v
    | Tlambda(prms,expr) ->
      fprintf fmt "@[<2>%a@ %a;@ %a@]"
        self#pp_acsl_keyword "\\lambda"
        self#quantifiers prms term expr
    | Ttypeof t ->
      fprintf fmt "%a(%a)" self#pp_acsl_keyword "\\typeof" self#term t
    | Ttype ty ->
      fprintf fmt "%a(%a)" self#pp_acsl_keyword "\\type" (self#typ None) ty
    | Tunion l
      when (List.for_all is_singleton l) && (not state.print_cil_as_is) ->
      fprintf fmt "{%a}" (Pretty_utils.pp_list ~sep:",@ " self#term) l
    | Tunion locs ->
      fprintf fmt "@[<hov 2>%a(@,%a)@]"
        self#pp_acsl_keyword "\\union"
        (Pretty_utils.pp_list ~sep:",@ " self#term) locs
    | Tinter locs ->
      fprintf fmt "@[<hov 2>%a(@,%a)@]"
        self#pp_acsl_keyword "\\inter"
        (Pretty_utils.pp_list ~sep:",@ " self#term) locs
    | Tempty_set -> self#pp_acsl_keyword fmt "\\empty"
    | Tcomprehension(lv,quant,p) ->
      fprintf fmt "{@[%a@ |@ %a%a@]}"
        (self#term_prec Precedence.bitwiseLevel) lv self#quantifiers quant
        (Pretty_utils.pp_opt
           (fun fmt p -> fprintf fmt ";@ %a" self#predicate p)) p
    | Trange(low,high) ->
      fprintf fmt "(%a)" self#range (low, high)
    | Tlet(def,body) ->
      assert
        (Kernel.verify (def.l_labels = [])
           "invalid logic construction: local definition with label");
      assert
        (Kernel.verify (def.l_tparams = [])
           "invalid logic construction: polymorphic local definition");
      let v = def.l_var_info in
      let args = def.l_profile in
      let pp_defn = match def.l_body with
        | LBterm t -> fun fmt -> term fmt t
        | LBpred p -> fun fmt -> self#predicate fmt p
        | LBnone
        | LBreads _ | LBinductive _ ->
          Kernel.fatal "invalid logic local definition"
      in
      fprintf fmt "@[<hv 0>@[<hov 2>%a %a =@ %t%t;@]@ %a@]"
        self#pp_acsl_keyword "\\let"
        self#logic_var v
        (fun fmt -> if args <> [] then
            fprintf fmt "@[<2>%a@ %a;@]@ "
              self#pp_acsl_keyword "\\lambda"
              self#quantifiers args)
        pp_defn
        term body
    | TCast (true, ty,t) ->
      if (not (Cil.no_op_coerce ty t)) ||
         Kernel.is_debug_key_enabled Kernel.dkey_print_logic_coercions
      then
        fprintf fmt "(%a)" (self#logic_type None) ty;
      term fmt t;

  method private term_lval_prec contextprec fmt lv =
    if Precedence.getParenthLevelLogic (TLval lv) > contextprec then
      fprintf fmt "(%a)" self#term_lval lv
    else
      fprintf fmt "%a" self#term_lval lv

  method term_lval fmt lv = match lv with
    | TVar vi ,TNoOffset when vi.lv_name = "\\pi" -> Unicode.pp_pi fmt
    | TVar vi, o -> fprintf fmt "%a%a" self#logic_var vi self#term_offset o
    | TResult _, o ->
      fprintf fmt "%a%a" self#pp_acsl_keyword "\\result" self#term_offset o
    | TMem e, (TField({fname},o) | TModel ({mi_name = fname}, o)) ->
      fprintf fmt "%a->%a%a" (self#term_prec Precedence.arrowLevel) e
        self#varname fname self#term_offset o
    | TMem e, TNoOffset ->
      fprintf fmt "*%a" (self#term_prec Precedence.derefStarLevel) e
    | TMem e, (TIndex _ as o) ->
      fprintf fmt "(*%a)%a"
        (self#term_prec Precedence.derefStarLevel) e self#term_offset o

  method model_field fmt mi = self#varname fmt mi.mi_name

  method term_offset fmt o = match o with
    | TNoOffset -> ()
    | TField (fi,o) ->
      fprintf fmt ".%a%a" self#field fi self#term_offset o
    | TModel (mi,o) ->
      fprintf fmt ".%a%a" self#model_field mi self#term_offset o
    | TIndex({term_node = Trange(low,high); term_name; _},o) ->
      (* Make sure range names are printed (as in [self#term]). *)
      let aux fmt () =
        match term_name with
        | [] -> self#range fmt (low, high)
        | _ :: _ ->
          fprintf fmt "@[<2>%a:@ (%a)@]"
            (Pretty_utils.pp_list ~sep:":@ " self#name) term_name
            self#range (low, high)
      in
      fprintf fmt "[%a]%a" aux () self#term_offset o
    | TIndex(e,o) -> fprintf fmt "[%a]%a" self#term e self#term_offset o

  method term_lhost fmt (lh:term_lhost) =
    self#term_lval fmt (lh, TNoOffset)

  val module_stack : string Stack.t = Stack.create ()

  method logic_name fmt a =
    try
      let prefix = Stack.top module_stack in
      let shortname = String.remove_prefix prefix a in
      self#varname fmt @@ Option.value ~default:a shortname
    with Stack.Empty ->
      self#varname fmt a

  method logic_info fmt li = self#logic_var fmt li.l_var_info

  method logic_var fmt v =
    if Kernel.is_debug_key_enabled Kernel.dkey_print_vid then begin
      Format.fprintf fmt "/* ";
      (match v.lv_origin with
         None -> ()
       | Some v -> Format.fprintf fmt "vid:%d, " v.vid);
      Format.fprintf fmt "lvid:%d */" v.lv_id
    end;
    self#logic_name fmt v.lv_name

  method quantifiers fmt l =
    Pretty_utils.pp_list ~sep:",@ "
      (fun fmt lv ->
         let pvar fmt = self#logic_var fmt lv in
         self#logic_type (Some pvar) fmt lv.lv_type)
      fmt l

  method private pred_prec fmt (contextprec,p) =
    let thisLevel = Precedence.getParenthLevelPred p in
    let needParens = Precedence.needParens thisLevel contextprec in
    if needParens then fprintf fmt "@[<hov 2>(%a)@]" self#predicate_node p
    else self#predicate_node fmt p

  method private named_pred fmt (parenth, names, content) =
    match names with
    | [] -> self#pred_prec fmt (parenth,content)
    | _ :: _ ->
      if parenth = Precedence.upperLevel then
        fprintf fmt "@[<hv 2>%a:@ %a@]"
          (Pretty_utils.pp_list ~sep:":@ " self#name) names
          self#pred_prec (Precedence.upperLevel, content)
      else
        fprintf fmt "(@[<hv 2>%a:@ %a@])"
          (Pretty_utils.pp_list ~sep:":@ " self#name) names
          self#pred_prec (Precedence.upperLevel, content)

  method private pred_prec_named fmt (parenth,p) =
    self#named_pred fmt (parenth,p.pred_name,p.pred_content)

  method predicate fmt p =
    self#named_pred fmt (Precedence.upperLevel, p.pred_name, p.pred_content)

  method identified_predicate fmt p =
    if verbose then fprintf fmt "/* ip:%d */" p.ip_id;
    self#predicate fmt (Logic_const.pred_of_id_pred p)

  method private preds kw fmt l =
    Pretty_utils.pp_list ~suf:"@]@\n" ~sep:"@\n"
      (fun fmt p ->
         fprintf fmt "@[%s %a;@]" kw self#identified_predicate p) fmt l

  method private pand_list fmt l =
    let term = self#term_prec Precedence.comparativeLevel in
    let pred fmt p = self#pred_prec_named fmt (Precedence.and_level,p) in
    match l with
    | [] -> ()
    | [p] -> pred fmt p
    | { pred_content = Prel(rel1, low, mid1) } ::
      { pred_content = Prel(rel2, mid2, up)  } :: l
      when is_compatible_relation rel1 rel2 &&
           equal_mod_coercion mid1 mid2 ->
      fprintf fmt "@[%a@ %a@ %a@ %a@ %a"
        term low self#relation rel1 term mid1 self#relation rel2 term up;
      let dir = update_direction_rel (update_direction_rel Both rel1) rel2 in
      let rec rel_list dir t =
        function
        | [] -> fprintf fmt "@]"
        | { pred_content = Prel(rel,t',up) } :: l
          when is_same_direction_rel dir rel && equal_mod_coercion t t' ->
          fprintf fmt " %a@ %a" self#relation rel term up;
          rel_list (update_direction_rel dir rel) up l
        | l ->
          fprintf fmt "@] %a@ %a" self#term_binop LAnd self#pand_list l
      in
      rel_list dir up l
    | p :: l ->
      fprintf fmt "%a %a@ %a" pred p self#term_binop LAnd self#pand_list l

  method predicate_node fmt p =
    let current_level = Precedence.getParenthLevelPred p in
    let term = self#term_prec current_level in
    match p with
    | Pfalse -> self#pp_acsl_keyword fmt "\\false"
    | Ptrue -> self#pp_acsl_keyword fmt "\\true"
    | Papp (pi,labels,l) -> begin
        match Precedence.subset_is_backslash_in p with
        | Some (tl, tr) ->
          fprintf fmt "@[%a %t@ %a@]"
            term tl
            Unicode.pp_in_acsl
            term tr
        | None ->
          fprintf fmt "@[%a%a%a@]"
            self#logic_info pi
            self#labels labels
            (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]" ~sep:",@ " self#term) l
      end
    | Prel (rel,l,r) ->
      fprintf fmt "@[%a@ %a@ %a@]" term l self#relation rel term r
    | Pand (p1, p2) when not state.print_cil_as_is ->
      fprintf fmt "@[%a@]" self#pand_list (get_pand_list p1 [p2])
    | Pand (p1,p2) ->
      fprintf fmt "@[%a %a@ %a@]"
        self#pred_prec_named (current_level,p1)
        self#term_binop LAnd
        self#pred_prec_named (current_level,p2)
    | Por (p1, p2) ->
      fprintf fmt "@[%a %a@ %a@]"
        self#pred_prec_named (current_level,p1)
        self#term_binop LOr
        self#pred_prec_named (current_level,p2)
    | Pxor (p1, p2) ->
      fprintf fmt "@[%a %t@ %a@]"
        self#pred_prec_named (current_level,p1)
        Unicode.pp_xor
        self#pred_prec_named (current_level,p2)
    | Pimplies (p1,p2) ->
      fprintf fmt "@[%a %t@ %a@]"
        self#pred_prec_named (current_level,p1)
        Unicode.pp_implies
        self#pred_prec_named (current_level+1,p2)
    | Piff (p1,p2) ->
      fprintf fmt "@[%a %t@ %a@]"
        self#pred_prec_named (current_level,p1)
        Unicode.pp_iff
        self#pred_prec_named (current_level,p2)
    | Pnot a -> fprintf fmt "@[%t%a@]"
                  Unicode.pp_not
                  self#pred_prec_named (current_level,a)
    | Pif (c, p1, p2) ->
      fprintf fmt "@[<hv 2>@[%a@]@ ?@ @[%a@]@ :@ @[%a@]@]"
        self#pred_prec_named (current_level, c)
        self#pred_prec_named (current_level, p1)
        self#pred_prec_named (current_level, p2)
    | Plet (def, p) ->
      assert
        (Kernel.verify (def.l_labels = [])
           "invalid logic construction: local definition with label");
      assert
        (Kernel.verify (def.l_tparams = [])
           "invalid logic construction: polymorphic local definition");
      let v = def.l_var_info in
      let args = def.l_profile in
      let pp_defn = match def.l_body with
        | LBterm t -> fun fmt -> term fmt t
        | LBpred p -> fun fmt -> self#pred_prec_named fmt (current_level,p)
        | LBnone
        | LBreads _ | LBinductive _ ->
          Kernel.fatal "invalid logic local definition"
      in
      Precedence.needIndent current_level p fmt
        "@[<hv 0>@[<hov 2>%a %a =@ %t%t;@]@ %a@]"
        self#pp_acsl_keyword "\\let"
        self#logic_var v
        (fun fmt ->
           if args <> [] then
             fprintf fmt "@[<hv 2>%a@ %a;@]@ "
               self#pp_acsl_keyword "\\lambda"
               self#quantifiers args)
        pp_defn
        self#pred_prec_named (current_level,p)
    | Pforall (quant,pred) ->
      Precedence.needIndent current_level pred fmt
        "@[%t %a;@]@ %a"
        Unicode.pp_forall
        self#quantifiers quant self#pred_prec_named (current_level,pred)
    | Pexists (quant,pred) ->
      Precedence.needIndent current_level pred fmt
        "@[%t %a;@]@ %a"
        Unicode.pp_exists
        self#quantifiers quant self#pred_prec_named (current_level,pred)
    | Pfreeable (l,p) ->
      fprintf fmt "@[%a%a(@[%a@])@]"
        self#pp_acsl_keyword "\\freeable"
        self#labels [l] self#term p
    | Pallocable (l,p) ->
      fprintf fmt "@[%a%a(@[%a@])@]"
        self#pp_acsl_keyword "\\allocable"
        self#labels [l] self#term p
    | Pvalid (l,p) ->
      fprintf fmt "@[%a%a(@[%a@])@]"
        self#pp_acsl_keyword "\\valid"
        self#labels [l] self#term p
    | Pvalid_read (l,p) ->
      fprintf fmt "@[%a%a(@[%a@])@]"
        self#pp_acsl_keyword "\\valid_read"
        self#labels [l] self#term p
    | Pobject_pointer (l,p) ->
      fprintf fmt "@[%a%a(@[%a@])@]"
        self#pp_acsl_keyword "\\object_pointer"
        self#labels [l] self#term p
    | Pvalid_function p ->
      fprintf fmt "@[%a(@[%a@])@]"
        self#pp_acsl_keyword "\\valid_function"
        self#term p
    | Pinitialized (l,p) ->
      fprintf fmt "@[%a%a(@[%a@])@]"
        self#pp_acsl_keyword "\\initialized"
        self#labels [l] self#term p
    | Pdangling (l,p) ->
      fprintf fmt "@[%a%a(@[%a@])@]"
        self#pp_acsl_keyword "\\dangling"
        self#labels [l] self#term p
    | Paligned (p, n) ->
      fprintf fmt "@[%a(@[%a@],@[%a@])@]"
        self#pp_acsl_keyword "\\aligned"
        self#term p self#term n
    | Pfresh (l1,l2,e1,e2) ->
      fprintf fmt "@[%a%a(@[%a@],@[%a@])@]"
        self#pp_acsl_keyword "\\fresh"
        self#labels [l1;l2] self#term e1 self#term e2
    | Pseparated seps ->
      fprintf fmt "@[<hv 2>%a(@,%a@,)@]"
        self#pp_acsl_keyword "\\separated"
        (Pretty_utils.pp_list ~sep:",@ " self#term) seps
    | Pat(p,lab) ->
      let old_label = current_label in
      current_label <- lab;
      if Cil_datatype.Logic_label.equal lab Logic_const.old_label then
        fprintf fmt "@[%a(@[%a@])@]"
          self#pp_acsl_keyword "\\old"
          self#pred_prec_named (Precedence.upperLevel,p)
      else
        fprintf fmt "@[%a(@[<hv 0>@[%a@],@,%a@])@]"
          self#pp_acsl_keyword "\\at"
          self#pred_prec_named (Precedence.upperLevel,p)
          self#logic_label lab;
      current_label <- old_label

  method private decrement kw fmt (t, rel) = match rel with
    | None -> fprintf fmt "@[<2>%a@ %a;@]" self#pp_acsl_keyword kw self#term t
    | Some li ->
      fprintf fmt "@[<2>%a@ %a@ %a@ %a;@]"
        self#pp_acsl_keyword kw
        self#term t
        self#pp_acsl_keyword "for"
        self#logic_info li

  method decreases fmt v = self#decrement "decreases" fmt v
  method variant fmt v = self#decrement "loop variant" fmt v

  method private pp_predicate_kind fmt = function
    | Assert -> ()
    | Check -> self#pp_acsl_keyword fmt "check" ; pp_print_char fmt ' '
    | Admit -> self#pp_acsl_keyword fmt "admit" ; pp_print_char fmt ' '

  method private pp_lemma_kind fmt = function
    | Assert -> self#pp_acsl_keyword fmt "lemma"
    | Admit -> self#pp_acsl_keyword fmt "axiom"
    | Check ->
      begin
        self#pp_acsl_keyword fmt "check" ;
        pp_print_char fmt ' ' ;
        self#pp_acsl_keyword fmt "lemma" ;
      end

  method assumes fmt p =
    fprintf fmt "@[<hov 2>%a@ %a;@]"
      self#pp_acsl_keyword "assumes"
      self#identified_predicate p

  method requires fmt p =
    fprintf fmt "@[<hov 2>%a%a@ %a;@]"
      self#pp_predicate_kind p.ip_content.tp_kind
      self#pp_acsl_keyword "requires"
      self#identified_predicate p

  method extended fmt ext =
    Extensions.pp (self :> extensible_printer_type) fmt ext

  method short_extended fmt ext =
    Extensions.pp_short (self :> extensible_printer_type) fmt ext

  method post_cond fmt (k,p) =
    let kw = get_termination_kind_name k in
    fprintf fmt "@[<hov 2>%a%a@ %a;@]"
      self#pp_predicate_kind p.ip_content.tp_kind
      self#pp_acsl_keyword kw
      self#identified_predicate p

  method terminates fmt p =
    fprintf fmt "@[<hov 2>%a@ %a;@]"
      self#pp_acsl_keyword "terminates"
      self#identified_predicate p

  method private cd_behaviors fmt kind p =
    fprintf fmt "@[%a %a;@]"
      self#pp_acsl_keyword (kind^" behaviors")
      (Pretty_utils.pp_list ~pre:"@[<hv 0>" ~sep:",@ " pp_print_string)
      p

  method complete_behaviors fmt p = self#cd_behaviors fmt "complete" p
  method disjoint_behaviors fmt p = self#cd_behaviors fmt "disjoint" p

  method allocation ~isloop fmt = function
    | FreeAllocAny -> ()
    | FreeAlloc([],[]) ->
      fprintf fmt "@[%a@ %a;@]"
        self#pp_acsl_keyword (if isloop then "loop allocates" else "allocates")
        self#pp_acsl_keyword "\\nothing"
    | FreeAlloc(f,a) ->
      let pFreeAlloc kw fmt = function
        | [] -> ()
        | _ :: _ as af ->
          fprintf fmt "@[%a@ %a;@]"
            self#pp_acsl_keyword (if isloop then "loop "^kw else kw)
            (Pretty_utils.pp_list ~sep:",@ " self#identified_term) af
      in
      fprintf fmt "@[<v>%a%(%)%a@]"
        (pFreeAlloc "frees") f
        (if f != [] && a != [] then format_of_string "@ " else "")
        (pFreeAlloc "allocates") a

  method assigns kw fmt = function
    | WritesAny -> ()
    | Writes [] -> fprintf fmt "@[%a %a;@]"
                     self#pp_acsl_keyword kw self#pp_acsl_keyword "\\nothing"
    | Writes l ->
      let without_result =
        List.filter
          (function (a,_) -> not (Logic_const.is_exit_status a.it_content))
          l
      in
      let unique_assigned_locs =
        let same t1 t2 = Cil_datatype.Term.equal t1.it_content t2.it_content in
        let f l (a,_) = if List.exists (same a) l then l else a :: l in
        List.rev (List.fold_left f [] without_result)
      in
      fprintf fmt "@[<h>%t%a@]"
        (fun fmt -> if without_result <> [] then
            Format.fprintf fmt "%a " self#pp_acsl_keyword kw)
        (Pretty_utils.pp_list ~sep:",@ " ~suf:";@]"
           (fun fmt t -> self#identified_term fmt t))
        unique_assigned_locs

  method private assigns_deps kw fmt = function
    | WritesAny -> ()
    | Writes [] as a -> self#assigns kw fmt a
    | Writes l as a ->
      fprintf fmt "@[<v>%a%a@]"
        (self#assigns kw) a
        (Pretty_utils.pp_list ~pre:"@ @[" ~sep:"@\n" (self#from kw))
        (List.filter (fun (_, f) -> f <> FromAny) l);

  method from kw fmt (base,deps) = match deps with
    | FromAny -> ()
    | From l ->
      fprintf fmt "@[<hv 2>@[<h>%a@ %a@]@ @[<h>%a %a@];@]"
        self#pp_acsl_keyword kw
        self#identified_term base
        self#pp_acsl_keyword "\\from"
        (Pretty_utils.pp_list ~sep:",@ " ~empty:"\\nothing" self#identified_term) l

  (* not enclosed in a box *)
  method private terminates_decreases ~extra_nl nl fmt (terminates, variant) =
    let nl_terminates = nl || variant != None in
    let pp_opt nl fmt =
      let suf = if nl then format_of_string "@]@\n" else "@]" in
      Pretty_utils.pp_opt ~suf fmt
    in
    fprintf fmt "%a%a%(%)"
      (pp_opt nl_terminates self#terminates) terminates
      (pp_opt nl self#decreases) variant
      (format_of_string
         (if extra_nl && nl && (variant != None || terminates != None)
          then format_of_string "@\n"
          else ""))

  (* not enclosed in a box *)
  method private behavior_contents ~extra_nl nl ?terminates ?variant fmt b =
    self#set_current_behavior b;
    (* Template for correct line breaks:
       let nl_line_n = nb_line_(n+1) || is_empty clause_line_(n+1) *)
    let nl_assigns = nl || b.b_allocation != FreeAllocAny in
    let nl_extended = nl_assigns || b.b_assigns != WritesAny in
    let nl_ensures = nl_extended || b.b_extended != [] in
    let nl_decreases = nl_ensures || b.b_post_cond != [] in
    let nl_requires = nl_decreases || variant != None || terminates != None in
    let nl_assumes = nl_requires || b.b_requires != [] in
    let pp_list nl fmt =
      let suf = if nl then format_of_string "@]@\n" else "@]" in
      Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@\n" ~suf fmt
    in
    fprintf fmt "%a%a%a%a%a%a%(%)%a%(%)%(%)"
      (pp_list nl_assumes self#assumes) b.b_assumes
      (pp_list nl_requires self#requires) b.b_requires
      (self#terminates_decreases ~extra_nl:false nl_decreases)
      (terminates, variant)
      (pp_list nl_ensures self#post_cond) b.b_post_cond
      (pp_list nl_extended self#extended) b.b_extended
      (self#assigns_deps "assigns") b.b_assigns
      (format_of_string
         (if nl_assigns && b.b_assigns != WritesAny
          then format_of_string "@\n" else ""))
      (self#allocation ~isloop:false) b.b_allocation
      (format_of_string
         (if nl && b.b_allocation != FreeAllocAny
          then format_of_string "@\n" else ""))
      (format_of_string
         (if extra_nl && (nl_assumes || b.b_assumes != [])
          then format_of_string "@\n" else ""));
    self#reset_current_behavior ()

  method behavior fmt b =
    fprintf fmt "@[<v>%a %s:@;<1 2>@[%a@]@]"
      self#pp_acsl_keyword "behavior"
      b.b_name
      (self#behavior_contents ~extra_nl:false false
         ?terminates:None ?variant:None)
      b

  method funspec fmt ({ spec_behavior = behaviors;
                        spec_variant = variant;
                        spec_terminates = terminates;
                        spec_complete_behaviors = complete;
                        spec_disjoint_behaviors = disjoint } as spec) =
    let pp_list ?(extra_nl=false) nl fmt =
      let suf =
        if nl then
          if extra_nl then format_of_string "@]@\n@\n" else "@]@\n"
        else "@]"
      in
      let sep = if extra_nl then format_of_string "@\n@\n" else "@\n" in
      Pretty_utils.pp_list ~pre:"@[<v>" ~sep ~suf fmt
    in
    fprintf fmt "@[<v>";
    let default_bhv = Cil.find_default_behavior spec in
    let other_bhvs =
      List.filter (fun b -> not (Cil.is_default_behavior b)) behaviors
    in
    let nl_complete = disjoint != [] in
    let nl_other_bhvs = nl_complete || complete != [] in
    let nl_default = nl_other_bhvs || other_bhvs != [] in
    (match default_bhv with
     | None ->
       self#terminates_decreases ~extra_nl:nl_default nl_default fmt
         (terminates, variant)
     | Some b
       when b.b_assumes == [] && b.b_requires == [] && b.b_post_cond == []
            && b.b_extended == []
            && b.b_allocation == FreeAllocAny && b.b_assigns == WritesAny ->
       self#terminates_decreases ~extra_nl:nl_default nl_default fmt
         (terminates, variant)
     | Some b ->
       self#behavior_contents
         ~extra_nl:nl_default nl_default ?terminates ?variant fmt b);
    fprintf fmt "%a%a%a@]"
      (pp_list ~extra_nl:true nl_other_bhvs self#behavior) other_bhvs
      (pp_list nl_complete self#complete_behaviors) complete
      (pp_list false self#disjoint_behaviors) disjoint


  (* TODO: add the annot ID in debug mode?*)
  method code_annotation fmt ca =
    let pp_for_behavs fmt l =
      match l with
      | [] -> ()
      | l ->
        Format.fprintf fmt "%a @[%a@]:@ "
          self#pp_acsl_keyword "for"
          (Pretty_utils.pp_list ~sep:",@ " pp_print_string) l
    in
    match ca.annot_content with
    | AAssert (behav,p) ->
      fprintf fmt "@[<2>%a%a@ %a;@]"
        pp_for_behavs behav
        self#pp_acsl_keyword (string_of_assert p.tp_kind)
        self#predicate p.tp_statement
    | AStmtSpec(for_bhv, spec) ->
      fprintf fmt "@[<hv 2>%a%a@]"
        pp_for_behavs for_bhv
        self#funspec spec
    | AAssigns(behav,a) ->
      fprintf fmt "@[<2>%a%a@]"
        pp_for_behavs behav
        (self#assigns_deps "loop assigns") a
    | AAllocation(behav,af) ->
      fprintf fmt "@[<2>%a%a@]"
        pp_for_behavs behav
        (self#allocation ~isloop:true) af
    | AInvariant(behav,true, i) ->
      fprintf fmt "@[<2>%a%a%a@ %a;@]"
        pp_for_behavs behav
        self#pp_predicate_kind i.tp_kind
        self#pp_acsl_keyword "loop invariant"
        self#predicate i.tp_statement
    | AInvariant(behav,false,i) ->
      fprintf fmt "@[<2>%a%a%a@ %a;@]"
        pp_for_behavs behav
        self#pp_predicate_kind i.tp_kind
        self#pp_acsl_keyword "invariant"
        self#predicate i.tp_statement
    | AVariant v ->
      self#variant fmt v
    | AExtended (behav, is_loop, e) ->
      let prefix = if is_loop then "loop " else "" in
      fprintf fmt "@[<2>%a%s%a@]"
        pp_for_behavs behav prefix
        self#extended e

  method private logicPrms fmt arg =
    let pvar fmt = self#logic_var fmt arg in
    self#logic_type (Some pvar) fmt arg.lv_type

  method private polyTypePrms fmt tvars =
    Pretty_utils.pp_list ~pre:"<@[" ~suf:"@]>" ~sep:",@ "
      pp_print_string fmt tvars

  method logic_builtin_label fmt l =
    let s = match l with
      | Here -> "Here"
      | Old -> "Old"
      | Pre -> "Pre"
      | Post -> "Post"
      | LoopEntry -> "LoopEntry"
      | LoopCurrent -> "LoopCurrent"
      | Init -> "Init"
    in
    pp_print_string fmt s

  method logic_label fmt lab =
    match lab with
    | BuiltinLabel l -> self#logic_builtin_label fmt l
    | FormalLabel s -> pp_print_string fmt s
    | StmtLabel sref ->
      let rec pickLabel = function
        | [] -> None
        | Label (l, _, _) :: _ -> Some l
        | _ :: rest -> pickLabel rest
      in
      let s = match pickLabel !sref.labels with
        | Some l -> l
        | None -> "__invalid_label"
      in
      pp_print_string fmt s

  method private labels fmt labels =
    match labels with
    | [ l ] when
        Cil_datatype.Logic_label.equal current_label l
        && not state.print_cil_as_is
      -> ()
    | _ ->
      Pretty_utils.pp_list ~pre:"{@[" ~suf:"@]}" ~sep:",@ "
        self#logic_label fmt labels

  method model_info fmt mfi =
    let print_decl fmt = self#model_field fmt mfi in
    fprintf fmt "@[%a %a@ @[<2>{@ %a@ };@]"
      self#pp_acsl_keyword "model"
      (self#typ None) mfi.mi_base_type
      (self#logic_type (Some print_decl)) mfi.mi_field_type

  method private pp_loader fmt loader =
    if Datatype.String.equal loader.loader_plugin "kernel"
    then pp_print_string fmt loader.loader_name
    else fprintf fmt "\\%s::%s" loader.loader_plugin loader.loader_name

  method global_annotation fmt = function
    | Dtype_annot (a,_) ->
      let old_label = current_label in
      (match a.l_labels with [l] -> current_label <- l | _ -> ());
      fprintf fmt "@[<hv 2>@[%a %a%a=@]@ %a;@]@\n"
        self#pp_acsl_keyword "type invariant"
        self#logic_var a.l_var_info
        (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@] " ~sep:",@ "
           self#logicPrms) a.l_profile
        self#predicate (pred_body a.l_body);
      current_label <- old_label
    | Dmodel_annot (mfi,_) ->
      self#model_info fmt mfi
    | Dinvariant (pred,_) ->
      let old_label = current_label in
      (match pred.l_labels with [l] -> current_label <- l | _ -> ());
      fprintf fmt "@[<hv 2>@[%a %a:@]@ %a;@]@\n"
        self#pp_acsl_keyword "global invariant"
        self#logic_var pred.l_var_info
        self#predicate (pred_body pred.l_body);
      current_label <- old_label
    | Dlemma(name, labels, tvars, pred, _attr, _) ->
      (* attributes are meant to be purely internal for now. *)
      let old_lab = current_label in
      fprintf fmt "@[<hv 2>@[<hov 1>%a %a%a%a:@]@ %t%a;@]@\n"
        self#pp_lemma_kind pred.tp_kind
        self#logic_name name
        self#labels labels
        self#polyTypePrms tvars
        (* pretty printing of lemma statement is done inside an environment
           with potentially active labels. However, we want the declaration of
           the label to be explicit. Hence current_label must be updated after
           the pretty-printing of labels, and before pretty-printing predicate
        *)
        (fun _ -> (match labels with [l] -> current_label <- l | _ -> ()))
        self#predicate pred.tp_statement;
      current_label <- old_lab
    | Dtype (ti,_) ->
      fprintf fmt "@[<hv 2>@[%a %a%a%a;@]@\n"
        self#pp_acsl_keyword "type"
        self#logic_name ti.lt_name self#polyTypePrms ti.lt_params
        (fun fmt -> function
           | None -> fprintf fmt "@]"
           | Some d -> fprintf fmt " =@]@ %a" self#logic_type_def d)
        ti.lt_def
    | Dfun_or_pred (li,_) ->
      (match li.l_type with
       | Some rt ->
         fprintf fmt "@[<hov 2>@[%a %a"
           self#pp_acsl_keyword "logic"
           (self#logic_type None) rt
       | None ->
         (match li.l_body with
          | LBinductive _ ->
            fprintf fmt "@[<hv 2>@[%a" self#pp_acsl_keyword "inductive"
          | _ ->
            fprintf fmt
              "@[<hv 2>@[<hov 2>%a" self#pp_acsl_keyword "predicate"));
      fprintf fmt "@ %a@,%a@,%a@,%a"
        self#logic_var li.l_var_info
        self#labels li.l_labels
        self#polyTypePrms li.l_tparams
        (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@] " ~sep:",@ "
           self#logicPrms)
        li.l_profile;
      (* Except for inductive definitions, where this must be done for
         each individual case that declare a unique label, if the predicate
         declares a single logic label, it can be chosen as the current one
         to optimize calls. *)
      let old_lab = current_label in
      (match li.l_body with
       | LBnone -> fprintf fmt ";@]"
       | LBreads reads ->
         (match li.l_labels with | [ l ] -> current_label <- l | _ -> ());
         (match reads with
          | [] ->
            fprintf fmt "@]@\n@[%a %a;@]"
              self#pp_acsl_keyword "reads" self#pp_acsl_keyword "\\nothing"
          | _ ->
            fprintf fmt "@]@\n@[%a@ %a;@]"
              self#pp_acsl_keyword "reads"
              (Pretty_utils.pp_list
                 ~sep:",@ "
                 (fun fmt x -> self#term fmt x.it_content)) reads)
       | LBpred def ->
         (match li.l_labels with | [ l ] -> current_label <- l | _ -> ());
         fprintf fmt "=@]@ %a;"
           self#predicate def
       | LBinductive indcases ->
         fprintf fmt "{@]@ %a}"
           (Pretty_utils.pp_list ~pre:"@[<v 0>" ~suf:"@]@\n" ~sep:"@\n"
              (fun fmt (id,labels,tvars,p) ->
                 Format.fprintf fmt "%a %s%a%a: @[%t%a@];"
                   self#pp_acsl_keyword "case"
                   id
                   self#labels labels
                   self#polyTypePrms tvars
                   (fun _ -> (match labels with [l]-> current_label<-l |_-> ()))
                   self#predicate p;
                 (* we restore the current_label between two cases, since
                    they are completely independent *)
                 current_label <- old_lab))
           indcases
       | LBterm def ->
         (match li.l_labels with | [ l ] -> current_label <- l | _ -> ());
         fprintf fmt "=@]@ %a;"
           (self#term_prec Precedence.binderLevel) def);
      fprintf fmt "@]@\n";
      current_label <- old_lab
    | Dvolatile(tsets,rvi_opt,wvi_opt,_attr, _) ->
      (* attributes are meant to be purely internal for now. *)
      let pp_vol txt fmt = function
        | None -> () ;
        | Some vi -> fprintf fmt "@ %s %a" txt self#varinfo vi
      in
      fprintf fmt "@[<hov 2>%a@ %a%a%a;@]"
        self#pp_acsl_keyword "volatile"
        (Pretty_utils.pp_list ~sep:",@ "
           (fun fmt x -> self#term fmt x.it_content))
        tsets
        (pp_vol "reads") rvi_opt
        (pp_vol "writes") wvi_opt ;
    | Daxiomatic(id,decls,_attr, _) ->
      (* attributes are meant to be purely internal for now. *)
      fprintf fmt "@[@[<v 2>@[%a %s {@]@\n%a@]@\n@]}@\n"
        self#pp_acsl_keyword "axiomatic" id
        (Pretty_utils.pp_list ~pre:"@[<v 0>" ~suf:"@]" ~sep:"@\n"
           self#global_annotation)
        decls
    | Dmodule(id, _, _, Some loader, _)
      when not Kernel.(is_debug_key_enabled dkey_print_imported_modules) ->
      fprintf fmt "@[<hov 2>%a %a: %s %a _ ;@]@\n"
        self#pp_acsl_keyword "import"
        self#pp_loader loader id
        self#pp_acsl_keyword "\\as"
    | Dmodule(id, decls, _attr, loader, _) ->
      begin
        (* attributes are meant to be purely internal for now. *)
        fprintf fmt "@[@[<v 2>@[" ;
        if Kernel.(is_debug_key_enabled dkey_print_imported_modules) then
          Option.iter (fprintf fmt "// import %a:@\n" self#pp_loader) loader ;
        fprintf fmt "%a %a {@]"
          self#pp_acsl_keyword "module" self#logic_name id ;
        try
          Stack.push (id ^ "::") module_stack ;
          fprintf fmt "@\n%a@]@\n@]}@\n"
            (Pretty_utils.pp_list ~pre:"@[<v 0>" ~suf:"@]" ~sep:"@\n"
               self#global_annotation)
            decls ;
          ignore @@ Stack.pop module_stack ;
        with err ->
          ignore @@ Stack.pop module_stack ;
          raise err
      end
    | Dextended (e,_attr,_) -> self#extended fmt e

  method logic_type_def fmt = function
    | LTsum l ->
      Pretty_utils.pp_list ~sep:"@ |@ "
        (fun fmt info ->
           fprintf fmt "%s@[%a@]" info.ctor_name
             (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]" ~sep:",@ "
                (self#logic_type None)) info.ctor_params) fmt l
    | LTsyn typ ->
      self#logic_type None fmt typ

  method file fmt file =
    fprintf fmt "@[/* Generated by Frama-C */@\n" ;
    print_std_includes fmt file.globals;
    Cil.iterGlobals file (fun g -> self#global fmt g);
    fprintf fmt "@]@."

end (* class cil_printer *)

include Printer_builder.Make(struct class printer = cil_printer end)

(* initializing Cil's forward references *)
let () = Cil.pp_typ_ref := pp_typ
let () = Cil.pp_binop_ref := pp_binop
let () = Cil.pp_global_ref := pp_global
let () = Cil.pp_exp_ref := pp_exp
let () = Cil.pp_lval_ref := pp_lval
let () = Cil.pp_ikind_ref := pp_ikind
let () = Cil.pp_attribute_ref := pp_attribute
let () = Cil.pp_attributes_ref := pp_attributes
let () = Cil.pp_term_ref := pp_term
let () = Cil.pp_logic_type_ref := pp_logic_type
let () = Cil.pp_identified_term_ref := pp_identified_term
let () = Cil.pp_from_ref := pp_from
let () = Cil.pp_behavior_ref := pp_behavior
let () = Cil.pp_block_ref := pp_block
let () = Cil.pp_varinfo_ref := pp_varinfo
