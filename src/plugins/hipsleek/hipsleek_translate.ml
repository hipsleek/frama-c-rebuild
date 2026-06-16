open Cil_types

(* ------------------------------------------------------------------ *)
(* SL annotation extraction                                            *)
(*                                                                     *)
(* Pre/post specs use /*[SL] ... */:                                   *)
(*   /*[SL]                                                            *)
(*      requires x::ll<>                                               *)
(*      ensures  res::ll<>;                                            *)
(*   */                                                                *)
(*   int length(node* x) { ... }                                       *)
(*                                                                     *)
(* Predicate/view definitions use /*[SL_pred] ... */:                 *)
(*   /*[SL_pred]                                                       *)
(*      ll<> == self = null                                            *)
(*        or self::node_star<p> * p::node<_,q> * q::ll<>;             *)
(*   */                                                                *)
(*                                                                     *)
(* Specs are associated to the immediately following function.        *)
(* Predicate blocks are emitted verbatim at the top of the .ss file.  *)
(* ------------------------------------------------------------------ *)

let line_of_pos content pos =
  let n = ref 1 in
  for i = 0 to pos - 1 do
    if content.[i] = '\n' then incr n
  done;
  !n

(* Scan file content for blocks starting with marker, return (end_line, body) list. *)
let extract_tagged_blocks marker filename =
  if not (Sys.file_exists filename) then []
  else
    let ic = open_in filename in
    let content =
      let buf = Buffer.create 4096 in
      (try while true do
           Buffer.add_string buf (input_line ic);
           Buffer.add_char buf '\n'
         done
       with End_of_file -> ());
      close_in ic;
      Buffer.contents buf
    in
    let len = String.length content in
    let mlen = String.length marker in
    let result = ref [] in
    let rec scan i =
      if i + mlen > len then ()
      else if String.sub content i mlen = marker then begin
        let rec find_end j =
          if j + 1 >= len then len
          else if content.[j] = '*' && content.[j + 1] = '/' then j + 2
          else find_end (j + 1)
        in
        let end_pos = find_end (i + mlen) in
        let raw = String.sub content (i + mlen) (end_pos - i - mlen - 2) in
        let body = String.trim raw in
        let end_line = line_of_pos content end_pos in
        result := (end_line, body) :: !result;
        scan end_pos
      end
      else scan (i + 1)
    in
    scan 0;
    !result

(* Pre/post spec blocks: /*[SL] ... */ immediately before a function. *)
let extract_sl_annotations filename =
  extract_tagged_blocks "/*[SL]\n" filename

(* Predicate/view definition blocks: /*[SL_pred] ... */ emitted at top of .ss. *)
let extract_sl_preds filename =
  List.map snd (extract_tagged_blocks "/*[SL_pred]\n" filename)

(* Find the closest /*[SL]*/ block that ends before func_line.
   This tolerates any number of lines between the SL block and the
   function (e.g. an ACSL /*@ ... */ comment in between). *)
let find_annotation annotations func_line =
  let preceding =
    List.filter (fun (el, _) -> el < func_line) annotations
  in
  match List.sort (fun (a, _) (b, _) -> compare b a) preceding with
  | [] -> None
  | (_, body) :: _ -> Some body

(* ------------------------------------------------------------------ *)
(* Type translation                                                     *)
(*                                                                     *)
(* In HipSleek's .ss format, C pointer types T* are represented as    *)
(* a wrapper data type "T_star" with a single field "pdata : T".      *)
(* Predefined pointer types: void_star, int_star, char_star.           *)
(* ------------------------------------------------------------------ *)

let rec translate_typ t =
  match t.tnode with
  | TVoid            -> "void"
  | TInt IBool       -> "bool"
  | TInt _           -> "int"
  | TFloat _         -> "float"
  | TPtr { tnode = TVoid; _ }  -> "void_star"
  | TPtr { tnode = TInt IChar; _ } -> "char_star"
  | TPtr t'          -> translate_typ t' ^ "_star"
  | TComp ci         -> ci.cname
  | TNamed ti        -> translate_typ ti.ttype
  | TArray _         -> "int_star"  (* not in subset *)
  | TFun _           -> "void_star" (* not in subset *)
  | TEnum _          -> "int"
  | TBuiltin_va_list -> "void_star"


(* ------------------------------------------------------------------ *)
(* Expression translation                                               *)
(* ------------------------------------------------------------------ *)

let translate_binop = function
  | PlusA | PlusPI   -> "+"
  | MinusA | MinusPI | MinusPP -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | Mod   -> "%"
  | Shiftlt -> "<<"
  | Shiftrt -> ">>"
  | Lt    -> "<"
  | Gt    -> ">"
  | Le    -> "<="
  | Ge    -> ">="
  | Eq    -> "=="
  | Ne    -> "!="
  | BAnd  -> "&"
  | BXor  -> "^"
  | BOr   -> "|"
  | LAnd  -> "&&"
  | LOr   -> "||"

let rec translate_exp e =
  match e.enode with
  | Const(CInt64(n, _, _)) ->
    Z.to_string n
  | Const(CChr c) ->
    string_of_int (Char.code c)
  | Const(CReal(_, _, Some s)) -> s
  | Const(CReal(f, _, None))   -> string_of_float f
  | Const(CEnum ei) ->
    translate_exp ei.eival
  | Lval lv ->
    translate_lval lv
  | BinOp(op, e1, e2, _) ->
    "(" ^ translate_exp e1 ^ " " ^ translate_binop op ^ " " ^ translate_exp e2 ^ ")"
  | UnOp(Neg, e, _)  -> "(-" ^ translate_exp e ^ ")"
  | UnOp(BNot, e, _) -> "(~" ^ translate_exp e ^ ")"
  | UnOp(LNot, e, _) -> "(!" ^ translate_exp e ^ ")"
  (* NULL pointer cast to null *)
  | CastE({ tnode = TPtr _; _ }, { enode = Const(CInt64(n, _, _)); _ })
    when Z.equal n Z.zero ->
    "null"
  | CastE(_, e) ->
    translate_exp e
  | AddrOf lv  -> "(&" ^ translate_lval lv ^ ")"
  | StartOf lv -> translate_lval lv
  | SizeOf _   -> "/*sizeof*/"
  | SizeOfE _  -> "/*sizeof*/"
  | AlignOf _  -> "/*alignof*/"
  | AlignOfE _ -> "/*alignof*/"

(* Translate an lval.
   Key difference from C: p->field (Mem p, Field fi) becomes p.pdata.field
   because in .ss format, a C pointer variable p is of type T_star, and
   field access goes via the .pdata indirection. *)
and translate_lval = function
  | Var vi, NoOffset                  -> vi.vname
  | Var vi, Field(fi, NoOffset)       -> vi.vname ^ "." ^ fi.fname
  | Var vi, Index(e, NoOffset)        -> vi.vname ^ "[" ^ translate_exp e ^ "]"
  | Mem e,  NoOffset                  -> translate_exp e ^ ".pdata"
  | Mem e,  Field(fi, NoOffset)       -> translate_exp e ^ ".pdata." ^ fi.fname
  | _                                 -> "/*unsupported_lval*/"

(* ------------------------------------------------------------------ *)
(* Statement translation                                                *)
(*                                                                     *)
(* Frama-C normalises all functions to a single-return form:           *)
(*   __retres = e;  goto return_lbl;  ...  return_lbl: return __retres *)
(* We detect this pattern and reconstruct "return e;" so the .ss file  *)
(* contains idiomatic code that HipSleek can parse.                    *)
(* ------------------------------------------------------------------ *)

let retres_name = "__retres"

let is_retres_return stmt =
  match stmt.skind with
  | Return(Some { enode = Lval(Var vi, NoOffset); _ }, _)
    when vi.vname = retres_name -> true
  | _ -> false

let is_retres_goto stmt =
  match stmt.skind with
  | Goto(tgt, _) -> is_retres_return !tgt
  | _ -> false

let rec translate_stmts buf indent stmts =
  match stmts with
  | [] -> ()
  | s :: rest ->
    match s.skind with
    | Instr(Set((Var vi, NoOffset), e, _))
      when vi.vname = retres_name ->
      (match rest with
       | next_s :: rest2
         when is_retres_goto next_s || is_retres_return next_s ->
         let pad = String.make indent ' ' in
         Buffer.add_string buf (pad ^ "return " ^ translate_exp e ^ ";\n");
         translate_stmts buf indent rest2
       | _ ->
         translate_stmt buf indent s;
         translate_stmts buf indent rest)
    | _ when is_retres_return s ->
      translate_stmts buf indent rest
    | _ ->
      translate_stmt buf indent s;
      translate_stmts buf indent rest

and translate_stmt buf indent stmt =
  let pad = String.make indent ' ' in
  match stmt.skind with
  | Instr i ->
    translate_instr buf pad i
  | Return(None, _) ->
    Buffer.add_string buf (pad ^ "return;\n")
  | Return(Some e, _) ->
    Buffer.add_string buf (pad ^ "return " ^ translate_exp e ^ ";\n")
  | If(e, bthen, belse, _) ->
    Buffer.add_string buf (pad ^ "if (" ^ translate_exp e ^ ") {\n");
    translate_stmts buf (indent + 2) bthen.bstmts;
    if belse.bstmts <> [] then begin
      Buffer.add_string buf (pad ^ "} else {\n");
      translate_stmts buf (indent + 2) belse.bstmts
    end;
    Buffer.add_string buf (pad ^ "}\n")
  | Loop(_, body, _, _, _) ->
    Buffer.add_string buf (pad ^ "while (1) {\n");
    translate_stmts buf (indent + 2) body.bstmts;
    Buffer.add_string buf (pad ^ "}\n")
  | Break _    -> Buffer.add_string buf (pad ^ "break;\n")
  | Continue _ -> Buffer.add_string buf (pad ^ "continue;\n")
  | Block b ->
    translate_stmts buf indent b.bstmts
  | UnspecifiedSequence seq ->
    List.iter (fun (s, _, _, _, _) -> translate_stmt buf indent s) seq
  | Goto _         -> ()
  | Switch _       -> Buffer.add_string buf (pad ^ "/* switch: not in subset */\n")
  | Throw _        -> Buffer.add_string buf (pad ^ "/* throw: not in subset */\n")
  | TryCatch _     -> Buffer.add_string buf (pad ^ "/* try-catch: not in subset */\n")
  | TryFinally _   -> Buffer.add_string buf (pad ^ "/* try-finally: not in subset */\n")
  | TryExcept _    -> Buffer.add_string buf (pad ^ "/* try-except: not in subset */\n")

and translate_instr buf pad = function
  | Set(lv, e, _) ->
    Buffer.add_string buf
      (pad ^ translate_lval lv ^ " = " ^ translate_exp e ^ ";\n")
  | Call(ret, lhost, args, _) ->
    let callee = match lhost with
      | Var vi  -> vi.vname
      | Mem e   -> "(*" ^ translate_exp e ^ ")"
    in
    let call_str =
      callee ^ "(" ^ String.concat ", " (List.map translate_exp args) ^ ")"
    in
    (match ret with
     | None    -> Buffer.add_string buf (pad ^ call_str ^ ";\n")
     | Some lv ->
       Buffer.add_string buf
         (pad ^ translate_lval lv ^ " = " ^ call_str ^ ";\n"))
  | Local_init(vi, AssignInit(SingleInit e), _) ->
    Buffer.add_string buf
      (pad ^ vi.vname ^ " = " ^ translate_exp e ^ ";\n")
  | Local_init(vi, ConsInit(ctor, args, _), _) ->
    let call_str =
      ctor.vname ^ "(" ^ String.concat ", " (List.map translate_exp args) ^ ")"
    in
    Buffer.add_string buf (pad ^ vi.vname ^ " = " ^ call_str ^ ";\n")
  | Local_init(_, AssignInit(CompoundInit _), _) ->
    Buffer.add_string buf (pad ^ "/* compound init: not in subset */\n")
  | Asm _       -> Buffer.add_string buf (pad ^ "/* asm: not in subset */\n")
  | Skip _      -> ()
  | Code_annot _ -> ()

(* ------------------------------------------------------------------ *)
(* Struct definition translation                                        *)
(*                                                                     *)
(* For each struct T, emit:                                            *)
(*   data T { field1 ...; field2 ...; }   (pointer fields → T_star)   *)
(*   data T_star { T pdata; }             (the pointer wrapper)        *)
(* ------------------------------------------------------------------ *)

let translate_compinfo buf ci =
  if ci.cstruct then begin
    Buffer.add_string buf ("data " ^ ci.cname ^ " {\n");
    let fields = Option.value ~default:[] ci.cfields in
    List.iter (fun fi ->
        Buffer.add_string buf
          ("  " ^ translate_typ fi.ftype ^ " " ^ fi.fname ^ ";\n")
      ) fields;
    Buffer.add_string buf "}\n\n";
    Buffer.add_string buf ("data " ^ ci.cname ^ "_star {\n");
    Buffer.add_string buf ("  " ^ ci.cname ^ " pdata;\n");
    Buffer.add_string buf "}\n\n"
  end

(* ------------------------------------------------------------------ *)
(* Function definition translation                                      *)
(* ------------------------------------------------------------------ *)

let translate_fundec buf annotations fundec loc =
  let func_line = Fileloc.line loc in
  let spec = find_annotation annotations func_line in
  let ret_typ = match fundec.svar.vtype.tnode with
    | TFun(rt, _, _) -> translate_typ rt
    | _              -> translate_typ fundec.svar.vtype
  in
  let params =
    List.map (fun vi -> translate_typ vi.vtype ^ " " ^ vi.vname) fundec.sformals
  in
  Buffer.add_string buf
    (ret_typ ^ " " ^ fundec.svar.vname
     ^ "(" ^ String.concat ", " params ^ ")\n");
  (match spec with
   | None      -> ()
   | Some body ->
     List.iter (fun line ->
         let line = String.trim line in
         if line <> "" then
           Buffer.add_string buf ("  " ^ line ^ "\n")
       ) (String.split_on_char '\n' body));
  Buffer.add_string buf "{\n";
  let visible_locals =
    List.filter (fun vi -> vi.vname <> retres_name) fundec.slocals
  in
  List.iter (fun vi ->
      Buffer.add_string buf
        ("  " ^ translate_typ vi.vtype ^ " " ^ vi.vname ^ ";\n")
    ) visible_locals;
  if visible_locals <> [] then Buffer.add_string buf "\n";
  translate_stmts buf 2 fundec.sbody.bstmts;
  Buffer.add_string buf "}\n\n"

(* ------------------------------------------------------------------ *)
(* Top-level file translation                                           *)
(* ------------------------------------------------------------------ *)

let source_files_of globals =
  let seen = Hashtbl.create 8 in
  List.filter_map (fun g ->
      let loc_opt = match g with
        | GFun(_, l) | GVar(_, _, l) | GCompTag(_, l)
        | GCompTagDecl(_, l) | GType(_, l) | GFunDecl(_, _, l) -> Some l
        | _ -> None
      in
      match loc_opt with
      | None -> None
      | Some loc ->
        let path = Filepath.to_string (Fileloc.path loc) in
        if Hashtbl.mem seen path then None
        else begin Hashtbl.add seen path (); Some path end
    ) globals

let translate file =
  let buf = Buffer.create 4096 in
  let src_files = source_files_of file.globals in
  (* Emit SL_PRED view definitions verbatim at the top. *)
  let preds =
    List.concat_map extract_sl_preds src_files
  in
  List.iter (fun pred_body ->
      Buffer.add_string buf (pred_body ^ "\n\n")
    ) preds;
  let annotations =
    List.concat_map extract_sl_annotations src_files
  in
  List.iter (fun g ->
      match g with
      | GCompTag(ci, _)     -> translate_compinfo buf ci
      | GFun(fundec, loc)   -> translate_fundec buf annotations fundec loc
      | _                   -> ()
    ) file.globals;
  Buffer.contents buf
