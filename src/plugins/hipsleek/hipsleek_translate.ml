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

(* One /*[SL...]*/ comment block found in a source file.
   [blk_start] is the C line of the first non-blank body line (used to map spec
   clauses back to source); [blk_end] is the line of the closing "*/".
   [blk_file] keeps blocks from different source files apart -- line numbers
   alone are ambiguous once several files are translated together. *)
type sl_block = {
  blk_file  : string;
  blk_start : int;
  blk_end   : int;
  blk_body  : string;
}

(* Scan file content for blocks starting with marker. *)
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
        (* Skip leading whitespace/newlines so the start line is the first real
           body line, independent of how the block is laid out. *)
        let lead = ref 0 in
        while !lead < String.length raw &&
              (match raw.[!lead] with ' ' | '\t' | '\n' | '\r' -> true | _ -> false)
        do incr lead done;
        let start_line = line_of_pos content (i + mlen + !lead) in
        let end_line = line_of_pos content end_pos in
        result := { blk_file = filename; blk_start = start_line;
                    blk_end = end_line; blk_body = body } :: !result;
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
  List.map (fun b -> b.blk_body)
    (extract_tagged_blocks "/*[SL_pred]\n" filename)

(* Loop-spec blocks: /*[SL_loop] ... */ placed immediately before a while loop. *)
let extract_sl_loops filename =
  extract_tagged_blocks "/*[SL_loop]\n" filename

(* All /*[SL_loop]*/ blocks, set per [translate] run. The statement translator
   (top-level [let rec], not a closure) reads this from the Loop case, mirroring
   the warn_acc / linemap_acc accumulator pattern below. *)
let loop_annots : sl_block list ref = ref []

(* Blocks already attached to a function or a loop. A spec block describes
   exactly one thing, so consuming it on first use is what stops a later
   *unannotated* function/loop from silently adopting its neighbour's spec and
   being "verified" against a contract the user never wrote for it. *)
let used_blocks : sl_block list ref = ref []

(* Find the closest unconsumed block in [file] ending before [line].
   Searching for the closest preceding block (rather than requiring strict line
   adjacency) is deliberate: it tolerates an ACSL /*@ ... */ comment sitting
   between the SL block and the function. [after] bounds the search from below,
   which is how a loop spec is kept inside its own function's body. *)
let find_annotation ?(after = 0) blocks ~file ~line =
  let cands =
    List.filter
      (fun b ->
         b.blk_file = file && b.blk_end < line && b.blk_start > after
         && not (List.memq b !used_blocks))
      blocks
  in
  match List.sort (fun a b -> compare b.blk_end a.blk_end) cands with
  | [] -> None
  | b :: _ -> used_blocks := b :: !used_blocks; Some (b.blk_start, b.blk_body)

(* ------------------------------------------------------------------ *)
(* Translation-fidelity warnings                                       *)
(*                                                                     *)
(* The C/Cil AST -> HIP .ss translation drops or coarsens a number of *)
(* constructs (casts, globals, sizeof, switch, goto, nested lvalues,   *)
(* ...). When that happens inside a function body, a green HIP verdict *)
(* attests to a .ss that differs from the C. We accumulate a note per  *)
(* function (deduplicated) so the caller can surface "verified, but    *)
(* the .ss differs from your C here".                                  *)
(*                                                                     *)
(* Translation is sequential (one function at a time), so a single     *)
(* module-level accumulator, reset by translate_fundec, is sufficient. *)
(* ------------------------------------------------------------------ *)

let warn_acc : string list ref = ref []
let add_warn msg =
  if not (List.mem msg !warn_acc) then warn_acc := msg :: !warn_acc

(* Count newlines in a string (= number of complete lines). *)
let count_lines s =
  String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 s

(* Maps a generated-.ss line (relative to the current procedure buffer) to the
   originating C source line, so proof obligations (keyed by .ss line) can be
   reported against the user's code. Reset per function by translate_fundec. *)
let linemap_acc : (int * int) list ref = ref []
(* Record "the .ss line about to be written maps to C source line [cl]". *)
let record_cline buf cl =
  if cl > 0 then
    linemap_acc := (count_lines (Buffer.contents buf) + 1, cl) :: !linemap_acc
let record_line buf stmt =
  record_cline buf (Fileloc.line (Cil_datatype.Stmt.loc stmt))

(* First line of the function currently being translated, set by
   translate_fundec. The Loop case uses it as a lower bound when looking up a
   /*[SL_loop]*/ block, which is what keeps a loop spec from being picked up
   across a function boundary. *)
let cur_fn_start = ref 0

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
  | TArray _         ->
    add_warn "array type approximated as a pointer (size/elements lost)";
    "int_star"  (* not in subset *)
  | TFun _           ->
    add_warn "function-pointer type approximated as void*";
    "void_star" (* not in subset *)
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
    add_warn "cast(s) erased during translation (no width/sign/pointer reinterpretation)";
    translate_exp e
  | AddrOf lv  -> "(&" ^ translate_lval lv ^ ")"
  | StartOf lv -> translate_lval lv
  | SizeOf _   -> add_warn "sizeof not modelled"; "/*sizeof*/"
  | SizeOfE _  -> add_warn "sizeof not modelled"; "/*sizeof*/"
  | AlignOf _  -> add_warn "alignof not modelled"; "/*alignof*/"
  | AlignOfE _ -> add_warn "alignof not modelled"; "/*alignof*/"

(* Translate an lval.
   Key difference from C: p->field (Mem p, Field fi) becomes p.pdata.field
   because in .ss format, a C pointer variable p is of type T_star, and
   field access goes via the .pdata indirection. *)
and translate_lval lv =
  let note_global vi =
    if vi.vglob then
      add_warn
        (Printf.sprintf
           "references global '%s' (globals are not translated to the .ss)"
           vi.vname)
  in
  match lv with
  | Var vi, NoOffset                  -> note_global vi; vi.vname
  | Var vi, Field(fi, NoOffset)       -> note_global vi; vi.vname ^ "." ^ fi.fname
  | Var vi, Index(e, NoOffset)        ->
    note_global vi; vi.vname ^ "[" ^ translate_exp e ^ "]"
  | Mem e,  NoOffset                  -> translate_exp e ^ ".pdata"
  | Mem e,  Field(fi, NoOffset)       -> translate_exp e ^ ".pdata." ^ fi.fname
  | _                                 ->
    add_warn "unsupported lvalue (nested field/offset or multi-level deref) \
              emitted as a placeholder";
    "/*unsupported_lval*/"

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

(* Is [b] a block whose only statement is a break (possibly nested in blocks)? *)
let rec block_lone_break b =
  match b.bstmts with
  | [ s ] ->
    (match s.skind with
     | Break _  -> true
     | Block b' -> block_lone_break b'
     | _        -> false)
  | _ -> false

(* Cil normalizes `while(c) body` to a `Loop` whose first statement guards the
   exit: `if (c) {} else { break; }` (the guard's false branch breaks).
   Short-circuit guards nest that shape rather than building a BinOp, so
     while (a && b)  ->  if (a) { if (b) {} else break; } else break;
     while (a || b)  ->  if (a) {} else { if (b) {} else break; }
   and longer chains nest further. [guard_of_stmt] walks that tree back into a
   single expression. Reconstructing `a && b` is only sound because the shape
   only arises for side-effect-free Cil expressions: anything needing a
   temporary (`while (f(x) && y)`) is hoisted by Cil into an Instr ahead of the
   If, which matches no case here and falls back to `while (1)`. *)
let rec guard_of_stmt s =
  match s.skind with
  | Block b -> guard_of_block b
  | If(c, bthen, belse, _) ->
    let cs = translate_exp c in
    if bthen.bstmts = [] && block_lone_break belse then
      Some cs                                    (* if(c){}else break; -> c    *)
    else if belse.bstmts = [] && block_lone_break bthen then
      Some ("!(" ^ cs ^ ")")                     (* if(c) break;       -> !c   *)
    else if block_lone_break belse then
      (* if(c){ <chain> } else break;  ->  c && <chain> *)
      Option.map (fun g -> "(" ^ cs ^ " && " ^ g ^ ")") (guard_of_block bthen)
    else if bthen.bstmts = [] then
      (* if(c){} else { <chain> }      ->  c || <chain> *)
      Option.map (fun g -> "(" ^ cs ^ " || " ^ g ^ ")") (guard_of_block belse)
    else None
  | _ -> None

and guard_of_block b =
  match b.bstmts with
  | [ s ] -> guard_of_stmt s
  | _     -> None

(* Recover the guard text and the remaining real body statements. Returns None
   when the head doesn't match, so the caller can fall back to `while (1)`. *)
let recover_while_guard body =
  match body.bstmts with
  | head :: rest -> Option.map (fun g -> (g, rest)) (guard_of_stmt head)
  | []           -> None

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
         (* Record against the "__retres = e" statement's C location so the
            reconstructed "return e;" (and the heap obligations its dereferences
            generate) map to the C return line, not the previous statement. *)
         record_line buf s;
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
  record_line buf stmt;
  match stmt.skind with
  | Instr i ->
    translate_instr buf pad i
  | Return(None, _) ->
    Buffer.add_string buf (pad ^ "return;\n")
  | Return(Some e, _) ->
    Buffer.add_string buf (pad ^ "return " ^ translate_exp e ^ ";\n")
  | If(e, bthen, belse, _) ->
    let cond = translate_exp e in
    (* hip's .ss parser rejects an empty block, so an `if` with an empty arm is
       never emitted verbatim: Cil produces exactly that for short-circuit
       guards. Cil expressions are side-effect free, so dropping/inverting an
       empty arm preserves meaning. *)
    if bthen.bstmts = [] && belse.bstmts = [] then ()
    else if bthen.bstmts = [] then begin
      Buffer.add_string buf (pad ^ "if (!(" ^ cond ^ ")) {\n");
      emit_body buf (indent + 2) pad belse.bstmts;
      Buffer.add_string buf (pad ^ "}\n")
    end else begin
      Buffer.add_string buf (pad ^ "if (" ^ cond ^ ") {\n");
      emit_body buf (indent + 2) pad bthen.bstmts;
      if belse.bstmts <> [] then begin
        Buffer.add_string buf (pad ^ "} else {\n");
        emit_body buf (indent + 2) pad belse.bstmts
      end;
      Buffer.add_string buf (pad ^ "}\n")
    end
  | Loop(_, body, loc, _, _) ->
    (* Fall back to `while (1)` (keeping Cil's exit `if`/`break` in the body)
       when the guard can't be recovered. The loop spec is emitted either way:
       dropping it would leave the loop unspecified and silently change what is
       being proved. *)
    let guard, inner =
      match recover_while_guard body with
      | Some (guard, inner) -> guard, inner
      | None ->
        add_warn "could not recover while-loop guard; emitted while(1)";
        "1", body.bstmts
    in
    Buffer.add_string buf (pad ^ "while (" ^ guard ^ ")\n");
    (* Attach the closest unconsumed /*[SL_loop]*/ block ending before the
       loop's line and inside the enclosing function, emitting its
       requires/ensures clauses between the guard and the body. *)
    (match
       find_annotation !loop_annots ~after:!cur_fn_start
         ~file:(Filepath.to_string (Fileloc.path loc))
         ~line:(Fileloc.line loc)
     with
     | None ->
       add_warn "while loop has no /*[SL_loop]*/ spec; HipSleek may reject it"
     | Some (start_line, spec_body) ->
       (* Map each loop spec clause to its C source line, so the loop's
          PRE/POST obligations (keyed by HipSleek to these .ss spec lines)
          point at the /*[SL_loop]*/ clauses, mirroring how the function
          spec is recorded in translate_fundec. *)
       List.iteri (fun k raw ->
           let line = String.trim raw in
           if line <> "" then begin
             record_cline buf (start_line + k);
             Buffer.add_string buf (pad ^ "  " ^ line ^ "\n")
           end)
         (String.split_on_char '\n' spec_body));
    Buffer.add_string buf (pad ^ "{\n");
    emit_body buf (indent + 2) pad inner;
    Buffer.add_string buf (pad ^ "}\n")
  | Break _    -> Buffer.add_string buf (pad ^ "break;\n")
  | Continue _ -> Buffer.add_string buf (pad ^ "continue;\n")
  | Block b ->
    translate_stmts buf indent b.bstmts
  | UnspecifiedSequence seq ->
    List.iter (fun (s, _, _, _, _) -> translate_stmt buf indent s) seq
  | Goto _         -> add_warn "goto dropped (only the single-return pattern is reconstructed)"
  | Switch _       ->
    add_warn "switch not in subset (dropped)";
    Buffer.add_string buf (pad ^ "/* switch: not in subset */\n")
  | Throw _        ->
    add_warn "exceptions not in subset (dropped)";
    Buffer.add_string buf (pad ^ "/* throw: not in subset */\n")
  | TryCatch _     ->
    add_warn "exceptions not in subset (dropped)";
    Buffer.add_string buf (pad ^ "/* try-catch: not in subset */\n")
  | TryFinally _   ->
    add_warn "exceptions not in subset (dropped)";
    Buffer.add_string buf (pad ^ "/* try-finally: not in subset */\n")
  | TryExcept _    ->
    add_warn "exceptions not in subset (dropped)";
    Buffer.add_string buf (pad ^ "/* try-except: not in subset */\n")

(* Emit the statements of a block, substituting a no-op when the translation
   produces nothing: hip's .ss parser rejects `{}`, and a block can come out
   empty either from Cil (short-circuit guards) or from a construct the
   translation drops (goto, Skip). Appending to [buf] rather than a scratch
   buffer keeps record_cline's .ss line numbering intact. *)
and emit_body buf indent pad stmts =
  let before = Buffer.length buf in
  translate_stmts buf indent stmts;
  if Buffer.length buf = before then
    Buffer.add_string buf (pad ^ "  ;\n")

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
    add_warn "compound initializer not in subset (dropped)";
    Buffer.add_string buf (pad ^ "/* compound init: not in subset */\n")
  | Asm _       ->
    add_warn "inline asm not in subset (dropped)";
    Buffer.add_string buf (pad ^ "/* asm: not in subset */\n")
  | Skip _      -> ()
  | Code_annot _ -> add_warn "inline ACSL annotation ignored"

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

(* Translate one function into its own buffer and return
   (name, sl_spec_text option, generated .ss procedure text, fidelity warnings). *)
let translate_fundec spec_info fundec loc =
  warn_acc := [];
  let buf = Buffer.create 512 in
  let func_line = Fileloc.line loc in
  (* Seed the line map so obligations before any statement (e.g. POST at the
     ensures line) still resolve to the function's C declaration line. *)
  linemap_acc := [ (1, func_line) ];
  cur_fn_start := func_line;
  let spec = Option.map snd spec_info in
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
  (match spec_info with
   | None      -> ()
   | Some (start_line, body) ->
     (* Each emitted spec clause is mapped to its C source line, so POST/PRE
        obligations (which HipSleek keys to the ensures/requires .ss line) point
        at the SL spec rather than collapsing onto the declaration line. *)
     List.iteri (fun k raw_line ->
         let line = String.trim raw_line in
         if line <> "" then begin
           record_cline buf (start_line + k);
           Buffer.add_string buf ("  " ^ line ^ "\n")
         end
       ) (String.split_on_char '\n' body));
  Buffer.add_string buf "{\n";
  let body_start = Buffer.length buf in
  let visible_locals =
    List.filter (fun vi -> vi.vname <> retres_name) fundec.slocals
  in
  List.iter (fun vi ->
      Buffer.add_string buf
        ("  " ^ translate_typ vi.vtype ^ " " ^ vi.vname ^ ";\n")
    ) visible_locals;
  if visible_locals <> [] then Buffer.add_string buf "\n";
  translate_stmts buf 2 fundec.sbody.bstmts;
  (* hip rejects an empty procedure body just as it rejects any empty block. *)
  if Buffer.length buf = body_start then Buffer.add_string buf "  ;\n";
  Buffer.add_string buf "}\n\n";
  (fundec.svar.vname, spec, Buffer.contents buf,
   List.rev !warn_acc, List.rev !linemap_acc)

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

(* Result of translating a whole file:
   - full_ss   : the complete generated .ss program (fed to hip)
   - preds     : the [SL_pred] view-definition blocks (verbatim)
   - functions : per-function (name, sl_spec_text option, generated .ss proc text)
   - ss_spans  : per-function (name, start_line, end_line) within full_ss; used to
                 map HipSleek proof-log entries (keyed by .ss line) back to functions
   - fidelity  : per-function (name, translation-fidelity warnings)
   - linemaps  : per-function (name, (ss_line_relative_to_proc, c_source_line) list),
                 to report obligations against the user's C source *)
type translation = {
  full_ss   : string;
  preds     : string list;
  functions : (string * string option * string) list;
  ss_spans  : (string * int * int) list;
  fidelity  : (string * string list) list;
  linemaps  : (string * (int * int) list) list;
}

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
  (* Make /*[SL_loop]*/ blocks available to the statement translator's Loop case. *)
  loop_annots := List.concat_map extract_sl_loops src_files;
  used_blocks := [];
  (* Assign each /*[SL]*/ block to at most one function, walking functions in
     source order. Doing this up front rather than during the globals iteration
     below keeps the assignment independent of the order Frama-C hands us
     globals in, which the consume-once rule would otherwise depend on. *)
  let fn_specs =
    let fns =
      List.filter_map (function
          | GFun(fd, loc) ->
            Some (fd.svar.vname,
                  Filepath.to_string (Fileloc.path loc),
                  Fileloc.line loc)
          | _ -> None)
        file.globals
    in
    let fns =
      List.sort (fun (_, f1, l1) (_, f2, l2) -> compare (f1, l1) (f2, l2)) fns
    in
    List.filter_map (fun (name, file, line) ->
        Option.map (fun s -> (name, s))
          (find_annotation annotations ~file ~line))
      fns
  in
  let functions = ref [] in
  let ss_spans = ref [] in
  let fidelity = ref [] in
  let linemaps = ref [] in
  List.iter (fun g ->
      match g with
      | GCompTag(ci, _)     -> translate_compinfo buf ci
      | GFun(fundec, loc)   ->
        let spec_info = List.assoc_opt fundec.svar.vname fn_specs in
        let (name, spec, proc_text, warnings, linemap) =
          translate_fundec spec_info fundec loc in
        (* The procedure occupies lines [start_line, end_line] in full_ss. *)
        let start_line = count_lines (Buffer.contents buf) + 1 in
        Buffer.add_string buf proc_text;
        let end_line = count_lines (Buffer.contents buf) in
        functions := (name, spec, proc_text) :: !functions;
        ss_spans  := (name, start_line, end_line) :: !ss_spans;
        linemaps  := (name, linemap) :: !linemaps;
        if warnings <> [] then fidelity := (name, warnings) :: !fidelity
      | _                   -> ()
    ) file.globals;
  { full_ss = Buffer.contents buf;
    preds;
    functions = List.rev !functions;
    ss_spans  = List.rev !ss_spans;
    fidelity  = List.rev !fidelity;
    linemaps  = List.rev !linemaps }
