(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Pasting module. *)

open Cil_types

(*-----------------------------------------------------------------------*)

let dkey = Options.register_category "trace-table"

let hidden_attr = "hidden"
let () = Ast_attributes.register AttrUnknown hidden_attr
let loop_body_attr = "acsl!loop_body!"
let loop_number_attr = "acsl!loop_number!"

(** To get a fresh attribute name for a loop body inside a function *)
let loop_body_attr_name n =
  loop_body_attr ^ (string_of_int n)

(** To get a fresh attribute name for a loop number inside a function *)
let loop_number_attr_name n =
  loop_number_attr ^ (string_of_int n)

module S_Stmt = Cil_datatype.Stmt.Set
module Statement:sig
  (** find the ith loop of a function *)
  val find_loop_stmt_set: int -> kernel_function -> Cil_datatype.Stmt.Set.t

  (** find the ith loop body of a function *)
  val find_body_stmt_set:int -> kernel_function -> Cil_datatype.Stmt.Set.t

  (** find the ith asm stmt of a function *)
  val find_asm_stmt_set:int -> kernel_function -> Cil_datatype.Stmt.Set.t

  (** find the ith call stmt of a function *)
  val find_call_stmt_set:int -> kernel_function -> Cil_datatype.Stmt.Set.t

  (** find the ith/all direct/indirect call stmt of a function *)
  val find_call2_stmt_set:kernel_function option -> int -> kernel_function -> Cil_datatype.Stmt.Set.t

  (** find ith statement of a function *)
  val find_stmt: int -> kernel_function -> stmt

  (** Clear the memoized tables. *)
  val clear_temporary_table: unit -> unit

end
=
struct
  (** Iter on statements of a kernel function *)
  let iter_from_func f kf =
    let definition = Kernel_function.get_definition kf
    and visitor = object
      inherit Cil.nopCilVisitor as super
      method! vstmt stmt = f stmt; super#vstmt stmt
      (* speed up: skip non interesting subtrees *)
      method! vvdec _ = SkipChildren (* via visitCilFunction *)
      method! vspec _ = SkipChildren (* via visitCilFunction *)
      method! vcode_annot _ = SkipChildren (* via Code_annot stmt and Loop stmt *)
      method! vinst _ = SkipChildren (* via stmt such as Instr *)
      method! vexpr _ = SkipChildren (* via stmt such as Return, IF, ... *)
      method! vlval _ = SkipChildren (* via stmt such as Set, Call, Asm, ... *)
      method! vattr _ = SkipChildren (* via Asm stmt *)
    end
    in
    ignore (Cil.visitCilFunction (visitor:>Cil.cilVisitor) definition)

  exception FoundStmt of stmt

  module H_Int = Datatype.Int.Hashtbl
  module H_Int_S_Stmt = H_Int.Make(S_Stmt)

  let memoized_find find create find_kf replace_kf kf n compute =
    let tbl =
      try find_kf kf
      with Not_found ->
        let tbl = create 5
        in compute tbl ;
        replace_kf kf tbl ;
        tbl
    in find tbl n

  let on_acsl_attr tbl attr_regexp stmt battrs =
    List.iter (function (attr_name, args) when attr_name = hidden_attr ->
        List.iter (function AStr(name) ->
            (try (match Str.bounded_split_delim attr_regexp name 2 with
                   [ "" ; n ] ->
                   let n = int_of_string n in
                   let stmts =
                     try H_Int.find tbl n
                     with Not_found -> S_Stmt.empty
                   in
                   let stmts = S_Stmt.add (stmt ()) stmts
                   in H_Int.replace tbl n stmts
                 | _ -> ())
             with _ -> ())
                          | _ -> ()) args
                      | _ -> ()) battrs

  let find_from_func f kf =
    let f stmt = if f stmt then raise (FoundStmt stmt) in
    try
      iter_from_func f kf ;
      raise Not_found
    with FoundStmt stmt -> stmt

  (** Memoized loop number table. *)
  module Sloop =
    State_builder.Hashtbl
      (Cil_datatype.Kf.Hashtbl)
      (H_Int_S_Stmt)
      (struct
        let name = "LoopNumberIndex"
        let dependencies = [ ]
        let size = 7
      end)
  let _ = Ast.add_linked_state Sloop.self

  let find_loop_stmt_set n kf =
    memoized_find H_Int.find H_Int.create Sloop.find Sloop.replace kf n
      (fun tbl ->
         Options.debug ~level:2 ~dkey "Computing loop index table for function \"%a\""
           Kernel_function.pretty kf;
         let attr_regexp = Str.regexp_string loop_number_attr in
         let on_loop stmt = match stmt.skind with
           | Loop (_,{battrs},_,_,_) ->
             on_acsl_attr tbl attr_regexp (fun () -> stmt) battrs
           | _ -> ()
         in iter_from_func on_loop kf)

  (** Memoized loop body table. *)
  module Sbody =
    State_builder.Hashtbl
      (Cil_datatype.Kf.Hashtbl)
      (H_Int_S_Stmt)
      (struct
        let name = "LoopBodyIndex"
        let dependencies = [ ]
        let size = 7
      end)
  let _ = Ast.add_linked_state Sbody.self

  let find_body_stmt_set n kf =
    memoized_find H_Int.find H_Int.create Sbody.find Sbody.replace kf n
      (fun tbl ->
         Options.debug ~level:2 ~dkey "Computing loop body table for function \"%a\""
           Kernel_function.pretty kf;
         let attr_regexp = Str.regexp_string loop_body_attr in
         let on_body stmt = match stmt.skind with
           | Block {battrs} ->
             on_acsl_attr tbl attr_regexp (fun () -> stmt) battrs
           | _ -> ()
         in iter_from_func on_body kf)

  (** Memoized asm table. *)
  module Sasm =
    State_builder.Hashtbl
      (Cil_datatype.Kf.Hashtbl)
      (H_Int_S_Stmt)
      (struct
        let name = "AsmIndex"
        let dependencies = [ ]
        let size = 7
      end)
  let _ = Ast.add_linked_state Sasm.self

  let find_asm_stmt_set n kf =
    memoized_find H_Int.find H_Int.create Sasm.find Sasm.replace kf n
      (fun tbl ->
         Options.debug ~level:2 ~dkey "Computing asm call table for function \"%a\""
           Kernel_function.pretty kf;
         let cpt = ref 0 in
         let on_asm stmt = match stmt.skind with
           | Instr (Asm _) ->
             incr cpt ;
             H_Int.replace tbl !cpt (S_Stmt.singleton stmt)
           | _ -> ()
         in iter_from_func on_asm kf)

  (** Memoized call table. *)
  module P_S_Stmt = Datatype.Pair(S_Stmt)(S_Stmt)
  module H_Int_P_S_Stmt = H_Int.Make(P_S_Stmt)
  module Scall =
    State_builder.Hashtbl
      (Cil_datatype.Kf.Hashtbl)
      (H_Int_P_S_Stmt)
      (struct
        let name = "CallIndex"
        let dependencies = [ ]
        let size = 7
      end)
  let _ = Ast.add_linked_state Scall.self

  let find_call_stmt n kf =
    memoized_find H_Int.find H_Int.create Scall.find Scall.replace kf n
      (fun tbl ->
         Options.debug ~level:2 ~dkey "Computing call table1 for function \"%a\""
           Kernel_function.pretty kf;
         let cpt = ref 0 in
         let cpt_indirect = ref 0 in
         let indirect_calls = ref S_Stmt.empty in
         let get n = try H_Int.find tbl n with Not_found -> S_Stmt.empty, S_Stmt.empty in
         let on_call stmt = match stmt.skind with
           | Instr (Call (_, f, _ , _)) ->
             begin
               match Kernel_function.get_called f with
               | None ->
                 incr cpt_indirect ;
                 let all,indirect = get !cpt_indirect in
                 H_Int.replace tbl !cpt_indirect (all,(S_Stmt.add stmt indirect));
                 indirect_calls := (S_Stmt.add stmt !indirect_calls)
               | _ ->
                 incr cpt ;
                 let all,indirect = get !cpt in
                 H_Int.replace tbl !cpt ((S_Stmt.add stmt all), indirect) ;
             end
           | _ -> ()
         in iter_from_func on_call kf; H_Int.replace tbl 0 (S_Stmt.empty, !indirect_calls))

  let find_call_stmt_set n kf = fst (find_call_stmt n kf)

  (** Memoized call2 table. *)
  module H_Kf = Cil_datatype.Kf.Hashtbl
  module H_Kf_H_Int_S_Stmt = H_Kf.Make(H_Int_S_Stmt)
  module Scall2 =
    State_builder.Hashtbl
      (Cil_datatype.Kf.Hashtbl)
      (H_Kf_H_Int_S_Stmt)
      (struct
        let name = "Call2Index"
        let dependencies = [ ]
        let size = 7
      end)
  let _ = Ast.add_linked_state Scall2.self

  let find_call2_stmt called_kf kf =
    memoized_find H_Kf.find H_Kf.create Scall2.find Scall2.replace kf called_kf
      (fun tbl ->
         Options.debug ~level:2 ~dkey "Computing call table2 for function \"%a\""
           Kernel_function.pretty kf;
         let tbl_cpt = H_Kf.create 3 in
         let on_call stmt = match stmt.skind with
           | Instr (Call (_, f, _ , _)) -> begin
               match Kernel_function.get_called f with
               | Some called_kf ->
                 let cpt =
                   let cpt = try H_Kf.find tbl_cpt called_kf with Not_found -> 0 in
                   let cpt = cpt + 1 in
                   H_Kf.replace tbl_cpt called_kf cpt ; cpt
                 in
                 let tbl_stmt = try H_Kf.find tbl called_kf
                   with Not_found -> let tbl_stmt = H_Int.create 3 in
                     H_Kf.replace tbl called_kf tbl_stmt ; tbl_stmt
                 in
                 let s = try H_Int.find tbl_stmt cpt with Not_found -> S_Stmt.empty in
                 H_Int.replace tbl_stmt cpt (S_Stmt.add stmt s) ;
                 let s = try H_Int.find tbl_stmt 0 with Not_found -> S_Stmt.empty in
                 H_Int.replace tbl_stmt 0 (S_Stmt.add stmt s)
               | _ -> ()
             end
           | _ -> ()
         in iter_from_func on_call kf)

  let find_call2_stmt_set kf_opt n kf =
    match kf_opt with
    | None -> snd (find_call_stmt n kf)
    | Some called_kf -> H_Int.find (find_call2_stmt called_kf kf) n

  let find_stmt n kf =
    let nb = ref 0 in
    let is_stmt _stmt =
      incr nb ;
      !nb = n
    in find_from_func is_stmt kf

  (** Clear the memoized tables. *)
  let clear_temporary_table () =
    Options.debug ~level:2 ~dkey "Clear loop index table";
    Sloop.clear ();
    Options.debug ~level:2 ~dkey "Clear loop body table";
    Sbody.clear ();
    Options.debug ~level:2 ~dkey "Clear asm call table";
    Sasm.clear ();
    Options.debug ~level:2 ~dkey "Clear function call tables";
    Scall.clear ();
    Scall2.clear ()

end

(*-----------------------------------------------------------------------*)

module MacroIndex: sig
  (** Macro table management. *)

  type scope_t = Sfile | Smodule | Sfunction
  val pp_scope:  Format.formatter -> scope_t -> unit

  val dkey: Options.category

  (* val self : State.t
     (** Dependencies of the result of find_xxx functions. *)
  *)

  val clear_macro_table : scope_t -> unit
  (** To clear macro table in order to free memory,
      without clearing the result dependencies. *)

  val add_macro: scope_t -> string -> Logic_ptree.lexpr -> unit
  (** Modify the macro table,
      without clearing the result dependencies. *)

  val find_macro: scope_t -> string -> Logic_ptree.lexpr
  (** Find the macro definition.
      @raise Not_found for undefined macro.*)

end = struct

  type scope_t = Sfile | Smodule | Sfunction
  let pp_scope fmt = function
    | Sfile    -> Format.fprintf fmt "%s" "file"
    | Smodule  -> Format.fprintf fmt "%s" "module"
    | Sfunction -> Format.fprintf fmt "%s" "function"

  (** Memoized index macro table. *)
  module Sfile =
    State_builder.Hashtbl
      (Datatype.String.Hashtbl)
      (Cil_datatype.Lexpr)
      (struct
        let name = "FileMacroIndex"
        let dependencies = [ Ast.self ]
        let size = 3
      end)
  let _ = Ast.add_linked_state Sfile.self
  module Smodule =
    State_builder.Hashtbl
      (Datatype.String.Hashtbl)
      (Cil_datatype.Lexpr)
      (struct
        let name = "ModuleMacroIndex"
        let dependencies = [ Ast.self ]
        let size = 3
      end)
  let _ = Ast.add_linked_state Smodule.self
  module Sfunction =
    State_builder.Hashtbl
      (Datatype.String.Hashtbl)
      (Cil_datatype.Lexpr)
      (struct
        let name = "FunctionMacroIndex"
        let dependencies = [ Ast.self ]
        let size = 3
      end)
  let _ = Ast.add_linked_state Sfunction.self

  let find_macro scope m =
    try if scope = Sfunction then Sfunction.find m else raise Not_found
    with Not_found ->
    try if scope <> Sfile then Smodule.find m else raise Not_found
    with Not_found ->
      Sfile.find m

  let dkey = Options.register_category "trace-tables"

  let add_macro scope m def =
    Options.debug ~level:2 ~dkey "Add macro %S at %a scope"
      m  pp_scope scope;
    let replace = match scope with
      | Sfile     -> Sfile.replace
      | Smodule   -> Smodule.replace
      | Sfunction -> Sfunction.replace
    in replace m def

  (* let self = S.self *)

  (** Clear the memoized "MacroIndex" table in order to free memory,
      without clearing the result dependencies. *)
  let clear_macro_table scope =
    Options.debug ~level:2 ~dkey "Clear macro table from %a scope"
      pp_scope scope;
    if scope = Sfile then Sfile.clear () ;
    if scope <> Sfunction then Smodule.clear () ;
    Sfunction.clear () ;


end

module A2fc_inner_ast =
  State_builder.Option_ref(Cil_datatype.File)
    (struct
      let name = "A2fcPaste.A2fc_inner_ast"
      let dependencies = [ Ast.self ]
    end)
let _ = Ast.add_linked_state A2fc_inner_ast.self

module SymbolIndex: sig
  (** Symbol table management. *)

  val self : State.t
  (** Dependencies of the result of find_xxx functions. *)

  val clear_temporary_table : unit -> unit
  (** To clear temporary index table in order to free memory,
      without clearing the result dependencies. *)

  val find_kf : file:File.t option -> string -> Kernel_function.t
  (** Find [Kernel_function] related to a global annotation. *)

  val find_var_annot : file:File.t option -> Kernel_function.t ->  stmt ->
    ?label:string -> string -> logic_var
  (** Find variables related to a code annotation. *)

  val find_var_funspec : file:File.t option -> Kernel_function.t -> string -> logic_var
  (** Find variables related to a function contract. *)

  val find_var_global : file:File.t option -> string -> logic_var
  (** Find variables related to a global annotation. *)

  val find_enum_item : file:File.t option -> string -> exp * typ
  val find_type : file:File.t option -> Logic_typing.type_namespace -> string -> typ
end = struct

  module SymbolCode = struct

    type symbol_code =
      | SGdef | SGdec
      | SFdef | SFdec
      | STdef
      | SItem
      | SEdef | SEdec
      | SSdef | SSdec
      | SUdef | SUdec

    include Datatype.Make_with_collections
        (struct
          type t = symbol_code
          let name = "SymbolCode"
          let reprs = [ SGdef ]
          let structural_descr = Structural_descr.t_abstract
          let hash = function
            | SGdef -> 0 (* priority max *)
            | SGdec -> 1
            | SFdef -> 2
            | SFdec -> 3
            | STdef -> 4
            | SItem -> 5
            | SEdef -> 6
            | SEdec -> 7
            | SSdef -> 8
            | SSdec -> 9
            | SUdef -> 10
            | SUdec -> 11
          let compare s1 s2 = Stdlib.compare (hash s1) (hash s2)
          let equal s1 s2 = s1 = s2
          let copy id = id
          let rehash = Datatype.identity
          let pretty = Datatype.undefined
          let mem_project = Datatype.never_any_project
        end)

  end

  (** To encapsulate all global C symbols *)
  module Symbol = struct

    let dkey = Options.register_category "trace-symbols"

    type symbol =
      | Gdef of varinfo | Gdec of varinfo
      | Fdef of varinfo | Fdec of varinfo
      | Tdef of typeinfo
      | Item of enumitem
      | Edef of enuminfo | Edec of enuminfo
      | Sdef of compinfo | Sdec of compinfo
      | Udef of compinfo | Udec of compinfo

    let symbol_code = function
      | Gdef _ -> SymbolCode.SGdef | Gdec _ -> SymbolCode.SGdec
      | Fdef _ -> SymbolCode.SFdef | Fdec _ -> SymbolCode.SFdec
      | Tdef _ -> SymbolCode.STdef
      | Item _ -> SymbolCode.SItem
      | Edef _ -> SymbolCode.SEdef | Edec _ -> SymbolCode.SEdec
      | Sdef _ -> SymbolCode.SSdef | Sdec _ -> SymbolCode.SSdec
      | Udef _ -> SymbolCode.SUdef | Udec _ -> SymbolCode.SUdec

    let encode = function
      | GType (t,_) ->
        Options.debug ~level:2 ~dkey
          "Type: %S@." t.torig_name;
        Some (t.torig_name,
              Tdef (t))
      | GCompTag (c,_) ->
        Options.debug ~level:2 ~dkey
          "CompTag: %S@." c.corig_name;
        Some (c.corig_name,
              if c.cstruct then Sdef (c) else Udef (c))
      | GCompTagDecl (c,_)  ->
        Options.debug ~level:2 ~dkey
          "CompTagDecl: %S@." c.cname;
        Some (c.cname,
              if c.cstruct then Sdec (c)else Udec (c))
      | GEnumTag (e,_) ->
        Options.debug ~level:2 ~dkey
          "EnumTag: %S@." e.eorig_name;
        Some (e.eorig_name,
              Edef (e))
      | GEnumTagDecl (e,_) ->
        Options.debug ~level:2 ~dkey
          "EnumTagDecl: %S@." e.eorig_name;
        Some (e.eorig_name,
              Edec (e))
      | GVarDecl (vi,_) ->
        if vi.vglob then
          (Options.debug ~level:2 ~dkey
             "VarDecl: %S@." vi.vorig_name;
           Some (vi.vorig_name, Gdec (vi)))
        else None
      | GFunDecl (_,vi,_) ->
        if vi.vglob then
          (Options.debug ~level:2 ~dkey
             "FunDecl: %S@." vi.vorig_name;
           Some (vi.vorig_name,Fdec (vi)))
        else None
      | GVar (vi,_,_) ->
        if vi.vglob then
          (Options.debug ~level:2 ~dkey
             "Var: %S@." vi.vorig_name;
           Some (vi.vorig_name, Gdef (vi)))
        else None
      | GFun (f,_) ->
        if f.svar.vglob then
          (Options.debug ~level:2 ~dkey
             "Fun: %S@." f.svar.vorig_name;
           Some (f.svar.vorig_name, Fdef (f.svar)))
        else None
      | _ -> None

    include Datatype.Make_with_collections
        (struct
          type t = symbol
          let name = "Symbol"
          let reprs = List.map (fun v -> Gdef v) Cil_datatype.Varinfo.reprs
          let structural_descr = Structural_descr.t_abstract

          let compare s1 s2 =
            match s1, s2 with
            | ( Gdef (v1), Gdef (v2) | Gdec (v1), Gdec (v2)
              | Fdef (v1), Fdef (v2) | Fdec (v1), Fdec (v2)) ->
              Cil_datatype.Varinfo.compare v1 v2

            | Tdef (t1), Tdef (t2) ->
              Cil_datatype.Typeinfo.compare t1 t2
            | Item (i1), Item (i2) ->
              Cil_datatype.Enumitem.compare i1 i2
            | ( Edef (e1), Edef (e2) | Edec (e1), Edec (e2)) ->
              Cil_datatype.Enuminfo.compare e1 e2
            | ( Sdef (c1), Sdef (c2) | Sdec (c1), Sdec (c2)
              | Udef (c1), Udef (c2) | Udec (c1), Udec (c2)) ->
              Cil_datatype.Compinfo.compare c1 c2
            | _,_ ->
              SymbolCode.compare (symbol_code (s1)) (symbol_code (s2))

          let hash s1 =
            let h1 = match s1 with
              | ( Gdef (v1) | Gdec (v1) | Fdef (v1) | Fdec (v1)) ->
                Cil_datatype.Varinfo.hash v1
              | Tdef (t1) -> Cil_datatype.Typeinfo.hash t1
              | Item (i1) -> Cil_datatype.Enumitem.hash i1
              | ( Edef (e1) | Edec (e1)) -> Cil_datatype.Enuminfo.hash e1
              | ( Sdef (c1) | Sdec (c1)
                | Udef (c1) | Udec (c1)) -> Cil_datatype.Compinfo.hash c1
            in
            let h2 = SymbolCode.hash (symbol_code (s1)) in
            Hashtbl.hash (h1, h2)

          let equal s1 s2 =
            match s1, s2 with
            | ( Gdef (v1), Gdef (v2) | Gdec (v1), Gdec (v2)
              | Fdef (v1), Fdef (v2) | Fdec (v1), Fdec (v2)) ->
              Cil_datatype.Varinfo.equal v1 v2
            | Tdef (t1), Tdef (t2) ->
              Cil_datatype.Typeinfo.equal t1 t2
            | Item (i1), Item (i2) ->
              Cil_datatype.Enumitem.equal i1 i2
            | ( Edef (e1), Edef (e2) | Edec (e1), Edec (e2)) ->
              Cil_datatype.Enuminfo.equal e1 e2
            |(Sdef c1, Sdef c2 | Sdec c1, Sdef c2 | Udef c1, Udef c2
             | Udec c1, Udec c2) ->
              Cil_datatype.Compinfo.equal c1 c2
            | _,_ -> false

          let copy = Datatype.undefined
          let rehash = Datatype.identity
          let pretty = Datatype.undefined
          let mem_project = Datatype.never_any_project
        end)
  end

  module H_String = Datatype.String.Hashtbl
  module S_Symbol = Symbol.Set
  module M_SymbolCode = SymbolCode.Map
  module S_SymbolCode = SymbolCode.Set
  module H_String2S_Symbol = Datatype.String.Hashtbl.Make(S_Symbol)
  module M_SymbolCode2H_String2S_Symbol =
    SymbolCode.Map.Make(H_String2S_Symbol)

  (** Memoized index symbol table:
      orig_name -hash-> SymbolCode.t -map-> filename -hash-> Set of Symbol.t
      Note:
      -hash-> is used as -map-> (only one binding)
      -map -> is used when iteration order should use the key order *)
  module S =
    State_builder.Hashtbl
      (Datatype.String.Hashtbl)
      (M_SymbolCode2H_String2S_Symbol)
      (struct
        let name = "SymbolIndex"
        let dependencies = [ ]
        let size = 7
      end)
  let _ = Ast.add_linked_state S.self

  module First =
    State_builder.True_ref
      (struct
        let dependencies = [ S.self ]
        let name = "SymbolIndex.compute"
      end)

  let apply_once f =
    (fun () ->
       if First.get () then begin
         First.set false;
         try
           f ();
           assert (First.get () = false)
         with exn ->
           First.set true;
           raise exn
       end),
    First.self

  (** Compute once the index symbol table. *)
  let compute, self =
    let compute () =
      Options.debug
        ~level:2 ~dkey:MacroIndex.dkey "Indexing the C symbol table...";
      let ast =
        try A2fc_inner_ast.get ()
        with Not_found ->
          Options.fatal "No AST registered in ACSL importer plug-in"
      in
      Cil.iterGlobals
        ast
        (fun glob ->
           match Symbol.encode glob with
           | None -> ()
           | Some (name,symb) ->
             let file = Filepath.to_string
                 Cil_datatype.(Global.loc glob |> Fileloc.path)
             in
             let index na sy =
               let code = Symbol.symbol_code sy in
               let update_map htable symbols old_map =
                 H_String.replace htable file symbols ;
                 M_SymbolCode.add code htable old_map
               in
               let new_index old_map =
                 let htable = H_String.create(5) in
                 update_map htable (S_Symbol.singleton sy) old_map
               in
               ignore (S.memo
                         ~change:(fun old_map ->
                             try
                               let htable = M_SymbolCode.find code old_map in
                               let symbols =
                                 try H_String.find htable file
                                 with Not_found -> S_Symbol.empty
                               in
                               update_map htable (S_Symbol.add sy symbols) old_map
                             with Not_found -> new_index old_map)
                         (fun _ -> new_index M_SymbolCode.empty)
                         na) ;
             in
             index name symb; (* indexing the symbol *)
             match symb with
             | Symbol.Edef (enum) ->
               List.iter
                 (fun it -> index it.eiorig_name (Symbol.Item (it)))
                 enum.eitems
             | _ -> ()

        )
    in
    apply_once compute

  (** Clear the memoized [SymbolIndex] table and the fact it hash been computed
      in order to free memory. *)
  let clear_temporary_table () =
    Options.debug ~level:2 ~dkey "Clear symbol table";
    First.set true ;
    S.clear () ;
    Statement.clear_temporary_table ()

  exception FoundSymbol of Symbol.t

  (** Find a [Symbol.t] from an original name having the highest priority among
      [kinds], and lookup for [Symbol.t] used into [file] first when given. *)
  let find ~kinds ~file name =
    compute () ;
    let map = S.find name in (* raises Not_found when there is no symbol
                                entry for that name *)
    let find_first_symbol () =
      try
        S_SymbolCode.iter (* use the priority order *)
          (fun code ->
             try let htable = M_SymbolCode.find code map in
               H_String.iter
                 (fun _f set -> S_Symbol.iter (fun s -> raise(FoundSymbol s)) set)
                 htable
             with Not_found -> ())
          kinds ;
        raise Not_found
      with FoundSymbol symb -> symb
    in
    match file with
    | None -> find_first_symbol ()
    | Some file -> let file = (File.get_name file) in
      try
        S_SymbolCode.iter (* use the priority order *)
          (fun code ->
             try
               let htable = M_SymbolCode.find code map in
               let symbols = H_String.find htable file in
               S_Symbol.iter (fun s -> raise(FoundSymbol s)) symbols
             with Not_found -> ())
          kinds ;
        find_first_symbol ()
      with FoundSymbol symb -> symb

  let make_kinds kinds =
    List.fold_left
      (fun acc sc -> S_SymbolCode.add sc acc)
      S_SymbolCode.empty
      kinds

  let var_kinds = make_kinds [SymbolCode.SGdef ; SymbolCode.SGdec ;
                              SymbolCode.SFdef; SymbolCode.SFdec]
  let kf_kinds = make_kinds [SymbolCode.SFdef; SymbolCode.SFdec]
  let item_kinds = S_SymbolCode.singleton SymbolCode.SItem
  let type_kinds = S_SymbolCode.singleton SymbolCode.STdef
  let struct_kinds = make_kinds [SymbolCode.SSdef ; SymbolCode.SSdec]
  let union_kinds =  make_kinds [SymbolCode.SUdef ; SymbolCode.SUdec]
  let enum_kinds =  make_kinds [SymbolCode.SEdef ; SymbolCode.SEdec]

  let find_varinfo ~file name =
    match find ~kinds:var_kinds ~file name
    with
    | ( Symbol.Gdef (vi)
      | Symbol.Gdec (vi)
      | Symbol.Fdef (vi)
      | Symbol.Fdec (vi)) -> vi
    | _ -> (* IMPOSSIBLE *) assert false

  let find_kf_varinfo ~file name =
    match find ~kinds:kf_kinds ~file name
    with
    | ( Symbol.Fdef (vi)
      | Symbol.Fdec (vi)) -> vi
    | _ -> (* IMPOSSIBLE *) assert false

  let find_enum_item ~file name =
    match find ~kinds:item_kinds ~file name
    with
    | Symbol.Item (item) ->
      (* [VP 2013-11-06] an enumerated constant has the corresponding
         integral type, not an enumerated type. *)
      let typ = Cil_const.mk_tint item.eihost.ekind in
      let exp = Cil.new_exp ~loc:Fileloc.unknown (Const (CEnum (item)))
      in
      Options.debug ~level:2 ~dkey "Found enum item of name %s: symbol=%a type=%a@."
        name Printer.pp_exp exp Printer.pp_logic_type (Ctype typ);
      exp, typ

    | _ -> (* IMPOSSIBLE *) assert false

  let find_typedef_type ~file name =
    match find ~kinds:type_kinds ~file name
    with
    | Symbol.Tdef (ti) -> ti.ttype
    | _ -> (* IMPOSSIBLE *) assert false

  let find_struct_type ~file name =
    match find ~kinds:struct_kinds ~file name
    with
    | (Symbol.Sdef (ci) | Symbol.Sdec (ci)) ->
      Cil_const.mk_tcomp ci
    | _ -> (* IMPOSSIBLE *) assert false

  let find_union_type ~file name =
    match find ~kinds:union_kinds ~file name
    with
    | (Symbol.Udef (ci) | Symbol.Udec (ci)) ->
      Cil_const.mk_tcomp ci
    | _ -> (* IMPOSSIBLE *) assert false

  let find_enum_type ~file name =
    match find ~kinds:enum_kinds ~file name
    with
    | (Symbol.Edef (e) | Symbol.Edec (e)) ->
      Cil_const.mk_tenum e
    | _ -> (* IMPOSSIBLE *) assert false

  let find_type ~file tkind name =
    let find_type = match tkind with
      | Logic_typing.Typedef -> find_typedef_type
      | Logic_typing.Struct  -> find_struct_type
      | Logic_typing.Union   -> find_union_type
      | Logic_typing.Enum    -> find_enum_type
    in find_type ~file name

  let lookup vars x = List.find (fun vi -> vi.vorig_name = x) vars


  (** Find variables related to a global annotation. *)
  let find_var_global ~file x =
    let vi = (* look at file first *)
      find_varinfo ~file x
    in
    Cil.cvar_to_lvar vi

  (** Find [Kernel_function] related to a global annotation. *)
  let find_kf ~file x =
    let vi = (* look at file first *)
      find_kf_varinfo ~file x
    in
    Globals.Functions.get vi


  (** Find variables related to a function contract. *)
  let find_var_funspec ~file kf x =
    let vi =
      try
        lookup (Kernel_function.get_formals kf) x
      with Not_found ->
        find_varinfo ~file x
    in
    Cil.cvar_to_lvar vi


  (** Find variables related to a code annotation. *)
  let find_var_annot ~file kf stmt ?label var =
    let scope =
      match label with
      | None | Some "Here" | Some "Old" | Some "Post" -> Block_scope stmt
      | Some "Pre" ->
        let stmt = Kernel_function.find_first_stmt kf in Block_scope stmt
      | Some "Init" -> Program
      | Some "LoopEntry" | Some "LoopCurrent" ->
        let stmt = Kernel_function.find_enclosing_loop kf stmt in
        Block_scope stmt
      | Some lab ->
        let stmt = Kernel_function.find_label kf lab in Block_scope !stmt
    in
    let vi = Globals.Syntactic_search.find_in_scope var scope in
    let vi = match vi with
      | Some vi -> vi
      | None -> find_varinfo ~file var
    in
    Cil.cvar_to_lvar vi
end

(*-----------------------------------------------------------------------*)

let buffer = Buffer.create 80
let add_buffer s =
  Buffer.add_string buffer s

(*-----------------------------------------------------------------------*)

let prop_file = ref Filepath.empty (* file containing the property *)
let prop_line = ref 0  (* line containing the first property token *)
let buff_line = ref 0  (* line containing the first character of
                          the string to parse with acsl parser *)

let set_prop_loc file line =
  prop_file := file ;
  prop_line := line

let set_buff_loc line =
  buff_line := line ;
  Buffer.clear buffer

(** Get location of the first character of
    the string to parse with acsl parser *)
let get_buff_loc () =
  Filepos.make ~path:!prop_file ~line:!buff_line ~column:0 ~offset:0 ()

(** Get location of the first property token *)
let get_prop_loc () =
  Filepos.make ~path:!prop_file ~line:!prop_line ~column:0 ~offset:0 ()

(*-----------------------------------------------------------------------*)
let basename_chop_extension file =
  let basename = Filename.basename file in
  try
    Filename.chop_extension basename
  with
    Invalid_argument _ -> basename

let current_scope = ref MacroIndex.Sfile
let current_module = ref None
let current_function = ref None

let dkey = Options.register_category "trace-actions"

let set_current_scope scope =
  Options.debug ~level:2 ~dkey "Set current scope to %a@."
    MacroIndex.pp_scope scope ;
  MacroIndex.clear_macro_table scope ;
  current_scope := scope

let set_current_module ~is_from_file_name m =
  Options.debug ~level:2 ~dkey "Set current module to %S@." m ;
  set_current_scope (if is_from_file_name
                     then MacroIndex.Sfile
                     else MacroIndex.Smodule) ;
  current_function := None ;
  current_module :=
    if m = "" then None
    else
      try
        let file =
          Some (List.find
                  (fun file -> m = basename_chop_extension (File.get_name file))
                  (File.get_all ()))
        in
        Options.debug ~level:2 ~dkey "MODULE %s@." m ;
        file
      with Not_found -> None

let find_kf fct = SymbolIndex.find_kf ~file:!current_module fct

let set_current_function (fct,(source,_loc2)) =
  set_current_scope MacroIndex.Sfunction ;
  Options.debug ~level:2 ~dkey
    "Set current function to %S@." fct ;
  try
    current_function :=  Some (find_kf fct) ;
    Options.debug ~level:2 ~dkey "FUNCTION %s@." fct
  with
    Not_found ->
    Options.annot_error ~source "could not find function %s for ACSL importer." fct;
    current_function := None

exception Kf_not_found
let with_current_function ?source () =
  match !current_function with
  | None -> Options.annot_warning ~raising:(fun () -> raise Kf_not_found)
              ?source "no proper function found for ACSL importer."
  | Some kf -> kf

let get_current_function ?source () =
  match !current_function with
  | None -> Options.abort ?source "no proper function found for ACSL importer."
  | Some kf -> kf

(*--------*)
let init_ast =
  let first = ref true in
  fun ~file ~init_module_from_file_name ~init_typenames ast ->
    A2fc_inner_ast.set ast;
    if !first then begin
      first := false ;
      Logic_env.builtin_types_as_typenames ();
    end;
    if init_typenames then
      (* looks at type names of the [ast_file] to init the parser *)
      Cil.iterGlobals ast
        (function
          | GType (tn, _loc) -> Logic_env.add_typename tn.torig_name
          | _ -> ()) ;
    if init_module_from_file_name then
      set_current_module ~is_from_file_name:true
        (basename_chop_extension (Filepath.to_string file))

(*-----------------------------------------------------------------------*)

(** Parse a global annotation. *)
let parse_global s =
  match Logic_lexer.annot (get_buff_loc (), s) with
  | Some (_, Logic_ptree.Adecl decls) ->
    (* update starting annotation location *)
    List.map (fun d -> {d with Logic_ptree.decl_loc
                               = (get_prop_loc (),
                                  snd d.Logic_ptree.decl_loc)})
      decls
  | _ -> Options.abort "[Syntax error] Unallowed global annotation."

(** Parse a function contract. *)
let parse_spec s =
  match Logic_lexer.spec (get_buff_loc (), s) with
  | Some (loc2, a) -> (* update starting annotation location *)
    a, (get_prop_loc (), loc2)
  | None -> Options.abort "[Syntax error] Invalid function contract"

(** Parse a code annotation. *)
let parse_annots s =
  match Logic_lexer.annot (get_buff_loc (), s) with
  | Some (_,Logic_ptree.Acode_annot ((_loc1,loc2),a)) ->
    (* update starting annotation location *)
    (get_prop_loc (), loc2), [a]
  | Some (_,Logic_ptree.Aloop_annot ((_loc1,loc2),a)) ->
    (* update starting annotation location *)
    (get_prop_loc (), loc2), a
  | _ ->
    Options.abort "[Syntax error] Unallowed annotation."

(** Parse a term/pred. *)
let parse_lexpr s =
  match Logic_lexer.lexpr (get_buff_loc (), s) with
  | Some (_, t) -> t, get_prop_loc ()
  | None -> Options.abort "[Syntax error] Invalid logic term"

(*-----------------------------------------------------------------------*)
exception Stmt_not_found of Kernel_function.t

let find_stmt_set ?source find =
  let kf = with_current_function ?source () in
  let stmts = try find kf with Not_found -> raise (Stmt_not_found kf) in
  stmts

let find_loop_stmt_set_from_loop_number ?source number =
  find_stmt_set ?source (Statement.find_loop_stmt_set number)

let find_loop_body_set_from_loop_number ?source number =
  find_stmt_set ?source (Statement.find_body_stmt_set number)

let find_stmt_set_from_call_to ?source kf_opt num_opt =
  find_stmt_set ?source (Statement.find_call2_stmt_set kf_opt num_opt)

let find_stmt_set_from_call_number ?source number =
  find_stmt_set ?source (Statement.find_call_stmt_set number)

let find_stmt_set_from_asm_number ?source number =
  find_stmt_set ?source (Statement.find_asm_stmt_set number)

let find_stmt_set_from_label ?source label =
  find_stmt_set ?source (fun kf -> S_Stmt.singleton !(Kernel_function.find_label kf label))

let find_stmt_set_from_sid ?source local_sid =
  find_stmt_set ?source (fun kf -> S_Stmt.singleton (Statement.find_stmt local_sid kf))

let find_stmt_set_from_return ?source () =
  find_stmt_set ?source (fun kf -> S_Stmt.singleton (Kernel_function.find_return kf))

let find_stmt_set_from_misc ?source label =
  let kf = get_current_function ?source () in
  let find_stmt,otherwise =
    try
      let local_sid = int_of_string label in
      (fun () -> Statement.find_stmt local_sid kf),
      (fun () -> Options.abort ?source "statement ID %s not found into %a function for ACSL import."
          label Kernel_function.pretty kf)
    with
    | Failure _ -> (* label is not a statement number *)
      let find_stmt =
        if label = "return" then
          (fun () -> Kernel_function.find_return kf)
        else
          (fun () -> !(Kernel_function.find_label kf label))
      in
      find_stmt,
      (fun () -> Options.abort ?source "statement label %S not found into %a function for ACSL import."
          label Kernel_function.pretty kf)
  in
  let stmt =
    try
      find_stmt ()
    with Not_found -> otherwise ()
  in S_Stmt.singleton stmt

(*-----------------------------------------------------------------------*)
let add_macro ~is_global_scope m =
  MacroIndex.add_macro (if is_global_scope then MacroIndex.Sfile else !current_scope) m

let integral_cast ty t =
  if Options.AddonIntegerCast.get () then
    begin
      let loc = t.term_loc in
      let source = fst loc in
      let ty = Ast_types.remove_attributes_for_logic_type ty in
      Options.warning ~wkey:Options.wkey_integer_cast ~source "Casting term %a of type %a into type %a."
        Printer.pp_term t Printer.pp_logic_type Linteger Printer.pp_typ ty;
      Logic_const.tcast ~loc t ty
    end
  else
    Cabs2cil.integral_cast ty t


(* messages that leads also to an annor_error in raising Exit*)
let lt_error (source, _ ) fmt = Options.annot_warning ~raising:(fun () -> raise Exit) ~source fmt

let lt_on_error action finally arg =
  try action arg
  with Exit -> finally (Fileloc.unknown,"Error"); raise Exit

(** Add global annotations. *)
let dkey = Options.register_category "trace-pasting"

let add_global_annot g_annots =
  let file = !current_module in
  let scope = !current_scope in
  let module LT =
    Logic_typing.Make
      (struct
        let anonCompFieldName = Cabs2cil.anonCompFieldName
        let conditionalConversion = Cabs2cil.logicConditionalConversion
        let is_loop () = false
        let find_macro m = MacroIndex.find_macro scope m
        let find_var ?label:_ var = SymbolIndex.find_var_global ~file var
        let find_enum_tag s = SymbolIndex.find_enum_item ~file s
        let find_comp_field info s =
          Cabs2cil.find_field_offset
            (fun fi -> fi.forig_name = s)
            (Option.value ~default:[] info.cfields)
        let find_type tkind s = SymbolIndex.find_type ~file tkind s
        let find_label _s = raise Not_found

        let integral_cast = integral_cast
        let error = lt_error
        let on_error = lt_on_error
      end)
  in
  let add_global parsed_g_annot =
    LT.annot parsed_g_annot |> function None -> () | Some g_annot ->
      if Options.continue_after_typing () then begin
        Options.debug ~level:2 ~dkey
          "Adding global annotation:@.%a" Printer.pp_global_annotation g_annot;
        Annotations.add_global Options.emitter g_annot
      end
  in
  let add_global parsed_g_annot=
    try add_global parsed_g_annot
    with | Exit ->
      Options.annot_error "global annotation ignored by ACSL import."
  in
  List.iter add_global g_annots

(** Add a function contract. *)
let add_funspec spec loc =
  try
    let kf = with_current_function ~source:(fst (loc)) () in
    let file = !current_module in
    let scope = !current_scope in
    let module LT =
      Logic_typing.Make
        (struct
          let anonCompFieldName = Cabs2cil.anonCompFieldName
          let conditionalConversion = Cabs2cil.logicConditionalConversion

          let is_loop () = false
          let find_macro m = MacroIndex.find_macro scope m
          let find_var ?label:_ var = SymbolIndex.find_var_funspec ~file kf var
          let find_enum_tag s = SymbolIndex.find_enum_item ~file s
          let find_comp_field info s =
            Cabs2cil.find_field_offset
              (fun fi -> fi.forig_name = s)
              (Option.value ~default:[] info.cfields)
          let find_type tkind s = SymbolIndex.find_type ~file tkind s
          let find_label s = Kernel_function.find_label kf s

          let integral_cast = integral_cast
          let error = lt_error
          let on_error = lt_on_error
        end)
    in
    let vi = Kernel_function.get_vi kf in
    let formals = Some (Kernel_function.get_formals kf) in
    let typ = Kernel_function.get_type kf in
    let old_spec = Annotations.funspec kf in
    let behaviors = Logic_utils.get_behavior_names old_spec in
    let ({ spec_behavior;
           spec_terminates;
           spec_variant;
           spec_complete_behaviors;
           spec_disjoint_behaviors } as spec) =  (* typed function contract *)
      LT.funspec behaviors vi formals typ spec
    in
    if Options.continue_after_typing () then begin
      Options.debug ~level:2 ~dkey
        "Adding function specification:@.%a" Printer.pp_funspec spec;
      Annotations.add_behaviors Options.emitter kf spec_behavior;
      Option.iter
        (Annotations.add_terminates Options.emitter kf)
        spec_terminates;
      Option.iter
        (Annotations.add_decreases Options.emitter kf)
        spec_variant;
      List.iter
        (Annotations.add_complete Options.emitter kf)
        spec_complete_behaviors;
      List.iter
        (Annotations.add_disjoint Options.emitter kf)
        spec_disjoint_behaviors
    end
  with | Kf_not_found
       | Exit ->
    Options.annot_error "function contract ignored by ACSL import."

(** Add code annotations. *)
let add_annots_aux kf file ?loop_number loc annots stmt =
  let scope = !current_scope in
  let module LT =
    Logic_typing.Make
      (struct
        let anonCompFieldName = Cabs2cil.anonCompFieldName
        let conditionalConversion = Cabs2cil.logicConditionalConversion

        let is_loop () = Kernel_function.stmt_in_loop kf stmt

        let find_macro m = MacroIndex.find_macro scope m
        let find_var ?label var =
          SymbolIndex.find_var_annot ~file kf stmt ?label var
        let find_enum_tag s = SymbolIndex.find_enum_item ~file s
        let find_comp_field info s =
          Cabs2cil.find_field_offset
            (fun fi -> fi.forig_name = s)
            (Option.value ~default:[] info.cfields)
        let find_type tkind s = SymbolIndex.find_type ~file tkind s
        let find_label s = Kernel_function.find_label kf s

        let integral_cast = integral_cast
        let error = lt_error
        let on_error = lt_on_error
      end)
  in
  let add_annot parsed_annot =
    let spec = Annotations.funspec kf in
    let annot = (* typed code annotation *)
      LT.code_annot loc
        (Logic_utils.get_behavior_names spec)
        (Ctype (Kernel_function.get_return_type kf))
        parsed_annot
    in
    let add_annot stmt =
      (* we are supposed to fill empty specs. Do not refrain from replacing
         WritesAny and FreeAllocAny with actual annotations. *)
      let keep_empty = false in
      if Options.continue_after_typing () then begin
        match annot.annot_content with
        | AStmtSpec (_bhv,_spec) -> (* Merging statement contract *)
          Options.debug ~level:2 ~dkey
            "Adding statement contract:@.%a" Printer.pp_code_annotation annot;
          Annotations.add_code_annot
            ~keep_empty Options.emitter ~kf stmt annot
        | AAllocation (_bhv,_fa) -> (* Merging loop allocation clause *)
          Options.debug ~level:2 ~dkey
            "Adding loop allocation clause:@.%a" Printer.pp_code_annotation annot;
          Annotations.add_code_annot
            ~keep_empty Options.emitter ~kf stmt annot
        | AAssigns (_bhv,_a) ->  (* Merging loop assigns clause *)
          Options.debug ~level:2 ~dkey
            "Adding loop assigns clause:@.%a" Printer.pp_code_annotation annot;
          Annotations.add_code_annot
            ~keep_empty Options.emitter ~kf stmt annot
        | AInvariant (bhv,false,pred) when loop_number <> None ->
          (* Converting invariant into loop invariant when possible *)
          Options.debug ~level:2 ~dkey
            "Adding invariant annotation:@.%a" Printer.pp_code_annotation annot;
          let loop_number =
            match loop_number with
            | Some loop_number -> loop_number
            | _ -> assert false
          in let add_invariant loop_stmt =
               let is_convertible =
                 (* it is convertible when the statement is the first statement of the loop block body
                    and this block has no local variables *)
                 match loop_stmt.skind with
                 | Loop (_li,{blocals=[];bstmts=s::_},_loc,_cont,_brk) when s.sid = stmt.sid -> true
                 | Loop _ -> false
                 | _ -> assert false
               in let stmt,annot =
                    if is_convertible then
                      (Options.debug ~level:2 ~dkey
                         "Converting invariant into loop invariant for loop #%d" loop_number;
                       loop_stmt, {annot with annot_content=AInvariant (bhv,true,pred)})
                    else stmt,annot
               in
               Annotations.add_code_annot
                 ~keep_empty Options.emitter ~kf stmt annot
          in
          S_Stmt.iter add_invariant
            (try find_loop_stmt_set_from_loop_number loop_number
             with Stmt_not_found kf ->
               lt_error loc "loop %d not found into %a function for ACSL import"
                 loop_number Kernel_function.pretty kf)
        | _ ->
          Options.debug ~level:2 ~dkey
            "Adding statement annotation:@.%a"
            Printer.pp_code_annotation annot;
          Annotations.add_code_annot
            ~keep_empty Options.emitter ~kf stmt annot
      end
    in
    let loop_stmt =
      let rec get_loop_stmt stmt = match stmt.skind with
        | Loop _ -> Some stmt
        | Block { bstmts=stmt::_ } -> get_loop_stmt stmt
        | _ -> None
      in get_loop_stmt stmt
    in
    match loop_stmt with
    | None when Logic_utils.is_loop_annot annot ->
      lt_error loc "loop annotations are only allowed for loop statements."
    | Some loop_stmt when Logic_utils.is_loop_annot annot -> add_annot loop_stmt
    | _ -> add_annot stmt
  in
  let add_annot parsed_annot =
    try add_annot parsed_annot
    with | Exit ->
      Options.annot_error "code annotation ignored by ACSL import."
  in
  List.iter add_annot annots

let add_annots ?loop_number stmts loc annots =
  try
    let kf = with_current_function ~source:(fst (loc)) () in
    let file = !current_module in
    S_Stmt.iter (add_annots_aux kf file ?loop_number loc annots) stmts
  with | Kf_not_found ->
    Options.annot_error "code annotation ignored by ACSL import."

(** Add Caveat Post clauses as an ensures and an exits clause. *)
let add_post kf id _loc post =
  let visitor = object(self)
    inherit Visitor.frama_c_inplace
    val mutable status = None
    method! vterm_lhost term_lhost =
      let change_to lvar = Cil.ChangeDoChildrenPost (TVar lvar, fun x -> x) in
      let continue () = Cil.JustCopy in
      match status, term_lhost with
      | None, TVar{lv_name = "\\exit_status"} ->
        (* meet first "\exit_status" ... *)
        let lvar = Cil_const.make_logic_var_quant "exit_status" Linteger in
        (* ... so, performs transformation for the ensures clause *)
        status <- Some (Normal, lvar, None) ;
        change_to lvar
      | None, TResult (typ) -> (* meet first "\result" so,... *)
        let lvar = Cil_const.make_logic_var_quant "result" (Ctype typ) in
        (* ... so, performs transformation for the exits clause *)
        status <- Some (Exits, lvar, None) ;
        change_to lvar

      | Some (Normal, lvar, _), TVar{lv_name = "\\exit_status"} ->
        (* a second "\\exit_status" ... *)
        change_to lvar
      | Some (Exits, lvar, _), TResult (_)  ->
        (* a second "\\result" *)
        change_to lvar

      | Some (Normal, lvar, None), TResult (typ)  ->
        (* first "\result" while transforming "\exit_status" *)
        status <-
          Some (Normal, lvar,
                Some (Cil_const.make_logic_var_quant "result" (Ctype typ)));
        continue ()

      | Some (Exits, lvar, None), TVar{lv_name = "\\exit_status"} ->
        (* first "\exit_status" while transforming "\result" *)
        status <-
          Some (Exits, lvar,
                Some (Cil_const.make_logic_var_quant "exit_status" Linteger));
        continue ()

      | _ -> continue ()

    (** Transform the predicate into two clauses: one ensures + one
        exits. *)
    method make_post_cond pred =
      (* look at "\result" and "\exit_status" and transform one of these *)
      let new_pred = Cil.visitCilPredicate (self :> Cil.cilVisitor) pred in
      let make_clause pred kind name =
        let nameid = if id = "" then name else (id ^ "_" ^ name) in
        kind,
        Logic_const.new_predicate {pred with pred_name=nameid::pred.pred_name}
      in
      let quantif lvar pred =
        {pred with pred_content=Pforall([lvar], {pred with pred_name=[]})}
      in
      let make_other_clause other pred kind =
        let other_pred =
          match other with
          | None -> pred
          | Some lvar -> (* a second transformation is needed ... *)
            status <- Some (kind, lvar, None) ;
            (* so, performs that second transformation *)
            quantif lvar (Cil.visitCilPredicate (self :> Cil.cilVisitor) pred)
        in
        make_clause other_pred kind
      in
      match status with
      | Some(Normal, lvar, other) -> (* "\result" has been transformed *)
        [make_clause (quantif lvar new_pred) Normal "at_return" ;
         make_other_clause other pred Exits "at_exit" ]
      | Some(Exits, lvar, other) -> (* "\exit_status" has been transformed *)
        [make_other_clause other pred Normal "at_return" ;
         make_clause (quantif lvar new_pred) Exits "at_exit" ]
      | _ ->
        [make_clause new_pred Normal "at_return" ;
         make_clause pred Exits "at_exit" ]
  end
  in
  let file = !current_module in
  let scope = !current_scope in
  let module LT =
    Logic_typing.Make
      (struct
        let anonCompFieldName = Cabs2cil.anonCompFieldName
        let conditionalConversion = Cabs2cil.logicConditionalConversion

        let is_loop () = false

        let find_macro m = MacroIndex.find_macro scope m
        let find_var ?label:_ var = SymbolIndex.find_var_funspec ~file kf var
        let find_enum_tag s = SymbolIndex.find_enum_item ~file s
        let find_comp_field info s =
          Cabs2cil.find_field_offset
            (fun fi -> fi.forig_name = s)
            (Option.value ~default:[] info.cfields)
        let find_type tkind s = SymbolIndex.find_type ~file tkind s

        let find_label s = Kernel_function.find_label kf s

        let integral_cast = integral_cast
        let error = lt_error
        let on_error = lt_on_error
      end)
  in
  let env = Logic_typing.post_state_env Exits
      (Ctype (Kernel_function.get_return_type kf))
  in
  let add_formal env vi =
    Logic_typing.add_var vi.vorig_name (Cil.cvar_to_lvar vi) env
  in
  let env = List.fold_left add_formal env (Kernel_function.get_formals kf) in
  try
    let pred = LT.predicate env post in
    if Options.continue_after_typing () then begin
      let behavior =
        Cil.mk_behavior ~name:id ~post_cond:(visitor#make_post_cond pred) ()
      in
      Annotations.add_behaviors Options.emitter kf [ behavior ]
    end
  with | Exit ->
    Options.annot_error "extended annotation ignored by ACSL import."

(*-----------------------------------------------------------------------*)

(** Grammar extension for "ensures_and_exits" clauses given into C files. *)
let ensures_and_exits_typer ~typing_context ~loc ps =
  match ps with
  | [p] ->
    begin
      let env =
        typing_context.Logic_typing.post_state [Normal; Exits] in
      let pred =
        typing_context.Logic_typing.type_predicate typing_context env p in
      let visitor = object(self)
        inherit Visitor.frama_c_inplace

        val mutable status = None

        method! vterm_lhost term_lhost =
          let change_to lvar = Cil.ChangeDoChildrenPost (TVar lvar, fun x -> x)
          and continue () = Cil.JustCopy
          in match status, term_lhost with
          | None, TVar{lv_name = "\\exit_status"} ->
            (* meet first "\exit_status" ... *)
            let lvar = Cil_const.make_logic_var_quant "exit_status" Linteger in
            (* ... so, performs transformation for the ensures clause *)
            status <- Some (Normal, lvar, None) ;
            change_to lvar
          | None, TResult (typ) -> (* meet first "\result" so,... *)
            let lvar = Cil_const.make_logic_var_quant "result" (Ctype typ) in
            (* ... so, performs transformation for the exits clause *)
            status <- Some (Exits, lvar, None) ;
            change_to lvar

          | Some (Normal, lvar, _), TVar{lv_name = "\\exit_status"} ->
            (* a second "\\exit_status" ... *)
            change_to lvar
          | Some (Exits, lvar, _), TResult (_)  ->
            (* a second "\\result" *)
            change_to lvar

          | Some (Normal, lvar, None), TResult (typ)  ->
            (* first "\result" while transforming "\exit_status" *)
            status <-
              Some (Normal, lvar,
                    Some
                      (Cil_const.make_logic_var_quant "result" (Ctype typ)));
            continue ()

          | Some (Exits, lvar, None), TVar{lv_name = "\\exit_status"} ->
            (* first "\exit_status" while transforming "\result" *)
            status <-
              Some (Exits, lvar,
                    Some
                      (Cil_const.make_logic_var_quant "exit_status" Linteger));
            continue ()

          | _ -> continue ()

        (** Transform the predicate into two clauses: one ensures + one
            exits. *)
        method make_post_cond pred =
          (* look at "\result" and "\exit_status" and transform one of these *)
          let new_pred = Cil.visitCilPredicate (self :> Cil.cilVisitor) pred
          and make_clause pred name =
            let pred_name = name :: pred.pred_name in {pred with pred_name}
          and quantif lvar pred =
            {pred with pred_content=Pforall([lvar], {pred with pred_name=[]})}
          in
          let make_other_clause other pred kind =
            let other_pred =
              match other with
              | None -> pred
              | Some lvar -> (* a second transformation is needed ... *)
                status <- Some (kind, lvar, None) ;
                (* so, performs that second transformation *)
                quantif lvar
                  (Cil.visitCilPredicate (self :> Cil.cilVisitor) pred)
            in
            make_clause other_pred
          in
          match status with
          | Some(Normal, lvar, other) -> (* "\result" has been transformed *)
            [make_clause (quantif lvar new_pred) "at_return" ;
             make_other_clause other pred Exits "at_exit" ]
          | Some(Exits, lvar, other) -> (* "\exit_status" has been transformed*)
            [make_other_clause other pred Normal "at_return" ;
             make_clause (quantif lvar new_pred) "at_exit" ]
          | _ ->
            [make_clause new_pred "at_return";
             make_clause pred "at_exit"]
      end
      in (* transform the predicate into two new clauses *)
      Ext_preds (visitor#make_post_cond pred)
    end
  | _ ->
    typing_context.Logic_typing.error loc
      "[Syntax error] Expecting a predicate after keyword ensures_and_exits."

let clause_extension = "ensures_and_exits"

(* Register the grammar extension for "ensures_and_exits" clauses. *)
let () =
  let clause_typer typing_context loc ps =
    if Options.AddonEnsuresAndExits.get () then
      ensures_and_exits_typer ~typing_context ~loc ps
    else typing_context.Logic_typing.error loc
        "[Setting error] Rejected clause extension: %s." clause_extension
  in
  Acsl_extension.register_behavior ~plugin:"acsl_importer"
    clause_extension clause_typer false

let () =
  let dkey = Options.register_category "trace-ensures-and-exits" in
  let code_transformation =
    File.register_code_transformation_category clause_extension
  in
  let mk_clause p =
    let kind =
      match p.pred_name with
      | "at_exit" :: _ -> Exits
      | "at_return" :: _ -> Normal
      | _ ->
        Options.fatal
          "Unrecognized predicate in %s extension %a"
          clause_extension Printer.pp_predicate p
    in
    let pred_name = match p.pred_name with
      | name::first::tl -> (name ^ "_" ^ first)::tl
      | _ -> p.pred_name
    in
    let pred = Logic_const.new_predicate { p with pred_name } in
    Options.debug ~level:2 ~dkey
      "Adding clause: %s %a@." (Cil_printer.get_termination_kind_name kind)
      Cil_printer.pp_identified_predicate pred;
    kind, pred
  in
  let transform ast =
    let vis =
      object(self)
        inherit Visitor.frama_c_inplace
        val mutable active = None

        method! vcode_annot ca =
          match ca.annot_content with
          | AStmtSpec(a,_) ->
            active <- Some a;
            DoChildrenPost (fun r -> active <- None; r)
          | _ -> SkipChildren (* nothing to do outside of contract. *)

        method! vbehavior bhv =
          let my_ext =
            List.filter
              (fun {ext_name} -> ext_name = clause_extension) bhv.b_extended
          in
          match my_ext with
          | [] -> SkipChildren
          | _ ->
            List.iter
              (fun clause_extension ->
                 Options.debug ~level:2 ~dkey
                   "Removing clause: %a@."
                   Cil_printer.pp_extended clause_extension;
                 (* note: the remove_extended never fails even if the clause
                    is not found and there are two possible emiter for it. *)
                 Annotations.remove_extended Emitter.end_user (*from C file*)
                   (Option.get self#current_kf)
                   clause_extension;
                 Annotations.remove_extended Options.emitter (*imported*)
                   (Option.get self#current_kf)
                   clause_extension
              )
              my_ext;
            let clauses =
              List.concat
                (List.map
                   (function
                     | {ext_kind = Ext_preds l} -> List.map mk_clause l
                     | _ ->
                       Options.fatal
                         "Unrecognized content of extension %s"
                         clause_extension)
                   my_ext)
            in
            let kf = Option.get self#current_kf in
            let stmt = self#current_stmt in
            let behavior =
              if Cil.is_default_behavior bhv then None else Some bhv.b_name
            in
            Annotations.add_ensures
              Options.emitter kf ?stmt ?active ?behavior clauses;
            SkipChildren
      end
    in
    Visitor.visitFramacFileSameGlobals vis ast
  in
  let deps = (* extension only active when this option is given. *)
    [(module Options.AddonEnsuresAndExits: Parameter_sig.S)]
  in
  let after = [Options.main_import] in
  File.add_code_transformation_after_cleanup
    ~deps ~after code_transformation transform

(*-----------------------------------------------------------------------*)

(* "Implicit state variables"
   !prop_file: file containing the property
   !prop_line: line containing the first property token
   !buff_line: line containing the first character of
               the string to parse with acsl parser
   !current_module: None -> lookup into all files
                    Some file -> lookup first into file (with its full path)
*)
(** Do not forget "Implicit state variables" for plug-in API: *)
let paste_global_annot s =
  let global_annot = parse_global s in
  add_global_annot global_annot

(** Do not forget "Implicit state variables" for plug-in API: *)
let paste_fun_spec s =
  let fun_spec, loc = parse_spec s in
  add_funspec fun_spec loc

(** Do not forget "Implicit state variables" for plug-in API: *)
let paste_postcond kf id s =
  let post, loc = parse_lexpr s in
  add_post kf id loc post

(** Do not forget "Implicit state variables" for plug-in API: *)
let paste_code_annot ?loop_number stmts s =
  let loc, annots = parse_annots s in
  add_annots ?loop_number stmts loc annots

(*-- Pasting global annotations from buffer --*)

let dkey = Options.register_category "trace-importations"
let paste_at_global ~clause =
  let prop = Buffer.contents buffer in
  Options.debug ~level:2 ~dkey
    "Importing %s %s;@." clause prop ;
  paste_global_annot (clause ^ " " ^ prop ^ ";")

(*-- Pasting function contracts from buffer --*)

let paste_at_func ~clause =
  let prop = Buffer.contents buffer in
  Options.debug ~level:2 ~dkey
    "Importing %s %s;@." clause prop ;
  paste_fun_spec (clause ^ " " ^ prop ^ ";")

let paste_at_func_behavior ~clause ~behav =
  let prop = Buffer.contents buffer in
  let prop = clause ^ " " ^ prop ^ ";" in
  let prop = if behav = "" then prop else ("behavior " ^ behav ^ ": " ^ prop) in
  Options.debug ~level:2 ~dkey
    "Importing %s@." prop ;
  paste_fun_spec prop

let paste_post ~behav =
  let prop = Buffer.contents buffer in
  Options.debug ~level:2 ~dkey
    "Importing ensures_and_exits %s: %s;@." behav prop ;
  let kf = get_current_function () in
  paste_postcond kf behav prop

(*-- Pasting code annotations from buffer --*)

let paste_loop_body ~clause ~loop =
  let prop = Buffer.contents buffer in
  Options.debug ~level:2 ~dkey
    "Importing AT loop %s: %s %s;@." loop clause prop ;
  let loop_number = int_of_string loop in
  let stmts = try find_loop_body_set_from_loop_number loop_number
    with Stmt_not_found kf ->
      Options.abort "loop body %d not found into %a function for ACSL import."
        loop_number Kernel_function.pretty kf
  in paste_code_annot ~loop_number stmts (clause ^ " " ^ prop ^ ";")

let paste_code ~clause ~label =
  let prop = Buffer.contents buffer in
  Options.debug ~level:2 ~dkey
    "Importing AT %s: %s %s;@." label clause prop ;
  let stmts = find_stmt_set_from_misc label
  in paste_code_annot stmts (clause ^ " " ^ prop ^ ";")

let paste_at_stmt ~clause ~loop ~label =
  if loop = "" then paste_code ~clause ~label
  else paste_loop_body ~clause ~loop

let paste_at_loop ~clause ~loop =
  let prop = Buffer.contents buffer in
  Options.debug ~level:2 ~dkey
    "Importing AT loop %s: loop %s %s;@." loop clause prop ;
  let loop_number = int_of_string loop in
  let stmts = try find_loop_stmt_set_from_loop_number loop_number
    with  Stmt_not_found kf ->
      Options.abort "loop %d not found into %a function for ACSL import."
        loop_number Kernel_function.pretty kf
  in paste_code_annot stmts ("loop " ^ clause ^ " " ^ prop ^ ";")

(*-----------------------------------------------------------------------*)
let init_pasting ~pfile ~pline ?(bline=pline) ~cfile ast =
  init_ast ~file:cfile ~init_module_from_file_name:true ~init_typenames:true ast;
  prop_file := (Filepath.of_string pfile) ;
  prop_line := pline ;
  buff_line := bline

(** For external plug-in API: *)
let paste_global_annot ~pfile ~pline ~cfile s ast =
  init_pasting ~pfile ~pline ~cfile ast ;
  paste_global_annot s

let dkey = Options.register_category "trace-actions"

(** For external plug-in API: *)
let paste_fun_spec kf ~pfile ~pline ~cfile s ast =
  init_pasting ~pfile  ~pline ~cfile ast ;
  Options.debug ~level:2 ~dkey
    "Set current function to %a@." Kernel_function.pretty kf ;
  current_function := Some kf ;
  paste_fun_spec s

(** For external plug-in API: *)
let paste_code_annot kf stmt ~pfile ~pline ~cfile s ast=
  init_pasting ~pfile ~pline ~cfile ast;
  Options.debug ~level:2 ~dkey
    "Set current function to %a@." Kernel_function.pretty kf ;
  current_function := Some kf ;
  paste_code_annot (S_Stmt.singleton stmt) s

(*-----------------------------------------------------------------------*)
