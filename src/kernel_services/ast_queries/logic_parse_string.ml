(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

exception Error of Cil_types.location * string
exception Unbound of string

let find_var kf kinstr ?label var =
  let vi =
    try
      let scope =
        match kinstr with
        | Kglobal -> Whole_function kf
        | Kstmt stmt ->
          (match label with
           | None | Some "Here" | Some "Post" | Some "Old" -> Block_scope stmt
           | Some "Pre" -> raise Not_found (* no local variable in scope. *)
           | Some "Init" -> raise Not_found (* no local variable in scope. *)
           | Some "LoopEntry" | Some "LoopCurrent" ->
             if not (Kernel_function.stmt_in_loop kf stmt) then
               Kernel.fatal
                 "Use of LoopEntry or LoopCurrent outside of a loop";
             Block_scope (Kernel_function.find_enclosing_loop kf stmt)
           | Some l ->
             (try let s = Kernel_function.find_label kf l in Block_scope !s
              with Not_found ->
                Kernel.fatal
                  "Use of label %s that does not exist in function %a"
                  l Kernel_function.pretty kf))
      in
      Globals.Vars.find_from_astinfo var scope
    with Not_found ->
    try
      Globals.Vars.find_from_astinfo var (Formal kf)
    with Not_found ->
      Globals.Vars.find_from_astinfo var Global
  in
  Cil.cvar_to_lvar vi

(** Create a logic typer, the interpretation being done for the given
    kernel_function and stmt (the stmt is used check that loop invariants
    are allowed). *)

let default_typer kf kinstr =
  let module LT = Logic_typing.Make
      (struct
        let anonCompFieldName = Cabs2cil.anonCompFieldName
        let conditionalConversion = Cabs2cil.logicConditionalConversion

        let is_loop () =
          match kinstr with
          | Kglobal -> false
          | Kstmt s -> Kernel_function.stmt_in_loop kf s

        let find_macro _ = raise Not_found

        let find_var ?label var = find_var kf kinstr ?label var

        let find_enum_tag x =
          try
            Globals.Types.find_enum_tag x
          with Not_found ->
            (* The ACSL typer tries to parse a string, first as a variable,
               then as an enum. We report the "Unbound variable" message
               here, as it is nicer for the user. However, this short-circuits
               the later stages of resolution, for example global logic
               variables. *)
            raise (Unbound ("Unbound variable " ^ x))

        let find_comp_field info s =
          let field = Cil.getCompField info s in
          Field(field,NoOffset)

        let find_type = Globals.Types.find_type

        let find_label s = Kernel_function.find_label kf s

        let integral_cast ty t =
          raise
            (Failure
               (Format.asprintf
                  "term %a has type %a, but %a is expected."
                  Printer.pp_term t
                  Printer.pp_logic_type Linteger
                  Printer.pp_typ ty))

        let error loc msg =
          Format.kasprintf (fun e -> raise (Error (loc, e))) msg

        let on_error f rollback x =
          try f x with Error (loc,msg) as exn -> rollback (loc,msg); raise exn

      end)
  in
  (module LT : Logic_typing.S)


(** Set up the parser for the infamous 'C hack' needed to parse typedefs *)
let sync_typedefs () =
  Logic_env.reset_typenames ();
  Globals.Types.iter_types
    (fun name _ ns ->
       if ns = Logic_typing.Typedef then
         try ignore @@ String.index name ':' with Not_found ->
           Logic_env.add_typename name)

let wrap f parsetree loc =
  match parsetree with
  | None -> raise (Error (loc, "Syntax error in annotation"))
  | Some annot -> try f annot with Unbound s -> raise (Error (loc, s))

let code_annot kf stmt s =
  sync_typedefs ();
  let module LT = (val default_typer kf (Kstmt stmt) : Logic_typing.S) in
  let loc = Stmt.loc stmt in
  let pa =
    Option.bind
      (function (_, Logic_ptree.Acode_annot (_,a)) -> Some a | _ -> None)
      (Logic_lexer.annot (fst loc,s))
  in
  let parse pa =
    Populate_spec.populate_funspec kf [`Assigns];
    LT.code_annot
      (Stmt.loc stmt)
      (Logic_utils.get_behavior_names (Annotations.funspec kf))
      (Ctype (Kernel_function.get_return_type kf)) pa
  in
  wrap parse pa loc

let default_term_env () =
  Logic_typing.append_here_label (Logic_typing.Lenv.empty())

let term kf ?(loc=Fileloc.unknown) ?(env=default_term_env ()) s =
  sync_typedefs ();
  let module LT = (val default_typer kf Kglobal : Logic_typing.S) in
  let pa_expr = Option.map snd (Logic_lexer.lexpr (fst loc, s)) in
  let parse pa_expr = LT.term env pa_expr in
  wrap parse pa_expr loc

let term_lval kf ?(loc=Fileloc.unknown) ?(env=default_term_env ()) s =
  match (term kf ~loc ~env s).term_node with
  | TLval lv -> lv
  | _ -> raise (Error (loc, "Syntax error (expecting an lvalue)"))

let predicate kf ?(loc=Fileloc.unknown) ?(env=default_term_env ()) s =
  sync_typedefs ();
  let module LT = (val default_typer kf Kglobal : Logic_typing.S) in
  let pa_expr = Option.map snd (Logic_lexer.lexpr (fst loc, s)) in
  let parse pa_expr = LT.predicate env pa_expr in
  wrap parse pa_expr loc
