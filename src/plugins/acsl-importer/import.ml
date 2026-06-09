(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** To import spec from include file *)

open Logic_ptree

let dkey = Options.register_category "trace-inputs"
(* ---------------------------------------------------------------------- *)

let get_input_channel ~pfile = open_in pfile

let get_input_channel ~iDir ~pfile =
  Options.debug ~level:2 ~dkey "Looking for file: %s@." pfile;
  if not (Filename.is_relative pfile) then (get_input_channel ~pfile), pfile
  else
    let get_channel ~dir =
      let pfile = Filename.concat dir pfile in
      (get_input_channel ~pfile), pfile
    in
    let rec get_input = function
      | []    -> get_input_channel ~pfile, pfile
      | [dir] -> get_channel ~dir
      | dir::dirs -> (try get_channel ~dir with | _ -> get_input dirs)
    in
    get_input iDir

(* ---------------------------------------------------------------------- *)
exception Eof of (Logic_ptree.ext_spec * string)

let parse ~iDir ~pfile ~init_module_from_file_name ~init_typenames ast =
  try
    let inc, file = get_input_channel ~iDir ~pfile in
    try
      Paste.init_ast ~file:(Filepath.of_string file)
        ~init_module_from_file_name ~init_typenames ast;
      let lexbuf = Lexing.from_channel inc in
      lexbuf.Lexing.lex_curr_p <-
        {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname=file;};
      try
        let ext_spec = Logic_lexer.ext_spec lexbuf
        in raise (Eof ((ext_spec,file))) ;
      with
      | Failure s ->
        Options.abort
          ~source:(Filepos.of_lexing_pos lexbuf.Lexing.lex_curr_p)
          "[Failure while parsing] %s."
          s
      | Sys_error s ->
        Options.abort
          ~source:(Filepos.of_lexing_pos lexbuf.Lexing.lex_curr_p)
          "[System error while parsing] %s."
          s
      | Logic_utils.Not_well_formed(loc, s)         ->
        Options.abort ~source:(fst loc)
          "[Syntax error] %s (near %s)." s
          (Lexing.lexeme lexbuf)
      | Parsing.Parse_error ->
        Options.abort
          ~source:(Filepos.of_lexing_pos lexbuf.Lexing.lex_curr_p)
          "[Syntax error] %s."
          (Lexing.lexeme lexbuf)
    with
    | exn -> (* closing the in-channel *)
      close_in_noerr inc ;
      raise exn
  with
  | Eof ext_spec_file -> ext_spec_file

let pp_stmt_markup = Pretty_utils.pp_list ~sep:" " Format.pp_print_string

type loop_markup_t =
  | LoopBody
  | LoopStmt
  | Loop

type markup_t =
  | AtLoop of loop_markup_t * int
  | AtCallN of int
  | AtCallF of Kernel_function.t option * int
  | AtAsm of int
  | AtStmt of int
  | AtLabel of string
  | AtReturn
  | AtError

exception StmtMarkupError

let typecheck ~iDir ext_spec_file ast =

  let decode =
    let decode_pos number =
      let v = int_of_string number in
      if v <= 0 then raise StmtMarkupError;
      v
    in
    let decode_place = function
      | "stmt" -> LoopStmt
      | "body" -> LoopBody
      | _ -> raise StmtMarkupError
    in function
      | "loop"::loop_place::number::[] -> (try AtLoop (decode_place loop_place, decode_pos number) with _ -> AtError)
      | "loop"::number::[] -> (try AtLoop (Loop, decode_pos number) with _ -> AtError)
      | "asm"::number::[] -> (try AtAsm (decode_pos number) with _ -> AtError)
      | "indirect_call"::[] -> AtCallF (None, 0)
      | "call"::id_number::[] ->
        (try AtCallN (int_of_string id_number) with _ -> (try AtCallF (Some (Paste.find_kf id_number), 0) with _ -> AtError))
      | "indirect_call"::number::[] -> (try AtCallF (None, decode_pos number) with _ -> AtError)
      | "call"::id::number::[] -> (try AtCallF (Some (Paste.find_kf id), decode_pos number) with _ -> AtError)
      | "return"::[]  -> AtReturn
      | markup::[] -> (try AtStmt (decode_pos markup) with _ -> AtLabel markup)
      | _ -> AtError
  in

  let rec typecheck (ext_spec, parsed_file) =
    let dir = Filename.dirname parsed_file::iDir in
    let paste_decl_spec = function
      | Ext_decl decl -> if Options.continue_after_parsing () then Paste.add_global_annot [decl]
      | Ext_macro (is_global_scope, macro, def) -> if Options.continue_after_parsing () then Paste.add_macro ~is_global_scope macro def
      | Ext_include (_wide, pfile, _loc) ->
        let ext_spec_file =
          parse ~iDir:dir ~pfile ~init_module_from_file_name:false ~init_typenames:false ast
        in
        typecheck ext_spec_file
    in

    let paste_fun_spec = function
      | Ext_glob decl_spec -> paste_decl_spec decl_spec
      | Ext_spec (spec, loc) -> if Options.continue_after_parsing () then Paste.add_funspec spec loc
      | Ext_stmt (stmt_markup, annot, (source,_loc2)) ->
        if Options.continue_after_parsing () then
          begin
            let at_markup = decode stmt_markup in
            try
              let adds =
                match at_markup, annot with
                | AtLoop (Loop, loop), Acode_annot _
                | AtLoop (LoopBody, loop), _  -> Paste.add_annots ~loop_number:loop (Paste.find_loop_body_set_from_loop_number ~source loop)
                | AtLoop (Loop,     loop), _
                | AtLoop (LoopStmt, loop), _  -> Paste.add_annots (Paste.find_loop_stmt_set_from_loop_number ~source loop)
                | AtStmt sid, _               -> Paste.add_annots (Paste.find_stmt_set_from_sid ~source sid)
                | AtLabel label, _            -> Paste.add_annots (Paste.find_stmt_set_from_label ~source label)
                | AtCallN call, _             -> Paste.add_annots (Paste.find_stmt_set_from_call_number ~source call)
                | AtCallF (kf_opt, num_opt),_ -> Paste.add_annots (Paste.find_stmt_set_from_call_to ~source kf_opt num_opt)
                | AtAsm  asm, _               -> Paste.add_annots (Paste.find_stmt_set_from_asm_number ~source asm)
                | AtReturn, _                 -> Paste.add_annots (Paste.find_stmt_set_from_return ~source ())
                | AtError, _ -> raise StmtMarkupError
              in
              match annot with
              | Aloop_annot (loc, annots) ->
                adds loc annots;
              | Acode_annot (loc, annot) ->
                adds loc [annot]
              | Adecl _ ->
                Options.annot_error ~source "disallowed declaration by ACSL importer."
              | Aspec  ->
                Options.annot_error ~source "disallowed incomplete function specification by ACSL importer."
            with
            | StmtMarkupError ->
              Options.annot_error ~source "invalid statement markup '%a' for ACSL importer."
                pp_stmt_markup stmt_markup
            | Paste.Kf_not_found ->
              Options.annot_error ~source "invalid function markup for ACSL importer."
            | Paste.Stmt_not_found kf ->
              begin
                match at_markup, annot with
                | AtLoop(Loop, loop), Acode_annot _
                | AtLoop(LoopBody, loop), _  ->
                  Options.annot_error ~source "loop body %d not found into %a function for ACSL importer."
                    loop Kernel_function.pretty kf
                | AtLoop(Loop, loop), _
                | AtLoop(LoopStmt, loop), _  ->
                  Options.annot_error ~source "loop %d not found into %a function for ACSL importer."
                    loop Kernel_function.pretty kf
                | AtStmt sid, _              ->
                  Options.annot_error ~source "statement ID %d not found into %a function for ACSL importer."
                    sid Kernel_function.pretty kf
                | AtLabel label, _           ->
                  Options.annot_error ~source "statement label %S not found into %a function for ACSL importer."
                    label Kernel_function.pretty kf
                | AtCallN _, _
                | AtCallF _, _             ->
                  Options.annot_error ~source "call %a not found into %a function for ACSL importer."
                    pp_stmt_markup (match stmt_markup with _::m | m -> m) Kernel_function.pretty kf
                | AtAsm asm, _             ->
                  Options.annot_error ~source "assembly call %d not found into %a function for ACSL importer."
                    asm Kernel_function.pretty kf
                | AtReturn, _                ->
                  Options.annot_error ~source "return statement not found into %a function for ACSL importer."
                    Kernel_function.pretty kf
                | AtError, _                ->
                  Options.annot_error ~source "invalid statement markup '%a' for ACSL importer."
                    pp_stmt_markup stmt_markup
              end
          end
    in
    let paste_function_spec (f,specs) =
      if Options.continue_after_parsing () then
        (match f with | None -> () | Some f -> Paste.set_current_function f) ;
      List.iter paste_fun_spec specs  ;
    in
    let paste_module_spec (m,decl_specs,fun_specs) =
      if Options.continue_after_parsing () then
        (match m with | None -> () | Some m -> Paste.set_current_module ~is_from_file_name:false m) ;
      List.iter paste_decl_spec decl_specs ;
      List.iter paste_function_spec fun_specs  ;
    in
    List.iter paste_module_spec ext_spec
  in
  typecheck ext_spec_file

let import ~iDir ~pfile ~init_typenames ast =
  try
    let ext_spec_file = parse ~iDir:[] ~pfile
        ~init_module_from_file_name:true ~init_typenames ast
    in
    typecheck ~iDir ext_spec_file ast;
    let mode = match (Options.continue_after_typing ()), (Options.continue_after_parsing ()) with
      | true,true  -> ""
      | true,false -> "full parsing (without import)"
      | false,_    -> "full typing (without import)"
    in
    Options.feedback "Success%s for %s" mode (Filename.basename pfile) ;
    Paste.MacroIndex.clear_macro_table Paste.MacroIndex.Sfile ;
  with
  | exn ->
    Paste.MacroIndex.clear_macro_table Paste.MacroIndex.Sfile ;
    raise exn


(* ---------------------------------------------------------------------- *)
