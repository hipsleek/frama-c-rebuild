(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Va_types
module Typ = Extends.Typ

(* In place visitor for translation *)

let translate_variadics (file : file) =
  (* Environment filled with global symbols. *)
  let env = Environment.from_file file in

  (* Table associating varinfo of variadic functions to a variadic_function
     description *)
  let module Table = Cil_datatype.Varinfo.Hashtbl in
  let classification : variadic_function Table.t = Table.create 17 in
  let v = object
    inherit Cil.nopCilVisitor

    method! vglob glob =
      begin match glob with
        | GFunDecl(_, vi, _) | GFun ({svar = vi}, _) ->
          if not (Table.mem classification vi) then begin
            let vf = Classify.classify env vi in
            Option.iter (Table.add classification vi) vf
          end;
          Cil.SkipChildren
        | _ ->
          Cil.SkipChildren
      end
  end
  in
  Cil.visitCilFile v file;

  (* Utility function that tells if there is a call inside a statement *)
  let contains_call stmt =
    let result = ref false in
    let vis = object
      inherit Cil.nopCilVisitor
      method! vinst = function
        | Call _ -> result := true; Cil.SkipChildren
        | _ -> Cil.DoChildren
      method! vexpr _ = Cil.SkipChildren
      method! vtype _ = Cil.SkipChildren
    end
    in
    ignore (Cil.visitCilStmt vis stmt);
    !result
  in

  (* The translating visitor *)
  let v = object (self)
    inherit Cil.nopCilVisitor

    val curr_block = Stack.create () (* Opened blocks to store generated locals *)
    val mutable pending_globals = [] (* List of globals to add before the current global once it's finished visiting *)

    method! vblock b =
      Stack.push b curr_block;
      Cil.DoChildrenPost (fun b -> ignore (Stack.pop curr_block); b)

    method private enclosing_block () =
      try Stack.top curr_block
      with Stack.Empty -> Kernel.fatal "No enclosing block here"

    method private make_builder ~loc ~fundec ~ghost ~mk_call =
      let module B =
      struct
        include Cil_builder.Stateful ()

        let loc = loc

        let finish_function () =
          pending_globals <- finish_function () :: pending_globals

        let finish_declaration () =
          pending_globals <-
            finish_declaration ~register:false () :: pending_globals

        let start_translation () =
          open_block ~loc ~into:fundec ~ghost ()

        let translated_call callee args =
          of_instr
            (mk_call (cil_lhost ~loc callee) (List.map (cil_exp ~loc) args))
      end
      in
      (module B : Builder.S)


    method! vtype _typ =
      Cil.DoChildrenPost (Generic.translate_type)

    (* Translate types and signatures *)
    method! vglob glob =
      begin match glob with
        | GFunDecl(_, vi, _) ->
          (match Table.find_opt classification vi with
           | None -> Cil.DoChildren (* may transform the type *)
           | Some { vf_class = Builtin } ->
             Kernel.result ~current:true ~dkey:Kernel.dkey_variadic
               "Variadic builtin %s left untransformed." vi.vname;
             Cil.SkipChildren
           | Some { vf_class = NoTranslation } ->
             Cil.SkipChildren
           | Some { vf_class = Defined | Aggregator _ | Unknown } ->
             Generic.add_vpar vi;
             Cil.DoChildren
           | Some { vf_class = Misc | FormatFun _ | Overload _ } ->
             Cil.SkipChildren
          )

        | GFun ({svar = vi} as fundec, _) ->
          if Table.mem classification vi then begin
            Generic.add_vpar vi;
            fundec.sformals <- Cil.getFormalsDecl vi;
          end;
          pending_globals <- [];
          Cil.DoChildrenPost (fun globs ->
              List.rev_append pending_globals globs)

        | _ ->
          Cil.DoChildren
      end

    method! vstmt s =
      match s.skind with
      | Instr (Call _) ->
        (* Separate locals created by a variadic call in their own block.
           This can't be done for Local_init(x,ConsInit _,_), as this
           instruction must be kept a direct child of the enclosing block,
           that determines the scope of x.
        *)
        let block = Cil.mkBlock [] in
        Stack.push block curr_block;
        let keep_block_if_needed s =
          ignore (Stack.pop curr_block);
          match s.skind with
          | Block b' ->
            (* We have introduced several instructions, and potentially locals.
               Scope of locals is in [block], that will replace b'. *)
            block.bstmts <- b'.bstmts;
            s.skind <- Block block;
            s
          | _ -> s
        in
        Cil.DoChildrenPost keep_block_if_needed
      | UnspecifiedSequence _ ->
        let update_seq_if_needed s =
          match s.skind with
          | UnspecifiedSequence seq ->
            let update (stmt,modified,writes,reads,calls) =
              let contains_call' stmt_ref = contains_call !stmt_ref in
              (stmt,modified,writes,reads,List.filter contains_call' calls)
            in
            s.skind <- UnspecifiedSequence (List.map update seq);
            s
          | _ -> s
        in
        Cil.DoChildrenPost update_seq_if_needed
      | _ -> Cil.DoChildren

    (* Replace variadic calls *)
    method! vinst i =
      let fundec = Option.get self#current_func in
      let loc = Cil_datatype.Instr.loc i in
      let block = self#enclosing_block () in
      let ghost = (Option.get self#current_stmt).ghost in
      let translate_call mk_call translator args =
        File.must_recompute_cfg fundec;
        let builder = self#make_builder ~loc ~fundec ~ghost ~mk_call in
        translator ~builder args;
        let module Builder = (val builder : Builder.S) in
        Builder.finish_instr_list ~scope:block ()
      in
      let mk_translator f =
        let vf = Table.find classification f in
        let cover_failure f =
          fun ~builder args ->
          try
            f ~builder args
          with Standard.Translate_call_exn callee ->
            Standard.fallback_fun_call ~callee ~builder env vf args
        in
        match vf.vf_class with
        | Overload o -> cover_failure (Standard.overloaded_call o vf)
        | Aggregator a -> cover_failure (Standard.aggregator_call a vf)
        | FormatFun f -> cover_failure (Standard.format_fun_call env f vf)
        | Builtin ->
          Kernel.result ~current:true ~dkey:Kernel.dkey_variadic
            "Call to variadic builtin %s left untransformed." f.vname;
          raise Not_found
        | NoTranslation ->
          raise Not_found
        | _ ->
          Generic.translate_call (Var f)
      in
      begin match i with
        (* Translate builtins *)
        | Call(_, Var vi, _, _)
          when Classify.is_va_builtin vi.vname ->
          File.must_recompute_cfg fundec;
          Cil.ChangeTo (Generic.translate_va_builtin fundec i)

        (* Translate variadic calls *)
        | Call(lv, Var vi, args, loc) ->
          begin
            try
              let mk_call f args = Call (lv, f, args, loc) in
              let translator = mk_translator vi in
              let instr_list = translate_call mk_call translator args in
              Cil.ChangeTo instr_list
            with Not_found ->
              Cil.DoChildren
          end

        | Call(lv, callee, args, loc) ->
          let is_variadic =
            List.mem Generic.vpar (Typ.params (Cil.typeOfLhost callee))
          in
          if is_variadic then begin
            let mk_call f args = Call (lv, f, args, loc) in
            let translator = Generic.translate_call callee in
            let instr_list = translate_call mk_call translator args in
            Cil.ChangeTo instr_list
          end else
            Cil.DoChildren

        | Local_init(v, ConsInit(c, args, kind), loc) ->
          begin
            try
              let mk_call f args =
                let args =
                  match kind, args with
                  | Constructor, [] ->
                    Kernel.fatal
                      "Constructor %a is expected to have at least one argument"
                      Cil_printer.pp_varinfo c
                  | Constructor, _::tl -> tl
                  | Plain_func, args -> args
                in
                let f =
                  match f with
                  | Var f -> f
                  | _ ->
                    Kernel.fatal
                      "Constructor cannot be translated as indirect call"
                in
                Local_init(v,ConsInit(f,args,kind),loc)
              in
              let args =
                match kind with
                | Plain_func -> args
                | Constructor -> Cil.mkAddrOfVi v :: args
              in
              let translator = mk_translator c in
              let instr_list = translate_call mk_call translator args in
              Cil.ChangeTo instr_list
            with Not_found ->
              Cil.DoChildren
          end
        | _-> Cil.DoChildren
      end

    method! vexpr exp =
      begin match exp.enode with
        | AddrOf (Var vi, NoOffset)
          when Ast_types.is_variadic vi.vtype
            && Classify.is_frama_c_builtin vi ->
          Kernel.not_yet_implemented
            ~source:(fst exp.eloc)
            "Option %s doesn't handle calls to a pointer to the \
             variadic builtin %s."
            Kernel.VariadicTranslation.name vi.vname
        | _ -> Cil.DoChildren
      end
  end
  in
  Cil.visitCilFile v file
