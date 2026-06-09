(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

let warn kind name =
  Options.warning ~once:true "unobfuscated %s name `%s'" kind name

class visitor = object

  inherit Visitor.frama_c_inplace

  val varinfos_visited = Varinfo.Hashtbl.create 17
  val logic_vars_visited = Logic_var.Hashtbl.create 7
  val id_pred_visited = Identified_predicate.Hashtbl.create 7

  method! vtype t =
    match t.tnode with
    | TFun(rt, args, variadic) ->
      let args' =
        match args with
        | None -> None
        | Some l ->
          Some
            (List.map
               (fun (s,t,a) ->
                  (Dictionary.fresh Obfuscator_kind.Formal_in_type s, t, a)) l)
      in
      Cil.ChangeDoChildrenPost(Cil_const.mk_tfun ~tattr:t.tattr rt args' variadic, Fun.id)
    | _ -> Cil.DoChildren

  method! vglob_aux = function
    | GType (ty,_) ->
      if not (Cil.is_in_libc (Ast_types.get_attributes ty.ttype)) then
        ty.tname <- Dictionary.fresh Obfuscator_kind.Type ty.tname;
      Cil.DoChildren
    | GVarDecl (v, _) | GVar (v, _, _)
    | GFun ({svar = v}, _) | GFunDecl (_, v, _)
      when Cil_builtins.is_unused_builtin v ->
      Cil.SkipChildren
    | _ ->
      Cil.DoChildren

  method! vcompinfo ci =
    ci.cname <- Dictionary.fresh Obfuscator_kind.Type ci.cname;
    Cil.DoChildren

  method! venuminfo ei =
    ei.ename <- Dictionary.fresh Obfuscator_kind.Type ei.ename;
    Cil.DoChildren

  method! vfieldinfo fi =
    fi.fname <- Dictionary.fresh Obfuscator_kind.Field fi.fname;
    Cil.DoChildren

  method! venumitem ei =
    ei.einame <- Dictionary.fresh Obfuscator_kind.Enum ei.einame;
    Cil.DoChildren

  method! vvdec vi =
    (* Varinfo can be visited (and obfuscated) more than once:
       functions for their declaration and definition, variables
       as parts of the type of the function, and in the body of
       the function declaration, etc. Thus we make sure that the
       obfuscator does not visit them twice.
       Moreover, string literals have their own special treatment.
    *)
    if Varinfo.Hashtbl.mem varinfos_visited vi || Ast_info.is_string_literal vi
    then
      Cil.SkipChildren
    else begin
      if Ast_types.is_fun vi.vtype then begin
        if vi.vname <> "main"
        && not (Cil_builtins.is_builtin vi)
        && not (Cil.is_in_libc vi.vattr) then
          vi.vname <- Dictionary.fresh Obfuscator_kind.Function vi.vname
      end
      else begin
        let add =
          if vi.vglob then Dictionary.fresh Obfuscator_kind.Global_var
          else if vi.vformal then Dictionary.fresh Obfuscator_kind.Formal_var
          else Dictionary.fresh Obfuscator_kind.Local_var
        in
        vi.vname <- add vi.vname;
      end;
      Varinfo.Hashtbl.add varinfos_visited vi ();
      Cil.DoChildren
    end

  method! vlogic_var_decl lvi =
    match lvi.lv_kind with
    | LVGlobal | LVFormal | LVQuant | LVLocal ->
      if Logic_var.Hashtbl.mem logic_vars_visited lvi then
        Cil.SkipChildren
      else begin
        lvi.lv_name <- Dictionary.fresh Obfuscator_kind.Logic_var lvi.lv_name;
        Logic_var.Hashtbl.add logic_vars_visited lvi ();
        Cil.DoChildren
      end
    | LVC ->
      Cil.SkipChildren

  method! vstmt_aux stmt =
    let labels =
      List.map
        (function
          | Label(s, loc, true) ->
            (* only obfuscate user's labels, not Cil's ones *)
            let s' = Dictionary.fresh Obfuscator_kind.Label s in
            Label(s', loc, true)
          | Label(_, _, false) | Case _ | Default _ as label -> label)
        stmt.labels
    in
    stmt.labels <- labels;
    Cil.DoChildren

  method! videntified_predicate p =
    if Identified_predicate.Hashtbl.mem id_pred_visited p then
      Cil.SkipChildren
    else begin
      Identified_predicate.Hashtbl.add id_pred_visited p ();
      let { tp_kind; tp_statement = pred } = p.ip_content in
      let names = pred.pred_name in
      let names' =
        List.map (Dictionary.fresh Obfuscator_kind.Predicate) names
      in
      let pred' = { pred with pred_name = names' } in
      let ip_content = Logic_const.toplevel_predicate ~kind:tp_kind pred' in
      let p' = { p with ip_content } in
      Cil.ChangeDoChildrenPost (p', Fun.id)
    end

  method! vterm t =
    List.iter (fun s -> warn "term" s) t.term_name;
    Cil.DoChildren

  method! vannotation = function
    | Daxiomatic(str, globs, attrs, loc) ->
      let str' = Dictionary.fresh Obfuscator_kind.Axiomatic str in
      Cil.ChangeDoChildrenPost(Daxiomatic(str',globs,attrs,loc),Fun.id)
    | Dlemma(str, labs, typs, pred, attrs, loc) ->
      let str' = Dictionary.fresh Obfuscator_kind.Lemma str in
      Cil.ChangeDoChildrenPost(
        Dlemma(str',labs,typs, pred, attrs, loc),Fun.id)
    | _ ->
      Cil.DoChildren

  method! vmodel_info mi =
    warn "model" mi.mi_name;
    Cil.DoChildren

  method! vlogic_type_info_decl lti =
    if not (Logic_env.is_builtin_logic_type lti.lt_name)
    then lti.lt_name <- Dictionary.fresh Obfuscator_kind.Logic_type lti.lt_name ;
    Cil.DoChildren

  method! vlogic_ctor_info_decl lci =
    if not (Logic_env.is_builtin_logic_ctor lci.ctor_name)
    then
      lci.ctor_name <-
        Dictionary.fresh Obfuscator_kind.Logic_constructor lci.ctor_name ;
    Cil.DoChildren

  method! vattr (str, _) =
    warn "attribute" str;
    Cil.DoChildren

  method! vattrparam p =
    (match p with
     | AStr str | ACons(str, _) | ADot(_, str) -> warn "attribute parameter" str
     | _ -> ());
    Cil.DoChildren

end

let obfuscate_behaviors () =
  (* inheriting method vbehavior or vspec does not work since only a copy of the
     piece of spec is provided. *)
  Globals.Functions.iter
    (fun kf ->
       let h = Datatype.String.Hashtbl.create 7 in
       Annotations.iter_behaviors_by_emitter
         (fun emitter b ->
            if Emitter.equal emitter Emitter.end_user
            && not (Cil.is_default_behavior b)
            then begin
              Annotations.remove_behavior ~force:true emitter kf b;
              let new_ = Dictionary.fresh Obfuscator_kind.Behavior b.b_name in
              Datatype.String.Hashtbl.add h b.b_name new_;
              b.b_name <- new_;
              Annotations.add_behaviors emitter kf [ b ];
            end)
         kf;
       let handle_bnames iter remove add =
         iter
           (fun emitter l ->
              remove emitter kf l;
              add emitter kf (List.map (Datatype.String.Hashtbl.find h) l))
           kf
       in
       handle_bnames
         Annotations.iter_complete
         (fun e kf l -> Annotations.remove_complete e kf l)
         (fun e kf l -> Annotations.add_complete e kf l);
       handle_bnames
         Annotations.iter_disjoint
         (fun e kf l -> Annotations.remove_disjoint e kf l)
         (fun e kf l -> Annotations.add_disjoint e kf l))

let define_string_lit fmt v =
  Format.fprintf fmt "#define %s %a@\n"
    v.vname Cil_printer.pp_str_literal (Globals.Vars.get_string_literal v)

module UpdatePrinter (X: Printer.PrinterClass) = struct
  (* obfuscated printer *)
  class printer () = object(self)
    inherit X.printer () as super

    method! file fmt ast =
      let literal_strings =
        Globals.Vars.fold
          (fun v _ acc ->
             if Ast_info.is_string_literal v then
               Cil_datatype.Varinfo.Set.add v acc
             else acc)
          Cil_datatype.Varinfo.Set.empty
      in
      if not (Cil_datatype.Varinfo.Set.is_empty literal_strings) then begin
        let string_fmt =
          if Options.String_literal.is_default () then fmt
          else begin
            let file = Options.String_literal.get () in
            try
              let cout = open_out file in
              Format.formatter_of_out_channel cout
            with Sys_error _ as exn ->
              Options.error "@[cannot generate the literal string dictionary \
                             into file `%s':@ %s@]"
                file
                (Printexc.to_string exn);
              fmt
          end
        in
        Format.fprintf string_fmt "\
/* *********************************************************** */@\n\
/* start of dictionary required to compile the obfuscated code */@\n\
/* *********************************************************** */@\n";
        Cil_datatype.Varinfo.Set.iter
          (define_string_lit string_fmt) literal_strings;
        Format.fprintf string_fmt "\
/* ********************************************************* */@\n\
/* end of dictionary required to compile the obfuscated code */@\n\
/* ********************************************************* */@\n@\n";
        if fmt != string_fmt then begin
          Format.pp_print_flush string_fmt ();
          Format.fprintf fmt "\
/* include the dictionary of literal strings */@\n\
@[#include \"%s\"@]@\n@\n"
            (Options.String_literal.get ())
        end
      end;
      super#file fmt ast

    method! lval fmt lv =
      match lv with
      | Var v, (NoOffset | Index _ as o)
        when Ast_info.is_string_literal v ->
        Format.fprintf fmt "%s%a" v.vname self#offset o
      | _ -> super#lval fmt lv

    method! term_lval fmt lv =
      match lv with
      | TVar { lv_origin = Some v }, (TNoOffset | TIndex _ as o)
        when Ast_info.is_string_literal v ->
        Format.fprintf fmt "%s%a" v.vname self#term_offset o
      | _ -> super#term_lval fmt lv

    method! global fmt g =
      match g with
      (* do not output literal string globals even with -print-as-is
         (which makes little sense with -obfuscate anyways)
      *)
      | GVarDecl (v,_) | GVar(v,_,_) when Ast_info.is_string_literal v -> ()
      | _ -> super#global fmt g

  end
end

let obfuscate () =
  Dictionary.mark_as_computed ();
  obfuscate_behaviors ();
  Visitor.visitFramacFile
    (new visitor :> Visitor.frama_c_visitor)
    (Ast.get ());
  Printer.update_printer (module UpdatePrinter: Printer.PrinterExtension)
