(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let analyses_feedback msg =
  Options.feedback ~level:2 "%s in %a" msg Project.pretty (Project.current ())

module Terms = Misc.Id_term.Hashtbl

let check_integrity () =
  if Kernel.Check.get () then begin
    let visited_terms = Terms.create 7 in
    let integrity_checker = object
      inherit Visitor.frama_c_inplace

      (* do not check function declaration
         reason: for some function definition with a contract
         Prepare_ast.prepare_global will generate a declaration
         for that function with the same contract *)
      method! vglob = function
        | GFunDecl _ -> Cil.SkipChildren
        | _ -> Cil.DoChildren

      method! vannotation = function
        | Dfun_or_pred (li, _) ->
          begin match li.l_body with
            | LBinductive _ ->
              (* inductives are translated into predicate/function definitions,
                 but we also keep the old definition in the file, without unsharing.
                 But these are not translated, so it's no problem. *)
              Cil.SkipChildren
            | _ ->
              if li.l_labels <> [] then
                (* because of Here-inlining:
                   it creates a copy without unsharing the contracts, but
                   we only translate predicates/functions without labels *)
                Cil.SkipChildren
              else
                Cil.DoChildren
          end
        | _ -> Cil.DoChildren

      method! vterm t =
        let () =
          if Terms.mem visited_terms t
          then Options.fatal "shared term %a in AST" Printer.pp_term t
          else Terms.add visited_terms t ()
        in
        Cil.DoChildren
    end in
    ignore @@ Visitor.visitFramacFile integrity_checker @@ Ast.get ();
  end

let preprocess () =
  let ast = Ast.get () in
  analyses_feedback "preprocessing annotations";
  Logic_normalizer.preprocess ast;
  analyses_feedback "normalizing quantifiers";
  Bound_variables.preprocess ast;
  analyses_feedback "inferring RTEs";
  Rte_analysis.preprocess ast;
  analyses_feedback "inferring interval of annotations";
  Interval.infer_program ast;
  check_integrity ();
  analyses_feedback "typing annotations";
  Typing.type_program ast;
  analyses_feedback
    "computing future locations of labeled predicates and terms";
  Labels.preprocess ast

let reset () =
  Memory_tracking.reset ();
  Literal_strings.reset ();
  Bound_variables.clear_guards ();
  Logic_normalizer.clear ();
  Rte_analysis.clear ();
  Inductive.clear ();
  Interval.clear ();
  Typing.clear ();
  Labels.reset ()
