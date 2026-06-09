(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

(* [VP] Need to get rid of those global references at some point. *)
let c_file = ref Filepath.empty
let dot_file = ref Filepath.empty

(* Performs some checks before calling [open_in f], reporting ["errmsg: <f>"]
   in case of error. *)
let check_and_open_in (f : Filepath.t) errmsg =
  if not (Filesystem.file_exists f) then
    Aorai_option.abort "%s: %a" errmsg Filepath.pretty f;
  open_in (Filepath.to_string_abs f)

let load_ya_file filename  =
  let channel = check_and_open_in filename "invalid Ya file" in
  let lexbuf = Lexing.from_channel channel in
  Lexing.(lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with pos_fname = Filepath.to_string_abs filename
    });
  try
    let automata = Yaparser.main Yalexer.token lexbuf in
    close_in channel;
    Data_for_aorai.setAutomata automata
  with
  | Parsing.Parse_error ->
    Utils_parser.abort_current lexbuf "syntax error"

let display_status () =
  if Aorai_option.verbose_atleast 2 then begin
    Aorai_option.feedback "\n"  ;
    Aorai_option.feedback "C file:            '%a'\n" Filepath.pretty !c_file ;
    Aorai_option.feedback "Entry point:       '%a'\n"
      Kernel_function.pretty (fst (Globals.entry_point())) ;
    if Aorai_option.Dot.get () then
      Aorai_option.feedback "Dot file:          '%a'\n" Filepath.pretty !dot_file;
  end

let init_file_names () =
  let freshname ?opt_suf file suf =
    let name = Filepath.to_string_abs file in
    let pre = Filename.remove_extension name in
    let pre = match opt_suf with None -> pre | Some s -> pre ^ s in
    let rec fn p s n =
      let fp = Filepath.of_string (p ^ (string_of_int n) ^ s) in
      if not (Filesystem.exists fp) then fp
      else fn p s (n+1)
    in
    let name =
      let fp = Filepath.of_string (pre^suf) in
      if not (Filesystem.exists fp) then fp
      else fn pre suf 0
    in
    name
  in
  if Aorai_option.Ya.is_empty () then
    Aorai_option.abort "empty Ya file name";
  if Aorai_option.Dot.get() then
    dot_file := freshname (Aorai_option.Ya.get ()) ".dot";
  display_status ()

let printverb s = Aorai_option.feedback ~level:2 "%s" s

let output () =
  (* Dot file *)
  if (Aorai_option.Dot.get()) then
    begin
      Pretty_automaton.Typed.output_dot_automata (Data_for_aorai.getAutomata ())
        (Filepath.to_string_abs !dot_file);
      printverb "Generating dot file    : done\n"
    end

let work () =
  let file = Ast.get () in
  Aorai_utils.initFile file;
  printverb "C file loading         : done\n";
  if not (Aorai_option.Ya.is_empty ()) then
    load_ya_file (Aorai_option.Ya.get ());
  let root = fst (Globals.entry_point ()) in
  let axiomatization = Aorai_option.Axiomatization.get() in
  if axiomatization then
    begin
      (* Step 5 : incrementing pre/post
             conditions with states and transitions information *)
      printverb "Refining pre/post      : \n";
      Aorai_dataflow.compute ();
      (* Step 6 : Removing transitions never crossed *)
      let automaton_has_states =
        if (Aorai_option.AutomataSimplification.get()) then
          begin
            printverb "Removing unused trans  : done\n";
            try
              Data_for_aorai.removeUnusedTransitionsAndStates ();
              true
            with Data_for_aorai.Empty_automaton ->
              Aorai_option.warning
                "No state of the automaton is reachable. \
                 Program and specification are incompatible, \
                 instrumentation will not be generated.";
              false
          end
        else
          (printverb "Removing unused trans  : skipped\n"; true)
      in
      if automaton_has_states then begin
        (* Step 7 : Labeling abstract file *)
        (* Finally the information is added into the Cil automata. *)
        Aorai_utils.initGlobals root axiomatization;
        Aorai_visitors.add_sync_with_buch file;
        if Aorai_option.GenerateAnnotations.get () then
          Aorai_visitors.add_pre_post_from_buch file
            (Aorai_option.advance_abstract_interpretation ())
        else if Aorai_option.ConsiderAcceptance.get () then begin
          let kf,_ = Globals.entry_point() in
          let bhv = Aorai_utils.mk_acceptance_bhv () in
          Annotations.add_behaviors Aorai_option.emitter kf [bhv]
        end;
        Aorai_eva_analysis.setup ();
        printverb "Annotation of Cil      : done\n";
      end
    end
  else
    begin
      (* Step 4': Computing the set of possible pre-states and post-states of each function *)
      (*          And so for pre/post transitions *)
      printverb "Abstracting pre/post   : skipped\n";

      (* Step 5': incrementing pre/post conditions with states and transitions information *)
      printverb "Refining pre/post      : skipped\n";

      (* Step 6 : Removing transitions never crossed *)
      printverb "Removing unused trans  : skipped\n";

      (* Step 7 : Labeling abstract file *)
      (* Finally the information is added into the Cil automata. *)
      Aorai_utils.initGlobals root axiomatization;
      Aorai_visitors.add_sync_with_buch file;
      printverb "Annotation of Cil      : partial\n"
    end;

  (* Step 8 : clearing tables whose information has been
     invalidated by our transformations.
  *)
  Ast.mark_as_changed();
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  Ast.clear_last_decl ();
  if Kernel.Check.get() then Filecheck.check_ast "aorai";
  output ()

let run () =
  (* Step 1 : Capture files names *)
  init_file_names ();
  (* Step 2 : Work in our own project, initialized by a copy of the main
     one. *)
  let work_prj =
    File.create_project_from_visitor "aorai"
      (fun prj -> new Visitor.frama_c_copy prj)
  in
  Project.copy ~selection:(Parameter_state.get_selection ()) work_prj;
  Project.on work_prj work ()

(* Plugin registration *)

let run =
  Dynamic.register
    ~plugin:"Aorai"
    "run"
    (Datatype.func Datatype.unit Datatype.unit)
    run

let run, _ =
  State_builder.apply_once
    "Aorai"
    (let module O = Aorai_option in
     [ O.Ya.self; O.Axiomatization.self; O.ConsiderAcceptance.self;
       O.AutomataSimplification.self; O.AbstractInterpretation.self;
       O.AddingOperationNameAndStatusInSpecification.self ])
    run

let main () = if Aorai_option.is_on () then run ()
let () = Boot.Main.extend main
