(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

let parse_to_cabs ~original (path : Filepath.t) =
  try
    Kernel.feedback ~level:2 "Parsing %a" Filepath.pretty path;
    Errorloc.clear_errors () ;
    let lexbuf, lexer =
      Clexer.init ~filename:(Filepath.to_string_abs path) Clexer.initial in
    (* The pwd during preprocessing might have changed, e.g. if a JCDB has
       been used; we may need to adjust Errorloc's working directory to
       compensate for it, otherwise relative line directives previously added
       by the preprocessor will be invalid. *)
    let pwd =
      match Parse_env.get_workdir path with
      | None -> Filepath.pwd ()
      | Some workdir -> workdir
    in
    Errorloc.setCurrentWorkingDirectory pwd;
    let cabs = Cparser.file lexer lexbuf in
    (* Cprint.print_defs cabs;*)
    Clexer.finish ();
    if Errorloc.had_errors () then begin
      Kernel.abort "There were parsing errors in %a"
        Filepath.pretty original
    end;

    (path, cabs)
  with
  | Sys_error msg ->
    Clexer.finish () ;
    Kernel.abort "Cannot open %a : %s" Filepath.pretty path msg ;
  | Parsing.Parse_error ->
    Errorloc.parse_error "syntax error"

module Syntactic_transformations = Hook.Fold(struct type t = Cabs.file end)
let add_syntactic_transformation = Syntactic_transformations.extend

let parse ~original path =
  Kernel.feedback ~level:2 "Parsing %a to Cabs" Filepath.pretty path;
  let cabs = parse_to_cabs ~original path in
  let cabs = Syntactic_transformations.apply cabs in
  Kernel.feedback ~level:2 "Converting %a from Cabs to CIL"
    Filepath.pretty path;
  let cil = Cabs2cil.convFile cabs in
  cil, cabs
