(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Basic plug-in internal documentation at the end of this file. *)

open Pdg_types

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:Pdg_tbl.self
         [ From.self ])

let compute_for_kf kf =
  let all = Pdg_parameters.BuildAll.get () in
  (all && Eva.Results.is_called kf) ||
  Kernel_function.Set.mem kf (Pdg_parameters.BuildFct.get ())

let compute () =
  Eva.Analysis.compute ();
  let do_kf_pdg kf =
    if compute_for_kf kf then
      let pdg = Pdg_tbl.get kf in
      let dot_basename = Pdg_parameters.DotBasename.get () in
      if dot_basename <> "" then
        let fname = Kernel_function.get_name kf in
        Pdg_tbl.print_dot pdg (dot_basename ^ "." ^ fname ^ ".dot")
  in
  Callgraph.Uses.iter_in_rev_order do_kf_pdg;
  let pp_sep fmt () = Format.pp_print_string fmt "," in
  Pdg_parameters.(
    debug "Logging keys : %a"
      (Format.pp_print_list ~pp_sep pp_category) (get_debug_keys ()));
  if Pdg_parameters.BuildAll.get () then
    Pdg_parameters.feedback "====== PDG GRAPH COMPUTED ======"

let output () =
  let bw  = Pdg_parameters.PrintBw.get () in
  let do_kf_pdg kf =
    if compute_for_kf kf then
      let pdg = Pdg_tbl.get kf in
      let header fmt =
        Format.fprintf fmt "PDG for %a" Kernel_function.pretty kf
      in
      Pdg_parameters.printf ~header "@[ @[%a@]@]"
        (PdgTypes.Pdg.pretty_bw ~bw) pdg
  in
  Callgraph.Uses.iter_in_rev_order do_kf_pdg

let compute_and_output () = compute (); output ()

let run () =
  if Pdg_parameters.BuildAll.get ()
  || not (Kernel_function.Set.is_empty (Pdg_parameters.BuildFct.get ()))
  then compute_and_output ()

let deps =
  [Pdg_tbl.self; Pdg_parameters.BuildAll.self; Pdg_parameters.BuildFct.self]

let run_once, _ = State_builder.apply_once "Pdg.Register.main" deps run

let () = Boot.Main.extend run_once

(* {2 Overview}

   The main modules are :
   - {!module: PdgIndex} that can be used to store different kind of information
     related to a function (not only related to PDG)
   - the types are defined in {!module: PdgTypes}.
   - the PDG computation is done in {!module: Build}.  It also use the lexical
     successor graph, which is computed in {!module:Lexical_successors}.
   - {!module:Sets} provides functions to read a PDG.
   - {!module:Print} provides functions to print a PDG either in textual form or
     in a dot file (See {i "How to see a PDG"} below).


   {2 What is a PDG ?}

   A {b Program Dependences Graph} represent the dependences between the
   statements of a function. So the nodes of the graph mainly represent the
   statements (some more nodes are used to represents things like declarations,
   inputs, outputs, etc.) and the edges represent the dependences.

   [Y -> X] means that the computation of the statement Y depend on (the result of)
   the statement X. Example :
   {C {v X : x = a + b; Y : y = x + 1; v}}

   There are three kinds of dependencies :
   - a {b data} dependency : the simpler one, illustrated by the above example,
   - a {b control} dependency :
     {C Example : {v if (c) X : x = a + b; v}}
     X is control dependent on (c) because the statement will be executed or not
     according to the evaluation of the condition,
   - an {b address} dependency : dependencies on the elements that are used to
     compute the left part of an assignment, ie that decide which data will be
     modified.
     {C Example : {v t[i] = 3; v}}
    We say that this statement have address dependencies on the declaration of
    [tab] and the computation of [i].

   A dependency between two nodes can have any combination of these kinds.

   {2 Dynamic dependencies}

   After having built the PDG for a function, there is a way of adding dynamically
   some dependencies to it. There are not stored directly in the PDG so they can be
   cleared later on.

   As PDG doesn't interpret the annotations of the code,
   this feature can for instance be used to add dependencies on assertions.

   To see an example of how to use it, please have a look at
   [tests/pdg/dyn_dpds.ml].

   {2 How to see a PDG ?}

   Please, use the [-help] option of the tool to get the PDG options names.

   The PDG of a function can be seen either in textual form
   or exported in a {b dot} file
   which is the format of the {{:http://www.graphviz.org/}Graphviz}
   tool set.
   They can be viewed using
   {{:http://zvtm.sourceforge.net/zgrviewer.html}zgrviewer}
   or exported in SVG format to be seen with some browser
   or {{:http://www.inkscape.org/}Inkscape}.

   The graph is unfortunately generated with the output of the function at the top
   and its inputs at the bottom. If you find it uncomfortable to read,
   just change [TB] by [BT] in the [rankdir] property at the beginning of the
   dot file before viewing it.

   The color and the shape of the nodes are used to make it easier to read the
   graph, but add no more meaning.

   For the edges :
   - the color (blue) represent the {b data} dependency,
   - the shape of the arrow (circled) represent the {b control} dependency,
   - and the kind of line (dotted) represent the {b address} dependency.

   So a solid blue edge with a circle arrow represent a data+control dependency for
   instance, while a dotted black edge with a triangle arrow represent a address
   dependency.
*)
