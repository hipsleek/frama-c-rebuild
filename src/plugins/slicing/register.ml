(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Basic plug-in internal documentation at the end of this file. *)

(** Find a unique name to export the sliced project into.
    The name is derived from the given [project_name] and the
    [ExportedProjectPostfix] parameter. *)
let get_sliced_project_name project_name =
  let sliced_project_name =
    project_name ^ (SlicingParameters.ExportedProjectPostfix.get ())
  in
  let rec aux s idx =
    match Project.find_all s with
    | [] -> s
    | _ :: _ ->
      let idx = succ idx in
      let s = Format.asprintf "%s %d" s idx in
      aux s idx
  in
  aux sliced_project_name 1


let main () =
  if SlicingParameters.is_on () then begin
    SlicingParameters.feedback ~level:1 "slicing requests in progress...";

    (* have to do the value analysis before the selections
     * because some functions use its results,
     * and the value analysis is not launched automatically. *)
    Eva.Analysis.compute ();

    let project_name = SlicingParameters.ProjectName.get () in
    Api.Project.reset_slicing ();
    Api.Request.add_persistent_cmdline ();
    (* Apply all pending requests. *)
    if Api.Request.is_request_empty_internal () then
      begin
        SlicingParameters.warning "No internal slicing request from the command line." ;
        if SlicingParameters.Mode.Callers.get () then
          let kf_entry, _library = Globals.entry_point () in
          SlicingParameters.warning "Adding an extra request on the entry point of function: %a." Kernel_function.pretty kf_entry;
          let set = Api.Select.empty_selects in
          let set = Api.Select.select_func_calls_into set ~spare:true kf_entry in
          Api.Request.add_persistent_selection set
      end;

    Api.Request.apply_all_internal ();

    if SlicingParameters.Mode.Callers.get () then
      Api.Slice.remove_uncalled ();
    let sliced_project_name = get_sliced_project_name project_name in
    SlicingParameters.set_off ();
    let sliced_project = Api.Project.extract sliced_project_name in
    Project.on sliced_project SlicingParameters.clear ();
    SlicingParameters.feedback ~level:2 "done (slicing requests in progress).";
  end

(** Register the function [main] as a main entry point. *)
let () = Boot.Main.extend main

(* {2 Overview}

   To have more details about what we are trying to do,
   you may have a look to the specification report (in French).

   The internal types module ({!module:SlicingTypes.Internals})
   can give a pretty good idea of the kind of objects that we deal with in this
   module.

   You can also find some general information below.

   {3 Project}

   The project was the global repository of the results obtained so far.
   If is mainly composed of
   a list of actions waiting to be applied,
   and the already computed slices.

   More precisely, see its type definition
   {!type:SlicingTypes.Internals.t_project}
   if you want to know what it is composed of,
   and the module {!module:SlicingProject} of the functions to handle it.

   {3 Program Dependence Graph}

   This computation is not part of this module anymore.
   See the {{:../html/Db.Pdg.html}API of Pdg module}.

   It is enough to know that the PDG of a function is a graph composed
   of nodes that represent the elements of a function (declarations, statements,
   and so on) and of edges that represent the dependencies relations between those
   elements.

   {3 Sliced function}

   A sliced function contains a mapping between the PDG nodes of a function
   and the some marks that are computed by the application of the actions.
   It also has a mapping between the function calls and the function called by the
   slice that can be either some other slices, or the source function
   (or nothing if the call is invisible in that slice).

   There can be more than one slice for a source function.

   See their type {!type:SlicingTypes.Internals.t_fct_slice},
   and the associated functions in {!module:Fct_slice}.

   See also {!module:SlicingMarks}
   for more information about the low level marks computation.

   {3 Actions}

   The actions are the way of giving an order to modify the current application.

   There are many kinds of actions, but only one is really used to build the
   slice which is a list of nodes from the PDG of a function,
   and their associated marks.
   All the other actions dealing with the marks are first decomposed
   before being applied.

   Some other actions are can be used to manage the interprocedural part,
   ie. which slice to call where.

   See the top type {!type:SlicingTypes.Internals.t_criterion} or
   the functions in {!module:SlicingActions}.

   {3 Options}

   The propagation of the marks to the function call depend on a
   {!type:SlicingTypes.Internals.t_call_option}.
   Choosing this level makes it possible to obtain a more or less precise result.

   {3 High level commands}

   The module {!module:SlicingCmds} is a bit external because it only uses
   the {{:../html/Db.Pdg.html}slicing API} to define higher level function
   that are only a composition of the basic functions.

   {3 Producing a result }

   When there are non more actions in the task list,
   the project can be exported. This is done in
   {!module:SlicingTransform} module
   by building a new CIL application.
*)
