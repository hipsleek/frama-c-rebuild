(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module P =
  State_builder.Option_ref
    (SlicingTypes.Sl_project)
    (struct
      let name = "Slicing.Project"
      let dependencies = [] (* others delayed in Register *)
    end)

let self = P.self
let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:self
         [ Pdg.Api.self; Inout.self ])

let get () =
  try P.get ()
  with Not_found -> SlicingParameters.fatal "slicing not initialized."

let may f = match P.get_option () with
  | None -> ()
  | Some _ -> f ()

let may_map ~none f = match P.get_option () with
  | None -> none
  | Some _ -> f ()

let reset_slicing () =
  Eva.Analysis.compute () ;
  let initialized = match P.get_option () with | None -> false | Some _ -> true in
  if not initialized then
    SlicingParameters.feedback ~level:1 "initializing slicing ..."
  else
    SlicingParameters.feedback ~level:1 "reinitializing slicing ...";
  P.set
    SlicingInternals.{ functions = Cil_datatype.Varinfo.Hashtbl.create 17;
                       actions = [] };
  if not initialized then
    SlicingParameters.feedback ~level:2 "done (initializing slicing)."
  else
    SlicingParameters.feedback ~level:2 "done (reinitializing slicing)."
