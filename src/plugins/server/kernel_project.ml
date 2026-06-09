(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Data
module Md = Markdown

let package = Package.package ~name:"project"
    ~title:"Project Management" ~readme:"project.md" ()


module Jproject_id = Jint

let _current_project_signal =
  States.register_state ~package
    ~name:"current"
    ~descr:(Md.plain "Current Frama-C project")
    ~data:(module Jproject_id)
    ~get:(fun () -> Project.(current () |> get_pid))
    ~set:(fun pid -> Project.(from_pid pid |> set_current))
    ~add_hook:(Project.register_after_set_current_hook ~user_only:false)
    ()

let () = Request.register
    ~package ~kind:`SET ~name:"create"
    ~descr:(Md.plain "Creates a new Frama-C project with the given name")
    ~input:(module Jstring) ~output:(module Junit)
    (fun name -> Project.create name |> Project.set_current)

let no_project_found pid =
  Format.asprintf "No project with id %d found." pid

let () = Request.register
    ~package ~kind:`SET ~name:"rename"
    ~descr:(Md.plain "Rename a project")
    ~input:(module Jpair (Jproject_id) (Jstring))
    ~output:(module Joption (Jstring))
    (fun (project_id, new_name) ->
       try
         let project = Project.from_pid project_id in
         Project.set_name project new_name;
         None
       with Project.Unknown_project ->
         let err = no_project_found project_id in
         Some err)

let () = Request.register
    ~package ~kind:`SET ~name:"remove"
    ~descr:(Md.plain "Remove a project from the session")
    ~input:(module Jproject_id) ~output:(module Joption (Jstring))
    (fun project_id ->
       try
         let project = Project.from_pid project_id in
         Project.remove ~project ();
         None
       with
       | Project.Unknown_project ->
         let err = no_project_found project_id in
         Some err
       | Project.Cannot_remove p ->
         let err = Format.asprintf "Cannot remove project %s." p in
         Some err)

let () = Request.register
    ~package ~kind:`SET ~name:"copy"
    ~descr:(Md.plain "Duplicate a project")
    ~input:(module Jpair (Jproject_id) (Jstring)) ~output:(module Joption (Jstring))
    (fun (project_id, new_name) ->
       try
         let project = Project.from_pid project_id in
         let _ = Project.create_by_copy ~last:false ~src:project new_name in
         None
       with Project.Unknown_project ->
         let err = no_project_found project_id in
         Some err)

let () = Request.register
    ~package ~kind:`SET ~name:"load"
    ~descr:(Md.plain "Load a saved project")
    ~input:(module Jfile) ~output:(module Joption (Jstring))
    (fun filepath ->
       try
         Project.load filepath
         |> Project.set_current;
         None
       with Project.IOError err ->
         Some err)

let () = Request.register
    ~package ~kind:`SET ~name:"save"
    ~descr:(Md.plain "Save a project on disk")
    ~input:(module Jpair (Jproject_id) (Jfile)) ~output:(module Joption (Jstring))
    (fun (project_id, filepath) ->
       try
         let project = Project.from_pid project_id in
         Project.save ~project filepath;
         None
       with
       | Project.Unknown_project ->
         let err = no_project_found project_id in
         Some err
       | Project.IOError err ->
         Some err)

let _project_list =
  let model = States.model () in

  States.column model ~name:"id"
    ~descr:(Md.plain "Project ID")
    ~data:(module Jproject_id)
    ~get:Project.get_pid;

  States.column model ~name:"name"
    ~descr:(Md.plain "Project name")
    ~data:(module Jstring)
    ~get:Project.get_name;

  let add_update_hook f =
    Project.register_create_hook f;
    Project.register_after_load_hook f;
    Project.register_after_set_name_hook (fun (p, _) -> f p);
  in
  let add_remove_hook f =
    Project.register_before_remove_hook f
  in
  let add_reload_hook f =
    Project.register_after_global_load_hook f;
  in
  States.register_array ~package
    ~name:"list"
    ~descr:(Md.plain "List of Frama-C projects")
    ~key:(fun p -> Project.get_pid p |> string_of_int)
    ~iter:Project.iter_on_projects
    ~add_update_hook
    ~add_remove_hook
    ~add_reload_hook
    model
