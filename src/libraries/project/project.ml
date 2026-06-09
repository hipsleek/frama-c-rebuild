(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* ************************************************************************** *)
(** {2 Initialization} *)
(* ************************************************************************** *)

let not_inilialized () =
  failwith "Project.init must be called at least once during program initialization"

let get_source = ref not_inilialized
let get_seed   = ref not_inilialized

let init ~seed ~source =
  get_source := (fun () -> source);
  get_seed := (fun () -> seed)

(* ************************************************************************** *)
(** {2 Warning, debug and feedback} *)
(* ************************************************************************** *)

let debug = ref false
let feedback = ref false
let warning_level = ref 2

let set_debug b = debug := b
let set_feedback b = feedback := b
let set_warn_level l = warning_level := l

let pretty kind fmt msg =
  let evt = {
    Log.evt_kind = kind;
    evt_plugin = !get_source ();
    evt_category = Some "project";
    evt_source = None;
    evt_message = Rich_text.of_string msg;
  } in
  Format.fprintf fmt "%a" (Log.Event.pretty ?truncate:None) evt

let print_aux ?(post=Fun.id) should_print kind msg =
  if should_print then
    Format.kasprintf (fun str ->
        Format.printf "%a" (pretty kind) str; post ()
      ) msg
  else Pretty_utils.nullprintf msg

let print_warning msg =
  let should_print = !warning_level > 0 in
  let kind =
    match !warning_level with
    | x when x <= 0 -> Log.Result (* will not be printed anyway *)
    | 1 -> Feedback
    | 2 -> Warning
    | _ -> Failure
  in
  let post () =
    if !warning_level >= 3 then failwith "An error occurred in project library"
  in
  print_aux ~post should_print kind msg

let print_debug msg = print_aux !debug Debug msg
let print_feedback msg = print_aux !feedback Feedback msg

let guarded_feedback selection fmt_msg =
  if !feedback then begin
    if State_selection.is_full selection then
      print_feedback fmt_msg
    else
      let n = State_selection.cardinal selection in
      if n = 0 then Pretty_utils.nullprintf fmt_msg
      else
        let states =
          if n > 1 then Format.sprintf " (for %d states)" n
          else Format.sprintf " (for 1 state)"
        in
        let f s = print_feedback "%s%s" s states in
        Format.kasprintf f fmt_msg
  end
  else
    Pretty_utils.nullprintf fmt_msg

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

let compress_saved_session = ref true

(* Keep [p] as the current project when calling {!on p}. *)
let keep_current: bool ref = ref false

let set_keep_current b = keep_current := b

(* ************************************************************************** *)
(** {2 Project skeleton} *)
(* ************************************************************************** *)

open Project_skeleton

(* re-exporting record fields *)
type project = t = private
  { pid : int;
    mutable name : string }

let rehash_ref = ref (fun _ -> assert false)

module D =
  Datatype.Make_with_collections
    (struct
      type t = project
      let name = "Project"
      let structural_descr =
        Structural_descr.t_record
          [| Structural_descr.p_int;
             Structural_descr.p_string |]
      let reprs = [ dummy ]
      let equal = (==)
      let compare p1 p2 = Datatype.Int.compare p1.pid p2.pid
      let hash p = p.pid
      let rehash x = !rehash_ref x
      let copy = Datatype.undefined
      let pretty fmt p = Format.fprintf fmt "project %S" p.name
      let mem_project f x = f x
    end)
include (D: Datatype.S_no_copy with type t = Project_skeleton.t)

module Project_tbl = Hashtbl.Make(D)

(* ************************************************************************** *)
(** {2 States operations} *)
(* ************************************************************************** *)

let current_selection = ref State_selection.empty
let get_current_selection () = !current_selection

module States_operations = struct

  module H = Hashtbl
  open State
  module Hashtbl = H

  let iter f x =
    current_selection := State_selection.full;
    State_dependency_graph.G.iter_vertex
      (fun s -> f s x)
      State_dependency_graph.graph

  let iter_on_selection
      ?(iter=State_selection.iter) ?(selection=State_selection.full) f x =
    current_selection := selection;
    iter (fun s -> f s x) selection

  let fold_on_selection ?(selection=State_selection.full) f x =
    current_selection := selection;
    State_selection.fold (fun s -> f s x) selection

  let create = iter (fun s -> (private_ops s).create)
  let remove = iter (fun s -> (private_ops s).remove)
  let clean = iter (fun s -> (private_ops s).clean)

  let commit ?selection =
    iter_on_selection ?selection (fun s -> (private_ops s).commit)

  let update ?selection =
    (* since the developer may add hooks on update which may depend on each
       others, iterating in the dependencies order is required. *)
    iter_on_selection
      ~iter:State_selection.iter_in_order
      ?selection
      (fun s -> (private_ops s).update)

  let clear ?(selection=State_selection.full) p =
    print_debug "clearing following selection:@.  @[%a@]@.%a"
      State_selection.pretty_witness selection State_selection.pretty selection;
    let clear s = (private_ops s).clear in
    if State_selection.is_full selection then
      iter clear p (* clearing the static states also clears the dynamic ones *)
    else begin
      current_selection := selection;
      State_selection.iter (fun s -> clear s p) selection
    end

  let clear_some_projects ?selection f p =
    let states_to_clear =
      fold_on_selection
        ?selection
        (fun s p acc ->
           let is_cleared = (private_ops s).clear_some_projects f p in
           if is_cleared then
             State_selection.union
               (State_selection.with_dependencies s)
               acc
           else
             acc)
        p
        State_selection.empty
    in
    if not (State_selection.is_empty states_to_clear) then begin
      print_warning "clearing dangling project pointers in project %S" p.name;
      print_debug "@[the involved states are:%t@]"
        (fun fmt ->
           iter_on_selection
             ~selection:states_to_clear
             (fun s () -> Format.fprintf fmt "@ %S" (get_name s))
             ())
    end

  let copy ?selection src =
    iter_on_selection ?selection (fun s -> (private_ops s).copy src)

  let serialize ?selection p =
    fold_on_selection
      ?selection
      (fun s p acc -> (get_unique_name s, (private_ops s).serialize p) :: acc)
      p
      []

  let unserialize ?selection dst loaded_states =
    let pp_err fmt n msg_sing msg_plural =
      if n > 0 then begin
        print_warning fmt n
          (if n = 1 then "" else "s")
          (if n = 1 then msg_sing else msg_plural)
      end
    in
    let tbl = Hashtbl.create 97 in
    List.iter (fun (k, v) -> Hashtbl.add tbl k v) loaded_states;
    let invalid_on_disk = State.Hashtbl.create 7 in
    iter_on_selection
      ?selection
      (fun s () ->
         try
           let n = get_unique_name s in
           let d = Hashtbl.find tbl n in
           (try
              (private_ops s).unserialize dst d;
              (* do not remove if [State.Incompatible_datatype] occurs *)
              Hashtbl.remove tbl n
            with
            | Not_found ->
              failwith "unexpected 'Not_found' when unserializing; \
                        possibly an issue with a hook"
            | State.Incompatible_datatype _ ->
              (* datatype of [s] on disk is incompatible with the one in RAM: as
                 [dst] is a new project, [s] is already equal to its default
                 value. However must clear the dependencies for consistency, but
                 it is doable only when all states are loaded. *)
              State.Hashtbl.add invalid_on_disk s ())
         with Not_found ->
           (* [s] is in RAM but not on disk: silently ignore it!  Furthermore,
              all the dependencies of [s] are consistent with this default
              value. So no need to clear them. Whenever the value of [s] in
              [dst] changes, the dependencies will be cleared (if required by
              the user). *)
           ())
      ();
    (* warns for the saved states that cannot be loaded
       (either they are not in RAM or they are incompatible). *)
    let nb_ignored =
      Hashtbl.fold (fun _ s n -> if s.on_disk_saved then succ n else n) tbl 0
    in
    pp_err
      "%d state%s in saved file ignored. %s this Frama-C configuration."
      nb_ignored
      "It is invalid in"
      "They are invalid in";
    if !debug then
      Hashtbl.iter (fun k s ->
          if s.on_disk_saved then print_debug "ignoring state %s" k
        ) tbl;
    (* after loading, reset dependencies of incompatible states *)
    let to_be_cleared =
      State.Hashtbl.fold
        (fun s () ->
           State_selection.union
             (State_selection.only_dependencies s))
        invalid_on_disk
        State_selection.empty
    in
    let nb_cleared = State_selection.cardinal to_be_cleared in
    if nb_cleared > 0 then begin
      pp_err "%d state%s in memory reset to their default value. \
              %s this Frama_C configuration."
        nb_cleared
        "It is inconsistent in"
        "They are inconsistent in";
      clear ~selection:to_be_cleared dst
    end

end

module Q = Qstack.Make(struct type t = project let equal = equal end)

let projects = Q.create ()
(* The stack of projects. *)

let current () = Q.top projects
let is_current p = equal p (current ())

let last_created_by_copy_ref: t option ref = ref None
let last_project_created_by_copy  () =
  Option.map (fun p -> p.pid) !last_created_by_copy_ref

let iter_on_projects f = Q.iter f projects
let fold_on_projects f acc = Q.fold f acc projects

let find_all name = Q.filter (fun p -> p.name = name) projects

let pick_most_recently_created projects =
  (* Since the IDs of projects are monotonically increasing, we can order the
     list from the greatest to least pid and return the first project to get the
     most recently created one. *)
  let compare { pid = lpid; _ } { pid = rpid; _ } =
    Int.compare rpid lpid
  in
  List.sort compare projects |> List.hd

exception Unknown_project
let from_pid pid =
  try Q.find (fun p -> p.pid = pid) projects
  with Not_found -> raise Unknown_project

module Setter = Make_setter ()

module Set_Name_Hook = Hook.Build(struct type t = project * string end)

let register_after_set_name_hook = Set_Name_Hook.extend

let set_name p s =
  print_feedback "renaming project %S to %S" p.name s;
  let old_name = p.name in
  Setter.set_name p s;
  Set_Name_Hook.apply (p, old_name);

module Create_Hook = Hook.Build(struct type t = project end)
let register_create_hook = Create_Hook.extend

let create name =
  print_feedback "creating project %S" name;
  let p = Setter.make name in
  print_feedback "its unique name is %S" (get_project_debug_name p);
  Q.add_at_end p projects;
  States_operations.create p;
  Create_Hook.apply p;
  p

let get_pid p = p.pid
let get_name p = p.name
let get_debug_name = get_project_debug_name

let get_current_pid () = current () |> get_pid

let pid_to_name pid = from_pid pid |> get_name

let name_to_pid p_name =
  let project =
    match find_all p_name with
    | [ p ] -> Some p
    | _ :: _ as projects ->
      print_debug
        "multiple projects named `%s', choosing most recently created"
        p_name;
      Some (pick_most_recently_created projects)
    | [] -> None
  in
  Option.map get_pid project

exception NoProject = Q.Empty

module Set_Current_Hook_User = Hook.Build (struct type t = project end)
module Set_Current_Hook = Hook.Build(struct type t = project end)

let register_after_set_current_hook ~user_only =
  if user_only then Set_Current_Hook_User.extend else Set_Current_Hook.extend

let force_set_current =
  let apply_hook = ref false in
  fun on selection p ->
    if not (Q.mem p projects) then
      invalid_arg ("Project.set_current: " ^ p.name ^ " does not exist");
    let old = current () in
    States_operations.commit ~selection old;
    (try Q.move_at_top p projects with Invalid_argument _ -> assert false);
    guarded_feedback selection "%S is now the current project" p.name;
    assert (equal p (current ()));
    States_operations.update ~selection p;
    (* do not apply hook if an hook calls [set_current] *)
    if not !apply_hook then begin
      apply_hook := true;
      if not on then Set_Current_Hook_User.apply old;
      Set_Current_Hook.apply old;
      apply_hook := false
    end

let set_current ?(on=false) ?(selection=State_selection.full) p =
  if not (equal p (current ())) then force_set_current on selection p

let set_current_as_last_created () =
  Option.iter (fun p -> set_current p) !last_created_by_copy_ref

let on ?selection p f x =
  let old_current = current () in
  if old_current == p then f x
  else
    let set p = set_current ~on:true ?selection p in
    let rec set_to_old old =
      if not !keep_current then
        try
          (* if someone asks for keeping [p] as current during the execution of
             [f], do not restore [old_current] at the end. *)
          set old
        with Invalid_argument _ ->
          (* the old current project has been remove: replace it by the previous
             one, if any *)
          if Q.length projects < 2 then
            print_warning
              "cannot restore project '%s'. Keep '%s' as default project."
              old_current.name (current ()).name
          else
            set_to_old (Q.nth 1 projects)
    in
    let go () = set p; f x in
    let finally () = set_to_old old_current in
    Fun.protect ~finally go

let on_from_pid ?selection pid f x =
  on ?selection (from_pid pid) f x

(* [set_current] must never be called internally. *)
module Hide_set_current = struct let set_current () = assert false end
open Hide_set_current
(* Silence warning on unused and unexported functions *)
let () = if false then set_current ()

exception Cannot_remove of string

module Before_remove = Hook.Build(struct type t = project end)
let register_before_remove_hook = Before_remove.extend

let remove ?(project=current()) () =
  print_feedback "removing project %S" project.name;
  if Q.length projects = 1 then raise (Cannot_remove project.name);
  Before_remove.apply project;
  States_operations.remove project;
  let old_current = current () in
  Q.remove project projects;
  if equal project old_current then begin
    (* we removed the current project. So there is a new current project
       and we have to update the local states according to it. *)
    let c = current () in
    States_operations.update c;
    Set_Current_Hook_User.apply c
  end;
  (* if we removed the last created_by_copy project, there is no last one *)
  Option.iter
    (fun p -> if equal project p then last_created_by_copy_ref := None)
    !last_created_by_copy_ref;
  (* clear all the states of other projects referring to the delete project *)
  Q.iter (States_operations.clear_some_projects (equal project)) projects

let remove_all () =
  print_feedback "removing all existing projects";
  try
    iter_on_projects Before_remove.apply;
    States_operations.clean ();
    Q.clear projects;
    last_created_by_copy_ref := None;
    Gc.full_major ()
  with NoProject ->
    ()

let copy ?(selection=State_selection.full) ?(src=current()) dst =
  guarded_feedback selection "copying project from %S to %S"
    src.name dst.name;
  States_operations.commit ~selection src;
  States_operations.copy ~selection src dst

module Before_Clear_Hook = Hook.Build(struct type t = project end)
let register_todo_before_clear = Before_Clear_Hook.extend

module After_Clear_Hook = Hook.Build(struct type t = project end)
let register_todo_after_clear = After_Clear_Hook.extend

let clear ?(selection=State_selection.full) ?(project=current()) () =
  guarded_feedback selection "clearing project %S" project.name;
  Before_Clear_Hook.apply project;
  States_operations.clear ~selection project;
  After_Clear_Hook.apply project

let clear_all () =
  Q.iter States_operations.clear projects;
  Gc.full_major ()

(* ************************************************************************** *)
(* Save/load *)
(* ************************************************************************** *)

exception IOError = Sys_error

module Before_load = Hook.Build (struct type t = project end)
let register_before_load_hook = Before_load.extend

module After_load = Hook.Build (struct type t = project end)
let register_after_load_hook = After_load.extend

module After_global_load = Hook.Make()
let register_after_global_load_hook = After_global_load.extend

let magic = 9 (* magic number *)

(* Cannot use Temp_files.file as it would create a cricular dependency *)
let temp_file ~prefix ~suffix =
  try
    let file = Filesystem.temp_file ~prefix ~suffix in
    Extlib.safe_at_exit (fun () -> Filesystem.remove_file file);
    file
  with Sys_error s ->
    failwith (Format.sprintf "cannot create temporary file: %s" s)

let save_projects ?(compress = !compress_saved_session) selection projects
    (filename : Filepath.t) =
  let open Filesystem.Operators in
  let$ cout = Filesystem.Compressed.with_open_out_exn ~compress filename in
  Channel.output_value cout (!get_seed ());
  Channel.output_value cout magic;
  Channel.output_value cout !Graph.Blocks.cpt_vertex;
  let states : (t * (string * State.state_on_disk) list) list =
    Q.fold
      (fun acc p ->
         (* project + serialized version of all its states *)
         (p, States_operations.serialize ~selection p) :: acc)
      []
      projects
  in
  (* projects are stored on disk from the current one to the last project.
     !last_created_by_copy_ref must be saved at the same time to share the
     project on disk *)
  Channel.output_value cout (List.rev states, !last_created_by_copy_ref)

let save ?compress ?(selection=State_selection.full) ?(project=current()) filename =
  guarded_feedback selection "saving project %S into file %a"
    project.name Filepath.pretty filename;
  save_projects ?compress selection (Q.singleton project) filename

let save_all ?compress ?(selection=State_selection.full) filename =
  guarded_feedback selection "saving the current session into file %a"
    Filepath.pretty filename;
  save_projects ?compress selection projects filename

module Descr = struct

  let project_under_copy_ref: project option ref = ref None
  (* The project which is currently copying. Only set by [create_by_copy].
     In this case, there is no possible dangling project pointers (projects
     at saving time and at loading time are the same).
     Furthermore, we have to merge pre-existing projects and loaded
     projects, except the project under copy. *)

  module Rehash =
    Hashtbl.Make
      (struct
        type t = project
        let hash p = Hashtbl.hash p.pid
        let equal x y =
          match !project_under_copy_ref with
          | Some p when p.pid <> x.pid && p.pid <> y.pid ->
            (* Merge projects on disk with pre-existing projects, except the
               project under copy; so don't use (==) in this context. *)
            x.pid = y.pid
          | None | Some _ ->
            (* In all other cases, don't merge.
               (==) ensures that there is no sharing between a pre-existing
               project and a project on disk. Great! *)
            x == y
      end)

  let rehash_cache : project Rehash.t = Rehash.create 7
  let existing_projects : unit Project_tbl.t = Project_tbl.create 7

  let rehash p =
    (*    Format.printf "REHASHING %S (%d;%x)@." p.unique_name p.pid (Extlib.address_of_value p);*)
    try
      Rehash.find rehash_cache p
    with Not_found ->
      let v = create p.name (* real name set when loading the key project *) in
      Rehash.add rehash_cache p v;
      v
  let () = rehash_ref := rehash

  let init project_under_copy =
    assert (Rehash.length rehash_cache = 0
            && Project_tbl.length existing_projects = 0);
    project_under_copy_ref := project_under_copy;
    Q.fold
      (fun acc p -> Project_tbl.add existing_projects p (); p :: acc)
      []
      projects

  let finalize loaded_states selection =
    (match !project_under_copy_ref with
     | None ->
       List.iter
         (fun ( (p, _)) ->
            States_operations.clear_some_projects
              ~selection
              (fun p -> not (Project_tbl.mem existing_projects p))
              p)
         loaded_states
     | Some _ ->
       ());
    Rehash.clear rehash_cache;
    Project_tbl.clear existing_projects

  let global_state name selection =
    let state_on_disk s =
      (*      Format.printf "State %S@." s;*)
      let descr =
        try State.get_descr (State.get s)
        with State.Unknown -> Structural_descr.p_unit (* dummy value *)
      in
      Descr.t_record
        [| descr;
           Structural_descr.p_bool;
           Structural_descr.p_bool;
           Structural_descr.p_string |]
        State.dummy_state_on_disk
    in
    let tbl_on_disk = Descr.dependent_pair Descr.t_string state_on_disk in
    let one_state =
      let unmarshal_states p =
        Descr.dynamic
          (fun () ->
             (* Local states must be up-to-date according to [p] when
                unmarshalling states of [p] *)
             force_set_current true selection p;
             Before_load.apply p;
             Descr.t_list tbl_on_disk)
      in
      Descr.dependent_pair datatype_descr unmarshal_states
    in
    let final_one_state =
      Descr.transform
        one_state
        (fun (p, s as c) ->
           (* if we provide an explicit name different of the current one,
              rename project [p] *)
           (match name with Some s when s <> p.name -> set_name p s | _ -> ());
           Project_tbl.add existing_projects p ();
           (* At this point, the local states are always up-to-date according
              to the current project, since we load first the old current
              project *)
           States_operations.unserialize ~selection p s;
           After_load.apply p;
           c)
    in
    Descr.t_pair
      (Descr.t_list final_one_state)
      (Descr.t_option D.datatype_descr) (* the last saved project *)

  let input_val = Descr.input_val

end

let load_projects ~project_under_copy selection ?name (filename : Filepath.t) =
  let open Filesystem.Operators in
  let ocamlgraph_counter, pre_existing_projects, loaded_states, last_created =
    try
      let$ cin = Filesystem.Compressed.with_open_in_exn filename in
      let check_magic format current =
        let old = Channel.input_value cin in
        if old <> current then begin
          let s =
            Format.asprintf
              "project saved with an incompatible version \
               (old: \"%a\", current: \"%a\")"
              (fun fmt -> Format.fprintf fmt format) old
              (fun fmt -> Format.fprintf fmt format) current
          in
          raise (IOError s)
        end
      in
      check_magic "%S" (!get_seed ());
      check_magic "magic number %d" magic;
      let ocamlgraph_counter = Channel.input_value cin in
      let pre_existing_projects = Descr.init project_under_copy in
      let loaded_states, last_created =
        Descr.input_val cin (Descr.global_state name selection)
      in
      ocamlgraph_counter, pre_existing_projects, loaded_states, last_created
    with
    | Failure s ->
      raise (IOError s)
    | End_of_file ->
      let msg =
        Format.asprintf "unexpected end of file while loading '%a'"
          Filepath.pretty filename
      in
      raise (IOError msg)
  in
  last_created_by_copy_ref := last_created;
  Descr.finalize loaded_states selection;
  Graph.Blocks.after_unserialization ocamlgraph_counter;
  (* [set_current] done when unmarshalling and hooks may reorder
     projects: rebuild it in the good order *)
  let last = current () in
  Q.clear projects;
  let loaded_projects =
    List.fold_right
      (fun (p, _) acc -> Q.add p projects; p :: acc) loaded_states []
  in
  List.iter (fun p -> Q.add p projects) pre_existing_projects;
  (* We have to restore all the local states if the last loaded project is
     not the good current one. The trick is to call [set_current] on [current
     ()], but we ensure that this operation **does** something (that is not
     the case by default) by putting [last] as current project
     temporarily. *)
  let true_current = current () in
  Q.add last projects;
  force_set_current true selection true_current;
  Q.remove last projects;
  After_global_load.apply ();
  loaded_projects

let load_with_copy
    ?project_under_copy ?(selection=State_selection.full) ?name filename =
  guarded_feedback selection "loading the project saved in file %a"
    Filepath.pretty filename;
  match load_projects ~project_under_copy selection ?name filename with
  | [ p ] -> p
  | [] | _ :: _ :: _ -> assert false

let load = load_with_copy ?project_under_copy:None

let load_all ?(selection=State_selection.full) filename =
  remove_all ();
  guarded_feedback selection "loading the session saved in file %a"
    Filepath.pretty filename;
  try
    ignore (load_projects ~project_under_copy:None selection filename)
  with IOError _ as e ->
    force_set_current false selection (create "default");
    raise e

module Create_by_copy_hook = Hook.Build(struct type t = project * project end)
let create_by_copy_hook f =
  Create_by_copy_hook.extend (fun (src, dst) -> f src dst)

let create_by_copy
    ?(selection=State_selection.full) ?(src=current()) ~last name =
  guarded_feedback selection "creating project %S by copying project %S"
    name (src.name);
  let filename = temp_file ~prefix:"frama_c_create_by_copy" ~suffix:".sav" in
  save ~compress:false ~selection ~project:src filename;
  try
    let prj = load_with_copy ~project_under_copy:src ~selection ~name filename in
    Filesystem.remove_file filename;
    if last then last_created_by_copy_ref := Some prj;
    Create_by_copy_hook.apply (src, prj);
    prj
  with e ->
    Filesystem.remove_file filename;
    raise e

(* Exporting Datatype for an easy external use *)
module Datatype = D
