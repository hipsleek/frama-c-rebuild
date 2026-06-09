(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* ************************************************************************* *)
(** {2 Handling group of parameters} *)
(* ************************************************************************* *)

let selection : (State.t * bool) list ref = ref []
let no_reset_selection: (State.t * bool) list ref = ref []
let collections: State.Set.t ref = ref State.Set.empty

let get_selection_gen ?(is_set=true) selection =
  let l =
    if is_set then
      List.rev (List.rev_map fst selection)
    else
      List.fold_left
        (fun acc (x, b) -> if b then acc else x :: acc)
        []
        selection
  in
  State_selection.of_list l

let get_selection ?is_set () = get_selection_gen ?is_set !selection

let get_selection_context ?is_set () =
  let has_dependencies s =
    (* each collection whose state is a string has at least 1 dependency
       which is its typed state. *)
    let degree = if State.Set.mem s !collections then 1 else 0 in
    State_dependency_graph.G.out_degree State_dependency_graph.graph s > degree
  in
  (* automatically select all options that have some dependencies:
     they have an impact on some analysis. *)
  let states =
    State_selection.fold
      (fun s acc -> if has_dependencies s then s :: acc else acc)
      (get_selection ?is_set ())
      []
  in
  State_selection.of_list states

let get_reset_selection ?is_set () =
  let all = get_selection ?is_set () in
  let no_reset = get_selection_gen ?is_set !no_reset_selection in
  State_selection.diff all no_reset

let extend_selection is_set s = selection := (s, is_set) :: !selection

let extend_no_reset_selection is_set s =
  no_reset_selection := (s,is_set) :: !no_reset_selection

(* ************************************************************************* *)
(** {2 Generic implementation} *)
(* ************************************************************************* *)

module Make
    (P: sig val shortname: string end)
    (X: sig
       include Datatype.S
       val default: unit -> t
       val option_name: string
       val functor_name: string
     end) =
struct

  let is_dynamic = true
  let projectify = !Parameter_customize.projectify_ref
  let reset_on_copy = !Parameter_customize.reset_on_copy_ref
  let must_save = !Parameter_customize.must_save_ref
  let is_visible = !Parameter_customize.is_visible_ref
  let is_safe = !Parameter_customize.is_safe_ref
  let group = !Parameter_customize.group_ref
  let stage = !Parameter_customize.cmdline_stage_ref

  let () = match !Parameter_customize.cmdline_stage_ref with
    | Cmdline.Early | Cmdline.Extending | Cmdline.Extended
    | Cmdline.Exiting | Cmdline.Loading ->
      Parameter_customize.do_not_projectify ()
    | Cmdline.Configuring ->
      ()

  (* quite an inlining of [State_builder.Ref]; but handle [projectify_ref] *)
  module Option_state_builder
      (X:sig
         include Datatype.S
         val unique_name: string
         val pretty_name: string
         val default: unit -> t
       end) =
  struct

    type data = X.t

    let create () = ref (X.default ())
    let state = ref (create ())

    include State_builder.Register
        (struct
          include Datatype.Ref(X)
          let datatype_descr =
            if must_save then datatype_descr else Descr.unmarshable
        end)
        (struct
          type t = data ref
          let get () = !state
          let create = if projectify then create else (* do an alias *) get
          let clear x = if projectify then x := X.default ()
          let set x =
            if projectify then state := x (* else there is already an alias *)
          let clear_some_projects = Datatype.never_any_project
        end)
        (struct
          let name = X.pretty_name
          let unique_name = X.unique_name
          let dependencies = []
        end)

    let set v = !state := v
    let get () = !(!state)

  end

  module Internal_state =
    Option_state_builder
      (struct
        include X
        (* Hack for the parsing of the command line: C files are recognized
           as an argument of the option with name '""' (empty string). *)
        let option_name =
          if X.option_name = "" then "Input C files" else X.option_name
        let unique_name = option_name
        let pretty_name = option_name
      end)

  module D = Datatype
  include Internal_state

  type t = Internal_state.data
  let () =
    extend_selection false self;
    if not reset_on_copy then extend_no_reset_selection false self

  let is_default () = X.equal (X.default ()) (Internal_state.get ())

  let get_default () = X.default ()

  module Is_set =
    Option_state_builder
      (struct
        include D.Bool
        let pretty_name = X.option_name ^ " is set"
        let unique_name = pretty_name
        let default () = false
      end)
  let () =
    State_dependency_graph.add_dependencies ~from:Is_set.self [ self ];
    extend_selection true Is_set.self;
    if not reset_on_copy then extend_no_reset_selection true self

  module Set_hook = Hook.Build(struct type t = X.t * X.t end)
  let add_set_hook f = Set_hook.extend (fun (old, x) -> f old x)

  let add_update_hook f =
    add_set_hook f;
    add_hook_on_update
      (fun x ->
         let old = get () in
         let new_ = !x in
         if not (X.equal old new_) then f old new_)

  (* like set, but do not clear the dependencies *)
  let unsafe_set x =
    Is_set.set true;
    let old = Internal_state.get () in
    if not (X.equal x old) then begin
      Internal_state.set x;
      Set_hook.apply (old, x)
    end

  let force_set x =
    let old = Internal_state.get () in
    (* First clear the dependency and next apply the hooks since these hooks may
       set some states in the dependencies. Even do so for unprojectifed options
       since they actually use the project system (in a fake way, see module
       {!Options_state_builder}) and could have some dependencies that must be
       kept consistent. *)
    let selection =
      State_selection.diff
        (State_selection.with_dependencies self)
        (State_selection.singleton Is_set.self)
    in
    (try Project.clear ~selection ()
     with Project.NoProject ->
       (* unprojectified early options can be set even before creating the
          initial project. Do nothing for these ones: consistency will be
          automatically ensured when creating the initial project. *)
       assert (
         Kernel_log.verify (not projectify)
           "projectified option %s cannot be set before creating the initial \
            project" X.option_name
       ));
    Internal_state.set x;
    Set_hook.apply (old, x)

  let set x =
    Is_set.set true;
    if not (X.equal x (Internal_state.get ())) then force_set x

  let unguarded_clear () =
    force_set (X.default ());
    Is_set.set false

  let clear () =
    if Is_set.get () || not (is_default ()) then unguarded_clear ()

  let equal = X.equal

  let register_dynamic name ty1 ty2 f =
    if is_dynamic then
      let ty = D.func ty1 ty2 in
      Dynamic.register
        ~plugin:""
        (Dynamic.Parameter.get_name X.functor_name name X.option_name)
        ty
        f
    else
      f

  let get, set, unsafe_set, clear, is_set, is_default =
    register_dynamic "get" D.unit X.ty Internal_state.get,
    register_dynamic "set" X.ty D.unit set,
    register_dynamic "unsafe_set" X.ty D.unit unsafe_set,
    register_dynamic "clear" D.unit D.unit clear,
    register_dynamic "is_set" D.unit D.bool Is_set.get,
    register_dynamic "is_default" D.unit D.bool is_default

  let option_name = X.option_name

  let add_aliases ?visible ?deprecated =
    Cmdline.add_aliases
      option_name ~plugin:P.shortname ~group stage ?visible ?deprecated

  let print_help fmt =
    Cmdline.print_option_help fmt ~plugin:P.shortname ~group option_name

end
