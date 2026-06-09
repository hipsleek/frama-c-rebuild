(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* all the collection's internal states that depend on the AST.
   Forward dependency because of linking order (see special_hooks.ml). *)
let ast_dependencies: State.t list ref = ref []
let extend_ast_dependencies s = ast_dependencies := s :: !ast_dependencies

module D = Datatype (* hide after applying Parameter_state.Make *)
let empty_string = ""

let find_kf_by_name
  : (string -> kernel_function) ref
  = Extlib.mk_fun "Parameter_builder.find_kf_by_name"

let find_kf_def_by_name
  : (string -> kernel_function) ref
  = Extlib.mk_fun "Parameter_builder.find_kf_def_by_name"

let find_kf_decl_by_name
  : (string -> kernel_function) ref
  = Extlib.mk_fun "Parameter_builder.find_kf_decl_by_name"

let kf_category
  : (unit -> kernel_function Parameter_category.t) ref
  = Extlib.mk_fun "Parameter_builder.kf_category"

let kf_def_category
  : (unit -> kernel_function Parameter_category.t) ref
  = Extlib.mk_fun "Parameter_builder.kf_def_category"

let kf_decl_category
  : (unit -> kernel_function Parameter_category.t) ref
  = Extlib.mk_fun "Parameter_builder.kf_decl_category"

let fundec_category
  : (unit -> fundec Parameter_category.t) ref
  = Extlib.mk_fun "Parameter_builder.fundec_category"

let kf_string_category
  : (unit -> string Parameter_category.t) ref
  = Extlib.mk_fun "Parameter_builder.kf_string_category"

let force_ast_compute
  : (unit -> unit) ref
  = Extlib.mk_fun "Parameter_builder.force_ast_compute"

let get_definition kf =
  match kf.fundec with
  | Definition (fundec, _) -> Some fundec
  | Declaration _ -> None

let is_definition kf = Option.is_some (get_definition kf)
let is_declaration kf = Option.is_none (get_definition kf)

let get_c_ified_functions s =
  Cil_datatype.Kf.Set.elements (Parameter_customize.get_c_ified_functions s)

(* ************************************************************************* *)
(** {2 Specific functors} *)
(* ************************************************************************* *)

let is_parameter_reconfigurable stage =
  match !Parameter_customize.is_reconfigurable_ref, stage with
  | Some false, _
  | None, (Cmdline.Early | Cmdline.Extending | Cmdline.Extended
          | Cmdline.Exiting | Cmdline.Loading) ->
    false
  | Some true, _ | None, Cmdline.Configuring ->
    true

module Make
    (P: sig
       val shortname: string
       val parameters: (string, Typed_parameter.t list) Hashtbl.t
       module L: sig
         val abort: ('a,'b) Log.pretty_aborter
         val warning: 'a Log.pretty_printer
       end
     end) =
struct

  module Build = Parameter_state.Make(P)

  let parameters_ref : Typed_parameter.t list ref = ref []
  let parameters () = !parameters_ref

  let add_parameter group _stage param =
    parameters_ref := param :: !parameters_ref;
    let parameter_groups = P.parameters in
    try
      let group_name = Cmdline.Group.name group in
      let parameters = Hashtbl.find P.parameters group_name in
      Hashtbl.replace parameter_groups group_name (param :: parameters)
    with Not_found ->
      assert false

  (* ************************************************************************ *)
  (** {3 Bool} *)
  (* ************************************************************************ *)

  module Bool(X:sig include Parameter_sig.Input val default: bool end) = struct

    include Build
        (struct
          include Datatype.Bool
          include X
          let default () = default
          let functor_name = "Bool"
        end)

    let on = register_dynamic "on" D.unit D.unit (fun () -> set true)
    let off = register_dynamic "off" D.unit D.unit (fun () -> set false)

    let generic_add_option name help visible safe value =
      Cmdline.add_option
        name
        ~plugin:P.shortname
        ~group
        ~help
        ~visible
        ~safe
        ~ext_help:!Parameter_customize.optional_help_ref
        stage
        (Cmdline.Unit (fun () -> set value))

    let negate_name name =
      (* do we match '-shortname-'? (one dash before, one after) *)
      let len = String.length P.shortname + 2  in
      if String.length name <= len || P.shortname = empty_string then
        "-no" ^ name
      else
        let bef = Str.string_before name len in
        if bef = "-" ^ P.shortname ^ "-" then
          bef ^ "no-" ^ Str.string_after name len
        else
          "-no" ^ name

    let negative_option_name name =
      let s = !Parameter_customize.negative_option_name_ref in
      match s with
      | None -> negate_name name
      | Some s ->
        assert (s <> empty_string);
        s

    let default_message opp = Format.asprintf " (set by default%s)" opp

    let add_option opp name =
      let opp_msg name = "opposite option is " ^ negative_option_name name in
      let help =
        if X.default then
          if X.help = empty_string then empty_string
          else
            X.help ^
            if opp then default_message (", " ^ opp_msg name)
            else default_message empty_string
        else
        if opp then Format.asprintf "%s (%s)" X.help (opp_msg name)
        else X.help
      in
      generic_add_option name help is_visible is_safe true

    let add_negative_option name =
      let neg_name = negative_option_name name in
      let mk_help s =
        if X.default then s else s ^ default_message empty_string
      in
      (* Add help messages even for invisible negative options, since
         these messages are useful for option '-explain' *)
      let s = !Parameter_customize.negative_option_help_ref in
      let neg_visible, neg_help =
        if s = empty_string then
          !Parameter_customize.negative_option_name_ref <> None,
          (if not X.default then "(set by default) " else "") ^
          "opposite of option " ^ name ^ ", whose help message is:\n" ^ X.help
        else
          is_visible, mk_help s
      in
      generic_add_option neg_name neg_help neg_visible is_safe false;
      neg_name

    let negative_option_ref = ref None

    let parameter =
      let negative_option =
        match !Parameter_customize.negative_option_name_ref, stage with
        | Some "", _  | None, Cmdline.Exiting ->
          add_option false X.option_name;
          None
        | _ ->
          add_option true X.option_name;
          Some (add_negative_option X.option_name)
      in
      negative_option_ref := negative_option;
      let accessor =
        Typed_parameter.Bool
          ({ Typed_parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           negative_option)
      in
      let reconfigurable = is_parameter_reconfigurable stage in
      let p =
        Typed_parameter.create ~name ~help:X.help ~accessor:accessor
          ~visible:is_visible ~safe:is_safe ~reconfigurable ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      Parameter_customize.reset ();
      if is_dynamic then
        let plugin = empty_string in
        Dynamic.register
          ~plugin X.option_name Typed_parameter.ty p
      else p

    let add_aliases ?visible ?deprecated list =
      add_aliases ?visible ?deprecated list;
      match !negative_option_ref with
      | None -> ()
      | Some negative_option ->
        let negative_list = List.map negate_name list in
        let plugin = P.shortname in
        Cmdline.add_aliases
          negative_option ~plugin ~group ?visible ?deprecated stage negative_list

  end

  module False(X: Parameter_sig.Input) =
    Bool(struct include X let default = false end)

  module True(X: Parameter_sig.Input) =
    Bool(struct include X let default = true end)

  module Action(X: Parameter_sig.Input) = struct

    (* [JS 2011/09/29]
       The ugly hack seems to be required anymore neither for Value nor Wp.
       Maybe it is time to remove it? :-) *)

    (* do not save it but restore the "good" behavior when creating by copy *)

    let () = Parameter_customize.do_not_save ()
    (* [JS 2011/01/19] Not saving this kind of options is a quite bad hack with
       several drawbacks (see Frama-C commits 2011/01/19, message of JS around
       15 PM). I'm quite sure there is a better way to not display results too
       many times (e.g. by using the "isset" flag).  That is also the origin of
       bug #687 *)

    include False(X)

    let () =
      Project.create_by_copy_hook
        (fun src p ->
           Project.copy
             ~selection:(State_selection.singleton Is_set.self) ~src p;
           let selection = State_selection.singleton self in
           let opt = Project.on ~selection src get () in
           if opt then Project.on ~selection p set true)

  end

  (* ************************************************************************ *)
  (** {3 Integer} *)
  (* ************************************************************************ *)

  module Int(X: sig include Parameter_sig.Input_with_arg val default: int end) =
  struct

    include Build
        (struct
          include Datatype.Int
          include X
          let default () = default
          let functor_name = "Int"
        end)

    let incr =
      let incr () = set (succ (get ())) in
      register_dynamic "incr" D.unit D.unit incr

    let add_option name =
      Cmdline.add_option
        name
        ~argname:X.arg_name
        ~help:X.help
        ~visible:is_visible
        ~safe:is_safe
        ~ext_help:!Parameter_customize.optional_help_ref
        ~plugin:P.shortname
        ~group
        stage
        (Cmdline.Int set)

    let range = ref (min_int, max_int)
    let set_range ~min ~max = range := min, max
    let get_range () = !range

    let parameter =
      add_set_hook
        (fun _ n ->
           let min, max = !range in
           if n < min then
             P.L.abort "argument of %s must be at least %d." name min;
           if n > max then
             P.L.abort "argument of %s must be no more than %d." name max);
      let accessor =
        Typed_parameter.Int
          ({ Typed_parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           get_range)
      in
      let reconfigurable = is_parameter_reconfigurable stage in
      let p =
        Typed_parameter.create ~name ~help:X.help ~accessor
          ~visible:is_visible ~safe:is_safe ~reconfigurable ~is_set:is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name;
      Parameter_customize.reset ();
      if is_dynamic then
        let plugin = empty_string in
        Dynamic.register
          ~plugin X.option_name Typed_parameter.ty p
      else p

  end

  module Zero(X: Parameter_sig.Input_with_arg) =
    Int(struct include X let default = 0 end)

  (* ************************************************************************ *)
  (** {3 Float} *)
  (* ************************************************************************ *)

  module Float
      (X: sig include Parameter_sig.Input_with_arg val default: float end) =
  struct

    include Build
        (struct
          include Datatype.Float
          include X
          let default () = default
          let functor_name = "Float"
        end)

    let add_option name =
      Cmdline.add_option
        name
        ~argname:X.arg_name
        ~help:X.help
        ~visible:is_visible
        ~safe:is_safe
        ~ext_help:!Parameter_customize.optional_help_ref
        ~plugin:P.shortname
        ~group
        stage
        (Cmdline.Float set)

    let range = ref (min_float, max_float)
    let set_range ~min ~max = range := min, max
    let get_range () = !range

    let parameter =
      add_set_hook
        (fun _ f ->
           let min, max = !range in
           if f < min then
             P.L.abort "argument of %s must be at least %f." name min;
           if f > max then
             P.L.abort "argument of %s must be no more than %f." name max);
      let accessor =
        Typed_parameter.Float
          ({ Typed_parameter.get = get; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           get_range)
      in
      let reconfigurable = is_parameter_reconfigurable stage in
      let p =
        Typed_parameter.create ~name ~help:X.help ~accessor
          ~visible:is_visible ~safe:is_safe ~reconfigurable ~is_set:is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name;
      Parameter_customize.reset ();
      if is_dynamic then
        let plugin = empty_string in
        Dynamic.register
          ~plugin X.option_name Typed_parameter.ty p
      else p

  end

  (* ************************************************************************ *)
  (** {3 String} *)
  (* ************************************************************************ *)

  module String
      (X: sig include Parameter_sig.Input_with_arg val default: string end) =
  struct

    include Build
        (struct
          include Datatype.String
          include X
          let default () = default
          let functor_name = "String"
        end)

    let add_option name =
      let help =
        Format.asprintf "%s (preferably use %s=\"%s\")" X.help name X.arg_name
      in
      Cmdline.add_option
        name
        ~argname:X.arg_name
        ~help
        ~visible:is_visible
        ~safe:is_safe
        ~ext_help:!Parameter_customize.optional_help_ref
        ~plugin:P.shortname
        ~group
        stage
        (Cmdline.String set)

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    let get_function_name =
      let allow_fundecl = !Parameter_customize.argument_may_be_fundecl_ref in
      fun () ->
        let s = get () in
        (* Using a parameter that is in fact a function name only makes sense
           if we have an AST somewhere. *)
        !force_ast_compute();
        let possible_funcs = get_c_ified_functions s in
        let possible_funcs =
          if allow_fundecl then possible_funcs
          else List.filter is_definition possible_funcs
        in
        match possible_funcs with
        | [] ->
          P.L.abort
            "'%s' is not a %sfunction. \
             Please choose a valid function name for option %s"
            s (if allow_fundecl then "" else "defined ") name
        | [ kf ] -> (Cil_datatype.Kf.vi kf).vname
        | kf :: _ ->
          P.L.warning
            "ambiguous function name %s for option %s. \
             Choosing arbitrary function with corresponding name."
            s name;
          (Cil_datatype.Kf.vi kf).vname

    let get_plain_string = get

    let get =
      if !Parameter_customize.argument_is_function_name_ref then
        get_function_name
      else get

    let parameter =
      add_set_hook
        (fun _ s ->
           match !possible_values with
           | [] -> ()
           | v when List.mem s v -> ()
           | v ->
             P.L.abort
               "invalid input '%s' for option %s.@ Possible values are: %a"
               s
               name
               (Pretty_utils.pp_list ~sep:",@ " Format.pp_print_string) v);
      let accessor =
        Typed_parameter.String
          ({ Typed_parameter.get = get_plain_string; set = set;
             add_set_hook = add_set_hook; add_update_hook = add_update_hook },
           get_possible_values)
      in
      let reconfigurable = is_parameter_reconfigurable stage in
      let p =
        Typed_parameter.create ~name ~help:X.help ~accessor
          ~visible:is_visible ~safe:is_safe  ~reconfigurable ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name;
      Parameter_customize.reset ();
      if is_dynamic then
        let plugin = empty_string in
        Dynamic.register
          ~plugin X.option_name Typed_parameter.ty p
      else
        p

  end

  module Empty_string(X: Parameter_sig.Input_with_arg) =
    String(struct include X let default = empty_string end)

  (* ************************************************************************ *)
  (** {3 Filepath} *)
  (* ************************************************************************ *)

  (* Deprecated module, Use [Fclib.Filepath] instead. *)
  module Fc_Filepath = Filepath

  let normalize_filepath ~existence ~file_kind s =
    try
      Filepath.of_string ~existence s
    with
    | Filepath.No_file ->
      P.L.abort "%s%sfile '%s' does not exist"
        file_kind
        (if file_kind = "" then "" else " ")
        (Filepath.(to_string (of_string s)))
    | Filepath.File_exists ->
      P.L.abort "%s%sfile '%s' already exists"
        file_kind
        (if file_kind = "" then "" else " ")
        (Filepath.(to_string (of_string s)))

  module Filepath
      (X: sig
         include Parameter_sig.Input_with_arg
         val existence : Filepath.existence
         val file_kind: string
       end) =
  struct

    include Build
        (struct
          include Fclib.Filepath
          include X
          let default () = Filepath.empty
          let functor_name = "Filepath"
        end)

    let convert f oldstr newstr =
      let oldfp = Filepath.to_string oldstr in
      let newfp = Filepath.to_string newstr in
      f oldfp newfp

    let set_str s =
      set (normalize_filepath ~existence:X.existence ~file_kind:X.file_kind s)

    let add_option name =
      Cmdline.add_option
        name
        ~argname:X.arg_name
        ~help:X.help
        ~visible:is_visible
        ~safe:is_safe
        ~ext_help:!Parameter_customize.optional_help_ref
        ~plugin:P.shortname
        ~group
        stage
        (Cmdline.String set_str)

    let parameter_get fp = Filepath.to_string (get fp)
    let parameter_add_set_hook f = add_set_hook (convert f)
    let parameter_add_update_hook f = add_update_hook (convert f)

    let parameter =
      let accessor =
        Typed_parameter.String
          ({ Typed_parameter.get = parameter_get;
             set = set_str;
             add_set_hook = parameter_add_set_hook;
             add_update_hook = parameter_add_update_hook },
           fun () -> [])
      in
      let reconfigurable = is_parameter_reconfigurable stage in
      let p =
        Typed_parameter.create ~name ~help:X.help ~accessor
          ~visible:is_visible ~safe:is_safe ~reconfigurable ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      add_option X.option_name;
      Parameter_customize.reset ();
      if is_dynamic then
        let plugin = empty_string in
        Dynamic.register
          ~plugin X.option_name Typed_parameter.ty p
      else
        p

    let is_empty () = Filepath.is_empty (get ())
  end

  (* ************************************************************************ *)
  (** {3 Make_*_dir} *)
  (* ************************************************************************ *)

  (** Builds a Site_dir from an existing one. The corresponding directory always
      performs a full path resolution.

      @since 30.0-Zinc
  *)
  module Make_site_dir
      (Parent: Parameter_sig.Site_dir)
      (Info: sig val name: string end)
    : Parameter_sig.Site_dir
  =
  struct
    (* Note: it recursively rebuilds the path relative to the root directory,
       until we reach the root and resolve the path. *)
    let get_dir name = Parent.get_dir (Info.name ^ "/" ^ name)
    let get_file name = Parent.get_file (Info.name ^ "/" ^ name)
  end

  (** Builds a User_dir from an existing one.

      @since 30.0-Zinc
  *)
  module Make_user_dir
      (Parent: Parameter_sig.User_dir)
      (Info: sig val name: string end)
    : Parameter_sig.User_dir
  =
  struct
    let get_dir ?create_path name =
      Parent.get_dir ?create_path (Info.name ^ "/" ^ name)
    let get_file ?create_path name =
      Parent.get_file ?create_path (Info.name ^ "/" ^ name)
  end

  module Make_user_dir_opt
      (Parent: Parameter_sig.User_dir)
      (Info: sig
         include Parameter_sig.Input_with_arg
         val env: string option
         val dirname: string
       end): Parameter_sig.User_dir_opt
  =
  struct
    open Fclib.Filepath

    module Dir_name =
      Filepath
        (struct
          include Info
          let existence = Fclib.Filepath.Indifferent
          let file_kind = ""
        end)

    include Dir_name

    let get () =
      if Dir_name.is_set () then Dir_name.get ()
      else
        match Option.bind Sys.getenv_opt Info.env with
        | Some s when s <> "" -> of_string s
        | _ -> Parent.get_dir Info.dirname

    let is_set () =
      Dir_name.is_set () ||
      Option.fold ~none:false ~some:((<>) "")
        (Option.bind Sys.getenv_opt Info.env)

    let cached_value = ref None

    let () =
      (* In case of reset, we just want to forget everything.
         So, let's always forget, the next get will update the cache. *)
      Dir_name.add_set_hook (fun _ _ -> cached_value := None)

    let get () =
      match !cached_value with
      | Some value -> value
      | None ->
        let value = get () in
        cached_value := Some value ;
        value

    let expected ~dir path =
      if dir <> Filesystem.dir_exists path then
        P.L.abort "%a is expected to be a %s"
          pretty path (if dir then "directory" else "file")

    let mk_dir d =
      try Filesystem.make_dir d
      with Sys_error _ ->
        P.L.abort "cannot create %s directory `%a'" Info.dirname pretty d

    let get_dir ?(create_path=false) s =
      let dir = concat (get ()) s in
      if Filesystem.exists dir
      then (expected ~dir:true dir ; dir)
      else if create_path
      then (mk_dir dir ; dir)
      else dir

    let get_file ?create_path s =
      let base_dir = get_dir ?create_path @@ Filename.dirname s in
      (* No need to create anything here, as the path of sub-directories has
         been already created by [get_dir] for computing [base_dir]. *)
      let path = concat base_dir @@ Filename.basename s in
      if Filesystem.exists path then
        expected ~dir:false path ;
      path
  end

  (* ************************************************************************ *)
  (** {3 Custom parameters} *)
  (* ************************************************************************ *)

  exception Cannot_build of string

  let cannot_build msg = raise (Cannot_build msg)

  module Custom
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_with_arg
         val default: V.t
       end) =
  struct

    include Build
        (struct
          include V
          include X
          let default () = default
          let functor_name = "Value"
        end)

    let possible_values = ref []
    let set_possible_values s = possible_values := s
    let get_possible_values () = !possible_values

    (* Same interface as current module but with t replaced with string *)
    module String_parameter =
    struct
      let get () =
        V.to_string (get ())

      let set s =
        try set (V.of_string s)
        with Cannot_build msg ->
          P.L.abort "invalid input '%s' for option %s: %s" s name msg

      let add_set_hook f =
        let f' x1 x2 = f (V.to_string x1) (V.to_string x2) in
        add_set_hook f'

      let add_update_hook f =
        let f' x1 x2 = f (V.to_string x1) (V.to_string x2) in
        add_update_hook f'
    end

    let parameter =
      let accessor =
        let open String_parameter in
        Typed_parameter.String (
          { get; set; add_set_hook; add_update_hook },
          get_possible_values)
      in
      let reconfigurable = is_parameter_reconfigurable stage in
      let p =
        Typed_parameter.create ~name ~help:X.help ~accessor
          ~visible:is_visible ~safe:is_safe ~reconfigurable ~is_set
      in
      add_parameter !Parameter_customize.group_ref stage p;
      Cmdline.add_option option_name
        ~argname:X.arg_name
        ~help:X.help
        ~visible:is_visible
        ~safe:is_safe
        ~ext_help:!Parameter_customize.optional_help_ref
        ~plugin:P.shortname
        ~group
        stage
        (Cmdline.String String_parameter.set);
      Parameter_customize.reset ();
      if is_dynamic then
        Dynamic.register ~plugin:empty_string X.option_name Typed_parameter.ty p
      else
        p
  end

  module Enum
      (X: sig
         include Parameter_sig.Input
         type t
         val default: t
         val values: (t * string) list
       end) =
  struct

    let string_list ~sep = List.map snd X.values |> Stdlib.String.concat sep

    module Custom_value =
    struct
      include Datatype.Make_with_set_and_map (struct
          include Datatype.Serializable_undefined
          let name = "Parameter_builder.Enum(" ^ X.option_name ^ ")"
          type t = X.t
          let copy x = x
          let compare = Extlib.compare_basic
          let equal = Datatype.from_compare
          let reprs = List.map fst X.values
        end)

      let of_string_opt s =
        X.values
        |> List.map (fun (x, s) -> (s, x))
        |> List.assoc_opt s

      let of_string s =
        match of_string_opt s with
        | Some s -> s
        | None -> cannot_build ("possible values are " ^ string_list ~sep:",")

      let to_string x =
        try
          List.assoc x X.values
        with Not_found -> invalid_arg "not one of possible values"
    end

    module Custom_input =
    struct
      include X
      let arg_name = string_list ~sep:"|"
    end

    include Custom (Custom_value) (Custom_input)

    let () = set_possible_values (List.map snd X.values)
  end

  (* ************************************************************************ *)
  (** {3 Collections} *)
  (* ************************************************************************ *)

  type collect_action = Add | Remove

  module Make_collection
      (E: sig (* element in the collection *)
         type t
         val ty: t Type.t
         val to_string: t -> string
       end)
      (C: sig (* the collection, as a persistent datastructure *)
         type t
         val equal: t -> t -> bool
         val empty: t
         val is_empty: t -> bool
         val mem: E.t -> t -> bool
         val add: E.t -> t -> t
         val remove: E.t -> t -> t
         val iter: (E.t -> unit) -> t -> unit
         val fold: (E.t -> 'a -> 'a) -> t -> 'a -> 'a
         val of_string: string -> t (* may raise [Cannot_build] *)
         val reorder: t -> t
         (* Used after having parsed a comma-separated string representing
            parameters. The add actions are done in the reverse order with
            respect to the list. Can be [Fun.id] for unordered collections.
         *)
       end)
      (S: sig (* the collection, as a state *)
         include State_builder.S
         val memo: (unit -> C.t) -> C.t
         val clear: unit -> unit
       end)
      (X: (* standard option builder *) sig
         include Parameter_sig.Input_collection
         val default: C.t
       end)
  =
  struct

    type t = C.t
    type elt = E.t

    (* ********************************************************************** *)
    (* Categories *)
    (* ********************************************************************** *)

    type category = E.t Parameter_category.t

    (* the available custom categories for this option *)
    let available_categories
      : category Datatype.String.Hashtbl.t
      = Datatype.String.Hashtbl.create 7

    module Category = struct

      type elt = E.t
      type t = category

      let check_category_name s =
        if Datatype.String.Hashtbl.mem available_categories s
        || Datatype.String.equal s "all"
        || Datatype.String.equal s ""
        || Datatype.String.equal s "default"
        then
          P.L.abort "invalid category name '%s'" s

      let use categories =
        List.iter
          (fun c ->
             Parameter_category.use S.self c;
             Datatype.String.Hashtbl.add
               available_categories
               (Parameter_category.get_name c)
               c)
          categories

      let unsafe_add name states accessor =
        let c =
          Parameter_category.create name E.ty ~register:false states accessor
        in
        use [ c ];
        c

      let add name states get_values =
        check_category_name name;
        unsafe_add name states get_values

      let none =
        let o = object
          method fold: 'b. ('a -> 'b -> 'b) -> 'b -> 'b = (fun _ acc -> acc);
          method mem = fun _ -> false
        end in
        unsafe_add "" [] o

      let default_ref =
        let o = object
          method fold
            : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
            = fun f acc -> C.fold f X.default acc
          method mem x = C.mem x X.default
        end in
        let c = unsafe_add "default" [] o in
        Datatype.String.Hashtbl.add available_categories "default" c;
        ref c

      let default () = !default_ref
      let set_default c =
        Datatype.String.Hashtbl.replace available_categories "default" c;
        default_ref := c

      let all_ref: t ref = ref none
      let all () = !all_ref

      let on_enable_all c =
        (* interpretation may have change:
           reset the state to force the interpretation again *)
        S.clear ();
        all_ref := c

      let enable_all_as c =
        use [ c ];
        let all = Parameter_category.copy_and_rename "all" ~register:false c in
        Datatype.String.Hashtbl.add available_categories "all" all;
        on_enable_all all

      let enable_all states get_values =
        let all = unsafe_add "all" states get_values in
        on_enable_all all;
        all

    end

    (* ********************************************************************** *)
    (* Parsing *)
    (* ********************************************************************** *)

    let use_category = !Parameter_customize.use_category_ref

    (* parsing builds a list of triples  (action, is_category?, word) *)

    let add_action a l = (a, false, None) :: l

    let add_char c = function
      | [] -> assert false
      | (a, f, None) :: l ->
        (* first char of a new word *)
        let b = Buffer.create 7 in
        Buffer.add_char b c;
        (a, f, Some b) :: l
      | ((_, _, Some b) :: _) as l ->
        (* extend the current word *)
        Buffer.add_char b c;
        l

    let set_category_flag = function
      | (a, false, None) :: l -> (a, true, None) :: l
      | _ -> assert false

    type position =
      | Start (* the very beginning or after a comma *)
      | Word of (* action already specified, word is being read *)
          bool (* [true] iff beginning a category with '@' is allowed *)
      | Escaped (* the next char is escaped in the current word *)

    let parse_error msg =
      P.L.abort "@[@[incorrect argument for option %s@ (%s).@]"
        X.option_name msg

    (* return the list of tokens, in reverse order *)
    let parse s =
      let len = Stdlib.String.length s in
      let rec aux acc pos i s =
        if i = len then acc
        else
          let next = i + 1 in
          let read_char_in_word f_acc new_pos =
            (* assume 'Add' by default *)
            let acc = if pos = Start then add_action Add acc else acc in
            aux (f_acc acc) new_pos next s
          in
          let read_std_char_in_word c =
            read_char_in_word (add_char c) (Word false)
          in
          let read_backslash_and_char c =
            (* read '\\' and [c], without considering than '\\' is the escaping
               character *)
            read_char_in_word
              (fun acc -> add_char c (add_char '\\' acc)) (Word false)
          in
          match Stdlib.String.get s i, pos with
          | '+', Start when use_category ->
            aux (add_action Add acc) (Word true) next s
          | '-', Start when use_category ->
            aux (add_action Remove acc) (Word true) next s
          | '\\', (Start | Word _) -> read_char_in_word (fun x -> x) Escaped
          | ',', (Start | Word _) -> read_char_in_word (fun x -> x) Start
          | (' ' | '\t' | '\n' | '\r'), Start ->
            (* ignore whitespace at beginning of words (must be escaped) *)
            aux acc pos next s
          | '@', (Start | Word true) when use_category ->
            read_char_in_word set_category_flag (Word false)
          | c, (Start | Word _) -> read_std_char_in_word c
          | (',' | '\\' as c), Escaped -> read_std_char_in_word c
          | ('+' | '-' | '@' | ' ' | '\t' | '\n' | '\r' as c),
            Escaped when i = 1 ->
            if use_category then read_std_char_in_word c
            else read_backslash_and_char c
          | c, Escaped ->
            read_backslash_and_char c
      in
      aux [] Start 0 s

    (* ********************************************************************** *)
    (* The parameter itself, as a special string option *)
    (* ********************************************************************** *)

    let string_of_collection c =
      if C.is_empty c then ""
      else
        let b = Buffer.create 17 in
        let first = ref true in
        let to_escape =
          if use_category then
            [ '+'; '-'; '@'; ' '; '\t'; '\n'; '\r' ]
          else
            []
        in
        C.iter
          (fun e ->
             let raw = E.to_string e in
             if !first then begin if raw <> "" then first := false end
             else Buffer.add_char b ',';
             if raw <> "" && List.mem (Stdlib.String.get raw 0) to_escape then
               Buffer.add_char b '\\';
             Stdlib.String.iter
               (fun c ->
                  if c = ',' || c = '\\' then
                    Buffer.add_char b '\\';
                  Buffer.add_char b c)
               raw)
          c;
        Buffer.contents b

    (* a collection is a standard string option... *)
    module As_string = struct

      include String(struct
          include X
          let default = string_of_collection X.default
        end)

      let () = Parameter_state.collections :=
          State.Set.add self !Parameter_state.collections

      let get () =
        (* the default string may have a custom interpretation when the
           category @default has been customized:
           in that case, interpret "@default" to get it *)
        if use_category && is_default () then "@default" else get ()

    end

    (* ... which is cumulative, when set from the cmdline (but uniquely from
       this way since it is very counter-intuitive from the other ways
       (i.e. programmatically or the GUI). *)
    let () =
      Cmdline.replace_option_setting
        X.option_name
        ~plugin:P.shortname
        ~group:As_string.group
        (Cmdline.String
           (fun s ->
              let old = As_string.get () in
              As_string.set
                (if Datatype.String.equal old empty_string then s
                 else old ^ "," ^ s)))

    (* JS personal note: I'm still not fully convinced by this cumulative
       semantics. *)

    let () =
      (* the typed state depends on the string representation *)
      State_dependency_graph.add_codependencies
        ~onto:S.self
        (As_string.self :: X.dependencies)

    let check_possible_value elt =
      let a = Category.all () in
      if a != Category.none && not (Parameter_category.get_mem a elt) then
        parse_error ("impossible value " ^  E.to_string elt)

    (* may be costly: use it with parsimony *)
    let collection_of_string ~check s =
      (*        Format.printf "READING %s: %s@." X.option_name s;*)
      let tokens = parse s in
      (* remember: tokens are in reverse order. So handle the last one
         first. *)
      let unparsable, col =
        List.fold_right
          (fun (action, is_category, word) (unparsable, col) ->
             let extend = match action with
               | Add -> C.add
               | Remove -> C.remove
             in
             let word = match word with
               | None -> ""
               | Some b -> Buffer.contents b
             in
             (*              Format.printf "TOKEN %s@." word;*)
             if is_category then
               try
                 let c =
                   Datatype.String.Hashtbl.find available_categories word
                 in
                 if word = "all" then
                   match action with
                   | Add ->
                     unparsable, Parameter_category.get_fold c C.add C.empty
                   | Remove ->
                     (* -@all is always equal to the emptyset, even if there
                        were previous elements which are now impossible *)
                     None, C.empty
                 else
                   unparsable, Parameter_category.get_fold c extend col
               with Not_found ->
                 parse_error ("unknown category '" ^ word ^ "'")
             else (* not is_category *)
               try
                 let elts = C.of_string word in
                 unparsable, C.fold extend elts col
               with Cannot_build msg ->
                 Some msg, col)
          tokens
          (None, C.empty)
      in
      let col = C.reorder col in
      (* check each element after parsing all of them,
         since an element may be added, then removed later (e.g +h,-@all):
         that has to be accepted *)
      if check then begin
        Option.iter parse_error unparsable;
        C.iter check_possible_value col
      end;
      col

    (* ********************************************************************** *)
    (* Memoized access to the state *)
    (* ********************************************************************** *)

    let get () =
      let compute () =
        let s = As_string.get () in
        (*let c =*) collection_of_string ~check:true s (*in*)
        (*Format.printf "GET %s@." (As_string.get ());
          C.iter (fun s -> Format.printf "ELT %s@." (E.to_string s)) c;
          c*)
      in
      S.memo compute

    (* ********************************************************************** *)
    (* Implement the state, by overseded [As_string]:

       not the more efficient, but the simplest way that prevent to introduce
       subtle bugs *)
    (* ********************************************************************** *)

    let set c = As_string.set (string_of_collection c)
    let unsafe_set c = As_string.unsafe_set (string_of_collection c)

    let convert_and_apply f = fun old new_ ->
      f
        (collection_of_string ~check:false old)
        (collection_of_string ~check:true new_)

    let add_set_hook f = As_string.add_set_hook (convert_and_apply f)
    let add_update_hook f = As_string.add_update_hook (convert_and_apply f)

    (* ********************************************************************** *)
    (* Implement operations *)
    (* ********************************************************************** *)

    let add e = set (C.add e (get ()))
    let is_empty () = C.is_empty (get ())
    let iter f = C.iter f (get ())
    let fold f acc = C.fold f (get ()) acc

    (* ********************************************************************** *)
    (* Re-export values *)
    (* ********************************************************************** *)

    let name = As_string.name
    let option_name = As_string.option_name
    let is_default = As_string.is_default
    let is_set = As_string.is_set
    let clear = As_string.clear
    let print_help = As_string.print_help
    let add_aliases = As_string.add_aliases
    let self = As_string.self
    let parameter = As_string.parameter

    let equal = C.equal
    let is_computed = S.is_computed
    let mark_as_computed = S.mark_as_computed

    (* [Datatype] is fully abstract from outside anyway *)
    module Datatype = As_string.Datatype

    (* cannot be called anyway since [Datatype] is abstract *)
    let howto_marshal _marshal _unmarshal =
      P.L.abort "[how_to_marshal] cannot be implemented for %s." X.option_name

    (* same as above *)
    let add_hook_on_update _ =
      P.L.abort "[add_hook_on_update] cannot be implemented for %s."
        X.option_name

  end

  module Make_set
      (E: Parameter_sig.Value_datatype_with_collections)
      (X: sig
         include Parameter_sig.Input_collection
         val default: E.Set.t
       end):
  sig
    include Parameter_sig.Set with type elt = E.t and type t = E.Set.t
    module S: sig val self: State.t end (* typed state *)
  end =
  struct

    module C = struct
      include E.Set
      let reorder = Fun.id
      let of_string s = E.Set.of_list (E.of_string s)
    end

    module S = struct

      include State_builder.Option_ref
          (E.Set)
          (struct
            let name = X.option_name ^ " set"
            let dependencies = X.dependencies
          end)

      let memo f = memo f (* ignore the optional argument *)
    end

    include Make_collection(E)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let mem e = E.Set.mem e (get ())
    let exists f = E.Set.exists f (get ())
    let get_default () = X.default

  end

  module String_for_collection = struct
    include Datatype.String
    let of_string s = [ s ]
    let to_string = Datatype.identity
  end

  module String_set(X: Parameter_sig.Input_with_arg) =
    Make_set
      (String_for_collection)
      (struct
        include X
        let dependencies = []
        let default = Datatype.String.Set.empty
      end)

  module Filled_string_set
      (X: sig
         include Parameter_sig.Input_with_arg
         val default: Datatype.String.Set.t
       end) =
    Make_set
      (String_for_collection)
      (struct include X let dependencies = [] end)

  let check_function s must_exist require_fundecl list =
    let specific_msg = if require_fundecl then " declaration" else "" in
    if list = [] then
      if Cmdline.permissive && not (must_exist || require_fundecl)
      then P.L.warning "ignoring non-existing function%s '%s'." specific_msg s
      else cannot_build (Format.asprintf "no function%s '%s'" specific_msg s);
    list

  module Kernel_function_string(
      A: sig
        val accept_fundecl: bool
        val require_fundecl: bool
        val must_exist: bool
      end) =
  struct

    include Cil_datatype.Kf

    (* Cannot reuse any code to implement [to_string] without forward
       reference. Prefer small code duplication here. *)
    let to_string kf = match kf.fundec with
      | Definition(d, _) -> d.svar.vname
      | Declaration(_, vi, _, _) -> vi.vname

    let of_string s =
      let fcts = get_c_ified_functions s in
      let res =
        if A.require_fundecl then
          List.filter is_declaration fcts
        else if A.accept_fundecl then
          fcts
        else
          List.filter is_definition fcts
      in
      check_function s A.must_exist A.require_fundecl res

  end

  module Kernel_function_set(X: Parameter_sig.Input_with_arg) = struct

    module A = struct
      let accept_fundecl = !Parameter_customize.argument_may_be_fundecl_ref
      let require_fundecl = !Parameter_customize.argument_must_be_fundecl_ref
      let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref
    end

    include Make_set
        (Kernel_function_string(A))
        (struct
          include X
          let dependencies = []
          let default = Cil_datatype.Kf.Set.empty
        end)

    let () =
      if A.accept_fundecl then Category.enable_all_as (!kf_category ())
      else
      if A.require_fundecl then Category.enable_all_as (!kf_decl_category ())
      else Category.enable_all_as (!kf_def_category ())

    let () = extend_ast_dependencies S.self

  end

  module Fundec_set(X: Parameter_sig.Input_with_arg) = struct
    let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref
    let require_fundecl = !Parameter_customize.argument_must_be_fundecl_ref

    include Make_set
        (struct
          include Cil_datatype.Fundec

          let to_string f = f.svar.vname

          let of_string s =
            let fcts = get_c_ified_functions s in
            let defs = List.filter_map get_definition fcts in
            check_function s must_exist require_fundecl defs

        end)
        (struct
          include X
          let dependencies = []
          let default = Cil_datatype.Fundec.Set.empty
        end)

    let () = Category.enable_all_as (!fundec_category ())
    let () = extend_ast_dependencies S.self

  end

  module Make_list
      (E: sig
         include Parameter_sig.Value_datatype
         val of_string: string -> t list
       end)
      (X: sig include Parameter_sig.Input_collection val default: E.t list end):
    Parameter_sig.List with type elt = E.t and type t = E.t list =
  struct

    module C = struct
      include Datatype.List(E)
      let empty = []
      let is_empty l = l == []
      let add (x:E.t) l = x :: l
      let mem = List.mem
      let remove x l = List.filter (fun y -> not (E.equal x y)) l
      let iter = List.iter
      let fold f l acc = List.fold_left (fun acc x -> f x acc) acc l
      let reorder = List.rev
      let of_string = E.of_string
    end

    module S = struct

      include State_builder.Option_ref
          (C)
          (struct
            let name = X.option_name ^ " list"
            let dependencies = X.dependencies
          end)

      let memo f = memo f (* ignore the optional argument *)

    end

    include Make_collection(E)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let append_before l = set (l @ get ())
    let append_after l = set (get () @ l)

    let get_default () = X.default

  end

  module String_list(X: Parameter_sig.Input_with_arg) =
    Make_list
      (String_for_collection)
      (struct
        include X
        let dependencies = []
        let default = []
      end)

  module Filepath_list
      (X: sig
         include Parameter_sig.Input_with_arg
         val existence: Fclib.Filepath.existence
         val file_kind: string
       end) =
    Make_list
      (struct
        include Fclib.Filepath
        let to_string s = Fclib.Filepath.to_string_abs s

        let of_string s =
          [ normalize_filepath ~existence:X.existence ~file_kind:X.file_kind s ]
      end)
      (struct
        include X
        let dependencies = []
        let default = []
      end)

  module Value_int = struct
    include Datatype.Int

    let of_string s =
      try int_of_string s
      with Failure _ -> raise (Cannot_build ("'" ^ s ^ "' is not an integer"))

    let to_string = string_of_int
  end

  module Value_string = struct
    include Datatype.String
    let of_string s = s
    let to_string s = s
  end

  module Make_map
      (K: Parameter_sig.Value_datatype_with_collections)
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_collection
         val default: V.t K.Map.t
       end) =
  struct

    type key = K.t
    type value = V.t

    let of_val k v =
      try V.of_string v
      with Cannot_build s ->
        cannot_build (Format.asprintf "@[value bound to '%s':@ %s@]" k s)

    module Pair = struct
      include Datatype.Pair(K)(V)

      let to_string (key, v) =
        Format.asprintf "%s:%s" (K.to_string key) (V.to_string v)
    end

    module C = struct
      type t = V.t K.Map.t
      let equal = K.Map.equal V.equal
      let empty = K.Map.empty
      let is_empty = K.Map.is_empty
      let add (k, v) m =
        try
          let old = K.Map.find k m in
          if V.equal old v then
            m
          else begin
            P.L.warning "@[option %s:@ '%a' previously bound to '%a';@ \
                         now bound to '%a'.@]"
              X.option_name K.pretty k V.pretty old V.pretty v;
            K.Map.add k v m
          end
        with Not_found ->
          K.Map.add k v m

      let mem (k, _v) m = K.Map.mem k m
      let remove (k, _v) m = K.Map.remove k m
      let iter f m = K.Map.iter (fun k v -> f (k, v)) m
      let fold f m acc = K.Map.fold (fun k v -> f (k, v)) m acc
      let reorder = Fun.id

      let of_string =
        let r = Str.regexp "\\([^:]\\|^\\):\\([^:]\\|$\\)" in
        (* delimiter is no more than 3 characters long, the first belonging to
           the element before it, the third belonging to the element after it.
           Treats :: as part of a word to be able to handle C++ function names
           in a non too awkward manner.
        *)
        let split_delim d = (* handle different possible length of the delimiter *)
          let rbis = Str.regexp ":" in
          match Str.bounded_full_split rbis d 2 with
          | [ Str.Delim _] -> (empty_string, empty_string)
          | [ Str.Delim _; Str.Text t2 ] -> (empty_string, t2)
          | [ Str.Text t1; Str.Delim _; ] -> (t1, empty_string)
          | [ Str.Text t1; Str.Delim _; Str.Text t2 ] -> (t1, t2)
          | _ -> (* impossible case *)
            raise (Cannot_build ("delimiter="^d))
        in
        fun s ->
          let (keys, value) =
            let get_pairing k v_opt =
              K.Set.of_list (K.of_string k), of_val k v_opt
            in
            match Str.bounded_full_split r s 2 with
            | [] -> cannot_build ("cannot interpret '" ^ s ^ "'")
            | [ Str.Text k ] -> cannot_build ("no value bound to '" ^ k ^ "'")
            | [ Str.Delim d ] ->
              let (f,s) = split_delim d in
              get_pairing f s
            | [ Str.Delim d; Str.Text t ] ->
              let (f,s) = split_delim d in
              get_pairing f (s ^ t)
            | [ Str.Text t1; Str.Delim d; Str.Text t2 ] ->
              let (f,s) = split_delim d in
              get_pairing (t1 ^ f) (s ^ t2)
            | [ Str.Text t; Str.Delim d] ->
              let (f,s) = split_delim d in
              get_pairing (t ^ f) s
            | _ -> (* by definition of [Str.bounded_full_split]: *)
              assert false
          in
          K.Set.fold (fun key map -> add (key, value) map) keys K.Map.empty
    end

    module S = struct

      include State_builder.Option_ref
          (K.Map.Make(V))
          (struct
            let name = X.option_name ^ " map"
            let dependencies = X.dependencies
          end)

      let memo f = memo f (* ignore the optional argument *)

    end

    include Make_collection(Pair)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let find k = K.Map.find k (get ())
    let mem k = K.Map.mem k (get ())
    let get_default () = X.default

  end

  module String_map
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_with_arg
         val default: V.t Datatype.String.Map.t
       end) =
    Make_map
      (String_for_collection)
      (V)
      (struct include X let dependencies = [] end)

  module Filepath_map
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_with_arg
         val existence: Fclib.Filepath.existence
         val default: V.t Fclib.Filepath.Map.t
       end) =
    Make_map
      (struct
        include Fclib.Filepath
        let of_string s =
          try
            [ Fclib.Filepath.of_string ~existence:X.existence s ]
          with
          | Fclib.Filepath.No_file ->
            P.L.abort "file '%s' not found" s
          | Fclib.Filepath.File_exists ->
            P.L.abort "file '%s' already exists" s
        let to_string p = Fclib.Filepath.to_string_rel p
        let pretty = Fclib.Filepath.pretty_rel
      end)
      (V)
      (struct include X let dependencies = [] end)

  module Kernel_function_map
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_with_arg
         val default: V.t Cil_datatype.Kf.Map.t
       end) =
  struct

    module A = struct
      let accept_fundecl = !Parameter_customize.argument_may_be_fundecl_ref
      let require_fundecl = !Parameter_customize.argument_must_be_fundecl_ref
      let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref
    end

    include Make_map
        (Kernel_function_string(A))
        (V)
        (struct include X let dependencies = [] end)

    let () = extend_ast_dependencies S.self

  end

  module Make_multiple_map
      (K: Parameter_sig.Value_datatype_with_collections)
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_collection
         val default: V.t list K.Map.t
       end) =
  struct

    type key = K.t
    type value = V.t

    let of_val k v =
      try V.of_string v
      with Cannot_build s ->
        cannot_build (Format.asprintf "@[value bound to '%s':@ %s@]" k s)

    module Pair = struct
      include Datatype.Pair(K)(Datatype.List(V))

      let to_string (key, l) =
        Format.asprintf "%s%t"
          (K.to_string key)
          (fun fmt ->
             let rec pp_custom_list = function
               | [] -> ()
               | v :: l ->
                 Format.fprintf fmt ":%s" (V.to_string v);
                 pp_custom_list l
             in
             pp_custom_list l)
    end

    module C = struct
      type t = V.t list K.Map.t
      let equal = K.Map.equal (List.for_all2 V.equal)
      let empty = K.Map.empty
      let is_empty = K.Map.is_empty
      let add (k, l) m =
        try
          let l' = K.Map.find k m in
          K.Map.add k (l @ l') m
        with Not_found ->
          K.Map.add k l m
      let mem (k, _) m = K.Map.mem k m
      let remove (k, _) m = K.Map.remove k m
      let iter f m = K.Map.iter (fun k l -> f (k, l)) m
      let fold f m acc = K.Map.fold (fun k v -> f (k, v)) m acc
      let reorder = Fun.id

      let of_string =
        let r = Str.regexp "[^:]:[^:]" in
        let split_delim d =
          (Stdlib.String.sub d 0 1, Stdlib.String.sub d 2 1)
        in
        let rec parse_values k acc s = function
          | [] -> List.rev (of_val k s :: acc)
          | [Str.Text t] -> List.rev (of_val k (s ^ t) :: acc)
          | Str.Text t :: Str.Delim d :: l ->
            let (suf, pre) = split_delim d in
            let v = of_val k (s ^ t ^ suf) in
            parse_values k (v :: acc) pre l
          | Str.Delim d :: l ->
            let (suf,pre) = split_delim d in
            let v = of_val k (s ^ suf) in
            parse_values k (v :: acc) pre l
          | Str.Text _ :: Str.Text _ :: _ ->
            (* By construction, there must be a Delim between two consecutive
               Text in the value returned by full_split *)
            assert false
        in
        let get_pairing k v l =
          K.Set.of_list (K.of_string k), parse_values k [] v l
        in
        fun s ->
          let (keys, values) =
            match Str.full_split r s with
            | [] -> cannot_build ("cannot interpret '" ^ s ^ "'")
            | [Str.Text k] -> cannot_build ("no value bound to '" ^ k ^ "'")
            | Str.Delim d :: l ->
              let (f,s) = split_delim d in
              get_pairing f s l
            | Str.Text t :: Str.Delim d :: l ->
              let (f,s) = split_delim d in
              get_pairing (t ^ f) s l
            | Str.Text _ :: Str.Text _ :: _ -> (* see above *) assert false
          in
          K.Set.fold (fun key map -> K.Map.add key values map) keys K.Map.empty
    end

    module S = struct

      include State_builder.Option_ref
          (K.Map.Make(Datatype.List(V)))
          (struct
            let name = X.option_name ^ " map"
            let dependencies = X.dependencies
          end)

      let memo f = memo f (* ignore the optional argument *)

    end

    include Make_collection(Pair)(C)(S)(X)

    (* ********************************************************************** *)
    (* Accessors *)
    (* ********************************************************************** *)

    let find k = K.Map.find k (get ())
    let mem k = K.Map.mem k (get ())
    let get_default () = X.default

  end

  module String_multiple_map
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_with_arg
         val default: V.t list Datatype.String.Map.t
       end) =
    Make_multiple_map
      (String_for_collection)
      (V)
      (struct include X let dependencies = [] end)

  module Kernel_function_multiple_map
      (V: Parameter_sig.Value_datatype)
      (X: sig
         include Parameter_sig.Input_with_arg
         val default: V.t list Cil_datatype.Kf.Map.t
       end) =
  struct

    module A = struct
      let accept_fundecl = !Parameter_customize.argument_may_be_fundecl_ref
      let require_fundecl = !Parameter_customize.argument_must_be_fundecl_ref
      let must_exist = !Parameter_customize.argument_must_be_existing_fun_ref
    end

    include Make_multiple_map
        (Kernel_function_string(A))
        (V)
        (struct include X let dependencies = [] end)

    let () = extend_ast_dependencies S.self

  end

end
