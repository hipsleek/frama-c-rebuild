(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Debugging                                                          --- *)
(* -------------------------------------------------------------------------- *)

let error ~name ~message ~details =
  Kernel_log.error "cannot load plug-in '%s': %s%t" name message
    (fun fmt ->
       if details <> "" then
         Format.fprintf fmt "@\nDetails: %s" details)

(* -------------------------------------------------------------------------- *)
(* --- Dynlink Common Interface & Dynamic Library                         --- *)
(* -------------------------------------------------------------------------- *)

module Tbl = Type.String_tbl(struct type 'a t = 'a end)

let dynlib_init = ref false
let dynlib_init () =
  if not !dynlib_init then
    begin
      dynlib_init := true ;
      Dynlink.allow_unsafe_modules true ;
    end

exception Incompatible_type = Tbl.Incompatible_type
exception Unbound_value = Tbl.Unbound_value

let dynlib_error name = function
  | Dynlink.Error e ->
    error ~name ~message:"cannot load module" ~details:(Dynlink.error_message e) ;
  | Sys_error _ as e ->
    error ~name ~message:"system error" ~details:(Printexc.to_string e)
  (* the three next errors may be raised in case of incompatibilities with
     another plug-in *)
  | Incompatible_type s ->
    error ~name ~message:"code incompatibility" ~details:s
  | Unbound_value s ->
    error ~name ~message:"code incompatibility" ~details:("unbound value " ^ s)
  | Type.No_abstract_type s ->
    error ~name ~message:"code incompatibility" ~details:("unbound abstract type " ^ s)
  | Log.AbortError _ | Log.AbortFatal _ | Log.FeatureRequest _ as e ->
    raise e
  | e ->
    error ~name ~message:("unexpected exception: " ^ Printexc.to_string e)
      ~details:(Printexc.get_backtrace ())

let dynlib_module name file =
  Kernel_log.feedback ~dkey:Kernel_log.dkey_dynlink
    "Loading module '%s' from '%s'." name file ;
  try
    dynlib_init () ;
    Dynlink.loadfile file ;
  with error ->
    Cmdline.add_loading_failures name;
    dynlib_error name error

(* -------------------------------------------------------------------------- *)
(* --- Utilities                                                          --- *)
(* -------------------------------------------------------------------------- *)

let split_ext p =
  try
    let k = String.rindex p '.' in
    let d = try String.rindex p '/' with Not_found -> 0 in
    (* check for '.' belonging to directory or not *)
    if d <= k then
      let n = String.length p in
      String.sub p 0 k , String.sub p k (n-k)
    else p , ""
  with Not_found -> p , ""

let is_package =
  let pkg = Str.regexp "[a-z-_][a-z-_0-9.]*$" in
  fun name -> Str.string_match pkg name 0

let is_file base ext =
  let file = base ^ ext in
  if (Filesystem.exists (Filepath.of_string file)) then Some file else None

let is_object base =
  if Dynlink.is_native then is_file base ".cmxs" else
    match is_file base ".cma" with
    | Some _ as file -> file
    | None -> is_file base ".cmo"

(* -------------------------------------------------------------------------- *)
(* --- Package Loading                                                    --- *)
(* -------------------------------------------------------------------------- *)

let load_packages pkgs =
  List.iter Dune_site_plugins.V1.load pkgs

(* -------------------------------------------------------------------------- *)
(* --- Command-Line Entry Points                                          --- *)
(* -------------------------------------------------------------------------- *)

let load_plugin_path () =
  System_config.Plugins.load_all ()

let load_plugin m =
  try System_config.Plugins.load m
  (* Ok, this is ugly, but Dune Site does not give any way to catch this ...
     Note that we abort with a user error.
  *)
  with e -> Kernel_log.abort "Failed to load plug-in %S@.Exception: %s" m
              (Printexc.to_string e)

let load_module m =
  let base,ext = split_ext m in
  match ext with
  | ".ml" ->
    Kernel_log.error "Script loading has been removed; see section \"Loading Single OCaml Files as Plug-ins\" in the Frama-C user manual for an alternative."
  | _ ->
    begin
      (* load object or compile script or find package *)
      match is_object base with
      | Some file -> dynlib_module (Filename.basename base) file
      | None ->
        if is_package m && Dune_site_plugins.V1.available m then load_packages [m]
        else
          load_plugin m
    end

let () = Printexc.register_printer (function
    | Dynlink.Error err -> Some (Dynlink.error_message err)
    | _ -> None
  )

let () = Dynlink.allow_unsafe_modules true


(* ************************************************************************* *)
(** {2 Registering and accessing dynamic values} *)
(* ************************************************************************* *)

let dynamic_values = Tbl.create 97
let comments_fordoc = Hashtbl.create 97

let register ?(comment="") ~plugin name ty f =
  Kernel_log.debug ~level:5 "registering dynamic function %s" name;
  let key = plugin ^ "." ^ name in
  Tbl.add dynamic_values key ty f;
  if comment <> "" then Hashtbl.add comments_fordoc key comment ;
  f

let get ~plugin name ty =
  Tbl.find dynamic_values (plugin ^ "." ^ name) ty

let iter f = Tbl.iter f dynamic_values
let iter_comment f = Hashtbl.iter f comments_fordoc

(* ************************************************************************* *)
(** {2 Specialised interface for parameters} *)
(* ************************************************************************* *)

module Parameter = struct

  module type Common = sig
    type t
    val get: string -> unit -> t
    val set: string -> t  -> unit
    val clear: string -> unit -> unit
    val is_set: string -> unit -> bool
    val is_default: string  -> unit -> bool
  end

  let get_name functor_name fct_name option_name =
    Format.sprintf "Dynamic.Parameter.%s.%s %S"
      functor_name fct_name option_name

  let get_parameter option_name =
    get ~plugin:"" option_name Typed_parameter.ty

  let get_state option_name =
    let prm = get ~plugin:"" option_name Typed_parameter.ty in
    State.get prm.Typed_parameter.name

  let apply modname name s ty1 ty2 =
    get ~plugin:""  (get_name modname s name) (Datatype.func ty1 ty2)

  module Common(X: sig type t val modname:string val ty: t Type.t end ) = struct
    type t = X.t
    let ty = X.ty
    let get name = apply X.modname name "get" Datatype.unit ty
    let set name = apply X.modname name "set" ty Datatype.unit
    let clear name = apply X.modname name "clear" Datatype.unit Datatype.unit
    let is_set name = apply X.modname name "is_set" Datatype.unit Datatype.bool
    let is_default name =
      apply X.modname name "is_default" Datatype.unit Datatype.bool
  end

  module Bool = struct
    include Common
        (struct type t = bool let ty = Datatype.bool let modname = "Bool"end )
    let on name = apply "Bool" name "on" Datatype.unit Datatype.unit
    let off name = apply "Bool" name "off" Datatype.unit Datatype.unit
  end

  module Int = struct
    include Common
        (struct type t = int let ty = Datatype.int let modname = "Int" end )
    let incr name = apply "Int" name "incr" Datatype.unit Datatype.unit
  end

  module String =
    Common
      (struct
        type t = string
        let ty = Datatype.string
        let modname = "String"
      end)

  module Filepath =
    Common
      (struct
        type t = Filepath.t
        let ty = Filepath.ty
        let modname = "Filepath"
      end)

  module StringSet = struct
    include Common
        (struct include Datatype.String.Set let modname = "StringSet" end)
    let add name = apply "StringSet" name "add" Datatype.string Datatype.unit
    let remove name =
      apply "StringSet" name "remove" Datatype.string Datatype.unit
    let is_empty name =
      apply "StringSet" name "is_empty" Datatype.unit Datatype.bool
    let iter name =
      apply "StringSet" name "iter"
        (Datatype.func Datatype.string Datatype.unit) Datatype.unit
  end

  module StringList = struct
    include Common
        (struct
          include Datatype.List(Datatype.String)
          let modname = "StringList"
        end)
    let add name = apply "StringList" name "add" Datatype.string Datatype.unit
    let append_before name = apply "StringList" name "append_before"
        (Datatype.list Datatype.string) Datatype.unit
    let append_after name = apply "StringList" name "append_after"
        (Datatype.list Datatype.string) Datatype.unit
    let remove name =
      apply "StringList" name "remove" Datatype.string Datatype.unit
    let is_empty name =
      apply "StringList" name "is_empty" Datatype.unit Datatype.bool
    let iter name =
      apply "StringList" name "iter"
        (Datatype.func Datatype.string Datatype.unit) Datatype.unit
  end

end
