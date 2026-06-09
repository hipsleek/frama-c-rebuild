(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Logic_typing
open Logic_ptree

type extension_preprocessor =
  lexpr list -> lexpr list
type extension_typer =
  typing_context -> location -> lexpr list -> acsl_extension_kind
type extension_preprocessor_block =
  string * extended_decl list -> string * extended_decl list
type extension_typer_block =
  typing_context -> location -> string * extended_decl list -> acsl_extension_kind
type extension_visitor =
  Cil.cilVisitor -> acsl_extension_kind -> acsl_extension_kind Cil.visitAction
type extension_printer =
  Printer_api.extensible_printer_type -> Format.formatter ->
  acsl_extension_kind -> unit
type extension_same =
  acsl_extension_kind -> acsl_extension_kind -> Ast_diff.is_same_env -> bool
type extension_module_importer =
  module_builder -> location -> string list -> unit

type register_extension =
  plugin:string -> string ->
  ?preprocessor:extension_preprocessor -> extension_typer ->
  ?visitor:extension_visitor ->
  ?printer:extension_printer -> ?short_printer:extension_printer ->
  ?is_same_ext:extension_same -> bool ->
  unit

type register_extension_block =
  plugin: string -> string ->
  ?preprocessor:extension_preprocessor_block -> extension_typer_block ->
  ?visitor:extension_visitor ->
  ?printer:extension_printer -> ?short_printer:extension_printer ->
  ?is_same_ext:extension_same -> bool -> unit

type extension_single = {
  preprocessor: extension_preprocessor ;
  typer: extension_typer ;
  status: bool ;
  plugin: string ;
}
type extension_block = {
  preprocessor: extension_preprocessor_block ;
  typer: extension_typer_block ;
  status: bool ;
  plugin: string ;
}
type extension_common = {
  category: ext_category ;
  visitor: extension_visitor ;
  printer: extension_printer ;
  short_printer: extension_printer ;
  plugin: string;
  is_same_ext: extension_same;
}
type extension_importer = {
  plugin: string;
  importer: extension_module_importer;
}

let default_printer printer fmt = function
  | Ext_id i -> Format.pp_print_int fmt i
  | Ext_terms ts -> Pretty_utils.pp_list ~sep:",@ " printer#term fmt ts
  | Ext_preds ps -> Pretty_utils.pp_list ~sep:",@ " printer#predicate fmt ps
  | Ext_annot (_,an) ->
    Pretty_utils.pp_list ~pre:"@[<v 0>" ~suf:"@]@\n" ~sep:"@\n"
      printer#extended fmt an

let default_short_printer name _printer fmt _ext_kind =
  Format.pp_print_string fmt name

let rec default_is_same_ext ext1 ext2 env =
  match ext1, ext2 with
  | Ext_id n1, Ext_id n2 -> n1 = n2
  | Ext_terms l1, Ext_terms l2 ->
    Ast_diff.is_same_list Ast_diff.is_same_term l1 l2 env
  | Ext_preds l1, Ext_preds l2 ->
    Ast_diff.is_same_list Ast_diff.is_same_predicate l1 l2 env
  | Ext_annot(s1,l1), Ext_annot(s2,l2) ->
    s1 = s2 && Ast_diff.is_same_list default_is_same_ext_kind l1 l2 env
  | (Ext_id _ | Ext_terms _ | Ext_preds _ | Ext_annot _), _ -> false
and default_is_same_ext_kind ext1 ext2 env =
  default_is_same_ext ext1.ext_kind ext2.ext_kind env

let make
    ~plugin
    name category
    ?(preprocessor=Fun.id)
    typer
    ?(visitor=fun _ _ -> Cil.DoChildren)
    ?(printer=default_printer)
    ?(short_printer=default_short_printer name)
    ?(is_same_ext=default_is_same_ext)
    status : extension_single * extension_common =
  { preprocessor; typer; status; plugin},
  { category; visitor; printer; short_printer; plugin; is_same_ext }

let make_block
    ~plugin
    name category
    ?(preprocessor=Fun.id)
    typer
    ?(visitor=fun _ _ -> Cil.DoChildren)
    ?(printer=default_printer)
    ?(short_printer=default_short_printer name)
    ?(is_same_ext=default_is_same_ext)
    status : extension_block * extension_common =
  { preprocessor; typer; status; plugin},
  { category; visitor; printer; short_printer; plugin; is_same_ext }

let make_importer ~plugin importer : extension_importer =
  { plugin; importer }

module Extensions = struct
  (* Hash table for extension_common. A name can be use by different plugins to
     register an extension so we keep a list. *)
  let ext_tbl : (string, extension_common list) Hashtbl.t =
    Hashtbl.create 5

  (* Hash table for extension_single. A name can be use by different plugins to
     register an extension so we keep a list. *)
  let ext_single_tbl : (string, extension_single list) Hashtbl.t =
    Hashtbl.create 5

  (* Hash table for extension_block. A name can be use by different plugins to
     register an extension so we keep a list. *)
  let ext_block_tbl : (string, extension_block list) Hashtbl.t =
    Hashtbl.create 5

  (* Hash table for extension_importer. A name can be use by different plugins
     to register an extension so we keep a list. *)
  let ext_importer_tbl : (string, extension_importer list) Hashtbl.t =
    Hashtbl.create 5

  let add_to_list tbl key value =
    match Hashtbl.find_opt tbl key with
    | None -> Hashtbl.add tbl key [value]
    | Some values -> Hashtbl.replace tbl key (value :: values)

  (* [get_plugin] is a getter to access the [plugin] field from extension_*
     types, to keep this function generic. *)
  let mem_gen ~get_plugin tbl ~plugin name =
    match Hashtbl.find_opt tbl name with
    | None -> false
    | Some [] -> assert false
    | Some [e] -> String.equal plugin (get_plugin e)
    (* Ambiguity on name, look for an extension with the right plugin. *)
    | Some l -> List.exists (fun e -> String.equal plugin (get_plugin e)) l

  (* Generic function for find functions, catch Not_found and throw a fatal
     instead. [get_plugin] is a getter to access the [plugin] field from
     extension_* types, to keep this function generic. *)
  let find_gen ~get_plugin data tbl ~plugin name =
    try
      match Hashtbl.find tbl name with
      | [] -> assert false
      | [e] -> if String.equal plugin (get_plugin e) then e else raise Not_found
      | l -> List.find (fun e -> String.equal plugin (get_plugin e)) l
    with Not_found ->
      Kernel.fatal ~current:true "Unsupported %s extension named '\\%s::%s'"
        data plugin name

  let get_plugin_common (e : extension_common) = e.plugin

  let get_plugin_single (e : extension_single) = e.plugin

  let get_plugin_block (e : extension_block) = e.plugin

  let get_plugin_importer (e : extension_importer) = e.plugin

  let find_single =
    find_gen ~get_plugin:get_plugin_single "clause" ext_single_tbl

  let find_common =
    find_gen ~get_plugin:get_plugin_common "clause" ext_tbl

  let find_block =
    find_gen ~get_plugin:get_plugin_block "clause" ext_block_tbl

  let find_importer =
    find_gen ~get_plugin:get_plugin_importer "module importer" ext_importer_tbl

  let category ~plugin name = (find_common ~plugin name).category

  let importer ~plugin name = (find_importer ~plugin name).importer

  let is_extension = mem_gen ~get_plugin:get_plugin_common ext_tbl

  let is_extension_block = mem_gen ~get_plugin:get_plugin_block ext_block_tbl

  let is_importer = mem_gen ~get_plugin:get_plugin_importer ext_importer_tbl

  let fullname plugin name =
    if Datatype.String.equal plugin "kernel"
    then name
    else Format.sprintf "\\%s::%s" plugin name

  let register_gen ~make ~tbl cat ~plugin name
      ?preprocessor typer ?visitor ?printer ?short_printer ?is_same_ext status =
    Kernel.debug ~dkey:Kernel.dkey_acsl_extension
      "Registering acsl extension %s" (fullname plugin name);
    let info1,info2 =
      make ~plugin name cat ?preprocessor typer
        ?visitor ?printer ?short_printer ?is_same_ext status
    in
    if is_extension ~plugin:"kernel" name then
      Kernel.warning ~wkey:Kernel.wkey_acsl_extension
        "Trying to register ACSL extension %s reserved by frama-c. \
         Rename this extension to avoid conflict with the kernel. Ignored \
         extension" name
    else begin
      if is_extension ~plugin name then
        Kernel.warning ~wkey:Kernel.wkey_acsl_extension
          "Trying to register ACSL extension %s twice with plugin %s. \
           Ignoring the second extension"
          name plugin
      else begin
        add_to_list tbl name info1;
        add_to_list ext_tbl name info2
      end
    end

  let register = register_gen ~make ~tbl:ext_single_tbl

  let register_block = register_gen ~make:make_block ~tbl:ext_block_tbl

  let register_module_importer ~plugin name importer =
    Kernel.debug ~dkey:Kernel.dkey_acsl_extension
      "Registering module importer extension %s" (fullname plugin name);
    if is_importer ~plugin:"kernel" name then
      Kernel.warning ~wkey:Kernel.wkey_acsl_extension
        "Trying to register module importer extension %s reserved by frama-c. \
         Rename to avoid conflict with the kernel. Ignored module importer" name
    else
      begin
        if is_importer ~plugin name then
          Kernel.warning ~wkey:Kernel.wkey_acsl_extension
            "Trying to register module importer extension %s twice with plugin \
             %s. Ignoring the second extension" name plugin
        else
          add_to_list ext_importer_tbl name (make_importer ~plugin importer)
      end

  let preprocess ~plugin name = (find_single ~plugin name).preprocessor

  let preprocess_block ~plugin name = (find_block ~plugin name).preprocessor

  let typing_gen ~typer ~status name typing_context loc es =
    let normal_error = ref false in
    let has_error _ = normal_error := true in
    let wrapper =
      typing_context.on_error (typer typing_context loc) has_error
    in
    try status, wrapper es
    with
    | (Log.AbortError _ | Log.AbortFatal _) as exn -> raise exn
    | exn when not !normal_error ->
      Kernel.fatal "Typechecking ACSL extension %s raised exception %s"
        name (Printexc.to_string exn)

  let typing ~plugin name =
    let ext_info = find_single ~plugin name in
    let status = ext_info.status in
    let typer =  ext_info.typer in
    typing_gen ~typer ~status name

  let typing_block ~plugin name =
    let ext_info = find_block ~plugin name in
    let status = ext_info.status in
    let typer =  ext_info.typer in
    typing_gen ~typer ~status name

  let visit ~plugin name = (find_common ~plugin name).visitor

  let print ~plugin name printer fmt kind =
    let ext_common = find_common ~plugin name in
    let plugin = get_plugin_common ext_common in
    let full_name = fullname plugin name in
    let pp = ext_common.printer printer in
    match kind with
    | Ext_annot (id,_) ->
      Format.fprintf fmt "@[<v 2>@[%s %s {@]@\n%a}@]" full_name id pp kind
    | _ ->
      Format.fprintf fmt "@[<hov 2>%s %a;@]" full_name pp kind

  let short_print ~plugin name printer fmt kind =
    let pp = (find_common ~plugin name).short_printer in
    Format.fprintf fmt "%a" (pp printer) kind

  let is_same_ext ~plugin name ext1 ext2 =
    let is_same = (find_common ~plugin name).is_same_ext in
    is_same ext1 ext2

  (* Called if we cannot discriminate between several extensions. Cannot happen
     if the parameter [plugin] is provided to mem/find functions. *)
  let throw_ambiguity_error ~get_plugin name l =
    let pp_plugin fmt e = Format.fprintf fmt "%s" (get_plugin e) in
    Kernel.abort ~current:true
      "Conflicts on extension named '%s' registered by different \
       plugins (%a), use '\\plugin::ext_name' syntax to avoid this ambiguity"
      name (Pretty_utils.pp_list ~pre:"" ~suf:"" ~sep:",@ " pp_plugin) l

  let find_plugin ~get_plugin tbl ~plugin name =
    match plugin with
    | Some plugin ->
      if mem_gen ~get_plugin tbl ~plugin name then plugin
      else raise Not_found
    | None ->
      match Hashtbl.find_opt tbl name with
      | None -> raise Not_found
      | Some [] -> assert false
      | Some [e] -> get_plugin e
      | Some l -> throw_ambiguity_error ~get_plugin name l

  let extension_from ?plugin name =
    find_plugin ~get_plugin:get_plugin_common ext_tbl ~plugin name

  let importer_from ?plugin name =
    find_plugin ~get_plugin:get_plugin_importer ext_importer_tbl ~plugin name
end

(* Registration functions *)

let register_behavior =
  Extensions.register Ext_contract
let register_global =
  Extensions.register Ext_global
let register_global_block =
  Extensions.register_block Ext_global
let register_code_annot =
  Extensions.register (Ext_code_annot Ext_here)
let register_code_annot_next_stmt =
  Extensions.register (Ext_code_annot Ext_next_stmt)
let register_code_annot_next_loop =
  Extensions.register (Ext_code_annot Ext_next_loop)
let register_code_annot_next_both =
  Extensions.register (Ext_code_annot Ext_next_both)
let register_module_importer =
  Extensions.register_module_importer

(* Setup global references *)

let () =
  Logic_env.set_extension_handler
    ~category:Extensions.category
    ~is_extension: Extensions.is_extension
    ~is_importer:Extensions.is_importer
    ~preprocess: Extensions.preprocess
    ~is_extension_block: Extensions.is_extension_block
    ~preprocess_block: Extensions.preprocess_block
    ~extension_from: Extensions.extension_from
    ~importer_from: Extensions.importer_from;
  Logic_typing.set_extension_handler
    ~is_extension: Extensions.is_extension
    ~typer: Extensions.typing
    ~typer_block: Extensions.typing_block
    ~importer: Extensions.importer;
  Cil.set_extension_handler
    ~visit: Extensions.visit ;
  Cil_printer.set_extension_handler
    ~print: Extensions.print
    ~short_print:Extensions.short_print;
  Ast_diff.set_extension_diff
    ~is_same_ext: Extensions.is_same_ext
[@@alert "-acsl_extension_handler"]
