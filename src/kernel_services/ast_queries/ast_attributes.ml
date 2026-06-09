(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This file contains attribute related types/functions/values. *)

open Cil_types

let wkey = Kernel.wkey_attrs
let dkey = Kernel.dkey_attrs

(* Construct sorted lists of attributes *)
let get_name (an, _) =
  String.trim_underscores an

(* Attributes are added as they are (e.g. if we add ["__attr"] and then ["attr"]
   both are added). When checking for the presence of an attribute [x] or trying
   to remove it, underscores are removed at the beginning and the end of the
   attribute for both the [x] attribute and the attributes of the list. For
   example, if have a call:

   drop_attribute "__const" [ ("const", []) ; ("__const", []) ; ("__const__", []) ]

   The result is [].
*)
let add ((an, _) as a) al =
  let rec insert_sorted = function
    | [] -> [a]
    | (((an0, _) as a0) :: rest) as l ->
      if an < an0 then a :: l
      else if Cil_datatype.Attribute.equal a a0 then l (* Do not add if already in there *)
      else a0 :: insert_sorted rest (* Make sure we see all attributes with
                                     * this name *)
  in
  insert_sorted al

(* The second attribute list is sorted *)
let add_list al0 al =
  if al0 == [] then
    al
  else
    List.fold_left (fun acc a -> add a acc) al al0

let drop an al =
  let an = String.trim_underscores an in
  List.filter (fun a -> get_name a <> an) al

let rec drop_list anl al =
  match al with
  | [] -> []
  | a :: q ->
    let q' = drop_list anl q in
    if List.mem (get_name a) anl then
      q' (* drop this attribute *)
    else
    if q' == q then al (* preserve sharing *) else a :: q'

let replace_params name params al =
  add (name, params) (drop name al)

let contains an al =
  let an = String.trim_underscores an in
  List.exists (fun a -> get_name a = an) al

let find_params an al =
  let an = String.trim_underscores an in
  List.fold_left (fun acc ((_, param) as a) ->
      if get_name a = an then param @ acc else acc
    ) [] al

let filter an al =
  let an = String.trim_underscores an in
  List.filter (fun a -> get_name a = an) al

(**************************************)
(* Attribute registration and classes *)
(**************************************)

type attribute_class =
  | AttrName of bool
  | AttrFunType of bool
  | AttrType
  | AttrStmt
  | AttrUnknown

type attribute_info = {
  attr_class : attribute_class;
  attr_ignore: bool;
  attr_print : bool;
}

let pp_class fmt = function
  | AttrName    b -> Format.fprintf fmt "AttrName %b" b
  | AttrFunType b -> Format.fprintf fmt "AttrFunType %b" b
  | AttrType      -> Format.fprintf fmt "AttrType"
  | AttrStmt      -> Format.fprintf fmt "AttrStmt"
  | AttrUnknown   -> Format.fprintf fmt "AttrUnknown"

let pp_info fmt ai =
  Format.fprintf fmt "Class: %a, Ignored: %B, Printed: %B"
    pp_class ai.attr_class
    ai.attr_ignore
    ai.attr_print

(* This table contains the mapping of predefined attributes to classes.
 * Extend this table with more attributes as you need. This table is used to
 * determine how to associate attributes with names or type during cabs2cil
 * conversion *)
let known_table : (string, attribute_info) Hashtbl.t = Hashtbl.create 59

let register ?(print=true) ?ignore ac an =
  let attr_ignore = Option.value ~default:(ac=AttrUnknown) ignore in
  let nc = {attr_class = ac; attr_print = print; attr_ignore} in
  match Hashtbl.find_opt known_table an with
  | None ->
    Kernel.debug ~dkey "Registering attribute %S with information@ %a"
      an pp_info nc;
    Hashtbl.add known_table an nc
  | Some info
    when info.attr_class = ac
      && info.attr_ignore = attr_ignore
      && info.attr_print = print ->
    Kernel.debug ~dkey
      "Attribute %S already registered with information %a. Nothing to do"
      an pp_info nc;
    ()
  | Some info ->
    Kernel.debug ~dkey
      "Replacing existing class and status for attribute %s:@ was (%a),@ now \
       (%a)"
      an pp_info info pp_info nc;
    Hashtbl.replace known_table an nc

let register_noprint = register ~print:false

let register_list ?print ?ignore ac al =
  List.iter (register ?print ?ignore ac) al

let remove = Hashtbl.remove known_table

let find_known = Hashtbl.find_opt known_table

let is_known = Hashtbl.mem known_table

let get_class ~default name =
  match (Hashtbl.find known_table name).attr_class with
  | exception Not_found -> default
  | AttrUnknown -> default
  | ac -> ac

let should_print name =
  match find_known name with
  | None -> true
  | Some info -> info.attr_print

let should_ignore name =
  match find_known name with
  | None -> true
  | Some info -> info.attr_ignore

let partition ~(default:attribute_class) (attrs: attributes) :
  attributes * attributes * attributes =
  let rec loop (n,f,t) = function
    | [] -> n, f, t
    | ((an, _) as a) :: rest ->
      match get_class ~default an with
      | AttrName _ -> loop (add a n, f, t) rest
      | AttrFunType _ ->
        loop (n, add a f, t) rest
      | AttrType -> loop (n, f, add a t) rest
      | AttrStmt ->
        Kernel.warning ~wkey "unexpected statement attribute %s" an;
        loop (n,f,t) rest
      | AttrUnknown -> loop (n, f, t) rest
  in
  loop ([], [], []) attrs

(*************************)
(* qualifiers attributes *)
(*************************)

let qualifier_attributes = [ "const"; "restrict"; "volatile"; "ghost" ]
let () = register_list AttrType qualifier_attributes

(**********************)
(* storage attributes *)
(**********************)

let () = register (AttrName false) "static"

(*******************************)
(* Frama-C internal attributes *)
(*******************************)

(* AttrUnknown attributes. *)

let anonymous_attribute_name = "fc_anonymous"
let anonymous_attribute = (anonymous_attribute_name, [])

let () = register_noprint AttrUnknown anonymous_attribute_name

(* AttrType attributes. *)

let bitfield_attribute_name = "FRAMA_C_BITFIELD_SIZE"
let cast_irrelevant_attributes = ["visibility"]

let () = register_list AttrType
    (bitfield_attribute_name :: cast_irrelevant_attributes)

let () = register_list ~ignore:true AttrType ["declspec"; "arraylen"]

(* AttrStmt attributes. *)

let frama_c_keep_block = "FRAMA_C_KEEP_BLOCK"
let frama_c_ghost_else = "fc_ghost_else"

let () = register_list ~print:false ~ignore:true AttrStmt
    [frama_c_keep_block; frama_c_ghost_else]

(* Attrname attributes. *)

let frama_c_ghost_formal = "fc_ghost_formal"
let frama_c_init_obj     = "fc_initialized_object"
let frama_c_mutable      = "fc_mutable"
let fc_stdlib            = "fc_stdlib"
let fc_stdlib_generated  = "fc_stdlib_generated"
let fc_local_static      = "fc_local_static"
let fc_literal           = "fc_literal"
let frama_c_destructor   = "fc_destructor"
let fc_oldstyleproto     = "FC_OLDSTYLEPROTO"
let fc_missingproto      = "missingproto"

let () =
  register_list ~print:false ~ignore:true (AttrName false)
    [ frama_c_ghost_formal
    ; frama_c_init_obj
    ; frama_c_mutable
    ; fc_stdlib
    ; fc_stdlib_generated
    ; fc_local_static
    ; fc_literal
    ; frama_c_destructor
    ]

let () =
  register_list ~ignore:true (AttrName false)
    [fc_oldstyleproto; fc_missingproto]

(* AttFuntype attributes. *)

let frama_c_inlined = "fc_inlined"

let () = register_noprint ~ignore:true (AttrFunType false) frama_c_inlined

let () = register (AttrFunType false) "warn_unused_result"

(* Globals (extern or not) internal to Frama-C's libc *)
let fc_stdlib_internal = "fc_stdlib_internal"
let () =
  register (AttrName false) fc_stdlib_internal

(* Extern globals that replace a real libc global *)
let fc_stdlib_for_macro = "fc_stdlib_for_macro"
let () =
  register (AttrName false) fc_stdlib_for_macro

let find_fc_stdlib_extern_replacement attributes =
  let open Option.Operators in
  let is_extern_replace_attr attribute =
    let name = get_name attribute in
    String.equal name fc_stdlib_for_macro
  in
  let* extern_replace_attribute =
    List.find_opt is_extern_replace_attr attributes
  in
  let attrparams = snd extern_replace_attribute in
  match attrparams with
  | [ AStr replacement ] -> Some replacement
  | _ ->
    Kernel.error
      "attribute %s expects one string parameter."
      fc_stdlib_for_macro;
    None

(* List of attributes for internal uses. *)

let fc_internal_attributes =
  [ fc_stdlib
  ; anonymous_attribute_name
  ; frama_c_keep_block
  ; frama_c_ghost_else
  ; frama_c_ghost_formal
  ; frama_c_init_obj
  ; frama_c_mutable
  ; fc_stdlib_generated
  ; fc_literal
  ; fc_local_static
  ; frama_c_destructor
  ; frama_c_inlined
  ; fc_oldstyleproto
  ; fc_missingproto
  ; fc_stdlib_internal
  ; fc_stdlib_for_macro
  ; "declspec"
  ; "arraylen"
  ]

let spare_attributes_for_c_cast = fc_internal_attributes @ qualifier_attributes

let spare_attributes_for_logic_cast = spare_attributes_for_c_cast

(***************************************)
(* Some (mostly GCC / MSVC) attributes *)
(***************************************)

let () =
  register_list (AttrName false)
    [ "section"; "constructor"; "destructor"; "unused"; "used"; "weak";
      "no_instrument_function"; "alias"; "no_check_memory_usage";
      "exception"; "model"; "aconst";
      (* Gcc uses this to specify the name to be used in assembly for a global. *)
      "asm" ]

let () =
  (* Now come the MSVC declspec attributes *)
  register_list (AttrName true)
    [ "thread"; "naked"; "dllimport"; "dllexport"; "selectany"; "allocate";
      "nothrow"; "novtable"; "property"; "uuid"; "align" ]

let () =
  register_list (AttrFunType false)
    [ "format"; "regparm"; "longcall"; "noinline"; "always_inline" ]

let () =
  register_list (AttrFunType true)
    [ "stdcall";"cdecl"; "fastcall"; "noreturn" ]

let () =
  (* GCC label and statement attributes. *)
  register_list AttrStmt
    [ "hot"; "cold"; "fallthrough"; "assume"; "musttail" ]

(* GCC 'malloc' attributes can refer to erased functions and make the code
   un-reparsable, so we keep them in the AST but not pretty-print them. *)
let () = register_noprint ~ignore:true AttrUnknown "malloc"

(**********************)
(* Unknown attributes *)
(**********************)

(* packed and aligned are treated separately, we ignore them during standard
   processing. *)
let () = register_list AttrUnknown [ "packed" ; "aligned" ]

(* These attributes are registered to help case studies. We parse them and
   reprint them, but they are ignored when comparing types and might be assigned
   to the wrong AST node.
*)
let () =
  register_list AttrUnknown
    [ "dummy"
    ; "signal"  (* AVR-specific attribute *)
    ; "leaf"
    ; "nonnull"
    ; "deprecated"
    ; "access"
    ; "returns_twice"
    ; "pure"
    ; "cleanup"
    ; "warning"
    ; "format_arg"
    ; "no_sanitize"
    ; "target"
    ]

(*********************)
(* Utility functions *)
(*********************)

let filter_qualifiers al =
  List.filter (fun a -> List.mem (get_name a) qualifier_attributes) al

let split_array_attributes al =
  List.partition (fun a -> List.mem (get_name a) qualifier_attributes) al

let split_storage_modifiers al =
  let isstoragemod ((an, _) : attribute) : bool =
    match (Hashtbl.find known_table an).attr_class with
    | exception Not_found -> false
    | AttrName issm | AttrFunType issm -> issm
    | _ -> false
  in
  let stom, rest = List.partition isstoragemod al in
  if not (Machine.msvcMode ()) then stom, rest
  else
    (* Put back the declspec. Put it without the leading __ since these will
     * be added later *)
    let stom' =
      List.map (fun (an, args) -> ("declspec", [ACons(an, args)])) stom
    in
    stom', rest

(******************************************)
(* Forward declaration from Cil_datatype. *)
(******************************************)

let () =
  Cil_datatype.drop_non_logic_attributes :=
    drop_list spare_attributes_for_logic_cast

let () =
  Cil_datatype.drop_fc_internal_attributes :=
    drop_list fc_internal_attributes

let () =
  Cil_datatype.drop_ignored_attributes :=
    let keep_attr (name, _) = not (should_ignore name) in
    (fun attributes -> List.filter keep_attr attributes)

(* Registering attributes from -register-attributes *)

let class_of_string = function
  | "name" -> AttrName false
  | "type" -> AttrType
  | "funtype" -> AttrFunType false
  | "stmt" -> AttrStmt
  | "unknown" -> AttrUnknown
  | _ -> assert false

let fold_attr attr values =
  let init =
    match find_known attr with
    | None -> (AttrUnknown, None, None)
    | Some info ->
      (info.attr_class, Some info.attr_ignore, Some info.attr_print)
  in
  List.fold_left (fun (c, i, p) v ->
      match v with
      | Kernel.Default -> (c, i, p)
      | Class s -> ((class_of_string s), i, p)
      | Ignore b -> (c, Some b, p)
      | Print b -> (c, i, Some b)
    ) init values

let () =
  Cmdline.run_after_configuring_stage (fun () ->
      Kernel.RegisterAttributes.iter (fun (name, values) ->
          let (attr_class, ignore, print) = fold_attr name values in
          register ?ignore ?print attr_class name
        )
    )


