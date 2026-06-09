(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Data
module Md = Markdown
module Js = Yojson.Basic.Util
module Pkg = Package
open Cil_types

let package = Pkg.package ~title:"Ast Services" ~name:"ast" ~readme:"ast.md" ()

(* -------------------------------------------------------------------------- *)
(* --- Compute Ast                                                        --- *)
(* -------------------------------------------------------------------------- *)

let () = Request.register ~package
    ~kind:`EXEC ~name:"compute"
    ~descr:(Md.plain "Ensures that AST is computed")
    ~input:(module Junit) ~output:(module Junit) Ast.compute

let ast_changed_signal = Request.signal ~package ~name:"changed"
    ~descr:(Md.plain "Emitted when the AST has been changed")

let ast_changed () = Request.emit ast_changed_signal

let ast_update_hook f =
  begin
    Ast.add_hook_on_update f;
    Ast.apply_after_computed (fun _ -> f ());
  end

let () = ast_update_hook ast_changed
let () = Annotations.add_hook_on_change ast_changed

(* -------------------------------------------------------------------------- *)
(* --- File Positions                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Position =
struct
  type t = Filepos.t

  let jtype = Data.declare ~package ~name:"source"
      ~descr:(Md.plain "Source file positions.")
      (Jrecord [
          "dir", Jstring;
          "base", Jstring;
          "file", Jstring;
          "line", Jnumber;
        ])

  let to_json p =
    let path = Filepos.path p |> Filepath.to_string in
    let file =
      if Server_parameters.has_relative_filepath ()
      then path
      else Filepos.path p |> Filepath.to_string_abs
    in
    `Assoc [
      "dir"  , `String (Filename.dirname path) ;
      "base" , `String (Filename.basename path) ;
      "file" , `String file ;
      "line" , `Int (Filepos.line p) ;
    ]

  let of_json js =
    let fail () = failure_from_type_error "Invalid source format" js in
    match js with
    | `Assoc assoc ->
      begin
        match List.assoc "file" assoc, List.assoc "line" assoc with
        | `String path, `Int line ->
          Log.source ~file:(Filepath.of_string path) ~line
        | _, _ -> fail ()
        | exception Not_found -> fail ()
      end
    | _ -> fail ()

end

(* -------------------------------------------------------------------------- *)
(* ---  Generic Markers                                                   --- *)
(* -------------------------------------------------------------------------- *)

module type TagInfo =
sig
  type t
  val name : string
  val descr : string
  val create : t -> string
  module H : Hashtbl.S with type key = t
end

module type Tag =
sig
  include Data.S
  val index : t -> string
  val find : string -> t
end

module MakeTag(T : TagInfo) :
sig
  include Tag with type t = T.t
  val iter : (t * string -> unit) -> unit
  val hook : (t * string -> unit) -> unit
end =
struct

  type t = T.t

  type index = {
    tags : string T.H.t ;
    items : (string,T.t) Hashtbl.t ;
  }

  let index () = {
    tags = T.H.create 0 ;
    items = Hashtbl.create 0 ;
  }

  let module_name = String.capitalize_ascii T.name

  module TYPE : Datatype.S with type t = index =
    Datatype.Make
      (struct
        type t = index
        include Datatype.Undefined
        let reprs = [index()]
        let name = Printf.sprintf "Server.Kernel_ast.%s.TYPE" module_name
        let mem_project = Datatype.never_any_project
      end)

  module STATE = State_builder.Ref(TYPE)
      (struct
        let name = Printf.sprintf "Server.Kernel_ast.%s.STATE" module_name
        let dependencies = [ Ast.self ]
        let default = index
      end)
  let () = Ast.add_monotonic_state STATE.self

  let iter f =
    T.H.iter (fun key str -> f (key, str)) (STATE.get ()).tags

  let hooks = ref []
  let hook f = hooks := !hooks @ [f]

  let index item =
    let { tags ; items } = STATE.get () in
    try T.H.find tags item
    with Not_found ->
      let tag = T.create item in
      T.H.add tags item tag ;
      Hashtbl.add items tag item ;
      List.iter (fun fn -> fn (item,tag)) !hooks ; tag

  let find tag = Hashtbl.find (STATE.get()).items tag

  let jtype = Data.declare ~package ~name:T.name
      ~descr:(Md.plain T.descr)
      (Pkg.Jkey T.name)

  let to_json item = `String (index item)
  let of_json js =
    try find (Js.to_string js)
    with Not_found ->
      Data.failure "invalid %s (%a)" T.name Json.pp_dump js

end

module Decl = MakeTag
    (struct
      open Printer_tag
      type t = declaration
      let name = "decl"
      let descr = "AST Declarations markers"
      module H = Declaration.Hashtbl
      let kid = ref 0
      let create = function
        | SEnum _ -> Printf.sprintf "#E%d" (incr kid ; !kid)
        | SComp _ -> Printf.sprintf "#C%d" (incr kid ; !kid)
        | SType _ -> Printf.sprintf "#T%d" (incr kid ; !kid)
        | SGlobal vi -> Printf.sprintf "#G%d" vi.vid
        | SFunction kf -> Printf.sprintf "#F%d" @@ Kernel_function.get_id kf
        | SGAnnot _ -> Printf.sprintf "#A%d" (incr kid; !kid)
    end)

module Marker = MakeTag
    (struct
      open Printer_tag
      type t = localizable
      let name = "marker"
      let descr = "Localizable AST markers"
      module H = Localizable.Hashtbl
      let kid = ref 0
      let create = function
        | PStmt(_,s) -> Printf.sprintf "#s%d" s.sid
        | PStmtStart(_,s) -> Printf.sprintf "#k%d" s.sid
        | PVDecl(_,_,v) -> Printf.sprintf "#v%d" v.vid
        | PLval _ -> Printf.sprintf "#l%d" (incr kid ; !kid)
        | PExp(_,_,e) -> Printf.sprintf "#e%d" e.eid
        | PTermLval _ -> Printf.sprintf "#t%d" (incr kid ; !kid)
        | PGlobal _ -> Printf.sprintf "#g%d" (incr kid ; !kid)
        | PIP _ -> Printf.sprintf "#p%d" (incr kid ; !kid)
        | PType _ -> Printf.sprintf "#y%d" (incr kid ; !kid)
    end)

module PrinterTag = Printer_tag.Make(struct let tag = Marker.index end)

(* -------------------------------------------------------------------------- *)
(* --- Ast Data                                                           --- *)
(* -------------------------------------------------------------------------- *)

module Lval =
struct
  open Printer_tag

  type t = kinstr * lval
  let jtype = Marker.jtype

  let to_json (kinstr, lval) =
    let kf = match kinstr with
      | Kglobal -> None
      | Kstmt stmt -> Some (Kernel_function.find_englobing_kf stmt)
    in
    Marker.to_json (PLval (kf, kinstr, lval))

  let find = function
    | PLval (_, kinstr, lval) -> kinstr, lval
    | PVDecl (_, kinstr, vi) -> kinstr, Cil.var vi
    | PGlobal (GVar (vi, _, _) | GVarDecl (vi, _)) -> Kglobal, Cil.var vi
    | _ -> raise Not_found

  let mem tag = try let _ = find tag in true with Not_found -> false

  let of_json js =
    try find (Marker.of_json js)
    with Not_found -> Data.failure "not a lval marker"

end

module Stmt =
struct
  type t = stmt
  let jtype = Marker.jtype
  let to_json st =
    let kf = Kernel_function.find_englobing_kf st in
    Marker.to_json (PStmtStart(kf,st))
  let of_json js =
    let open Printer_tag in
    match Marker.of_json js with
    | PStmt(_,st) | PStmtStart(_,st) -> st
    | _ -> Data.failure "not a stmt marker"
end

module Kinstr =
struct
  type t = kinstr
  let jtype = Pkg.Joption Marker.jtype
  let to_json = function
    | Kglobal -> `Null
    | Kstmt st -> Stmt.to_json st
  let of_json = function
    | `Null -> Kglobal
    | js -> Kstmt (Stmt.of_json js)
end

(* -------------------------------------------------------------------------- *)
(* --- Declaration Attributes                                             --- *)
(* -------------------------------------------------------------------------- *)

module DeclKind =
struct
  open Printer_tag
  type t = declaration
  let jtype = Data.declare
      ~package ~name:"declKind"
      ~descr:(Md.plain "Declaration kind")
      (Junion [
          (* C *)
          Jkey "ENUM";
          Jkey "UNION";
          Jkey "STRUCT";
          Jkey "TYPEDEF";
          Jkey "GLOBAL";
          Jkey "FUNCTION";
          (* ACSL *)
          Jkey "LFUNPRED";
          Jkey "INVARIANT";
          Jkey "AXIOMATIC";
          Jkey "MODULE";
          Jkey "LEMMA";
          Jkey "EXTENSION";
          Jkey "VOLATILE";
          Jkey "LTYPE";
          Jkey "MODEL";
        ])

  let global_annotation_kind = function
    | Dfun_or_pred _ -> "LFUNPRED"
    | Dinvariant _ -> "INVARIANT"
    | Dtype_annot _ -> "INVARIANT"
    | Daxiomatic _ -> "AXIOMATIC"
    | Dmodule _ -> "MODULE"
    | Dlemma _ -> "LEMMA"
    | Dextended _ -> "EXTENSION"
    | Dvolatile _ -> "VOLATILE"
    | Dtype _ -> "LTYPE"
    | Dmodel_annot _ -> "MODEL"

  let to_json = function
    | SEnum _ -> `String "ENUM"
    | SComp { cstruct = true } -> `String "STRUCT"
    | SComp { cstruct = false } -> `String "UNION"
    | SType _ -> `String "TYPEDEF"
    | SGlobal _ -> `String "GLOBAL"
    | SFunction _ -> `String "FUNCTION"
    | SGAnnot a -> `String (global_annotation_kind a)
end

module GAnnotRoots = struct
  include State_builder.Hashtbl
      (Cil_datatype.Global_annotation.Hashtbl)
      (Datatype.Unit)
      (struct
        let name = "Server.Kernel_ast.GAnnotsRoots"
        let size = 43
        let dependencies = [ Ast.self ]
      end)

  let is_root ga = mem ga
end

module DeclAttributes =
struct
  open Printer_tag

  let model = States.model ()

  (* We must iterate over all known declaration in the Ast, contrarily to
     markers, for which we only need attributes of already generated markers. *)
  let iter_declaration f =
    if Ast.is_computed () then
      let marked = Declaration.Hashtbl.create 0 in
      Cil.iterGlobals
        (Ast.get())
        (fun g ->
           begin match g with
             | GAnnot(ga,_) -> GAnnotRoots.add ga ()
             | _ -> ()
           end ;
           match declaration_of_global g with
           | None -> ()
           | Some d ->
             if not @@ Declaration.Hashtbl.mem marked d then
               begin
                 Declaration.Hashtbl.add marked d () ;
                 f (d,Decl.index d)
               end)

  let () =
    States.column
      ~name:"kind"
      ~descr:(Md.plain "Declaration kind")
      ~data:(module DeclKind)
      ~get:fst
      model

  let () =
    States.column
      ~name:"self"
      ~descr:(Md.plain "Declaration's marker")
      ~data:(module Marker)
      ~get:(fun (decl,_) -> localizable_of_declaration decl)
      model

  let () =
    States.column
      ~name:"name"
      ~descr:(Md.plain "Declaration identifier")
      ~data:(module Jstring)
      ~get:(fun (decl,_) -> name_of_declaration decl)
      model

  let () =
    States.column
      ~name:"label"
      ~descr:(Md.plain "Declaration label (uncapitalized kind & name)")
      ~data:(module Jstring)
      ~get:(fun (decl,_) -> Pretty_utils.to_string pp_declaration decl)
      model

  let () =
    States.column
      ~name:"source"
      ~descr:(Md.plain "Source location")
      ~data:(module Position)
      ~get:(fun (decl,_) -> fst @@ loc_of_declaration decl)
      model

  let array = States.register_array
      ~package
      ~name:"declAttributes"
      ~descr:(Md.plain "Declaration attributes")
      ~key:snd
      ~keyName:"decl"
      ~keyType:Decl.jtype
      ~iter:iter_declaration
      ~add_reload_hook:ast_update_hook
      model

  let update (d, s) =
    match d with
    | SGAnnot ga when not @@ GAnnotRoots.is_root ga -> ()
    | _ -> States.update array (d,s)

  let () = Decl.hook update

end

(* -------------------------------------------------------------------------- *)
(* --- Decl Printer                                                       --- *)
(* -------------------------------------------------------------------------- *)

let with_print_libc f =
  if Kernel.PrintLibc.get () then
    f ()
  else
    let finally =
      if Kernel.PrintLibc.is_set ()
      then fun () -> Kernel.PrintLibc.unsafe_set false
      else fun () -> Kernel.PrintLibc.clear ()
    in
    Kernel.PrintLibc.unsafe_set true;
    Fun.protect ~finally f

let print_global_ast global =
  let printer = PrinterTag.(with_unfold_precond (fun _ -> true) pp_global) in
  with_print_libc (fun () -> Jbuffer.to_json printer global)

let () = Request.register ~package
    ~kind:`GET ~name:"printDeclaration"
    ~descr:(Md.plain "Prints an AST Declaration")
    ~signals:[ast_changed_signal]
    ~input:(module Decl) ~output:(module Jtext)
    (fun d -> print_global_ast @@ Printer_tag.global_of_declaration d)

(* -------------------------------------------------------------------------- *)
(* --- Marker Attributes                                                  --- *)
(* -------------------------------------------------------------------------- *)

module MarkerKind =
struct
  open Printer_tag
  type t = localizable
  let jtype = Data.declare
      ~package ~name:"markerKind"
      ~descr:(Md.plain "Marker kind")
      (Junion [
          Jkey "STMT";
          Jkey "LFUN"; Jkey "DFUN";
          Jkey "LVAR"; Jkey "DVAR";
          Jkey "LVAL"; Jkey "EXP";
          Jkey "TERM";
          Jkey "TYPE";
          Jkey "PROPERTY";
          Jkey "DECLARATION";
        ])
  let to_json = function
    | PStmt _ | PStmtStart _ -> `String "STMT"
    | PVDecl(_,Kglobal,vi) ->
      `String (if Globals.Functions.mem vi then "DFUN" else "DVAR")
    | PVDecl _ -> `String "DVAR"
    | PTermLval(_,_,_,(TVar { lv_origin = Some vi },TNoOffset))
    | PLval(_,_,(Var vi,NoOffset)) ->
      `String (if Globals.Functions.mem vi then "LFUN" else "LVAR")
    | PLval _ -> `String "LVAL"
    | PExp _ -> `String "EXP"
    | PTermLval _ -> `String "TERM"
    | PType _ -> `String "TYPE"
    | PIP _ -> `String "PROPERTY"
    | PGlobal _ -> `String "DECLARATION"
end

module MarkerAttributes =
struct
  open Printer_tag

  let global_annotation_label_kind short = function
    | Dfun_or_pred ({ l_type = None }, _) ->
      if short then "Pred" else "Predicate"
    | Dfun_or_pred _ ->
      if short then "LFun" else "Logic Function"
    | Dinvariant _ ->
      if short then "Inv" else "Invariant"
    | Dtype_annot _ ->
      if short then "TInv" else "Type Invariant"
    | Daxiomatic _ ->
      if short then "Ax" else "Axiomatic"
    | Dmodule _ ->
      if short then "Mod" else "Module"
    | Dlemma _ ->
      "Lemma"
    | Dextended _ ->
      if short then "Ext" else "Extension"
    | Dvolatile _ ->
      if short then "Vol" else "Volatile"
    | Dtype _ ->
      if short then "LType" else "Logic Type"
    | Dmodel_annot _ ->
      "Model"

  let label_kind ~short m =
    match varinfo_of_localizable m with
    | Some vi ->
      if Globals.Functions.mem vi then "Function" else
      if vi.vglob then
        if short then "Global" else "Global Variable"
      else if vi.vformal then
        if short then "Formal" else "Formal Parameter"
      else if vi.vtemp then
        if short then "Temp" else "Temporary Variable (generated)"
      else
      if short then "Local" else "Local Variable"
    | None ->
      match m with
      | PStmt _ | PStmtStart _ -> if short then "Stmt" else "Statement"
      | PLval _ -> if short then "Lval" else "L-value"
      | PTermLval _ -> if short then "Lval" else "ACSL L-value"
      | PVDecl _ -> assert false
      | PExp _ -> if short then "Expr" else "Expression"
      | PIP _ -> if short then "Prop" else "Property"
      | PGlobal (GType _ | GCompTag _ | GEnumTag _ | GEnumTagDecl _)
      | PType _ -> "Type"
      | PGlobal (GAnnot (ga, _)) ->
        global_annotation_label_kind short ga
      | PGlobal _ -> if short then "Decl" else "Declaration"

  let descr_localizable fmt = function
    | PGlobal (GType(ti,_)) ->
      PrinterTag.pp_typ fmt (Cil_const.mk_tnamed ti)
    | PGlobal (GCompTag(ci,_) | GCompTagDecl(ci,_)) ->
      PrinterTag.pp_typ fmt (Cil_const.mk_tcomp ci)
    | PGlobal (GEnumTag(ei,_) | GEnumTagDecl(ei,_)) ->
      PrinterTag.pp_typ fmt (Cil_const.mk_tenum ei)
    | g -> pp_localizable fmt g

  let model = States.model ()

  let () =
    States.column
      ~name:"kind"
      ~descr:(Md.plain "Marker kind (key)")
      ~data:(module MarkerKind) ~get:fst
      model

  let () =
    States.option
      ~name:"scope"
      ~descr:(Md.plain "Marker Scope (where it is printed in)")
      ~data:(module Decl)
      ~get:(fun (tag,_) -> declaration_of_localizable tag)
      model

  let () =
    States.option
      ~name:"definition"
      ~descr:(Md.plain "Marker's Target Definition (when applicable)")
      ~data:(module Marker)
      ~get:(fun (tag,_) -> definition_of_localizable tag)
      model

  let () =
    States.column
      ~name:"labelKind"
      ~descr:(Md.plain "Marker kind label")
      ~data:(module Jalpha)
      ~get:(fun (tag,_) -> label_kind ~short:true tag)
      model

  let () =
    States.column
      ~name:"titleKind"
      ~descr:(Md.plain "Marker kind title")
      ~data:(module Jalpha)
      ~get:(fun (tag,_) -> label_kind ~short:false tag)
      model


  let () =
    States.option
      ~name:"name"
      ~descr:(Md.plain "Marker identifier (when applicable)")
      ~data:(module Jalpha)
      ~get:(fun (tag, _) -> Printer_tag.name_of_localizable tag)
      model

  let () =
    States.column
      ~name:"descr"
      ~descr:(Md.plain "Marker description")
      ~data:(module Jstring)
      ~get:(fun (tag, _) -> Rich_text.sprintf "%a" descr_localizable tag)
      model

  let () =
    let get (tag, _) =
      let pos = fst (Printer_tag.loc_of_localizable tag) in
      if Filepos.is_known pos then Some pos else None
    in
    States.option
      ~name:"sloc"
      ~descr:(Md.plain "Source location")
      ~data:(module Position)
      ~get
      model

  let array = States.register_array
      ~package
      ~name:"markerAttributes"
      ~descr:(Md.plain "Marker attributes")
      ~key:snd
      ~keyName:"marker"
      ~keyType:Marker.jtype
      ~iter:Marker.iter
      ~add_reload_hook:ast_update_hook
      model

  let () = Marker.hook (States.update array)

end

(* -------------------------------------------------------------------------- *)
(* --- Filters                                                            --- *)
(* -------------------------------------------------------------------------- *)

(*  Filters can be defined on elements of type ['a] with a unique name and
    a boolean function f: 'a -> bool, allowing the user to show/hide elements
    for which f is true or false.
    Additional information for each filter includes:
    - whether the filter is currently active;
    - positive/negative labels shown to the user to show/hide elements
      for which f is true/false respectively.
    - default values for the filter, i.e. whether elements for which [f] is
      true/false are shown or hidden by default.
*)

let filter_jtype =
  let jtype =
    Package.Jrecord
      [ "id", Jstring; (* Unique name. *)
        "enabled", Jboolean; (* Is the filter currently enabled? *)
        "positive_label", Jstring; (* Label for positive elements. *)
        "negative_label", Jstring; (* Label for negative elements. *)
        "positive_default", Jboolean; (* Are positive elements shown by default? *)
        "negative_default", Jboolean; (* Are negative elements shown by default? *)
      ]
  in
  let descr = Md.plain "Type of filters that can be applied to AST elements" in
  Data.declare ~package ~name:"filter" ~descr jtype

module MakeFilter (Info: sig type t val name: string end) = struct

  type filter = {
    name: string; (* Unique identifiant of the filter *)
    enable: unit -> bool; (* Is the filter currently enabled? *)
    value: Info.t -> bool; (* Compute the filter value for an element *)
    labels: string * string; (* Positive and negative labels shown to the user *)
    default: bool * bool; (* Are positive/negative elements shown by default? *)
  }

  module Filter = struct
    type t = filter
    let jtype = filter_jtype

    let to_json filter = `Assoc [
        "id", `String filter.name;
        "enabled", `Bool (filter.enable ());
        "positive_label", `String (fst filter.labels);
        "negative_label", `String (snd filter.labels);
        "positive_default", `Bool (fst filter.default);
        "negative_default", `Bool (snd filter.default);
      ]

    let of_json _ = Data.failure "Filter.of_json not implemented"
  end

  (* List of filters registered via [register] below. *)
  let filters_ref : Filter.t list ref = ref []

  (* List of hooks registered via [register] below, used to refresh filter
     requests whenever a filter changes. *)
  let hooks_ref : ((unit -> unit) -> unit) list ref = ref []

  (* Signal emitted whenever a filter changes. *)
  let signal =
    let name = String.lowercase_ascii Info.name ^ "Filters" in
    let descr = Md.plain ("Signal for " ^ Info.name ^ " filters") in
    Request.signal ~package ~name ~descr

  (* Default positive and negative labels for a filter of name [name]. *)
  let default_labels name =
    let lower = String.lowercase_ascii in
    let positive_label = lower name ^ " " ^ lower Info.name in
    positive_label, "non-" ^ positive_label

  (* Registers a new filter. *)
  let register name
      ?(labels = default_labels name) ?default
      ?(enable=fun _ -> true) ?add_hook f =
    let default =
      Option.fold default ~none:(true, true) ~some:(fun b -> b, not b)
    in
    let filter = { name; enable; value = f; labels; default } in
    filters_ref := filter :: !filters_ref;
    Option.iter (fun f -> hooks_ref := f :: !hooks_ref) add_hook;
    Option.iter (fun f -> f (fun _ -> Request.emit signal)) add_hook;
    Request.emit signal

  (* GET request listing all registered filters. *)
  let () =
    let name = "get" ^ String.capitalize_ascii Info.name ^ "Filters" in
    let descr = Md.plain ("List of filters for " ^ Info.name) in
    Request.register
      ~package ~kind:`GET ~name ~descr ~signals:[signal]
      ~input:(module Junit) ~output:(module Jlist (Filter))
      (fun () -> List.rev !filters_ref)

  (* Compute the value of each registered filter for an element [elt]. *)
  let compute_filters elt =
    let aux acc filter =
      if filter.enable ()
      then (filter.name, filter.value elt) :: acc
      else acc
    in
    List.fold_left aux [] !filters_ref

  let add_hook (f: unit -> unit) =
    List.iter (fun add_hook -> add_hook f) !hooks_ref
end

(* Filters on functions. *)
module FctFilters = struct
  include MakeFilter
      (struct type t = kernel_function let name = "functions" end)

  let get_vi = Kernel_function.get_vi

  let () =
    register "builtin" (fun kf -> Cil_builtins.has_fc_builtin_attr (get_vi kf))
      ~labels:("Frama-C builtins", "source functions") ~default:false;
    register "stdlib" Kernel_function.is_in_libc ~default:false;
    register "defined" Kernel_function.is_definition
      ~labels:("defined functions", "undefined functions");
    register "extern" (fun kf -> (get_vi kf).vstorage = Extern);
    register "ghost" Kernel_function.is_ghost;
end

(* Filters on variables. *)
module VarFilters = struct
  include MakeFilter (struct type t = varinfo let name = "variables" end)

  let () =
    register "stdlib" ((fun vi -> Cil.is_in_libc vi.vattr)) ~default:false;
    register "extern" (fun vi -> vi.vstorage = Extern);
    register "const" (fun vi -> Cil.isGlobalInitConst vi);
    register "volatile" (fun vi -> Ast_types.is_volatile vi.vtype);
    register "ghost" (fun vi -> Ast_types.is_ghost vi.vtype);
    register "init" (fun vi -> Option.is_some (Globals.Vars.find vi).init)
      ~labels:("variables with explicit initializer",
               "variables without explicit initializer");
    register "source" (fun vi -> vi.vsource) ~default:true
      ~labels:("variables from the source code",
               "variables generated from analyses");
end

type 'a filter_registration =
  string -> ?labels:string * string -> ?default:bool ->
  ?enable:(unit -> bool) -> ?add_hook:((unit -> unit) -> unit) ->
  ('a -> bool) -> unit

let register_fct_filter = FctFilters.register
let register_var_filter = VarFilters.register


(* -------------------------------------------------------------------------- *)
(* --- Functions                                                          --- *)
(* -------------------------------------------------------------------------- *)

let () = Request.register ~package
    ~kind:`GET ~name:"getMainFunction"
    ~descr:(Md.plain "Get the current 'main' function.")
    ~input:(module Junit) ~output:(module Joption(Decl))
    begin fun () ->
      try Some (SFunction (fst @@ Globals.entry_point ()))
      with Globals.No_such_entry_point _ -> None
    end

let () = Request.register ~package
    ~kind:`GET ~name:"getFunctions"
    ~descr:(Md.plain "Collect all functions in the AST")
    ~input:(module Junit) ~output:(module Jlist(Decl))
    begin fun () ->
      let pool = ref [] in
      Globals.Functions.iter
        (fun kf -> pool := Printer_tag.SFunction kf :: !pool) ;
      List.rev !pool
    end

module Functions =
struct

  let key kf = Printf.sprintf "kf#%d" (Kernel_function.get_id kf)

  let signature kf =
    let g = Printer_tag.PGlobal (Kernel_function.get_global kf) in
    let to_string () = Rich_text.sprintf "%a" Printer_tag.pp_localizable g in
    let txt = with_print_libc to_string in
    if Kernel_function.is_entry_point kf then (txt ^ " /* main */") else txt

  let is_builtin kf =
    Cil_builtins.has_fc_builtin_attr (Kernel_function.get_vi kf)

  let is_extern kf =
    let vi = Kernel_function.get_vi kf in
    vi.vstorage = Extern

  let iter f =
    Globals.Functions.iter
      (fun kf ->
         let name = Kernel_function.get_name kf in
         if not (Ast_info.start_with_frama_c_builtin name) then f kf)

  let array : kernel_function States.array =
    begin
      let model = States.model () in
      States.column model
        ~name:"decl"
        ~descr:(Md.plain "Declaration Tag")
        ~data:(module Decl)
        ~get:(fun kf -> Printer_tag.SFunction kf) ;
      States.column model
        ~name:"name"
        ~descr:(Md.plain "Name")
        ~data:(module Data.Jalpha)
        ~get:Kernel_function.get_name ;
      States.column model
        ~name:"signature"
        ~descr:(Md.plain "Signature")
        ~data:(module Data.Jstring)
        ~get:signature ;
      States.column model
        ~name:"main"
        ~descr:(Md.plain "Is the function the main entry point")
        ~data:(module Data.Jbool)
        ~default:false
        ~get:Kernel_function.is_entry_point;
      States.column model
        ~name:"defined"
        ~descr:(Md.plain "Is the function defined?")
        ~data:(module Data.Jbool)
        ~default:false
        ~get:Kernel_function.is_definition;
      States.column model
        ~name:"stdlib"
        ~descr:(Md.plain "Is the function from the Frama-C stdlib?")
        ~data:(module Data.Jbool)
        ~default:false
        ~get:Kernel_function.is_in_libc;
      States.column model
        ~name:"builtin"
        ~descr:(Md.plain "Is the function a Frama-C builtin?")
        ~data:(module Data.Jbool)
        ~default:false
        ~get:is_builtin;
      States.column model
        ~name:"extern"
        ~descr:(Md.plain "Is the function extern?")
        ~data:(module Data.Jbool)
        ~default:false
        ~get:is_extern;
      States.column model
        ~name:"sloc"
        ~descr:(Md.plain "Source location")
        ~data:(module Position)
        ~get:(fun kf -> fst (Kernel_function.get_location kf));
      States.column model
        ~name:"filters"
        ~descr:(Md.plain "List of filter values")
        ~data:(module Data.Jlist (Data.Jpair (Data.Jstring) (Data.Jbool)))
        ~get:FctFilters.compute_filters;
      States.register_array model
        ~package ~key
        ~name:"functions"
        ~descr:(Md.plain "AST Functions")
        ~iter
        ~add_reload_hook:(fun f -> ast_update_hook f; FctFilters.add_hook f)
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Global variables                                                   --- *)
(* -------------------------------------------------------------------------- *)

module GlobalVars = struct

  let key vi = Printf.sprintf "vi#%d" vi.vid

  let _ : varinfo States.array =
    let model = States.model () in
    States.column model
      ~name:"decl"
      ~descr:(Md.plain "Declaration Tag")
      ~data:(module Decl)
      ~get:(fun vi -> Printer_tag.SGlobal vi);
    States.column model
      ~name:"name"
      ~descr:(Md.plain "Name")
      ~data:(module Data.Jalpha)
      ~get:(fun vi -> vi.vname);
    States.column model
      ~name:"type"
      ~descr:(Md.plain "Type")
      ~data:(module Jstring)
      ~get:(fun vi -> Rich_text.sprintf "%a" PrinterTag.pp_typ vi.vtype);
    States.column model
      ~name:"stringLiteral"
      ~descr:(Md.plain "Does the variable represent a string literal?")
      ~data:(module Data.Jbool)
      ~get:Ast_info.is_string_literal;
    States.column model
      ~name:"sloc"
      ~descr:(Md.plain "Source location")
      ~data:(module Position)
      ~get:(fun vi -> fst vi.vdecl);
    States.column model
      ~name:"filters"
      ~descr:(Md.plain "List of filter values")
      ~data:(module Data.Jlist (Data.Jpair (Data.Jstring) (Data.Jbool)))
      ~get:VarFilters.compute_filters;
    States.register_array model
      ~package ~key
      ~name:"globals"
      ~descr:(Md.plain "AST global variables")
      ~iter:(fun f -> Globals.Vars.iter (fun vi _init -> f vi))
      ~add_reload_hook:(fun f -> ast_update_hook f; VarFilters.add_hook f)
end

(* -------------------------------------------------------------------------- *)
(* --- Marker Information                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Information =
struct

  type info = {
    id: string;
    rank: int;
    label: string; (* short name *)
    title: string; (* full title name *)
    descr: string; (* description for information values *)
    enable: unit -> bool;
    pretty: Format.formatter -> Printer_tag.localizable -> unit
  }

  (* Info markers serialization *)

  module S =
  struct
    type t = (info * Jtext.t)
    let jtype = Package.(Jrecord[
        "id", Jstring ;
        "label", Jstring ;
        "title", Jstring ;
        "descr", Jstring ;
        "text", Jtext.jtype ;
      ])
    let of_json _ = failwith "Information.Info"
    let to_json (info,text) = `Assoc [
        "id", `String info.id ;
        "label", `String info.label ;
        "title", `String info.title ;
        "descr", `String info.descr ;
        "text", text ;
      ]
  end

  (* Info markers registry *)

  let rankId = ref 0
  let registry : (string,info) Hashtbl.t = Hashtbl.create 0

  let jtext pp marker =
    try
      let buffer = Jbuffer.create () in
      let fmt = Jbuffer.formatter buffer in
      pp fmt marker;
      Format.pp_print_flush fmt ();
      Jbuffer.contents buffer
    with Not_found ->
      `Null

  let rank ({rank},_) = rank
  let by_rank a b = Stdlib.compare (rank a) (rank b)

  let get_information tgt =
    let infos = ref [] in
    Hashtbl.iter
      (fun _ info ->
         if info.enable () then
           match tgt with
           | None -> infos := (info, `Null) :: !infos
           | Some marker ->
             let text = jtext info.pretty marker in
             if not (Jbuffer.is_empty text) then
               infos := (info, text) :: !infos
      ) registry ;
    List.sort by_rank !infos

  let signal = Request.signal ~package
      ~name:"getInformationUpdate"
      ~descr:(Md.plain "Updated AST information")

  let update () = Request.emit signal

  let register ~id ~label ~title
      ?(descr = title)
      ?(enable = fun _ -> true)
      pretty =
    let rank = incr rankId ; !rankId in
    let info = { id ; rank ; label ; title ; descr; enable ; pretty } in
    if Hashtbl.mem registry id then
      ( let msg = Format.sprintf
            "Server.Kernel_ast.register_info: duplicate %S" id in
        raise (Invalid_argument msg) );
    Hashtbl.add registry id info

end

let () = Request.register ~package
    ~kind:`GET ~name:"getInformation"
    ~descr:(Md.plain
              "Get available information about markers. \
               When no marker is given, returns all kinds \
               of information (with empty `descr` field).")
    ~input:(module Joption(Marker))
    ~output:(module Jlist(Information.S))
    ~signals:[Information.signal]
    Information.get_information

(* -------------------------------------------------------------------------- *)
(* --- Default Kernel Information                                         --- *)
(* -------------------------------------------------------------------------- *)

let () = Information.register
    ~id:"kernel.ast.location"
    ~label:"Location"
    ~title:"Source file location"
    begin fun fmt loc ->
      let pos = fst @@ Printer_tag.loc_of_localizable loc in
      if Filepath.is_empty (Filepos.path pos) then
        raise Not_found ;
      Filepos.pretty fmt pos
    end

let () = Information.register
    ~id:"kernel.ast.varinfo"
    ~label:"Var"
    ~title:"Variable Information"
    begin fun fmt loc ->
      match loc with
      | PLval (_ , _, (Var x,NoOffset)) | PVDecl(_,_,x) ->
        if not x.vreferenced then Format.pp_print_string fmt "unused " ;
        begin
          match x.vstorage with
          | NoStorage -> ()
          | Extern -> Format.pp_print_string fmt "extern "
          | Static -> Format.pp_print_string fmt "static "
          | Register -> Format.pp_print_string fmt "register "
        end ;
        if x.vghost then Format.pp_print_string fmt "ghost " ;
        if x.vaddrof then Format.pp_print_string fmt "aliased " ;
        if x.vformal then Format.pp_print_string fmt "formal" else
        if x.vglob then Format.pp_print_string fmt "global" else
        if x.vtemp then Format.pp_print_string fmt "temporary" else
          Format.pp_print_string fmt "local" ;
      | _ -> raise Not_found
    end

let () = Information.register
    ~id:"kernel.ast.typeinfo"
    ~label:"Type"
    ~title:"Type of C/ACSL expression"
    begin fun fmt loc ->
      match loc with
      | PExp (_, _, e) -> PrinterTag.pp_typ fmt (Cil.typeOf e)
      | PLval (_, _, lval) -> PrinterTag.pp_typ fmt (Cil.typeOfLval lval)
      | PVDecl (_, _, vi) -> PrinterTag.pp_typ fmt vi.vtype
      | PTermLval (_, _, _, tlval) ->
        PrinterTag.pp_logic_type fmt (Cil.typeOfTermLval tlval)
      | _ -> raise Not_found
    end

let () = Information.register
    ~id:"kernel.ast.typedef"
    ~label:"Typedef"
    ~title:"Type Definition"
    begin fun fmt loc ->
      match loc with
      | PType ({ tnode = TNamed _ } as ty)
      | PGlobal (GType({ ttype = ty },_)) ->
        PrinterTag.pp_typ fmt (Ast_types.unroll ty)
      | _ -> raise Not_found
    end

let () = Information.register
    ~id:"kernel.ast.typesizeof"
    ~label:"Sizeof"
    ~title:"Size of a C-type or C-variable"
    begin fun fmt loc ->
      let typ =
        match loc with
        | PType typ -> typ
        | PVDecl(_,_,vi) when Ast_types.is_object vi.vtype -> vi.vtype
        | PGlobal (GType(ti,_)) -> ti.ttype
        | PGlobal (GCompTagDecl(ci,_) | GCompTag(ci,_)) -> Cil_const.mk_tcomp ci
        | PGlobal (GEnumTagDecl(ei,_) | GEnumTag(ei,_)) -> Cil_const.mk_tenum ei
        | _ -> raise Not_found
      in
      try
        let bits = Cil.bitsSizeOf typ in
        let bytes = bits / 8 in
        let rbits = bits mod 8 in
        if rbits > 0 then
          if bytes > 0 then
            Format.fprintf fmt "%d bytes + %d bits" bytes rbits
          else
            Format.fprintf fmt "%d bits" rbits
        else
          Format.fprintf fmt "%d bytes" bytes
      with Cil.SizeOfError (msg, _) ->
        Format.fprintf fmt "Unknown size: %s" msg
    end


let () = Information.register
    ~id:"kernel.ast.alignof"
    ~label:"Alignof"
    ~title:"Alignment of a C type, variable or field"
    begin fun fmt loc ->
      let print kind alignof elt =
        try
          Format.fprintf fmt "%d bytes (%s alignment)" (alignof elt) kind
        with Cil.SizeOfError (msg, _typ) ->
          Format.fprintf fmt "Unknown alignment: %s" msg
      in
      match loc with
      | PType typ
      | PGlobal (GType ( { ttype=typ }, _ )) ->
        print "type" Cil.bytesAlignOf typ
      | PVDecl (_, _, vi)
      | PLval (_, _, (Var vi, NoOffset))
      | PGlobal (GVarDecl (vi, _) | GVar (vi, _, _))
        when Ast_types.is_object vi.vtype ->
        print "variable" Cil.bytesAlignOfVarinfo vi
      | PLval (_, _, lval) ->
        begin
          match Cil.lastOffset (snd lval) with
          | Field (fi, NoOffset) -> print "field" Cil.bytesAlignOfField fi
          | _ -> raise Not_found
        end
      | PGlobal (GCompTagDecl (ci, _) | GCompTag (ci, _)) ->
        print "type" Cil.bytesAlignOf (Cil_const.mk_tcomp ci)
      | PGlobal (GEnumTagDecl (ei, _) | GEnumTag (ei, _)) ->
        print "type" Cil.bytesAlignOf (Cil_const.mk_tenum ei)
      | _ -> raise Not_found
    end

let () = Information.register
    ~id:"kernel.ast.propertyStatus"
    ~label:"Status"
    ~title:"Property Consolidated Status"
    begin fun fmt loc ->
      match loc with
      | PIP prop when Property.has_status prop ->
        Property_status.Feedback.pretty fmt @@
        Property_status.Feedback.get prop
      | _ -> raise Not_found
    end

let () = Information.register
    ~id:"kernel.ast.marker"
    ~label:"Marker"
    ~title:"Ivette marker (for debugging)"
    ~enable:(fun _ -> Server_parameters.debug_atleast 1)
    begin fun fmt loc ->
      let tag = Marker.index loc in
      Format.fprintf fmt "%S" tag
    end

let () = Server_parameters.Debug.add_hook_on_update
    (fun _ -> Information.update ())

(* -------------------------------------------------------------------------- *)
(* --- Marker at a position                                               --- *)
(* -------------------------------------------------------------------------- *)

let get_marker_at ~file ~line ~col =
  if file="" then None else
    let path = Filepath.of_string file in
    let pos = Filepos.make ~path ~line ~column:col ~offset:0 () in
    Printer_tag.pos_to_localizable ~precise_col:true pos

let () =
  let descr =
    Md.plain
      "Returns the marker and function at a source file position, if any. \
       Input: file path, line and column. \
       File can be empty, in case no marker is returned."
  in
  let signature = Request.signature
      ~output:(module Joption(Marker)) () in
  let get_file = Request.param signature
      ~name:"file" ~descr:(Md.plain "File path") (module Jstring) in
  let get_line = Request.param signature
      ~name:"line" ~descr:(Md.plain "Line (1-based)") (module Jint) in
  let get_col = Request.param signature
      ~name:"column" ~descr:(Md.plain "Column (0-based)") (module Jint) in
  Request.register_sig signature
    ~package ~descr ~kind:`GET ~name:"getMarkerAt"
    ~signals:[ast_changed_signal]
    (fun rq () ->
       get_marker_at ~file:(get_file rq) ~line:(get_line rq) ~col:(get_col rq))

(* -------------------------------------------------------------------------- *)
(* --- Files                                                              --- *)
(* -------------------------------------------------------------------------- *)

let get_files () =
  let files = Kernel.Files.get () in
  List.map (fun f -> (Filepath.to_string_abs f)) files

let () =
  Request.register
    ~package
    ~descr:(Md.plain "Get the currently analyzed source file names")
    ~kind:`GET
    ~name:"getFiles"
    ~input:(module Junit) ~output:(module Jlist(Jstring))
    get_files

let set_files files =
  let s = String.concat "," files in
  Kernel.Files.As_string.set s

let () =
  Request.register
    ~package
    ~descr:(Md.plain "Set the source file names to analyze.")
    ~kind:`SET
    ~name:"setFiles"
    ~input:(module Jlist(Jstring))
    ~output:(module Junit)
    set_files

(* -------------------------------------------------------------------------- *)
(* --- Build a marker from an ACSL term                                   --- *)
(* -------------------------------------------------------------------------- *)

let environment () =
  let open Logic_typing in
  Lenv.empty () |> append_pre_label |> append_here_label

let parse_expr env kf stmt term =
  let term = Logic_parse_string.term ~env kf term in
  let exp = Logic_to_c.term_to_exp term in
  Printer_tag.PExp (Some kf, Kstmt stmt, exp)

let parse_lval env kf stmt term =
  let term = Logic_parse_string.term_lval ~env kf term in
  let lval = Logic_to_c.term_lval_to_lval term in
  Printer_tag.PLval (Some kf, Kstmt stmt, lval)

let build_marker parse marker term =
  match Printer_tag.ki_of_localizable marker with
  | Kglobal -> Data.failure "No statement at selection point"
  | Kstmt stmt ->
    let module C = Logic_to_c in
    let module Parser = Logic_parse_string in
    let kf () = Kernel_function.find_englobing_kf stmt in
    try parse (environment ()) (kf ()) stmt term
    with Not_found | Parser.(Error _ | Unbound _) | C.No_conversion ->
      Data.failure "Invalid term"

let () =
  let module Md = Markdown in
  let s = Request.signature ~output:(module Marker) () in
  let get_marker = Request.param s ~name:"stmt"
      ~descr:(Md.plain "Marker position from where to localize the term")
      (module Marker) in
  let get_term = Request.param s ~name:"term"
      ~descr:(Md.plain "ACSL term to parse")
      (module Data.Jstring) in
  Request.register_sig ~package s
    ~kind:`GET ~name:"parseExpr"
    ~descr:(Md.plain "Parse a C expression and returns the associated marker")
    (fun rq () -> build_marker parse_expr (get_marker rq) (get_term rq))

let () =
  let module Md = Markdown in
  let s = Request.signature ~output:(module Marker) () in
  let get_marker = Request.param s ~name:"stmt"
      ~descr:(Md.plain "Marker position from where to localize the term")
      (module Marker) in
  let get_term = Request.param s ~name:"term"
      ~descr:(Md.plain "ACSL term to parse")
      (module Data.Jstring) in
  Request.register_sig ~package s
    ~kind:`GET ~name:"parseLval"
    ~descr:(Md.plain "Parse a C lvalue and returns the associated marker")
    (fun rq () -> build_marker parse_lval (get_marker rq) (get_term rq))

(* -------------------------------------------------------------------------- *)
