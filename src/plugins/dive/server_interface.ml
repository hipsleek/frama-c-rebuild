(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server
open Data
open Dive_types

let package = Package.package ~plugin:"dive" ~title:"Dive Services" ()
let declare name ?(descr = name) =
  Data.declare ~package ~name ~descr:(Markdown.plain descr)

module Enum (X: sig type t end) =
struct
  include Enum
  let dictionary: X.t dictionary = Enum.dictionary ()
  let tag name descr = Enum.tag ~name ~descr:(Markdown.plain descr) dictionary
  let publish lookup name descr =
    set_lookup dictionary lookup;
    Request.dictionary ~package ~name ~descr:(Markdown.plain descr) dictionary
end

module Record () =
struct
  include Record
  type record
  let record : record Record.signature = Record.signature ()
  let field name ?(descr = name) =
    Record.field record ~name ~descr:(Markdown.plain descr)
  let option name ?(descr = name) =
    Record.option record ~name ~descr:(Markdown.plain descr)
  let publish ?descr name =
    let descr = Option.map Markdown.plain descr in
    Record.publish record ~package ~name ?descr
end


(* -------------------------------------------------------------------------- *)
(* --- Data types                                                         --- *)
(* -------------------------------------------------------------------------- *)

let origin_to_locations = function
  | Studia.Writes.Assign s
  | CallDirect s -> [Printer_tag.localizable_of_stmt s]
  | CallIndirect _s -> []
  | GlobalInit (vi, _init) ->
    [ Printer_tag.localizable_of_declaration (SGlobal vi) ]
  | FormalInit (_vi, callsites) ->
    List.concat_map
      (fun (_,l) -> List.map Printer_tag.localizable_of_stmt l)
      callsites


module Range : Data.S with type t = range =
struct
  include Record ()

  let backward =
    option "backward" ~descr:"range for the write dependencies" jint
  let forward =
    option "forward" ~descr:"range for the read dependencies" jint

  let descr = "Parametrization of the exploration range."
  include (val publish "range" ~descr)
  type t = range

  let to_json r=
    default |>
    set backward r.backward |>
    set forward r.forward |>
    to_json

  let of_json js =
    let r = of_json js in
    { backward = get backward r; forward = get forward r; }
end


module Window : Data.S with type t = window =
struct
  include Record ()

  let perception =
    let descr = "how far dive will explore from root nodes ; \
                 must be a finite range" in
    field "perception" ~descr (module Range)

  let horizon =
    let descr = "range beyond which the nodes must be hidden" in
    field "horizon" ~descr (module Range)

  let descr = "Global parametrization of the exploration."
  include (val publish "explorationWindow" ~descr)
  type t = window

  let to_json w =
    default |>
    set perception w.perception |>
    set horizon w.horizon |>
    to_json

  let of_json js =
    let r = of_json js in
    { perception = get perception r; horizon = get horizon r; }
end


module NodeId =
struct
  include Data.Jint
  let jtype = declare "nodeId" ~descr:"A node identifier in the graph" jtype
end

module Callsite =
struct
  type t = Cil_types.kernel_function * Cil_types.kinstr
  let jtype = declare "callsite" (Jrecord [
      "fun", Jstring;
      "instr", Junion [ Jnumber ; Jtag "global" ];
    ])

  let output_kinstr = function
    | Cil_types.Kglobal -> `String "global"
    | Cil_types.Kstmt stmt -> `Int stmt.sid

  let to_json (kf, kinstr) =
    `Assoc [
      ("fun", `String (Kernel_function.get_name kf)) ;
      ("instr", output_kinstr kinstr) ;
    ]

  let of_json _ = Data.failure "Callsite.of_json not implemented"
end

module Callstack =
struct
  include Data.Jlist (Callsite)
  let descr = "The callstack context for a node"
  let jtype = declare "callstack" ~descr jtype
end

module NodeLocality =
struct
  include Record ()

  let file = field "file" jstring
  let callstack = option "callstack" (module Callstack)

  let descr = "The description of a node locality"
  include (val publish "nodeLocality" ~descr)
  type t = node_locality

  let to_json t =
    let to_option = function [] -> None | l -> Some l in
    default |>
    set file t.loc_file |>
    set callstack (to_option t.loc_callstack) |>
    to_json

  let of_json js =
    let r = of_json js in
    { loc_file = get file r ;
      loc_callstack = Option.value ~default:[] (get callstack r) }
end

module NodeKind = struct
  include Enum (struct type t = node_kind end)

  let scalar =    tag "scalar"    "a single memory cell"
  let composite = tag "composite" "a memory bloc containing cells"
  let scattered = tag "scattered" "a set of memory locations designated by \
                                   an lvalue"
  let unknown =   tag "unknown"   "an unresolved memory location"
  let alarm =     tag "alarm"     "an alarm emitted by Frama-C"
  let absolute =  tag "absolute"  "a memory location designated by a range \
                                   of addresses"
  let error =     tag "error"     "a placeholder node when an error prevented \
                                   the generation process"
  let const =     tag "const"     "a numeric constant literal"

  let lookup = function
    | Scalar _ -> scalar
    | Composite _ -> composite
    | Scattered _ -> scattered
    | Unknown _ -> unknown
    | Alarm _ -> alarm
    | AbsoluteMemory -> absolute
    | Const _ -> const
    | Error _ -> error

  include (val publish lookup "nodeKind" "The nature of a node")
end

module Taint = struct
  include Enum (struct type t = Eva.Results.taint end)

  let untainted = tag "untainted" "not tainted by anything"
  let indirect = tag "indirect" "tainted by control"
  let direct = tag "direct" "tainted by data"

  let lookup = function
    | Eva.Results.Direct -> direct
    | Indirect -> indirect
    | Untainted -> untainted

  include (val publish lookup "taint" "Taint of a memory location")
end

module Computation = struct
  include Enum (struct type t = computation end)

  let yes = tag "yes" "all dependencies have been computed"
  let partial = tag "partial" "some dependencies have been explored"
  let no = tag "no" "dependencies have not been computed"

  let lookup = function
    | Done -> yes
    | Partial _ -> partial
    | NotDone -> no

  let descr = "The computation state of a node read or write dependencies"
  include (val publish lookup "exploration" descr)
end

module NodeSpecialRange = struct
  include Enum (struct type t = node_range end)

  let empty = tag "empty" "no value ever computed for this node"
  let singleton = tag "singleton" "this node can only have one value"
  let wide = tag "wide" "this node can take almost all values of its type"

  let lookup = function
    | Empty -> empty
    | Singleton -> singleton
    | Wide -> wide
    | Normal _ -> raise Not_found

  let descr = "A qualitative description of the range of values \
               that this node can take."

  include (val publish lookup "nodeSpecialRange" descr)
end

module NodeRange = struct
  type t = node_range
  let descr = "A qualitative or quantitative description of the range of \
               values that this node can take."
  let jtype = declare "nodeRange" ~descr
      (Junion [ Jnumber ; NodeSpecialRange.jtype ])

  let to_json = function
    | Normal range_grade -> `Int range_grade
    | range -> NodeSpecialRange.to_json range

  let of_json _ = Data.failure "NodeRange.of_json not implemented"
end

module Node =
struct
  include Record ()

  let id = field "id" (module NodeId)
  let label = field "label" jstring
  let nkind = field "nkind" (module NodeKind)
  let locality = field "locality" (module NodeLocality)
  let is_root = field "is_root" jbool
  let backward_explored = field "backward_explored" (module Computation)
  let forward_explored = field "forward_explored" (module Computation)
  let writes = field "writes" (module Jlist (Kernel_ast.Marker))
  let values = field "values" (joption jstring)
  let range = field "range" (module NodeRange)
  let typ = option "type" jstring
  let taint = option "taint" (module Taint)

  include (val publish "node")

  let node_type node =
    match Node_kind.to_lval node.node_kind with
    | None -> None
    | Some lval ->
      let typ = Cil.typeOfLval lval in
      Some (Pretty_utils.to_string Cil_printer.pp_typ typ)

  let to_json node =
    default |>
    set id node.node_key |>
    set label (Pretty_utils.to_string Node_kind.pretty node.node_kind) |>
    set nkind node.node_kind |>
    set locality node.node_locality |>
    set is_root node.node_is_root |>
    set backward_explored node.node_writes_computation |>
    set forward_explored node.node_reads_computation |>
    set writes (List.concat_map origin_to_locations node.node_writes) |>
    set values
      (Option.map (Pretty_utils.to_string Cvalue.V.pretty) node.node_values) |>
    set range node.node_range |>
    set typ (node_type node) |>
    set taint node.node_taint |>
    to_json
end

module Dependency =
struct
  include Record ()

  let id = field "id" jint
  let src = field "src" (module NodeId)
  let dst = field "dst" (module NodeId)
  let dkind = field "dkind" jstring
  let origins = field "origins" (module Jlist (Kernel_ast.Marker))

  include (val publish "dependency" ~descr:"The dependency between two nodes")

  let dep_kind = function
    | Callee -> "callee"
    | Data -> "data"
    | Address -> "addr"
    | Control -> "ctrl"
    | Composition -> "comp"

  let to_json (n1, dep, n2) =
    default |>
    set id dep.dependency_key |>
    set src n1.node_key |>
    set dst n2.node_key |>
    set dkind  (dep_kind dep.dependency_kind) |>
    set origins (List.concat_map origin_to_locations dep.dependency_origins) |>
    to_json
end

module Element =
struct
  type t = Context.element = Node of node | Edge of (node * dependency * node)
  let descr = "A graph element, either a node or a dependency"
  let jtype = declare "element" ~descr (Junion [Node.jtype ; Dependency.jtype])

  let to_json = function
    | Node v -> Node.to_json v
    | Edge edge -> Dependency.to_json edge
end


(* -------------------------------------------------------------------------- *)
(* --- State handling                                                     --- *)
(* -------------------------------------------------------------------------- *)

let global_window = ref {
    perception = { backward = Some 2 ; forward = Some 1 };
    horizon = { backward = None ; forward = None };
  }

let get_context = (* TODO: projectify ? *)
  let context = Context.create () in
  fun () ->
    if Eva.Analysis.is_computed () then
      context
    else
      Server.Data.failure "Eva analysis not computed"

let vertices g = Dive_graph.fold_vertex (fun n acc -> n ::acc) g []
let edges g = Dive_graph.fold_edges_e (fun d acc -> d ::acc) g []

let output_graph g =
  `Assoc [
    ("nodes", `List (List.map Node.to_json (vertices g))) ;
    ("deps", `List (List.map Dependency.to_json (edges g)))
  ]

let output_to_json out_channel g =
  let json = output_graph g in
  Yojson.Basic.to_channel out_channel json

module Graph =
struct
  let name = "graph"
  let model = States.model ()
  let descr = Markdown.plain "The graph being built as a set of vertices and \
                              edges"

  let key = function
    | Element.Node v -> Format.sprintf "n%d" v.node_key
    | Edge (_,dep,_) -> Format.sprintf "d%d" dep.dependency_key

  let () = States.column model ~name:"element"
      ~descr:(Markdown.plain "a graph element")
      ~data:(module Element)
      ~get:(fun el -> el)


  let iter f =
    let context = get_context () in
    let graph = Context.get_graph context in
    Dive_graph.iter_vertex (fun v -> f (Element.Node v)) graph;
    Dive_graph.iter_edges_e (fun e -> f (Element.Edge e)) graph

  let _array =
    let hook f =
      fun g -> f (get_context ()) g
    in
    States.register_array ~package ~name ~descr ~key ~iter model
      ~add_update_hook:(hook Context.set_update_hook)
      ~add_remove_hook:(hook Context.set_remove_hook)
      ~add_reload_hook:(hook Context.set_clear_hook)
end


module NodeId' =
struct
  type t = node
  let jtype = NodeId.jtype
  let to_json node = NodeId.to_json node.node_key
  let of_json json =
    let node_key = Data.Jint.of_json json in
    try
      Context.find_node (get_context ()) node_key
    with Not_found ->
      Data.failure "no node '%d' in the current graph" node_key
end


(* -------------------------------------------------------------------------- *)
(* --- Actions                                                            --- *)
(* -------------------------------------------------------------------------- *)

let finalize' context = function
  | None -> ()
  | Some node ->
    let may_explore f =
      Option.iter (fun depth -> f ~depth context node)
    in
    may_explore Build.explore_backward !global_window.perception.backward;
    may_explore Build.explore_forward !global_window.perception.forward;
    let horizon = !global_window.horizon in
    if Option.is_some horizon.forward ||
       Option.is_some horizon.backward
    then
      Build.reduce_to_horizon context horizon node

let finalize context node =
  finalize' context (Some node)

let () = Request.register ~package
    ~kind:`SET ~name:"window"
    ~descr:(Markdown.plain "Set the exploration window")
    ~input:(module Window) ~output:(module Data.Junit)
    (fun window -> global_window := window)

let () = Request.register ~package
    ~kind:`EXEC ~name:"clear"
    ~descr:(Markdown.plain "Erase the graph and start over with an empty one")
    ~input:(module Data.Junit) ~output:(module Data.Junit)
    (fun () -> Context.clear (get_context ()))

let () = Request.register ~package
    ~kind:`EXEC ~name:"add"
    ~descr:(Markdown.plain "Add a node to the graph")
    ~input:(module Kernel_ast.Marker) ~output:(module Joption (NodeId'))
    begin fun loc ->
      let context = get_context () in
      let node = Build.add_localizable context loc in
      finalize' context node;
      node
    end

let () = Request.register ~package
    ~kind:`EXEC ~name:"explore"
    ~descr:(Markdown.plain "Explore the graph starting from an existing vertex")
    ~input:(module NodeId') ~output:(module Data.Junit)
    begin fun node ->
      let context = get_context () in
      Build.show context node;
      finalize context node
    end

let () = Request.register ~package
    ~kind:`EXEC ~name:"show"
    ~descr:(Markdown.plain "Show the dependencies of an existing vertex")
    ~input:(module NodeId') ~output:(module Data.Junit)
    begin fun node ->
      let context = get_context () in
      Build.show context node;
      Build.explore_backward ~depth:1 context node;
      finalize' context None
    end

let () = Request.register ~package
    ~kind:`EXEC ~name:"hide"
    ~descr:(Markdown.plain "Hide the dependencies of an existing vertex")
    ~input:(module NodeId') ~output:(module Data.Junit)
    begin fun node ->
      let context = get_context () in
      Build.hide context node;
      finalize' context None
    end
