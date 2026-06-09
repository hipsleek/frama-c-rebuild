(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server

module G = Services.G

(* --- Package declaration --- *)

let package = Package.package ~plugin:"callgraph" ~title:"Callgraph Services" ()


(* --- Helper modules --- *)

module Record () =
struct
  module Record = Data.Record
  type record
  let record : record Record.signature = Record.signature ()
  let field name ?(descr = name) =
    Record.field record ~name ~descr:(Markdown.plain descr)
  let publish ?descr name =
    let descr = Option.map Markdown.plain descr in
    Record.publish record ~package ~name ?descr
end

module Enum (X: sig type t end) =
struct
  module Enum = Data.Enum
  let dictionary: X.t Enum.dictionary = Enum.dictionary ()
  let tag name descr = Enum.tag ~name ~descr:(Markdown.plain descr) dictionary
  let publish lookup name descr =
    Enum.set_lookup dictionary lookup;
    Request.dictionary ~package ~name ~descr:(Markdown.plain descr) dictionary
end


(* --- Types --- *)

module Vertex =
struct
  include Record ()

  let name = field "name" (module Data.Jstring)
      ~descr: "The function name of the node"
  let decl = field "decl" (module Kernel_ast.Decl)
      ~descr: "The declaration tag of the function"
  let is_root = field "is_root" Data.jbool
      ~descr: "whether this node is the root of a service"
  let root = field "root" (module Kernel_ast.Decl)
      ~descr: "the root of this node's service"

  include (val publish "vertex")
  type t = Cil_types.kernel_function Service_graph.vertex

  let to_json (v : Cil_types.kernel_function Service_graph.vertex) =
    default |>
    set name (Kernel_function.get_name v.node) |>
    set decl (Printer_tag.SFunction v.node) |>
    set root (SFunction v.root.node) |>
    set is_root v.is_root |>
    to_json

  let of_json _js = Data.failure "Vertex.of_json not implemented"
end

module EdgeKind =
struct
  include Enum (struct type t = Service_graph.edge end)

  let inter_services =  tag "inter_services" "a call between two services"
  let inter_functions = tag "inter_functions" "a call inside a service"
  let both =            tag "both" "both cases above"

  let lookup = function
    | Service_graph.Inter_services -> inter_services
    | Inter_functions -> inter_functions
    | Both -> both

  include (val publish lookup
              "edgeKind" "Whether the call goes through services or not")
end

module Edge =
struct
  include Record ()

  let src = field "src" (module Kernel_ast.Decl)
  let dst = field "dst" (module Kernel_ast.Decl)
  let kind = field "kind" (module EdgeKind)

  include (val publish "edge")
  type t = G.E.t

  let to_json (e : t) =
    default |>
    set src (SFunction (G.E.src e).node) |>
    set dst (SFunction (G.E.dst e).node) |>
    set kind (G.E.label e) |>
    to_json

  let of_json _js = Data.failure "Edge.of_json not implemented"
end

module Graph =
struct
  include Record ()

  let vertices = field "vertices" (module Data.Jlist (Vertex))
  let edges = field "edges" (module Data.Jlist (Edge))

  include (val publish "graph" ~descr:"The callgraph of the current project")
  type t = G.t

  let get_vertices (g : t) =
    G.fold_vertex (fun v acc -> v :: acc ) g []

  let get_edges (g : t) =
    G.fold_edges_e (fun v acc -> v :: acc ) g []

  let to_json (g : t) =
    default |>
    set vertices (get_vertices g) |>
    set edges (get_edges g) |>
    to_json

  let of_json _js = Data.failure "Graph.of_json not implemented"
end


(* --- Requests --- *)

let _signal =
  States.register_value
    ~package ~name:"callgraph"
    ~descr:(Markdown.plain
              "The current callgraph or an empty graph if it has not been computed yet")
    ~output:(module Data.Joption (Graph))
    ~add_hook:Services.add_hook
    ~get:
      begin fun () ->
        if Services.is_computed () then
          Some (Services.get ())
        else
          None
      end
    ()

let _signal =
  States.register_value
    ~package ~name:"isComputed"
    ~descr:(Markdown.plain
              "This boolean is true if the graph has been computed")
    ~output:(module Data.Jbool)
    ~add_hook:Services.add_hook
    ~get:Services.is_computed
    ()

let () = Request.register ~package
    ~kind:`EXEC ~name:"compute"
    ~descr:(Markdown.plain "Compute the callgraph for the current project")
    ~input:(module Data.Junit) ~output:(module Data.Junit)
    (fun () -> ignore (Services.get ()))
