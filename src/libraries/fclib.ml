(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Module aliases for all our libraries. Useful when shadowing module's name.
    @since 32.0-Germanium
*)

(** {2 Floating point} *)

module Floating_point = Floating_point
module Typed_float = Typed_float

(** {2 Arithmetic} *)

module Field = Field
module Finite = Finite
module Linear = Linear
module Nat = Nat
module Rational = Rational

(** {2 Zarith} *)

(** @since 33.0-Arsenic *)
module Z = Z

(** {2 Datatype} *)

module Datatype = Datatype
module Descr = Descr
module Structural_descr = Structural_descr
module Type = Type
module Unmarshal = Unmarshal
module Unmarshal_z = Unmarshal_z

(** {2 Monads} *)

module Composition = Composition
module Identity = Identity
module Monad = Monad
module State_monad = State_monad

(** {2 Project} *)

module Project = Project
module Project_skeleton = Project_skeleton
module State = State
module State_builder = State_builder
module State_dependency_graph = State_dependency_graph
module State_selection = State_selection
module State_topological = State_topological

(** {2 Stdlib} *)

module Array = Array
module Extlib = Extlib
module Fun = Fun
module Hash = Hash
module Hashtbl = Hashtbl
module Int = Int
module Integer = Integer [@@deprecated "Use Fclib.Z instead."]
module List = List
module Map = Map
module Option = Option
module Pretty = Pretty
module Result = Result
module Set = Set
module String = String

(** {2 Utils} *)

module Ansi_escape = Ansi_escape
module Async = Async
module Bag = Bag
module Binary_cache = Binary_cache
module Bitvector = Bitvector
module Channel = Channel
module Command = Command
module Compression = Compression
module Dotgraph = Dotgraph
module Escape = Escape
module Filepath = Filepath
module Filesystem = Filesystem
module Hook = Hook
module Hpath = Hpath
module Hptmap = Hptmap
module Hptmap_sig = Hptmap_sig
module Hptset = Hptset
module Json = Json
module Log = Log
module Markdown = Markdown
module Parray = Parray
module Pretty_utils = Pretty_utils
module Qstack = Qstack
module Rangemap = Rangemap
module Rich_text = Rich_text
module Sanitizer = Sanitizer
module Task = Task
module Unicode = Unicode
module Utf8_logic = Utf8_logic
module Vector = Vector
module Wto = Wto
