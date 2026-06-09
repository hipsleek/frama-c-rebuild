(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Statistics are stored in a dictionary, implemented as two hashtables from
   keys to integers and floating-point numbers respectively.

   [Key] is the representation of the dictionary keys: a couple of a registered
   statistic (type [('k,'ty) t]) accompanied by the function or the statement
   the stat is about (kind ['k], type ['ty]).

   Statistics must be registered before usage. The registry keeps track of the
   registered statistics and allow the reloading of projects by matching the
   previous stats to the current ones.
*)

type ('a,'b) cmp = Eq : ('a, 'a) cmp | Neq : ('a, 'b) cmp

(* --- Kind --- *)

type _ kind =
  | Global : unit kind
  | Function : Cil_types.kernel_function kind
  | Statement : Cil_types.stmt kind

let equal_kind (type a) (type b) (k1 : a kind) (k2 : b kind) : (a, b) cmp =
  match k1, k2 with
  | Global, Global -> Eq
  | Function, Function -> Eq
  | Statement, Statement -> Eq
  | _, _ -> Neq


(* --- Type --- *)

type _ typ =
  | Int : int typ
  | Float : float typ

let equal_ty (type a) (type b) (t1 : a typ) (t2 : b typ) : (a, b) cmp =
  match t1, t2 with
  | Int, Int -> Eq
  | Float, Float -> Eq
  | _, _ -> Neq


(* Statistics keys *)

type ('k,'ty) t = {
  id: int;
  name: string;
  kind: 'k kind;
  ty: 'ty typ;
}


(* --- Registry --- *)

type registered_stat = Registered : ('k, 'v) t -> registered_stat [@@unboxed]

let registry = Hashtbl.create 13
let last_id = ref 0

let register
    (type k ty) (name : string) (kind : k kind) (ty : ty typ) : (k, ty) t =
  try
    (* If the stat is already registered, return the previous one *)
    let Registered stat = Hashtbl.find registry name in
    match equal_kind stat.kind kind, equal_ty stat.ty ty with
    | Eq, Eq -> stat
    | Neq, _ | _, Neq ->
      Self.fatal
        "statistic \"%s\" was already registered with a different type or kind"
        name
  with Not_found ->
    (* Otherwise, create a new record for the stat *)
    incr last_id;
    let stat = { id = !last_id; name; kind; ty } in
    Hashtbl.add registry name (Registered stat);
    stat

let register_global_stat name =
  register name Global

let register_function_stat name =
  register name Function

let register_statement_stat name =
  register name Statement


(* --- Keys --- *)

type key = Key : ('k,'ty) t * 'k -> key

module Key = struct

  type cmp = { cmp : 'k1 'k2 'ty1 'ty2. ('k1, 'ty1) t -> ('k2, 'ty2) t -> int }

  let compare_key (cmp: cmp) (Key (s1, x1)) (Key (s2, x2)) =
    let c = cmp.cmp s1 s2 in
    if c <> 0 then c else
      match s1.kind, s2.kind with
      | Global, Global -> 0
      | Function, Function -> Kernel_function.compare x1 x2
      | Statement, Statement -> Cil_datatype.Stmt.compare x1 x2
      | Global, (Function | Statement) -> -1
      | (Function | Statement), Global -> 1
      | Function, Statement -> -1
      | Statement, Function -> 1

  (* Optimized comparison, using the key id. *)
  let compare_opt =
    compare_key { cmp = fun s1 s2 -> s1.id - s2.id }

  (* Lexicographical comparison, using the key name. *)
  let compare_lex =
    compare_key { cmp = fun s1 s2 -> String.compare s1.name s2.name }

  let hash_key (Key (s, x)) =
    let h = match s.kind with
      | Global -> 0
      | Function -> Kernel_function.hash x
      | Statement -> Cil_datatype.Stmt.hash x
    in
    Hashtbl.hash (s.id, h)

  let pretty_key fmt (Key (s, x)) =
    match s.kind with
    | Global ->
      Format.fprintf fmt "%s" s.name
    | Function ->
      Format.fprintf fmt "%s:%a" s.name Kernel_function.pretty x
    | Statement ->
      Format.fprintf fmt "%s:%a" s.name Fileloc.pretty (Cil_datatype.Stmt.loc x)

  module Prototype = struct
    include Datatype.Serializable_undefined
    type t = key
    let name = "Statistics.Key"
    let reprs =
      [ Key ({ id = 0; name="dummy"; kind=Global; ty=Int }, ())]
    let compare = compare_opt
    let equal = Datatype.from_compare
    let hash = hash_key
    let pretty = pretty_key
    let copy k = k
    let rehash (Key (s, x)) = (Key (register s.name s.kind s.ty, x))
  end

  include Datatype.Make_with_collections (Prototype)

  let name (Key (s, _x)) = s.name

  let pretty_kf fmt (Key (s, x)) =
    match s.kind with
    | Global -> ()
    | Function -> Kernel_function.pretty fmt x
    | Statement -> Kernel_function.(pretty fmt (find_englobing_kf x))

  let pretty_stmt fmt (Key (s, x)) =
    match s.kind with
    | Global | Function -> ()
    | Statement -> Fileloc.pretty fmt (Cil_datatype.Stmt.loc x)
end

(* --- Projectified state --- *)

module IntState =
  State_builder.Hashtbl
    (Key.Hashtbl)
    (Datatype.Int)
    (struct
      let name = "Eva.Statistics.IntState"
      let dependencies = [ Self.state ]
      let size = 17
    end)

module FloatState =
  State_builder.Hashtbl
    (Key.Hashtbl)
    (Datatype.Float)
    (struct
      let name = "Eva.Statistics.FloatState"
      let dependencies = [ Self.state ]
      let size = 17
    end)


(* --- Statistics retrieval --- *)

let get (type k ty) (stat : (k, ty) t) (x : k) : ty =
  let key = Key (stat, x) in
  match stat.ty with
  | Int ->
    IntState.find_opt key
    |> Option.value ~default:0
  | Float ->
    FloatState.find_opt key
    |> Option.value ~default:0.0


(* --- Statistics update --- *)

let set (type k ty) (stat : (k, ty) t) (x : k) (value : ty) =
  let k = Key (stat, x) in
  match stat.ty with
  | Int ->
    IntState.replace k value
  | Float ->
    FloatState.replace k value

let update (type k ty) (stat : (k, ty) t) (x : k) (f : ty -> ty) =
  let k = Key (stat, x) in
  match stat.ty with
  | Int ->
    IntState.replace k (f (get stat x))
  | Float ->
    FloatState.replace k (f (get stat x))

let incr (type k) (stat : (k, int) t) (x : k) =
  update stat x Int.succ

let add (type k ty) (stat : (k, ty) t) (x : k) (value : ty) =
  let f : ty -> ty -> ty =
    match stat.ty with
    | Int -> Int.add
    | Float -> Float.add
  in
  update stat x (f value)

let grow (type k ty) (stat : (k, ty) t) (x : k) (value : ty) =
  let f : ty -> ty -> ty =
    match stat.ty with
    | Int -> Int.max
    | Float -> Float.max
  in
  update stat x (f value)

let reset_all () =
  IntState.clear ();
  FloatState.clear ()


(* --- Hook to compute statistics when requested --- *)

module ComputeHook = Hook.Make ()

let add_compute_hook = ComputeHook.extend


(* -- Export --- *)

type value = Value : 'ty typ * 'ty -> value

let export_as_list () =
  ComputeHook.apply ();
  let int_bindings =
    IntState.to_seq ()
    |> Seq.map (fun (k, v) -> k, Value (Int, v))
  and float_bindings =
    FloatState.to_seq ()
    |> Seq.map (fun (k, v) -> k, Value (Float, v))
  in
  Seq.append int_bindings float_bindings
  |> List.of_seq
  |> List.sort (fun (k1,_v1) (k2,_v2) -> Key.compare_lex k1 k2)

let export_as_csv_to_channel out_channel =
  let fmt = Format.formatter_of_out_channel out_channel in
  let l = export_as_list () in
  let pp_value fmt = function
    | Value (Int, x) -> Format.pp_print_int fmt x
    | Value (Float, x) -> Format.pp_print_float fmt x
  in
  let pp_stat fmt (key, value) =
    Format.fprintf fmt "%s\t%a\t%a\t%a\n"
      (Key.name key)
      Key.pretty_kf key
      Key.pretty_stmt key
      pp_value value
  in
  List.iter (pp_stat fmt) l

let export_as_csv_to_file filename =
  match Filesystem.with_open_out filename export_as_csv_to_channel with
  | Ok () -> ()
  | Error (msg, _) ->
    Self.warning "failed to output statistics: %s" msg

let export_as_csv ?filename () =
  match filename with
  | None ->
    if not (Parameters.StatisticsFile.is_empty ()) then
      let filename = Parameters.StatisticsFile.get () in
      export_as_csv_to_file filename
  | Some filename ->
    export_as_csv_to_file filename


(* Centralized statistics registration *)

let memory_usage =
  register_global_stat "memory-usage" Int
let alarm_count =
  register_global_stat "alarm-count" Int
let stmt_coverage =
  register_global_stat "stmt-coverage" Float
let fun_coverage =
  register_global_stat "fun-coverage" Float
let analysis_duration =
  register_global_stat "analysis-time" Float
let iterations =
  register_statement_stat "iterations" Int
let memexec_hits =
  register_function_stat "memexec-hits" Int
let memexec_misses =
  register_function_stat "memexec-misses" Int
let max_widenings =
  register_statement_stat "max-widenings" Int
let max_unrolling =
  register_statement_stat "max-unrolling" Int
let partitioning_index_hits =
  register_global_stat "partitioning-index-hits" Int
let partitioning_index_misses =
  register_global_stat "partitioning-index-misses" Int

(* Memory usage computation *)

let compute_memory_usage () =
  let stats = Gc.stat () and control = Gc.get () in
  let words = stats.top_heap_words + control.minor_heap_size in
  let kilobytes = words * (Sys.word_size / 8) / 1024 in
  set memory_usage () kilobytes

let () = add_compute_hook compute_memory_usage
