(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Server requests about information inferred by Eva on AST elements:
    - callers and callees of functions;
    - unreachable functions and statements;
    - high-priority and tainted logic properties.
*)

open Server
open Cil_types

let package =
  let title = "Information from Eva about AST elements" in
  Package.package ~plugin:"eva" ~name:"ast" ~title ()


(* ----- Callers & Callees -------------------------------------------------- *)

module CallSite =
struct
  type t = kernel_function * stmt
  let jtype = Data.declare ~package ~name:"CallSite"
      ~descr:(Markdown.plain "Callee function and caller stmt")
      (Jrecord [
          "call", Kernel_ast.Decl.jtype;
          "stmt", Kernel_ast.Stmt.jtype;
        ])
  let to_json (kf,stmt) = `Assoc [
      "call", Kernel_ast.Decl.to_json (SFunction kf);
      "stmt", Kernel_ast.Stmt.to_json stmt;
    ]
  let of_json _ = failwith "CallSite"
end

let callers = function
  | Printer_tag.SFunction kf ->
    let list = Results.callsites kf in
    List.concat (List.map (fun (kf, l) -> List.map (fun s -> kf, s) l) list)
  | _ -> []

let () = Request.register ~package
    ~kind:`GET ~name:"getCallers"
    ~descr:(Markdown.plain "Get the list of call sites for a function")
    ~input:(module Kernel_ast.Decl) ~output:(module Data.Jlist (CallSite))
    ~signals:[Analysis_requests.computation_signal]
    callers

let eval_callee stmt f =
  Results.(before stmt |> eval_callee f |> default [])

let callees = function
  | Printer_tag.PLval (_kf, Kstmt stmt, (Mem _, NoOffset as lval))
    when Cil.(Ast_types.is_fun (typeOfLval lval)) ->
    List.map (fun kf -> Printer_tag.SFunction kf) @@
    eval_callee stmt (fst lval)
  | Printer_tag.PLval (_kf, Kstmt stmt, lval)
    when Ast_types.is_fun_ptr (Cil.typeOfLval lval) ->
    List.map (fun kf -> Printer_tag.SFunction kf) @@
    eval_callee stmt (Mem (Eva_utils.lval_to_exp lval))
  | _ -> []

let () = Request.register ~package
    ~kind:`GET ~name:"getCallees"
    ~descr:(Markdown.plain
              "Return the functions pointed to by a function pointer")
    ~input:(module Kernel_ast.Marker)
    ~output:(module Data.Jlist(Kernel_ast.Decl))
    ~signals:[Analysis_requests.computation_signal]
    callees


(* ----- Functions ---------------------------------------------------------- *)

let () =
  Kernel_ast.register_fct_filter "eva_analyzed"
    ~labels:("functions analyzed by Eva",
             "functions unreached by Eva")
    ~enable:Analysis.is_computed
    ~add_hook:(fun f -> Analysis.register_computation_hook (fun _ -> f ()))
    Results.is_called


(* ----- Dead code: unreachable and non-terminating statements -------------- *)

type dead_code =
  { kf: Kernel_function.t;
    reached : stmt list;
    unreachable : stmt list;
    non_terminating : stmt list; }

module DeadCode = struct
  open Server.Data

  type record
  let record : record Record.signature = Record.signature ()

  let reached = Record.field record ~name:"reached"
      ~descr:(Markdown.plain "List of statements reached by the analysis.")
      (module Data.Jlist (Kernel_ast.Marker))

  let unreachable = Record.field record ~name:"unreachable"
      ~descr:(Markdown.plain "List of unreachable statements.")
      (module Data.Jlist (Kernel_ast.Marker))

  let non_terminating = Record.field record ~name:"nonTerminating"
      ~descr:(Markdown.plain "List of reachable but non terminating statements.")
      (module Data.Jlist (Kernel_ast.Marker))

  let data = Record.publish record ~package ~name:"deadCode"
      ~descr:(Markdown.plain "Unreachable and non terminating statements.")

  module R : Record.S with type r = record = (val data)
  type t = dead_code
  let jtype = R.jtype

  let to_json dead_code =
    let make_stmt stmt = Printer_tag.PStmt (dead_code.kf, stmt) in
    let make_non_term stmt = Printer_tag.PStmtStart (dead_code.kf, stmt) in
    R.default |>
    R.set reached (List.map make_stmt dead_code.reached) |>
    R.set unreachable (List.map make_stmt dead_code.unreachable) |>
    R.set non_terminating (List.map make_non_term dead_code.non_terminating) |>
    R.to_json

  let of_json _ = Data.failure "DeadCode.of_json not implemented"
end

let all_statements kf =
  try (Kernel_function.get_definition kf).sallstmts
  with Kernel_function.No_Definition -> []

let dead_code = function
  | Printer_tag.SFunction kf ->
    let empty = { kf; reached = []; unreachable = []; non_terminating = [] } in
    let record =
      if Analysis.is_computed () then
        let body = all_statements kf in
        match Analysis.status kf with
        | Unreachable | SpecUsed | Builtin _ -> { empty with unreachable = body }
        | Analyzed NoResults -> empty
        | Analyzed (Partial | Complete) ->
          let classify { kf ; reached ; unreachable ; non_terminating = nt } stmt =
            let before = Results.(before stmt |> is_empty) in
            let after = Results.(after stmt |> is_empty) in
            let unreachable = if before then stmt :: unreachable else unreachable in
            let reached = if not before then stmt :: reached else reached in
            let non_terminating = if not before && after then stmt :: nt else nt in
            { kf ; reached ; unreachable ; non_terminating }
          in
          List.fold_left classify empty body
      else empty
    in
    Some record
  | _ -> None

let () = Request.register ~package
    ~kind:`GET ~name:"getDeadCode"
    ~descr:(Markdown.plain
              "Get the lists of unreachable and of non terminating \
               statements in a function")
    ~input:(module Kernel_ast.Decl)
    ~output:(module Data.Joption (DeadCode))
    ~signals:[Analysis_requests.computation_signal]
    dead_code


(* ----- Red and tainted alarms --------------------------------------------- *)

let () =
  let model = States.model () in
  let descr = "Is the property invalid in some context of the analysis?" in
  States.column model
    ~name:"priority"
    ~descr:(Markdown.plain descr)
    ~data:(module Data.Jbool)
    ~get:Red_statuses.is_red ;
  let descr = "Is the property tainted according to the Eva taint domain?" in
  States.column model
    ~name:"taint"
    ~descr:(Markdown.plain descr)
    ~data:(module Taint_requests.TaintStatus)
    ~get:Taint_requests.is_tainted_property ;
  let add_update_hook hook =
    Red_statuses.register_hook (function Prop p -> hook p | Alarm _ -> ())
  in
  ignore @@ States.register_array
    ~package
    ~name:"properties"
    ~descr:(Markdown.plain "Status of Registered Properties")
    ~key:(fun ip -> Kernel_ast.Marker.index (PIP ip))
    ~keyType:Kernel_ast.Marker.jtype
    ~iter:Property_status.iter
    ~add_update_hook
    ~add_reload_hook:Taint_requests.register_hook
    model
