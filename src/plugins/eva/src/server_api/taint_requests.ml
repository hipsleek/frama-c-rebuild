(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Server requests about taint analysis. *)

open Server
open Cil_types

let package =
  Package.package ~plugin:"eva" ~name:"taint" ~title:"Taint Analysis" ()


(* ----- Taint names -------------------------------------------------------- *)

let _signal =
  States.register_value ~package
    ~name:"taintNames"
    ~descr:(Markdown.plain "List of taint names")
    ~output:(module Data.Jlist (Data.Jstring))
    ~get:Taint_domain.taint_names
    ~add_hook:Analysis_requests.register_computation_hook
    ()

(* List of currently selected taints. *)
module CurrentTaints = struct
  module Info = struct
    let name = "Eva.Taint_requests.CurrentTaints"
    let dependencies = [ Self.state ]
    let default = Taint_domain.taint_names
  end

  include State_builder.Ref (Datatype.List (Datatype.String)) (Info)
end

(* At the end of an analysis, select all taint names by default. *)
let () =
  Self.ComputationState.add_hook_on_change
    (fun _ -> CurrentTaints.set (Taint_domain.taint_names ()))

let current_taint_signal =
  States.register_framac_state ~package
    ~name:"currentTaints"
    ~descr:(Markdown.plain "Names of the currently selected taints, if any")
    ~data:(module Data.Jlist (Data.Jstring))
    (module CurrentTaints)

(* Takes into account the currently selected taints. *)
let is_tainted zone request =
  match CurrentTaints.get () with
  | [] -> Ok Results.Untainted
  | names -> Results.is_tainted ~names zone request

let taint_names_by_kind zone request =
  let open Option.Operators in
  match CurrentTaints.get () with
  | [] ->
    let empty = Datatype.String.Set.empty in
    Some Results.{ direct_taint_names = empty; indirect_taint_names = empty }
  | current_names ->
    let+ names = Results.taint_names_by_kind zone request |> Result.to_option in
    let selected = Datatype.String.Set.of_list current_names in
    let restrict = Datatype.String.Set.inter selected in
    let direct_taint_names = restrict names.direct_taint_names in
    let indirect_taint_names = restrict names.indirect_taint_names in
    Results.{ direct_taint_names; indirect_taint_names }

let register_hook f =
  Analysis_requests.register_computation_hook f;
  CurrentTaints.add_hook_on_change (fun _ -> f ())

(* ----- Taint statuses ----------------------------------------------------- *)

type taint = Results.taint = Direct | Indirect | Untainted
type error = NotComputed | Irrelevant | LogicError

module TaintStatus = struct
  open Server.Data

  let dictionary = Enum.dictionary ()

  let tag value name label short_descr long_descr =
    Enum.tag ~name
      ~label:(Markdown.plain label)
      ~descr:(Markdown.bold (short_descr ^ ": ") @ Markdown.plain long_descr)
      ~value dictionary

  let tag_not_computed =
    tag (Error NotComputed) "not_computed" "" "Not computed"
      "the Eva taint domain has not been enabled, \
       or the Eva analysis has not been run"

  let tag_error =
    tag (Error LogicError) "error" "Error" "Error"
      "the memory zone on which this property depends could not be computed"

  let tag_not_applicable =
    tag (Error Irrelevant) "not_applicable" "—" "Not applicable"
      "no taint for this kind of property"

  let tag_direct_taint =
    tag (Ok Direct) "direct_taint" "Tainted (direct)" "Direct taint"
      "this property is related to a memory location that can be affected \
       by an attacker"

  let tag_indirect_taint =
    tag (Ok Indirect) "indirect_taint" "Tainted (indirect)" "Indirect taint"
      "this property is related to a memory location whose assignment depends \
       on path conditions that can be affected by an attacker"

  let tag_untainted =
    tag (Ok Untainted) "not_tainted" "Untainted" "Untainted property"
      "this property is safe"

  let () = Enum.set_lookup dictionary @@ function
    | Error NotComputed -> tag_not_computed
    | Error Irrelevant -> tag_not_applicable
    | Error LogicError -> tag_error
    | Ok Direct -> tag_direct_taint
    | Ok Indirect -> tag_indirect_taint
    | Ok Untainted -> tag_untainted

  let data = Request.dictionary ~package ~name:"taintStatus"
      ~descr:(Markdown.plain "Taint status of logical properties") dictionary

  include (val data : S with type t = (taint, error) result)
end


(* ----- Register Eva taints information ------------------------------------ *)

let expr_of_lval v = Cil.new_exp ~loc:Fileloc.unknown (Lval v)

let term_lval_to_lval kf tlval =
  try
    let result = Option.bind Eva_utils.find_return_var kf in
    Logic_to_c.term_lval_to_lval ?result tlval
  with Logic_to_c.No_conversion -> raise Not_found

module EvaTaints = struct

  let evaluate expr request =
    let open Option.Operators in
    let Deps.{ data } = Results.expr_dependencies expr request in
    let* taint = is_tainted data request |> Result.to_option in
    let* names = taint_names_by_kind data request in
    Some (taint, names)

  let expr_of_marker = let open Printer_tag in function
      | PLval (_, Kstmt stmt, lval) -> Some (expr_of_lval lval, stmt)
      | PExp (_, Kstmt stmt, expr) -> Some (expr, stmt)
      | PVDecl (_, Kstmt stmt, vi) -> Some (expr_of_lval (Var vi, NoOffset), stmt)
      | PTermLval (kf, Kstmt stmt, _, tlval) ->
        Some (term_lval_to_lval kf tlval |> expr_of_lval, stmt)
      | _ -> None

  let of_marker marker =
    let open Option.Operators in
    let* expr, stmt = expr_of_marker marker in
    let* before = evaluate expr (Results.before stmt) in
    let* after  = evaluate expr (Results.after  stmt) in
    Some (before, after)

  let to_string taint Results.{ direct_taint_names; indirect_taint_names } =
    match taint with
    | Untainted ->
      "untainted"
    | Indirect ->
      Format.asprintf "indirect taint @[<h>%a@]"
        Datatype.String.Set.pretty indirect_taint_names
    | Direct when Datatype.String.Set.is_empty indirect_taint_names ->
      Format.asprintf "direct taint @[<h>%a@]"
        Datatype.String.Set.pretty direct_taint_names
    | Direct ->
      Format.asprintf "direct taint @[<h>%a@], indirect taint @[<h>%a@]"
        Datatype.String.Set.pretty direct_taint_names
        Datatype.String.Set.pretty indirect_taint_names

  let pp fmt = fun (taint, taint_names) ->
    Format.fprintf fmt "%s" (to_string taint taint_names)

  let print_taint fmt marker =
    match of_marker marker with
    | None -> raise Not_found
    | Some (before, after) ->
      if before = after
      then Format.fprintf fmt "%a" pp before
      else Format.fprintf fmt "Before: %a@\nAfter: %a" pp before pp after

  let eva_taints_descr =
    "Taint status:\n\
     - Direct taint: data dependency from values provided by the attacker, \
     meaning that the attacker may be able to alter this value\n\
     - Indirect taint: the attacker cannot directly alter this value, but he \
     may be able to impact the path by which its value is computed.\n\
     - Untainted: cannot be modified by the attacker."

  let () =
    let taint_computed = Taint_domain.Store.is_computed in
    let enable () = Analysis.is_computed () && taint_computed () in
    Server.Kernel_ast.Information.register
      ~id:"eva.taint" ~label:"Taint" ~title:"Taint status according to Eva"
      ~descr:eva_taints_descr ~enable print_taint

  let () = register_hook Server.Kernel_ast.Information.update
end


(* ----- Tainted lvalues ---------------------------------------------------- *)

module LvalueTaints = struct
  module Table = Cil_datatype.Lval.Hashtbl

  module Status = struct
    type record
    let record: record Data.Record.signature = Data.Record.signature ()
    let field name d = Data.Record.field record ~name ~descr:(Markdown.plain d)
    let lval_field = field "lval" "tainted lvalue" (module Kernel_ast.Lval)
    let taint_field = field "taint" "taint status" (module TaintStatus)
    let name, descr = "LvalueTaints", Markdown.plain "Lvalue taint status"
    let publication = Data.Record.publish record ~package ~name ~descr
    include (val publication: Data.Record.S with type r = record)
    let create lval taint = set lval_field lval @@ set taint_field taint default
  end

  let current_project () = Visitor_behavior.inplace ()
  class tainted_lvalues taints = object (self)
    inherit Visitor.generic_frama_c_visitor (current_project ())
    method! vlval lval =
      let expr = expr_of_lval lval in
      match self#current_stmt with
      | None -> Cil.DoChildren
      | Some stmt ->
        match Results.after stmt |> EvaTaints.evaluate expr with
        | Some (Results.Untainted, _) -> DoChildren
        | Some (t, _) -> Table.add taints lval (Kstmt stmt, t) ; Cil.DoChildren
        | None -> Cil.DoChildren
  end

  let get_tainted_lvals kf =
    try
      let fn = Kernel_function.get_definition kf in
      let taints = Table.create 17 in
      Visitor.visitFramacFunction (new tainted_lvalues taints) fn |> ignore ;
      let fn lval (ki, taint) acc = Status.create (ki, lval) (Ok taint) :: acc in
      Table.fold fn taints [] |> List.rev
    with Kernel_function.No_Definition -> []

  let () = Request.register ~package ~kind:`GET ~name:"taintedLvalues"
      ~descr:(Markdown.plain "Get the tainted lvalues of a given function")
      ~input:(module (Kernel_ast.Decl))
      ~output:(module (Data.Jlist (Status)))
      ~signals:[Analysis_requests.computation_signal; current_taint_signal]
      (function SFunction kf -> get_tainted_lvals kf | _ -> [])

end


(* ----- Tainted properties ------------------------------------------------- *)

let zone_of_predicate kinstr predicate =
  let state = Results.(before_kinstr kinstr |> get_cvalue_model) in
  let env = Eval_terms.env_only_here state in
  let logic_deps = Eval_terms.predicate_deps env predicate in
  match Option.map Cil_datatype.Logic_label.Map.bindings logic_deps with
  | Some [ BuiltinLabel Here, zone ] -> Ok zone
  | _ -> Error LogicError

let get_predicate = function
  | Property.IPCodeAnnot ica ->
    begin
      match ica.ica_ca.annot_content with
      | AAssert (_, predicate) | AInvariant (_, _, predicate) ->
        Ok predicate.tp_statement
      | _ -> Error Irrelevant
    end
  | IPPropertyInstance { ii_pred = None } -> Error LogicError
  | IPPropertyInstance { ii_pred = Some ip } -> Ok ip.ip_content.tp_statement
  | _ -> Error Irrelevant

let is_tainted_property ip =
  if Analysis.is_computed () && Taint_domain.Store.is_computed () then
    let (let+) = Result.bind in
    let kinstr = Property.get_kinstr ip in
    let+ predicate = get_predicate ip in
    let+ zone = zone_of_predicate kinstr predicate in
    let result = Results.before_kinstr kinstr |> is_tainted zone in
    Result.map_error (fun _ -> NotComputed) result
  else Error NotComputed
