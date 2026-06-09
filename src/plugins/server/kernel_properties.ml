(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Md = Markdown
module Pkg = Package

open Data
open Kernel_ast

let package = Pkg.package ~title:"Property Services" ~name:"properties" ()

(* -------------------------------------------------------------------------- *)
(* --- Property Kind                                                      --- *)
(* -------------------------------------------------------------------------- *)

module PropKind =
struct
  let kinds = Enum.dictionary ()

  let t_kind name descr =
    Enum.tag ~name ~label:(Md.plain name) ~descr:(Md.plain descr) kinds

  let t_clause name =
    t_kind name (Printf.sprintf "Clause `@%s`" name)

  let t_loop name =
    t_kind ("loop_" ^ name) (Printf.sprintf "Clause `@loop %s`" name)

  let t_behavior = t_kind "behavior" "Contract behavior"
  let t_complete = t_kind "complete" "Complete behaviors clause"
  let t_disjoint = t_kind "disjoint" "Disjoint behaviors clause"

  let t_assumes = t_clause "assumes"
  let t_requires = t_kind "requires" "Function precondition"
  let t_instance = t_kind "instance" "Instance of a precondition at a call site"
  let t_breaks = t_clause "breaks"
  let t_continues = t_clause "continues"
  let t_returns = t_clause "returns"
  let t_exits = t_clause "exits"
  let t_ensures = t_kind "ensures" "Function postcondition"
  let t_terminates = t_kind "terminates" "Function termination clause"
  let t_allocates = t_kind "allocates" "Function allocation"
  let t_decreases = t_clause "decreases"
  let t_assigns = t_kind "assigns" "Function assigns"
  let t_froms = t_kind "froms" "Functional dependencies in function assigns"

  let t_assert = t_kind "assert" "Assertion"
  let t_check = t_kind "check" "Check"
  let t_admit = t_kind "admit" "Hypothesis"
  let t_loop_invariant = t_loop "invariant"
  let t_loop_assigns = t_loop "assigns"
  let t_loop_variant = t_loop "variant"
  let t_loop_allocates = t_loop "allocates"

  let t_reachable = t_kind "reachable" "Reachable statement"
  let t_code_contract = t_kind "code_contract" "Statement contract"
  let t_code_invariant = t_kind "code_invariant" "Generalized loop invariant"
  let t_type_invariant = t_kind "type_invariant" "Type invariant"
  let t_global_invariant = t_kind "global_invariant" "Global invariant"

  let t_axiomatic = t_kind "axiomatic" "Axiomatic definitions"
  let t_module = t_kind "module" "Logic module"
  let t_axiom = t_kind "axiom" "Logical axiom"
  let t_lemma = t_kind "lemma" "Logical lemma"
  let t_check_lemma = t_kind "check_lemma" "Logical check lemma"

  let t_ext = t_kind "extension" "ACSL extension"
  let t_other = t_kind "generic" "Generic Property"

  open Property

  let lookup = function
    | IPPredicate { ip_kind } ->
      begin match ip_kind with
        | PKRequires _ -> t_requires
        | PKAssumes _ -> t_assumes
        | PKEnsures(_,Normal) -> t_ensures
        | PKEnsures(_,Exits) -> t_exits
        | PKEnsures(_,Breaks) -> t_breaks
        | PKEnsures(_,Continues) -> t_continues
        | PKEnsures(_,Returns) -> t_returns
        | PKTerminates -> t_terminates
      end
    | IPExtended { ie_ext={ ext_name=_ } } -> t_ext
    | IPAxiomatic _ -> t_axiomatic
    | IPModule _ -> t_module
    | IPLemma { il_pred = { tp_kind = Admit } } -> t_axiom
    | IPLemma { il_pred = { tp_kind = Assert } } -> t_lemma
    | IPLemma { il_pred = { tp_kind = Check } } -> t_check_lemma
    | IPBehavior _ -> t_behavior
    | IPComplete _ -> t_complete
    | IPDisjoint _ -> t_disjoint
    | IPCodeAnnot { ica_ca={ annot_content } } ->
      begin match annot_content with
        | AAssert (_, {tp_kind = Assert}) -> t_assert
        | AAssert (_, {tp_kind = Check }) -> t_check
        | AAssert (_, {tp_kind = Admit }) -> t_admit
        | AStmtSpec _ -> t_code_contract
        | AInvariant(_,false,_) -> t_code_invariant
        | AInvariant(_,true,_) -> t_loop_invariant
        | AVariant _ -> t_loop_variant
        | AAssigns _ -> t_loop_assigns
        | AAllocation _ -> t_loop_allocates
        | AExtended(_,_,{ext_name=_}) -> t_ext
      end
    | IPAllocation _ -> t_allocates
    | IPAssigns _ -> t_assigns
    | IPFrom _ -> t_froms
    | IPDecrease _ -> t_decreases
    | IPReachable _ -> t_reachable
    | IPPropertyInstance _ -> t_instance
    | IPTypeInvariant _ -> t_type_invariant
    | IPGlobalInvariant _ -> t_global_invariant
    | IPOther _ -> t_other

  let () = Enum.set_lookup kinds lookup
  let data = Request.dictionary ~package
      ~name:"propKind"
      ~descr:(Md.plain "Property Kinds")
      kinds

  include (val data : S with type t = Property.t)
end

(* -------------------------------------------------------------------------- *)
(* --- Property Status                                                    --- *)
(* -------------------------------------------------------------------------- *)

module PropStatus =
struct

  let status = Enum.dictionary ()

  let t_status value name ?label descr =
    Enum.tag ~name
      ?label:(Option.map Md.plain label)
      ~descr:(Md.plain descr) ~value status

  open Property_status.Feedback

  let t_unknown =
    t_status Unknown "unknown" "Unknown status"
  let t_never_tried =
    t_status Never_tried "never_tried"
      ~label:"Never tried" "Unknown status (never tried)"
  let t_inconsistent =
    t_status Inconsistent "inconsistent" "Inconsistent status"
  let t_valid =
    t_status Valid "valid" "Valid property"
  let t_valid_under_hyp =
    t_status Valid_under_hyp "valid_under_hyp"
      ~label:"Valid (?)" "Valid (under hypotheses)"
  let t_considered_valid =
    t_status Considered_valid "considered_valid"
      ~label:"Valid (!)" "Valid (external assumption)"
  let t_invalid =
    t_status Invalid "invalid" "Invalid property (counter example found)"
  let t_invalid_under_hyp =
    t_status Invalid_under_hyp "invalid_under_hyp"
      ~label:"Invalid (?)" "Invalid property (under hypotheses)"
  let t_invalid_but_dead =
    t_status Invalid_but_dead "invalid_but_dead"
      ~label:"Invalid (✝)" "Dead property (but invalid)"
  let t_valid_but_dead =
    t_status Valid_but_dead "valid_but_dead"
      ~label:"Valid (✝)" "Dead property (but valid)"
  let t_unknown_but_dead =
    t_status Unknown_but_dead "unknown_but_dead"
      ~label:"Unknown (✝)" "Dead property (but unknown)"

  let () = Enum.set_lookup status
      begin function
        | Valid -> t_valid
        | Invalid -> t_invalid
        | Unknown -> t_unknown
        | Never_tried -> t_never_tried
        | Valid_under_hyp -> t_valid_under_hyp
        | Valid_but_dead -> t_valid_but_dead
        | Considered_valid -> t_considered_valid
        | Invalid_under_hyp -> t_invalid_under_hyp
        | Invalid_but_dead -> t_invalid_but_dead
        | Unknown_but_dead -> t_unknown_but_dead
        | Inconsistent -> t_inconsistent
      end

  let data = Request.dictionary ~package ~name:"propStatus"
      ~descr:(Md.plain "Property Status (consolidated)") status

  include (val data : S with type t = Property_status.Feedback.t)
end

(* -------------------------------------------------------------------------- *)
(* --- Alarm kind                                                         --- *)
(* -------------------------------------------------------------------------- *)

[@@@ warning "-60"]
module AlarmKind =
struct

  let alarms = Enum.dictionary ()

  let register alarm =
    let name = Alarms.get_short_name alarm in
    let label = Md.plain name in
    let descr = Md.plain (Alarms.get_description alarm) in
    Enum.add alarms ~name ~label ~descr

  let () = List.iter register Alarms.reprs

  let () = Enum.set_lookup alarms
      begin function alarm ->
        let name = Alarms.get_short_name alarm in
        try Enum.find_tag alarms name
        with Not_found -> failure "Unknown alarm kind: %s" name
      end

  let data = Request.dictionary
      ~package ~name:"alarms" ~descr:(Md.plain "Alarm Kinds") alarms

  include (val data : S with type t = Alarms.t)
end
[@@@ warning "+60"]

(* -------------------------------------------------------------------------- *)
(* --- Property Model                                                     --- *)
(* -------------------------------------------------------------------------- *)

let find_alarm = function
  | Property.IPCodeAnnot annot -> Alarms.find annot.ica_ca
  | _ -> None

let is_libc ip =
  match Property.source ip with
  | None -> false
  | Some position ->
    let libc_path = Kernel.Share.get_dir "libc" in
    Filepath.is_relative ~base:libc_path (Filepos.path position)

let model = States.model ()

let () = States.column model ~name:"descr"
    ~descr:(Md.plain "Full description")
    ~data:(module Jstring)
    ~get:(fun ip -> Format.asprintf "%a" Property.pretty ip)

let () = States.column model ~name:"kind"
    ~descr:(Md.plain "Kind")
    ~data:(module PropKind)
    ~get:(fun ip -> ip)

let () = States.column model ~name:"names"
    ~descr:(Md.plain "Names")
    ~data:(module Jlist(Jstring))
    ~get:Property.get_names

let () = States.column model ~name:"status"
    ~descr:(Md.plain "Status")
    ~data:(module PropStatus)
    ~get:
      begin fun ip ->
        if Property.has_status ip
        then Property_status.Feedback.get ip
        else Property_status.Feedback.Never_tried
      end


let () = States.option model ~name:"scope"
    ~descr:(Md.plain "Declaration Scope")
    ~data:(module Decl) ~get:Printer_tag.declaration_of_property

let () = States.column model ~name:"kinstr"
    ~descr:(Md.plain "Instruction")
    ~data:(module Kinstr) ~get:Property.get_kinstr

let () = States.column model ~name:"source"
    ~descr:(Md.plain "Position")
    ~data:(module Kernel_ast.Position)
    ~get:(fun ip -> Property.location ip |> fst)

let () = States.column model ~name:"from_libc"
    ~descr:(Md.plain "Is the property from the Frama-C libc?")
    ~data:(module Jbool)
    ~get:is_libc

let () = States.column model ~name:"alarm"
    ~descr:(Md.plain "Alarm name (if the property is an alarm)")
    ~data:(module Joption(Jstring))
    ~get:(fun ip -> Option.map Alarms.get_short_name (find_alarm ip))

let () = States.column model ~name:"alarm_descr"
    ~descr:(Md.plain "Alarm description (if the property is an alarm)")
    ~data:(module Joption(Jstring))
    ~get:(fun ip -> Option.map Alarms.get_description (find_alarm ip))

let () = States.column model ~name:"predicate"
    ~descr:(Md.plain "Predicate")
    ~data:(module Joption(Jstring))
    ~get:(fun ip -> Option.map snd (Description.property_kind_and_node ip))

let is_relevant ip =
  match Property.get_kf ip with
  | None -> true
  | Some kf ->
    not (Ast_info.start_with_frama_c_builtin (Kernel_function.get_name kf)
         || Cil_builtins.is_unused_builtin (Kernel_function.get_vi kf))

let iter f =
  Property_status.iter (fun ip -> if is_relevant ip then f ip)

(* Must reload the entire table when status changed: property dependencies
   are not taken into account when status are updated. *)
let add_reload_hook (f : unit -> unit) : unit =
  Property_status.register_status_update_hook
    (fun _emitter _ip _status -> f());
  Project.register_after_set_current_hook ~user_only:false
    (fun _project -> f ())

let add_update_hook (f : Property.t -> unit) : unit =
  Property_status.register_property_add_hook
    (fun ip -> if is_relevant ip then f ip)

let add_remove_hook (f : Property.t -> unit) : unit =
  Property_status.register_property_remove_hook
    (fun ip -> if is_relevant ip then f ip)

let array =
  States.register_array
    ~package
    ~name:"status"
    ~descr:(Md.plain "Status of Registered Properties")
    ~key:(fun ip -> Kernel_ast.Marker.index (PIP ip))
    ~keyType:Kernel_ast.Marker.jtype
    ~iter
    ~add_reload_hook
    ~add_remove_hook
    ~add_update_hook
    model

let reload () = States.reload array

(* -------------------------------------------------------------------------- *)
