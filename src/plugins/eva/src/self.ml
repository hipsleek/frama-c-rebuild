(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Parses a string given to -eva-msg-key or -eva-warn-key into a list of
   message or warning category names. *)
let parse_categories s =
  let categories = String.split_on_char ',' s in
  let parse_name s =
    match String.split_on_char '=' s with
    | [] -> assert false (* split_on_char never returns an empty list *)
    | name :: _ ->
      match String.get name 0 with
      | '-' | '+' -> String.sub name 1 (String.length name - 1)
      | _ -> name
  in
  List.map parse_name categories

(* Split a category "x:y:z" into a list of subcategories "x", "x:y", "x:y:z". *)
let split_category name =
  let list = String.split_on_char ':' name in
  let concat acc elt = let acc = acc ^ elt in acc ^ ":", acc in
  snd (List.fold_left_map concat "" list)


let default_verbosity = 5
let () = Plugin.set_default_verbose_level default_verbosity

include Plugin.Register
    (struct
      let name = "Eva"
      let shortname = "eva"
      let help =
        "automatically computes variation domains for the variables of the program"
    end)

let () =
  add_plugin_output_aliases ~visible:false ~deprecated:true [ "value" ; "val" ]

let () = Verbose.set_range ~min:0 ~max:11

(* ----- Analysis state ----------------------------------------------------- *)

(* Do not add dependencies to Kernel parameters here, but at the top of
   Parameters. *)
let kernel_dependencies =
  [ Ast.self;
    Alarms.self;
    Annotations.code_annot_state; ]

let proxy = State_builder.Proxy.(create "eva" Forward kernel_dependencies)
let state = State_builder.Proxy.get proxy

(* Current state of the analysis *)
type computation_state = NotComputed | Computing | Computed | Aborted

module ComputationState =
struct
  let to_string = function
    | NotComputed -> "NotComputed"
    | Computing -> "Computing"
    | Computed -> "Computed"
    | Aborted -> "Aborted"

  module Prototype =
  struct
    include Datatype.Serializable_undefined
    type t = computation_state
    let name = "Eva.Analysis.ComputationState"
    let pretty fmt s = Format.pp_print_string fmt (to_string s)
    let reprs = [ NotComputed ; Computing ; Computed ; Aborted ]
    let dependencies = [ state ]
    let default () = NotComputed
  end

  module Datatype' = Datatype.Make (Prototype)
  include (State_builder.Ref (Datatype') (Prototype))
end

exception Abort

let is_computed () =
  match ComputationState.get () with
  | Computed | Aborted -> true
  | NotComputed | Computing -> false

let clear_results () =
  Project.clear ~selection:(State_selection.with_dependencies state) ();
  (* Explicit clear to apply hooks on changes. *)
  ComputationState.clear ()


(* ----- Verbosity configuration -------------------------------------------- *)

(* List of message and warn categories manually set by the user via -eva-msg-key
   or -eva-warn-key. Here, names of both category kinds are in the same list,
   assuming no messages and warnings share the same category name.
   In Log, enabled categories are not projectified nor saved on disk, so we
   simply use a reference. *)
let user_categories : string list ref = ref []
let add_user_category s = user_categories := s :: !user_categories

(* Avoid enabling/disabling categories set by the user. *)
let is_used_category name =
  split_category name |>
  List.exists (fun name -> List.mem name !user_categories)

(* Hook to register categories set by the user. *)
let () =
  let hook _ s = parse_categories s |> List.iter add_user_category in
  Message_category.add_set_hook hook;
  Warn_category.add_set_hook hook

module IntTbl = Hashtbl.Make (Datatype.Int)

(* Eva message category can be bound to a verbosity level, at which the key
   is automatically enabled. This table binds each verbosity level to the list
   of message keys enabled at this level. *)
let dkey_by_verbosity : category list IntTbl.t = IntTbl.create 11

(* Some Eva warning categories are feedback by default, and are bound to a
   verbosity level, as for message categories. This table binds each verbosity
   level to the list of warning keys enabled as feedback at this level. *)
let wkey_by_verbosity : warn_category list IntTbl.t = IntTbl.create 11

let register_key_verbosity tbl category level =
  assert (level >= 0 && level <= 11);
  (* No need to register keys with a verbosity level of 0,
     as they are always enabled. *)
  if level > 0 then
    let list = IntTbl.find_default ~default:[] tbl level in
    IntTbl.replace tbl level (category :: list)

(* Enable/disable message and warning categories according to -eva-verbose,
   except for categories manually set by the user. *)
let configure_verbosity () =
  let level = Verbose.get () in
  let change_message positive category =
    if not (is_used_category (dkey_name category)) then
      (if positive then add_debug_keys else del_debug_keys) category
  in
  let enable i list = List.iter (change_message (i <= level)) list in
  IntTbl.iter enable dkey_by_verbosity;
  let change_warning positive warn_category =
    if not (is_used_category (wkey_name warn_category)) then
      let status = if positive then Log.Wfeedback else Log.Winactive in
      set_warn_status warn_category status
  in
  let enable i list = List.iter (change_warning (i <= level)) list in
  IntTbl.iter enable wkey_by_verbosity

(* Makes the help message mandatory and adds an optional verbosity level. *)
let register_category ?level ~help name =
  let default = Option.fold ~none:false ~some:((>=) default_verbosity) level in
  let category = register_category ~help ~default name in
  Option.iter (register_key_verbosity dkey_by_verbosity category) level;
  category

(* Default status of warning categories: feedback is associated to a verbosity
   level. *)
type warn_default = Inactive | Feedback of int | Error

(* Makes the help message of various categories mandatory, and adds a verbosity
   level to the Feedback default status. *)
let register_warn_category ~help ?default name =
  let default, level =
    match default with
    | None -> None, None
    | Some Inactive -> Some Log.Winactive, None
    | Some Error -> Some Log.Werror, None
    | Some (Feedback level) -> Some Log.Wfeedback, Some level
  in
  let category = register_warn_category ~help ?default name in
  Option.iter (register_key_verbosity wkey_by_verbosity category) level;
  category

(* ----- Help message about categories -------------------------------------- *)

let is_domain_category name = Stdlib.String.starts_with ~prefix:"d-" name

let print_all_categories () =
  let get_info category = dkey_name category, get_category_help category in
  let list = get_all_categories () |> List.map get_info in
  let length = Stdlib.String.length in
  let max = List.fold_left (fun m (name, _) -> max m (length name)) 0 list in
  let print_one_elt fmt (name, help) =
    Format.fprintf fmt "%-*s : @[<hov>%a@]" max name Format.pp_print_text help
  in
  let is_domain (name, _) = is_domain_category name in
  let domains, others = List.partition is_domain list in
  feedback ~level:0 "@[<v>Standard Eva message categories are:@;%a@]"
    (Format.pp_print_list print_one_elt) others;
  feedback ~level:0
    "@[<v>Additional message categories for printing domain states \
     on user directives:@;%a@]"
    (Format.pp_print_list print_one_elt) domains

let print_categories_by_verbosity () =
  let pp_level level list =
    let is_no_domain c = not (is_domain_category (dkey_name c)) in
    let list = List.filter is_no_domain list in
    printf ~level:0 "  %2i: %a" level
      (Pretty_utils.pp_list ~sep:" " pp_category) list
  in
  feedback ~level:0 "Message categories by verbosity:";
  IntTbl.iter_sorted pp_level dkey_by_verbosity;
  printf ~level:0
    "-eva-verbose N automatically enables all message categories \
     with a verbosity equal to or less than N. Default to %i."
    default_verbosity

let print_categories () =
  print_all_categories ();
  print_categories_by_verbosity ();
  raise Cmdline.Exit

(* Hook to register categories set by the user. *)
let () =
  Message_category.add_set_hook
    (fun _ s -> if s = "help" then print_categories ())


(* ----- Message categories ------------------------------------------------- *)

(* Each message category is automatically enabled at a given level of verbosity:
   0: No messages.
   1: Minimal general info (starting analysis, etc) and summary.
   2: Directives given by user: Frama_C_show_each, split, etc.
   3-4: Important information about the analysis: partitioning, imprecisions…
   5: Initial and final states.
   6-8: Advanced information about automatic behaviors.
   9: Additional information such as callstacks in messages.
   10: Progress of the analysis (equivalent to -eva-show-progress).
   11: All messages (except debug messages).
*)

let dkey_show =
  register_category "show" ~level:2
    ~help:"show values/states inferred by the analysis on directives \
           such as Frama_C_show_each and Frama_C_dump_each"

let dkey_initial_state =
  register_category "initial-state" ~level:5
    ~help:"at the start of the analysis, \
           print the initial value of global variables"

let dkey_final_states =
  register_category "final-states" ~level:5
    ~help:"at the end of the analysis, print final values inferred \
           at the return point of each analyzed function "

let dkey_summary =
  register_category "summary" ~level:1
    ~help:"print a summary of the analysis at the end, including coverage \
           and alarm numbers"

let dkey_pointer_comparison =
  register_category "pointer-comparison" ~level:7
    ~help:"messages about the evaluation of pointer comparisons"

let dkey_cvalue_domain =
  register_category "d-cvalue" ~level:0
    ~help:"print states of the cvalue domain"

let dkey_iterator =
  register_category "iterator"
    ~help:"debug messages about the fixpoint engine on the control-flow graph \
           of functions"

let dkey_widening =
  register_category "widening" ~level:7
    ~help:"print a message at each point where the analysis applies a widening"

let dkey_partition =
  register_category "partition" ~level:4
    ~help:"messages about states partitioning"

let dkey_split_return =
  register_category "split-return" ~level:4
    ~help:"messages related to option -eva-split-return"

let dkey_precision_settings =
  register_category "precision-settings" ~level:3
    ~help:"messages about the automatic configuration of the analysis by \
           option -eva-precision"

let dkey_progress =
  register_category "progress" ~level:10
    ~help:"messages about the analysis progress in the C code"

let dkey_callstacks =
  register_category "callstacks" ~level:9
    ~help:"print the current callstack alongside some messages"

let dkey_callstack_hash =
  register_category "callstack-hash" ~level:9
    ~help:"additionally print the current callstack hash in some messages"

let dkey_include_string_literal =
  register_category "include-string-literals" ~level:11
    ~help:"when printing a state, \
           also include globals representing string literals"


(* ----- Warning categories ------------------------------------------------- *)

let wkey_alarm =
  register_warn_category "alarm"
    ~help:"warnings for each possible undefined behavior detected \
           by the analysis"

let wkey_locals_escaping =
  register_warn_category "locals-escaping"
    ~help:"a pointer p points to an out of scope local variable \
           (any use of p also generates an alarm)"

let _wkey_garbled_mix =
  register_warn_category "garbled-mix"
    ~help:"warnings about very imprecise values inferred for pointers, \
           named garbled mix"

let wkey_garbled_mix_write =
  register_warn_category "garbled-mix:write"
    ~help:"the interpretation of an assignment creates a garbled mix"
    ~default:(Feedback 3)

let wkey_garbled_mix_assigns =
  register_warn_category "garbled-mix:assigns"
    ~help:"the interpretation of a specification creates a garbled mix"
    ~default:(Feedback 3)

let wkey_garbled_mix_summary =
  register_warn_category "garbled-mix:summary"
    ~help:"list the origins of garbled mix at the end of an analysis"
    ~default:(Feedback 3)

let _wkey_builtins =
  register_warn_category "builtins"
    ~help:"warnings related to builtins used to interpret some libc functions"

let wkey_builtins_missing_spec =
  register_warn_category "builtins:missing-spec"
    ~help:"the ACSL specification on which a builtin soundness relies is missing"

let wkey_builtins_override =
  register_warn_category "builtins:override"
    ~help:"a builtin overrides a function definition, which is therefore \
           not analyzed"

let _wkey_libc =
  register_warn_category "libc"
    ~help:"warnings related to the interpretation of the standard C library"

let wkey_libc_unsupported_spec =
  register_warn_category "libc:unsupported-spec"
    ~help:"the ACSL specification of a libc function is not supported by Eva"

let _wkey_loop_unroll =
  register_warn_category "loop-unroll"
    ~help:"messages about loop unrolling"

let wkey_loop_unroll_auto =
  register_warn_category "loop-unroll:auto"
    ~help:"a loop is automatically unrolled by -eva-auto-loop-unroll"
    ~default:(Feedback 4)

let wkey_loop_unroll_partial =
  register_warn_category "loop-unroll:partial"
    ~help:"a loop has been partially but not completely unrolled"
    ~default:(Feedback 4)

let wkey_missing_loop_unroll =
  register_warn_category "loop-unroll:missing"
    ~help:"a loop has no unroll annotation"
    ~default:Inactive

let wkey_missing_loop_unroll_for =
  register_warn_category "loop-unroll:missing:for"
    ~help:"a for loop has no unroll annotation"
    ~default:Inactive

let wkey_signed_overflow =
  register_warn_category "signed-overflow"
    ~help:"two's complement is used to interpret a signed overflow \
           (when signed overflow alarms are disabled)"

let _wkey_assigns =
  register_warn_category "assigns"
    ~help:"warnings related to the interpretation of assigns clauses \
           in ACSL specification"

let wkey_invalid_assigns =
  register_warn_category "assigns:invalid-location"
    ~help:"the memory location targeted by an assigns clause is invalid \
           in at least one analysis state"
    ~default:(Feedback 4)

let wkey_missing_assigns =
  register_warn_category "assigns:missing"
    ~help:"assigns clauses are missing or incomplete from an ACSL \
           specification on which the analysis soundness relies"
    ~default:Error

let wkey_missing_assigns_result =
  register_warn_category "assigns:missing-result"
    ~help:"an assigns \\result clause is missing from an ACSL specification \
           on which the analysis soundness relies"

let wkey_experimental =
  register_warn_category "experimental"
    ~help:"an experimental feature of Eva is enabled"

let wkey_unknown_size =
  register_warn_category "unknown-size"
    ~help:"the analysis cannot compute the size of a variable, \
           which will thus be very imprecise"

let wkey_ensures_false =
  register_warn_category "ensures-false"
    ~help:"a post-condition evaluates to false; \
           there might be an error in the specification"

let wkey_watchpoint =
  register_warn_category "watchpoint"
    ~help:"undocumented"
    ~default:(Feedback 2)

let wkey_recursion =
  register_warn_category "recursion"
    ~help:"a recursive call is analyzed"
    ~default:(Feedback 3)

let wkey_acsl =
  register_warn_category "acsl"
    ~help:"messages about evaluation of ACSL terms and predicates"
    ~default:(Feedback 4)

let wkey_acsl_unsupported =
  register_warn_category "acsl:unsupported"
    ~help:"messages about ACSL terms not supported by Eva"
    ~default:(Feedback 4)

(* ----- Log with positions ------------------------------------------------- *)

type 'a pretty_printer =
  ?emitwith:(Log.event -> unit) -> ?once:bool ->
  ?pos:Position.t -> ?current:bool -> ?source:Filepos.t ->
  ?stacktrace:bool ->  ?append:(Format.formatter -> unit) -> ?echo:bool ->
  ('a,Format.formatter,unit) format -> 'a

type ('a,'b) pretty_aborter =
  ?pos:Position.t -> ?current:bool -> ?source:Filepos.t ->
  ?stacktrace:bool -> ?append:(Format.formatter -> unit) -> ?echo:bool ->
  ('a,Format.formatter,unit,'b) format4 -> 'a

let append_callstack ?(stacktrace=false) ?append ~callstack fmt =
  let pretty_hash fmt cs =
    if is_debug_key_enabled dkey_callstack_hash then
      Format.fprintf fmt "<%a> " Callstack.pretty_hash cs
  in
  Option.iter (fun append -> append fmt) append;
  if stacktrace && is_debug_key_enabled dkey_callstacks then
    match callstack with
    | None -> ()
    | Some cs ->
      (* note: the "\n" before the pretty print of the stack is required by:
         FRAMAC_LIB/analysis-scripts/make_wrapper.py *)
      Format.fprintf fmt "@\nstack: @[<hv>%a%a@]"
        pretty_hash cs
        Callstack.pretty cs

let lift_aborter (aborter : ('a,'b) Log.pretty_aborter)
  : ('a,'b) pretty_aborter =
  fun ?pos ?current ?source ?stacktrace ?append ->
  (* Extract source location *)
  match pos with
  | Some pos ->
    let callstack = Position.callstack pos in
    let source = Option.value ~default:(Position.pos pos) source
    (* Append callstack if requested *)
    and append = append_callstack ?stacktrace ?append ~callstack in
    aborter ?current:None ~source ~append
  | None ->
    let callstack = Current_callstack.get () in
    let append = append_callstack ?stacktrace ?append ~callstack in
    aborter ?current ?source ~append


let lift_printer (printer : 'a Log.pretty_printer) : 'a pretty_printer =
  fun ?emitwith ?once -> lift_aborter (printer ?emitwith ?once)

let result ?level ?dkey =
  lift_printer (result ?level ?dkey)

let feedback ?ontty ?level ?dkey  =
  lift_printer (feedback ?ontty ?level ?dkey )

let debug ?level ?dkey =
  lift_printer (debug ?level ?dkey)

let warning ?wkey : 'a pretty_printer =
  lift_printer (warning ?wkey)

let alarm ?emitwith =
  warning ~wkey:wkey_alarm ?emitwith

let error ?emitwith =
  lift_printer error ?emitwith

let abort ?pos =
  lift_aborter abort ?pos

let failure ?emitwith =
  lift_printer failure ?emitwith

let fatal ?pos =
  lift_aborter fatal ?pos
