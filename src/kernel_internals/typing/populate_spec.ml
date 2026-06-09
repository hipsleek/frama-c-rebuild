(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type mode =
  (* Modes available for specification generation. Skip is only available via
     -generated-spec-custom (cf. option description). *)
  | ACSL | Safe | Frama_C | Skip
  (* Allow user to use a custom mode, see {!register}. *)
  | Other of string

(* Allow customization, each clause can be handled with a different {!mode}. *)
type config = {
  c_exits: mode;
  c_assigns: mode;
  c_requires: mode;
  c_allocates: mode;
  c_terminates: mode;
}

(* Either keep old specification or generate a new one. Existing specification
   from complete behaviors can be combined and used for default behavior. *)
type 'a result = Kept | Generated of 'a

type t_exits = (termination_kind * identified_predicate) list
type t_assigns = Cil_types.assigns
type t_requires = identified_predicate list
type t_allocates = Cil_types.allocation
type t_terminates = identified_predicate option

(* Generation function type and status. *)
type 'a gen = (kernel_function -> spec -> 'a)
type status = Property_status.emitted_status

(* For each clause, we need a generation function and a status to be emitted. *)
type 'a elem = {
  gen: 'a gen option;
  status : status option;
}

(* Allow user to create a mode by choosing how each clause should be generated
   and which status to emit. *)
type custom_mode = {
  custom_exits: t_exits elem;
  custom_assigns: t_assigns elem;
  custom_requires: t_requires elem;
  custom_allocates: t_allocates elem;
  custom_terminates: t_terminates elem;
}

(* Used to store custom modes. *)
let custom_modes = Hashtbl.create 17

let default = Cil.default_behavior_name

let emitter_populate =
  Emitter.create "Populated spec"
    [ Funspec ] ~correctness:[] ~tuning:[]

let emitter_status =
  Emitter.create "Populated status"
    [ Property_status ] ~correctness:[] ~tuning:[]

(* Emit [status] on the property [ppt]. *)
let emit_status status ppt =
  Property_status.emit emitter_status ~hyps:[] ppt status

(* Generic function to fill the clause hashtbl.
   [filter] returns all the clauses of behavior [b] and depend on the type of
   clause. [is_empty] correspond to each {!Generator.is_empty} function.
   The table also remembers for each behavior if it has assumes clause or not.
*)
let collect_behaviors_generic filter is_empty spec =
  let table = Hashtbl.create 17 in
  let iter b =
    let clauses = filter b in
    if not (is_empty clauses) then
      Hashtbl.add table b.b_name (b.b_assumes = [], clauses)
  in
  List.iter iter spec.spec_behavior;
  table

(* Filter [table] to keep behaviors without guard (assume) clause. *)
let unguarded table =
  let filter _ (unguarded, clause) acc =
    if unguarded then clause :: acc else acc
  in
  match Hashtbl.fold filter table [] with
  | [] -> None
  | l -> Some l

(* Find the first group of complete behaviors in which all behaviors are in our
   table. *)
let completes (type clause) completes table =
  let exception Ok of clause list in
  let collect l b =
    let _, c = Hashtbl.find table b in
    c :: l
  in
  let collect bhvs =
    try let r = List.fold_left collect [] bhvs in raise (Ok r)
    with Not_found -> ()
  in
  try
    List.iter collect completes; None
  with Ok l -> Some l

(* Concat all clauses from the table into a list. *)
let get_all_clauses table =
  let clauses =
    Hashtbl.fold (fun _ (_,clause) clauses -> clause :: clauses) table []
  in
  match clauses with
  | [] -> None
  | l -> Some l

(* This function search for existing clauses inside our table. The search is
   done in a specific order and stops when some clauses are found :
   - unguarded behaviors (no assumes).
   - complete behaviors (each one of them needs to be in our table).
   - all behaviors in our table.

   It is a generic function used by {!Generator.get_clauses}.
*)
let get_clauses_generic spec table =
  match unguarded table with
  | Some l -> Some l
  | None ->
    match completes spec.spec_complete_behaviors table with
    | Some l -> Some l
    | None -> get_all_clauses table

(* Register a new custom mode (or replace an existing one). *)
let register ?gen_exits ?status_exits ?gen_assigns ?status_assigns
    ?gen_requires ?gen_allocates ?status_allocates ?gen_terminates
    ?status_terminates name =
  let f gen status = {gen; status} in
  let mode = {
    custom_exits = f gen_exits status_exits;
    custom_assigns = f gen_assigns status_assigns;
    custom_requires = f gen_requires None;
    custom_allocates = f gen_allocates status_allocates;
    custom_terminates = f gen_terminates status_terminates;
  } in
  Hashtbl.replace custom_modes name mode

(* Return a custom mode from the registered ones if it exists. *)
let get_custom_mode mode =
  match Hashtbl.find_opt custom_modes mode with
  | None -> Kernel.abort "Mode %s is not registered" mode
  | Some custom_mode -> custom_mode

(* Use this instead of Identified_term.compare. *)
let compare_it it1 it2 =
  Cil_datatype.Term.compare it1.it_content it2.it_content

(* Return true if [kf] is a builtin of Frama-C. *)
let is_frama_c_builtin kf =
  Kernel_function.get_vi kf |> Cil_builtins.is_builtin

(* This module is used to define clauses generators. *)
module type Generator =
sig

  (* Generator's clause : exits, assigns, requires, allocates or terminates. *)
  type clause
  (* Store information regarding original specification clauses. *)
  type behaviors

  (* Used for messages in logs/warnings, etc. *)
  val name : string
  (* Used to check if we actually generated something. *)
  val is_empty : clause -> bool

  (* Return true if default behavior contains this Generator's clause. *)
  val has_default_behavior : behaviors -> bool
  (* Collect all clauses from function specification. *)
  val collect_behaviors : spec -> behaviors
  (* Collect clauses given a [spec] and the result of {!collect_behaviors}. *)
  val get_clauses : spec -> behaviors -> clause list option

  (* Generate a default clause in ACSL mode. *)
  val acsl_default : kernel_function -> clause
  (* Generate a default clause in Safe mode. *)
  val safe_default : kernel_function -> clause
  (* Generate a default clause in Frama_C mode. *)
  val frama_c_default : kernel_function -> clause
  (* Generate a default clause in Other (custom) modes. *)
  val custom_default : string -> kernel_function -> spec -> clause

  (* Combine clauses from existing behaviors to generate clauses inside
     default behavior. *)
  val combine_default : clause list -> clause

  (* Emit property status depending on the selected mode. *)
  val emit : mode -> kernel_function -> funbehavior -> clause -> unit

end

(* Build Generators. *)
module Make(G : Generator) =
struct

  (* Either combine existing clauses or generate new ones depending on the
     selected mode and original specification. *)
  let combine_or_default mode kf spec table =
    if mode = ACSL then false, G.acsl_default kf
    else
      let clauses_opt = G.get_clauses spec table in
      match mode, clauses_opt with
      | (Safe | Frama_C | Other _), Some clauses ->
        true, G.combine_default clauses
      | Safe, None ->
        false, G.safe_default kf
      | Frama_C, None ->
        false, G.frama_c_default kf
      | Other mode, None ->
        false, G.custom_default mode kf spec
      | (Skip | ACSL), _ -> assert false

  (* Return a new clause as [Generated g] or [Kept] (if no action is needed),
     and an option used for warnings. [combined] is true if the generated
     clause comes from existing ones.
  *)
  let get_default mode kf spec =
    let table = G.collect_behaviors spec in
    if mode = Skip || G.has_default_behavior table then Kept, None
    else
      let combined, g = combine_or_default mode kf spec table in
      let has_body = Kernel_function.has_definition kf in
      if G.is_empty g then Kept, None
      else if has_body || is_frama_c_builtin kf then Generated g, None
      else Generated g, Some(combined, G.name)

  (* Interface to call {!G.emit}. Only emit properties for non empty clauses
     generated for a declaration. *)
  let emit mode kf bhv = function
    | Kept -> ()
    | Generated _ when Kernel_function.has_definition kf -> ()
    | Generated clauses ->
      G.emit mode kf bhv clauses
end

(*******************************************************************)
(* *********************** Exits generator *********************** *)
(* |-------------------------------------------------------------| *)
(* |     ACSL      |    Frama-c   |     Safe      |     Other    | *)
(* |-------|-------|-------|------|-------|-------|-------|------| *)
(* | Proto | Body  | Proto | Body | Proto | Body  | Proto | Body | *)
(* |-------|-------|-------|------|-------|-------|-------|------| *)
(* | false | false | ACSL  | ACSL | ----- | false |  ???  |  ??  | *)
(* |-------------------------------------------------------------| *)
(* *****************************************************************)
(* ****** Status emitted on prototypes ******)
(* |--------------------------------------| *)
(* | ACSL |  Frama-c  |   Safe    | Other | *)
(* |------|-----------|-----------|-------| *)
(* | True | Dont_know | Dont_know |  ???  | *)
(* |--------------------------------------| *)
(********************************************)
module Exits_generator =
struct

  type clause = t_exits
  type behaviors = (string, (bool * clause)) Hashtbl.t

  let name = "exits"

  let is_empty c = c = []

  let has_default_behavior behaviors =
    Hashtbl.mem behaviors default

  let collect_behaviors spec =
    let filter b = List.filter (fun (k, _ ) -> Exits = k) b.b_post_cond in
    collect_behaviors_generic filter is_empty spec

  let get_clauses = get_clauses_generic

  let acsl_default _kf =
    [ Exits, Logic_const.(new_predicate pfalse) ]

  let safe_default kf =
    if Kernel_function.has_definition kf
    then [ Exits, Logic_const.(new_predicate pfalse) ]
    else []

  let frama_c_default kf =
    acsl_default kf

  let combine_default (clauses : clause list) =
    let collect acc clauses = List.rev_append (List.rev clauses) acc in
    let preds =
      List.map
        (fun p -> p.ip_content.tp_statement)
        (List.fold_left collect [] clauses |> List.split |> snd)
      |> List.sort_uniq (Cil_datatype.PredicateStructEq.compare)
    in
    [ Exits, Logic_const.new_predicate (Logic_const.pors preds) ]

  let custom_default mode kf spec =
    let custom_mode = get_custom_mode mode in
    match custom_mode.custom_exits.gen with
    | None ->
      Kernel.warning ~once:true
        "Custom generation from mode %s not defined for exits, using \
         frama-c mode instead" mode;
      frama_c_default kf
    | Some f -> f kf spec

  let emit_status kf bhv exits status =
    let ppt_l =
      List.map (fun e -> Property.ip_of_ensures kf Kglobal bhv e) exits
    in
    List.iter (emit_status status) ppt_l

  let emit mode kf bhv exits =
    match mode with
    | Skip -> assert false
    | ACSL | Safe | Frama_C ->
      emit_status kf bhv exits Property_status.Dont_know
    | Other mode ->
      let custom_mode = get_custom_mode mode in
      match custom_mode.custom_exits.status with
      | None ->
        Kernel.warning ~once:true
          "Custom status from mode %s not defined for exits" mode;
        ()
      | Some pst -> emit_status kf bhv exits pst

end


(*********************************************************************)
(* *********************** Assigns generator *********************** *)
(* |---------------------------------------------------------------| *)
(* |     ACSL      |    Frama-c   |      Safe       |     Other    | *)
(* |-------|-------|-------|------|-------|---------|-------|------| *)
(* | Proto | Body  | Proto | Body | Proto |  Body   | Proto | Body | *)
(* |-------|-------|-------|------|-------|---------|-------|------| *)
(* |  Any  |  Any  | Auto  | Auto |  Any  | Nothing |  ???  |  ??  | *)
(* |---------------------------------------------------------------| *)
(* *******************************************************************)
(* ****** Status emitted on prototypes ******)
(* |--------------------------------------| *)
(* | ACSL |  Frama-c  |   Safe    | Other | *)
(* |------|-----------|-----------|-------| *)
(* | ---- | Dont_know | Dont_know |  ???  | *)
(* |--------------------------------------| *)
(********************************************)
module Assigns_generator =
struct

  type clause = t_assigns
  type behaviors = (string, (bool * clause)) Hashtbl.t

  let name = "assigns"

  let is_empty c = c = WritesAny

  let has_default_behavior behaviors =
    Hashtbl.mem behaviors default

  let collect_behaviors spec =
    let filter b = b.b_assigns in
    collect_behaviors_generic filter is_empty spec

  let get_clauses = get_clauses_generic

  let acsl_default _kf =
    WritesAny

  let safe_default kf =
    if Kernel_function.has_definition kf
    then Writes []
    else WritesAny

  let frama_c_default kf =
    (* TODO: use genassigns for Definitions. *)
    Writes (Infer_assigns.from_prototype kf)

  let compare_deps d1 d2 =
    match d1, d2 with
    | FromAny, FromAny -> 0
    | FromAny, From _ -> 1
    | From _, FromAny -> -1
    | From l1, From l2 ->
      List.compare compare_it l1 l2

  let compare_from (f1, d1) (f2, d2) =
    let r = compare_it f1 f2 in
    if r <> 0 then r else compare_deps d1 d2

  let combine_default clauses =
    (* Note: combination is made on a set of behaviors in the sens of
       {!get_clauses} and {!collect_behaviors}, thus here all clauses are
       [Writes ...]. *)
    let collect acc = function
      | Writes l -> List.rev_append (List.rev l) acc
      | WritesAny -> assert false
    in
    let deps = function
      | FromAny -> FromAny
      | From l -> From (List.sort_uniq compare_it l)
    in
    let froms =
      List.fold_left collect [] clauses
      |> List.map (fun (e, ds) -> e, deps ds)
    in
    Writes (List.sort_uniq compare_from froms)

  let custom_default mode kf spec =
    let custom_mode = get_custom_mode mode in
    match custom_mode.custom_assigns.gen with
    | None ->
      Kernel.warning  ~once:true
        "Custom generation from mode %s not defined for assigns, using \
         frama-c mode instead" mode;
      frama_c_default kf
    | Some f -> f kf spec

  let emit_status kf bhv assigns status =
    let ppt_opt =
      Property.ip_of_assigns kf Kglobal
        (Property.Id_contract (Datatype.String.Set.empty,bhv)) assigns
    in
    Option.iter (emit_status status) ppt_opt;
    match assigns with
    | WritesAny -> assert false
    | Writes froms ->
      let emit from =
        let ppt_opt =
          Property.ip_of_from
            kf Kglobal
            (Property.Id_contract (Datatype.String.Set.empty,bhv)) from
        in
        Option.iter (emit_status status) ppt_opt
      in
      List.iter emit froms

  let emit mode kf bhv assigns =
    match mode with
    | Skip | ACSL -> assert false
    | Safe | Frama_C -> emit_status kf bhv assigns Property_status.Dont_know
    | Other mode ->
      let custom_mode = get_custom_mode mode in
      match custom_mode.custom_assigns.status with
      | None ->
        Kernel.warning ~once:true
          "Custom status from mode %s not defined for assigns" mode;
        ()
      | Some pst -> emit_status kf bhv assigns pst

end


(*****************************************************************)
(* ********************* Requires generator ******************** *)
(* |-----------------------------------------------------------| *)
(* |     ACSL     |    Frama-c   |     Safe     |     Other    | *)
(* |-------|------|-------|------|-------|------|-------|------| *)
(* | Proto | Body | Proto | Body | Proto | Body | Proto | Body | *)
(* |-------|------|-------|------|-------|------|-------|------| *)
(* | ----- | ---- | ACSL  | ACSL | false | ---- |  ???  |  ??  | *)
(* |-----------------------------------------------------------| *)
(* ***************************************************************)
(* ** Status emitted on prototypes ***)
(* |-------------------------------| *)
(* | ACSL | Frama-c | Safe | Other | *)
(* |------|---------|------|-------| *)
(* | ---- | ------- | ---- | ----- | *)
(* |-------------------------------| *)
(*************************************)
module Requires_generator =
struct

  type clause = t_requires
  type behaviors = (string, (bool * clause)) Hashtbl.t

  let name = "requires"

  let is_empty c = c = []

  let has_default_behavior behaviors =
    Hashtbl.mem behaviors default

  let collect_behaviors spec =
    let filter b = b.b_requires in
    collect_behaviors_generic filter is_empty spec

  let get_clauses = get_clauses_generic

  let acsl_default _kf = []

  let safe_default kf =
    if Kernel_function.has_definition kf
    then []
    else [ Logic_const.(new_predicate pfalse) ]

  let frama_c_default kf =
    acsl_default kf

  let combine_default (clauses : clause list) =
    let flatten_require clause =
      List.map (fun p -> p.ip_content.tp_statement) clause
      |> List.sort_uniq Cil_datatype.PredicateStructEq.compare
      |> Logic_const.pands
    in
    let preds =
      List.map flatten_require clauses
      |> Logic_const.pors
    in
    [ Logic_const.new_predicate preds ]

  let custom_default mode kf spec =
    let custom_mode = get_custom_mode mode in
    match custom_mode.custom_requires.gen with
    | None ->
      Kernel.warning ~once:true
        "Custom generation from mode %s not defined for requires, using \
         frama-c mode instead" mode;
      frama_c_default kf
    | Some f -> f kf spec

  let emit _mode _kf _bhv _requires = ()

end


(*************************************************************************)
(* ************************* Allocates generator *********************** *)
(* |-------------------------------------------------------------------| *)
(* |       ACSL        |    Frama-c   |      Safe       |     Other    | *)
(* |---------|---------|-------|------|-------|---------|-------|------| *)
(* |  Proto  |  Body   | Proto | Body | Proto |  Body   | Proto | Body | *)
(* |---------|---------|-------|------|-------|---------|-------|------| *)
(* | Nothing | Nothing | ACSL  | ACSL |  Any  | Nothing |  ???  |  ??  | *)
(* |-------------------------------------------------------------------| *)
(* ***********************************************************************)
(* ****** Status emitted on prototypes ******)
(* |--------------------------------------| *)
(* | ACSL |  Frama-c  |   Safe    | Other | *)
(* |------|-----------|-----------|-------| *)
(* | True | Dont_know | Dont_know |  ???  | *)
(* |--------------------------------------| *)
(********************************************)
module Allocates_generator =
struct

  type clause = t_allocates
  type behaviors = (string, (bool * clause)) Hashtbl.t

  let name = "allocates"

  let is_empty c = c = FreeAllocAny

  let has_default_behavior behaviors =
    Hashtbl.mem behaviors default

  let collect_behaviors spec =
    let filter b = b.b_allocation in
    collect_behaviors_generic filter is_empty spec

  let get_clauses = get_clauses_generic

  let acsl_default _kf =
    FreeAlloc([],[])

  let safe_default kf =
    if Kernel_function.has_definition kf
    then FreeAlloc([],[])
    else FreeAllocAny

  let frama_c_default kf =
    acsl_default kf

  let combine_default clauses =
    (* Note: combination is made on a set of behaviors in the sens of
       {!get_clauses} and {!collect_behaviors}, thus here all clauses are
       [FreeAlloc ...]. *)
    let collect (facc, aacc) = function
      | FreeAlloc(f, a) ->
        List.rev_append (List.rev f) facc, List.rev_append (List.rev a) aacc
      | FreeAllocAny -> assert false
    in
    let f, a = List.fold_left collect ([],[]) clauses in
    let f = List.sort_uniq compare_it f in
    let a = List.sort_uniq compare_it a in
    FreeAlloc(f, a)

  let custom_default mode kf spec =
    let custom_mode = get_custom_mode mode in
    match custom_mode.custom_allocates.gen with
    | None ->
      Kernel.warning ~once:true
        "Custom generation from mode %s not defined for allocates, using \
         frama-c mode instead" mode;
      frama_c_default kf
    | Some f -> f kf spec

  let emit_status kf bhv allocates status =
    let ppt_opt =
      Property.ip_of_allocation kf Kglobal
        (Property.Id_contract (Datatype.String.Set.empty,bhv)) allocates
    in
    Option.iter (emit_status status) ppt_opt

  let emit mode kf bhv allocates =
    match mode with
    | Skip -> assert false
    | ACSL ->
      emit_status kf bhv allocates Property_status.True
    | Safe | Frama_C ->
      emit_status kf bhv allocates Property_status.Dont_know
    | Other mode ->
      let custom_mode = get_custom_mode mode in
      match custom_mode.custom_allocates.status with
      | None ->
        Kernel.warning ~once:true
          "Custom status from mode %s not defined for allocates" mode;
        ()
      | Some pst -> emit_status kf bhv allocates pst

end

(*****************************************************************)
(* ******************** Terminates generated ******************* *)
(* |-----------------------------------------------------------| *)
(* |     ACSL     |    Frama-c   |     Safe     |     Other    | *)
(* |-------|------|-------|------|-------|------|-------|------| *)
(* | Proto | Body | Proto | Body | Proto | Body | Proto | Body | *)
(* |-------|------|-------|------|-------|------|-------|------| *)
(* | true  | true | ACSL  | ACSL | false | true |  ???  |  ??  | *)
(* |-----------------------------------------------------------| *)
(* Note: Terminates are set to false in ACSL, Frama-C and Safe   *)
(* modes if the function has the attribute noreturn.             *)
(* ***************************************************************)
(* ****** Status emitted on prototypes ******)
(* |--------------------------------------| *)
(* | ACSL |  Frama-c  |    Safe   | Other | *)
(* |------|-----------|-----------|-------| *)
(* | True | Dont_know | Dont_know |  ???  | *)
(* |--------------------------------------| *)
(********************************************)
module Terminates_generator =
struct

  type clause = t_terminates
  type behaviors = bool

  let name = "terminates"

  let is_empty c = c = None

  let has_default_behavior behaviors =
    behaviors

  let collect_behaviors spec =
    None <> spec.spec_terminates

  let get_clauses _spec _table = None

  let acsl_default kf =
    if Kernel_function.has_noreturn_attr kf then
      Some(Logic_const.(new_predicate pfalse))
    else
      Some(Logic_const.(new_predicate ptrue))

  let safe_default kf =
    if Kernel_function.has_noreturn_attr kf || not (Kernel_function.has_definition kf) then
      Some(Logic_const.(new_predicate pfalse))
    else Some(Logic_const.(new_predicate ptrue))

  let frama_c_default kf =
    acsl_default kf

  let combine_default _clauses =
    assert false

  let custom_default mode kf spec =
    let custom_mode = get_custom_mode mode in
    match custom_mode.custom_terminates.gen with
    | None ->
      Kernel.warning ~once:true
        "Custom generation from mode %s not defined for terminates, using \
         frama-c mode instead" mode;
      frama_c_default kf
    | Some f -> f kf spec

  let emit_status kf _bhv terminates status =
    match terminates with
    | None -> assert false
    | Some terminates ->
      Property.ip_of_terminates kf Kglobal terminates
      |> emit_status status

  let emit mode kf bhv terminates =
    match mode with
    | Skip -> assert false
    | ACSL ->
      emit_status kf bhv terminates Property_status.True
    | Safe | Frama_C ->
      emit_status kf bhv terminates Property_status.Dont_know
    | Other mode ->
      let custom_mode = get_custom_mode mode in
      match custom_mode.custom_terminates.status with
      | None ->
        Kernel.warning ~once:true
          "Custom status from mode %s not defined for terminates" mode;
        ()
      | Some pst -> emit_status kf bhv terminates pst

end

module Exits = Make(Exits_generator)
module Assigns = Make(Assigns_generator)
module Requires = Make(Requires_generator)
module Allocates = Make(Allocates_generator)
module Terminates = Make(Terminates_generator)

(* Convert string from parameter [-generated-spec-mode] to [mode]. *)
let get_mode ~allow_skip = function
  | "frama-c" -> Frama_C
  | "acsl" -> ACSL
  | "safe" -> Safe
  | "skip" when allow_skip -> Skip
  | "skip" ->
    Kernel.warning ~once:true
      "Mode skip is only available via -generated-spec-custom.@, The mode \
       frama-c will be used instead";
    Frama_C
  | s -> Other s

(* Given a [mode], returns the configuration for each clause. *)
let build_config mode =
  {
    c_exits = mode;
    c_assigns = mode;
    c_requires = mode;
    c_allocates = mode;
    c_terminates = mode;
  }

(* Build configuration from parameter [-generated-spec-mode]. *)
let get_config_mode () =
  Kernel.GeneratedSpecMode.get () |> get_mode ~allow_skip:false |> build_config

(* Build the default configuration, then select modes depending on the
   parameter [-generated-spec-custom]. *)
let get_config_custom () =
  let default = get_config_mode () in
  let collect (k,v) config =
    let mode = get_mode ~allow_skip:true v in
    match k with
    | "exits" -> {config with c_exits = mode}
    | "assigns" -> {config with c_assigns = mode}
    | "requires" -> {config with c_requires = mode}
    | "allocates" -> {config with c_allocates = mode}
    | "terminates" -> {config with c_terminates = mode}
    | s ->
      Kernel.abort
        "@['%s'@] is not a valid key for -generated-spec-custom.@, Accepted \
         keys are 'exits', 'assigns', 'requires', 'allocates' and \
         'terminates'." s
  in
  Kernel.GeneratedSpecCustom.fold collect default

let libc_mode kf =
  if Kernel_function.is_variadic kf then Frama_C else ACSL

(* Create the final configuration used for the specification.
   First we create the configuration to be used (either from command line
   parameters, or specific modes for builtins and frama-c's stdlib).
   Then we activate clauses selected for the generation, skipping all other
   clauses.
*)
let activated_config kf clauses =
  let default =
    if is_frama_c_builtin kf then build_config Frama_C
    else if Kernel_function.is_in_libc kf then build_config (libc_mode kf)
    else get_config_custom ()
  in
  let collect config clause =
    match clause with
    | `Exits -> {config with c_exits = default.c_exits}
    | `Assigns -> {config with c_assigns = default.c_assigns}
    | `Requires -> {config with c_requires = default.c_requires}
    | `Allocates -> {config with c_allocates = default.c_allocates}
    | `Terminates -> {config with c_terminates = default.c_terminates}
  in
  List.fold_left collect (build_config Skip) clauses

let pretty_clauses fmt clauses =
  List.pretty_text Format.pp_print_string ~sep:", " ~last:" and "
    fmt (List.rev clauses)

(* Emit warnings if we generated some clauses and if [kf] is a declaration
   and not a builtin of frama-c (cf. {!Make.get_default}).
   The message varies depending on
   - if the spec was empty.
   - if we used existing clauses.
   - the number of generated clause types.
*)
let do_warning kf ?(loc=Kernel_function.get_location kf) funspec = function
  | None -> ()
  | Some (combined, clauses) ->
    let msg =
      if Cil.is_empty_funspec funspec then
        Format.asprintf
          "Neither code nor specification for function %a, \
           generating default %a"
          Kernel_function.pretty kf
          pretty_clauses clauses
      else
        let source =
          if combined then
            if List.length clauses = 1
            then " from the specification"
            else " (some from the specification)"
          else ""
        in
        Format.asprintf
          "Neither code nor explicit %a for function %a, \
           generating default clauses%s"
          pretty_clauses clauses
          Kernel_function.pretty kf source
    in
    Kernel.warning
      ~once:true ~source:(fst loc) ~wkey:Kernel.wkey_missing_spec
      "@[%a.@ See -generated-spec-* options for more info.@]"
      Format.pp_print_text msg

(* Perform generation of all clauses, add them to the original specification,
   and emit property status for each of them. *)
let do_populate ?loc kf clauses =
  let original_spec = Annotations.funspec kf in
  let config = activated_config kf clauses in

  let apply warn_acc get_default mode =
    let g, warn = get_default mode kf original_spec in
    match g, warn with
    | Kept, _ -> g, warn_acc
    | Generated _, None -> g, warn_acc
    | Generated _, Some (c, clause) ->
      match warn_acc with
      | None -> g, Some (c, [clause])
      | Some (c', clauses) ->
        g, Some (c || c', clause :: clauses)
  in
  let exits, warn = apply None Exits.get_default config.c_exits in
  let assigns, warn = apply warn Assigns.get_default config.c_assigns in
  let requires, warn = apply warn Requires.get_default config.c_requires in
  let allocates, warn = apply warn Allocates.get_default config.c_allocates in
  let terminates, warn = apply warn Terminates.get_default config.c_terminates in

  do_warning ?loc kf original_spec warn;

  let generated original = function
    | Kept -> original
    | Generated clauses -> clauses
  in

  let bhv = Cil.mk_behavior () in
  bhv.b_post_cond <- generated bhv.b_post_cond exits;
  bhv.b_assigns <- generated bhv.b_assigns assigns;
  bhv.b_requires <- generated bhv.b_requires requires;
  bhv.b_allocation <- generated bhv.b_allocation allocates;

  let is_empty_bhv =
    exits = Kept && assigns = Kept && requires = Kept && allocates = Kept
  in

  let spec = Cil.empty_funspec () in
  spec.spec_behavior <- if is_empty_bhv then spec.spec_behavior else [ bhv ];
  spec.spec_terminates <- generated spec.spec_terminates terminates;

  Annotations.add_spec emitter_populate kf spec;
  Exits.emit config.c_exits kf bhv exits;
  Assigns.emit config.c_assigns kf bhv assigns;
  Requires.emit config.c_assigns kf bhv requires;
  Allocates.emit config.c_allocates kf bhv allocates;
  Terminates.emit config.c_terminates kf bhv terminates

type clause = [
  | `Exits
  | `Assigns
  | `Requires
  | `Allocates
  | `Terminates
]

module Clauses =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = clause
      let name = "clause"
      let reprs = [`Exits; `Assigns; `Requires; `Allocates; `Terminates]
      let equal a b = a = b
      let compare = Stdlib.compare
      let hash = Hashtbl.hash
    end)

module Key =
  Datatype.Pair_with_collections (Kernel_function) (Clauses)

(* Hashtbl used to memoize which kernel function has been populated. *)
module Is_populated =
  State_builder.Hashtbl
    (Key.Hashtbl)
    (Datatype.Unit)
    (struct
      let size = 17
      let dependencies = [ Annotations.funspec_state ]
      let name = "Populate_spec.Is_populated"
    end)

let () = Ast.add_linked_state Is_populated.self

(* Return the list of clauses not already done for function [kf]. *)
let todo_clauses kf clauses =
  let not_populated c = not (Is_populated.mem (kf, c)) in
  List.filter not_populated clauses

(* Perform specification generation for [kf] if all requirements are met :
   - [clauses] is not empty
     AND
     [clauses] contains clauses not already generated for [kf]
   - [kf] is a prototype
     OR
     [do_body] is true
*)
let populate_funspec ?loc ?(do_body=false) kf clauses =
  let todo = todo_clauses kf clauses in
  if todo <> [] then
    let has_body = Kernel_function.has_definition kf in
    if not has_body || do_body then begin
      do_populate ?loc kf todo;
      List.iter (fun c -> Is_populated.add (kf, c) ()) todo
    end
