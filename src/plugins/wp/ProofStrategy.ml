(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Logic_ptree
open Pattern
module D = Datatype

(* -------------------------------------------------------------------------- *)
(* --- Proof Strategy Engine                                              --- *)
(* -------------------------------------------------------------------------- *)

type 'a loc = { loc : location ; value : 'a }

let localize ~loc value = { loc ; value }

(* Abstract Syntax Tree: must be stdlib-marshallable *)
type strategy = {
  name: string loc ;
  alternatives: alternative loc list ;
}

and tactic = {
  tactic : string loc ;
  lookup : lookup list ;
  select : value list ;
  params : (string loc * value) list ;
  children : (string loc * string loc) list ; (* name prefix and strategy *)
  default: string loc option; (* None is default *)
}

and alternative =
  | Default
  | Strategy of string loc
  | Provers of string loc list * float option (* timeout *)
  | Auto of string loc (* deprecated -wp-auto *)
  | Tactic of tactic

type hint = string * string list (* strategy name, targets *)

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

(* Strategies applies to all projects *)

let kid = ref 0
let hid : (int,hint) Hashtbl.t = Hashtbl.create 0
let sid : (int,strategy) Hashtbl.t = Hashtbl.create 0
let strategies : (string,strategy) Hashtbl.t = Hashtbl.create 0
let revhints : hint list ref = ref []

(* -------------------------------------------------------------------------- *)
(* --- Printers                                                           --- *)
(* -------------------------------------------------------------------------- *)

let pp_name fmt { value = s } = Format.pp_print_string fmt s
let pp_quoted fmt { value = s } = Format.fprintf fmt "%S" s

let pp_lookup fmt = function
  | { head = true ; goal = true ; hyps = false ; pattern } ->
    Format.fprintf fmt "\\goal(%a)" Pattern.pp_pattern pattern
  | { head = true ; goal = false ; pattern } ->
    Format.fprintf fmt "\\when(%a)" Pattern.pp_pattern pattern
  | { head = false ; goal = true ; hyps = false ; pattern } ->
    Format.fprintf fmt "\\ingoal(%a)" Pattern.pp_pattern pattern
  | { head = false ; goal = false ; pattern } ->
    Format.fprintf fmt "\\incontext(%a)" Pattern.pp_pattern pattern
  | { goal = true ; hyps = true ; pattern } ->
    Format.fprintf fmt "\\pattern(%a)" Pattern.pp_pattern pattern

let pp_select fmt s =
  Format.fprintf fmt "\\select(%a)" Pattern.pp_value s

let pp_param fmt (p,v) =
  Format.fprintf fmt "\\param(%a,%a)" pp_quoted p Pattern.pp_value v

let pp_child fmt (p,s) =
  Format.fprintf fmt "\\child(%a,%a)" pp_quoted p pp_name s

let pp_children fmt s =
  Format.fprintf fmt "\\children(%a)" pp_name s

let pp_alternative_node fmt = function
  | Default -> Format.fprintf fmt "\\default"
  | Strategy s -> pp_name fmt s
  | Auto { value = s } -> Format.fprintf fmt "\\auto(%S)" s
  | Provers([],None) -> Format.fprintf fmt "\\prover()"
  | Provers([],Some tm) -> Format.fprintf fmt "\\prover(%.1f)" tm
  | Provers(p::ps,None) ->
    Format.fprintf fmt "@[<hov 2>\\prover(%a" pp_quoted p ;
    List.iter (Format.fprintf fmt ",@,%a" pp_quoted) ps ;
    Format.fprintf fmt ")@]" ;
  | Provers(ps,Some tm) ->
    Format.fprintf fmt "@[<hov 2>\\prover(" ;
    List.iter (Format.fprintf fmt "%a,@," pp_quoted) ps ;
    Format.fprintf fmt "%.1f)@]" tm ;
  | Tactic { tactic ; lookup ; select ; params ; children ; default } ->
    Format.fprintf fmt "@[<hv 2>\\tactic(%a" pp_quoted tactic ;
    List.iter (Format.fprintf fmt ",@ %a" pp_lookup) lookup ;
    List.iter (Format.fprintf fmt ",@ %a" pp_select) select ;
    List.iter (Format.fprintf fmt ",@ %a" pp_param) params ;
    List.iter (Format.fprintf fmt ",@ %a" pp_child) children ;
    Option.iter (Format.fprintf fmt ",@ %a" pp_children) default ;
    Format.fprintf fmt "@,)@]"

let pp_alternative fmt a = pp_alternative_node fmt a.value

let pp_strategy fmt s =
  Format.fprintf fmt "%s:@ " s.name.value ;
  Pretty_utils.pp_list ~sep:",@ " pp_alternative fmt s.alternatives

let re_ident = Str.regexp "[_a-zA-Z][_a-zA-Z0-9]*$"

let pp_option fmt s =
  if Str.string_match re_ident s 0 then
    Format.pp_print_string fmt s
  else
    Format.fprintf fmt "%S" s

let pp_hint fmt ((s,ps): hint) =
  Format.fprintf fmt "%s:@ " s ;
  Pretty_utils.pp_list ~sep:",@ " pp_option fmt ps

(* -------------------------------------------------------------------------- *)
(* --- Alternative Parser                                                 --- *)
(* -------------------------------------------------------------------------- *)

type context = {
  p_ctxt: Pattern.context ;
  dbg_tbl: (string, Pattern.pattern) Hashtbl.t ;
  mutable last: int ;
}

let context ?tc () =
  { p_ctxt = Pattern.context ?tc () ;
    dbg_tbl = Hashtbl.create 17 ;
    last = 0 ;
  }

let debug_table context = context.dbg_tbl

let error ctxt = error ctxt.p_ctxt

let debug fmt p =
  Format.fprintf fmt "@[<hov 2>at: %a@]" Logic_print.print_lexpr p

let rec parse_provers ctxt provers timeout = function
  | [] -> List.rev provers,timeout
  | p::ps ->
    let loc = p.lexpr_loc in
    match p.lexpr_node with
    | PLconstant (IntConstant t) ->
      let time = try int_of_string t with Invalid_argument _ ->
        error ctxt loc "Invalid timeout" in
      if time < 0 then error ctxt loc "Invalid timeout" ;
      if timeout <> None then error ctxt loc "Duplicate timeout" ;
      parse_provers ctxt provers (Some (float time)) ps
    | PLconstant (FloatConstant t) ->
      let time = try float_of_string t with Invalid_argument _ ->
        error ctxt loc "Invalid timeout" in
      if time < 0.0 then error ctxt loc "Invalid timeout" ;
      if timeout <> None then error ctxt loc "Duplicate timeout" ;
      parse_provers ctxt provers (Some time) ps
    | PLconstant (StringConstant value) ->
      parse_provers ctxt ( { loc ; value } :: provers ) timeout ps
    | _ -> error ctxt loc "Invalid prover specification (%a)" debug p

let parse_name ctxt ~kind ?check p =
  let loc = p.lexpr_loc in
  match p.lexpr_node with
  | PLvar value
  | PLapp(value,[],[])
  | PLconstant(StringConstant value)
    ->
    Option.iter (fun f -> f loc value) check ;
    { loc ; value }
  | _ -> error ctxt loc "%s name expected (%a)" kind debug p

let parse_lookup ctxt
    ?(head=true) ?(goal=false) ?(hyps=false) ?(split=false) p =
  let name = Format.asprintf "$%d" ctxt.last in
  ctxt.last <- ctxt.last + 1 ;
  let pattern = Pattern.(named name @@ pa_pattern ctxt.p_ctxt p) in
  Hashtbl.add ctxt.dbg_tbl name pattern ;
  Pattern.{ goal ; hyps ; head ; split ; pattern }

let autoselect select lookup =
  match select , lookup with
  | [] , p::ps ->
    let q,v = Pattern.self p.pattern in
    [v] , { p with pattern = q }::ps
  | _ -> select, lookup

let rec parse_tactic_params ctxt
    ~tactic ~select ~lookup ~params ~children ~default ps =
  match ps with
  | [] ->
    let select = List.rev select in
    let lookup = List.rev lookup in
    let select,lookup = autoselect select lookup in
    Tactic {
      tactic ; select ; lookup ;
      params = List.rev params ;
      children = List.rev children ;
      default ;
    }
  | p::ps ->
    let loc = p.lexpr_loc in
    let cc = parse_tactic_params ctxt ~tactic in
    match p.lexpr_node with
    | PLapp("\\goal",[],qs) ->
      let qs = List.map (parse_lookup ~goal:true ctxt) qs in
      let lookup = List.rev_append qs lookup in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\when",[],qs) ->
      let qs = List.map (parse_lookup ~hyps:true ~split:true ctxt) qs in
      let lookup = List.rev_append qs lookup in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\ingoal",[],qs) ->
      let qs = List.map (parse_lookup ~head:false ~goal:true ctxt) qs in
      let lookup = List.rev_append qs lookup in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\incontext",[],qs) ->
      let qs = List.map (parse_lookup ~head:false ~hyps:true ~split:true ctxt) qs in
      let lookup = List.rev_append qs lookup in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\pattern",[],qs) ->
      let qs = List.map
          (parse_lookup ~head:false ~goal:true ~hyps:true ctxt) qs in
      let lookup = List.rev_append qs lookup in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\select",[],vs) ->
      let vs = List.map (Pattern.pa_value ctxt.p_ctxt) vs in
      let select = List.rev_append vs select in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\param",[],[param;value]) ->
      let param = parse_name ctxt ~kind:"Parameter" param in
      let value = Pattern.pa_value ctxt.p_ctxt value in
      let params = (param,value)::params in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\child",[],[prefix;strategy]) ->
      let subgoal = parse_name ctxt ~kind:"Subgoal" prefix in
      let strategy = parse_name ctxt ~kind:"Strategy" strategy in
      let children = (subgoal,strategy)::children in
      cc ~select ~lookup ~params ~children ~default ps
    | PLapp("\\children",[],[strategy]) ->
      if default <> None then error ctxt loc "Duplicate \\children parameter" ;
      let default = Some (parse_name ctxt ~kind:"Strategy" strategy) in
      cc ~select ~lookup ~params ~children ~default ps
    | _ -> error ctxt loc "Tactic parameter expected (%a)" debug p

let parse_alternative ctxt p =
  let loc = p.lexpr_loc in
  match p.lexpr_node with
  | PLvar("\\default") -> [ localize ~loc Default ]
  | PLapp("\\prover",[],ps) ->
    let prvs,timeout = parse_provers ctxt [] None ps in
    [ localize ~loc @@ Provers(prvs,timeout) ]
  | PLapp("\\tactic",[],p::ps) ->
    let tactic = parse_name ctxt ~kind:"tactic" p in
    [ localize ~loc @@
      parse_tactic_params ctxt ~tactic
        ~select:[] ~lookup:[] ~params:[] ~children:[] ~default:None ps ]
  | PLapp("\\auto",[],ps) ->
    List.map
      (fun p -> localize ~loc @@ Auto (parse_name ctxt ~kind:"auto" p))
      ps
  | PLvar value | PLapp(value,[],[]) ->
    [ localize ~loc @@ Strategy { loc ; value } ]
  | _ -> error ctxt loc "Strategy definition expected (%a)" debug p

(* -------------------------------------------------------------------------- *)
(* --- Strategy Parser                                                    --- *)
(* -------------------------------------------------------------------------- *)

let parse_strategy_name ctxt loc = function
  | [] -> error ctxt loc "Empty strategy"
  | p::ps ->
    match p.lexpr_node with
    | PLnamed(value,p) -> { loc ; value }, p::ps
    | _ -> error ctxt loc "Missing strategy name (%a)" debug p

let parse_alternatives ctxt ps =
  List.concat @@ List.map (parse_alternative ctxt) ps

let parse_strategy ctxt loc ps =
  let name,ps = parse_strategy_name ctxt loc ps in
  try
    let old = Hashtbl.find strategies name.value in
    error ctxt loc "Duplicate strategy definition ('%s', at %a)"
      name.value Fileloc.pretty old.name.loc
  with Not_found ->
    let alternatives = parse_alternatives ctxt ps in
    let strategy = { name ; alternatives } in
    let id = incr kid ; !kid in
    Hashtbl.add strategies name.value strategy ;
    Hashtbl.add sid id strategy ; Ext_id id

(* -------------------------------------------------------------------------- *)
(* --- Proof Parser                                                       --- *)
(* -------------------------------------------------------------------------- *)

let parse_hints ctxt p =
  let loc = p.lexpr_loc in
  match p.lexpr_node with
  | PLvar x -> [x]
  | PLconstant(StringConstant x) -> String.split_on_char ',' x
  | _ -> error ctxt loc "Proof hint expected (see -wp-prop) (%a)" debug p

let parse_proofs ctxt loc ps =
  let name , ps = parse_strategy_name ctxt loc ps in
  let strategy = name.value in
  if not (Hashtbl.mem strategies strategy) then
    error ctxt name.loc "Unknown strategy '%s'" strategy ;
  let props = List.concat @@ List.map (parse_hints ctxt) ps in
  let hint = (strategy, props) in
  revhints := hint :: !revhints ;
  let id = incr kid ; !kid in
  Hashtbl.add hid id hint ; Ext_id id

(* -------------------------------------------------------------------------- *)
(* --- Strategy ACSL Extensions                                           --- *)
(* -------------------------------------------------------------------------- *)

let registered = ref false
let register () =
  if not !registered && Wp_parameters.StrategyEngine.get () then
    begin
      registered := true ;
      let parse_strategy tc = parse_strategy (context ~tc ()) in
      let parse_proofs tc = parse_proofs (context ~tc ()) in
      let printer hmap pp _ fmt = function
        | Ext_id id -> Option.iter (pp fmt) (Hashtbl.find_opt hmap id)
        | _ -> () in
      Acsl_extension.register_global ~plugin:"wp" "strategy"
        ~printer:(printer sid pp_strategy) parse_strategy false ;
      Acsl_extension.register_global ~plugin:"wp" "proof"
        ~printer:(printer hid pp_hint) parse_proofs false ;
    end

let () = Cmdline.run_after_configuring_stage register

(* -------------------------------------------------------------------------- *)
(* --- Strategy Resolution                                                --- *)
(* -------------------------------------------------------------------------- *)

let name s = s.name.value
let loc s = s.name.loc
let find = Hashtbl.find_opt strategies

let resolve_strategy name =
  find name.value

let resolve_auto name =
  try Some (Strategy.lookup ~id:name.value)
  with Not_found -> None

let resolve_prover name =
  Prover.parse name.value

let resolve_tactic name =
  try Some (Tactical.lookup ~id:name.value)
  with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Strategy Checking                                                  --- *)
(* -------------------------------------------------------------------------- *)

let typecheck_strategy env s =
  if Option.is_none @@ resolve_strategy s then
    Pattern.typecheck_error env s.loc
      "Strategy '%s' undefined (skipped)." s.value

let typecheck_prover env p =
  if Option.is_none @@ resolve_prover p then
    Pattern.typecheck_error env p.loc
      "Prover '%s' not found (skipped)." p.value

let typecheck_auto env a =
  if Option.is_none @@ resolve_auto a then
    Pattern.typecheck_error env a.loc
      "Auto-Strategy '%s' not found (skipped)." a.value

let typecheck_parameter env (t : Tactical.tactical) (p,v) =
  try
    let prm = List.find (fun q -> Tactical.pident q = p.value) t#params in
    match prm with
    | Checkbox _ -> Pattern.typecheck_value env ~tau:Qed.Logic.Bool v
    | _ -> ()
  with Not_found ->
    Pattern.typecheck_error env p.loc
      "Parameter '%s' not found in tactic '%s'"
      p.value t#id

let typecheck_tactic env { tactic ; lookup ; params ; children ; default } =
  List.iter (Pattern.typecheck_lookup env) lookup ;
  begin match resolve_tactic tactic with
    | None ->
      Pattern.typecheck_error env tactic.loc
        "Tactical '%s' not found (skipped alternative)." tactic.value ;
    | Some tactical ->
      List.iter (typecheck_parameter env tactical) params ;
  end ;
  List.iter (fun (_,s) -> typecheck_strategy env s) children ;
  Option.iter (typecheck_strategy env) default

let typecheck_alternative_node env = function
  | Default -> ()
  | Strategy s -> typecheck_strategy env s
  | Auto s -> typecheck_auto env s
  | Provers(pvs,_) -> List.iter (typecheck_prover env) pvs
  | Tactic t -> typecheck_tactic env t

let typecheck_alternative a =
  let env = Pattern.env () in
  typecheck_alternative_node env a.value

let typecheck () =
  Hashtbl.iter
    (fun _ s -> List.iter typecheck_alternative s.alternatives) strategies

(* -------------------------------------------------------------------------- *)
(* --- Strategy Hints                                                     --- *)
(* -------------------------------------------------------------------------- *)

let iter f =
  let module M = Map.Make(String) in
  let pool = ref M.empty in
  Hashtbl.iter (fun a s -> pool := M.add a s !pool) strategies ;
  M.iter (fun _ s -> f s) !pool

let default () =
  List.filter_map
    (fun s ->
       try Some (Hashtbl.find strategies s)
       with Not_found ->
         Wp_parameters.warning ~current:false ~once:true
           "Invalid -wp-strategy '%s' (undefined strategy name)" s ;
         None
    ) @@
  Wp_parameters.DefaultStrategies.get ()

let hints ?node goal =
  let smoke = Wpo.is_smoke_test goal in
  let pool = ref [] in
  let add s = if not @@ List.memq s !pool then pool := s :: !pool in
  let addname name = Option.iter add @@ Hashtbl.find_opt strategies name in
  Option.iter addname (Option.bind ProofEngine.get_hint node) ;
  let pid = goal.Wpo.po_pid in
  List.iter
    (fun (name,ps) ->
       if not smoke || List.mem "smoke" ps then
         if WpPropId.select_by_name ps pid then addname name
    ) !revhints ;
  if not smoke then
    List.iter add @@ List.rev @@ default () ;
  List.rev !pool

let has_hint goal =
  let smoke = Wpo.is_smoke_test goal in
  let pid = goal.Wpo.po_pid in
  List.exists (fun (_,ps) ->
      (not smoke || List.mem "smoke" ps) &&
      WpPropId.select_by_name ps pid
    ) !revhints

(* -------------------------------------------------------------------------- *)
(* --- Strategy Forward Step                                              --- *)
(* -------------------------------------------------------------------------- *)

let alternatives s = s.alternatives

let timeout = function
  | Some tm -> tm | None -> float @@ Wp_parameters.Timeout.get ()

let provers ?(default=[]) alt =
  match alt.value with
  | Provers([],tm) -> default, timeout tm
  | Provers(ps,tm) -> List.filter_map resolve_prover ps, timeout tm
  | Default | Strategy _ | Tactic _ | Auto _ -> [],0.0

let fallback alt =
  match alt.value with
  | Strategy s -> resolve_strategy s
  | Tactic _ | Auto _ | Provers _ -> None
  | Default ->
    let loc = Filepos.(unknown, unknown) in
    Some {
      name = { value = "\\default" ; loc } ;
      alternatives =
        List.map (fun s -> localize ~loc @@ Strategy s.name) @@ default () ;
    }

let auto alt =
  match alt.value with
  | Default | Strategy _  | Tactic _ | Provers _ -> None
  | Auto s -> resolve_auto s

(* -------------------------------------------------------------------------- *)
(* --- Strategy Tactical Step                                             --- *)
(* -------------------------------------------------------------------------- *)

let dkey_tactical = Wp_parameters.register_category "tactical"

let tactical a =
  match resolve_tactic a with None -> raise Not_found | Some t -> t

let parameter env (t : Tactical.tactical) a =
  try List.find (fun p -> Tactical.pident p = a.value) t#params
  with Not_found ->
    typecheck_error env a.loc
      "Parameter '%s' not found (skipped alternative)." a.value ;
    raise Not_found

let rec bind sigma sequent = function
  | [] -> sigma
  | lookup::binders ->
    match Pattern.psequent lookup sigma sequent with
    | None ->
      Wp_parameters.debug ~dkey:dkey_tactical "[failed] %a@."
        pp_lookup lookup ;
      raise Not_found
    | Some sigma ->
      Wp_parameters.debug ~dkey:dkey_tactical "[found] %a@."
        pp_lookup lookup ;
      bind sigma sequent binders

let select sigma ?goal = function
  | [] ->
    begin
      match goal with
      | None -> Tactical.Empty
      | Some p -> Tactical.(Clause (Goal p))
    end
  | [v] -> Pattern.select sigma v
  | vs -> Tactical.Multi (List.map (Pattern.select sigma) vs)

let configure env tactic sigma (a,v) =
  match parameter env tactic a with
  | Checkbox fd ->
    begin
      try tactic#set_field fd (Pattern.bool v)
      with Not_found ->
        typecheck_error env a.loc
          "Expected boolean for parameter '%s' (%a)" a.value
          Pattern.pp_value v ;
        raise Not_found
    end
  | Spinner(fd,_) ->
    begin
      let value = Pattern.select sigma v in
      match Tactical.get_int value with
      | Some v -> tactic#set_field fd v
      | None ->
        typecheck_error env a.loc
          "Expected integer for parameter '%s'@ (%a)" a.value
          Tactical.pp_selection value ;
        raise Not_found
    end
  | Composer(fd,_) ->
    tactic#set_field fd (Pattern.select sigma v)
  | Selector(fd,vs,_) ->
    begin
      try
        let id = Pattern.string v in
        let v = List.find (fun v -> v.Tactical.vid = id) vs in
        tactic#set_field fd v.value
      with Not_found ->
        typecheck_error env a.loc
          "Expected string for parameter '%s'@ (%a)" a.value
          Pattern.pp_value v ;
        raise Not_found
    end
  | Search(fd,_,lookup) ->
    begin
      try
        let id = Pattern.string v in
        let v = lookup id in
        tactic#set_field fd (Some v)
      with Not_found ->
        typecheck_error env a.loc
          "Expected string for parameter '%s'@ (%a)" a.value
          Pattern.pp_value v ;
        raise Not_found
    end

let subgoal (children : (string loc * string loc) list)
    (default : string loc option) (goal,node) =
  let hint = List.find_map (fun (g,s) ->
      if String.starts_with ~prefix:g.value goal then Some s else None
    ) children in
  begin
    match hint, default with
    | None, None -> ()
    | Some s , _ | None , Some s ->
      if not @@ Hashtbl.mem strategies s.value then
        Wp_parameters.error ~source:(fst s.loc)
          "Unknown strategy '%s' (skipped)" s.value
      else ProofEngine.set_hint node s.value
  end ; node

let tactic tree node strategy alt =
  match alt.value with
  | Default | Strategy _ | Auto _ | Provers _ -> None
  | Tactic t ->
    try
      let tactic = tactical t.tactic in
      let pool = ProofEngine.pool tree in
      let title = tactic#title in
      let ctxt = ProofEngine.node_context node in
      let sequent = snd @@ Wpo.compute @@ ProofEngine.goal node in
      let console = new ProofScript.console ~pool ~title in
      let subgoals = WpContext.on_context ctxt
          begin fun () ->
            let env = Pattern.env () in
            let sigma = bind Pattern.empty sequent t.lookup in
            let goal = if t.lookup = [] then Some (snd sequent) else None in
            let selection = select sigma ?goal t.select in
            List.iter (configure env tactic sigma) t.params ;
            match Lang.local ~pool (tactic#select console) selection with
            | exception (Not_found | Exit) -> raise Not_found
            | Not_applicable ->
              raise Not_found
            | exception exn when Wp_parameters.protect exn ->
              Wp_parameters.warning ~source:(fst t.tactic.loc)
                "Tactical '%s' configuration error (%s)"
                t.tactic.value (Printexc.to_string exn) ;
              raise Not_found
            | Not_configured ->
              Wp_parameters.error ~source:(fst t.tactic.loc)
                "Tactical '%s' configuration error"
                t.tactic.value ;
              raise Not_found
            | Applicable process ->
              let strategy = strategy.name.value in
              let script = ProofScript.jtactic ~strategy tactic selection in
              let fork = ProofEngine.fork tree ~anchor:node script process in
              snd @@ ProofEngine.commit fork
          end () in
      Some (List.map (subgoal t.children t.default) subgoals)
    with Not_found -> None

(* -------------------------------------------------------------------------- *)
