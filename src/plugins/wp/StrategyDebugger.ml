(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server
module Md = Markdown

(* -------------------------------------------------------------------------- *)
(* --- Strategy Information                                               --- *)
(* -------------------------------------------------------------------------- *)

type diagnostic = {
  message : string ;
  severity : [ `Ok | `Ignored | `Warning | `Error ] ;
  location : Cil_types.location option ;
}

let valid ~loc ~message =
  { message ; severity = `Ok ; location = Some loc }

let ignored ~loc ~reason =
  { message = reason ; severity = `Ignored ; location = Some loc }

let warning ~loc ~message =
  { message ; severity = `Warning ; location = Some loc }

let error ~loc ~message =
  { message ; severity = `Error ; location = Some loc }

type field = {
  label: string ;
  title: string ;
  value: string ;
  debug: string ;
  target: Ptip.target ;
}

type alternative = {
  location: Cil_types.location option ;
  diagnostic: diagnostic list ;
  fields: field list ;
}

let result ?loc ?(fields=[]) diagnostic =
  { location = loc ; diagnostic ; fields }

let failed ?loc message =
  result ?loc [{ message ; severity = `Error ; location = loc }]

(* -------------------------------------------------------------------------- *)
(* --- Ivette Serializers                                                 --- *)
(* -------------------------------------------------------------------------- *)

let package =
  Package.package ~plugin:"wp"
    ~name:"strategydebugger"
    ~title:"WP Strategy Debugger" ()

module Range : Data.S with type t = Cil_types.location =
struct
  type t = Cil_types.location
  let jtype =
    Data.declare ~package ~name:"range" @@
    Jrecord [ "offset", Jnumber ; "length", Jnumber ]

  let to_json (loc : t) =
    let offset = Filepos.input_offset (fst loc) in
    let length = Filepos.input_offset (snd loc) - offset in
    `Assoc [ "offset", `Int offset ; "length", `Int length ]

  let of_json _ =
    failwith "Wp.StrategyDebugger.Range" [@coverage off]
end

module Target : Data.S with type t = Ptip.target =
struct
  type t = Ptip.target
  module WpTipApioTerm = Data.Joption(WpTipApi.Term)
  let jtype =
    Data.declare ~package ~name:"target" @@
    Jrecord [
      "part", WpTipApi.Part.jtype ;
      "term", WpTipApioTerm.jtype ;
    ]

  let to_json tgt =
    let part = match fst tgt with
      | Ptip.Term -> `Term
      | Ptip.Goal -> `Goal
      | Ptip.Step s -> `Step s.id in
    let term = snd tgt in
    `Assoc [
      "part" , WpTipApi.Part.to_json part ;
      "term" , WpTipApioTerm.to_json term ;
    ]

  let of_json _ =
    failwith "Wp.StrategyDebugger.Target" [@coverage off]
end

module Field : Data.S with type t = field =
struct
  type t = field

  let jtype =
    Data.declare ~package ~name:"field" @@
    Jrecord [
      "label", Jstring ;
      "title", Jstring ;
      "value", Jstring ;
      "debug", Jstring ;
      "target" , Target.jtype ;
    ]

  let to_json fd =
    `Assoc [
      "label" , `String fd.label ;
      "title" , `String fd.title ;
      "value" , `String fd.value ;
      "debug" , `String fd.debug ;
      "target" , Target.to_json fd.target ;
    ]

  let of_json _ =
    failwith "Wp.StrategyDebugger.Field" [@coverage off]
end

module Fields = Data.Jlist(Field)
module RangeOpt = Data.Joption(Range)

module Diagnostic : Data.S with type t = diagnostic =
struct
  type t = diagnostic

  let jseverity_tag = function
    | `Ok -> "Ok"
    | `Ignored -> "Ignored"
    | `Warning -> "Warning"
    | `Error -> "Error"

  let jseverity =
    Data.declare ~package ~name:"severity" @@
    Junion [
      Jtag "Ok" ;
      Jtag "Ignored" ;
      Jtag "Warning" ;
      Jtag "Error" ;
    ]

  let jtype =
    Data.declare ~package ~name:"diagnostic" @@
    Package.(Jrecord [
        "message", Jstring ;
        "severity", jseverity ;
        "range", RangeOpt.jtype ;
      ])

  let to_json diag = `Assoc [
      "severity" , `String (jseverity_tag diag.severity) ;
      "message" , `String diag.message ;
      "range" , RangeOpt.to_json diag.location ;
    ]

  let of_json _ =
    failwith "Wp.StrategyDebugger.Diag" [@coverage off]
end

module Diagnostics = Data.Jlist(Diagnostic)

module Alternative : Data.S with type t = alternative =
struct
  type t = alternative
  let jtype =
    Data.declare ~package ~name:"alternative" @@
    Package.(Jrecord [
        "location", RangeOpt.jtype ;
        "diagnostics", Diagnostics.jtype ;
        "fields", Fields.jtype ;
      ])

  let to_json alt =
    `Assoc [
      "location" , RangeOpt.to_json alt.location ;
      "diagnostics" , Diagnostics.to_json alt.diagnostic ;
      "fields", Fields.to_json alt.fields ;
    ]

  let of_json _ =
    failwith "Wp.StrategyDebugger.Alternative_result" [@coverage off]
end

module Alternatives = Data.Jlist(Alternative)

(* -------------------------------------------------------------------------- *)
(* --- Local tokenizer                                                    --- *)
(* -------------------------------------------------------------------------- *)

exception ParseError of Cil_types.location * string

let set_initial_position dest_lexbuf src_pos =
  dest_lexbuf.Lexing.lex_curr_p <- src_pos;
  dest_lexbuf.lex_abs_pos <- src_pos.pos_cnum

let parse_string s =
  let open Current_loc.Operators in
  let path = Filepath.of_string "<user-string>" in
  let s = String.cat s "\n" in
  let column = String.length s in
  let line =
    let i = ref 0 in
    String.iter (function '\n' -> incr i | _ -> ()) s ; !i in
  let pbeg = Filepos.make ~path ~line:0 ~column:0 ~offset:0 () in
  let pend = Filepos.make ~path ~line ~column ~offset:0 () in
  let lb = Lexing.from_string s in
  let get_loc () =
    Filepos.of_lexing_pos @@ Lexing.lexeme_start_p lb,
    Filepos.of_lexing_pos @@ Lexing.lexeme_end_p lb
  in
  let<> UpdatedCurrentLoc = (pbeg, pend) in
  set_initial_position lb (Filepos.to_lexing_pos pbeg);
  try Logic_parser.lexpr_list_eof Logic_lexer.token lb
  with
  | Logic_utils.Not_well_formed (loc, msg) ->
    raise (ParseError (loc, msg))
  | Logic_lexer.Error (_, msg) ->
    raise (ParseError(get_loc (), msg))
  | Parsing.Parse_error ->
    let loc = get_loc () in
    let tok = Lexing.lexeme lb in
    let msg =
      if tok = "" then "unexpected end of strategy" else
        Printf.sprintf "unexpected token %S" tok in
    raise (ParseError (loc, msg))

let parse_string s =
  Logic_env.builtin_types_as_typenames () ;
  let finally = Logic_env.reset_typenames in
  let work () = parse_string s in
  Fun.protect ~finally work

(* -------------------------------------------------------------------------- *)
(* --- Debugger                                                           --- *)
(* -------------------------------------------------------------------------- *)

(* Custom printers for clause and selection, using TIP printer:
   we want printed values to be consistent with the current printer. *)

let rec pp_selection (printer : Ptip.pseq) fmt = function
  | Tactical.Empty ->
    Format.pp_print_string fmt "None."
  | Inside(_,t) ->
    Format.fprintf fmt "Term: %a" printer#pp_term t
  | Clause (Goal p) -> Format.fprintf fmt "Goal: %a" printer#pp_pred p
  | Clause (Step s) -> printer#pp_step fmt s
  | Compose(Cint k) ->
    Format.fprintf fmt "Value: %a" Z.pretty k
  | Compose(Range(a,b)) ->
    Format.fprintf fmt "Range: %d..%d" a b
  | Compose(Code(e,_,_)) ->
    Format.fprintf fmt "@[<hov 2>Calc: %a@]" printer#pp_term e ;
  | Multi es ->
    Format.fprintf fmt "@[<hov 2>Multi:" ;
    List.iter (Format.fprintf fmt "@ %a;" @@ pp_selection printer) es ;
    Format.fprintf fmt "@]"

let field ~label ?(title="") (printer : Ptip.pseq) pvalue =
  let value = Format.asprintf "%a" (pp_selection printer) pvalue in
  let debug = Format.asprintf "%a" Tactical.pp_selection pvalue in
  let target = printer#selection_to_target pvalue in
  { label ; title ; value ; debug ; target }

type parameter =
  | Selection of Tactical.selection
  | String of string

let extract_matchings debug_table printer ?select ?(params=[]) sigma =
  let selection =
    (* Extracting selection *)
    match select with
    | None -> []
    | Some pvalue -> [field ~label:"Selection" printer pvalue] in
  let params =
    (* Extracting parameters *)
    List.map
      begin fun (a, param) ->
        let label = Format.asprintf "Parameter %S" a in
        match param with
        | Selection sel -> field ~label printer sel
        | String s ->
          { label ; title = "" ; value = s ; debug = s ; target = Term, None }
      end params
  in
  let matched = ref [] in
  Pattern.iter_sigma
    (fun name pvalue ->
       let label =
         if name = "" then "Pattern" else
         if name.[0] = '$' then Printf.sprintf "Pattern %s" name else
           Printf.sprintf "Variable %s" name in
       let title =
         match Hashtbl.find_opt debug_table name with
         | None -> "Pattern variable"
         | Some pattern -> Format.asprintf "%a" Pattern.pp_pattern pattern
       in let fd = field ~label ~title printer pvalue in
       matched := fd :: !matched
    ) sigma ;
  let by_name f g = String.compare f.label g.label in
  selection @ params @ List.sort by_name !matched

let parameter (t : Tactical.tactical) (a: string ProofStrategy.loc) =
  try List.find (fun p -> Tactical.pident p = a.value) t#params
  with Not_found ->
    Format.kasprintf
      (fun e -> raise (Pattern.TypeError(a.loc, e)))
      "Parameter '%s' not found" a.value

let configure_parameter env tactic sigma (a,v) =
  a.ProofStrategy.value,
  match parameter tactic a with
  | Checkbox _ | Spinner _ | Composer _ ->
    ProofStrategy.configure env tactic sigma (a, v) ;
    Selection(Pattern.select sigma v)
  | Selector _ | Search _ ->
    ProofStrategy.configure env tactic sigma (a, v) ;
    String(Pattern.string v)

let configure env sigma tactical params =
  let fold_parameter (sels, diags) (a, v) =
    try (configure_parameter env tactical sigma (a, v) :: sels), diags
    with Pattern.TypeError(loc, message) -> sels, (error ~loc ~message :: diags)
  in
  List.fold_left fold_parameter ([], []) params

let debug_apply ~loc (tactical : Tactical.tactical) select sequent =
  let pool = Lang.new_pool ~vars:(Conditions.vars_seq sequent) () in
  let console = new ProofScript.console ~pool ~title:"debug" in
  match Lang.local ~pool (tactical#select console) select with
  | exception exn ->
    let message =
      Format.asprintf
        "Tactic configuration error (%s)"
        (Printexc.to_string exn) in
    [ error ~loc ~message ]
  | Not_configured ->
    let message =
      match console#get_error with
      | Some msg -> msg
      | None -> "Tactic configuration error"
    in [ error ~loc ~message ]
  | Not_applicable ->
    [ warning ~loc ~message:"Tactic cannot be applied" ]
  | Applicable _ ->
    [ valid ~loc ~message:"Applicable tactic" ]

let debug_tactic env ctxt loc (tac: ProofStrategy.tactic) node =
  match node with
  | None -> result ~loc [valid ~loc ~message:"Valid tactic (syntax only)"]
  | Some node ->
    let printer = WpTipApi.lookup_printer node in
    let dtable = ProofStrategy.debug_table ctxt in
    let sequent = snd @@ Wpo.compute @@ ProofEngine.goal node in
    let rec apply_all sigma = function
      | [] -> (* we successfully matched all patterns *)
        let goal = if tac.lookup = [] then Some (snd sequent) else None in
        let tactical = ProofStrategy.tactical tac.tactic in
        let select = ProofStrategy.select sigma ?goal tac.select in
        let params, diags = configure env sigma tactical tac.params in
        let fields = extract_matchings dtable printer ~select ~params sigma in
        if diags <> [] then
          result ~loc ~fields diags
        else
          result ~loc ~fields @@ debug_apply ~loc tactical select sequent

      | p::ps ->
        match Pattern.psequent p sigma sequent with
        | Some sigma -> (* there are still patterns to match *)
          apply_all sigma ps
        | None -> (* we failed to match all patterns *)
          let loc = Pattern.pattern_loc p.pattern in
          let fields = extract_matchings dtable printer sigma in
          let diag = warning ~loc ~message:"Unmatched pattern" in
          result ~loc ~fields [diag]

    in apply_all Pattern.empty tac.lookup

let debug_alternative ctxt strategy node alt =
  let mk_result diags = Some (result ~loc:alt.ProofStrategy.loc diags) in
  let env = Pattern.env ~raise:true () in
  try
    match alt with
    | ProofStrategy.{ value = Default } ->
      None (* *silently* ignored (no loc to display, nor useful feedback) *)

    | { value = Strategy s }  ->
      if s.value <> strategy then ProofStrategy.typecheck_strategy env s ;
      let reason = "Debugging is not recursively applied" in
      mk_result [ignored ~loc:s.loc ~reason]

    | { value = Auto a }  ->
      ProofStrategy.typecheck_auto env a ;
      let reason = "Debugging is not recursively applied" in
      mk_result [ignored ~loc:a.loc ~reason]

    | { value = Provers (provers, _) }  ->
      let diag prover =
        try ProofStrategy.typecheck_prover env prover ; None
        with Pattern.TypeError (loc, message) -> Some(error ~loc ~message)
      in
      let reason = "Debugging does not execute provers" in
      mk_result @@
      begin
        match List.filter_map diag provers with
        | [] -> [ignored ~loc:alt.loc ~reason]
        | l -> l
      end

    | { value = Tactic t ; loc = alt_loc }  ->
      ProofStrategy.typecheck_tactic env t ;
      Some (debug_tactic env ctxt alt_loc t node)

  with Pattern.TypeError(loc, message) ->
    mk_result [error ~loc ~message]

exception Empty

let debug strategy ?node () =
  let ctxt = ProofStrategy.context () in

  let parse string =
    match parse_string string with
    | [] ->
      raise Empty
    | Logic_ptree.{ lexpr_node = PLnamed(value, p) } :: ps ->
      value, ProofStrategy.parse_alternatives ctxt (p :: ps)
    | ps ->
      "", ProofStrategy.parse_alternatives ctxt ps
  in
  try match parse strategy with
    | exception Empty -> []
    | exception ParseError (loc, message)
    | exception Pattern.TypeError (loc, message) ->
      [ failed ~loc message ]
    | strategy, alternatives ->
      List.filter_map (debug_alternative ctxt strategy node) alternatives
  with exn ->
    [ failed @@ Printf.sprintf "Failure (%s)" (Printexc.to_string exn) ]

let () =
  let signature = Request.signature ~output:(module Alternatives) () in
  let get_text = Request.param signature ~name:"strategy"
      ~descr:(Md.plain "Strategy text")
      ~default:"" (module Data.Jstring) in
  let get_node = Request.param_opt signature ~name:"node"
      ~descr:(Md.plain "Node to check strategy on (optional)")
      (module WpTipApi.Node) in
  Request.register_sig ~package ~kind:`GET ~name:"debug"
    ~descr:(Md.plain "Debug strategy")
    signature
    begin fun rq () ->
      let text = get_text rq in
      let node = get_node rq in
      debug text ?node ()
    end

(* -------------------------------------------------------------------------- *)
(* --- Tests                                                              --- *)
(* -------------------------------------------------------------------------- *)

(* These tests do not cover *)

module Test =
struct
  [@@@ coverage off]

  (* We ignore locations and targets that are too fragile. *)
  let equal_diagnostic d1 d2 =
    let r = d1.severity = d2.severity in
    if r then String.equal d1.message d2.message else r

  let equal_field f1 f2 =
    (* Fast enough for testing. *)
    let l1 = [ f1.label ; f1.title ; f1.value ; f1.debug ] in
    let l2 = [ f2.label ; f2.title ; f2.value ; f2.debug ] in
    List.equal String.equal l1 l2

  let equal_alternative a1 a2 =
    let r = List.equal equal_diagnostic a1.diagnostic a2.diagnostic in
    if r then List.equal equal_field a1.fields a2.fields
    else r

  let equal_alternatives l1 l2 = List.equal equal_alternative l1 l2

  let error = error ~loc:Fileloc.unknown
  let ignored = ignored ~loc:Fileloc.unknown
  let valid = valid ~loc:Fileloc.unknown

  let debug ?node content () =
    let res = debug ?node content () in
    ignore @@ Alternatives.to_json res ; (* check that it does not fail only *)
    res
end

let%test "Empty alternatives" =
  let content = {||} in
  let alts = Test.debug content () in
  let expected = [] in
  Test.equal_alternatives alts expected

let%test "Silently ignore default" =
  let content = {|\default|} in
  let alts = Test.debug content () in
  let expected = [] in
  Test.equal_alternatives alts expected

let%test "Recursion (ignored)" =
  let content = {|name: name|} in
  let alts = Test.debug content () in
  let ignored = Test.ignored ~reason:"Debugging is not recursively applied" in
  let expected = [result [ignored]] in
  Test.equal_alternatives alts expected

let%test "Syntax error: unexpected end" =
  let content = {|\tactic(|} in
  let alts = Test.debug content () in
  let error = Test.error ~message:"unexpected end of strategy" in
  let expected = [result [error]] in
  Test.equal_alternatives alts expected

let%test "Syntax error: unexpected token" =
  let content = {|name: +,|} in
  let alts = Test.debug content () in
  let error = Test.error ~message:"unexpected token \",\"" in
  let expected = [result [error]] in
  Test.equal_alternatives alts expected

let%test "Syntax error: wide strings" =
  let content = {|L"name": a|} in
  let alts = Test.debug content () in
  let error = Test.error ~message:"Wide strings are not allowed as labels." in
  let expected = [result [error]] in
  Test.equal_alternatives alts expected

let%test "Lexer error" =
  let content = {|name: */|} in
  let alts = Test.debug content () in
  let error = Test.error ~message:"lexical error, unexpected block-comment closing" in
  let expected = [result [error]] in
  Test.equal_alternatives alts expected

let%test "Unexisting strategy" =
  let content = {|name: unexisting|} in
  let alts = Test.debug content () in
  let error = Test.error ~message:"Strategy 'unexisting' undefined (skipped)." in
  let expected = [result [error]] in
  Test.equal_alternatives alts expected

let%test "Existing strategy" = true (* cannot be easily tested *)

let%test "Unexisting deprecated strategy" =
  let content = {|\auto("unexisting")|} in
  let alts = Test.debug content () in
  let error = Test.error ~message:"Auto-Strategy 'unexisting' not found (skipped)." in
  let expected = [result [error]] in
  Test.equal_alternatives alts expected

let%test "Existing deprecated strategy" =
  let content = {|\auto("wp:bitrange")|} in
  let alts = Test.debug content () in
  let ignored = Test.ignored ~reason:"Debugging is not recursively applied" in
  let expected = [result [ignored]] in
  Test.equal_alternatives alts expected

let%test "Unexisting provers" =
  let content = {|\prover("alt-ergo", "fake", "other")|} in
  let alts = Test.debug content () in
  let unknown_prover name =
    Test.error
      ~message:(Format.asprintf "Prover '%s' not found (skipped)." name)
  in
  let expected = [result @@ List.map unknown_prover [ "fake" ; "other" ]] in
  Test.equal_alternatives alts expected

let%test "Existing provers" =
  let content = {|\prover("alt-ergo")|} in
  let alts = Test.debug content () in
  let ignored = Test.ignored ~reason:"Debugging does not execute provers" in
  let expected = [result [ignored]] in
  Test.equal_alternatives alts expected

let%test "Basic type error in pattern" =
  let content = {|\tactic("Wp.range", \pattern( (0..x) ))|} in
  let alts = Test.debug content () in
  let error = Test.error ~message:"Invalid bound (int expected)" in
  let expected = [result [error]] in
  Test.equal_alternatives alts expected

let%test "Syntactically correct tactic" =
  let content = {|\tactic("Wp.range", \pattern(_))|} in
  let alts = Test.debug content () in
  let valid = Test.valid ~message:"Valid tactic (syntax only)" in
  let expected = [result [valid]] in
  Test.equal_alternatives alts expected

(* -------------------------------------------------------------------------- *)
