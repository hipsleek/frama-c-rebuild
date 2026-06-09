(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Server requests about the values inferred by the Eva analysis. *)

open Server
open Data
open Cil_types

module Kmap = Kernel_function.Hashtbl
module Smap = Cil_datatype.Stmt.Hashtbl
module CSet = Callstack.Set
module CSmap = Callstack.Hashtbl

module Md = Markdown
module Jdecl = Kernel_ast.Decl
module Jstmt = Kernel_ast.Stmt
module Jmarker = Kernel_ast.Marker

let package =
  let title = "Values inferred by the Eva analysis" in
  Package.package ~plugin:"eva" ~name:"values" ~title ()

(* ----- Simple values information for the Inspector ------------------------ *)

type evaluation_point =
  | Initial
  | Pre of kernel_function
  | Stmt of kernel_function * stmt

let post kf =
  if Analysis.use_spec_instead_of_definition kf
  then raise Not_found
  else
    try Stmt (kf, Kernel_function.find_return kf)
    with Kernel_function.No_Statement -> raise Not_found

let request_at = function
  | Initial -> Results.at_start
  | Stmt (_, stmt) -> Results.before stmt
  | Pre kf -> Results.at_start_of kf

let property_evaluation_point = function
  | Property.IPCodeAnnot { ica_kf = kf; ica_stmt = stmt }
  | IPPropertyInstance { ii_kf = kf; ii_stmt = stmt } -> Stmt (kf, stmt)
  | IPPredicate {ip_kf; ip_kind = PKEnsures (_, Normal)} -> post ip_kf
  | IPPredicate { ip_kf = kf;
                  ip_kind = PKRequires _ | PKAssumes _ | PKTerminates }
  | IPAssigns {ias_kf = kf} | IPFrom {if_kf = kf} ->
    Pre kf
  | IPPredicate _ | IPComplete _ | IPDisjoint _ | IPDecrease _
  | IPAxiomatic _ | IPModule _ | IPLemma _
  | IPTypeInvariant _ | IPGlobalInvariant _
  | IPOther _ | IPAllocation _ | IPReachable _ | IPExtended _ | IPBehavior _ ->
    raise Not_found

let marker_evaluation_point = function
  | Printer_tag.PGlobal _ -> Initial
  | PStmt (kf, stmt) | PStmtStart (kf, stmt) -> Stmt (kf, stmt)
  | PVDecl (kf, kinstr, v) when not (v.vformal || v.vglob) ->
    begin
      (* Only evaluate declaration of local variable if it is initialized. *)
      match kf, kinstr with
      | Some kf, Kstmt ({skind = Instr (Local_init _)} as s) -> Stmt (kf, s)
      | _ -> raise Not_found
    end
  | PLval (kf, ki, _) | PExp (kf, ki, _) | PVDecl (kf, ki, _) ->
    begin
      match kf, ki with
      | Some kf, Kstmt stmt -> Stmt (kf, stmt)
      | Some kf, Kglobal -> Pre kf
      | None, Kglobal -> Initial
      | None, Kstmt _ -> assert false
    end
  | PTermLval (_, _, prop, _) | PIP prop -> property_evaluation_point prop
  | PType _ -> raise Not_found

let term_lval_to_lval kf tlval =
  try
    let result = Option.bind Eva_utils.find_return_var kf in
    Logic_to_c.term_lval_to_lval ?result tlval
  with Logic_to_c.No_conversion -> raise Not_found

(* Returns the server identifier of marker corresponding to varinfo [vi]
   at [eval_point]. *)
let tag_varinfo_as_lval_at eval_point =
  let kf, kinstr =
    match eval_point with
    | Initial -> None, Kglobal
    | Pre kf -> Some kf, Kglobal
    | Stmt (kf, stmt) -> Some kf, Kstmt stmt
  in
  fun vi ->
    let marker = Printer_tag.PLval (kf, kinstr, Cil.var vi) in
    Server.Kernel_ast.Marker.index marker

(* Executes function [f] with an updated global printer which prints
   any varinfo as a lvalue marker at the given evaluation point. *)
let with_updated_varinfo_printer eval_point f =
  let tag_vi = tag_varinfo_as_lval_at eval_point in
  let module Printer_class(X: Printer.PrinterClass) = struct
    class printer () = object
      inherit X.printer () as super

      method! varinfo fmt vi =
        Format.fprintf fmt "@{<%s>%a@}" (tag_vi vi) super#varinfo vi;
    end
  end in
  let printer = Printer.current_printer () in
  let finally () = Printer.set_printer printer in
  Printer.update_printer (module Printer_class: Printer.PrinterExtension);
  Fun.protect ~finally f

let print_value fmt loc =
  let is_scalar = Ast_types.is_scalar in
  let evaluation_point = marker_evaluation_point loc in
  let request = request_at evaluation_point in
  let eval =
    match loc with
    | Printer_tag.PLval (_, _, lval) when is_scalar (Cil.typeOfLval lval) ->
      Results.eval_lval lval
    | Printer_tag.PExp (_, _, expr) when is_scalar (Cil.typeOf expr) ->
      Results.eval_exp expr
    | PVDecl (_, _, vi) when is_scalar vi.vtype ->
      Results.eval_var vi
    | PTermLval (kf, _, _ip, tlval) ->
      let lval = term_lval_to_lval kf tlval in
      if is_scalar (Cil.typeOfLval lval)
      then Results.eval_lval lval
      else raise Not_found
    | _ -> raise Not_found
  in
  let pretty = Cvalue.V_Or_Uninitialized.pretty in
  let eval_cvalue at = Results.(eval at |> as_cvalue_or_uninitialized) in
  let before = eval_cvalue request in
  let print =
    match evaluation_point with
    | Initial | Pre _ -> fun () -> pretty fmt before
    | Stmt (_, stmt) ->
      let after = eval_cvalue (Results.after stmt) in
      if Cvalue.V_Or_Uninitialized.equal before after
      then fun () -> pretty fmt before
      else fun () ->
        Format.fprintf fmt "Before: %a@\nAfter:  %a" pretty before pretty after
  in
  with_updated_varinfo_printer evaluation_point print

let () =
  Server.Kernel_ast.Information.register
    ~id:"eva.value"
    ~label:"Value"
    ~title:"Possible values inferred by Eva"
    ~enable:Analysis.is_computed
    print_value


(* ----- Detailed values by callstack for the values table ------------------ *)

type term = Pexpr of exp | Plval of lval | Ppred of predicate

(* A term and the program point where it should be evaluated. *)
type probe = term * evaluation_point

type truth = Abstract_interp.truth

(* The result of an evaluation:
   - the resulting value as a text to be printed;
   - the alarms emitted for the evaluation;
   - the variables pointed by the resulting value, if any. *)
type evaluation = {
  value: Jtext.t;
  alarms: ( truth * string ) list ;
  pointed_vars: (string * Printer_tag.localizable) list;
}

(* Evaluations after the given statement. If the statement is a conditional
   branch, evaluations in the [then] and [else] branch. *)
type 'v next =
  | After of 'v
  | Cond of 'v * 'v
  | Nothing

type evaluations = {
  here: evaluation;
  next: evaluation next;
}

let signal = Request.signal ~package ~name:"changed"
    ~descr:(Md.plain "Emitted when EVA results has changed")

let () = Analysis.register_computation_hook ~on:Computed
    (fun _ -> Request.emit signal)

(* -------------------------------------------------------------------------- *)
(* --- Marker Utilities                                                   --- *)
(* -------------------------------------------------------------------------- *)

let next_steps = function
  | Initial | Pre _ -> `None
  | Stmt (_, stmt) ->
    match stmt.skind with
    | If (cond, _, _, _) -> `Condition (stmt, cond)
    | Instr (Set _ | Call _ | Local_init _) -> `Effect stmt
    | Instr _ when Annotations.has_code_annot stmt -> `Effect stmt
    | Instr (Asm _ | Code_annot _ | Skip _)
    | Switch _ | Loop _ | Block _ | UnspecifiedSequence _
    | TryCatch _ | TryFinally _ | TryExcept _
    | Return _ | Break _ | Continue _ | Goto _ | Throw _ -> `None

let probe_stmt stmt =
  match stmt.skind with
  | Instr (Set (lv, _, _))
  | Instr (Call (Some lv, _, _, _)) -> Plval lv
  | Instr (Local_init (v, _, _)) -> Plval (Var v, NoOffset)
  | Return (Some e, _) | If (e, _, _, _) | Switch (e, _, _, _) -> Pexpr e
  | _ -> raise Not_found

let probe_code_annot = function
  | AAssert (_, p) | AInvariant (_, true, p) -> Ppred p.tp_statement
  | _ -> raise Not_found

let probe_property = function
  | Property.IPCodeAnnot ica -> probe_code_annot ica.ica_ca.annot_content
  | IPPropertyInstance { ii_pred = Some pred }
  | IPPredicate {ip_pred = pred} ->
    Ppred (Logic_const.pred_of_id_pred pred)
  | _ -> raise Not_found

let probe_marker = function
  | Printer_tag.PLval (_, _, lval)
    when Ast_types.is_fun (Cil.typeOfLval lval) -> raise Not_found
  | PVDecl (_, _, vi) when Ast_types.is_fun vi.vtype -> raise Not_found
  | PLval (_, _, l) -> Plval l
  | PExp (_, _, e) -> Pexpr e
  | PStmt (_, s) | PStmtStart (_, s) -> probe_stmt s
  | PVDecl (_, _, v) -> Plval (Var v, NoOffset)
  | PTermLval (kf, _, _, tlval) ->
    Plval (term_lval_to_lval kf tlval)
  | PIP property -> probe_property property
  | _ -> raise Not_found

let probe marker =
  try Some (probe_marker marker, marker_evaluation_point marker)
  with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Stmt Ranking                                                       --- *)
(* -------------------------------------------------------------------------- *)

module type Ranking_sig = sig
  val stmt : stmt -> int
  val sort : Callstack.t list -> Callstack.t list
end

module Ranking : Ranking_sig = struct

  class ranker = object(self)
    inherit Visitor.frama_c_inplace
    (* ranks really starts at 1 *)
    (* rank < 0 means not computed yet *)
    val mutable rank = (-1)
    val rmap = Smap.create 0
    val fmark = Kmap.create 0
    val fqueue = Queue.create ()

    method private call kf =
      if not (Kmap.mem fmark kf) then
        ( Kmap.add fmark kf () ; Queue.push kf fqueue )

    method private newrank s =
      let r = succ rank in
      Smap.add rmap s r ;
      rank <- r ; r

    method! vlval lv =
      begin
        try match fst lv with
          | Var vi -> self#call (Globals.Functions.get vi)
          | _ -> ()
        with Not_found -> ()
      end ; Cil.DoChildren

    method! vstmt_aux s =
      ignore (self#newrank s) ;
      Cil.DoChildren

    method flush =
      while not (Queue.is_empty fqueue) do
        let kf = Queue.pop fqueue in
        ignore (Visitor.(visitFramacKf (self :> frama_c_visitor) kf))
      done

    method compute =
      match Globals.entry_point () with
      | kf , _ -> self#call kf ; self#flush
      | exception Globals.No_such_entry_point _ -> ()

    method rank s =
      if rank < 0 then (rank <- 0 ; self#compute) ;
      try Smap.find rmap s
      with Not_found ->
        let kf = Kernel_function.find_englobing_kf s in
        self#call kf ;
        self#flush ;
        try Smap.find rmap s
        with Not_found -> self#newrank s

  end

  let stmt = let rk = new ranker in rk#rank

  let ranks (cs : Callstack.t) : int list =
    List.map stmt (Callstack.to_stmt_list cs)

  let order : int list -> int list -> int = Stdlib.compare

  let sort (wcs : Callstack.t list) : Callstack.t list =
    List.map fst @@
    List.sort (fun (_,rp) (_,rq) -> order rp rq) @@
    List.map (fun cs -> cs , ranks cs) wcs

end

(* -------------------------------------------------------------------------- *)
(* --- Domain Utilities                                                   --- *)
(* -------------------------------------------------------------------------- *)

module Jcallstack : S with type t = Callstack.t =
  Data.Index
    (Callstack.Map)
    (struct
      let package = package
      let name = "callstack"
      let descr = Md.plain "Callstack identifier"
    end)

module Jcalls : Request.Output with type t = Callstack.t = struct

  type t = Callstack.t

  let jcallsite = Server.Data.declare ~package
      ~name:"callsite" ~descr:(Md.plain "Call site infos")
      (Jrecord [
          "callee" , Jdecl.jtype ;
          "caller" , Joption Jdecl.jtype ;
          "stmt" , Joption Jstmt.jtype ;
          "rank" , Joption Jnumber ;
        ])

  let jtype = Package.(Jarray jcallsite)

  let jcallsite ~jcaller ~jcallee stmt =
    `Assoc [
      "callee", jcallee ;
      "caller", jcaller ;
      "stmt", Jstmt.to_json stmt ;
      "rank", Jint.to_json (Ranking.stmt stmt) ;
    ]

  let to_json (cs : t) =
    let aux (acc, jcaller) (callee, stmt) =
      let jcallee = Jdecl.to_json (SFunction callee) in
      jcallsite ~jcaller ~jcallee stmt :: acc, jcallee
    in
    let entry_point = Jdecl.to_json (SFunction cs.entry_point) in
    let l, _last_callee =
      List.fold_left aux
        ([`Assoc [ "callee", entry_point ]], entry_point)
        (List.rev cs.stack)
    in `List l

end

module Jtruth : Data.S with type t = truth = struct
  type t = truth
  let jtype = Package.(Junion [ Jtag "True" ; Jtag "False" ; Jtag "Unknown" ])
  let to_json = function
    | Abstract_interp.Unknown -> `String "Unknown"
    | True -> `String "True"
    | False -> `String "False"
  let of_json = function
    | `String "True" -> Abstract_interp.True
    | `String "False" -> Abstract_interp.False
    | _ -> Abstract_interp.Unknown
end

(* -------------------------------------------------------------------------- *)
(* --- Utility functions for cvalue and offsetmaps                        --- *)
(* -------------------------------------------------------------------------- *)

type offsetmap =
  | Offsetmap of Cvalue.V_Offsetmap.t
  | Bottom | Empty | Top | InvalidLoc

let pp_offsetmap typ fmt = function
  | Bottom -> Format.fprintf fmt "<BOTTOM>"
  | Empty -> Format.fprintf fmt "<EMPTY>"
  | Top -> Format.fprintf fmt "<NO INFORMATION>"
  | InvalidLoc -> Format.fprintf fmt "<INVALID LOCATION>"
  | Offsetmap offsm ->
    Cvalue.V_Offsetmap.pretty_generic ~typ () fmt offsm ;
    Eval_op.pretty_stitched_offsetmap fmt typ offsm

let extract_single_var vi state =
  let b = Base.of_varinfo vi in
  try
    match Cvalue.Model.find_base b state with
    | `Bottom -> Bottom
    | `Value m -> Offsetmap m
    | `Top -> Top
  with Not_found -> InvalidLoc

let reduce_loc_and_eval state loc =
  if Cvalue.Model.is_top state then Top
  else if not (Cvalue.Model.is_reachable state) then Bottom
  else if Z_or_top.is_zero loc.Locations.size then Empty
  else
    let loc' = Locations.(valid_part Read loc) in
    if Locations.is_bottom loc' then InvalidLoc
    else
      try
        let size = Z_or_top.project loc'.Locations.size in
        match Cvalue.Model.copy_offsetmap loc'.Locations.addr size state with
        | `Bottom -> InvalidLoc
        | `Value offsm -> Offsetmap offsm
      with Abstract_interp.Error_Top -> Top

let find_offsetmap cvalue_state precise_loc =
  let f loc acc =
    match acc, reduce_loc_and_eval cvalue_state loc with
    | Offsetmap o1, Offsetmap o2 -> Offsetmap (Cvalue.V_Offsetmap.join o1 o2)
    | Bottom, v | v, Bottom -> v
    | Empty, v | v, Empty -> v
    | Top, Top -> Top
    | InvalidLoc, InvalidLoc -> InvalidLoc
    | InvalidLoc, (Offsetmap _ as res) -> res
    | Offsetmap _, InvalidLoc -> acc
    | Top, r | r, Top -> r (* cannot happen, we should get Top everywhere *)
  in
  Precise_locs.fold f precise_loc Bottom

(* Get pointed bases from a cvalue. *)
let get_bases cvalue =
  try Base.SetLattice.project (Cvalue.V.get_bases cvalue)
  with Abstract_interp.Error_Top -> Base.Hptset.empty

(* Get pointed bases from an offsetmap.  *)
let get_pointed_bases = function
  | Offsetmap offsm ->
    let get_bases v = Cvalue.V_Or_Uninitialized.get_v v |> get_bases in
    let f v acc = get_bases v |> Base.Hptset.union acc in
    Cvalue.V_Offsetmap.fold_on_values f offsm Base.Hptset.empty
  | Bottom | Empty | Top | InvalidLoc -> Base.Hptset.empty

(* Only keep a list of C variables from both previous functions. *)
let filter_variables bases =
  let add_var base acc =
    try Base.to_varinfo base :: acc
    with Base.Not_a_C_variable -> acc
  in
  let vars = List.rev (Base.Hptset.fold add_var bases []) in
  List.filter (fun vi -> not (Ast_types.is_fun vi.vtype)) vars

(* -------------------------------------------------------------------------- *)
(* --- EVA Proxy                                                          --- *)
(* -------------------------------------------------------------------------- *)

module type EvaProxy = sig
  val callstacks : Domain_store.control_point -> Callstack.t list
  val evaluate : probe -> Callstack.t option -> evaluations
end

module Proxy(A : Engine_sig.S_with_results) : EvaProxy = struct

  open Eval
  type dstate = A.Dom.state or_top_bottom

  let get_precise_loc =
    let default = fun _ -> Precise_locs.loc_top in
    Option.value ~default (A.Loc.get Main_locations.PLoc.key)

  let get_cvalue =
    let default = fun _ -> Cvalue.V.top in
    Option.value ~default (A.Val.get Main_values.CVal.key)

  let callstacks control_point =
    match A.callstacks control_point with
    | `Top -> []
    | `Value list -> list

  let domain_state callstack = function
    | Initial -> A.get_state Initial
    | Pre kf -> A.get_state ?callstack (Start kf)
    | Stmt (_, stmt) -> A.get_state ?callstack (Before stmt)

  (* --- Converts an evaluation [result] into an exported [value]. ---------- *)

  (* Result of an evaluation: a generic value for scalar types, or an offsetmap
     for struct and arrays. *)
  type result =
    | Value of A.Val.t Eval.flagged_value
    | Offsetmap of offsetmap
    | Status of truth

  let pp_result typ fmt = function
    | Value v ->
      (Eval.pretty_value_with_flags (A.Val.pretty_typ (Some typ))) fmt v
    | Offsetmap offsm -> pp_offsetmap typ fmt offsm
    | Status truth -> Alarmset.Status.pretty fmt truth

  let get_pointed_bases = function
    | Value v -> get_bases Bottom.(map get_cvalue v.v |> value ~bottom:Cvalue.V.bottom)
    | Offsetmap offsm -> get_pointed_bases offsm
    | Status _ -> Base.Hptset.empty

  let get_pointed_markers eval_point result =
    let bases = get_pointed_bases result in
    let vars = filter_variables bases in
    let kf, kinstr =
      match eval_point with
      | Initial -> None, Kglobal
      | Pre kf -> Some kf, Kglobal
      | Stmt (kf, stmt) -> Some kf, Kstmt stmt
    in
    let to_marker vi =
      let text = Pretty_utils.to_string Printer.pp_varinfo vi in
      let marker = Printer_tag.PLval (kf, kinstr, Cil.var vi) in
      text, marker
    in
    List.map to_marker vars

  (* Creates an exported [value] from an evaluation result. *)
  let make_value typ eval_point (result, alarms) =
    let descr = Format.asprintf "@[<hov 2>%a@]" Alarms.pretty in
    let f alarm status acc = (status, descr alarm) :: acc in
    let alarms = Alarmset.fold f [] alarms |> List.rev in
    let pretty_eval = Bottom.pretty (pp_result typ) in
    let result_to_json () = Data.jpretty pretty_eval result in
    let value = with_updated_varinfo_printer eval_point result_to_json in
    let pointed_markers = get_pointed_markers eval_point in
    let pointed_vars = Bottom.(map pointed_markers result |> value ~bottom:[]) in
    { value; alarms; pointed_vars }

  (* --- Evaluates an expression or lvalue into an evaluation [result]. ----- *)

  let lval_to_offsetmap (lval : Eva_ast.lval) state =
    let cvalue_state = A.Dom.get_cvalue_or_top state in
    match lval.node with
    | Var vi, NoOffset ->
      let r = extract_single_var vi cvalue_state in
      `Value r, Alarmset.none
    | _ ->
      A.eval_lval_to_loc state lval >>=: fun loc ->
      let precise_loc = get_precise_loc loc in
      find_offsetmap cvalue_state precise_loc

  let eval_lval (lval : Eva_ast.lval) state =
    match Ast_types.unroll_node lval.typ with
    | TInt _ | TEnum _ | TPtr _ | TFloat _ ->
      A.copy_lvalue state lval >>=: fun value -> Value value
    | _ ->
      lval_to_offsetmap lval state >>=: fun offsm -> Offsetmap offsm

  let eval_expr expr state =
    A.eval_expr state expr >>=: fun value ->
    Value { v = `Value value; initialized = true; escaping = false }

  let eval_pred eval_point predicate state =
    let result =
      match eval_point with
      | Initial | Pre _ -> None
      | Stmt (kf, _) -> Eva_utils.find_return_var kf
    in
    let env =
      Abstract_domain.{ states = (function _ -> A.Dom.top) ; result }
    in
    let truth = A.Dom.evaluate_predicate env state predicate in
    `Value (Status truth), Alarmset.none

  (* --- Evaluates all steps (before/after the statement). ------------------ *)

  let do_next eval state eval_point callstack =
    match next_steps eval_point with
    | `Condition (stmt, cond) ->
      let cond' = Eva_ast.translate_exp cond in
      let kf = Kernel_function.find_englobing_kf stmt in
      let dummy_callstack = Callstack.init ~thread:0 ~entry_point:kf in
      let pos = Position.local stmt dummy_callstack  in
      let then_state = (A.assume_cond ~pos state cond' true :> dstate) in
      let else_state = (A.assume_cond ~pos state cond' false :> dstate) in
      Cond (eval then_state, eval else_state)
    | `Effect stmt ->
      let after_state = A.get_state ?callstack (After stmt) in
      After (eval after_state)
    | `None -> Nothing

  let eval_steps typ eval eval_point callstack =
    let default str =
      let value = Data.jtext str in
      { value; alarms = []; pointed_vars = []; }
    in
    let eval = function
      | `Bottom -> default "Unreachable"
      | `Top -> default "No information"
      | `Value state -> make_value typ eval_point (eval state)
    in
    let before = domain_state callstack eval_point in
    let here = eval before in
    let next =
      match before with
      | `Value state -> do_next eval state eval_point callstack
      | _ -> Nothing
    in
    { here; next; }

  let evaluate (term, eval_point) callstack =
    match term with
    | Plval lval ->
      let lval' = Eva_ast.translate_lval lval in
      eval_steps lval'.typ (eval_lval lval') eval_point callstack
    | Pexpr expr ->
      let expr' = Eva_ast.translate_exp expr in
      eval_steps expr'.typ (eval_expr expr') eval_point callstack
    | Ppred pred ->
      eval_steps Cil_const.intType (eval_pred eval_point pred) eval_point callstack
end

let proxy =
  let make (a : (module Engine_sig.S_with_results)) =
    (module Proxy (val a) : EvaProxy)
  in
  let current = ref (make @@ Engine.current ()) in
  let hook a = current := make a ; Request.emit signal in
  Engine.register_hook hook ;
  fun () -> !current

(* -------------------------------------------------------------------------- *)
(* --- Request getCallstacks                                              --- *)
(* -------------------------------------------------------------------------- *)

let () =
  Request.register ~package
    ~kind:`GET ~name:"getCallstacks"
    ~descr:(Md.plain "Callstacks for markers")
    ~input:(module Jlist(Jmarker))
    ~output:(module Jlist(Jcallstack))
    begin fun markers ->
      let module A : EvaProxy = (val proxy ()) in
      let gather_callstacks cset marker =
        let list =
          match probe marker with
          | Some (_, Stmt (_, stmt)) -> A.callstacks (Before stmt)
          | Some (_, Pre kf) -> A.callstacks (Start kf)
          | Some (_, Initial) | None -> []
        in
        List.fold_left (fun set elt -> CSet.add elt set) cset list
      in
      let cset = List.fold_left gather_callstacks CSet.empty markers in
      Ranking.sort (CSet.elements cset)
    end

(* -------------------------------------------------------------------------- *)
(* --- Request getCallstackInfo                                           --- *)
(* -------------------------------------------------------------------------- *)

let () =
  Request.register ~package
    ~kind:`GET ~name:"getCallstackInfo"
    ~descr:(Md.plain "Callstack Description")
    ~input:(module Jcallstack)
    ~output:(module Jcalls)
    begin fun cs -> cs end

(* -------------------------------------------------------------------------- *)
(* --- Request getStmtInfo                                                --- *)
(* -------------------------------------------------------------------------- *)

let () =
  let getStmtInfo = Request.signature ~input:(module Jstmt) () in
  let set_fct = Request.result getStmtInfo ~name:"fct"
      ~descr:(Md.plain "Function name")
      (module Jstring)
  and set_rank = Request.result getStmtInfo ~name:"rank"
      ~descr:(Md.plain "Global stmt order")
      (module Jint)
  in
  Request.register_sig ~package getStmtInfo
    ~kind:`GET ~name:"getStmtInfo"
    ~descr:(Md.plain "Stmt Information")
    begin fun rq s ->
      set_fct rq Kernel_function.(get_name @@ find_englobing_kf s) ;
      set_rank rq (Ranking.stmt s) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Request getProbeInfo                                               --- *)
(* -------------------------------------------------------------------------- *)

let is_reachable = function
  | Stmt (_, stmt) -> Results.is_reachable stmt
  | Pre kf -> Results.is_called kf
  | Initial -> Results.is_reachable_kinstr Kglobal

let () =
  let getProbeInfo = Request.signature ~input:(module Jmarker) () in
  let set_evaluable = Request.result getProbeInfo
      ~name:"evaluable" ~descr:(Md.plain "Can the probe be evaluated?")
      (module Jbool)
  and set_code = Request.result_opt getProbeInfo
      ~name:"code" ~descr:(Md.plain "Probe source code")
      (module Jstring)
  and set_stmt = Request.result_opt getProbeInfo
      ~name:"stmt" ~descr:(Md.plain "Probe statement")
      (module Jstmt)
  and set_effects = Request.result getProbeInfo
      ~name:"effects" ~descr:(Md.plain "Effectfull statement")
      ~default:false (module Jbool)
  and set_condition = Request.result getProbeInfo
      ~name:"condition" ~descr:(Md.plain "Conditional statement")
      ~default:false (module Jbool)
  in
  let set_probe rq pp p eval_point =
    let computed = Analysis.is_computed () in
    let reachable = is_reachable eval_point in
    set_evaluable rq (computed && reachable);
    set_code rq (Some (Pretty_utils.to_string pp p));
    begin
      match eval_point with
      | Initial | Pre _ -> ()
      | Stmt (_kf, stmt) -> set_stmt rq (Some stmt)
    end ;
    match next_steps eval_point with
    | `None -> ()
    | `Condition _ -> set_condition rq true
    | `Effect _ -> set_effects rq true
  in
  Request.register_sig ~package getProbeInfo
    ~kind:`GET ~name:"getProbeInfo"
    ~descr:(Md.plain "Probe information")
    begin fun rq marker ->
      match probe marker with
      | None -> set_evaluable rq false
      | Some (term, eval_point) ->
        match term with
        | Plval l -> set_probe rq Printer.pp_lval l eval_point
        | Pexpr e -> set_probe rq Printer.pp_exp e eval_point
        | Ppred p -> set_probe rq Printer.pp_predicate p eval_point
    end

(* -------------------------------------------------------------------------- *)
(* --- Request getValues                                                  --- *)
(* -------------------------------------------------------------------------- *)

module JEvaluation = struct
  open Server.Data

  type record
  let record: record Record.signature = Record.signature ()

  let value = Record.field record ~name:"value"
      ~descr:(Markdown.plain "Textual representation of the value")
      (module Data.Jtext)
  let alarms = Record.field record ~name:"alarms"
      ~descr:(Markdown.plain "Alarms raised by the evaluation")
      (module Jlist (Jpair (Jtruth) (Jstring)))
  let pointed_vars = Record.field record ~name:"pointedVars"
      ~descr:(Markdown.plain "List of variables pointed by the value")
      (module Jlist (Jpair (Jstring) (Jmarker)))

  let data = Record.publish record ~package ~name:"evaluation"
      ~descr:(Markdown.plain "Evaluation of an expression or lvalue")

  module R: Record.S with type r = record = (val data)
  type t = evaluation
  let jtype = R.jtype

  let to_json t =
    R.default |>
    R.set value t.value |>
    R.set alarms t.alarms |>
    R.set pointed_vars t.pointed_vars |>
    R.to_json
end

let () =
  let getValues = Request.signature () in
  let get_tgt = Request.param getValues ~name:"target"
      ~descr:(Md.plain "Works with all markers containing an expression")
      (module Jmarker)
  and get_cs = Request.param_opt getValues ~name:"callstack"
      ~descr:(Md.plain "Callstack to collect (defaults to none)")
      (module Jcallstack)
  and set_before = Request.result_opt getValues ~name:"vBefore"
      ~descr:(Md.plain "Domain values before execution")
      (module JEvaluation)
  and set_after = Request.result_opt getValues ~name:"vAfter"
      ~descr:(Md.plain "Domain values after execution")
      (module JEvaluation)
  and set_then = Request.result_opt getValues ~name:"vThen"
      ~descr:(Md.plain "Domain values for true condition")
      (module JEvaluation)
  and set_else = Request.result_opt getValues ~name:"vElse"
      ~descr:(Md.plain "Domain values for false condition")
      (module JEvaluation)
  in
  Request.register_sig ~package getValues
    ~kind:`GET ~name:"getValues"
    ~descr:(Md.plain "Abstract values for the given marker")
    begin fun rq () ->
      let module A : EvaProxy = (val proxy ()) in
      let marker = get_tgt rq and callstack = get_cs rq in
      match probe marker with
      | None -> ()
      | Some probe ->
        let domain = A.evaluate probe callstack in
        set_before rq (Some domain.here);
        match domain.next with
        | After value -> set_after rq (Some value)
        | Cond (v_then, v_else) ->
          set_then rq (Some v_then);
          set_else rq (Some v_else)
        | Nothing -> ()
    end

(* -------------------------------------------------------------------------- *)
