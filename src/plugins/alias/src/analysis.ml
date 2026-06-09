(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

module Dataflow = Dataflow2

module type Table = sig
  type key
  type value
  val find: key -> value
  (** @raise Not_found if the key is not in the table. *)
end

module type InternalTable = sig
  include Table
  val add : key -> value -> unit
  val iter : (key -> value -> unit) -> unit
end

module Make_table (H: Hashtbl.S) (V: sig type t val size : int end)
  : InternalTable with type key = H.key and type value = V.t = struct
  type key = H.key
  type value = V.t
  let tbl = H.create V.size
  let add = H.replace tbl
  let find = H.find tbl
  let iter f = H.iter f tbl
end

(* In Function_table, value None means the function has no definition *)
module Function_table = Make_table (Kernel_function.Hashtbl) (struct
    type t = Abstract_state.summary option
    let size = 7
  end)

let analyse_function_ref = Extlib.mk_fun "analyse_function"

let function_summary kf =
  try Function_table.find kf
  with Not_found ->
    let result = !analyse_function_ref kf in
    Function_table.add kf result;
    result

(* In Stmt_table, value None means abstract state = Bottom *)
module Stmt_table = struct
  include Dataflow.StartData (struct
      type t = Abstract_state.t option
      let size = 7
    end)
  type key = stmt
  type value = data
end

(* Abstract state after taking into account all global variable definitions *)
let initial_state = ref @@ Some Abstract_state.empty

let warn_unsupported_explicit_pointer pp_obj obj loc =
  Options.warning ~source:(fst loc) ~wkey:Options.Warn.unsupported_address
    "unsupported feature: explicit pointer address: %a; analysis may be unsound" pp_obj obj

let do_assignment (lv:lval) exp_o (s:Abstract_state.t) : Abstract_state.t =
  try Abstract_state.assignment s lv exp_o
  with Simplified.Explicit_pointer_address loc ->
    warn_unsupported_explicit_pointer (Pretty_utils.pp_opt Printer.pp_exp) exp_o loc;
    s

let do_init (lv:lval) (init : init_or_str option) state =
  let rec aux lv init state =
    match init with
    | None -> Option.map (do_assignment lv None) state
    | Some (SingleInit e) -> Option.map (do_assignment lv (Some e)) state
    | Some (CompoundInit (_, l)) ->
      List.fold_left (fun state (o, init) -> aux (Cil.addOffsetLval o lv) (Some init) state) state l
  in
  match init with
  | Some (CInit init) -> aux lv (Some init) state
  | Some StrInit _ ->
    (* TODO: consider potential aliases for literal with common suffixes. *)
    state
  | None -> aux lv None state


let pp_abstract_state_opt ?(debug=false) fmt v =
  match v with
  | None -> Unicode.pp_bottom fmt
  | Some a -> Abstract_state.pretty ~debug fmt a

let analyse_global_var v initinfo st =
  Options.feedback ~level:3
    "@[analysing global variable definition:@ @[%a@]@ =@ @[%a@];@]"
    Printer.pp_varinfo v
    Printer.pp_initinfo initinfo;
  let result = do_init (Var v, NoOffset) initinfo.init st in
  Options.feedback ~level:3
    "@[May-aliases after global variable definition@;<2>@[%a@]@;<2>are@;<2>@[%a@]@]"
    Printer.pp_varinfo v
    (pp_abstract_state_opt ~debug:false) result;
  Options.debug ~level:3
    "@[May-alias graph after global variable definition@;<2>@[%a@]@;<2>is@;<4>@[%a@]@]"
    Printer.pp_varinfo v
    (pp_abstract_state_opt ~debug:true) result;
  result

let do_function_call (stmt:stmt) state (res : lval option) (f : lhost) (args: exp list) loc =
  let is_malloc (s:string) : bool =
    (s = "malloc") || (s = "calloc") (* todo : add all function names *)
  in
  match f with
  | Var v  when is_malloc v.vname ->
    (* special case for malloc *)
    begin match (state, res) with
      | (None, _) -> None
      | (Some a, None) -> (Options.warning "Memory allocation not stored (ignored)"; Some a)
      | (Some a, Some lv) ->
        try Some (Abstract_state.assignment_x_allocate_y a lv)
        with Simplified.Explicit_pointer_address loc ->
          warn_unsupported_explicit_pointer Printer.pp_stmt stmt loc;
          Some a
    end
  | _ -> (* general case *)
    let summaries =
      match Kernel_function.get_called f with
      | Some kf when Kernel_function.is_main kf -> []
      | Some kf -> [function_summary kf]
      | None -> (* dereference function pointer using the results of the points-to analysis *)
        let lvf = f,NoOffset in
        begin match Stmt_table.find stmt with
          | Some state ->
            let targets = Abstract_state.find_vars lvf state in
            Options.feedback ~level:3 "%a is an indirect function call to one of @[%a@]"
              Printer.pp_stmt stmt
              Abstract_state.VarSet.pretty targets;
            let kf_of_var {vname; _} =
              try Some (Globals.Functions.find_def_by_name vname) with Not_found -> None
            in
            let kfs = Seq.filter_map kf_of_var @@ Abstract_state.VarSet.to_seq targets in
            List.of_seq @@ Seq.map function_summary kfs
          | _ ->
            Options.fatal "unsupported call to function pointer: %a" Lval.pretty lvf
        end
    in
    let apply_summary state summary =
      match (state, summary) with
      | (None, _) -> None
      | (Some a, Some summary) -> Some (Abstract_state.call a res args summary)
      | (Some a, None) ->
        Options.warning ~wkey:Options.Warn.undefined_function ~once:true ~source:(fst loc)
          "function %a has no definition" Printer.pp_lhost f;
        Some a
    in
    List.fold_left apply_summary state summaries

let do_cons_init (s:stmt) (v:varinfo) f arg t loc state =
  Cil.treat_constructor_as_func (do_function_call s state) v f arg t loc

let analyse_instr (s:stmt) (i:instr) (a:Abstract_state.t option) : Abstract_state.t option =
  match i with
  | Set (lv,exp,_) -> Option.map (do_assignment lv (Some exp)) a
  | Local_init (v,AssignInit i,_) -> do_init (Var v, NoOffset) (Some (CInit i)) a
  | Local_init (v,ConsInit (f,arg,t),loc) -> do_cons_init s v f arg t loc a
  | Code_annot _ -> a
  | Skip _ -> a
  | Call (res,ef,es,loc) -> do_function_call s a res ef es loc
  | Asm (_,_,_,loc) ->
    Options.warning
      ~source:(fst loc) ~wkey:Options.Warn.unsupported_asm
      "unsupported feature: assembler code; skipping";
    a

let do_instr (s:stmt) (i:instr) (a:Abstract_state.t option) : Abstract_state.t option =
  Options.feedback ~level:3 "@[analysing instruction:@ %a@]" Printer.pp_stmt s;
  let result = analyse_instr s i a in
  Options.feedback ~level:3 "@[May-aliases after instruction@;<2>@[%a@]@;<2>are@;<2>@[%a@]@]"
    Printer.pp_stmt s (pp_abstract_state_opt ~debug:false) result;
  Options.debug ~level:3 "@[May-alias graph after instruction@;<2>@[%a@]@;<2>is@;<4>@[%a@]@]"
    Printer.pp_stmt s (pp_abstract_state_opt ~debug:true) result;
  result

module T = struct
  let name = "alias"

  let debug = true (* TODO see options *)

  type t = Abstract_state.t option

  module StmtStartData = Stmt_table

  let copy x = x (* we only have persistent data *)

  let pretty fmt a =
    match a with
    | None -> Format.fprintf fmt "<No abstract state>"
    | Some a -> Abstract_state.pretty fmt a

  let computeFirstPredecessor _ a = a

  let combinePredecessors _stmt ~old state =
    match old, state with
    | _, None -> assert false
    | None, Some _ -> Some state (* [old] already included in [state] *)
    | Some old, Some new_ ->
      if Abstract_state.is_included new_ old then
        None
      else
        Some (Some (Abstract_state.union old new_))

  let doInstr = do_instr

  let doGuard _ _ a = Dataflow.GUse a, Dataflow.GUse a

  let doStmt _ _ = Dataflow.SDefault

  let doEdge _ _ a = a
end

module F = Dataflow.Forwards (T)

let do_stmt (a: Abstract_state.t) (s:stmt) :  Abstract_state.t =
  match s.skind with
  | Instr i ->
    begin match do_instr s i (Some a) with
      | None -> Options.fatal "problem here"
      | Some a -> a
    end
  | _ -> a

let analyse_function (kf:kernel_function) =
  let final_state =
    if not @@ Kernel_function.has_definition kf then None else
      let () =
        Options.feedback ~level:2 "analysing function: %a"
          Kernel_function.pretty kf
      in
      let first_stmt =
        try Kernel_function.find_first_stmt kf
        with Kernel_function.No_Statement -> assert false
      in
      T.StmtStartData.add first_stmt !initial_state;
      F.compute [first_stmt];
      let return_stmt = Kernel_function.find_return kf in
      try Stmt_table.find return_stmt
      with Not_found ->
        let source, _ = Kernel_function.get_location kf in
        Options.warning ~source ~wkey:Options.Warn.no_return_stmt
          "function %a does not return; analysis may be unsound"
          Kernel_function.pretty kf;
        !initial_state
  in
  let level = if Kernel_function.is_main kf then 1 else 2 in
  final_state |> Option.iter (fun s ->
      Options.feedback ~level "@[May-aliases at the end of function %a:@ @[%a@]"
        Kernel_function.pretty kf
        (Abstract_state.pretty ~debug:false) s;
      Options.debug ~level "May-alias graph at the end of function %a:@;<4>@[%a@]"
        Kernel_function.pretty kf
        (Abstract_state.pretty ~debug:true)s;
    );
  let result =
    match final_state with
    (* final state is None if kf has no definition *)
    | None -> None
    | Some fs ->
      let summary = Abstract_state.make_summary fs kf in
      Options.debug ~level:2 "Summary of function %a:@ @[%a@]"
        Kernel_function.pretty kf
        (Abstract_state.pretty_summary ~debug:false) summary;
      Some summary
  in
  if Kernel_function.is_main kf then begin
    match Options.Dot_output.get (), final_state with
    | "", _ -> ()
    | _, None -> ()
    | fname, Some final_state -> Abstract_state.print_dot fname final_state
  end;
  result

let () = analyse_function_ref := analyse_function

let make_summary (state:Abstract_state.t) (kf:kernel_function) =
  match function_summary kf with
  | Some s -> (state, s)
  | None -> Options.fatal "not implemented"

let computed_flag = ref false

let is_computed () = !computed_flag

let print_stmt_table_elt fmt k v :unit =
  let print_key = Stmt.pretty in
  let print_value fmt v =
    match v with
    | None -> Format.fprintf fmt "<Bot>"
    | Some a -> Abstract_state.pretty ~debug:(Options.DebugTable.get ()) fmt a
  in
  Format.fprintf fmt "Before statement %a :@[<hov 2> %a@]@." print_key k print_value v

let print_function_table_elt fmt kf s : unit =
  let function_name = Kernel_function.get_name kf in
  match s with
  | None -> Options.debug "function %s -> None" function_name
  | Some s ->
    Format.fprintf fmt "Summary of function %s:@;<5 2>@[%a@]@."
      function_name
      (Abstract_state.pretty_summary ~debug:(Options.DebugTable.get ())) s

let compute () =
  Ast.compute ();
  initial_state := Globals.Vars.fold_in_file_order analyse_global_var (Some Abstract_state.empty);
  Globals.Functions.iter (fun kf -> ignore @@ function_summary kf);
  computed_flag := true;
  if Options.ShowStmtTable.get () then
    Stmt_table.iter (print_stmt_table_elt Format.std_formatter);
  if Options.ShowFunctionTable.get () then
    Function_table.iter (print_function_table_elt Format.std_formatter);
  Options.debug ~level:2 "node counter: %d" !Abstract_state.node_counter

let clear () =
  computed_flag := false;
  initial_state := Some Abstract_state.empty;
  Stmt_table.clear ()

let get_state_before_stmt stmt =
  if is_computed ()
  then try Stmt_table.find stmt with Not_found -> None
  else None

let get_summary kf =
  if is_computed ()
  then try Function_table.find kf with Not_found -> None
  else None
