(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let pretty_with_indirect fmt v =
  let deps = Functionwise.get v in
  From_memory.pretty_with_type_indirect (Kernel_function.get_type v) fmt deps

let display fmtopt =
  Option.iter (fun fmt -> Format.fprintf fmt "@[<v>") fmtopt;
  Callgraph.Uses.iter_in_rev_order
    (fun kf ->
       if Eva.Results.is_called kf then
         let header fmt =
           Format.fprintf fmt "Function %a:" Kernel_function.pretty kf
         in
         let pretty =
           if From_parameters.ShowIndirectDeps.get ()
           then pretty_with_indirect
           else Functionwise.pretty
         in
         match fmtopt with
         | None ->
           From_parameters.printf ~header "@[  %a@]" pretty kf
         | Some fmt ->
           Format.fprintf fmt "@[%t@]@ @[  %a]" header pretty kf
    );
  Option.iter (fun fmt -> Format.fprintf fmt "@]") fmtopt

module SortCalls = struct
  type t = stmt
  (* Sort first by original source code location, then by sid *)
  let compare s1 s2 =
    let r = Fileloc.compare
        (Cil_datatype.Stmt.loc s1) (Cil_datatype.Stmt.loc s2) in
    if r = 0
    then Cil_datatype.Stmt.compare s1 s2 (* This is not really stable, but no
                                            good criterion is left *)
    else r
end
module MapStmtCalls = Map.Make(SortCalls)

let iter_callwise_calls_sorted f =
  let hkf = Kernel_function.Hashtbl.create 17 in
  let kglobal = ref None in
  Callwise.iter
    (fun ki d ->
       match ki with
       | Kglobal -> kglobal := Some d
       | Kstmt s ->
         let kf = Kernel_function.find_englobing_kf s in
         let m =
           try Kernel_function.Hashtbl.find hkf kf
           with Not_found ->  MapStmtCalls.empty
         in
         let m = MapStmtCalls.add s d m in
         Kernel_function.Hashtbl.replace hkf kf m
    );
  Callgraph.Uses.iter_in_rev_order
    (fun kf ->
       try
         let m = Kernel_function.Hashtbl.find hkf kf in
         MapStmtCalls.iter (fun s d -> f (Kstmt s) d) m
       with Not_found -> ()
    );
  match !kglobal with
  | None -> ()
  | Some d -> f Kglobal d

let print_deps () =
  From_parameters.feedback
    "====== DEPENDENCIES COMPUTED ======@\n\
     These dependencies hold at termination for the executions that terminate:";
  display None;
  From_parameters.feedback "====== END OF DEPENDENCIES ======"

let print_calldeps () =
  let treat_call s funtype =
    let caller = Kernel_function.find_englobing_kf s in
    let f, typ_f =
      if not (Eva.Analysis.save_results caller)
      then "<unknown>", funtype
      else
        match Eva.Results.callee s with
        | kf :: _ ->
          Pretty_utils.to_string Kernel_function.pretty kf,
          Kernel_function.get_type kf
        | [] ->
          From_parameters.fatal
            ~source:(fst (Cil_datatype.Stmt.loc s))
            "Invalid call %a@." Printer.pp_stmt s
    in
    (fun fmt ->
       Format.fprintf fmt "@[call to %s at %a (by %a)%t:@]"
         f
         Fileloc.pretty (Cil_datatype.Stmt.loc s)
         Kernel_function.pretty caller
         (fun fmt ->
            if From_parameters.debug_atleast 1 then
              Format.fprintf fmt " <sid %d>" s.Cil_types.sid)
    ),
    typ_f
  in
  From_parameters.feedback "====== DISPLAYING CALLWISE DEPENDENCIES ======";
  iter_callwise_calls_sorted
    (fun ki d ->
       let header, typ =
         match ki with
         | Kglobal ->
           (fun fmt -> Format.fprintf fmt "@[entry point:@]"),
           Kernel_function.get_type (fst (Globals.entry_point ()))
         | Kstmt ({skind = Instr (Call (_, vkf, _, _))} as s) ->
           treat_call s (Cil.typeOfLhost vkf)
         | Kstmt ({skind = Instr (Local_init(_,ConsInit(f,_,_),_))} as s)->
           treat_call s f.vtype
         | _ -> assert false (* Not a call *)
       in
       From_parameters.printf ~header
         "@[  %a@]"
         ((if From_parameters.ShowIndirectDeps.get ()
           then From_memory.pretty_with_type_indirect
           else From_memory.pretty_with_type) typ)
         d);
  From_parameters.feedback "====== END OF CALLWISE DEPENDENCIES ======"


let output_deps () =
  Functionwise.compute_all ();
  if From_parameters.verbose_atleast 1 then print_deps ()

let output_calldeps () =
  Callwise.compute_all_calldeps ();
  if From_parameters.verbose_atleast 1 then print_calldeps ()

let output_deps_once =
  let name = "From.output_deps" in
  State_builder.apply_once name [ Functionwise.self ] output_deps |> fst

let output_calldeps_once =
  let name = "From.output_calldeps" in
  State_builder.apply_once name [ Callwise.self ] output_calldeps |> fst

let main () =
  if From_parameters.ForceDeps.get () then output_deps_once ();
  if From_parameters.ForceCallDeps.get () then output_calldeps_once ()

let () = Boot.Main.extend main
