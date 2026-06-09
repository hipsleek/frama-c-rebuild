open Cil_types

(* just here to ensure we load the corresponding transformation. *)
let _ = Ghost_cfg.transform_category

let loc = Fileloc.unknown

let report file_name s =
  let summary =
    Printf.sprintf
      "%s. Saving ghostified file in %s"
      s (Filepath.to_string_abs file_name)
  in
  Crowbar.fail summary

type stmt_pos =
  | Normal
  | Case_no_default of bool (* normal case, no default clause generated. *)
  | Case of bool (* normal case, default clause generated. *)
  | Last_case_no_default of bool (* last case, no default clause generated. *)
  | Last_case of bool (* last case, default clause generated. *)
  | Default of bool

type switch_or_loop = Is_switch | Is_loop

type env = {
  switch_or_loop: (switch_or_loop * bool) list;
  should_fail: bool;
  in_ghost: bool;
  stmt_stack: stmt list;
  stmt_pos: stmt_pos;
}

let empty_env =
  { switch_or_loop = []; should_fail = false; in_ghost = false;
    stmt_stack = []; stmt_pos = Normal
  }

let merge old_env new_env =
  { old_env with
    should_fail = old_env.should_fail || new_env.should_fail;
    stmt_stack = new_env.stmt_stack;
  }

let add_stack stmt env = { env with stmt_stack = stmt :: env.stmt_stack }

let () = Project.set_current (Project.create "simple project")

let x = Cil.makeGlobalVar "x" Cil_const.intType

let y = Cil.makeGlobalVar ~ghost:true "y" Cil_const.intType

let f = Cil.makeGlobalVar "f" Cil_const.(mk_tfun voidType (Some []) false)

let return = Cil.mkStmt (Return (None, loc))

let forward_goto_target = Cil.mkStmtOneInstr (Skip loc)

let incr_stmt =
  Cil.mkStmtOneInstr (Set (Cil.var x, Cil.increm (Cil.evar ~loc x) 1,loc))

let prepare () =
  Kernel.set_warn_status Kernel.wkey_ghost_bad_use Log.Wabort;
  Messages.reset_once_flag ();
  return.skind <- Return (None, loc);
  forward_goto_target.labels <- [Label("Unreach", loc, true)];
  let old = Project.current () in
  Project.set_current (Project.create "simple project");
  Project.remove ~project:old ()

let end_of_body = [ incr_stmt; return; forward_goto_target ]

let ghost_status env ghost =
  match env.stmt_pos with
  | Normal -> env.in_ghost || ghost
  | _ -> env.in_ghost

let gen_stmts gen_stmt =
  let open Crowbar in
  fix
    (fun gen_stmts ->
       choose [
         const (fun env -> env,[]);
         map [gen_stmt; gen_stmts]
           (fun f1 f2 env ->
              let (new_env, stmt) = f1 env in
              let env = merge env new_env in
              let (new_env, stmts) = f2 env in
              let env = merge env new_env in
              env, stmt :: stmts)])

let gen_inst ghost env =
  let ghost = ghost_status env ghost in
  let v = if ghost then y else x in
  let stmt =
    Cil.mkStmtOneInstr
      ~ghost
      (Set
         (Cil.var v,
          Cil.new_exp ~loc
            (BinOp (PlusA,Cil.evar v,Cil.one ~loc,Cil_const.intType)),loc))
  in
  let env = add_stack stmt env in
  env, stmt

let gen_block ghost f env =
  let ghost = ghost_status env ghost in
  let new_env = { env with in_ghost = ghost } in
  let (new_env, stmts) = f new_env in
  let env = merge env new_env in
  env, Cil.mkStmt ~ghost (Block (Cil.mkBlock stmts))

let gen_return ghost env =
  let ghost = ghost_status env ghost in
  let stmt = Cil.mkStmt ~ghost (Return (None, loc)) in
  let e =
    Cil.new_exp ~loc (BinOp(Lt,Cil.evar x,Cil.integer ~loc 53,Cil_const.intType))
  in
  let stmt = Cil.mkStmt ~ghost (If (e,Cil.mkBlock [stmt],Cil.mkBlock[],loc)) in
  let env = if ghost then { env with should_fail = true } else env in
  let env = add_stack stmt env in
  env, stmt

let mk_label =
  let nb = ref 0 in
  fun stmt ->
    match List.filter (function Label _ -> true | _ -> false) stmt.labels with
    | [] ->
      incr nb;
      let name = "L" ^ (string_of_int !nb) in
      stmt.labels <- [ Label (name, loc, true) ]
    | _ -> ()

(* approximation for gotos: if all the statements we jump over are ghost, we
   consider that we stay inside a ghost block. Might not be completely accurate.
*)
let rec all_ghosts n l =
  match l with
  | [] -> assert false
  | s :: _ when n = 0 -> s.ghost
  | s :: tl -> s.ghost && all_ghosts (n-1) tl

let gen_goto ghost tgt env =
  let ghost = ghost_status env ghost in
  let len = List.length env.stmt_stack in
  let tgt = tgt mod (len + 1) in
  let env, stmt =
    if tgt = len then
      begin
        let env = { env with should_fail = env.should_fail || ghost } in
        let stmt =
          Cil.mkStmt ~ghost (Goto (ref forward_goto_target, loc))
        in
        let env = add_stack stmt env in
        env, stmt
      end
    else
      begin
        let stmt = List.nth env.stmt_stack tgt in
        mk_label stmt;
        let should_fail =
          (ghost && not (all_ghosts tgt env.stmt_stack))
          || (not ghost && stmt.ghost)
        in
        let should_fail = env.should_fail || should_fail in
        let env = { env with should_fail } in
        let stmt = Cil.mkStmt ~ghost (Goto (ref stmt, loc)) in
        let env = add_stack stmt env in
        env, stmt
      end
  in
  let e = Cil.(new_exp ~loc (BinOp (Gt,evar ~loc x,integer ~loc 64, Cil_const.intType))) in
  env, Cil.mkStmt ~ghost (If (e,Cil.mkBlock [stmt],Cil.mkBlock [],loc))

let gen_break ghost env =
  let ghost = ghost_status env ghost in
  let skind, should_fail =
    match env.switch_or_loop with
    | [] -> Instr (Skip loc), false
    | (Is_loop,g) :: _ -> Break loc, not g && ghost
    | (Is_switch,g)::_ ->
      (match env.stmt_pos with
       | Normal -> Break loc, not g && ghost
       | Case g1 -> Break loc, not g && (g1 || ghost)
       | Case_no_default _ -> Break loc, false
       | Last_case g1 -> Break loc, not g && (g1 || ghost)
       | Last_case_no_default _ -> Break loc, false
       | Default g1 -> Break loc, not g && not g1 && ghost)
  in
  let should_fail = env.should_fail || should_fail in
  let stmt = Cil.mkStmt ~ghost skind in
  let e = Cil.(new_exp ~loc (BinOp (Gt,evar ~loc x,integer ~loc 75,Cil_const.intType))) in
  let stmt = Cil.mkStmt ~ghost (If (e,Cil.mkBlock [stmt],Cil.mkBlock [],loc)) in
  let env = { env with should_fail } in
  let env = add_stack stmt env in
  env, stmt

let gen_continue ghost env =
  let ghost = ghost_status env ghost in
  let is_loop = function (Is_loop,_) -> true | (Is_switch,_) -> false in
  let skind, should_fail =
    match List.find_opt is_loop env.switch_or_loop with
    | None -> Instr (Skip loc), false
    | Some (_,g) -> Continue loc, not g && ghost
  in
  let should_fail = should_fail || env.should_fail in
  let stmt = Cil.mkStmt ~ghost skind in
  let e = Cil.(new_exp ~loc (BinOp (Gt,evar ~loc x,integer ~loc 86,Cil_const.intType))) in
  let stmt = Cil.mkStmt ~ghost (If (e,Cil.mkBlock [stmt],Cil.mkBlock [],loc)) in
  let env = { env with should_fail } in
  let env = add_stack stmt env in
  env, stmt

let gen_if ghost ghost_else stmt_then stmt_else env =
  let ghost = ghost_status env ghost in
  let stmt = Cil.mkEmptyStmt ~ghost ~loc () in
  let e =
    Cil.new_exp ~loc (BinOp (Ne,Cil.evar ~loc x,Cil.zero ~loc,Cil_const.intType))
  in
  let if_env = add_stack stmt env in
  let if_env = { if_env with in_ghost = ghost } in
  let new_env, then_s = stmt_then if_env in
  let env = merge env new_env in
  let ghoste = ghost_status if_env ghost_else in
  let new_env = { if_env with in_ghost = ghoste } in
  let new_env, else_s = stmt_else new_env in
  let env = merge env new_env in
  let else_b = Cil.mkBlock else_s in
  if (not ghost) && ghoste then begin
    let attr = (Ast_attributes.frama_c_ghost_else,[]) in
    else_b.battrs <- Ast_attributes.add attr else_b.battrs;
  end;
  stmt.skind <- If(e,Cil.mkBlock then_s, Cil.mkBlock else_s,loc);
  env, stmt

let gen_default should_break stmts env =
  let ghost = env.in_ghost in
  (* default clause always has the same status as underlying switch.
     This simplifies deciding whether the check should fail or not.
  *)
  let new_env = { env with stmt_pos = Default ghost } in
  let new_env, stmts = stmts new_env in
  (* Don't make the statements in the default branch potential goto targets:
     they could be picked up by gotos inside case branches, which would become
     forward gotos, complicating the should_fail oracle computation.
  *)
  let new_env = { new_env with stmt_stack = env.stmt_stack } in
  let _,s1 = gen_inst ghost env in
  let epilogue =
    if should_break then
      [s1; Cil.mkStmt ~ghost (Break loc)]
    else
      [s1]
  in
  let stmts = stmts @ epilogue in
  let h = Cil.mkEmptyStmt ~ghost ~loc () in
  h.labels <- Default loc :: h.labels;
  let stmts = h :: stmts in
  let env = merge env new_env in
  env, Some stmts, []

let ghost_case_breaks has_default other_cases =
  let rec find_ghost_seq seq = function
    (* if there's a (non-ghost by definition) default, inspect the ghost
       cases. Otherwise, non-ghost exits the switch anyway, ghost can break
       at any time. *)
    | [] -> if has_default then seq else []
    | hd :: tl ->
      if (List.hd hd).ghost then find_ghost_seq (hd :: seq) tl else seq
  in
  let seq = find_ghost_seq [] other_cases in
  let module M = struct exception Has_break end in
  let vis =
    object
      inherit Cil.nopCilVisitor
      method! vstmt s =
        match s.skind with
        | Break _ -> raise M.Has_break
        | Loop _ | Switch _ -> (* a break there won't break the switch *)
          Cil.SkipChildren
        | _ -> Cil.DoChildren
    end
  in
  try
    List.iter
      (List.iter (fun s -> ignore (Cil.visitCilStmt vis s))) seq;
    false
  with M.Has_break -> true

let rec no_ghost_case_breaks = function
  | [] -> false (* should not happen, we have at least one non-ghost case before
                   default. *)
  | stmts :: others ->
    if (List.hd stmts).ghost then
      begin
        match (List.last stmts).skind with
        | Break _ -> false
        | _ -> no_ghost_case_breaks others
      end else true



let gen_case ghost should_break my_case cases env =
  let (env,default,others) = cases env in
  let ghost = ghost_status env ghost in
  let non_ghost_cases =
    if env.in_ghost then others else
      List.filter (fun x -> not (List.hd x).ghost) others
  in
  let has_default = match default with None -> false | Some _ -> true in
  let stmt_pos =
    match non_ghost_cases with
    | [] ->
      if has_default then Last_case ghost else Last_case_no_default ghost
    | _ -> if has_default then Case ghost else Case_no_default ghost
  in
  let should_fail =
    match stmt_pos, env.switch_or_loop with
    | (Normal | Default _),_ -> assert false
    | _,[] -> assert false
    | Last_case_no_default _,_ -> false
    | Last_case g1, (_,g2)::_ ->
      (* if the switch is non-ghost, but the last case is, and ends with
         a break, the default clause won't be taken. *)
      not g2 && g1 && should_break
    | Case_no_default g1, (_, g2)::_ ->
      (* fails iff switch is non-ghost, current case is, and doesn't break,
         going to the next case. *)
      not g2 && g1 && not should_break && no_ghost_case_breaks others
    | Case g1, (_,g2) :: _ ->
      not g2 && g1 (* prevents taking the default clause.*)
  in
  let should_fail =
    should_fail ||
    (* if non-ghost case is supposed to fall through another non-ghost case,
       we have to inspect all ghost cases in between to search for a
       break, which would indicate a failure. *)
    (not ghost && not should_break && ghost_case_breaks has_default others)
  in
  let should_fail = env.should_fail || should_fail in
  let new_env = { env with in_ghost = ghost; stmt_pos; should_fail } in
  let new_env, stmts = my_case new_env in
  let _, s1 = gen_inst ghost env in
  let epilogue =
    if should_break then
      [ s1; Cil.mkStmt ~ghost (Break loc)]
    else [s1]
  in
  let stmts = stmts @ epilogue in
  let lab_stmt = Cil.mkEmptyStmt ~ghost ~loc () in
  let stmts = lab_stmt :: stmts in
  let env = merge env new_env in
  env, default, stmts :: others

let gen_cases gen_stmt =
  let open Crowbar in
  fix
    (fun gen_cases ->
       choose [
         const (fun env -> env,None,[]);
         map [ bool; gen_stmts gen_stmt ] gen_default;
         map [ bool; bool; gen_stmts gen_stmt; gen_cases ] gen_case
       ])

let gen_switch ghost cases env =
  let ghost = ghost_status env ghost in
  let stmt = Cil.mkEmptyStmt ~ghost ~loc () in
  let new_env =
    { env with
      in_ghost = ghost;
      switch_or_loop = (Is_switch, ghost) :: env.switch_or_loop;
      stmt_stack = stmt :: env.stmt_stack;
    }
  in
  let (new_env, default, cases) = cases new_env in
  let acc =
    match default with
    | None -> [],[]
    | Some stmts -> [List.hd stmts],stmts
  in
  let count_case = ref 0 in
  let mk_switch case (labels, stmts) =
    let h = List.hd case in
    h.labels <-
      Cil_types.Case (Cil.integer ~loc !count_case, loc)
      :: h.labels;
    incr count_case;
    (h::labels, case @ stmts)
  in
  let (labels, stmts) = List.fold_right mk_switch cases acc in
  let block = Cil.mkBlock stmts in
  let env = merge env new_env in
  let env = match default with
    | None | Some [] -> env
    | Some (s :: _) -> add_stack s env
  in
  stmt.skind <- Switch (Cil.evar ~loc x,block,labels,loc);
  env, stmt

let gen_loop ghost stmts env =
  let ghost = ghost_status env ghost in
  let stmt = Cil.mkEmptyStmt ~ghost ~loc () in
  let new_env =
    { env with
      in_ghost = ghost;
      switch_or_loop = (Is_loop, ghost) :: env.switch_or_loop;
      stmt_stack = stmt :: env.stmt_stack;
    }
  in
  let (new_env, stmts) = stmts new_env in
  let e =
    Cil.new_exp ~loc (BinOp (Ge,Cil.evar x,Cil.integer ~loc 42,Cil_const.intType))
  in
  let cond_stmt =
    Cil.mkStmt ~ghost
      (If (e,Cil.mkBlock [Cil.mkStmt ~ghost (Break loc)],Cil.mkBlock [],loc))
  in
  let _,inc_stmt = gen_inst ghost new_env in
  let new_env = add_stack inc_stmt new_env in
  let stmts = cond_stmt :: stmts @ [inc_stmt] in
  let env = merge env new_env in
  stmt.skind <- Loop([],Cil.mkBlock stmts,loc,None,None);
  env, stmt

let gen_stmt =
  let open Crowbar in
  fix (fun gen_stmt ->
      choose [
        map [bool] gen_inst;
        map [bool; gen_stmts gen_stmt] gen_block;
        map [bool] gen_return;
        map [bool; uint16] gen_goto;
        map [bool] gen_break;
        map [bool] gen_continue;
        map [bool; bool; gen_stmts gen_stmt; gen_stmts gen_stmt] gen_if;
        map [bool; gen_cases gen_stmt] gen_switch;
        map [bool; gen_stmts gen_stmt] gen_loop;
      ])

let gen_body =
  Crowbar.map [gen_stmts gen_stmt]
    (fun f ->
       let (env, stmts) = f empty_env in
       let stmts = stmts @ end_of_body in
       env, Cil.mkBlock stmts)


let gen_file =
  Crowbar.map [gen_body]
    (fun (env, body) ->
       let file = Crowbar_utils.generate_cil_file "ghostified" in
       let f = Cil.emptyFunctionFromVI f in
       f.svar.vdefined <- true;
       f.sbody <- body;
       (env,
        { file with
          globals = [
            GVarDecl (x,Fileloc.unknown);
            GVarDecl (y,Fileloc.unknown);
            GFun (f, Fileloc.unknown)
          ]
        }))

let ignore_deferred_errors () =
  try Log.treat_deferred_error () with Log.AbortError _ -> ()

let success_remove file =
  Filesystem.remove_file file.fileName;
  true

let check_file (env, file) =
  prepare();
  let file_name = file.fileName in
  Crowbar_utils.generate_file file;
  let success =
    try
      File.prepare_cil_file file;
      Log.treat_deferred_error ();
      true
    with
    | Log.AbortError _ ->
      ignore_deferred_errors ();
      false
    | exn ->
      Printf.printf
        "Uncaught exception: %s\n%t\nFile saved in %s\n%!"
        (Printexc.to_string exn)
        Printexc.print_backtrace
        (Filepath.to_string_abs file_name);
      ignore_deferred_errors ();
      report file_name "Found code leading to an unknown exception"
  in
  if env.should_fail && success then
    report file_name "Found ghost code that should not have been accepted"
  else if not (env.should_fail) then begin
    if success then begin
      let norm_buf = Buffer.create 150 in
      let fmt = Format.formatter_of_buffer norm_buf in
      let f = Globals.Functions.find_by_name "f" in
      Kernel_function.pretty fmt f;
      let prj2 = Project.create "copy" in
      Project.set_current prj2;
      Kernel.Files.set [ file_name ];
      let parse_success =
        try
          File.init_from_cmdline (); true
        with
          Log.AbortError _ -> ignore_deferred_errors (); false
      in
      if parse_success then begin
        let copy_buf = Buffer.create 150 in
        let fmt = Format.formatter_of_buffer copy_buf in
        let f = Globals.Functions.find_by_name "f" in
        Kernel_function.pretty fmt f;
        if Buffer.contents norm_buf <> Buffer.contents copy_buf then begin
          let norm = open_out (Filepath.to_string_abs file_name ^ ".norm.c") in
          Buffer.output_buffer norm norm_buf;
          flush norm;
          close_out norm;
          let copy = open_out (Filepath.to_string_abs file_name ^ ".copy.c") in
          Buffer.output_buffer copy copy_buf;
          flush copy;
          close_out copy;
          report file_name "Found ghost code not well pretty-printed"
        end else success_remove file
      end else begin
        report file_name "Error during re-parsing of pretty-printed code"
      end
    end
    else
      report file_name "Found ghost code that should have been accepted"
  end
  else success_remove file

let () = Log.set_echo false

let f () =
  Crowbar.add_test ~name:"ghost cfg" [gen_file]
    (fun res -> Crowbar.check (check_file res))

let () = Crowbar_utils.run "test_ghost_cfg" f
