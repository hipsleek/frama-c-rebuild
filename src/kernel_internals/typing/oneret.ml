(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

open Cil_types
open Logic_const

let adjust_assigns_clause loc var code_annot =
  let change_result = object
    inherit Cil.nopCilVisitor
    method! vterm_lhost = function
      | TResult _ -> ChangeTo (TVar var)
      | TVar _ | TMem _ -> DoChildren
  end
  in
  let change_term t = Cil.visitCilTerm change_result t in
  let module M = struct exception Found end in
  let check_var = object
    inherit Cil.nopCilVisitor
    method! vterm_lhost = function
      | TVar v when Cil_datatype.Logic_var.equal var v -> raise M.Found
      | TVar _ | TResult _ | TMem _ -> DoChildren
  end
  in
  let contains_var l =
    try ignore (Cil.visitCilAssigns check_var (Writes l)); false
    with M.Found -> true
  in
  let change_from = function
    | FromAny -> FromAny
    | From l -> From (List.map Logic_const.refresh_identified_term l)
  in
  let adjust_lval (_,assigns as acc) (loc,from) =
    if Logic_utils.contains_result loc.it_content then begin
      true,
      (Logic_const.new_identified_term (change_term loc.it_content),
       change_from from)::assigns
    end else acc
  in
  let adjust_clause b =
    match b.b_assigns with
    | WritesAny -> ()
    | Writes l ->
      if not (contains_var l) then begin
        let (changed, a) = List.fold_left adjust_lval (false,l) l in
        let a =
          if changed then a
          else
            (Logic_const.new_identified_term (Logic_const.tvar ~loc var),
             FromAny)
            :: a
        in
        b.b_assigns <- Writes a
      end
  in
  match code_annot with
  | AStmtSpec (_,s) -> List.iter adjust_clause s.spec_behavior
  | _ -> ()

type returns_clause =
  Cil_types.stmt * Cil_types.behavior * Cil_types.identified_predicate

type goto_annot =
  Cil_types.stmt * Cil_types.code_annotation

type callback = returns_clause -> goto_annot list -> unit

let collect_returns (ca : Cil_types.code_annotation) =
  match ca.annot_content with
  | AStmtSpec(_bhvs,spec) ->
    List.fold_left
      (fun acc bhv ->
         List.fold_left
           (fun acc (kind,predicate) ->
              match kind with
              | Returns -> (bhv,predicate) :: acc
              | _ -> acc
           ) acc bhv.b_post_cond
      ) [] spec.spec_behavior
  | _ -> []

let encapsulate_local_vars f =
  (* We must ensure that our gotos to the end of the function do not bypass
     declarations of objects with destructors, or we may miss to insert the
     corresponding destructor calls. Here is an example, with T a class:

     extern void Ctor(T* x);

     int f(void) {
       goto L;
       T __attribute__((__fc_destructor)) x;
       Ctor(&x);
       L: return 42;
     }

     Treating such a special case may be hard and not sufficient. Then, although
     coarse, our general solution is to enclose the function body (except the
     return statement and the declaration of the retvar) inside a block. *)
  if
    List.exists (fun v ->
        Ast_attributes.(contains frama_c_destructor v.vattr)
      ) f.slocals
  then begin
    let exception Found of (block Stack.t * stmt) in
    let vis = object
      val sb = Stack.create ()
      inherit Cil.nopCilVisitor
      method! vblock b =
        Stack.push b sb;
        DoChildrenPost (fun b -> ignore (Stack.pop sb); b)

      method! vstmt s =
        match s.skind with
        | Return _ -> raise (Found (sb, s));
        | Instr _ -> SkipChildren
        | _ -> DoChildren
    end
    in
    let stack_block, ret_stmt =
      try
        ignore (Cil.visitCilBlock vis f.sbody);
        Kernel.fatal "No return statement found inside %a"
          Cil_printer.pp_varinfo f.svar
      with Found res -> res
    in
    let ret_block = Stack.top stack_block in
    let ret_block_bstmts =
      List.filter
        (fun s -> match s.skind with Return _ -> false | _ -> true)
        ret_block.bstmts
    in
    ret_block.bstmts <- ret_block_bstmts;
    let s1 = Cil.mkStmt (Block f.sbody) in
    let b = Cil.mkBlock [s1; ret_stmt] in
    (* As we filtered [ret_stmt] out its original block, we now need to move the
       declaration of the return variable into [b]. Before doing that, we must
       remove such a declaration from its original block, which lies somewhere
       in the stack of blocks [stack_block]. *)
    let retvar =
      match ret_stmt.skind with
      | Return(None, _) -> None
      | Return(Some {enode = Lval (Var v,NoOffset)}, _) -> Some v
      | Return _ ->
        Kernel.fatal "Return node in unexpected format after oneret call"
      | _ ->
        Kernel.fatal "find_return did not return Return node"
    in
    let filter_vi vi block =
      block.blocals <-
        List.filter (fun vi' -> not (Cil_datatype.Varinfo.equal vi vi'))
          block.blocals
    in
    let iter_block vi = Stack.iter (filter_vi vi) stack_block in
    Option.iter iter_block retvar;
    (* We can now safely add [retvar] into [b]'s declarations, and use [b] as
       the new function's body block. *)
    b.blocals <- Option.to_list retvar;
    f.sbody <- b
  end

let oneret ?(callback: callback option) (f: fundec) : unit =
  let open Current_loc.Operators in
  let fname = f.svar.vname in
  (* Get the return type *)
  let retTyp =
    match f.svar.vtype.tnode with
      TFun(rt, _, _) -> rt
    | _ ->
      Kernel.fatal "Function %s does not have a function type" f.svar.vname
  in
  (* Does it return anything ? *)
  let hasRet = match Ast_types.unroll_node retTyp with TVoid -> false | _ -> true in

  (* Memoize the return result variable. Use only if hasRet *)
  let lastloc = ref Fileloc.unknown in
  let getRetVar =
    let retVar : varinfo option ref = ref None in
    fun loc ->
      match !retVar with
      | None ->
        let ghost = f.svar.vghost in
        (* don't collide *)
        let rv = Cil.makeLocalVar ~loc ~ghost ~temp:true f "__retres" retTyp in
        retVar := Some rv;
        rv
      | Some rv ->
        if rv.vdecl = Fileloc.unknown then
          rv.vdecl <- loc;
        rv
  in
  let convert_result p =
    let vis = object
      inherit Cil.nopCilVisitor
      method! vterm_lhost = function
        | TResult _ ->
          let v = getRetVar Fileloc.unknown in
          ChangeTo (TVar (Cil.cvar_to_lvar v))
        | TMem _ | TVar _ -> DoChildren
    end
    in Cil.visitCilPredicate vis p
  in
  let assert_of_returns ca =
    match ca.annot_content with
    | AAssert _ | AInvariant _ | AVariant _
    | AAssigns _ | AAllocation _ | AExtended _ -> ptrue
    | AStmtSpec (_bhvs,s) ->
      let res =
        List.fold_left
          (fun acc bhv ->
             pand
               (acc,
                pimplies
                  (pands
                     (List.map
                        (fun p ->
                           let p = Logic_const.pred_of_id_pred p in
                           pold ~loc:p.pred_loc p)
                        bhv.b_assumes),
                   pands
                     (List.fold_left
                        (fun acc (kind,p) ->
                           match kind with
                             Returns -> Logic_const.pred_of_id_pred p :: acc
                           | Normal | Exits | Breaks | Continues -> acc)
                        [ptrue] bhv.b_post_cond)
                  )))
          ptrue s.spec_behavior
      in convert_result res
  in
  (* Remember if we have introduced goto's *)
  let haveGoto = ref false in
  (* Memoize the return statement *)
  let retStmt : stmt ref = ref Cil_datatype.Stmt.dummy in
  let getRetStmt (_x: unit) : stmt =
    if !retStmt == Cil_datatype.Stmt.dummy then begin
      let sr =
        let getLastLoc () = (* CEA modified to have a good [!lastloc] *)
          let rec setLastLoc = function
            | [] -> ()
            | {skind=Block b} :: [] -> setLastLoc b.bstmts
            | {skind=UnspecifiedSequence seq}::[] ->
              setLastLoc (List.map (fun (x,_,_,_,_) -> x) seq)
            | {skind= _} as s :: [] -> lastloc := Cil_datatype.Stmt.loc s
            | {skind=_s} :: l -> setLastLoc l
          in
          setLastLoc f.sbody.bstmts; !lastloc
        in
        let loc = getLastLoc () in
        (* Must create a statement *)
        let rv =
          if hasRet then
            Some (Cil.new_exp ~loc (Lval(Var (getRetVar loc), NoOffset)))
          else None
        in
        Cil.mkStmt (Return (rv, loc))
      in retStmt := sr;
      sr
    end else
      !retStmt
  in
  (* Stack of predicates that must hold in case of returns
     (returns clause with \old transformed into \at(,L) for a suitable L).
     TODO: split that into behaviors and generates for foo,bar: assert instead
     of plain assert.
  *)
  let returns_stack :
    (Cil_types.predicate * Cil_types.stmt * Cil_types.code_annotation) Stack.t
    = Stack.create () in
  let popn n =
    try for _ = 1 to n do ignore (Stack.pop returns_stack) done
    with Stack.Empty -> assert false
  in
  let to_callback = Hashtbl.create 8 in
  let do_callback cb = Hashtbl.iter (fun _ (ca,gs) -> cb ca gs) to_callback in
  let register_goto (ca : returns_clause) (gc : goto_annot) =
    let (_,_, { ip_id }) = ca in
    let gs = try snd (Hashtbl.find to_callback ip_id) with Not_found -> [] in
    Hashtbl.replace to_callback ip_id (ca,gc::gs)
  in
  (* Now scan all the statements. Know if you are the main body of the
   * function and be prepared to add new statements at the end.
   * popstack indicates whether we should pop the stack after having analyzed
     current statement. It is an int since nothing in ACSL prevents from having
     multiple statement contracts on top of each other before finding an
     actual statement...
  *)
  let rec scanStmts acc (mainbody: bool) popstack = function
    | [] when mainbody -> (* We are at the end of the function. Now it is
                           * time to add the return statement *)
      let rs = getRetStmt () in
      if !haveGoto then
        rs.labels <- (Label("return_label", !lastloc, false)) :: rs.labels;
      List.rev (rs :: acc)

    | [] -> List.rev acc

    | [{skind=Return (Some ({enode = Lval(Var _,NoOffset)}), _l)} as s]
      when mainbody && not !haveGoto ->
      (* We're not changing the return into goto, so returns clause will still
         have effect.
      *)
      popn popstack;
      List.rev (s::acc)

    | ({skind=Return (retval, loc)} as s) :: rests ->
      let<> UpdatedCurrentLoc = loc in
    (*
      ignore (E.log "Fixing return(%a) at %a\n"
      insert
      (match retval with None -> text "None"
      | Some e -> d_exp () e)
      d_loc l);
     *)
      if hasRet && retval = None then
        Kernel.fatal ~current:true
          "Found return without value in function %s" fname;
      if not hasRet && retval <> None then
        Kernel.fatal ~current:true "Found return in subroutine %s" fname;
      (* Keep this statement because it might have labels. But change it to
       * an instruction that sets the return value (if any). *)
      s.skind <- begin
        match retval with
          Some rval -> Instr (Set((Var (getRetVar loc), NoOffset), rval, loc))
        | None -> Instr (Skip loc)
      end;
      let returns_assert = ref ptrue in
      Stack.iter
        (fun (p,_,_) -> returns_assert := pand ~loc (p, !returns_assert))
        returns_stack;
      (match retval with
       | Some _ ->
         let lvar = Cil.cvar_to_lvar (getRetVar loc) in
         Stack.iter
           (fun (_,_,ca) -> adjust_assigns_clause loc lvar ca.annot_content)
           returns_stack
       | None -> () (* There's no \result: no need to adjust it *)
      );
      (* See if this is the last statement in function, and we don't
         have a statement contract above us. In that last case, it is best
         to keep a small block with a goto the actual return statement, so
         as to preserve the fact that we don't fall through the return
         statement. In particular, the following contract holds, and so should
         its normalization:

         /*@ returns \true; ensures \false; */
         returns 0;
      *)
      if mainbody && rests == [] && popstack = 0 then begin
        (* last statement, no contract, we can just fall through. *)
        scanStmts (s :: acc) mainbody 0 rests
      end else begin
        (* Add a Goto and put everything into a block on which
           the statement contract(s) will apply.
        *)
        let sgref = ref (getRetStmt ()) in
        let sg = Cil.mkStmt ~ghost:s.ghost (Goto (sgref, loc)) in
        haveGoto := true;
        let b_stmts =
          match !returns_assert with
          | { pred_content = Ptrue } -> [s; sg]
          | p ->
            let p = Logic_const.toplevel_predicate p in
            let a = Logic_const.new_code_annotation (AAssert ([],p)) in
            let sta = Cil.mkStmt (Instr (Code_annot (a,loc))) in
            if callback<>None then
              ( let gclause = sta , a in
                Stack.iter
                  (fun (_,str,ca) ->
                     List.iter
                       (fun (bhv,ret) -> register_goto (str,bhv,ret) gclause)
                       (collect_returns ca)
                  ) returns_stack ) ;
            [ s; sta; sg ]
        in
        let s = Cil.mkStmt (Block (Cil.mkBlock b_stmts)) in
        popn popstack;
        scanStmts (s :: acc) mainbody 0 rests
      end

    | ({skind=If(eb,t,e,l)} as s) :: rests ->
      (*CEA currentLoc := l;*)
      s.skind <- If(eb, scanBlock false t, scanBlock false e, l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({skind=Loop(a,b,l,lb1,lb2)} as s) :: rests ->
      (*CEA currentLoc := l;*)
      s.skind <- Loop(a,scanBlock false b, l,lb1,lb2);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({skind=Switch(e, b, cases, l)} as s) :: rests ->
      (*CEA currentLoc := l;*)
      s.skind <- Switch(e, scanBlock false b, cases, l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | [{skind=Block b} as s] when popstack = 0 ->
      (* if we have a statement contract just above (i.e popstack<>0),
         don't consider that we are in the main body.
         Depending on the semantics we want to give to ACSL contracts
         wrt control flow that jumps in the middle of a block over which
         a statement contract applies, this might lead to incorrect contract
         in the end. For now, it's safer to ensure that the goto
         is directed outside of such a block. *)
      s.skind <- Block (scanBlock mainbody b);
      popn popstack;
      List.rev (s::acc)
    | ({skind=Block b} as s) :: rests ->
      s.skind <- Block (scanBlock false b);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    (* Don't try to put a return inside a UnspecifiedSequence: this would
       mean that it could be evaluated at any point of the sequence. *)
    | ({skind = UnspecifiedSequence seq} as s) :: rests ->
      s.skind <-
        UnspecifiedSequence
          (List.concat
             (List.map (fun (s,m,w,r,c) ->
                  let res = scanStmts [] false 0 [s] in
                  (List.hd res,m,w,r,c)::
                  (List.map (fun x -> x,[],[],[],[]) (List.tl res)))
                 seq));
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | {skind=Instr(Code_annot ({ annot_content = AStmtSpec _ } as ca,_))} as s
      :: rests ->
      let returns = assert_of_returns ca in
      let returns = Logic_utils.translate_old_label s returns in
      Stack.push (returns,s,ca) returns_stack;
      scanStmts (s::acc) mainbody (popstack + 1) rests
    | { skind = Instr (Code_annot _) } as s :: rests ->
      scanStmts (s::acc) mainbody popstack rests
    | { skind = TryCatch(t,c,l) } as s :: rests ->
      let scan_one_catch (e,b) = (e,scanBlock false b) in
      let t = scanBlock false t in
      let c = List.map scan_one_catch c in
      s.skind <- TryCatch(t,c,l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({skind=(Goto _ | Instr _ | Continue _ | Break _
              | TryExcept _ | TryFinally _ | Throw _)} as s)
      :: rests ->
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests

  and scanBlock (mainbody: bool) (b: block) =
    { b with bstmts = scanStmts [] mainbody 0 b.bstmts;}

  in
  (*CEA since Current_loc isn't set
    ignore (visitCilBlock dummyVisitor f.sbody) ; *)(* sets Current_loc *)
  (*CEA so, [scanBlock] will set [lastloc] when necessary
    lastloc := !currentLoc ;  *) (* last location in the function *)
  f.sbody <- scanBlock true f.sbody ;
  if !haveGoto && !retStmt != Cil_datatype.Stmt.dummy then
    encapsulate_local_vars f;
  Option.iter do_callback callback
