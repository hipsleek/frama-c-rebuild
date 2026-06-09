open Cil_types

module StmtSet = Cil_datatype.Stmt.Hptset

let get = function
  | None -> "none"
  | Some stmt -> string_of_int stmt.sid

let pp_res =
  Pretty_utils.pp_list ~pre:"(" ~sep:", " ~suf:")" Format.pp_print_string

(** For each statement of [f], find its immediate dominator and postdominator
    and print the triplets. *)
let print_immediate f =
  let res =
    List.map (fun s ->
        let dom = Dominators.get_idom s in
        let postdom = Dominators.get_ipostdom s in
        [string_of_int s.sid; get dom; get postdom]
      ) f.sallstmts
  in
  Format.printf "@[<v2>Immediate dominators of %s (sid, idom, ipostdom):@;%a@]@;"
    f.svar.vname
    (Pretty_utils.pp_list ~pre:"@[" ~sep:",@ " ~suf:"@]" pp_res) res

(* Make sure that the difference between (post)dominators and strict
   (post)dominators of a statement [s] is equal to the singleton [s]. *)
let test_strict f =
  let test s dom strict_dom =
    if StmtSet.is_empty dom
    then assert (StmtSet.is_empty strict_dom)
    else assert (StmtSet.diff dom strict_dom == StmtSet.singleton s)
  in
  List.iter (fun s ->
      let dom = Dominators.get_dominators s in
      let sdom = Dominators.get_strict_dominators s in
      let postdom = Dominators.get_postdominators s in
      let spostdom = Dominators.get_strict_postdominators s in
      test s dom sdom;
      test s postdom spostdom;
      assert (Dominators.strictly_dominates s s = false);
      assert (Dominators.strictly_postdominates s s = false)
    ) f.sallstmts

let test_nearest kf f =
  (* Simple test for empty list coverage of nearest function. *)
  assert (Dominators.nearest_common_ancestor [] = None);
  assert (Dominators.nearest_common_child [] = None);

  (* Since first and last statement do not have an ancestor/child,
     common nearest for all statements should be empty. *)
  assert (Dominators.nearest_common_ancestor f.sallstmts = None);
  assert (Dominators.nearest_common_child f.sallstmts = None);

  if List.length f.sallstmts > 1 then begin

    let first = Kernel_function.find_first_stmt kf in
    let last = Kernel_function.find_return kf in
    let all_but_first = List.filter (fun s -> s != first) f.sallstmts in
    let all_but_last = List.filter (fun s -> s != last) f.sallstmts in

    (* Test that all statements (except the first one) have as common ancestor
       the first statement of f, unless one of them is unreachable. *)
    begin match Dominators.nearest_common_ancestor all_but_first with
      | None -> (* At least one statement is unreachable. *) ()
      | Some ancestor -> assert (first == ancestor)
    end;

    (* Test that all statements (except the last one) have as common child
       the return statement of f, unless one of them is unreachable. This test
       supposes that frama-c's normalization removed all but one return statement.
    *)
    begin match Dominators.nearest_common_child all_but_last with
      | None -> (* At least one statement is unreachable. *) ()
      | Some child -> assert (last == child)
    end
  end

class visitPostDom = object(self)
  inherit Visitor.frama_c_inplace

  method! vfunc f =
    let kf = Option.get (self#current_kf) in
    Format.printf "@.@[<v>Computing for function %s:@;%a@?@;@?@]"
      f.svar.vname Cil_printer.pp_block f.sbody;

    Dominators.compute_dominators kf;
    Dominators.print_dot_dominators "dom_graph" kf;

    Dominators.compute_postdominators kf;
    Dominators.print_dot_postdominators "postdom_graph" kf;

    print_immediate f;
    test_strict f;
    test_nearest kf f;

    SkipChildren
end

let cover_errors () =
  Format.printf "Trigger some errors/warnings :@.";

  (* Test when a statement is not in a function. *)
  let loc = Fileloc.unknown in
  let skip = Cil_builder.Pure.(cil_stmt ~loc skip) in
  try ignore (Dominators.get_postdominators skip) with Log.AbortFatal _ -> ();

    let trigger kf =
      match kf.fundec with
      | Definition _ -> ()
      | Declaration _ ->
        (* Test Kernel_function.find_first_stmt on decl. *)
        ignore @@ Dominators.compute_dominators kf;
        (* Test Kernel_function.find_return on decl. *)
        ignore @@ Dominators.compute_postdominators kf;
        (* Test print_dot on decl. *)
        ignore @@ Dominators.print_dot_dominators "tmp" kf
    in
    Globals.Functions.iter trigger

let pretty () =
  Format.printf "@.@[<v2>Dominators analysis:@;%a@]\n@."
    Dominators.pretty_dominators ();
  Format.printf "@[<v2>Postominators analysis:@;%a@]\n@."
    Dominators.pretty_postdominators ()

let startup () =
  ignore (Cil.visitCilFileSameGlobals (new visitPostDom:>Cil.cilVisitor) (Ast.get ()));
  pretty ();
  Ast.mark_as_changed ();
  Format.printf "Invalidate tables, which should now be empty\n";
  pretty ();
  cover_errors ()

let () = Boot.Main.extend startup
