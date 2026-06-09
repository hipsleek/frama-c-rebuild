open Cil_types

let fold t =
  match t.term_node with
  | TSizeOf _ | TSizeOfE _ |  TAlignOf _ | TAlignOfE _
  | TUnOp _ | TBinOp _ ->
    let t' = Cil.constFoldTerm t in
    Format.printf "  %a folds to %a@." Cil_printer.pp_term t Cil_printer.pp_term t'
  | _ -> ()

class visitTerm prj = object(_)
  inherit Visitor.frama_c_copy prj

  method! vterm t =
    fold t;
    Cil.JustCopy

end

let test_terms () =
  let open Cil_builder.Exp in
  let loc = Fileloc.unknown in
  let e1 = lognot ((of_int 21) + (of_int 21)) in
  let e2 = lognot ((of_int 21) - (of_int 21)) in
  let e3 = lt zero (logand (of_int 42) (of_int 21)) in
  let e4 = gt one (logor zero (of_int 21)) in
  let e5 = ne zero (le zero (logand (of_int 42) (of_int 21))) in
  let e6 = eq one (ge one (logor one zero)) in
  let t1 = cil_term ~loc e1 in
  let t2 = cil_term ~loc e2 in
  let t3 = cil_term ~loc e3 in
  let t4 = cil_term ~loc e4 in
  let t5 = cil_term ~loc e5 in
  let t6 = cil_term ~loc e6 in
  Format.printf "Custom terms :@.";
  fold t1;
  fold t2;
  fold t3;
  fold t4;
  fold t5;
  fold t6

let startup () =
  test_terms ();
  Format.printf "File terms :@.";
  let prj = File.create_project_from_visitor "test_const_fold"
      (fun prj -> new visitTerm prj)
  in
  Project.set_current prj

let () = Boot.Main.extend startup
