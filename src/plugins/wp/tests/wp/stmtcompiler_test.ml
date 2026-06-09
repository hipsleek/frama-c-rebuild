(* test API of StmtSemantics *)

open Wp
open Factory
open Memory

let mode = `Tree

let bar = String.make 60 '-'

let run () =
  let setup : Factory.setup = { mheap = Typed MemTyped.NoCast;
                                mvar = Var;
                                cint = Cint.Natural;
                                cfloat = Cfloat.Real} in
  let driver = Driver.load_driver () in
  let model = Factory.instance setup driver in
  let module C = (val (Factory.compiler setup.mheap setup.mvar)) in
  let module Compiler = StmtSemantics.Make(C) in
  let module Cfg = Compiler.Cfg in

  let provers =
    List.fold_right
      (fun pname prvs -> match Prover.parse pname with
         | None -> prvs
         | Some Prover.Tactical -> prvs
         | Some prv -> (Prover.InteractiveMode.Batch, prv) :: prvs)
      ["qed"] []
  in

  let spawn goal =
    let result _ prv res =
      Format.printf "[%a] %t@.@\n"
        Prover.pretty prv (VCS.pp_result_qualif prv res)
    in
    let server = ProverTask.server () in
    ProverTask.spawn goal ~delayed:true ~result provers;
    Task.launch server
  in

  let prove_sequent kf prop_id sequent =
    Format.printf "@[<3>@[%s sequent:@]@ %a@]@."
      (Kernel_function.get_name kf)
      !Conditions.pretty sequent;
    let goal = Wpo.GOAL.make sequent in
    let vc_annot = Wpo.VC_Annot.{
        axioms = None;
        goal;
        tags = []; warn = [];
        deps = Property.Set.empty;
        path = Cil_datatype.Stmt.Set.empty;
        source = None;
      } in
    let po = Wpo.{
        po_gid = ""; po_sid = ""; po_name = "";
        po_idx = Function(kf, None); po_model = model;
        po_pid = prop_id;
        po_formula = vc_annot;
      } in
    Format.printf "@[%a@]" Wpo.pp_goal po;
    spawn po;
    Format.printf "%s@." bar ;
  in

  let goal_read acc g =
    let reads = Cfg.P.reads g.Compiler.goal_pred in
    Cfg.Node.Map.union
      (fun _ -> C.M.Sigma.union)
      reads acc
  in

  let prove_goal kf start cfg goal =
    let pred = goal.Compiler.goal_pred in
    let user_reads = goal_read Cfg.Node.Map.empty goal in
    let posts = Cfg.P.nodes pred in
    let name = Kernel_function.get_name kf in
    let (_, nsigmas,sequence) = Compiler.Cfg.compile ~mode ~name start posts user_reads cfg in
    (* Format.printf "Nodes of %a: " Lang.F.pp_pred (Cfg.P.get pred); *)
    let map n _domain =
      (* Format.printf "%a " Cfg.Node.pp n; *)
      try Cfg.Node.Map.find n nsigmas with Not_found ->
        (* Format.printf "unknown node %a@." Cfg.Node.pp n; *) assert false in
    let pred = Cfg.P.relocate
        (Cfg.Node.Map.map map (Cfg.P.reads pred)) pred in
    (* Format.printf "@."; *)
    let p = (Cfg.P.get pred) in
    let sequent = sequence, p in
    prove_sequent kf goal.Compiler.goal_prop sequent
  in

  (* Test on real Cil functions *)
  let _run_test model kf =
    let context = model , WpContext.Kf kf in
    WpContext.on_context context
      begin fun () ->
        let automaton = Interpreted_automata.build_automaton ~annotations:true kf in
        (* Format.printf "@[%s body cil:%a@]@." fct Printer.pp_block block; *)
        let seq = {Memory.pre = Cfg.node (); post = Cfg.node ()} in
        let env = Compiler.empty_env kf  in
        let env = Compiler.(env @* [Clabels.here,seq.pre;
                                    Clabels.next,seq.post]) in
        let paths = Compiler.automaton env automaton in
        let cfg, goals = paths.Compiler.paths_cfg, paths.Compiler.paths_goals in
        Format.printf "old way@.";
        Bag.iter
          (prove_goal kf seq.pre cfg)
          goals;
      end ()
  in

  let run_test_ia model kf =
    let context = model , WpContext.Kf kf in
    WpContext.on_context context
      begin fun () ->
        let paths,start = Compiler.compute_kf kf in
        let cfg, goals = paths.Compiler.paths_cfg, paths.Compiler.paths_goals in
        let fname = Filename.temp_file "cfg_pre_" (Kernel_function.get_name kf) in
        let cout = open_out fname in
        Compiler.Cfg.output_dot cout ~checks:(Bag.map (fun g -> g.Compiler.goal_pred) goals) cfg;
        close_out cout;
        Format.printf "new way@.";
        Bag.iter
          (prove_goal kf start cfg)
          goals;
      end ()
  in

  let ordered_kf =
    List.sort (fun kf1 kf2 ->
        Fileloc.compare
          (Kernel_function.get_location kf1)
          (Kernel_function.get_location kf2)
          (* String.compare *)
          (*   (Kernel_function.get_name kf1) *)
          (*   (Kernel_function.get_name kf2) *)
      )
      (Globals.Functions.fold (fun kf acc -> kf::acc) []) in

  List.iter (fun kf ->
      if Kernel_function.is_definition kf then
        Lang.local (run_test_ia model) kf
    )
    ordered_kf

(* Turn off populate spec for everything except assigns *)
let turn_off_populate () =
  Kernel.GeneratedSpecMode.set "skip";
  Kernel.GeneratedSpecCustom.add ("assigns", Some "frama-c")

let () = Cmdline.run_after_configuring_stage turn_off_populate

let () =  Boot.Main.extend run
