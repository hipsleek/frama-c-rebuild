let print () =
  File.pretty_ast ();
  Kernel.log "================================"

let print_status () =
  Kernel.log "printing status";
  let rte_state_getter_list = RteGen.Generator.all_statuses in
  Globals.Functions.iter
    (fun kf ->
       Kernel.log "kf = %s" (Kernel_function.get_name kf) ;
       List.iter
         (fun (s, _, getter) -> Kernel.log "- %s = %b" s (getter kf))
         rte_state_getter_list);
  Kernel.log "================================"

let main () =
  Dynamic.Parameter.Bool.set "-rte-mem" true;
  Dynamic.Parameter.Bool.set "-rte-pointer-call" true;
  Dynamic.Parameter.Bool.set "-rte-float-to-int" true;
  Dynamic.Parameter.Bool.set "-rte-div" true;
  Kernel.SignedDowncast.on ();
  Kernel.SignedOverflow.on ();
  if not(Ast.is_computed ()) then Ast.compute () ;
  print ();

  Globals.Functions.iter (fun kf -> RteGen.Visit.annotate kf);
  print () ;
  print_status ();

  let emitter = RteGen.Generator.emitter in
  let filter = function
    | Alarms.Overflow _ | Alarms.Division_by_zero _ -> true
    | _ -> false
  in
  Alarms.remove ~filter emitter;
  print ();
  print_status ()

let () = Boot.Main.extend main
