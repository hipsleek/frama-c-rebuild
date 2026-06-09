let main () =
  Eva.Analysis.compute ();
  Globals.set_entry_point "main2" false;
  Eva.Analysis.compute ();
;;

let () = Boot.Main.extend main
