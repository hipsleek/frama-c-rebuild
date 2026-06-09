let main () =
  Eva.Analysis.compute();
  Filecheck.check_ast "Check alarm";
  File.pretty_ast ()

let () = Boot.Main.extend main
