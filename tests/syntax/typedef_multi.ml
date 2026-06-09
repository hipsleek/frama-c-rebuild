let run () =
  File.reorder_ast ();
  File.pretty_ast ()

let () = Boot.Main.extend run
