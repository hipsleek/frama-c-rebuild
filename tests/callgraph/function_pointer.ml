let main () =
  Format.printf "number of calls = %d@." (Callgraph.Uses.nb_calls ())

let () = Boot.Main.extend main
