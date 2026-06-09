let run () =
  Globals.Functions.iter (fun kf ->
      Populate_spec.populate_funspec kf [`Assigns];
      ignore (Annotations.funspec kf)
    )

let () = Boot.Main.extend run
