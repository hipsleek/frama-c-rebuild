

(* Run the user commands *)
let run () =
  let p_default =
    Project.create_by_copy
      ~src:(Project.find_all "default" |> List.hd)
      ~last:false
      "default 2"
  in
  Eva.Analysis.compute ();
  Project.set_current p_default;
  Eva.Analysis.compute ();
  ()

let () = Boot.Main.extend run
