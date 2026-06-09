let sav_file = "load_one.sav"

let () = at_exit (fun _ -> Sys.remove sav_file)

let main ~compress () =
  Format.printf "MAIN (compressed: %B)@." compress;
  let sparecode () =
    Sparecode.Register.get ~select_annot:false ~select_slice_annot:false
  in
  let fp = Filepath.of_string sav_file in
  let p = sparecode () in
  Project.save ~compress fp;
  Project.remove ~project:p ();
  let p = Project.load fp in
  Project.on p (fun () -> Eva.Analysis.compute (); ignore (sparecode ())) ()

let () = Boot.Main.extend (main ~compress:false)
let () = Boot.Main.extend (main ~compress:true)

(* testing Project.create_by_copy *)
let main2 () =
  Eva.Analysis.compute ();
  let prj = Project.create_by_copy ~last:false "copy" in
  Format.printf "INIT AST@.";
  File.pretty_ast ();
  Format.printf "COPY AST@.";
  File.pretty_ast ~prj ()

let () = Boot.Main.extend main2
