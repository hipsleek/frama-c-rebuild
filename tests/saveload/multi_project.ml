let check name test =
  Kernel.log "Checking %S@." name;
  Project.on
    (Project.find_all name |> List.hd)
    (fun () -> assert (test (Kernel.Files.get ()) [])) ()

let main ~compress () =
  Format.printf "MAIN (compressed: %B):@." compress;
  ignore (Project.create_by_copy ~last:false "foo");
  ignore (Project.create "foobar");
  let fp = Filepath.of_string "foo.sav" in
  Project.save_all ~compress fp;
  check "foo" (<>);
  check "foobar" (=);
  check "default" (<>);
  Kernel.Files.set [];
  Project.load_all fp;
  Filesystem.remove_file fp;
  ignore (Project.create_by_copy ~last:false "bar");
  let default_prj =
    let projects = Project.find_all "default" in
    (* Only one default project after loading *)
    assert (List.length projects = 1);
    List.hd projects
  in
  assert (Project.equal (Project.current()) default_prj);
  check "foo" (<>);
  check "foobar" (=);
  check "default" (<>);
  check "bar" (<>)

let () = Boot.Main.extend (main ~compress:false)
let () = Boot.Main.extend (main ~compress:true)
