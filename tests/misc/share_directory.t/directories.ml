let () = Plugin.is_share_visible ()
module Self =
  Plugin.Register
    (struct
      let name = "Directories"
      let shortname = "dirs"
      let help = ""
    end)

module Share = Self.Share

module Path =
  Self.Make_site_dir
    (Share)
    (struct let name = "path" end)

let never_fail_get f x =
  try
    let s = f x in
    Self.feedback "Found: %a" Filepath.pretty s
  with _ -> ()

let run_all () =
  Kernel.feedback "IS_SET %b" (Share.is_set ()) ;

  Self.feedback "path (dir)" ;
  never_fail_get Share.get_dir "path" ;

  Self.feedback "Path (.)" ;
  never_fail_get Path.get_dir "." ;

  Self.feedback "path/file.txt (file)" ;
  never_fail_get Share.get_file "path/file.txt" ;

  Self.feedback "Path (file)" ;
  never_fail_get Path.get_file "file.txt" ;

  Self.feedback "foo (dir)" ;
  never_fail_get Share.get_dir "foo" ;

  Self.feedback "foo.txt (file)" ;
  never_fail_get Share.get_file "foo.txt" ;

  Self.feedback "path (file)" ;
  never_fail_get Share.get_file "path" ;

  Self.feedback "path/file.txt" ;
  never_fail_get Share.get_dir "path/file.txt"


let () = Boot.Main.extend run_all
