let norm1 = Filepath.of_string ~base:(Filepath.of_string "/dir1/") "dir/file" in
let norm2 = Filepath.of_string ~base:(Filepath.of_string "/dir2/") "dir/file" in
(* norm2 should be different than norm1 *)
Format.printf "norm1: %a\nnorm2: %a\n"
  Filepath.pretty_abs norm1
  Filepath.pretty_abs norm2
