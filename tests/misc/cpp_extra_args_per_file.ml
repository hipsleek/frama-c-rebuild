(* This test uses code very similar to what fc-mopsa does; it programmatically
   sets CppExtraArgsPerFile using unescaped strings coming from a JSON file. *)

let add_extra_args () =
  let fp = Filepath.of_string "./cpp_extra_args_per_file3.c" in
  (* flags contains an unescaped comma and unescaped colons *)
  let flags = "-DVERSION='\"svn-foo123-bar, built 2025-02-05 09:58:57 UTC\"'" in
  Kernel.CppExtraArgsPerFile.add(fp, flags)

let () = Cmdline.run_after_configuring_stage add_extra_args
