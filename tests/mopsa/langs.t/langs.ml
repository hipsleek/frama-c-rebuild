let parse_cc _file =
  (Cil_datatype.File.dummy, Cil_datatype.Cabs_file.dummy)

let () =
  Kernel.feedback "adding .cc to known file extensions";
  File.new_file_type ".cc" parse_cc
