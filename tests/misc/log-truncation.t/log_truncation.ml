let run () =
  (* This can be filtered to avoid too long test oracles. *)
  let very_long_line = String.make 5000 '#' in
  let line = "begin_line <-----" ^ very_long_line ^ "-----> end_line" in
  let middle_text =
    "[ This text should only appear in the log if is is redirected to a file. ]"
  in
  Kernel.warning
    "This is a very, very long message intended to test the truncation \
     of the Frama-C log: \n\
     @[<v>%s@;%s@;%s@]" line middle_text line

let () = Boot.Main.extend run
