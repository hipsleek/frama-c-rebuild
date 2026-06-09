let run () =
  let pp_pos = Filepos.pretty_long in
  let pp_loc (pos1, pos2, fname) =
    Format.printf "%a - %a -> %s@." pp_pos pos1 pp_pos pos2 fname
  in
  List.iter pp_loc (Cabs2cil.func_locs ())

let () = Boot.Main.extend run
