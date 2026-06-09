let visit_kf kf =
  if not @@ String.starts_with ~prefix:"__" @@ Kernel_function.get_name kf then
    let locals = Kernel_function.get_locals kf in
    let pp_varinfo fmt v =
      Format.fprintf fmt "_Alignof(%a) == %d"
        Cil_printer.pp_varinfo v
        (Cil.bytesAlignOfVarinfo v)
    in
    Kernel.feedback "Function: %a@\n%a"
      Kernel_function.pretty kf
      (Pretty_utils.pp_list ~sep:"@." pp_varinfo)
      locals

let visit_type _name t _ns =
  match t.Cil_types.tnode with
  | TComp { cfields } ->
    let pp_fieldinfo fmt fi =
      Format.fprintf fmt "_Alignof(%a) == %d"
        Cil_printer.pp_field fi
        (Cil.bytesAlignOfField fi)
    in
    Kernel.feedback "Compound: %a@\n%a"
      Cil_printer.pp_typ t
      (Pretty_utils.pp_opt @@ Pretty_utils.pp_list ~sep:"@." pp_fieldinfo)
      cfields
  | _ -> ()


let run () =
  Kernel.feedback "==========================================================" ;
  Kernel.feedback "Computed alignments" ;
  Globals.Types.iter_types visit_type ;
  Globals.Functions.iter visit_kf ;
  Kernel.feedback "=========================================================="

let () =
  Boot.Main.extend run
