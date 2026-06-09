
let get_formal_variables name =
  let add_kf_vars kf vars =
    try
      let v = Globals.Vars.find_from_astinfo name (Cil_types.Formal kf) in
      Format.printf "found variable vid:%d formal in %a@."
        v.Cil_types.vid Cil_datatype.Kf.pretty kf;
      v::vars
    with Not_found -> vars
  in
  let vars = Globals.Functions.fold add_kf_vars [] in
  vars

let get_local_variables name =
  let add_kf_vars kf vars =
    try
      let v = Globals.Vars.find_from_astinfo name (Cil_types.Whole_function kf)
      in
      Format.printf "found variable vid:%d formal in %a@."
        v.Cil_types.vid Cil_datatype.Kf.pretty kf;
      v::vars
    with Not_found -> vars
  in
  let vars = Globals.Functions.fold add_kf_vars [] in
  vars



let main () =
  Ast.compute ();
  let vars = get_formal_variables "x" in
  let vars' = get_local_variables "y" in
  let do_v v =
    let _, kind = Globals.Vars.get_astinfo v in
    Format.printf "[do_v] vid:%d %a@."  v.Cil_types.vid
      Cil_datatype.Syntactic_scope.pretty kind
  in List.iter do_v vars; List.iter do_v vars'

let () = Boot.Main.extend main
