(*
make -s tests/pdg/dyn_dpds.byte ; \
tests/pdg/dyn_dpds.byte -deps  tests/pdg/dyn_dpds.c; \
zgrviewer tests/pdg/dyn_dpds_1.dot ; \
zgrviewer tests/pdg/dyn_dpds_2.dot ;
*)

let get_zones str_data (stmt, kf) =
  let lval_term = Logic_parse_string.term_lval kf str_data in
  let lval = Logic_to_c.term_lval_to_lval lval_term in
  let exp = Cil.new_exp ~loc:Fileloc.unknown (Cil_types.Lval lval) in
  Eva.Results.(before stmt |> expr_deps exp)

let main _ =
  let memo_debug = Kernel.Debug.get () in
  Kernel.Debug.set 1;
  File.pretty_ast ();
  Kernel.Debug.set memo_debug ;
  let kf =  Globals.Functions.find_def_by_name "main" in
  let pdg = Pdg.Api.get kf in
  Format.printf "%a@." (Pdg.Api.pretty ~bw:false) pdg;
  Pdg.Api.extract pdg "dyn_dpds_0.dot";
  let assert_sid = 5 in (* assert ( *p>G) *)
  let assert_stmt, kf = Kernel_function.find_from_sid assert_sid in
  let _assert_node =
    match Pdg.Api.find_simple_stmt_nodes pdg assert_stmt with
    | n::[] -> n | _ -> assert false
  in
  let star_p = get_zones "*p" (assert_stmt, kf) in
  let data_nodes, undef =
    Pdg.Api.find_location_nodes_at_stmt pdg assert_stmt ~before:true star_p
  in
  assert (undef = None);
  let g_zone = get_zones "G" (assert_stmt, kf) in
  let g_nodes, undef =
    Pdg.Api.find_location_nodes_at_stmt pdg assert_stmt ~before:true g_zone
  in
  let _data_nodes = g_nodes @ data_nodes in
  let undef = match undef with None -> assert false | Some z -> z in
  Format.printf "Warning : cannot select %a in this function...@\n"
    Memory_zone.pretty undef;
  Format.printf "%a@." (Pdg.Api.pretty ~bw:false) pdg;
  Pdg.Api.extract pdg "dyn_dpds_1.dot"

let () = Boot.Main.extend main
