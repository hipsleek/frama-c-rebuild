(* Test of functions [get_results] and [set_results] from Eva_results module.
   They are no longer used in Eva, but may still be used by third-party plugins.

   This script uses "file.i" and:
   - runs an analysis from "precise" main function and saves results;
   - runs an analysis from "imprecise" main function and saves results;
   - sets the results of the first analysis and prints some info;
   - sets the results of the second analysis and prints some info.
*)

let analyze name =
  Format.printf "Analyzing from %s call…@." name;
  Kernel.MainFunction.set name;
  Eva.Analysis.compute ();
  Eva.Eva_results.get_results ()

let pp_list x = Pretty_utils.pp_list ~pre:"" ~suf:"" ~sep:"@;" x

let print_results name results =
  Eva.Eva_results.set_results results;
  Format.printf "@[<v 2>Results from %s call:" name;
  let kf = Globals.Functions.find_by_name "test" in
  (* Print callers of [kf]. *)
  let callers = Eva.Results.callers kf in
  Format.printf "@;@[Callers of %a: %a@]"
    Kernel_function.pretty kf
    (pp_list Kernel_function.pretty) callers;
  (* Print values of local variables of [kf]. *)
  let locals = Kernel_function.get_locals kf in
  let pp_varinfo fmt vi =
    let cvalue = Eva.Results.(at_end_of kf |> eval_var vi |> as_cvalue) in
    Format.fprintf fmt "%a: %a" Printer.pp_varinfo vi Cvalue.V.pretty cvalue
  in
  Format.printf "@;@[<v 2>Values at end of function %a:@;%a@]"
    Kernel_function.pretty kf (pp_list pp_varinfo) locals;
  (* Print status of all properties. *)
  let properties = Property_status.fold List.cons [] in
  let pp_property fmt property =
    let open Property_status.Consolidation in
    Format.fprintf fmt "%a: %a" Property.pretty property pretty (get property)
  in
  Format.printf "@;@[<v 2>Properties:@;%a@]" (pp_list pp_property) properties;
  Format.printf "@]@."

let test () =
  let precise_results = analyze "precise" in
  let imprecise_results = analyze "imprecise" in
  let selection = State_selection.with_dependencies Eva.Analysis.self in
  Project.clear ~selection ();
  print_results "precise" precise_results;
  print_results "imprecise" imprecise_results

let () = Boot.Main.extend test
