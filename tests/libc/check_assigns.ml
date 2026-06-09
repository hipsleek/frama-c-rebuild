open Cil_types

(* Note: this test must not include files with function definitions (.c), only
   headers with declarations. Otherwise, the reported locations for the
   functions will be the .c files, and not the .h where assigns are missing.
*)


let run () =
  let check_assigns kf acc =
    if Kernel_function.is_variadic kf
    then (* skip *) acc
    else
      let spec = Annotations.funspec kf in
      let loc = Kernel_function.get_location kf in
      let name = Kernel_function.get_name kf in
      match Cil.find_default_behavior spec with
      | None -> (name, loc) :: acc
      | Some default ->
        if default.b_assigns = WritesAny then
          (name, loc) :: acc
        else acc
  in
  let todo = Globals.Functions.fold check_assigns [] in
  if todo = [] then
    Kernel.feedback
      "All non-variadic functions in Frama-C's libc headers have assigns!@."
  else begin
    Kernel.warning "Missing assigns / default behavior in %d function(s):@."
      (List.length todo);
    List.iter (fun (name, loc) ->
        Format.printf "  %s (%a)@." name Fileloc.pretty loc
      ) todo
  end

let () = Boot.Main.extend run
