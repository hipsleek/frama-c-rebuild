let add_terminates_true _ =
  let terminates pred_loc =
    Logic_const.(new_predicate { ptrue with pred_loc })
  in
  let add_terminates kf =
    if Annotations.terminates kf = None then
      Annotations.add_terminates
        Emitter.kernel kf (terminates @@ Kernel_function.get_location kf)
  in
  Globals.Functions.iter add_terminates

let () =
  let category = File.register_code_transformation_category "my_category" in
  File.add_code_transformation_after_cleanup category add_terminates_true
