(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Bind parameters to printing functions to be applied on each function. *)
let param_pretty =
  let open Inout_parameters in
  [ ForceOut.(self, get), Outputs.pretty_internal;
    ForceExternalOut.(self, get), Outputs.pretty_external;
    ForceInput.(self, get), Inputs.pretty_external;
    ForceDeref.(self, get), Derefs.pretty_external;
    ForceInout.(self, get), Operational_inputs.pretty_operational_inputs_internal;
    ForceInoutExternalWithFormals.(self, get),
    Operational_inputs.pretty_operational_inputs_external_with_formals;
    ForceInputWithFormals.(self, get), Inputs.pretty_with_formals;
  ]

let run () =
  (* Only keep printing function for which the parameter is enabled. *)
  let aux ((_self, get), pretty) = if get () then Some pretty else None in
  let pretty_list = List.filter_map aux param_pretty in
  if pretty_list <> [] && Inout_parameters.Output.get ()
  then begin
    Eva.Analysis.compute ();
    Callgraph.Uses.iter_in_rev_order
      (fun kf ->
         if Kernel_function.is_definition kf && Eva.Results.is_called kf
         then begin
           if Inout_parameters.ForceDeref.get ()
           then Derefs.compute_external kf;
           List.iter (fun pp -> Inout_parameters.result "%a" pp kf) pretty_list
         end)
  end

let param_deps = List.map (fun ((self, _), _) -> self) param_pretty
let deps = Eva.Analysis.self :: param_deps

let run_once, _ = State_builder.apply_once "Inout.main" deps run

let () = Boot.Main.extend run_once
