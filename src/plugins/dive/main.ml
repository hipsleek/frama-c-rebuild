(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type format = Dot | Json

let output format context filename =
  let graph = Context.get_graph context in
  let output_function channel = match format with
    | Dot -> Dive_graph.output_to_dot channel graph
    | Json -> Server_interface.output_to_json channel graph
  in
  Self.result "output to %a" Filepath.pretty filename;
  match Filesystem.with_open_out filename output_function with
  | Ok () -> ()
  | Error (msg, _) ->
    Self.warning "failed to output graph: %s" msg

let main () =
  if not (Self.FromBases.is_empty () &&
          Self.FromFunctionAlarms.is_empty ()) then begin
    (* Make sure Eva is computed *)
    Eva.Analysis.compute ();
    (* Create the initial graph  *)
    let context = Context.create () in
    (* Handle parameters *)
    Self.UnfoldedBases.iter (Context.unfold context);
    Self.HiddenBases.iter (Context.hide context);
    let depth = Self.DepthLimit.get () in
    (* Add targeted vars to it *)
    let add_var vi =
      let node = Build.add_var context vi in
      Build.explore_backward ~depth context node
    in
    Self.FromBases.iter add_var;
    (* Add alarms *)
    let add_alarm _emitter kf stmt ~rank:_ alarm _code_annot =
      if Self.FromFunctionAlarms.mem kf then begin
        let node = Build.add_alarm context stmt alarm in
        Build.explore_backward ~depth context node
      end
    in
    if not (Self.FromFunctionAlarms.is_empty ()) then
      Alarms.iter add_alarm;
    (* Output it *)
    if not (Self.OutputDot.is_empty ()) then
      output Dot context (Self.OutputDot.get ());
    if not (Self.OutputJson.is_empty ()) then
      output Json context (Self.OutputJson.get ());
  end

let () =
  Boot.Main.extend main
