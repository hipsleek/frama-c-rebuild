(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Why3

(* -------------------------------------------------------------------------- *)
(* --- Why3 Config                                                        --- *)
(* -------------------------------------------------------------------------- *)

let why3_version = Why3.Config.version

let file () =
  let param = Wp_parameters.Why3Config.get () in
  if Filepath.is_empty param
  then None
  else Some (Filepath.to_string_abs param)

(* brittle but the best we can do with the current API *)
let extend_config config =
  let data = Why3.Autodetection.read_auto_detection_data config in
  let provers = Why3.Autodetection.find_provers data in
  let to_rc provers =
    let to_section (path, name, version) =
      let set_string f v s = Why3.Rc.set_string s f v in
      Why3.Rc.empty_section |>
      set_string "name" name |>
      set_string "path" path |>
      set_string "version" version
    in
    let sections = List.map to_section provers in
    Why3.Rc.set_simple_family Why3.Rc.empty "partial_prover" sections
  in
  !Whyconf.provers_from_detected_provers config (to_rc provers)

let the_config = ref None

let () =
  let must_reload_config _ _ = the_config := None in
  Wp_parameters.Why3Config.add_update_hook must_reload_config ;
  Wp_parameters.Why3ExtraConfig.add_update_hook must_reload_config

let config () =
  if Option.is_none !the_config then
    begin try
        let file = file () in
        let extra_config = Wp_parameters.Why3ExtraConfig.get () in
        let config = Why3.Whyconf.init_config ~extra_config file in
        let auto_detect = Wp_parameters.Why3Autodetect.get () in
        let config = if auto_detect then extend_config config else config in
        the_config := Some config ;
      with exn ->
        Wp_parameters.abort "%a" Why3.Exn_printer.exn_printer exn
    end ;
  Option.get !the_config

let flags_changed = ref true

let () =
  let must_reconfigure _ _ = flags_changed := true in
  Wp_parameters.Why3Flags.add_update_hook must_reconfigure

let configure =
  begin fun () ->
    if !flags_changed then
      begin
        let commands = "why3" :: Wp_parameters.Why3Flags.get () in
        let args = Array.of_list commands in
        begin try
            (* Ensure that an error message generating directly by why3 is
               reported as coming from Why3, not from Frama-C. *)
            Why3.Getopt.commands := commands;
            Why3.Getopt.parse_all
              (Why3.Debug.Args.[desc_debug;desc_debug_all;desc_debug_list])
              (fun opt -> raise (Arg.Bad ("unknown option: " ^ opt)))
              args
          with Arg.Bad s | Arg.Help s -> Wp_parameters.abort "%s" s
        end;
        ignore (Why3.Debug.Args.option_list ());
        Why3.Debug.Args.set_flags_selected ();
        flags_changed := false
      end
  end

let set_procs = Why3.Controller_itp.set_session_max_tasks

(* -------------------------------------------------------------------------- *)
(* --- Why3 Provers                                                       --- *)
(* -------------------------------------------------------------------------- *)

type t = Why3.Whyconf.prover

let ident_why3 = Why3.Whyconf.prover_parseable_format
let ident_wp s =
  let name = Why3.Whyconf.prover_parseable_format s in
  let prv = String.split_on_char ',' name in
  String.concat ":" prv

let title ?(version=true) p =
  if version then Pretty_utils.to_string Why3.Whyconf.print_prover p
  else p.Why3.Whyconf.prover_name
let compare = Why3.Whyconf.Prover.compare
let equal = Why3.Whyconf.Prover.equal
let hash = Why3.Whyconf.Prover.hash
let name p = p.Why3.Whyconf.prover_name

let version p = p.Why3.Whyconf.prover_version
let is_mainstream p = p.Why3.Whyconf.prover_version <> "" && p.Why3.Whyconf.prover_altern = ""
let is_auto (p : t) =
  match p.prover_name with
  | "Coq" | "Isabelle" -> false
  | "Alt-Ergo" | "Z3" | "CVC4" | "CVC5" | "Colibri2" -> true
  | _ ->
    let config = config () in
    try
      let prover_config = Why3.Whyconf.get_prover_config config p in
      not prover_config.interactive
    with Not_found -> true
let has_counter_examples p =
  List.mem "counterexamples" @@
  String.split_on_char '+' p.Why3.Whyconf.prover_altern

let provers () =
  Why3.Whyconf.Mprover.keys (Why3.Whyconf.get_provers (config ()))

let is_available p =
  Why3.Whyconf.Mprover.mem p (Why3.Whyconf.get_provers (config ()))

let with_counter_examples p =
  if has_counter_examples p then Some p else
    let name = p.prover_name in
    let version = p.prover_version in
    List.find_opt
      (fun (q : t) ->
         q.prover_name = name &&
         q.prover_version = version &&
         has_counter_examples q)
    @@ provers ()

(* -------------------------------------------------------------------------- *)
(* ---  Prover Lookup                                                     --- *)
(* -------------------------------------------------------------------------- *)

(* semantical version comparison *)

type sem = V of int | S of string
let sem s = try V (int_of_string s) with Failure _ -> S s
let cmp x y =
  match x,y with
  | V a,V b -> b - a
  | V _,S _ -> (-1)
  | S _,V _ -> (+1)
  | S a,S b -> String.compare a b
let scmp u v = cmp (sem u) (sem v)
let vcmp u v =
  List.compare scmp (String.split_on_char '.' u) (String.split_on_char '.' v)
let by_version (p:t) (q:t) = vcmp p.prover_version q.prover_version

let filter ~name ?version (p:t) =
  p.prover_altern = "" &&
  String.lowercase_ascii p.prover_name = name &&
  match version with None -> true | Some v -> p.prover_version = v

let select ~name ?version () =
  match
    List.sort by_version @@ List.filter (filter ~name ?version) @@ provers ()
  with p::_ -> Some p | [] -> None

let lookup ?(fallback=false) prover_name =
  match String.split_on_char ':' @@ String.lowercase_ascii prover_name with
  | [name] -> select ~name ()
  | [name;version] ->
    begin
      match select ~name ~version () with
      | Some _ as res -> res
      | None ->
        if fallback then
          match select ~name () with
          | None -> None
          | Some p as res ->
            Wp_parameters.warning ~once:true ~current:false
              "Prover %s not found, fallback to %s" prover_name (ident_wp p) ;
            res
        else None
    end
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Models                                                             --- *)
(* -------------------------------------------------------------------------- *)

type model = Why3.Model_parser.concrete_syntax_term
let pp_model = Why3.Model_parser.print_concrete_term

(* -------------------------------------------------------------------------- *)
