(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let dkey_shell = Wp_parameters.register_category "shell"

(* -------------------------------------------------------------------------- *)
(* --- Prover                                                             --- *)
(* -------------------------------------------------------------------------- *)

type t =
  | Why3 of Why3Provers.t
  | Qed
  | Tactical
  | CFG

let equal p q =
  match p,q with
  | Qed,Qed -> true
  | Tactical,Tactical -> true
  | CFG,CFG -> true
  | Why3 p, Why3 q -> Why3Provers.equal p q
  | (Why3 _ | CFG | Qed | Tactical) , _ -> false

let compare p q =
  match p,q with
  | Qed , Qed -> 0
  | Qed , _ -> (-1)
  | _ , Qed -> (+1)
  | CFG , CFG -> 0
  | CFG , _ -> (-1)
  | _ , CFG -> (+1)
  | Why3 p , Why3 q -> Why3Provers.compare p q
  | Why3 _ , _ -> (-1)
  | _ , Why3 _ -> (+1)
  | Tactical , Tactical -> 0

let hash = function
  | Qed -> 0
  | Tactical -> 1
  | CFG -> 2
  | Why3 p -> Why3Provers.hash p

let ident = function
  | Why3 s -> Why3Provers.ident_wp s
  | CFG -> "cfg"
  | Qed -> "qed"
  | Tactical -> "script"

let name = function
  | Why3 s -> Why3Provers.name s
  | CFG -> "CFG"
  | Qed -> "Qed"
  | Tactical -> "Script"

let shortcut = function
  | Why3 s -> String.lowercase_ascii @@ Why3Provers.name s
  | CFG -> "cfg"
  | Qed -> "qed"
  | Tactical -> "script"

let version = function
  | Why3 p -> Why3Provers.version p
  | _ -> System_config.Version.id_and_codename

let parse = function
  | "" | "none" -> None
  | "qed" | "Qed" -> Some Qed
  | "script" -> Some Tactical
  | "tip" -> Some Tactical
  | "why3" -> Some (Why3 { Why3.Whyconf.prover_name = "why3";
                           Why3.Whyconf.prover_version = "";
                           Why3.Whyconf.prover_altern = "generate only" })
  | name ->
    match Why3Provers.lookup name with
    | Some p -> Some (Why3 p)
    | None -> None

let title ?version = function
  | Why3 s ->
    let version = match version with Some v -> v | None ->
      not (Wp_parameters.has_dkey dkey_shell)
    in Why3Provers.title ~version s
  | CFG -> "CFG"
  | Qed -> "Qed"
  | Tactical -> "Script"

let pretty fmt p = Format.pp_print_string fmt (title p)

let is_auto = function
  | Qed -> true
  | CFG -> true
  | Tactical -> false
  | Why3 p -> Why3Provers.is_auto p

let is_tactical = function
  | Qed | CFG | Why3 _ -> false
  | Tactical -> true

let is_extern = function
  | Qed | CFG | Tactical -> false
  | Why3 _ -> true

let has_counter_examples = function
  | Qed | CFG | Tactical -> false
  | Why3 p -> Why3Provers.with_counter_examples p <> None

let sanitize_why3 s =
  let buffer = Buffer.create 80 in
  assert (s <> "ide");
  Buffer.add_string buffer "Why3_" ;
  String.iter
    (fun c ->
       let c = if
         ('0' <= c && c <= '9') ||
         ('a' <= c && c <= 'z') ||
         ('A' <= c && c <= 'Z')
         then c else '_'
       in Buffer.add_char buffer c) s ;
  Buffer.contents buffer

let filename_for = function
  | Why3 s -> sanitize_why3 (Why3Provers.ident_wp s)
  | CFG -> "CFG"
  | Qed -> "Qed"
  | Tactical -> "Tactical"


let of_name ?fallback = function
  | "qed" -> Some Qed
  | "script" -> Some Tactical
  | name ->
    match Why3Provers.lookup ?fallback name with
    | None -> None
    | Some prv -> Some (Why3 prv)

module P = struct type nonrec t = t let compare = compare end
module Pset = Set.Make(P)
module Pmap = Map.Make(P)

(* -------------------------------------------------------------------------- *)
(* --- Prover list                                                        --- *)
(* -------------------------------------------------------------------------- *)

let available_why3_provers () =
  List.map (fun p -> Why3 p) @@
  List.filter Why3Provers.is_mainstream @@
  Why3Provers.provers ()

module PTable = Hashtbl.Make
    (struct
      type nonrec t = t
      let equal = equal
      let hash = hash
    end)

type proving_config = {
  provers: bool PTable.t ;
  mutable strategies: bool ;
}

let config = ref None
let reload_hooks = ref []
let add_reload_hook f = reload_hooks := f :: !reload_hooks

let parse_and_set () =
  let provers = PTable.create 9 in
  List.iter
    (fun p -> PTable.add provers p false)
    (Qed :: Tactical :: available_why3_provers ());
  let has_none = ref false in
  let has_strat = ref false in
  let parse = function
    | "none" | "" -> has_none := true ;
    | "Qed" | "qed" -> PTable.replace provers Qed true
    | "tip" -> PTable.replace provers Tactical true ; has_strat := true
    | "script" -> PTable.replace provers Tactical true
    | name ->
      match parse name with
      | None -> Wp_parameters.error "Unknown prover %s" name
      | Some p -> PTable.replace provers p true
  in
  List.iter parse @@ Wp_parameters.Provers.get () ;
  if not (PTable.fold (fun _ v acc -> v || acc) provers false) && not !has_none
  then begin
    (* 1. take Alt-Ergo *)
    match Why3Provers.lookup "Alt-Ergo" with
    | Some p -> PTable.replace provers (Why3 p) true
    | None ->
      (* 2. take any automatic solver  *)
      match List.filter is_auto @@ available_why3_provers () with
      | p :: _  -> PTable.replace provers p true
      | [] ->
        (* 3. take any external solver *)
        match available_why3_provers () with
        | p :: _ -> PTable.replace provers p true
        (* 4. take Qed *)
        | [] -> PTable.replace provers Qed true
  end ;
  config := Some { provers ; strategies = !has_strat } ;
  List.iter (fun f -> f ()) !reload_hooks

let () =
  Wp_parameters.Provers.add_update_hook (fun _ _ -> parse_and_set ())

let get () =
  begin match !config with
    | None -> parse_and_set ()
    | _ -> ()
  end ;
  Option.get !config

let provers ?(filter=fun _ -> true) () =
  List.rev @@ PTable.fold_sorted
    ~cmp:compare
    (fun p _ l -> if filter p then p :: l else l) (get ()).provers []

let enabled p = PTable.find (get()).provers p

let prover_hooks = ref []
let add_prover_update_hook f = prover_hooks := f :: !prover_hooks

let set_prover p ~state =
  PTable.replace (get()).provers p state ;
  List.iter (fun f -> f p) !prover_hooks

let use_scripts () = PTable.find (get ()).provers Tactical
let use_strategies () = (get ()).strategies

let scripts_hooks = ref []
let add_scripts_update_hook f = scripts_hooks := f :: !scripts_hooks

let set_use_scripts value =
  let config = get() in
  PTable.replace config.provers Tactical value ;
  if not value then config.strategies <- false ;
  List.iter (fun f -> f ()) !scripts_hooks

let set_use_strategies value =
  let config = get () in
  config.strategies <- value ;
  if value then set_use_scripts true ;
  List.iter (fun f -> f ()) !scripts_hooks

(* -------------------------------------------------------------------------- *)
(* --- Interactive provers configuration                                  --- *)
(* -------------------------------------------------------------------------- *)

module ModeCache(Mode: sig
    module Parameter : Parameter_sig.String
    type t
    val parse: string -> t
    val name: string
    val default: unit -> t
  end)
=
struct
  open Mode
  let option = Parameter.name
  let variable = "FRAMAC_WP_" ^ (String.uppercase_ascii name)
  let cache_name = "Wp.Prover." ^ (String.capitalize_ascii name) ^ ".Cache"

  include WpContext.StaticGenerator(Datatype.Unit)
      (struct
        type key = unit
        type data = t
        let name = cache_name
        let compile () =
          let parse ~origin ~fallback s =
            try parse s
            with Not_found ->
              Wp_parameters.warning ~current:false
                "Unknown %s mode %S (use %s instead)" origin s fallback ;
              raise Not_found
          in
          try
            if Parameter.is_set ()
            then parse ~origin:option ~fallback:variable @@ Parameter.get ()
            else raise Not_found
          with Not_found ->
          try
            let param = Sys.getenv variable in
            if param = "" then raise Not_found
            else parse ~origin:variable ~fallback:option param
          with Not_found ->
            default ()
      end)

  let hooks = ref []
  let add_hook_on_update f = hooks := f :: !hooks
  let clear_then_hooks () = clear () ; List.iter (fun h -> h ()) !hooks

  let () =
    Parameter.add_update_hook
      (fun _ _ -> clear_then_hooks ())

  let parse = Mode.parse
  let get () = get ()
  let set m = set () m
end

module InteractiveMode = struct
  type t =
    | Batch
    | Update
    | Edit
    | Fix
    | FixUpdate

  let title = function
    | Fix -> "Fix"
    | Edit -> "Edit"
    | Batch -> "Batch"
    | Update -> "Update"
    | FixUpdate -> "Fix Update"

  let parse m =
    match String.lowercase_ascii m with
    | "fix" -> Fix
    | "edit" -> Edit
    | "batch" -> Batch
    | "update" -> Update
    | "fixup" -> FixUpdate
    | _ -> raise Not_found

  let pretty fmt m = Format.pp_print_string fmt (title m)

  include ModeCache(struct
      module Parameter = Wp_parameters.Interactive
      type nonrec t = t
      let parse = parse
      let name = "interactive"
      let default () = Batch
    end)
end

(* -------------------------------------------------------------------------- *)
(* --- TIP configuration                                                  --- *)
(* -------------------------------------------------------------------------- *)

module TipMode = struct
  type t =
    | Batch
    | Update
    | Dry
    | Init

  let parse = function
    | "batch" -> Batch
    | "update" -> Update
    | "dry" -> Dry
    | "init" -> Init
    | _ -> raise Not_found

  include ModeCache(struct
      module Parameter = Wp_parameters.ScriptMode
      type nonrec t = t
      let parse = parse
      let name = "script"
      let default () =
        let provers = Wp_parameters.Provers.get () in
        if List.mem "tip" provers then Update else
        if List.mem "script" provers then Batch else
          Dry
    end)

  let () =
    Wp_parameters.Provers.add_update_hook
      (fun _ _ -> clear_then_hooks ())

  let is_scratch () =
    match get () with
    | Batch | Update -> false
    | Dry | Init -> true

  let is_saving () =
    match get () with
    | Update | Init -> true
    | Batch | Dry -> false

end
