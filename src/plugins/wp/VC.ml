(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Wpo

(* -------------------------------------------------------------------------- *)
(* --- Verification Conditions Interface                                  --- *)
(* -------------------------------------------------------------------------- *)

type t = Wpo.t

let get_id = Wpo.get_gid
let get_model = Wpo.get_model
let get_scope = Wpo.get_scope
let get_context = Wpo.get_context
let get_description = Wpo.get_label
let get_property = Wpo.get_property
let get_sequent w = snd (Wpo.compute w)
let get_result = Wpo.get_result
let get_results = ProofEngine.results
let is_trivial = Wpo.is_trivial
let is_valid = Wpo.is_fully_valid
let is_passed = Wpo.is_passed
let has_unknown = Wpo.has_unknown

let get_formula po =
  WpContext.on_context
    (get_context po) (Wpo.GOAL.compute_proof ~pid:po.po_pid) po.po_formula.goal

let clear = Wpo.clear
let proof = Wpo.goals_of_property
let iter_ip on_goal ip = Wpo.iter ~ip ~on_goal ()
let iter_kf on_goal ?bhv kf =
  match bhv with
  | None ->
    (* iter on all behaviors, see Wpo.iter *)
    Wpo.iter ~index:(Wpo.Function(kf,None)) ~on_goal ()
  | Some bs ->
    List.iter
      (fun b ->
         Wpo.iter ~index:(Wpo.Function(kf,Some b)) ~on_goal ()
      ) bs

let remove = iter_ip Wpo.remove
let () = Property_status.register_property_remove_hook remove

(* -------------------------------------------------------------------------- *)
(* --- Generator Interface                                                --- *)
(* -------------------------------------------------------------------------- *)

let generator model =
  let setup = match model with
    | None -> None
    | Some s -> Some (Factory.parse [s]) in
  Generator.create ~dump:false ?setup ()

let generate_ip ?model ip =
  (generator model)#compute_ip ip

let generate_kf ?model ?bhv ?prop kf =
  let kfs = Kernel_function.Set.singleton kf in
  (generator model)#compute_main ~fct:(Fct_list kfs) ?bhv ?prop ()

let generate_call ?model stmt =
  (generator model)#compute_call stmt

let generate_all ?model ?bhv ?prop () =
  (generator model)#compute_main ~fct:Fct_all ?bhv ?prop ()

(* -------------------------------------------------------------------------- *)
(* --- Prover Interface                                                   --- *)
(* -------------------------------------------------------------------------- *)

let prove = ProverTask.prove
let spawn = ProverTask.spawn ~delayed:true
let server = ProverTask.server
let command = Register.do_wp_proofs

(* -------------------------------------------------------------------------- *)
