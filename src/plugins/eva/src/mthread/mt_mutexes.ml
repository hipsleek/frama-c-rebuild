(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_types
open Mt_thread
open Mt_cfg_types
open Mt_mutexes_types

(* -------------------------------------------------------------------------- *)
(* --- Checking that concurrent variables accesses are properly protected --- *)
(* -------------------------------------------------------------------------- *)


let mutexes_protecting_zones' accesses =
  let aux (z, set) =
    SetNodeIdAccess.fold
      (fun (rw, node, _id) acc ->
         let mutexes = node.cfgn_context.locked_mutexes in
         let mut = match rw with
           | Read -> { mutexes_for_write = Unaccessed;
                       mutexes_for_read = Mutexes mutexes }
           | Write _ -> { mutexes_for_read = Unaccessed;
                          mutexes_for_write = Mutexes mutexes }
           | ReadPos _ -> { mutexes_for_read = Mutexes mutexes;
                            mutexes_for_write = Unaccessed }
           | WritePos _ -> { mutexes_for_read = Unaccessed;
                             mutexes_for_write = Mutexes mutexes }
         in
         MutexesByZone.add_binding ~exact:false acc z mut
      ) set MutexesByZone.empty
  in
  let r1 = List.map aux accesses in
  let z = List.fold_left
      (fun r r' -> MutexesByZone.join r r') MutexesByZone.empty r1
  in z


(* Pretty a value of type [Mt_shared_vars.Precise.list_accesses]
   with the mutex information at each node concatenated to the output *)
let pretty_with_mutexes =
  Mt_shared_vars.Precise.pretty_concurrent_accesses
    ~f:(fun fmt (_, node, _) ->
        let mutexes = node.cfgn_context.locked_mutexes in
        if MutexPresence.is_empty mutexes then
          Format.fprintf fmt ",@ unprotected"
        else
          Format.fprintf fmt ",@ @[<hov>protected by %a@]"
            MutexPresence.pretty mutexes ;
        if Mt_options.PrintCallstacks.get ()
        then Format.fprintf fmt ",@ // %a" Callstack.pretty node.cfgn_stack
      ) ();
;;

type protection = Unprotected | Priority | Protected of Mutex.Set.t

let pretty_protection fmt = function
  | Unprotected -> Format.fprintf fmt "unprotected"
  | Priority -> Format.fprintf fmt "protected by priorities"
  | Protected set ->
    Format.fprintf fmt "@[<hov 2>protected by %a@]"
      (Pretty_utils.pp_iter Mutex.Set.iter Mutex.pretty) set

let pretty_protection_per_thread fmt (th_read, th_write, protection) =
  Format.fprintf fmt "@[<hov 2>Read by %a,@ Write by %a:@ %a@]"
    Thread.pretty th_read Thread.pretty th_write
    pretty_protection protection

type zone_protection =
  (Memory_zone.t * (Thread.t * Thread.t * protection) list) list

let pretty_zone_protection fmt (z, l) =
  Format.fprintf fmt "@[<hv 2>@[%a@]:@ %a@]"
    Memory_zone.pretty z
    (Pretty_utils.pp_list ~pre:"" ~suf:"" pretty_protection_per_thread) l

let check_protection analysis (l: Mt_shared_vars.Precise.list_accesses) : zone_protection =
  let aux (z, s) =
    let m_read = ref Thread.Map.empty in
    let m_write = ref Thread.Map.empty in
    (* YYY: we disregard information about accesses that may not be possibly
       simultaneous *)
    let add th node map =
      let mutexes' = MutexPresence.only_present node.cfgn_context.locked_mutexes in
      try
        let mutexes = Thread.Map.find th map in
        let inter = Mutex.Set.inter mutexes mutexes' in
        Thread.Map.add th inter map
      with Not_found -> Thread.Map.add th mutexes' map
    in
    let aux_nodes (op, n, th) =
      match op with
      | Read ->    m_read  := add th n !m_read
      | Write _ -> m_write := add th n !m_write
      | ReadPos _ -> m_read := add th n !m_read
      | WritePos _ -> m_write := add th n !m_write
    in
    SetNodeIdAccess.iter aux_nodes s;
    let classify_access th_read read th_write write classified =
      if not (Thread.equal th_read th_write) then begin
        let th_read_state = Mt_thread.thread_state analysis th_read in
        let th_write_state = Mt_thread.thread_state analysis th_write in
        let protection =
          match th_read_state.th_priority, th_write_state.th_priority with
          | PPriority p1, PPriority p2 when p1 > p2 ->
            (* Protection by mutexes not needed, th_read cannot be preempted *)
            Priority
          | _ ->
            let both_mutexes = Mutex.Set.inter read write in
            if Mutex.Set.is_empty both_mutexes
            then Unprotected
            else Protected both_mutexes
        in
        (th_read, th_write, protection) :: classified
      end
      else classified
    in
    let protections =
      Thread.Map.fold (fun th_read read acc ->
          Thread.Map.fold (fun th_write write acc ->
              classify_access th_read read th_write write acc
            ) !m_write acc
        ) !m_read []
    in
    (z, protections)
  in
  List.map aux l

let pretty_protections fmt l =
  Pretty_utils.pp_list
    ~pre:"@[<v>" ~suf:"@]" ~sep:"@ " pretty_zone_protection fmt l

let ill_protected (accesses: Mt_shared_vars.Precise.list_accesses) (protections: zone_protection) =
  let res = Cil_datatype.Stmt.Hashtbl.create 16 in
  let aux (z, nodes) (z', protections) =
    assert (z == z');
    let aux (th_read, _th_write, protect) =
      if protect = Unprotected then
        let aux (op, node, th) =
          if Thread.equal th th_read && op = Read then begin
            let stmts = CfgNode.node_stmt node in
            let aux stmt =
              let prev =
                try Cil_datatype.Stmt.Hashtbl.find res stmt
                with Not_found -> Memory_zone.bottom
              in
              let z = Memory_zone.join prev z in
              Cil_datatype.Stmt.Hashtbl.replace res stmt z
            in
            List.iter aux stmts
          end
        in
        SetNodeIdAccess.iter aux nodes
    in
    List.iter aux protections
  in
  List.iter2 aux accesses protections;
  res

let need_sync stmtsh =
  let aux stmt z acc =
    (* YYY: detection should be improved to handle unspecified sequences. *)
    match stmt.preds with
    | [stmt] when Mt_cil.is_call_to_sync stmt -> acc
    | _ -> (stmt, z) :: acc
  in
  Cil_datatype.Stmt.Hashtbl.fold aux stmtsh []
