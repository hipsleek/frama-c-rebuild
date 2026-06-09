(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Find the statements that reads a given zone, using Inout. (Thus,
    only operational reads are found.) *)

open Cil_types

type t =
  | Direct of Cil_types.stmt
  | Indirect of Cil_types.stmt

class find_read zlval = object
  inherit Visitor.frama_c_inplace

  val mutable res : t list = []

  method! vstmt_aux stmt =
    let aux_call lvopt _kf args _loc =
      let z = Inout.stmt_inputs stmt in
      if Memory_zone.intersects z zlval then begin
        (* Computes what is read to evaluate [args] and [lvopt] *)
        let deps =
          List.map (Inout.expr_inputs stmt) args
        in
        let deps = List.fold_left Memory_zone.join Memory_zone.bottom deps in
        let deps = match lvopt with
          | None -> deps
          | Some lv ->
            let dlv = Eva.Results.(before stmt |> address_deps lv) in
            Memory_zone.join dlv deps
        in
        let direct = Memory_zone.intersects deps zlval in
        (* now determine if the functions called at [stmt] read directly or
             indirectly [zlval] *)
        let aux_kf (direct, indirect) kf =
          let inputs = Inout.kf_inputs kf in
          (* TODO: change to this once we can get "full" inputs through Inout.
             Currently, non operational inputs disappear, and this function
             is not suitable.
             let inout = !Db.Operational_inputs.get_internal_precise ~stmt kf in
             let inputs = inout.Inout_type.over_inputs in *)
          if Memory_zone.intersects inputs zlval then
            if Eva.Analysis.use_spec_instead_of_definition kf then
              (* Direct, as there is no body for this function. *)
              (true, indirect)
            else
              (direct, true) (* Indirect effect *)
          else
            (direct, indirect) (* this function pointer does not read [zlval] *)
        in
        let kfs = Eva.Results.callee stmt in
        let direct, indirect = List.fold_left aux_kf (direct, false) kfs in
        if direct then
          res <- Direct stmt :: res;
        if indirect then
          res <- Indirect stmt :: res;
      end
    in
    match stmt.skind with
    | Instr (Call (lvopt, f, args, loc)) ->
      aux_call lvopt f args loc;
      Cil.SkipChildren
    | Instr (Local_init(v, ConsInit(f, args, k), l)) ->
      Cil.treat_constructor_as_func aux_call v f args k l;
      Cil.SkipChildren
    | Instr _ ->
      let z = Inout.stmt_inputs stmt in
      if Memory_zone.intersects z zlval then begin
        res <- Direct stmt :: res
      end;
      Cil.SkipChildren
    | If (e, _, _, _) | Switch (e, _, _, _) | Return (Some e, _) ->
      let z = Inout.expr_inputs stmt e in
      if Memory_zone.intersects z zlval then begin
        res <- Direct stmt :: res
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren

  method result = res
end

let compute z =
  let vis = new find_read z in
  let aux_kf_fundec kf =
    let all_in = Inout.kf_inputs kf in
    if Memory_zone.intersects all_in z then begin
      let fundec = Kernel_function.get_definition kf in
      ignore
        (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) fundec;)
    end
  in
  let aux_kf kf =
    if Kernel_function.is_definition kf then aux_kf_fundec kf
  in
  Globals.Functions.iter aux_kf;
  vis#result
