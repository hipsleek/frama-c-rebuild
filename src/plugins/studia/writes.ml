(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Find the statements that writes a given zone. This is a lightweight version
    of module [Scope.Defs]. Instead of using PDGs (that may be very costly to
    compute), we only use Inout. This also means that we can find effects
    *after* the stmt the user has chosen. *)

open Cil_types

type t =
  | Assign of Cil_types.stmt
  | CallDirect of Cil_types.stmt
  | CallIndirect of Cil_types.stmt
  | GlobalInit of Cil_types.varinfo * Cil_types.initinfo
  | FormalInit of
      Cil_types.varinfo *
      (Cil_types.kernel_function * Cil_types.stmt list) list

let (<?>) c lcmp =
  if c <> 0 then c else Lazy.force lcmp

let compare w1 w2 =
  let open Cil_datatype in
  match w1, w2 with
  | Assign s1, Assign s2
  | CallDirect s1, CallDirect s2
  | CallIndirect s1, CallIndirect s2 -> Stmt.compare s1 s2
  | GlobalInit (v1, i1), GlobalInit (v2, i2) ->
    Varinfo.compare v1 v2 <?> lazy (Initinfo.compare i1 i2)
  | FormalInit (v1, cs1), FormalInit (v2, cs2) ->
    let compare_callsites (kf1, stmts1) (kf2, stmts2) =
      Kf.compare kf1 kf2 <?> lazy (List.compare Stmt.compare stmts1 stmts2)
    in
    Varinfo.compare v1 v2 <?> lazy (List.compare compare_callsites cs1 cs2)
  | Assign _, _ -> 1
  | _, Assign _ -> -1
  | CallDirect _, _ -> 1
  | _, CallDirect _ -> -1
  | CallIndirect _, _ -> 1
  | _, CallIndirect _ -> -1
  | GlobalInit _, _ -> 1
  | _, GlobalInit _ -> -1

(** Does the functions called at [stmt] modify directly or indirectly [zlval] *)
let effects_of_call stmt zlval effects  =
  let aux_kf (direct, indirect) kf =
    let inout = Inout.get_precise_inout ~stmt kf in
    let out = inout.Inout_type.over_outputs in
    if Memory_zone.intersects out zlval then
      if Eva.Analysis.use_spec_instead_of_definition kf then
        (true, indirect) (* Direct effect: there is no body for this function. *)
      else
        (direct, true) (* Indirect effect *)
    else
      (direct, indirect)
  in
  let kfs = Eva.Results.callee stmt in
  List.fold_left aux_kf effects kfs

class find_write_insts zlval = object (self)
  inherit Visitor.frama_c_inplace

  val mutable res : t list = []

  method! vinst i =
    let stmt = Option.get self#current_stmt in
    begin
      let aux_call lvopt _kf _args _loc =
        (* Direct effect through the writing of [lvopt], or indirect inside
           the call. *)
        let z = Inout.stmt_outputs stmt in
        if Memory_zone.intersects z zlval then
          let direct = match lvopt with
            | None -> false
            | Some lv ->
              Eva.Results.(before stmt |> eval_address lv |> as_zone_result) |>
              Result.fold ~ok:(Memory_zone.intersects zlval) ~error:(fun _ -> false)
          in
          let direct, indirect = effects_of_call stmt zlval (direct, false) in
          if direct then
            res <- CallDirect stmt :: res;
          if indirect then
            res <- CallIndirect stmt :: res;
      in
      match i with
      | Set _ | Local_init(_, AssignInit _, _) ->
        (* Effect only through the written l-value *)
        let z = Inout.stmt_outputs stmt in
        if Memory_zone.intersects z zlval then begin
          res <- Assign stmt :: res
        end
      | Call (lvopt, f, args, loc) -> aux_call lvopt f args loc
      | Local_init(v, ConsInit(f, args, k), l) ->
        Cil.treat_constructor_as_func aux_call v f args k l
      | _ -> () (* No effect *)
    end;
    Cil.SkipChildren

  method result = res
end

let inst_writes z =
  let vis = new find_write_insts z in
  let aux_kf_fundec kf =
    let all_out = Inout.get_precise_inout kf in
    let zout = all_out.Inout_type.over_outputs in
    if Memory_zone.intersects zout z then begin
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

let base_inits base acc =
  match Base.to_varinfo base with
  | exception Base.Not_a_C_variable -> acc
  | vi when vi.vglob && vi.vsource ->
    let initinfo = Globals.Vars.find vi in
    GlobalInit (vi, initinfo) :: acc
  | vi when vi.vformal ->
    let kf = Option.get (Kernel_function.find_defining_kf vi) in
    let callsites = Eva.Results.callsites kf in
    FormalInit (vi, callsites) :: acc
  | _ -> acc (* Local init will be found by [inst_writes] *)

let compute z =
  inst_writes z @ Memory_zone.fold_bases base_inits z []
