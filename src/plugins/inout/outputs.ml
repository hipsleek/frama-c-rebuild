(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Visitor

class virtual do_it_ = object(self)
  inherit [Memory_zone.t] Cumulative_analysis.cumulative_visitor as super
  val mutable outs = Memory_zone.bottom

  method bottom = Memory_zone.bottom

  method result = outs

  method! vstmt_aux s =
    match s.skind with
    | UnspecifiedSequence seq ->
      List.iter
        (fun (stmt,_,_,_,_) ->
           ignore(visitFramacStmt (self:>frama_c_visitor) stmt))
        seq;
      Cil.SkipChildren (* do not visit the additional lvals *)
    | _ -> super#vstmt_aux s

  method join new_ =
    outs <- Memory_zone.join new_ outs;

    (* For local initializations, counts the written variable as an output of the
       function, even if it is const; thus, [for_writing] is false in this case. *)
  method private do_assign ~for_writing lv =
    let ki = self#current_kinstr in
    let bits_loc =
      Eva.Results.(before_kinstr ki |> eval_address ~for_writing lv |> as_zone)
    in
    self#join bits_loc

  method! vinst i =
    let stmt = Option.get self#current_stmt in
    if Eva.Results.is_reachable stmt
    then
      (* noassert needed for Eval.memoize. Not really satisfactory *)
      begin
        let assign_lval lval =
          let for_writing = not (Cil.is_mutable_or_initialized lval) in
          self#do_assign ~for_writing lval
        in
        match i with
        | Set (lv,_,_) ->
          assign_lval lv
        | Call (lv_opt, lv, _, _) ->
          begin
            Option.iter assign_lval lv_opt;
            let callees = Eva.Results.(before stmt |> eval_callee lv) in
            match callees with
            | Ok callees ->
              let join_outputs kf =
                let inout = Operational_inputs.get_external_aux ~stmt kf in
                self#join inout.over_outputs
              in
              List.iter join_outputs callees
            | Error (Top | DisabledDomain) -> self#join Memory_zone.top
            | Error Bottom -> ()
          end
        | Local_init (v, AssignInit i, _) ->
          let rec aux lv = function
            | SingleInit _ -> self#do_assign ~for_writing:false lv
            | CompoundInit (ct, initl) ->
              (* Avoid folding the implicit zero-initializers of large arrays. *)
              if Cumulative_analysis.fold_implicit_initializer ct
              then
                let implicit = true in
                let doinit o i _ () = aux (Cil.addOffsetLval o lv) i in
                Cil.foldLeftCompound ~implicit ~doinit ~ct ~initl ~acc:()
              else
                (* For arrays of scalar elements, all the zone covered by the
                   array is written. For arrays of structs containing padding
                   bits, this is a sound over-approximation. *)
                self#do_assign ~for_writing:false lv
          in
          aux (Cil.var v) i
        | Local_init (v, ConsInit(f, _, _),_) ->
          if Cvalue.Model.is_top Eva.Results.(before stmt |> get_cvalue_model)
          then self#join Memory_zone.top
          else begin
            let { Inout_type.over_outputs = z }  =
              Operational_inputs.get_external_aux ?stmt:self#current_stmt
                (Globals.Functions.get f)
            in
            self#do_assign ~for_writing:false (Cil.var v);
            (* might be redundant with z in case f takes address of
               v as first argument, but this shouldn't hurt. *)
            self#join z
          end
        | Asm _ | Skip _ | Code_annot _ -> ()
      end;
    Cil.SkipChildren

  method clean_kf_result kf r =
    Memory_zone.filter_base
      (Eva.Logic_inout.accept_base ~formals:true ~locals:true kf)
      r

  method compute_funspec kf =
    let state = self#specialize_state_on_call kf in
    let behaviors = Eva.Logic_inout.valid_behaviors kf state in
    let assigns = Ast_info.merge_assigns behaviors in
    Eva.Logic_inout.assigns_outputs_to_zone state ~result:None assigns
end

module Analysis = Cumulative_analysis.Make(
  struct
    let analysis_name ="outputs"

    type t = Memory_zone.t
    module T = Memory_zone

    class virtual do_it = do_it_
  end)

let get_internal = Analysis.kernel_function

let externalize kf x =
  Memory_zone.filter_base
    (Eva.Logic_inout.accept_base ~formals:false ~locals:false kf)
    x

module Externals =
  Kernel_function.Make_Table(Memory_zone)
    (struct
      let name = "Inout.Outputs.Externals"
      let dependencies = [ Analysis.Memo.self ]
      let size = 17
    end)

let get_external =
  Externals.memo (fun kf -> externalize kf (get_internal kf))

let pretty_internal fmt kf =
  try
    Format.fprintf fmt "@[Out (internal) for function %a:@\n@[<hov 2>  %a@]@]@\n"
      Kernel_function.pretty kf
      Memory_zone.pretty (get_internal kf)
  with Not_found ->
    ()

let pretty_external fmt kf =
  try
    Format.fprintf fmt "@[Out (external) for function %a:@\n@[<hov 2>  %a@]@]@\n"
      Kernel_function.pretty kf
      Memory_zone.pretty (get_external kf)
  with Not_found ->
    ()

let self = Externals.self
let compute kf = ignore (get_internal kf)
let statement = Analysis.statement

(* Registers functions used by Eva via the dynamic API. *)

let _kf_outputs =
  Dynamic.register
    ~comment:"Returns the memory zone modified by a given function."
    ~plugin:Inout_parameters.name
    "kf_outputs"
    Datatype.(func Kernel_function.ty Memory_zone.ty)
    get_internal
