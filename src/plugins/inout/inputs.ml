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
  val mutable inputs = Memory_zone.bottom

  method bottom = Memory_zone.bottom

  method result = inputs

  method join new_ =
    inputs <- Memory_zone.join new_ inputs;

  method! vstmt_aux s =
    match s.skind with
    | UnspecifiedSequence seq ->
      List.iter
        (fun (stmt,_,_,_,_) ->
           ignore (visitFramacStmt (self:>frama_c_visitor) stmt))
        seq;
      Cil.SkipChildren (* do not visit the additional lvals *)
    | _ -> super#vstmt_aux s

  (* On assignment and addrof, only read the lvalue address.  *)
  method private read_address lv =
    let request = Eva.Results.before_kinstr self#current_kinstr in
    let deps = Eva.Results.address_deps lv request in
    self#join deps

  method! vlval lv =
    let request = Eva.Results.before_kinstr self#current_kinstr in
    let deps = Eva.Results.lval_deps lv request in
    self#join deps;
    Cil.SkipChildren

  method private do_arg_calls stmt f args =
    let deps = Eva.Results.(before stmt |> lval_deps (f,NoOffset)) in
    self#join deps;
    let () =
      match Eva.Results.(before stmt |> eval_callee f) with
      | Ok callees ->
        List.iter (fun kf -> self#join (self#compute_kf kf)) callees
      | Error (Top | DisabledDomain) -> self#join Memory_zone.top;
      | Error Bottom -> ()
    in
    List.iter (fun e -> ignore (visitFramacExpr (self:>frama_c_visitor) e)) args;


  method! vinst i =
    let stmt = Option.get self#current_stmt in
    if Eva.Results.is_reachable stmt then begin
      match i with
      | Set (lv,exp,_) ->
        self#read_address lv;
        ignore (visitFramacExpr (self:>frama_c_visitor) exp);
        Cil.SkipChildren

      | Local_init(v, AssignInit i,_) ->
        let rec aux lv = function
          | SingleInit { enode = Lval (Var vi, NoOffset) }
            when Ast_info.is_string_literal vi ->
            self#read_address lv
          | SingleInit e ->
            self#read_address lv;
            ignore (visitFramacExpr (self:>frama_c_visitor) e)
          | CompoundInit (ct,initl) ->
            (* No need to consider implicit zero-initializers, for which
               nothing is read. *)
            let implicit = false in
            let doinit o i _ () =
              ignore (visitFramacOffset (self:>frama_c_visitor) o);
              aux (Cil.addOffsetLval o lv) i
            in
            Cil.foldLeftCompound ~implicit ~doinit ~ct ~initl ~acc:()
        in
        aux (Cil.var v) i;
        Cil.SkipChildren

      | Call (lv_opt,lv,args,_) ->
        Option.iter self#read_address lv_opt;
        self#do_arg_calls stmt lv args;
        Cil.SkipChildren
      | Local_init(v, ConsInit(f, args, Plain_func), _) ->
        self#read_address (Cil.var v);
        self#do_arg_calls stmt (Var f) args;
        Cil.SkipChildren
      | Local_init(v, ConsInit(f, args, Constructor), _) ->
        self#do_arg_calls stmt (Var f) (Cil.mkAddrOfVi v :: args);
        Cil.SkipChildren
      | Skip _ | Asm _ | Code_annot _ -> Cil.DoChildren
    end
    else Cil.SkipChildren

  method! vexpr exp =
    match exp.enode with
    | AddrOf lv | StartOf lv ->
      self#read_address lv;
      Cil.SkipChildren
    | SizeOfE _ | AlignOfE _ | SizeOf _ | AlignOf _ ->
      (* we're not evaluating an expression here: there's no input. *)
      Cil.SkipChildren
    | _ -> Cil.DoChildren

  method compute_funspec kf =
    let state = self#specialize_state_on_call kf in
    let behaviors = Eva.Logic_inout.valid_behaviors kf state in
    let assigns = Ast_info.merge_assigns behaviors in
    Eva.Logic_inout.assigns_inputs_to_zone state assigns

  method clean_kf_result (_ : kernel_function) (r: Memory_zone.t) = r
end


module Analysis = Cumulative_analysis.Make(
  struct
    let analysis_name ="inputs"

    type t = Memory_zone.t
    module T = Memory_zone

    class virtual do_it = do_it_
  end)

let get_internal = Analysis.kernel_function

module Externals =
  Kernel_function.Make_Table(Memory_zone)
    (struct
      let name = "Inout.Inputs.Externals"
      let dependencies = [ Analysis.Memo.self ]
      let size = 17
    end)

let get_external =
  Externals.memo
    (fun kf ->
       Memory_zone.filter_base
         (Eva.Logic_inout.accept_base ~formals:false ~locals:false kf)
         (get_internal kf))

let get_with_formals kf =
  Memory_zone.filter_base
    (Eva.Logic_inout.accept_base ~formals:true ~locals:false kf)
    (get_internal kf)

let compute kf = ignore (get_external kf)

let pretty_external fmt kf =
  Format.fprintf fmt "@[Inputs for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Memory_zone.pretty (get_external kf)

let pretty_with_formals fmt kf =
  Format.fprintf fmt "@[Inputs (with formals) for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Memory_zone.pretty (get_with_formals kf)

let self = Externals.self
let statement = Analysis.statement
let expr = Analysis.expr
