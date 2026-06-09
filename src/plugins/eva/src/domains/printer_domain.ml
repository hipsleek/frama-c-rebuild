(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval


(** An abstract domain based on Simple_Cvalue that will literally just print
    what goes through it. *)
module Simple : Simpler_domains.Simple_Cvalue = struct

  let feedback = Self.feedback ~current:true

  (* --- Datatype --- *)

  (* In this domain, the states contain nothing. We use [unit] as type formal
     the state and we reuse [Datatype.Unit] as a base for our domain. *)
  include Datatype.Unit
  let name = "printer"

  (* --- Lattice operators --- *)

  let top = ()
  let is_included _v1 _v2 =
    feedback "is_included";
    true

  let join _v1 _v2 =
    feedback "join";
    top

  let widen _kf _stmt _v1 v2 =
    feedback "widen";
    v2

  (* --- Query functions --- *)

  let extract_expr _state _exp =
    `Value (Cvalue.V.top)

  let extract_lval _state _lval _loc =
    `Value (Cvalue.V.top)

  (* --- Transfer functions --- *)

  let pp_list = Pretty_utils.pp_list ~sep:",@ "

  let pp_cvalue fmt value =
    Bottom.pretty Cvalue.V.pretty fmt value

  let pp_cvalue_assigned fmt value =
    pp_cvalue fmt (Eval.value_assigned value)

  let pp_arg fmt arg =
    Format.fprintf fmt "%a = %a"
      Eva_ast.pp_exp arg.concrete
      pp_cvalue_assigned arg.avalue

  let assign ~pos:_ loc exp cvalue_assigned _valuation state =
    feedback "assign %a with %a = %a"
      Eva_ast.pp_lval loc.lval
      Eva_ast.pp_exp exp
      pp_cvalue_assigned cvalue_assigned;
    `Value state

  let assume ~pos:_ exp truth _valuation state =
    feedback "assume %a is %b"
      Eva_ast.pp_exp exp
      truth;
    `Value state

  let start_call ~pos:_ call _valuation state =
    feedback "start call %s(%a)"
      (Kernel_function.get_name call.kf)
      (pp_list pp_arg) call.arguments;
    state

  let finalize_call ~pos:_ call ~pre:_ ~post =
    feedback  "finalize call to %s" (Kernel_function.get_name call.kf);
    `Value post

  (* --- Initialization of variables --- *)

  let pp_vi_list fmt l =
    pp_list Printer.pp_varinfo fmt l

  let pp_init_val fmt = function
    | Abstract_domain.Zero -> Format.fprintf fmt "0"
    | Abstract_domain.Top  -> Format.fprintf fmt "Top"

  let empty () =
    feedback "empty";
    ()

  let initialize_variable lval ~initialized:_ init state =
    feedback "initialize_variable %a with %a"
      Eva_ast.pp_lval lval
      pp_init_val init;
    state

  let enter_scope _kind vi_list state =
    feedback "enter_scope %a" pp_vi_list vi_list;
    state

  let leave_scope _kf vi_list state =
    feedback "leave_scope %a"  pp_vi_list vi_list;
    state
end

module Domain = Domain_builder.Complete_Simple_Cvalue (Simple)
include Domain

let registered =
  let name = "printer"
  and descr =
    "Debug domain, only useful for developers. Prints the transfer functions \
     used during the analysis."
  in
  Abstractions.Domain.register ~name ~descr (module Domain)
