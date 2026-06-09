(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Visitor

(* To avoid a performance issue, do not fold implicit zero-initializers of large
   arrays. For arrays of scalar elements, the outputs of an initializer is
   exactly the zone covered by the array. For arrays containing structs with
   padding bits, this is an over-approximation, so we prefer folding the
   initializer if the array is not too big (the 100 cells limit is arbitrary).
   We still need to fold the explicit initializers to collect the inputs. *)
let fold_implicit_initializer typ =
  not
    (Ast_types.is_array typ &&
     (Ast_types.(is_scalar (direct_element_type typ))
      || Ast_info.array_size typ > 100z))

let specialize_state_on_call ?stmt kf =
  match stmt with
  | None -> Eva.Results.(at_start_of kf |> get_cvalue_model)
  | Some stmt ->
    let filter = fun cs -> match Eva.Callstack.top_callsite cs with
      | Kstmt s -> Cil_datatype.Stmt.equal s stmt
      | Kglobal -> false
    in
    Eva.Results.(at_start_of kf |> filter_callstack filter |> get_cvalue_model)

class virtual ['a] cumulative_visitor = object
  inherit frama_c_inplace as self

  method specialize_state_on_call kf =
    specialize_state_on_call ?stmt:self#current_stmt kf

  method virtual compute_kf: kernel_function -> 'a

end

class type virtual ['a] cumulative_class = object
  inherit ['a] cumulative_visitor

  method bottom: 'a

  method result: 'a
  method join: 'a -> unit

  method compute_funspec : kernel_function -> 'a

  method clean_kf_result: kernel_function -> 'a -> 'a
end


module Make (X:
             sig
               val analysis_name: string

               type t
               module T: Datatype.S with type t = t

               class virtual do_it: [t] cumulative_class
             end) =
struct

  module Memo =
    Kernel_function.Make_Table(X.T)
      (struct
        let name = "Inout.Cumulative_analysis.Memo(" ^ X.analysis_name ^ ")"
        let dependencies = [ Eva.Analysis.self ]
        let size = 97
      end)

  class do_it_cached call_stack = object(self)
    inherit X.do_it

    (* The cycle variable holds the list of functions that are
       involved in a cycle. As long as it is not empty, we known that
       the results we are computing are not complete, and we do not memorize
       them *)
    val mutable cycle = Kernel_function.Hptset.empty
    method private add_cycle s = cycle <- Kernel_function.Hptset.union s cycle
    method cycle = cycle

    (* Computation using the body of a kernel function. The result is
       automatically cached by the function if possible *)
    method private compute_kf_with_def kf =
      let f = Kernel_function.get_definition kf in
      if List.exists (Kernel_function.equal kf) call_stack then (
        self#add_cycle (Kernel_function.Hptset.singleton kf);
        self#bottom
      )
      else
        let computer = new do_it_cached (kf :: call_stack) in
        ignore (visitFramacFunction (computer:>frama_c_visitor) f);
        (* Results on all the statements of the function *)
        let v = computer#result in
        let v = computer#clean_kf_result kf v in
        (* recursive calls detected during analysis of the statements*)
        let cycle_aux = Kernel_function.Hptset.remove kf computer#cycle in
        self#add_cycle cycle_aux;
        if Kernel_function.Hptset.is_empty cycle then (
          (* No recursive calls, our results are correct *)
          Inout_parameters.debug "Caching %s result for %a"
            X.analysis_name Kernel_function.pretty kf;
          Memo.add kf v;
        ) else
          Inout_parameters.debug
            "Not caching %s result for %a because of cycle"
            X.analysis_name Kernel_function.pretty kf;
        v

    (* Computation and caching for a kernel function, using its spec *)
    method private compute_kf_with_spec_generic kf =
      try Memo.find kf
      with Not_found ->
        let r_glob = self#compute_funspec kf in
        let r_glob = self#clean_kf_result kf r_glob in
        Memo.add kf r_glob;
        r_glob

    method compute_kf kf =
      if Eva.Analysis.use_spec_instead_of_definition kf then
        (* If only a declaration is available, or we are instructed to use
           the spec, do so. If a current stmt is available (most of the times),
           do not cache the results. Maybe [compute_funspec] will be able
           to deliver a more precise result on this given statement *)
        match self#current_stmt with
        | None -> self#compute_kf_with_spec_generic kf
        | Some _stmt -> self#compute_funspec kf
      else
        try Memo.find kf
        with Not_found -> self#compute_kf_with_def kf
  end

  let statement stmt =
    let computer = new do_it_cached [] in
    ignore (visitFramacStmt (computer:>frama_c_visitor) stmt);
    assert (Kernel_function.Hptset.is_empty computer#cycle);
    computer#result

  let expr stmt e =
    let computer = new do_it_cached [] in
    computer#push_stmt stmt;
    ignore (visitFramacExpr (computer:>frama_c_visitor) e);
    assert (Kernel_function.Hptset.is_empty computer#cycle);
    computer#result

  let kernel_function kf =
    let computer = new do_it_cached [] in
    computer#join (computer#compute_kf kf);
    assert (Kernel_function.Hptset.is_empty computer#cycle);
    computer#result

end
