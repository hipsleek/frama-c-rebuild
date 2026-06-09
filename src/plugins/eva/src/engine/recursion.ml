(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eval

module Varinfo = Cil_datatype.Varinfo

let mark_unknown_requires kinstr kf funspec =
  let stmt =
    match kinstr with
    | Kglobal -> assert false
    | Kstmt stmt -> stmt
  in
  let emitter = Eva_utils.emitter in
  let status = Property_status.Dont_know in
  let emit_behavior behavior =
    let emit_predicate predicate =
      let ip = Property.ip_of_requires kf Kglobal behavior predicate in
      Statuses_by_call.setup_precondition_proxy kf ip;
      let property = Statuses_by_call.precondition_at_call kf ip stmt in
      Property_status.emit ~distinct:true emitter ~hyps:[] property status
    in
    List.iter emit_predicate behavior.b_requires
  in
  List.iter emit_behavior funspec.spec_behavior

let check_spec kinstr kf =
  let funspec = Annotations.funspec kf in
  if List.for_all (fun b -> b.b_assigns = WritesAny) funspec.spec_behavior
  then
    Self.warning ~current:true ~wkey:Self.wkey_missing_assigns ~stacktrace:true
      "@[Recursive call to %a without assigns clause.@ \
       Generating probably incomplete assigns to interpret the call.@ \
       Try to increase the %s parameter \
       or write a correct specification for function %a.@]"
      Kernel_function.pretty kf
      Parameters.RecursiveUnroll.name
      Kernel_function.pretty kf
  else
    let depth = Parameters.RecursiveUnroll.get () in
    Self.warning ~once:true ~current:true
      "@[Using specification of function %a@ for recursive calls%s.@ \
       Analysis of function %a@ is thus incomplete@ and its soundness@ \
       relies on the written specification.@]"
      Kernel_function.pretty kf
      (if depth > 0 then Format.asprintf " of depth %i" depth else "")
      Kernel_function.pretty kf;
    mark_unknown_requires kinstr kf funspec

(* -------------------------------------------------------------------------- *)

module CallDepth =
  Datatype.Pair_with_collections (Kernel_function) (Datatype.Int)

module VarCopies =
  Datatype.List (Datatype.Pair (Varinfo) (Varinfo))

module VarStack =
  State_builder.Hashtbl
    (CallDepth.Hashtbl)
    (VarCopies)
    (struct
      let name = "Eva.Recursion.VarStack"
      let dependencies = [ Ast.self ]
      let size = 9
    end)

let copy_variable fundec depth varinfo =
  let name = Format.asprintf "\\copy<%s>[%i]" varinfo.vname depth in
  let v = Cil.copyVarinfo varinfo name in
  Cil.refresh_local_name fundec v;
  v

let make_stack (kf, depth) =
  let fundec =
    try Kernel_function.get_definition kf
    with Kernel_function.No_Definition -> assert false
  in
  let vars = Kernel_function.(get_formals kf @ get_locals kf) in
  let copy v = v, copy_variable fundec depth v in
  List.map copy vars

let get_stack kf depth = VarStack.memo make_stack (kf, depth)

let make_recursion call depth =
  let wkey = Self.wkey_recursion in
  Self.warning ~wkey ~once:true ~current:true ~stacktrace:true
    "@[detected recursive call@ of function %a.@]"
    Kernel_function.pretty call.kf;
  let substitution = get_stack call.kf depth in
  let add_if_copy acc argument =
    match argument.avalue with
    | Copy ({ lval = { node = Var vi, _ } }, _) -> Varinfo.Set.add vi acc
    | _ -> acc
  in
  let empty = Varinfo.Set.empty in
  let copied_varinfos = List.fold_left add_if_copy empty call.arguments in
  let may_be_used (vi, _) = vi.vaddrof || Varinfo.Set.mem vi copied_varinfos in
  let substitution, withdrawal = List.partition may_be_used substitution in
  let withdrawal = List.map fst withdrawal in
  let base_of_varinfo (v1, v2) = Base.of_varinfo v1, Base.of_varinfo v2 in
  let list_substitution = List.map base_of_varinfo substitution in
  let base_substitution = Base.substitution_from_list list_substitution in
  let list_withdrawal = List.map Base.of_varinfo withdrawal in
  let base_withdrawal = Base.Hptset.of_list list_withdrawal in
  { depth; substitution; base_substitution; withdrawal; base_withdrawal; }

let make call =
  let is_same_kf = Kernel_function.equal call.kf in
  let all_calls = Callstack.to_kf_list call.callstack in
  let previous_calls = List.filter is_same_kf all_calls in
  let depth = List.length previous_calls - 1 in
  if depth > 0
  then Some (make_recursion call depth)
  else None

let revert recursion =
  let revert (v1, v2) = v2, v1 in
  let substitution = List.map revert recursion.substitution in
  let base_of_varinfo (v1, v2) = Base.of_varinfo v1, Base.of_varinfo v2 in
  let list = List.map base_of_varinfo substitution in
  let base_substitution = Base.substitution_from_list list in
  { recursion with substitution; base_substitution; }
