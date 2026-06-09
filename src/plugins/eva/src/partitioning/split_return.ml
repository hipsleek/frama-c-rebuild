(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* Auxiliary module for inference of split criterion. We collect all the
   usages of a function call, and all places where they are compared against
   an integral constant *)
module ReturnUsage = struct
  let debug = false

  module MapLval = Cil_datatype.Lval.Map

  (* Uses of a given lvalue *)
  type return_usage_by_lv = {
    ret_callees: Kernel_function.Hptset.t (* all the functions that put their
                                             results in this lvalue *);
    ret_compared: Z.Set.t (* all the constant values this
                                            lvalue is compared against *);
  }
  (* Per-function usage: all interesting lvalues are mapped to the way
     they are used *)
  and return_usage_per_fun = return_usage_by_lv MapLval.t

  module RUDatatype = Kernel_function.Map.Make(Z.Set)

  let find_or_default uf lv =
    try MapLval.find lv uf
    with Not_found -> {
        ret_callees = Kernel_function.Hptset.empty;
        ret_compared = Z.Set.empty;
      }

  (* Treat a [Call] instruction. Immediate calls (no functions pointers)
     are added to the current usage store *)
  let add_call (uf: return_usage_per_fun) lv_opt f =
    match f, lv_opt with
    | Var vi, Some lv
      when Ast_types.is_integral_or_pointer (Cil.typeOfLval lv) ->
      let kf = Globals.Functions.get vi in
      let u = find_or_default uf lv in
      let funs = Kernel_function.Hptset.add kf u.ret_callees in
      let u = { u with ret_callees = funs } in
      if debug then Format.printf
          "[Usage] %a returns %a@." Kernel_function.pretty kf Printer.pp_lval lv;
      MapLval.add lv u uf
    | _ , _-> uf

  (* Treat a [Set] instruction [lv = (cast) lv']. Useful for return codes
     that are stored inside values of a slightly different type *)
  let add_alias (uf: return_usage_per_fun) lv_dest e =
    match e.enode with
    | CastE (typ, { enode = Lval lve })
      when Ast_types.is_integral_or_pointer typ &&
           Ast_types.is_integral_or_pointer (Cil.typeOfLval lve)
      ->
      let u = find_or_default uf lve in
      MapLval.add lv_dest u uf
    | _ -> uf

  (* add a comparison with the integer [i] to the lvalue [lv] *)
  let add_compare_ct uf i lv =
    if Ast_types.is_integral_or_pointer (Cil.typeOfLval lv) then
      let u = find_or_default uf lv in
      let v = Z.Set.add i u.ret_compared in
      let u = { u with ret_compared = v } in
      if debug then Format.printf
          "[Usage] Comparing %a to %a@." Printer.pp_lval lv Z.pretty i;
      MapLval.add lv u uf
    else
      uf


  (* Treat an expression [lv == ct], [lv != ct] or [!lv], possibly with some
     cast. [ct] is added to the store of usages. *)
  let add_compare (uf: return_usage_per_fun) cond =
    (* if [ct] is an integer constant, memoize it is compared to [lv] *)
    let add ct lv =
      (match Cil.constFoldToInt ct with
       | Some i -> add_compare_ct uf i lv
       | _ -> uf)
    in
    (match cond.enode with
     | BinOp ((Eq | Ne), {enode = Lval lv}, ct, _)
     | BinOp ((Eq | Ne), ct, {enode = Lval lv}, _) -> add ct lv
     | BinOp ((Eq | Ne), {enode = CastE (typ, {enode = Lval lv})}, ct, _)
     | BinOp ((Eq | Ne), ct, {enode = CastE (typ, {enode = Lval lv})}, _) ->
       if Ast_types.is_integral_or_pointer typ &&
          Ast_types.is_integral_or_pointer (Cil.typeOfLval lv)
       then add ct lv
       else uf
     | UnOp (LNot, {enode = Lval lv}, _) ->
       add_compare_ct uf Z.zero lv

     | UnOp (LNot, {enode = CastE (typ, {enode = Lval lv})}, _)
       when Ast_types.is_integral_or_pointer typ &&
            Ast_types.is_integral_or_pointer (Cil.typeOfLval lv) ->
       add_compare_ct uf Z.zero lv

     | _ -> uf)

  (* Treat an expression [v] or [e1 && e2] or [e1 || e2]. This expression is
     supposed to be just inside an [if(...)], so that we may recognize patterns
     such as [if (f() && g())]. Patterns such as [if (f() == 1 && !g())] are
     handled in another way: the visitor recognizes comparison operators
     and [!], and calls {!add_compare}. *)
  let rec add_direct_comparison uf e =
    match e.enode with
    | Lval lv ->
      add_compare_ct uf Z.zero lv

    | CastE (typ, {enode = Lval lv})
      when Ast_types.is_integral_or_pointer typ &&
           Ast_types.is_integral_or_pointer (Cil.typeOfLval lv) ->
      add_compare_ct uf Z.zero lv

    | BinOp ((LAnd | LOr), e1, e2, _) ->
      add_direct_comparison (add_direct_comparison uf e1) e2

    | _ -> uf


  (* Per-program split strategy. Functions are mapped
     to the values their return code should be split against. *)
  type return_split = Z.Set.t Kernel_function.Map.t


  (* add to [kf] hints to split on all integers in [s]. *)
  let add_split kf s (ru:return_split) : return_split =
    let cur =
      try Kernel_function.Map.find kf ru
      with Not_found -> Z.Set.empty
    in
    let s = Z.Set.union cur s in
    Kernel_function.Map.add kf s ru


  (* Extract global usage: map functions to integers their return values
     are tested against *)
  let summarize_by_lv (uf: return_usage_per_fun): return_split =
    let aux _lv u acc =
      if Z.Set.is_empty u.ret_compared then acc
      else
        let aux_kf kf ru = add_split kf u.ret_compared ru in
        Kernel_function.Hptset.fold aux_kf u.ret_callees acc
    in
    MapLval.fold aux uf Kernel_function.Map.empty


  class visitorVarUsage = object
    inherit Visitor.frama_c_inplace

    val mutable usage = MapLval.empty

    method! vinst i =
      (match i with
       | Set (lv, e, _) ->
         usage <- add_alias usage lv e
       | Call (lv_opt, lv, _, _) ->
         usage <- add_call usage lv_opt lv
       | Local_init(v, AssignInit i, _) ->
         let rec aux lv i =
           match i with
           | SingleInit e -> usage <- add_alias usage lv e
           | CompoundInit (_, l) ->
             List.iter (fun (o,i) -> aux (Cil.addOffsetLval o lv) i) l
         in
         aux (Cil.var v) i
       | Local_init(v, ConsInit(f,_,Plain_func), _) ->
         usage <- add_call usage (Some (Cil.var v)) (Var f)
       | Local_init(_, ConsInit _,_) -> () (* not a real assignment. *)
       | Asm _ | Skip _ | Code_annot _ -> ()
      );
      Cil.DoChildren

    method! vstmt_aux s =
      (match s.skind with
       | If (e, _, _, _)
       | Switch (e, _, _, _) ->
         usage <- add_direct_comparison usage e
       | _ -> ()
      );
      Cil.DoChildren

    method! vexpr e =
      usage <- add_compare usage e;
      Cil.DoChildren

    method result () =
      summarize_by_lv usage

    method! vtype _ = Cil.SkipChildren
    method! vspec _ = Cil.SkipChildren
    method! vcode_annot _ = Cil.SkipChildren
  end

  (* For functions returning pointers, add a split on NULL/non-NULL *)
  let add_null_pointers_split (ru: return_split): return_split =
    let null_set = Z.Set.singleton Z.zero in
    let aux kf acc =
      if Ast_types.is_ptr (Kernel_function.get_return_type kf) then
        add_split kf null_set acc
      else acc
    in
    Globals.Functions.fold aux ru


  let compute file =
    let vis = new visitorVarUsage in
    Visitor.visitFramacFileFunctions (vis:> Visitor.frama_c_visitor) file;
    let split_compared = vis#result () in
    let split_null_pointers = add_null_pointers_split split_compared in
    split_null_pointers

end

module AutoStrategy = State_builder.Option_ref
    (ReturnUsage.RUDatatype)
    (struct
      let name = "Value.Split_return.Autostrategy"
      let dependencies = [Ast.self]
    end)
let () = Ast.add_monotonic_state AutoStrategy.self

let compute_auto () =
  if AutoStrategy.is_computed () then
    AutoStrategy.get ()
  else begin
    let s = ReturnUsage.compute (Ast.get ()) in
    AutoStrategy.set s;
    AutoStrategy.mark_as_computed ();
    s
  end

(* Auto-strategy for one given function *)
let find_auto_strategy kf =
  try
    let s = Kernel_function.Map.find kf (compute_auto ()) in
    Split_strategy.SplitEqList (Z.Set.elements s)
  with Not_found -> Split_strategy.NoSplit

module KfStrategy = Kernel_function.Make_Table(Split_strategy)
    (struct
      let size = 17
      let dependencies = [Parameters.SplitReturnFunction.self;
                          Parameters.SplitReturn.self;
                          AutoStrategy.self]
      let name = "Value.Split_return.Kfstrategy"
    end)

(* Invariant: this function never returns Split_strategy.SplitAuto *)
let kf_strategy =
  KfStrategy.memo
    (fun kf ->
       try (* User strategies take precedence *)
         match Parameters.SplitReturnFunction.find kf with
         | Split_strategy.SplitAuto -> find_auto_strategy kf
         | s -> s
       with Not_found ->
       match Parameters.SplitReturn.get () with
       | Split_strategy.SplitAuto -> find_auto_strategy kf
       | s -> s
    )

let pretty_strategies fmt =
  Format.fprintf fmt "@[<v>";
  let open Split_strategy in
  let pp_list = Pretty_utils.pp_list ~sep:",@ " Z.pretty in
  let pp_one user_auto pp = function
    | NoSplit -> ()
    | FullSplit ->
      Format.fprintf fmt "@[\\full_split(%t)@]@ " pp
    | SplitEqList l ->
      Format.fprintf fmt "@[\\return(%t) == %a (%s)@]@ " pp pp_list l user_auto
    | SplitAuto -> assert false (* should have been replaced by SplitEqList *)
  in
  let pp_kf kf fmt = Kernel_function.pretty fmt kf in
  let pp_user (kf, strategy) =
    match strategy with
    | SplitAuto -> pp_one "auto" (pp_kf kf) (kf_strategy kf)
    | s -> pp_one "user" (pp_kf kf) s
  in
  Parameters.SplitReturnFunction.iter pp_user;
  if not (Parameters.SplitReturnFunction.is_empty ()) &&
     match Parameters.SplitReturn.get () with
     | Split_strategy.NoSplit | Split_strategy.SplitAuto -> false
     | _ -> true
  then Format.fprintf fmt "@[other functions:@]@ ";
  begin match Parameters.SplitReturn.get () with
    | SplitAuto ->
      let pp_auto kf s =
        if not (Parameters.SplitReturnFunction.mem kf) then
          let s = SplitEqList (Z.Set.elements s) in
          pp_one "auto" (pp_kf kf) s
      in
      let auto = compute_auto () in
      Kernel_function.Map.iter pp_auto auto;
    | s -> pp_one "auto" (fun fmt -> Format.pp_print_string fmt "@all") s
  end;
  Format.fprintf fmt "@]"

let pretty_strategies () =
  if not (Parameters.SplitReturnFunction.is_empty ()) ||
     (Parameters.SplitReturn.get () != Split_strategy.NoSplit)
  then
    let dkey = Self.dkey_split_return in
    Self.feedback ~dkey
      "@[<v 2>Splitting return states on:@;%t@]" pretty_strategies
