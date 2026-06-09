(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

exception NoAssigns

(*****************************************************************************)
(****************** Generation of function assigns ***************************)
(*****************************************************************************)

(* If an argument contains a pointer type, then it is undecidable which assigns
   clause should be generated, so skip the assigns generation in this case *)
let rec is_ptr_free typ = match Ast_types.unroll_node typ with
  | TVoid
  | TInt _
  | TFloat _ -> true
  | TPtr _ -> false
  | TArray (ty, _) -> is_ptr_free ty
  | TFun (_, _, _) ->
    (* a function cannot be an argument of a function *)
    assert false
  | TNamed _ ->
    (* The named types are unfolded with [Cil.unrolltype] *)
    assert false
  | TEnum _
  | TBuiltin_va_list -> true
  | TComp cinfo ->
    match cinfo.cfields with
    | None -> raise NoAssigns
    | Some fields ->
      List.for_all
        (fun fi -> fi.fbitfield <> None || is_ptr_free fi.ftype)
        fields

(* For a GMP argument of a function, we generate an assigns from its address,
   since these are the only semantically valid operations on integers.
   For the argument [__e_acsl_mpz_struct * x], this function generates the
   expression [*(( __e_acsl_mpz_struct * )x)] *)
let deref_gmp_arg ~loc var =
  Smart_exp.lval ~loc
    (Mem (Cil.mkAddrOrStartOf ~loc (Var var , NoOffset)), NoOffset)

let rec get_assigns_from ~loc env lprofile lv =
  match lprofile with
  | [] -> []
  | lvar :: lvars ->
    let var = Env.Logic_binding.get env lvar  in
    if is_ptr_free var.vtype then
      Smart_exp.lval ~loc (Cil.var var) :: get_assigns_from ~loc env lvars lv
      (* If the argument contains a pointer, but is an integer, then it is
         necessarily a GMP type *)
    else if lvar.lv_type = Linteger || Gmp_types.is_t var.vtype then
      (deref_gmp_arg ~loc var) :: (get_assigns_from ~loc env lvars lv)
    else begin
      Options.warning ~current:true "no assigns clause generated for \
                                     function %a because pointers as arguments \
                                     is not yet supported"
        Printer.pp_logic_var
        (Logic_normalizer.Logic_infos.origin_of_lv lv);
      raise NoAssigns
    end

(* Special case when the function takes an extra argument as its result:
   For the argument [__e_acsl_mpz_t *__retres_arg], this function generates the
   expression [( *__retres_arg )[0]]. For a struct argument, this function just
   generates the variable corresponding to the argument. *)
let get_assigned_var ~loc ~is_gmp vi =
  let var = if is_gmp then Smart_exp.mem ~loc vi else Cil.evar ~loc vi
  in Logic_utils.expr_to_term var
