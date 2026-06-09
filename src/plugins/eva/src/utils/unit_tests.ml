(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Lattice_bounds

(* If true, prints each operation performed for the tests. Otherwise, only
   prints wrong operations. *)
let print = false

let report bug format =
  if print || bug
  then Self.result ("%s" ^^ format) (if bug then "BUG " else "")
  else Format.ifprintf Format.std_formatter format


(* Tests the semantics of sign values, by comparison with the Ival semantics. *)
module Sign = struct

  module Sign = struct
    include Sign_value
    let zero = zero
    let pos = inject_int Cil_const.uintType Z.one
    let neg = inject_int Cil_const.uintType Z.minus_one
    let pos_or_zero = join zero pos
    let neg_or_zero = join zero neg
    let pos_neg = join pos neg
  end

  module Ival = struct
    include Ival
    let positive = inject_range (Some Z.one) None
    let negative = inject_range None (Some Z.minus_one)
    let pos irange =
      let max = Eval_typ.range_upper_bound irange in
      inject_range (Some Z.one) (Some max)
    let neg irange =
      let min = Eval_typ.range_lower_bound irange in
      inject_range (Some min) (Some Z.minus_one)
  end

  module Cval = Main_values.CVal

  (* List of pairs (ival, sign) such that γ(ival) ⊆ γ(sign) and all values in
     γ(sign) are in the range of [ikind]. *)
  let test_values ikind =
    let irange = Eval_typ.ik_range ikind in
    let list =
      [ Ival.zero, Sign.zero;
        Ival.pos irange, Sign.pos;
        Ival.zero, Sign.pos_or_zero;
        Ival.pos irange, Sign.pos_or_zero; ]
    in
    if not irange.Eval_typ.i_signed
    then list
    else
      list @
      [ Ival.neg irange, Sign.neg;
        Ival.zero, Sign.neg_or_zero;
        Ival.neg irange, Sign.neg_or_zero;
        Ival.pos irange, Sign.pos_neg;
        Ival.neg irange, Sign.pos_neg; ]

  let make_cvalue values =
    List.map (fun (ival, sign) -> Cvalue.V.inject_ival ival, sign) values

  (* Checks whether γ(ival) ⊆ γ(sign). *)
  let is_included cvalue sign =
    try
      let ival = Cvalue.V.project_ival cvalue in
      (not (Ival.contains_zero ival) || sign.Sign.zero) &&
      (Ival.(is_bottom (narrow ival positive)) || sign.Sign.pos) &&
      (Ival.(is_bottom (narrow ival negative)) || sign.Sign.neg)
    with Cvalue.V.Not_based_on_null -> Sign.(equal sign top)

  let test_unop unop typ values =
    let test (cval, sign) =
      let context = Abstract_value.{ from_domains = Unit_context.top } in
      let cval_res = Cval.forward_unop context typ unop cval in
      let sign_res = Sign.forward_unop context typ unop sign in
      let bug = not (Bottom.is_included is_included cval_res sign_res) in
      report bug "%a %a = %a  while  %a %a = %a"
        Eva_ast.pp_unop unop Cval.pretty cval (Bottom.pretty Cval.pretty) cval_res
        Eva_ast.pp_unop unop Sign.pretty sign (Bottom.pretty Sign.pretty) sign_res
    in
    List.iter test values

  let test_binop binop typ values =
    let test (cval1, sign1) (cval2, sign2) =
      let context = Abstract_value.{ from_domains = Unit_context.top } in
      let cval_res = Cval.forward_binop context typ binop cval1 cval2 in
      let sign_res = Sign.forward_binop context typ binop sign1 sign2 in
      let bug = not (Bottom.is_included is_included cval_res sign_res) in
      report bug "%a %a %a = %a  while  %a %a %a = %a"
        Cval.pretty cval1 Eva_ast.pp_binop binop Cval.pretty cval2
        (Bottom.pretty Cval.pretty) cval_res
        Sign.pretty sign1 Eva_ast.pp_binop binop Sign.pretty sign2
        (Bottom.pretty Sign.pretty) sign_res
    in
    List.iter (fun x -> List.iter (test x) values) values

  let test_typ ikind =
    let typ = Cil_const.mk_tint ikind
    and values = make_cvalue (test_values ikind) in
    let apply f op = f op typ values in
    List.iter (apply test_unop) [Neg; BNot; LNot];
    List.iter (apply test_binop)
      [PlusA; MinusA; Mult; Div; BAnd; BOr; BXor; LAnd; LOr]

  let test () = List.iter test_typ [IInt; IUInt]
end

(** Runs all tests. *)
let run () =
  Self.result "Runs unit tests: %s."
    (if print
     then "all operations will be printed"
     else  "only faulty operations will be printed");
  Sign.test ()
