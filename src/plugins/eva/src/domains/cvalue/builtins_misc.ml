(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds.Bottom.Operators

let frama_C_assert state actuals =
  let do_bottom () =
    Self.warning ~current:true ~once:true "Frama_C_assert: false";
    Cvalue.Model.bottom
  in
  match actuals with
  | [arg_exp, arg] ->
    let state =
      if Cvalue.V.is_zero arg
      then do_bottom ()
      else if Cvalue.V.contains_zero arg
      then begin
        let state =
          let* valuation = fst (Cvalue_queries.reduce state arg_exp true) in
          let valuation = Cvalue_queries.to_domain_valuation valuation in
          Cvalue_transfer.update valuation state
        in
        match state with
        | `Value state ->
          Self.warning ~current:true ~once:true "Frama_C_assert: unknown";
          state
        | `Bottom -> do_bottom ()
      end
      else begin
        Self.warning ~current:true ~once:true "Frama_C_assert: true";
        state
      end
    in
    Builtins.States [ state ]
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () = Builtins.register_builtin "Frama_C_assert" frama_C_assert
