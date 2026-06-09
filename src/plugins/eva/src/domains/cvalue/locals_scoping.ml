(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type clobbered_set = {
  mutable clob: Base.SetLattice.t
}

let packed_descr =
  let open Structural_descr in
  pack (t_record [| Base.SetLattice.packed_descr |])

let bottom () = { clob = Base.SetLattice.bottom }
let top () = { clob = Base.SetLattice.top }

let remember_bases_with_locals clob new_clob =
  clob.clob <- Base.SetLattice.join new_clob clob.clob

let remember_if_locals_in_value clob (left_loc : Locations.t) v =
  if Cvalue.V.contains_addresses_of_any_locals v then
    let new_clob = Addresses.Bits.get_bases left_loc.addr in
    remember_bases_with_locals clob new_clob

let offsetmap_contains_local offm =
  try
    Cvalue.V_Offsetmap.iter_on_values
      (fun v ->
         if Cvalue.V.contains_addresses_of_any_locals
             (Cvalue.V_Or_Uninitialized.get_v v)
         then raise Exit
      ) offm;
    false
  with Exit -> true


let warn_locals_escape is_block fundec k locals =
  let pretty_base = Base.pretty in
  let pretty_block fmt = Pretty_utils.pp_cond is_block fmt "a block of " in
  let sv = fundec.svar in
  Self.warning
    ~wkey:Self.wkey_locals_escaping
    ~current:true ~once:true
    "locals %a escaping the scope of %t%a through %a"
    Base.Hptset.pretty locals pretty_block Printer.pp_varinfo sv pretty_base k


(* Rebuild [offsm] by applying [f] to the bindings that verify [test].
   Also call [warn] in this case. *)
let rebuild_offsetmap f warn offsm =
  Cvalue.V_Offsetmap.fold
    (fun (_,_ as itv) (v, m, r) acc ->
       let changed, v' = f v in
       if changed then begin
         warn ~itv ~v:(Cvalue.V_Or_Uninitialized.get_v v);
         Cvalue.V_Offsetmap.add itv (v', m, r) acc
       end else
         acc)
    offsm
    offsm

(* make escaping the ranges of [offsetmap] that verify [test]. Honor [exact],
   and warn using [warn] on those ranges. *)
let make_escaping_offsetmap test warn ~exact offsetmap =
  let make_escaping v =
    Cvalue.V_Or_Uninitialized.unspecify_escaping_locals ~exact test v
  in
  rebuild_offsetmap make_escaping warn offsetmap

let make_escaping ~exact ~escaping ~on_escaping ~within state =
  (* Clean [offsm], and bind it to [base] if it is modified. *)
  let aux base offsm state =
    let test b = Base.Hptset.mem b escaping in
    let on_escaping = on_escaping ~b:base in
    let offsm' = make_escaping_offsetmap test on_escaping ~exact offsm in
    if Cvalue.V_Offsetmap.equal offsm' offsm then state
    else Cvalue.Model.add_base base offsm' state
  in
  (* Clean the offsetmap bound to [base] in [state] *)
  let aux' base state =
    try
      match Cvalue.Model.find_base base state with
      | `Top | `Bottom -> state
      | `Value offsm -> aux base offsm state
    with Not_found -> state
  in
  try (* Iterate on all the bases that might contain a variable to clean *)
    Base.SetLattice.fold aux' within (aux' Base.null state)
  with Abstract_interp.Error_Top ->
  (* [bases] is too imprecise. Iterate on the entire memory state instead,
     which is much slower *)
  match state with
  | Cvalue.Model.Top | Cvalue.Model.Bottom -> state
  | Cvalue.Model.Map m -> Cvalue.Model.fold aux m state

let make_escaping_fundec fundec clob vars state =
  let filter acc v =
    if v.vtemp || not v.vreferenced
    then acc else Base.Hptset.add (Base.of_varinfo v) acc
  in
  let vars = List.fold_left filter Base.Hptset.empty vars in
  if Base.Hptset.is_empty vars
  then state
  else
    (* Detect whether we are deallocating an inner block of the function,
       or a formal/a toplevel local. This is used for the warning message. *)
    let is_inner_block =
      let b = Base.Hptset.choose vars in
      not (Base.is_formal b fundec || Base.is_block_local b fundec.sbody)
    in
    let escaping = vars in
    let on_escaping ~b ~itv:_ ~v =
      let bases = match Cvalue.V.get_bases v with
        | Base.SetLattice.Top -> escaping
        | Base.SetLattice.Set bases -> Base.Hptset.inter bases escaping
      in
      warn_locals_escape is_inner_block fundec b bases
    in
    make_escaping ~exact:true ~escaping ~on_escaping ~within:clob.clob state

let substitute substitution clob state =
  (* Apply the [substitution] to [offsm]. If it is modified, bind the new
     offsetmap to [base] in [state], and add the [base] to [set]. *)
  let replace_offsm base offsm state =
    let f v = snd (Cvalue.V_Or_Uninitialized.replace_base substitution v) in
    let offsm' = Cvalue.V_Offsetmap.map_on_values f offsm in
    if Cvalue.V_Offsetmap.equal offsm' offsm
    then state
    else Cvalue.Model.add_base base offsm' state
  in
  (* Apply the substitution to the offsetmap bound to [base] in [state] *)
  let replace_base base acc =
    match Cvalue.Model.find_base base state with
    | `Value offsm -> replace_offsm base offsm acc
    | `Top | `Bottom -> acc
    | exception Not_found -> acc
  in
  (* Iterate on all the bases that might contain a variable to substitute *)
  try Base.SetLattice.fold replace_base clob.clob (replace_base Base.null state)
  with Abstract_interp.Error_Top ->
  (* [clob] is too imprecise. Iterate on the entire memory state instead,
     which is much slower. *)
  match state with
  | Cvalue.Model.Top | Cvalue.Model.Bottom -> state
  | Cvalue.Model.Map map -> Cvalue.Model.fold replace_offsm map state
