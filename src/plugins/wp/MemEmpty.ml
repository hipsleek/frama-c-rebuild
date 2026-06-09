(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Empty Memory Model                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F
open Memory

module Logic = Qed.Logic

let datatype = "MemEmpty"
let configure () =
  begin
    let orig_pointer = Context.push Lang.pointer Logic.Int in
    let orig_null    = Context.push Cvalues.null (p_equal e_zero) in
    let rollback () =
      Context.pop Lang.pointer orig_pointer ;
      Context.pop Cvalues.null orig_null ;
    in
    rollback
  end
let no_binder = { bind = fun _ f v -> f v }
let configure_ia _ = no_binder

let hypotheses p = p

type loc = unit
type segment = loc rloc
let lookup _ _ = raise Not_found
let updates _ _ = Bag.empty

let pretty _fmt () = ()
let vars _l = Vars.empty
let occurs _x _l = false

let null = ()
let cvar _x = ()
let pointer_loc _t = ()
let pointer_val () = e_zero

let field _l _f = ()
let shift _l _obj _k = ()
let base_addr _l = ()
let base_offset _l = e_zero
let block_length _s _obj _l = e_zero

let cast _ _l = ()
let loc_of_int _ _ = ()
let int_of_loc _ () = e_zero

let domain _obj _l = Sigma.Domain.empty
let is_well_formed _s = p_true

let source = "Empty Model"

let load _sigma _obj () = Warning.error ~source "Can not load value in Empty model"
let load_init _sigma _obj () =  Warning.error ~source "Can not load init in Empty model"
let copied _s _obj () () = []
let copied_init _s _obj () () = []
let stored _s _obj () _ = []
let stored_init _s _obj () _ = []
let assigned _s _obj _sloc = []

let no_pointer () = Warning.error ~source "Can not compare pointers in Empty model"

let is_null _ = no_pointer ()
let loc_eq _ _ = no_pointer ()
let loc_lt _ _ = no_pointer ()
let loc_leq _ _ = no_pointer ()
let loc_neq _ _ = no_pointer ()
let loc_diff _ _ _ = no_pointer ()

let frame _sigma = []
let alloc sigma _xs = sigma
let scope _seq _s _xs = []
let valid _sigma _acs _l = Warning.error ~source "No validity"
let invalid _sigma _l = Warning.error ~source "No validity"
let initialized _sigma _l = Warning.error ~source "MemEmpty: No initialized"
let global _sigma _p = p_true

let included _s1 _s2 = no_pointer ()
let separated _s1 _s2 = no_pointer ()
