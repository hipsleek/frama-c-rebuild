(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang
open Tactical

class overflow =
  object
    inherit Tactical.make
        ~id:"Wp.overflow"
        ~title:"Overflow"
        ~descr:"Split integer overflow into in and out of range."
        ~params:[]

    method select _feedback selection =
      let e = Tactical.selected selection in
      let open Qed.Logic in
      match F.repr e with
      | Fun(f,[v]) ->
        let open Lang.F in
        let open Lang.N in
        let min, max = Ctypes.bounds @@ Cint.to_cint f in
        let min, max = e_zint min, e_zint max in

        let lower = v < min and upper = max < v in
        let in_range = not (lower ||: upper) in

        let length = (max - min) + e_one in
        let overflow = min + ((v - min) mod length) in

        let replace_with v = fun u -> if u == e then v else raise Not_found in

        Applicable(fun (hs,g) -> [
              "In-Range",
              Conditions.subst (replace_with v) (hs , in_range ==> g) ;
              "Lower",
              Conditions.subst (replace_with overflow) (hs , lower ==> g) ;
              "Upper",
              Conditions.subst (replace_with overflow) (hs , upper ==> g)
            ])
      | _ -> Not_applicable

  end

let overflow = Tactical.export (new overflow)
