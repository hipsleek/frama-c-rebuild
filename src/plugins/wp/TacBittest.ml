(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang

(* -------------------------------------------------------------------------- *)
(* --- Helpers                                                            --- *)
(* -------------------------------------------------------------------------- *)

let positive e = F.p_leq F.e_zero e (* 0 <= n *)
let power k = F.e_bigint (Z.two_power_of_int k)

let lookup_int e =
  let open Qed.Logic in
  match F.repr e with
  | Kint z -> (try Some (Z.to_int z) with Z.Overflow -> None)
  | _ -> None

let rec lookup_bittest e =
  match F.repr e with
  | Not e -> lookup_bittest e
  | Fun(f,[n;ek]) when List.memq f Cint.f_bits ->
    begin
      match lookup_int ek with
      | Some k when 0 <= k && k < 128 -> Some (n,k)
      | _ -> None
    end
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Bit-Test Range                                                     --- *)
(* -------------------------------------------------------------------------- *)

class bittestrange =
  object
    inherit Tactical.make
        ~id:"Wp.bittestrange"
        ~title:"Bit-Test Range"
        ~descr:"Compute bounds with respect to bits."
        ~params:[]

    method select _feedback selection =
      let e = Tactical.selected selection in
      match lookup_bittest e with
      | Some (n,k) ->
        let bit = Cint.bit_test n k in
        let bit_set = F.p_bool bit in
        let bit_clear = F.p_not bit_set in
        let pos = positive n in
        let pk = power k in
        let pk1 = power (succ k) in
        let g_inf = F.p_hyps [pos] (F.p_leq pk n) in
        let g_sup = F.p_hyps [pos;F.p_lt n pk1] (F.p_lt n pk) in
        let name_inf = Printf.sprintf "Bit #%d (inf)" k in
        let name_sup = Printf.sprintf "Bit #%d (sup)" k in
        let at = Tactical.at selection in
        Tactical.Applicable (Tactical.insert ?at [
            name_inf , F.p_and bit_set g_inf ;
            name_sup , F.p_and bit_clear g_sup ;
          ])
      | None -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new bittestrange)
let strategy = Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
(* --- Auto Bitrange                                                      --- *)
(* -------------------------------------------------------------------------- *)

let rec lookup push step e =
  match F.repr e with
  | And es -> List.iter (lookup push step) es
  | Or es -> List.iter (lookup push step) es
  | Imply (hs,p) -> List.iter (lookup push step) (p::hs)
  | _ ->
    begin
      match lookup_bittest e with
      | None -> ()
      | Some _ ->
        push @@ strategy ~priority:0.3 (Tactical.Inside(step,e))
    end

class autobittestrange : Strategy.heuristic =
  object

    method id = "wp:bittestrange"
    method title = "Auto Bit-Test Range"
    method descr = "Apply bitwise tactics on bit-tests expressions."

    method search push (seq : Conditions.sequent) =
      Conditions.iter
        (fun step ->
           let p = Conditions.head step |> F.e_prop in
           lookup push (Tactical.Step step) p
        ) (fst seq) ;
      let p = snd seq in
      lookup push (Tactical.Goal p) (F.e_prop p)

  end

let () = Strategy.register (new autobittestrange)
