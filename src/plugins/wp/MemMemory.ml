(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Memory Model                                                       --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

module L = Qed.Logic

let library = "memory"

let ty_fst_arg = function
  | Some l :: _ -> l
  | _ -> raise Not_found


let f_eqmem = Lang.extern_fp ~library "eqmem"
let f_memcpy = Lang.extern_f ~library ~typecheck:ty_fst_arg "memcpy"
let p_framed = Lang.extern_fp ~coloring:true ~library "framed" (* ptr-memory -> prop *)
let p_sconst = Lang.extern_fp ~coloring:true ~library "sconst" (* int-memory -> prop *)
let p_scinit = Lang.extern_fp ~coloring:true ~library "scinit" (* init-memory -> prop *)

(* -------------------------------------------------------------------------- *)
(* --- Utilities                                                          --- *)
(* -------------------------------------------------------------------------- *)

let t_malloc = L.Array(L.Int,L.Int)
let t_mem t = L.Array(MemAddr.t_addr,t)
let t_init = L.Array(MemAddr.t_addr,L.Bool)

let sconst memory = p_call p_sconst [ memory ]
let scinit memory = p_call p_scinit [ memory ]
let framed memory = p_call p_framed [ memory ]

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for 'eqmem'                                             --- *)
(* -------------------------------------------------------------------------- *)

let r_eqmem = function
  | [_;_;_;n] when n = e_zero -> e_true
  | [m0;m1;p;n] when n = e_one -> e_eq (e_get m0 p) (e_get m1 p)
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for 'memcpy'                                            --- *)
(* -------------------------------------------------------------------------- *)

(* memcpy(m,q,m0,q0,n)[p] =
   - m[p] WHEN separated (p,1,q,n)
   - m0[q0 ++ p.offset - q.offset] WHEN not separated (p,1,q,n)
*)
let r_get_memcpy es ks =
  match es, ks with
  | [m;q;m0;q0;n],[p] ->
    begin
      match MemAddr.is_separated [p;e_one;q;n] with
      | L.Yes -> F.e_get m p
      | L.No ->
        if p == q then
          F.e_get m0 q0
        else
        if q == q0 then
          F.e_get m0 p
        else
          let i = MemAddr.offset p in
          let j = MemAddr.offset q in
          let q' = MemAddr.shift q0 (F.e_sub i j) in
          F.e_get m0 q'
      | _ -> raise Not_found
    end
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Simplifiers Registration                                           --- *)
(* -------------------------------------------------------------------------- *)

let () = Context.register
    begin fun () ->
      F.set_builtin f_eqmem r_eqmem ;
      F.set_builtin_get f_memcpy r_get_memcpy ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Unsupported Unions                                                 --- *)
(* -------------------------------------------------------------------------- *)

let wkey = Wp_parameters.register_warn_category "union"

let unsupported_union ~model (fd : Cil_types.fieldinfo) =
  if not fd.fcomp.cstruct then
    Wp_parameters.warning ~once:true ~wkey
      "Accessing union fields with %s model might be unsound.@\n\
       Please refer to WP manual." model

(* -------------------------------------------------------------------------- *)
