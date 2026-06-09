(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Symbols registration                                               --- *)
(* -------------------------------------------------------------------------- *)

let library = "memaddr"

(* Warning: DO NOT register map types using this constructor: it hides types
   needed by ProverWhy3 for typing terms of the form x[i].
*)

let t_addr = Qed.Logic.Data(Lang.datatype ~library "addr",[])
let t_table = Qed.Logic.Data(Lang.datatype ~library "table",[])
let t_malloc = Qed.Logic.Array(Qed.Logic.Int, Qed.Logic.Int)

let f_base   = Lang.extern_f ~library ~result:Qed.Logic.Int
    ~link:(Qed.Engine.F_subst ("base", "%1.base")) "base"

let f_offset = Lang.extern_f ~library ~result:Qed.Logic.Int
    ~link:(Qed.Engine.F_subst ("offset", "%1.offset")) "offset"

let f_shift  = Lang.extern_f ~library ~result:t_addr "shift"
let f_global = Lang.extern_f ~library ~result:t_addr ~category:Qed.Logic.Injection "global"
let f_null   = Lang.extern_f ~library ~result:t_addr "null"

let p_addr_lt = Lang.extern_p ~library ~bool:"addr_lt_bool" ~prop:"addr_lt" ()
let p_addr_le = Lang.extern_p ~library ~bool:"addr_le_bool" ~prop:"addr_le" ()

let f_addr_of_int = Lang.extern_f
    ~library ~result:t_addr "addr_of_int"

let f_int_of_addr = Lang.extern_f
    ~library ~result:Qed.Logic.Int "int_of_addr"

let p_statically_allocated = Lang.extern_fp ~library "statically_allocated"

let f_table_of_base = Lang.extern_f ~library
    ~category:Qed.Logic.Function ~result:t_table "table_of_base"

let f_table_to_offset = Lang.extern_f ~library
    ~category:Qed.Logic.Injection ~result:Qed.Logic.Int "table_to_offset"

let p_valid_rd = Lang.extern_fp ~library "valid_rd"
let p_valid_rw = Lang.extern_fp ~library "valid_rw"
let p_valid_obj = Lang.extern_fp ~library "valid_obj"
let p_invalid = Lang.extern_fp ~library "invalid"
let p_separated = Lang.extern_fp ~library "separated"
let p_included = Lang.extern_fp ~library "included"

(* base -> region *)
let f_region = Lang.extern_f ~coloring:true ~library ~result:Qed.Logic.Int "region"
(* always initialized bases *)
let p_binit =  Lang.extern_fp ~coloring:true ~library "binit"
(* allocation-table -> prop *)
let p_linked = Lang.extern_fp ~coloring:true ~library "linked"

(* -------------------------------------------------------------------------- *)
(* --- API                                                                --- *)
(* -------------------------------------------------------------------------- *)

(* Basic constructors and getters *)

let base addr = e_fun f_base [ addr ]
let offset addr = e_fun f_offset [ addr ]
let null = constant (e_fun f_null [])
let global base = e_fun f_global [ base ]
let shift addr offset = e_fun f_shift [ addr ; offset ]
let mk_addr base offset = shift (global base) offset

(* Comparisons *)

let addr_lt addr1 addr2 = p_call p_addr_lt [ addr1 ; addr2 ]
let addr_le addr1 addr2 = p_call p_addr_le [ addr1 ; addr2 ]

(* Regions *)

let region base = e_fun f_region [ base ]
let binit base = p_call p_binit [ base ]
let linked memory = p_call p_linked [ memory ]

(* Validity *)

let valid_rd alloc addr size = p_call p_valid_rd [ alloc ; addr ; size ]
let valid_rw alloc addr size = p_call p_valid_rw [ alloc ; addr ; size ]
let valid_obj alloc addr = p_call p_valid_obj [ alloc ; addr ]
let invalid alloc addr size = p_call p_invalid [ alloc ; addr ; size ]

(* Physical addresses *)

let addr_of_int i = e_fun f_addr_of_int [ i ]
let int_of_addr addr = e_fun f_int_of_addr [ addr ]

let static_alloc addr = p_call p_statically_allocated [ addr ]
(* This last function is not exposed, it does not have a particular meaning in
   ACSL, and is used only for int/addr conversions.
*)

let in_uintptr_range addr =
  p_hyps [ static_alloc @@ base addr ] @@
  Cint.range (Ctypes.c_ptr ()) @@ int_of_addr addr

(* Table of offsets *)

let base_offset base offset =
  let offset_index = e_fun f_table_of_base [base] in
  e_fun f_table_to_offset [offset_index ; offset]
(** Returns the offset in {i bytes} from the {i logic} offset
    (which is a memory cell index, actually) *)


(* -------------------------------------------------------------------------- *)
(* --- Qed Simplifiers                                                    --- *)
(* -------------------------------------------------------------------------- *)

(*
    Pointer arithmetic for structure access and array access could be
    defined directly using the record [{ base = p.base; offset = p.offset
    + c*i + c' }]. However that gives very bad triggers for the memory
    model axiomatization, so `shift p (c*i+c')` was used instead. It is
    not sufficient for user axiomatisation because memory access in
    axioms require trigger with arithmetic operators which is badly
    handled by provers. So for each c and c', ie for each kind of
    structure access and array access a specific function is used
    `shift_xxx`.

    Moreover no simplification of `shift_xxx` is done for keeping the
    same terms in axioms and the goal. `base` and `offset` function
    simplify all the `shift_xxx` because it seems they don't appear
    often in axioms and they are useful for simplifying `separated`,
    `assigns` and pointer comparisons in goals.

    To sum up memory access should match, but not `\base`, `\offset`,
    `\separated`, ...
*)

type addr_builtin = {
  base: term list -> term ;
  offset: term list -> term ;
}

module ADDR_BUILTIN = WpContext.Static
    (struct
      type key = Lang.lfun
      type data = addr_builtin
      let name = "MemMemory.ADDR_BUILTIN"
      include Lang.Fun
    end)

let phi_base l =
  match repr l with
  | Fun(f,[p;_]) when f==f_shift -> base p
  | Fun(f,[b]) when f==f_global -> b
  | Fun(f,[]) when f==f_null -> e_zero
  | Fun(f,args) -> (ADDR_BUILTIN.find f).base args
  | _ -> raise Not_found

let phi_offset l = match repr l with
  | Fun(f,[p;k]) when f==f_shift -> e_add (offset p) k
  | Fun(f,_) when f==f_global || f==f_null -> e_zero
  | Fun(f,args) -> (ADDR_BUILTIN.find f).offset args
  | _ -> raise Not_found

let phi_shift f p i =
  match repr p with
  | Fun(g,[q;j]) when f == g -> e_fun f [q;e_add i j]
  | _ -> raise Not_found

let eq_shift a b =
  let p = base a in
  let q = base b in
  let i = offset a in
  let j = offset b in
  if i==j then p_equal p q else
    match is_equal p q with
    | No -> p_false
    | Yes -> p_equal i j
    | Maybe -> raise Not_found

let eq_shift_gen phi a b =
  try phi a b with Not_found -> eq_shift a b

let nop _ = raise Not_found

let register ?(base=nop) ?(offset=nop) ?equal ?(linear=false) lfun =
  begin
    if base != nop || offset != nop then
      ADDR_BUILTIN.define lfun { base ; offset } ;
    if linear then
      set_builtin_2 lfun (phi_shift lfun) ;
    let phi_equal = match equal with
      | None -> eq_shift
      | Some phi -> eq_shift_gen phi
    in
    set_builtin_eqp lfun phi_equal ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for 'separated'                                         --- *)
(* -------------------------------------------------------------------------- *)

let r_separated = function
  | [p;a;q;b] ->
    if a == e_one && b == e_one then e_neq p q
    else
      begin
        let a_negative = e_leq a e_zero in
        let b_negative = e_leq b e_zero in
        if a_negative == e_true || b_negative == e_true then e_true else
          let bp = base p in
          let bq = base q in
          match is_true (e_eq bp bq) with
          | No -> e_true (* Have S *)
          | Yes when (a_negative == e_false && b_negative == e_false) ->
            (* Reduced to S *)
            let p_ofs = offset p in
            let q_ofs = offset q in
            let p_ofs' = e_add p_ofs a in
            let q_ofs' = e_add q_ofs b in
            e_or [ e_leq q_ofs' p_ofs ;
                   e_leq p_ofs' q_ofs ]
          | _ -> raise Not_found
      end
  | _ -> raise Not_found

let is_separated args = is_true (r_separated args)

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for 'included'                                          --- *)
(* -------------------------------------------------------------------------- *)

(* See: tests/why3/test_memory.why

   logic a : int
   logic b : int

   predicate R = p.base = q.base
              /\ (q.offset <= p.offset)
              /\ (p.offset + a <= q.offset + b)

   predicate included = 0 < a -> ( 0 <= b and R )
   predicate a_empty = a <= 0
   predicate b_negative = b < 0

   lemma SAME_P: p=q -> (R <-> a<=b)
   lemma SAME_A: a=b -> (R <-> p=q)

   goal INC_P:  p=q -> (included <-> ( 0 < a -> a <= b )) (by SAME_P)
   goal INC_A:  a=b -> 0 < a -> (included <-> R) (by SAME_A)
   goal INC_1:  a_empty -> (included <-> true)
   goal INC_2:  b_negative -> (included <-> a_empty)
   goal INC_3:  not R -> (included <-> a_empty)
   goal INC_4:  not a_empty -> not b_negative -> (included <-> R)
*)

let r_included = function
  | [p;a;q;b] ->
    if e_eq p q == e_true
    then e_imply [e_lt e_zero a] (e_leq a b) (* INC_P *)
    else
    if (e_eq a b == e_true) && (e_lt e_zero a == e_true)
    then e_eq p q (* INC_A *)
    else
      begin
        let a_empty = e_leq a e_zero in
        let b_negative = e_lt b e_zero in
        if a_empty == e_true then e_true (* INC_1 *) else
        if b_negative == e_true then a_empty (* INC_2 *) else
          let bp = base p in
          let bq = base q in
          match is_true (e_eq bp bq) with
          | No -> a_empty (* INC_3 *)
          | Yes when (a_empty == e_false && b_negative == e_false) ->
            (* INC_4 *)
            let p_ofs = offset p in
            let q_ofs = offset q in
            if a == b then e_eq p_ofs q_ofs
            else
              let p_ofs' = e_add p_ofs a in
              let q_ofs' = e_add q_ofs b in
              e_and [ e_leq q_ofs p_ofs ; e_leq p_ofs' q_ofs' ]
          | _ -> raise Not_found
      end
  | _ -> raise Not_found

let is_included args = is_true (r_included args)

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for int/addr conversion                                 --- *)
(* -------------------------------------------------------------------------- *)

let phi_int_of_addr p =
  if p == null then e_zero else
    match repr p with
    | Fun(f,[a]) when f == f_addr_of_int -> a
    | _ -> raise Not_found

let phi_addr_of_int p =
  if p == e_zero then null else raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for (in)validity                                        --- *)
(* -------------------------------------------------------------------------- *)

let null_base p = e_eq (e_fun f_base [p]) e_zero

(* See: tests/why3/test_memory.why *)

(* - lemma valid_rd_null: forall m n p. p.base = 0 -> (n <= 0 <-> valid_rd m p n)
   - lemma valid_rw_null: forall m n p. p.base = 0 -> (n <= 0 <-> valid_rw m p n)
*)
let r_valid_unref = function
  | [_; p; n] when decide (null_base p) ->
    e_leq n e_zero
  | _ -> raise Not_found

(* - lemma invalid_null: forall m n p. p.base = 0 -> invalid m p n *)
let r_invalid = function
  | [_; p; _] when decide (null_base p) -> e_true
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Simplifiers Registration                                           --- *)
(* -------------------------------------------------------------------------- *)

let () = Context.register
    begin fun () ->
      set_builtin_1   f_base   phi_base ;
      set_builtin_1   f_offset phi_offset ;
      set_builtin_2   f_shift  (phi_shift f_shift) ;
      set_builtin_eqp f_shift  eq_shift ;
      set_builtin_eqp f_global eq_shift ;
      set_builtin p_separated r_separated ;
      set_builtin p_included  r_included ;
      set_builtin_1 f_addr_of_int phi_addr_of_int ;
      set_builtin_1 f_int_of_addr phi_int_of_addr ;
      set_builtin p_invalid r_invalid ;
      set_builtin p_valid_rd r_valid_unref ;
      set_builtin p_valid_rw r_valid_unref ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Identify lfun                                                      --- *)
(* -------------------------------------------------------------------------- *)

let is_p_valid_rd lf = lf == p_valid_rd
let is_p_valid_rw lf = lf == p_valid_rw
let is_p_valid_obj lf = lf == p_valid_obj
let is_p_invalid lf = lf == p_invalid

let is_f_global lf = lf == f_global

(* -------------------------------------------------------------------------- *)
(* --- Range Comparison                                                   --- *)
(* -------------------------------------------------------------------------- *)

type range =
  | LOC of term * term (* loc - size *)
  | RANGE of term * Vset.set (* base - range offset *)

let range ~shift ~addrof ~sizeof = function
  | Memory.Rloc(obj,loc) ->
    LOC( addrof loc , sizeof obj )
  | Memory.Rrange(loc,obj,Some a,Some b) ->
    let s = sizeof obj in
    let p = addrof (shift loc obj a) in
    let n = e_mul s (e_range a b) in
    LOC( p , n )
  | Memory.Rrange(loc,_obj,None,None) ->
    RANGE( base (addrof loc) , Vset.range None None )
  | Memory.Rrange(loc,obj,Some a,None) ->
    let s = sizeof obj in
    RANGE( base (addrof loc) , Vset.range (Some (e_mul s a)) None )
  | Memory.Rrange(loc,obj,None,Some b) ->
    let s = sizeof obj in
    RANGE( base (addrof loc) , Vset.range None (Some (e_mul s b)) )

let range_set = function
  | LOC(l,n) ->
    let a = offset l in
    let b = e_add a n in
    base l , Vset.range (Some a) (Some b)
  | RANGE(base,set) -> base , set

let r_included r1 r2 =
  match r1 , r2 with
  | LOC(l1,n1) , LOC(l2,n2) ->
    p_call p_included [l1;n1;l2;n2]
  | _ ->
    let base1,set1 = range_set r1 in
    let base2,set2 = range_set r2 in
    p_if (p_equal base1 base2)
      (Vset.subset set1 set2)
      (Vset.is_empty set1)

let r_disjoint r1 r2 =
  match r1 , r2 with
  | LOC(l1,n1) , LOC(l2,n2) ->
    p_call p_separated [l1;n1;l2;n2]
  | _ ->
    let base1,set1 = range_set r1 in
    let base2,set2 = range_set r2 in
    p_imply (p_equal base1 base2) (Vset.disjoint set1 set2)

let included ~shift ~addrof ~sizeof s1 s2  =
  let range = range ~shift ~addrof ~sizeof in
  r_included (range s1) (range s2)

let separated ~shift ~addrof ~sizeof s1 s2 =
  let range = range ~shift ~addrof ~sizeof in
  r_disjoint (range s1) (range s2)
