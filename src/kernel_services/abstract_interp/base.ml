(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Abstract_interp

type variable_validity = {
  mutable weak: bool;
  mutable min_alloc : Z.t;
  mutable max_alloc : Z.t;
  max_allocable: Z.t (* not mutable, determined when the base is created *);
}

type validity =
  | Empty
  | Known of Z.t * Z.t
  | Unknown of Z.t * Z.t option * Z.t
  | Variable of variable_validity
  | Invalid

let pretty_validity fmt v =
  match v with
  | Empty -> Format.fprintf fmt "Empty"
  | Unknown (b,k,e)  ->
    Format.fprintf fmt "Unknown %a/%a/%a"
      Z.pretty b (Pretty_utils.pp_opt Z.pretty) k Z.pretty e
  | Known (b,e)  -> Format.fprintf fmt "Known %a-%a" Z.pretty b Z.pretty e
  | Invalid -> Format.fprintf fmt "Invalid"
  | Variable variable_v ->
    Format.fprintf fmt "Variable [0..%a--%a]"
      Z.pretty variable_v.min_alloc Z.pretty variable_v.max_alloc

module Validity = Datatype.Make
    (struct
      type t = validity
      let name = "Base.validity"
      let structural_descr = Structural_descr.t_abstract
      let reprs = [ Known (Z.zero, Z.one) ]

      (* Invalid > Variable > Unknown > Known > Empty *)
      let compare v1 v2 = match v1, v2 with
        | Empty, Empty -> 0
        | Known (b1, e1), Known (b2, e2) ->
          let c = Z.compare b1 b2 in
          if c = 0 then Z.compare e1 e2 else c
        | Unknown (b1, m1, e1), Unknown (b2, m2, e2) ->
          let c = Z.compare b1 b2 in
          if c = 0 then
            let c = Option.compare Z.compare m1 m2 in
            if c = 0 then Z.compare e1 e2 else c
          else c
        | Variable v1, Variable v2 ->
          let c = Z.compare v1.min_alloc v2.min_alloc in
          if c = 0 then
            let c = Z.compare v1.max_alloc v2.max_alloc in
            if c = 0 then Z.compare v1.max_allocable v2.max_allocable
            else c
          else c
        | Invalid, Invalid -> 0
        | Empty, (Known _ | Unknown _ | Variable _ | Invalid)
        | Known _, (Unknown _ | Variable _ | Invalid)
        | Unknown _, (Variable _ | Invalid)
        | Variable _, Invalid -> -1
        | Invalid, (Variable _ | Unknown _ | Known _ | Empty)
        | Variable _, (Unknown _ | Known _ | Empty)
        | Unknown _, (Known _ | Empty)
        | Known _, Empty -> 1

      let equal = Datatype.from_compare

      let hash v = match v with
        | Empty -> 13
        | Invalid -> 37
        | Known (b, e) -> Hashtbl.hash (3, Z.hash b, Z.hash e)
        | Unknown (b, m, e) ->
          Hashtbl.hash (7, Z.hash b, Option.hash Z.hash m, Z.hash e)
        | Variable variable_v ->
          Hashtbl.hash (Z.hash variable_v.min_alloc, Z.hash variable_v.max_alloc)

      let pretty = pretty_validity
      let mem_project = Datatype.never_any_project
      let rehash = Datatype.identity
      let copy (x:t) = x
    end)

type deallocation = Malloc | VLA | Alloca

type base =
  | Var of varinfo * validity
  | CLogic_Var of logic_var * typ * validity
  | Null (** base for addresses like [(int* )0x123] *)
  | Allocated of varinfo * deallocation * validity

let id = function
  | Var (vi,_) | Allocated (vi,_,_) -> vi.vid
  | CLogic_Var (lvi, _, _) -> lvi.lv_id
  | Null -> 0

let hash = id

let null = Null

let is_null x = match x with Null -> true | _ -> false

let is_string_literal = function
  | Var(v, _) -> Ast_info.is_string_literal v
  | _ -> false

let pretty fmt t =
  match t with
  | Var(v,_) when Ast_info.is_string_literal v ->
    Printer.pp_str_literal fmt (Globals.Vars.get_string_literal v)
  | Var (t,_) | Allocated (t,_,_) -> Printer.pp_varinfo fmt t
  | CLogic_Var (lvi, _, _) -> Printer.pp_logic_var fmt lvi
  | Null -> Format.pp_print_string fmt "NULL"

let pretty_addr fmt t =
  (match t with
   | Var (v,_) when Ast_info.is_string_literal v -> ()
   | Var _ | CLogic_Var _ | Allocated _ ->
     Format.pp_print_string fmt "&"
   | Null -> ()
  );
  pretty fmt t

let compare v1 v2 = Datatype.Int.compare (id v1) (id v2)

let typeof v =
  match v with
  | CLogic_Var (_, ty, _) -> Some ty
  | Null -> None
  | Var (v,_) | Allocated(v,_,_) -> Some (Ast_types.unroll v.vtype)

let bits_sizeof v =
  match v with
  | Null -> Z_or_top.top
  | Var (v,_) | Allocated (v,_,_) ->
    Bit_utils.sizeof_vid v
  | CLogic_Var (_, ty, _) -> Bit_utils.sizeof ty

let alignof base =
  try
    match base with
    | Null -> 0 (* Address of null is 0. *)
    | CLogic_Var (_, typ, _) -> Cil.bytesAlignOf typ
    | Var (vi, _) | Allocated (vi, _, _) -> Cil.bytesAlignOfVarinfo vi
  with Cil.SizeOfError (msg, _) ->
    (* Any address is possible: no alignment constraint. *)
    Kernel.warning ~once:true
      "Unknown alignment of base %a: %s" pretty base msg;
    1

let dep_absolute = [Kernel.AbsoluteValidRange.self]

module MinValidAbsoluteAddress =
  State_builder.Ref
    (Z)
    (struct
      let name = "MinValidAbsoluteAddress"
      let dependencies = dep_absolute
      let default () = Z.zero
    end)

module MaxValidAbsoluteAddress =
  State_builder.Ref
    (Z)
    (struct
      let name = "MaxValidAbsoluteAddress"
      let dependencies = dep_absolute
      let default () = Z.minus_one
    end)

let () =
  Kernel.AbsoluteValidRange.add_set_hook
    (fun _ x ->
       try Scanf.sscanf x "%s@-%s"
             (fun min max ->
                (* let mul_CHAR_BIT = Int64.mul (Int64.of_int (bitsSizeOf charType)) in *)
                (* the above is what we would like to write but it is too early *)
                let mul_CHAR_BIT = Z.mul 8z in
                MinValidAbsoluteAddress.set
                  (mul_CHAR_BIT (Z.of_string min));
                MaxValidAbsoluteAddress.set
                  ((Z.pred (mul_CHAR_BIT (Z.succ (Z.of_string max))))))
       with End_of_file | Scanf.Scan_failure _ | Failure _
          | Invalid_argument _ as e ->
         Kernel.abort "Invalid -absolute-valid-range integer-integer: each integer may be in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and has to hold in 64 bits. A correct example is -absolute-valid-range 1-0xFFFFFF0.@\nError was %S@."
           (Printexc.to_string e))

let min_valid_absolute_address = MinValidAbsoluteAddress.get
let max_valid_absolute_address = MaxValidAbsoluteAddress.get

let validity_from_size size =
  assert (Z.geq size 0z);
  if Z.is_zero size then Empty
  else Known (Z.zero, Z.pred size)

let validity_from_known_size size =
  match size with
  | `Value size ->
    (* all start to be valid at offset 0 *)
    validity_from_size size
  | `Top ->
    Unknown (Z.zero, None, Bit_utils.max_bit_address ())

let validity b =
  match b with
  | Var (_,v) | CLogic_Var (_, _, v) | Allocated (_,_,v) -> v
  | Null ->
    let mn = min_valid_absolute_address ()in
    let mx = max_valid_absolute_address () in
    if Z.gt mx mn then
      Known (mn, mx)
    else
      Invalid

let is_read_only base =
  match base with
  | Var (v,_) -> Ast_types.has_qualifier "const" v.vtype
  | _ -> false

(* Minor optimization compared to [is_weak (validity b)] *)
let is_weak = function
  | Allocated (_, _, Variable { weak }) -> weak
  | _ -> false

(* Does a C type end by an empty struct? *)
let rec final_empty_struct t =
  match t.tnode with
  | TArray (typ, _) -> final_empty_struct typ
  | TComp compinfo ->
    begin
      match compinfo.cfields with
      | Some [] | None -> true
      | Some l ->
        let last_field = List.(hd (rev l)) in
        try Cil.bitsSizeOf last_field.ftype = 0
        with Cil.SizeOfError _ -> false
    end
  | TNamed typeinfo -> final_empty_struct typeinfo.ttype
  | TVoid | TInt _ | TFloat _ | TPtr _ | TEnum _
  | TFun _ | TBuiltin_va_list -> false

(* Does a base end by an empty struct? *)
let final_empty_struct = function
  | Var (vi, _) | Allocated (vi, _, _) -> final_empty_struct vi.vtype
  | _ -> false

type access =
  | Read of Z.t
  | Write of Z.t
  | Object_pointer
  | Any_pointer

let for_writing = function
  | Write _ -> true
  | Read _ | Object_pointer | Any_pointer -> false

let is_empty_access = function
  | Read size | Write size -> Z.is_zero size
  | Object_pointer | Any_pointer -> true

(* Computes the last valid offset for an access of the base [base] with [max]
   valid bits: [max - size + 1] for an access of size [size]. *)
let last_valid_offset base max = function
  | Read size | Write size ->
    if Z.is_zero size
    (* For an access of size 0, [max] is the last valid offset, unless the base
       ends by an empty struct, in which case [max+1] is also a valid offset. *)
    then if final_empty_struct base then Z.succ max else max
    else Z.sub max (Z.pred size)
  | Object_pointer | Any_pointer ->
    Z.succ max (* A pointer can point just beyond its object. *)

let offset_for_validity ~bitfield access base =
  match validity base with
  | Empty -> if is_empty_access access then Ival.zero else Ival.bottom
  | Invalid -> if access = Any_pointer then Ival.zero else Ival.bottom
  | Known (min, max) | Unknown (min, _, max) ->
    let max = last_valid_offset base max access in
    if bitfield
    then Ival.inject_range (Some min) (Some max)
    else
      Ival.inject_interval ~min:(Some min) ~max:(Some max) ~rem:0z ~modu:8z
  | Variable variable_v ->
    let maxv = last_valid_offset base variable_v.max_alloc access in
    Ival.inject_range (Some 0z) (Some maxv)

let valid_offset ?(bitfield=true) access base =
  if for_writing access && is_read_only base
  then Ival.bottom
  else
    let offset = offset_for_validity ~bitfield access base in
    (* When -absolute-valid-range is enabled, the Null base has a Known validity
       that does not include 0. In this case, adds 0 as possible offset for a
       pointer without memory access. *)
    if access = Any_pointer && is_null base
    then Ival.(join zero offset)
    else offset

let offset_is_in_validity access base ival =
  let is_valid_for_bounds min_bound max_bound =
    match Ival.min_and_max ival with
    | Some min, Some max ->
      Z.geq min min_bound &&
      Z.leq max (last_valid_offset base max_bound access)
    | _, _ -> false
  in
  match validity base with
  | Empty -> is_empty_access access && Ival.(equal zero ival)
  | Invalid -> access = Any_pointer && Ival.(equal zero ival)
  | Known (min, max)
  | Unknown (min, Some max, _) -> is_valid_for_bounds min max
  | Unknown (_, None, _) -> false (* All accesses are possibly invalid. *)
  | Variable v -> is_valid_for_bounds Z.zero v.min_alloc

let is_valid_offset access base offset =
  Ival.is_bottom offset
  || not (for_writing access && (is_read_only base))
     && (offset_is_in_validity access base offset
         || access = Any_pointer && is_null base && Ival.(equal zero offset))

let is_function base =
  match base with
  | Null | CLogic_Var _ | Allocated _ -> false
  | Var(v,_) ->
    Ast_types.is_fun v.vtype

let equal v w = (id v) = (id w)

let is_any_formal_or_local v =
  match v with
  | Var (v,_) -> v.vsource && not v.vglob
  | Allocated _ | CLogic_Var _ -> false
  | Null -> false

let is_any_local v =
  match v with
  | Var (v,_) -> v.vsource && not v.vglob && not v.vformal
  | Allocated _ | CLogic_Var _ -> false
  | Null -> false

let is_global v =
  match v with
  | Var (v,_) -> v.vglob
  | Allocated _ | Null -> true
  | CLogic_Var _ -> false

let is_formal_or_local v fundec =
  match v with
  | Var (v,_) -> Ast_info.Function.is_formal_or_local v fundec
  | Allocated _ | CLogic_Var _ | Null -> false

let is_formal_of_prototype v vi =
  match v with
  | Var (v,_) -> Ast_info.Function.is_formal_of_prototype v vi
  | Allocated _ | CLogic_Var _ | Null -> false

let is_local v fundec =
  match v with
  | Var (v,_) -> Ast_info.Function.is_local v fundec
  | Allocated _ | CLogic_Var _ | Null -> false

let is_formal v fundec =
  match v with
  | Var (v,_)  -> Ast_info.Function.is_formal v fundec
  | Allocated _ | CLogic_Var _ | Null -> false

let is_block_local v block =
  match v with
  | Var (v,_) -> Ast_info.is_block_local v block
  | Allocated _ | CLogic_Var _ | Null -> false

let validity_from_type v =
  if Ast_types.is_fun v.vtype then Invalid
  else
    let max_valid = Bit_utils.sizeof_vid v in
    match max_valid with
    | `Top -> Unknown (Z.zero, None, Bit_utils.max_bit_address ())
    | `Value size -> validity_from_size size

type range_validity =
  | Invalid_range
  | Valid_range of Int_Intervals_sig.itv option

let valid_range = function
  | Invalid -> Invalid_range
  | Empty -> Valid_range None
  | Known (min_valid,max_valid)
  | Unknown (min_valid,_,max_valid)-> Valid_range (Some (min_valid, max_valid))
  | Variable variable_v -> Valid_range (Some (Z.zero, variable_v.max_alloc))

let is_weak_validity = function
  | Variable { weak } -> weak
  | _ -> false

let create_variable_validity ~weak ~min_alloc ~max_alloc =
  let max_allocable = Bit_utils.max_bit_address () in
  { weak; min_alloc; max_alloc; max_allocable }

let update_variable_validity v ~weak ~min_alloc ~max_alloc =
  v.min_alloc <- Z.min min_alloc v.min_alloc;
  v.max_alloc <- Z.max max_alloc v.max_alloc;
  if weak then v.weak <- true


module Base = struct
  include Datatype.Make_with_collections
      (struct
        type t = base
        let name = "Base"
        let structural_descr = Structural_descr.t_abstract (* TODO better *)
        let reprs = [ Null ]
        let equal = equal
        let compare = compare
        let pretty = pretty
        let hash = hash
        let mem_project = Datatype.never_any_project
        let rehash = Datatype.identity
        let copy = Datatype.undefined
      end)
  let id = id
end

include Base

module Hptshape = Hptmap.Shape (Base)

module Hptset = Hptset.Make
    (Base)
    (struct
      let initial_values = [ [Null] ]
      let dependencies = [ Ast.self ]
    end)
let () = Ast.add_monotonic_state Hptset.self
let () = Ast.add_hook_on_update Hptset.clear_caches

let null_set = Hptset.singleton Null

module VarinfoNotSource =
  Cil_state_builder.Varinfo_hashtbl
    (Base)
    (struct
      let name = "Base.VarinfoLogic"
      let dependencies = [ Ast.self ]
      let size = 89
    end)
let () = Ast.add_monotonic_state VarinfoNotSource.self

let base_of_varinfo varinfo =
  assert varinfo.vsource;
  let validity = validity_from_type varinfo in
  Var (varinfo, validity)

module Validities =
  Cil_state_builder.Varinfo_hashtbl
    (Base)
    (struct
      let name = "Base.Validities"
      let dependencies = [ Ast.self ]
      (* No dependency on Kernel.AbsoluteValidRange.self needed:
         the null base is not present in this table (not a varinfo) *)
      let size = 117
    end)
let () = Ast.add_monotonic_state Validities.self

let of_varinfo_aux = Validities.memo base_of_varinfo

let register_memory_var varinfo validity =
  assert (not varinfo.vsource && not (VarinfoNotSource.mem varinfo));
  let base = Var (varinfo,validity) in
  VarinfoNotSource.add varinfo base;
  base

let register_allocated_var varinfo deallocation validity =
  assert (not varinfo.vsource);
  let base = Allocated (varinfo,deallocation,validity) in
  VarinfoNotSource.add varinfo base;
  base

let of_c_logic_var lv =
  match Ast_types.unroll_logic lv.lv_type with
  | Ctype ty ->
    CLogic_Var (lv, ty, validity_from_known_size (Bit_utils.sizeof ty))
  | _ -> Kernel.fatal "Logic variable with a non-C type %s" lv.lv_name

let of_varinfo varinfo =
  if varinfo.vsource
  then of_varinfo_aux varinfo
  else
    try VarinfoNotSource.find varinfo
    with Not_found ->
      Kernel.fatal "Querying base for unknown non-source variable %a"
        Printer.pp_varinfo varinfo

exception Not_a_C_variable

let to_varinfo t = match t with
  | Var (t,_) | Allocated (t,_,_) -> t
  | CLogic_Var _ | Null -> raise Not_a_C_variable

module SetLattice = Make_Hashconsed_Lattice_Set(Base)(Hptset)

module BMap =
  Hptmap.Make (Base) (Base)
    (struct
      let initial_values = []
      let dependencies = [ Ast.self ]
    end)

type substitution = base Hptshape.map

let substitution_from_list list =
  let add map (key, elt) = BMap.add key elt map in
  List.fold_left add BMap.empty list
