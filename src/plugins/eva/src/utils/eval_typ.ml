(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cvalue

let bitfield_size_attributes attrs =
  match Ast_attributes.(find_params bitfield_attribute_name attrs) with
  | [AInt size] -> Some size
  | _ -> None

let sizeof_lval_typ typlv =
  match Ast_types.unroll typlv with
  | { tnode = (TInt _ | TEnum _); tattr } as t ->
    (match Ast_attributes.(find_params bitfield_attribute_name tattr) with
     | [AInt i] -> `Value i
     | _ -> Bit_utils.sizeof t)
  | t -> Bit_utils.sizeof t

let offsetmap_matches_type typ_lv o =
  let aux typ_matches = match V_Offsetmap.single_interval_value o with
    | None -> true (* multiple bindings. Assume that type matches *)
    | Some v ->
      let v = V_Or_Uninitialized.get_v v in
      try typ_matches (V.project_ival_bottom v)
      with V.Not_based_on_null -> true (* Do not mess with pointers *)
  in
  match Ast_types.unroll_node typ_lv with
  | TFloat _ -> aux Ival.is_float
  | TInt _ | TEnum _ | TPtr _ -> aux Ival.is_int
  | _ -> true


type fct_pointer_compatibility =
  | Compatible
  | Incompatible
  | Incompatible_but_accepted

let is_compatible_function ~typ_pointed ~typ_fun =
  (* our own notion of weak compatibility:
     - attributes and qualifiers are always ignored
     - all pointers types are considered compatible
     - enums and integer types with the same signedness and size are equal *)
  let weak_compatible t1 t2 =
    Cil.areCompatibleTypes t1 t2 ||
    match Ast_types.unroll_node t1, Ast_types.unroll_node t2 with
    | TVoid, TVoid -> true
    | TPtr _, TPtr _ -> true
    | (TInt ik1 | TEnum {ekind = ik1}),
      (TInt ik2 | TEnum {ekind = ik2}) ->
      Cil.isSigned ik1 = Cil.isSigned ik2 &&
      Cil.bitsSizeOfInt ik1 = Cil.bitsSizeOfInt ik2
    | TFloat fk1, TFloat fk2 -> fk1 = fk2
    | TComp ci1, TComp ci2 ->
      Cil_datatype.Compinfo.equal ci1 ci2
    | _ -> false
  in
  if Cil.areCompatibleTypes typ_fun typ_pointed then Compatible
  else
    let continue =
      match Ast_types.unroll_node typ_pointed, Ast_types.unroll_node typ_fun with
      | TFun (ret1, args1, var1), TFun (ret2, args2, var2) ->
        (* Either both functions are variadic, or none. Otherwise, it
           will be too complicated to make the argument match *)
        var1 = var2 &&
        (* Both functions return something weakly compatible *)
        weak_compatible ret1 ret2 &&
        (* Argument lists of the same length, with compatible arguments
           or unspecified argument lists *)
        (match args1, args2 with
         | None, None | None, Some _ | Some _, None -> true
         | Some lp, Some lf ->
           (* See corresponding function fold_left2_best_effort in
              Function_args *)
           let rec comp lp lf = match lp, lf with
             | _, [] -> true (* accept too many arguments passed *)
             | [], _ :: _ -> false (* fail on too few arguments *)
             | (_, tp, _) :: qp, (_, tf, _) :: qf ->
               weak_compatible tp tf && comp qp qf
           in
           comp lp lf
        )
      | _ -> false
    in
    if continue then Incompatible_but_accepted else Incompatible

let refine_fun_ptr typ args =
  match Ast_types.unroll typ, args with
  | { tnode = TFun (_, Some _, _) }, _ | _, None -> typ
  | { tnode = TFun (ret, None, var); tattr }, Some l ->
    let ltyps = List.map (fun arg -> "", arg, []) l in
    Cil_const.mk_tfun ~tattr ret (Some ltyps) var
  | _ -> assert false

(* Filters the list of kernel function [kfs] to only keep functions compatible
   with the type [typ_pointer]. *)
let compatible_functions typ_pointer ?args kfs =
  let typ_pointer = refine_fun_ptr typ_pointer args in
  let check_pointer (list, alarm) kf =
    let typ = Kernel_function.get_type kf in
    if Ast_types.is_fun typ then
      match is_compatible_function ~typ_pointed:typ_pointer ~typ_fun:typ with
      | Compatible -> kf :: list, alarm
      | Incompatible_but_accepted -> kf :: list, true
      | Incompatible -> list, true
    else list, true
  in
  List.fold_left check_pointer ([], false) kfs

(* Scalar types *)

type integer_range = { i_bits: int; i_signed: bool }

module DatatypeIntegerRange =
  Datatype.Make(struct
    include Datatype.Serializable_undefined

    type t = integer_range
    let reprs = [{i_bits = 1; i_signed = true}]
    let name = "Value.Eval_typ.DatatypeIntegerRange"
    let mem_project = Datatype.never_any_project
  end)

let ik_range ik : integer_range =
  { i_bits = Cil.bitsSizeOfInt ik; i_signed = Cil.isSigned ik }

let ik_attrs_range ik attrs =
  let i_bits =
    match bitfield_size_attributes attrs with
    | None -> Cil.bitsSizeOfInt ik
    | Some size -> Z.to_int size
  in
  { i_bits; i_signed = Cil.isSigned ik }

let range_inclusion r1 r2 =
  match r1.i_signed, r2.i_signed with
  | true, true
  | false, false -> r1.i_bits <= r2.i_bits
  | true, false ->  false
  | false, true ->  r1.i_bits <= r2.i_bits-1

let range_lower_bound r =
  if r.i_signed then Cil.min_signed_number r.i_bits else Z.zero

let range_upper_bound r =
  if r.i_signed
  then Cil.max_signed_number r.i_bits
  else Cil.max_unsigned_number r.i_bits


type scalar_typ =
  | TSInt of integer_range
  | TSPtr of integer_range
  | TSFloat of fkind

let pointer_range () =
  { i_bits = Cil.bitsSizeOfInt (Machine.uintptr_kind ());
    i_signed = false; }

let classify_as_scalar typ =
  match Ast_types.unroll typ with
  | { tnode = (TInt ik | TEnum { ekind = ik }); tattr } ->
    Some (TSInt (ik_attrs_range ik tattr))
  | { tnode = TPtr _ } -> Some (TSPtr (pointer_range ()))
  | { tnode = TFloat fk } -> Some (TSFloat fk)
  | _ -> None

let integer_range ~ptr typ =
  match Ast_types.unroll typ with
  | { tnode = (TInt ik | TEnum { ekind = ik }); tattr } ->
    Some (ik_attrs_range ik tattr)
  | { tnode = TPtr _ } when ptr -> Some (pointer_range ())
  | _ -> None

let need_cast t1 t2 =
  match classify_as_scalar t1, classify_as_scalar t2 with
  | None, None -> Cil.need_cast t1 t2
  | None, _ | _, None -> true
  | Some st1, Some st2 ->
    match st1, st2 with
    | (TSInt ir1 | TSPtr ir1), (TSInt ir2 | TSPtr ir2) -> ir1 <> ir2
    | TSFloat fk1, TSFloat fk2 -> fk1 <> fk2
    | (TSInt _ | TSPtr _ | TSFloat _), (TSInt _ | TSPtr _ | TSFloat _) -> true
