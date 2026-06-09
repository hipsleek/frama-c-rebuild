(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cvalue

open Cil_types
open Abstract_interp
open Lattice_bounds

let offsetmap_of_v ~typ v =
  let size = Z.of_int (Cil.bitsSizeOf typ) in
  let v = V_Or_Uninitialized.initialized v in
  V_Offsetmap.create ~size v ~size_v:size

let offsetmap_of_loc location state =
  let aux (loc: Locations.t) offsm_res =
    (* If the size is unknown, returns the complete offsetmap. *)
    let size =
      try Z_or_top.project loc.size
      with Abstract_interp.Error_Top -> Bit_utils.max_bit_size ()
    in
    let copy = Cvalue.Model.copy_offsetmap loc.addr size state in
    Bottom.join Cvalue.V_Offsetmap.join copy offsm_res
  in
  Precise_locs.fold aux location `Bottom

let v_uninit_of_offsetmap ~typ offsm =
  let size = Eval_typ.sizeof_lval_typ typ in
  match size with
  | `Top -> V_Offsetmap.find_imprecise_everywhere offsm
  | `Value size ->
    let validity = Base.validity_from_size size in
    let offsets = Ival.zero in
    V_Offsetmap.find ~validity ~conflate_bottom:false ~offsets ~size offsm

let backward_comp_int_left positive comp l r =
  if (Parameters.UndefinedPointerComparisonPropagateAll.get())
  && not (Cvalue_forward.are_comparable comp l r)
  then l
  else
    let binop = if positive then comp else Comp.inv comp in
    V.backward_comp_int_left binop l r

let backward_comp_float_left fkind positive comp l r =
  let back =
    if positive
    then V.backward_comp_float_left_true
    else V.backward_comp_float_left_false in
  back comp fkind l r

let backward_comp_left_from_type = function
  | Ctype typ -> begin
      match Ast_types.unroll_node typ with
      | TInt _ | TEnum _ | TPtr _ -> backward_comp_int_left
      | TFloat fk -> backward_comp_float_left (Fval.kind fk)
      | _ -> (fun _ _ v _ -> v) (* should never occur anyway *)
    end
  | Linteger -> backward_comp_int_left
  | Lreal -> backward_comp_float_left (Fval.Real)
  | _ -> (fun _ _ v _ -> v) (* should never occur anyway *)

exception Unchanged
exception Reduce_to_bottom

let reduce_by_initialized_defined f loc state =
  try
    let base, offset =
      Addresses.Bits.find_lonely_key loc.Locations.addr
    in
    if Base.is_weak base then raise Unchanged;
    let size = Z_or_top.project loc.Locations.size in
    let ll = Ival.project_int offset in
    let lh = Z.pred (Z.add ll size) in
    let offsm = match Model.find_base_or_default base state with
      | `Bottom | `Top -> raise Unchanged
      | `Value offsm -> offsm
    in
    let aux (offl, offh) (v, modu, shift) acc =
      let v' = f v in
      if v' != v then begin
        if V_Or_Uninitialized.is_bottom v' then raise Reduce_to_bottom;
        let il = Z.max offl ll and ih = Z.min offh lh in
        let abs_shift = Z.erem (Rel.add_abs offl shift) modu in
        (* il and ih are the bounds of the interval to reduce.
           We change the initialized flags in the following cases:
           - either we overwrite entire values, or the partly overwritten
             value is at the beginning or at the end of the subrange
           - or we do not lose information on misaligned or partial values:
             the result is a singleton *)
        if V_Or_Uninitialized.(cardinal_zero_or_one v' || is_isotropic v') ||
           ((Z.equal offl il || Z.equal (Z.erem ll modu) abs_shift) &&
            (Z.equal offh ih ||
             Z.equal (Z.erem (Z.succ lh) modu) abs_shift))
        then
          let diff = Rel.sub_abs il offl in
          let shift_il = Rel.erem (Rel.sub shift diff) modu in
          V_Offsetmap.add (il, ih) (v', modu, shift_il) acc
        else acc
      end
      else acc
    in
    let noffsm =
      V_Offsetmap.fold_between ~entire:true (ll, lh) aux offsm offsm
    in
    Model.add_base base noffsm state
  with
  | Reduce_to_bottom -> Model.bottom
  | Unchanged -> state
  | Abstract_interp.Error_Top (* from Z_or_top.project *)
  | Not_found (* from find_lonely_key *)
  | Ival.Not_Singleton_Int (* from Ival.project_int *) ->
    state

let reduce_by_valid_loc ~positive access loc typ state =
  let value = Cvalue.Model.find_indeterminate state loc in
  let addr_bytes = Cvalue.V_Or_Uninitialized.get_v value in
  let addr_bits = Addresses.Bits.of_bytes addr_bytes in
  let size = Bit_utils.sizeof_pointed typ in
  let location = Locations.make addr_bits size in
  let reduced_location =
    if positive
    then Locations.valid_part access location
    else Locations.invalid_part location
  in
  let reduced_addr_bytes = Locations.addr_bytes reduced_location in
  let reduced_value =
    if positive
    then Cvalue.V_Or_Uninitialized.initialized reduced_addr_bytes
    else Cvalue.V_Or_Uninitialized.map (fun _ -> reduced_addr_bytes) value
  in
  if Cvalue.V_Or_Uninitialized.equal value reduced_value
  then state
  else
  if Cvalue.V_Or_Uninitialized.(equal bottom reduced_value)
  then Cvalue.Model.bottom
  else Cvalue.Model.reduce_indeterminate_binding state loc reduced_value

let make_loc_contiguous loc =
  try
    let base, offset =
      Addresses.Bits.find_lonely_key loc.Locations.addr
    in
    if Ival.is_small_set offset
    then loc
    else
      let min, max, _rem, modu = Ival.min_max_r_mod offset in
      match min, max, loc.Locations.size with
      | Some min, Some max, `Value size when Z.equal modu size ->
        let size' = Z.add (Z.sub max min) modu in
        let i = Ival.inject_singleton min in
        let addr_bits = Addresses.Bits.inject base i in
        Locations.make addr_bits (`Value size')
      | _ -> loc
  with Not_found -> loc

let apply_on_all_locs f loc state =
  match loc.Locations.size with
  | `Top -> state
  | `Value _ as size ->
    let loc = Locations.valid_part Locations.Read loc in
    let plevel = Parameters.ArrayPrecisionLevel.get () in
    let ilevel = Int_set.get_small_cardinal () in
    let limit = max plevel ilevel in
    let apply_f base ival state =
      f (Locations.make (Addresses.Bits.inject base ival) size) state
    in
    let aux base ival state =
      if Ival.cardinal_is_less_than ival limit
      then Ival.fold_enum (fun i acc -> apply_f base i acc) ival state
      else state
    in
    try Addresses.Bits.fold_i aux loc.addr state
    with Abstract_interp.Error_Top -> state

(* Display [o] as a single value, when this is more readable and more precise
   than the standard display. *)
let pretty_stitched_offsetmap fmt typ o =
  if Ast_types.is_scalar typ &&
     not (Cvalue.V_Offsetmap.is_single_interval o)
  then
    match v_uninit_of_offsetmap ~typ o with
    | `Value v when not (Cvalue.V_Or_Uninitialized.is_isotropic v) ->
      Format.fprintf fmt "@\nThis amounts to: %a"
        Cvalue.V_Or_Uninitialized.pretty v
    | _ -> ()

let pretty_offsetmap typ fmt offsm =
  (* YYY: catch pointers to arrays, and print the contents of the array *)
  Format.fprintf fmt "@[";
  if Cvalue.V_Offsetmap.(equal empty offsm)
  then Unicode.pp_empty_set fmt
  else begin
    match Cvalue.V_Offsetmap.single_interval_value offsm with
    | Some value -> Cvalue.V_Or_Uninitialized.pretty_typ (Some typ) fmt value;
    | None ->
      Cvalue.V_Offsetmap.pretty_generic ~typ () fmt offsm;
      pretty_stitched_offsetmap fmt typ offsm
  end;
  Format.fprintf fmt "@]"

(* ------------------------- Under-approximation ---------------------------- *)

let add_if_singleton value acc =
  if Cvalue.V_Or_Uninitialized.cardinal_zero_or_one value
  then Cvalue.V_Or_Uninitialized.link value acc
  else acc

let find_offsm_under validity ival size offsm acc =
  let offsets = Tr_offset.trim_by_validity ival size validity in
  match offsets with
  | Tr_offset.Invalid | Tr_offset.Overlap _ -> acc
  | Tr_offset.Set list ->
    let find acc offset =
      let offsets = Ival.inject_singleton offset in
      let value = Cvalue.V_Offsetmap.find ~validity ~offsets ~size offsm in
      let value = Cvalue.V_Or_Uninitialized.inject_or_bottom value in
      add_if_singleton value acc
    in
    List.fold_left find acc list
  | Tr_offset.Interval (min, max, modu) ->
    let process (start, _stop) (v, v_size, v_offset) acc =
      if Rel.(is_zero v_offset) && Z.equal v_size size
         && Z.is_zero (Z.erem (Z.sub start min) modu)
      then add_if_singleton v acc
      else acc
    in
    Cvalue.V_Offsetmap.fold_between ~entire:true (min, max) process offsm acc

exception CannotComputeUnder

let find_lmap_under state location =
  match location.Locations.size with
  | `Top -> raise CannotComputeUnder
  | `Value size ->
    match location.Locations.addr with
    | Addresses.Bits.Top _ -> raise CannotComputeUnder
    | Addresses.Bits.Map map ->
      let process base offset acc =
        let offsm = Cvalue.Model.find_base_or_default base state in
        match offsm with
        | `Bottom -> acc
        | `Top -> raise CannotComputeUnder
        | `Value offsm ->
          let validity = Base.validity base in
          find_offsm_under validity offset size offsm acc
      in
      let acc = Cvalue.V_Or_Uninitialized.bottom in
      Addresses.Bits.M.fold process map acc

let find_under_approximation state location =
  try Some (find_lmap_under state location)
  with CannotComputeUnder -> None
