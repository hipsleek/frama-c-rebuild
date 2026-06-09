(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* Must be used inlined, as Machine.theMachine is mutable
   let pointer_size_bytes = Machine.sizeof_ptr ()
   let int_size_bytes = Machine.sizeof_int ()
*)
let size_char_in_bits = 8

module Types = struct

  type state = Cvalue.Model.t
  type value = Cvalue.V.t
  type zone = Memory_zone.t
  type slice = Cvalue.V_Offsetmap.t

  type functions_states = state Cil_datatype.Stmt.Hashtbl.t
  type map_functions_states = state Cil_datatype.Stmt.Map.t

  type state_accesser =
    | Global
    | Local of functions_states

  let map_functions_states_to_get_state m =
    fun s ->
    try Cil_datatype.Stmt.Map.find s m
    with Not_found -> Cvalue.Model.bottom

  let functions_states_to_request h stmt =
    let state =
      try Cil_datatype.Stmt.Hashtbl.find h stmt
      with Not_found -> Cvalue.Model.bottom
    in
    Results.in_cvalue_state state

  let iter_requests = function
    | Global ->
      fun stmt f ->
        let requests = Results.(before stmt |> by_callstack |> List.map snd) in
        List.iter (fun request-> f request) requests
    | Local hs ->
      fun stmt f -> f (functions_states_to_request hs stmt)

  let merge_map_non_map_functions_states map h =
    Cil_datatype.Stmt.Hashtbl.fold
      (fun stmt state m ->
         let previous =
           try Cil_datatype.Stmt.Map.find stmt m
           with Not_found -> Cvalue.Model.bottom
         in
         let join = Cvalue.Model.join previous state in
         if join != previous then Cil_datatype.Stmt.Map.add stmt join m else m
      ) h map

  let merge_map_functions_states =
    Cil_datatype.Stmt.Map.closed_union (fun _ -> Cvalue.Model.join)


  (* -------------------------------------------------------------------------- *)
  (* --- Ids                                                                --- *)
  (* -------------------------------------------------------------------------- *)

  type pointer = Cil_types.varinfo * int

  module Pointer = struct
    include Datatype.Pair_with_collections(Cil_datatype.Varinfo)(Datatype.Int)

    let pretty fmt ((v, o) : pointer) =
      if o = 0 then
        Format.fprintf fmt "&%a" Printer.pp_varinfo v
      else
        Format.fprintf fmt "&%a+%d" Printer.pp_varinfo v o

  end

end

let typ_array_char = Cil_const.(mk_tarray ucharType None)

let pretty_slice fmt s =
  Cvalue.V_Offsetmap.pretty_generic ~typ:typ_array_char () fmt s


let location_with_size_aux p sbytes =
  Addresses.Bits.of_bytes p,
  Z.of_int (size_char_in_bits * sbytes)

let location_with_size p sbytes =
  let addr_bits, size = location_with_size_aux p sbytes in
  Locations.make addr_bits (`Value size)

let location_of_pointer (p : Types.pointer) =
  Addresses.Bytes.inject
    (Base.of_varinfo (fst p) ) (Ival.of_int (snd p))


let read_int_pointer p state =
  let p = location_of_pointer p in
  let p = location_with_size p (Machine.Sizeof.int ()) in
  Cvalue.Model.find state p

(* TODO: restore warnings *)
let read_slice ~p ~sbytes state =
  let loc_bits, size = location_with_size_aux p sbytes in
  match Cvalue.Model.copy_offsetmap loc_bits size state with
  | `Bottom ->
    assert (Cvalue.Model.equal state Cvalue.Model.bottom);
    Mt_self.fatal "Reading inside bottom state"
  | `Value offs -> offs

let write_int_pointer p i state =
  let sbytes = Machine.Sizeof.int ()
  and value = Addresses.Bytes.inject Base.null (Ival.of_int i) in
  let pointer = location_of_pointer p in
  let p = location_with_size pointer sbytes in
  Mt_self.debug ~level:3 "# Write %a at %a, size %d bytes"
    Cvalue.V.pretty value Locations.pretty p sbytes;
  Cvalue.Model.add_binding ~exact:true state p value

let replace_value_at_int_pointer p ~before ~after state =
  let sbytes = Machine.Sizeof.int () in
  let value_after = Addresses.Bytes.inject Base.null (Ival.of_int after) in
  let value_before = Addresses.Bytes.inject Base.null (Ival.of_int before) in
  let pointer = location_of_pointer p in
  let p = location_with_size pointer sbytes in
  let cur = Cvalue.Model.find ~conflate_bottom:true state p in
  if Addresses.Bytes.equal cur value_before then
    Cvalue.Model.add_binding ~exact:true state p value_after
  else
  if Addresses.Bytes.is_included value_before cur then
    let v = Cvalue.V.(join (diff_if_one cur value_before) value_after) in
    Cvalue.Model.add_binding ~exact:true state p v
  else
    state

let write_slice ~p ~sbytes ~slice ~exact state =
  let pointer = Addresses.Bits.of_bytes (location_of_pointer p) in
  Cvalue.Model.paste_offsetmap
    ~from:slice ~dst_addr:pointer
    ~size:(Z.of_int (sbytes * size_char_in_bits))
    ~exact
    state


(* ----- Conversion from cvalue --------------------------------------------- *)

(* All conversion functions below return an error message in case of failure. *)
type 'a conversion = ('a, string) Result.t

let error format = Format.kasprintf Result.error format

let extract_definition name kf =
  if Kernel_function.has_definition kf
  then Result.Ok kf
  else error "Missing definition for function %s" name

let extract_fun value =
  match fst (Addresses.Bytes.find_lonely_key value) with
  | Base.Var (vi, _) when Globals.Functions.mem vi ->
    extract_definition vi.vname (Globals.Functions.get vi)
  | _ | exception Not_found ->
    error "Expected pointer to function, received %a" Cvalue.V.pretty value

let extract_pointer value =
  match Addresses.Bytes.find_lonely_key value with
  | Base.Var (v, _), i
  | Base.Allocated (v, _, _), i ->
    begin
      try Result.Ok (v, Z.to_int (Ival.project_int i))
      with Ival.Not_Singleton_Int | Z.Overflow ->
        error "Not a correct pointer, incorrect offset: %a" Ival.pretty i
    end
  | _ | exception Not_found ->
    error "Not a correct pointer '%a' (should be variable+offset)"
      Cvalue.V.pretty value

let to_int i =
  try Result.Ok (Z.to_int i)
  with Z.Overflow -> error "Overflow on integer %a" Z.pretty i

let extract_int value =
  try Cvalue.V.project_ival value |> Ival.project_int |> to_int
  with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int ->
    error "Non-singleton integer value: %a" Cvalue.V.pretty value

let extract_int_possibly_zero value =
  match Cvalue.V.project_ival value |> Ival.project_small_set with
  | Some [v] ->
    to_int v |> Result.map (fun v -> v, `Exact)
  | Some [v1; v2] when Z.is_zero v1 ->
    to_int v2 |> Result.map (fun v -> v, `WithZero)
  | Some [v1; v2] when Z.is_zero v2 ->
    to_int v1 |> Result.map (fun v -> v, `WithZero)
  | Some _ | None | exception Cvalue.V.Not_based_on_null ->
    error "Non-integer or imprecise value: %a" Cvalue.V.pretty value

let extract_int_list ~cardinal value =
  try
    let ival = Cvalue.V.project_ival value in
    if Ival.is_int ival && Ival.cardinal_is_less_than ival cardinal
    then
      Ival.to_int_seq ival |>
      List.of_seq |>
      List.map Z.to_int |>
      Result.ok
    else error "Imprecise value: %a" Ival.pretty ival
  with
  | Cvalue.V.Not_based_on_null ->
    error "Non-integer value: %a" Cvalue.V.pretty value
  | Z.Overflow ->
    error "Overflow integer value: %a" Cvalue.V.pretty value

let extract_constant_string value =
  match Addresses.Bytes.fold_i (fun b i l -> (b,i) :: l) value [] with
  | [Base.Var (vi, _), i] when Ival.is_zero i && Ast_info.is_string_literal vi ->
    let l = Globals.Vars.get_string_literal vi in
    (match l with
     | Str s -> Result.ok s
     | Wstr _ -> error "Expected a string, not a wide string")
  | _ | exception Abstract_interp.Error_Top ->
    error "When decoding string, incorrect value '%a'" Cvalue.V.pretty value

(* *)

let clear_non_globals =
  Cvalue.Model.filter_base (fun v -> not (Base.is_any_formal_or_local v))


(* *)

let join_state s1 s2 =
  let r = Cvalue.Model.join s2 s1 in
  r, Cvalue.Model.equal r s1 = false

let join_value v1 v2 =
  let r = Cvalue.V.join v1 v2 in
  r, Cvalue.V.equal r v1 = false

let rec join_params l1 l2 = match l1, l2 with
  | [], [] -> ([], false)
  | [], l | l, [] ->
    Mt_self.warning "Joining parameters lists of different lengths";
    (l, true)
  | x::xs , y::ys ->
    let v, recv = join_value x y and lv, recl = join_params xs ys in
    v :: lv, recv || recl

let int_to_value i = Cvalue.V.inject_ival (Ival.of_int i)
