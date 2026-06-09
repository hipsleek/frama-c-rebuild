(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast
open Abstract_interp
open Cvalue

(** Enumeration *)

(** Cardinal of an abstract value (-1 if not enumerable). Beware this builtin
    is not monotonic *)
let frama_c_cardinal _state = function
  | [_, v] ->
    let nb = match Cvalue.V.cardinal v with
      | None -> Cvalue.V.inject_int Z.minus_one
      | Some i -> Cvalue.V.inject_int i
    in
    Builtins.Result [nb]
  | _ -> Kernel.abort ~current:true "Incorrect argument for Frama_C_cardinal"

let () = Builtins.register_builtin "Frama_C_abstract_cardinal" frama_c_cardinal

(** Minimum or maximum of an integer abstract value, Top_int otherwise.
    Also not monotonic. *)
let frama_c_min_max f _state = function
  | [_, v] ->
    let nb =
      try
        match f (Ival.min_and_max (V.project_ival v)) with
        | None -> Cvalue.V.top_int
        | Some i -> Cvalue.V.inject_int i
      with V.Not_based_on_null -> Cvalue.V.top_int
    in
    Builtins.Result [nb]
  | _ -> Kernel.abort ~current:true "Incorrect argument for Frama_C_min/max"

let () =
  Builtins.register_builtin "Frama_C_abstract_min" (frama_c_min_max fst);
  Builtins.register_builtin "Frama_C_abstract_max" (frama_c_min_max snd)

(** Splitting values *)

let warning warn s =
  if warn then
    Self.result ~level:3 ~current:true ~once:true s
  else
    Pretty_utils.nullprintf s

(* Split the contents of lv (by using multiple states), provided that [lv] is a
   singleton location with an arithmetic type, and that it contains no more than
   [max_card] elements. *)
let split_v ~warn lv state max_card =
  if Ast_types.is_scalar lv.typ then
    let loc = Cvalue_queries.lval_to_loc state lv in
    if Addresses.Bits.cardinal_zero_or_one loc.Locations.addr then
      let v_indet = Cvalue.Model.find_indeterminate state loc in
      let v = Cvalue.V_Or_Uninitialized.get_v v_indet in
      if V.is_bottom v then (* Alarm. *)
        [state] (* Cannot split, but cardinal '0' anyway *)
      else
        try
          ignore (V.cardinal_less_than v max_card);
          let aux_v v states =
            let v_indet =
              (* Restore original dangling/unitialized flags *)
              Cvalue.V_Or_Uninitialized.map (fun _ -> v) v_indet
            in
            let state' =
              Model.add_indeterminate_binding  ~exact:true state loc v_indet
            in
            state' :: states
          in
          V.fold_enum aux_v v []
        with Not_less_than ->
          warning warn "Location %a points to too many values (%a). \
                        Cannot split." Eva_ast.pp_lval lv V.pretty v;
          [state]
    else begin
      warning warn "Location %a is not a singleton (%a). Cannot split."
        Eva_ast.pp_lval lv Locations.pretty loc;
      [state]
    end
  else begin
    warning warn "Cannot split on lvalue %a of non-arithmetic type"
      Eva_ast.pp_lval lv;
    [state]
  end

(* For an lvalue '*p' or 'p->off', split the values of 'p'. Do not split
   anything else. *)
let split_pointer ~warn lv state max_card =
  match lv.node with
  | (Mem {node = Lval lv}, _) -> split_v ~warn lv state max_card
  | _ ->
    warning warn "cannot split on non-pointer %a" Eva_ast.pp_lval lv;
    [state]

(** The three functions below gather all lvalues with integral type that appear
    in an expression, an lvalue, or the offset of an lvalue (respectively). We
    use a recursive descent instead of a visitor because we want to impose an
    order to the visit. In particular, we want to see 'i' before 't[i]' when
    examining 't[i]+1', as it is important to proceed by case analysis on 'i'
    first, then on 't[i]'. *)
let rec gather_lv_in_exp acc e =
  match e.node with
  | Const _ -> acc
  | Lval lv | AddrOf lv | StartOf lv -> gather_lv_in_lv acc lv
  | UnOp (_, e, _) | CastE (_, e) -> gather_lv_in_exp acc e
  | BinOp (_, e1, e2, _) -> gather_lv_in_exp (gather_lv_in_exp acc e1) e2
and gather_lv_in_lv acc lv =
  let (host, offset) = lv.node in
  let acc =
    if Ast_types.is_scalar lv.typ
    then lv :: acc
    else acc
  in
  let acc = match host with
    | Var _ -> acc
    | Mem e -> gather_lv_in_exp acc e
  in
  let acc = gather_lv_in_offset acc offset in
  (* All variants (host, o) where [o] is a strict prefix of [offset]
     have type [union], [struct] or [array], thus we have covered all
     combinations *)
  acc
and gather_lv_in_offset acc offset =
  match offset with
  | NoOffset -> acc
  | Field (_, o) -> gather_lv_in_offset acc o
  | Index (e, o) -> gather_lv_in_offset (gather_lv_in_exp acc e) o

(** Split recursively all the lvalues that appear in [lv], including [lv]
    itself if possible. *)
let split_all ~warn lv state max_card =
  let lvs = gather_lv_in_lv [] lv in
  (* split all the lvalues in [lvs], in all the states in [states]. May create
     *many* states. *)
  let rec split lvs states =
    match lvs with
    | [] -> states
    | lv :: q ->
      let aux_state states state =
        let states_lv = split_v ~warn lv state max_card in
        states_lv @ states
      in
      let states = List.fold_left aux_state [] states in
      split q states
  in
  split lvs [state]

(* Auxiliary function, used to register a 'Frama_C_split' variant. Only the
   parsing and the error handling is shared; all the hard work is done by [f] *)
let aux_split f state = function
  | [({ node = (Lval lv | CastE (_, {node = Lval lv}))}, _); (_, card)] ->
    let states =
      try
        let max_card =
          Z.to_int (Ival.project_int (V.project_ival_bottom card))
        in
        f ~warn:true lv state max_card
      with V.Not_based_on_null | Ival.Not_Singleton_Int ->
        Self.warning ~current:true ~once:true
          "Cannot use non-constant split level %a" V.pretty card;
        [state]
    in
    Builtins.States states
  | _ ->
    Self.warning ~current:true ~once:true
      "Cannot interpret split directive. Ignoring";
    Builtins.States [state]

let () =
  Builtins.register_builtin "Frama_C_builtin_split" (aux_split split_v);
  Builtins.register_builtin "Frama_C_builtin_split_pointer" (aux_split split_pointer);
  Builtins.register_builtin "Frama_C_builtin_split_all" (aux_split split_all)
