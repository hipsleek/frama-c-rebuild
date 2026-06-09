(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp
open Cvalue

type watch = Value of V.t | Cardinal of int

let equal_watch w1 w2 =
  match w1, w2 with
    Value v1, Value v2 -> V.equal v1 v2
  | Cardinal c1, Cardinal c2 -> c1 = c2
  | _ -> false

type watchpoint =
  { name_lv : Eva_ast.exp;
    loc: Locations.t;
    v: watch;
    mutable remaining_count: Z.t;
    mutable stmts: Cil_datatype.Stmt.Set.t }

let watch_table : watchpoint list ref = ref []

let new_watchpoint name_lv loc v n =
  { name_lv = name_lv;
    loc = loc;
    v = v;
    remaining_count = n;
    stmts = Cil_datatype.Stmt.Set.empty }

let add_watch make_watch state actuals =
  match actuals with
  | [(dst_e, dst); (_, size); (_, target_value); (_, number)] ->
    let size =
      try
        let size = Cvalue.V.project_ival size in
        Z.mul 8z (Ival.project_int size)
      with V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Builtins.Outside_builtin_possibilities
    in
    let number =
      try
        let number = Cvalue.V.project_ival number in
        Ival.project_int number
      with V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Builtins.Outside_builtin_possibilities
    in
    let addr_bits = Addresses.Bits.of_bytes dst in
    let loc = Locations.make addr_bits (`Value size) in
    let target_w = make_watch target_value in
    let current = !watch_table in
    if
      List.for_all
        (fun {loc=l; v=w} ->
           not (Locations.equal l loc && equal_watch w target_w))
        current
    then
      watch_table :=
        (new_watchpoint dst_e loc target_w number) :: current;
    Builtins.States [state]
  | _ -> raise (Builtins.Invalid_nb_of_args 4)

let make_watch_value target_value = Value target_value

let make_watch_cardinal target_value =
  try
    let target_value = Cvalue.V.project_ival target_value in
    Cardinal (Z.to_int (Ival.project_int target_value))
  with V.Not_based_on_null | Ival.Not_Singleton_Int
     | Z.Overflow (* from Z.to_int *) ->
    raise Builtins.Outside_builtin_possibilities

let () =
  Builtins.register_builtin "Frama_C_watch_value"
    (add_watch make_watch_value)
let () =
  Builtins.register_builtin "Frama_C_watch_cardinal"
    (add_watch make_watch_cardinal)

let watch_hook _callstack stmt states =
  let treat ({name_lv = name; loc=loc; v=wa; remaining_count=current; stmts=set} as w) =
    List.iter
      (fun state ->
         let vs = Model.find ~conflate_bottom:false state loc in
         let watching =
           match wa with
             Value v ->
             V.intersects vs v
           | Cardinal n ->
             ( try
                 ignore (V.cardinal_less_than vs n) ;
                 false
               with Not_less_than -> true)
         in
         if watching
         then begin
           Self.warning ~wkey:Self.wkey_watchpoint ~once:true ~current:true
             ~stacktrace:true
             "%a %a"
             Eva_ast.pp_exp name
             V.pretty vs;
           if Z.is_zero current ||
              (Cil_datatype.Stmt.Set.mem stmt set)
           then ()
           else
             let current = Z.pred current in
             if Z.is_zero current then
               Self.abort "Watchpoint builtin: countdown to zero";
             w.remaining_count <- current;
             w.stmts <- Cil_datatype.Stmt.Set.add stmt set;
         end)
      states
  in
  List.iter treat !watch_table

let () = Cvalue_callbacks.register_statement_hook watch_hook
