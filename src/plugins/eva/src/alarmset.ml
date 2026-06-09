(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module M = Alarms.Map

type alarm = Alarms.t
type status = Abstract_interp.Comp.result = True | False | Unknown
type s = status M.t
type t = Just of s | AllBut of s

type 'a if_consistent = [ `Value of 'a | `Inconsistent ]

let string_of_predicate_status = function
  | Unknown -> "unknown"
  | True -> "valid"
  | False -> "invalid"

let pretty_status fmt v =
  Format.fprintf fmt "%s" (string_of_predicate_status v)

module Status = struct
  include Datatype.Make_with_collections
      (struct
        type t = status
        include Datatype.Serializable_undefined
        let name = "Alarmset.status"
        let reprs = [ True; False; False; Unknown ]
        let mem_project = Datatype.never_any_project
        let pretty = pretty_status
        let compare (s1:t) (s2:t) = Stdlib.compare s1 s2
        let equal (s1:t) (s2:t) = s1 = s2
        let hash (s:t) = Hashtbl.hash s
      end)

  let join x y = match x, y with
    | True, True -> True
    | False, False -> False
    | True, False | False, True
    | Unknown, _ | _, Unknown -> Unknown

  let inter x y = match x, y with
    | Unknown, _ -> `Value y
    | _, Unknown -> `Value x
    | True, True -> `Value True
    | False, False -> `Value False
    | True, False | False, True -> `Inconsistent
end

module D = Alarms.Map.Make (Status)

let pretty fmt = function
  | Just m   -> Format.fprintf fmt "Just %a" D.pretty m
  | AllBut m -> Format.fprintf fmt "AllBut %a" D.pretty m

let none = Just M.empty
let all = AllBut M.empty

let equal a b = match a, b with
  | Just m, Just n
  | AllBut m, AllBut n -> M.equal Status.equal m n
  | _, _ -> false

let get = function Just m | AllBut m -> m

let is_empty = function
  | AllBut _ -> false
  | Just m -> M.is_empty m

let singleton ?(status=Unknown) a =
  if status = True then Just M.empty else Just (M.singleton a status)

let set alarm status = function
  | Just m   ->
    if status = True
    then Just (M.remove alarm m)
    else Just (M.add alarm status m)
  | AllBut m ->
    if status = Unknown
    then AllBut (M.remove alarm m)
    else AllBut (M.add alarm status m)

let default = function
  | Just _ -> True
  | AllBut _ -> Unknown

let find alarm set =
  try match set with
    | Just m
    | AllBut m -> M.find alarm m
  with Not_found -> default set

(* Merges two alarm maps [s1] and [s2], using [merge_status] to merge merge the
   statuses bound to a same alarm.

   If [combine] is true, both alarm maps [s1] and [s2] have been produced in
   the same state but for different expressions [e1] and [e2]: they may contain
   different alarms, but should always have the same status bound to a same
   alarm. Do not use the default status for alarms in [s1] missing in [s2] (and
   vice versa): such alarms may have no meaning for [e2], and should have the
   same status in [s2] than in [s1] anyway.

   If [combine] is false, both maps come from the evaluation of the same
   expression in different states. In this case, use the default status for any
   alarm missing in one side. *)
let merge ~combine merge_status s1 s2 =
  let d1 = default s1 and d2 = default s2 in
  let m1 = get s1 and m2 = get s2 in
  let closed_set = match merge_status d1 d2 with
    | True -> true
    | Unknown -> false
    | False -> assert false
  in
  let return = if closed_set
    then function True -> None | p -> Some p
    else function Unknown -> None | p -> Some p
  in
  let merge _ p1 p2 = match p1, p2 with
    | None, None -> assert false
    | Some p, None -> if combine then Some p else return (merge_status p d2)
    | None, Some p -> if combine then Some p else return (merge_status d1 p)
    | Some p1, Some p2 -> return (merge_status p1 p2)
  in
  if closed_set
  then Just (M.merge merge m1 m2)
  else AllBut (M.merge merge m1 m2)

let union = merge ~combine:false Status.join

exception Inconsistent
let intersect status1 status2 = match Status.inter status1 status2 with
  | `Value status -> status
  | `Inconsistent -> raise Inconsistent

let inter s1 s2 =
  try `Value (merge ~combine:false intersect s1 s2)
  with Inconsistent -> `Inconsistent

let combine s1 s2 =
  (* [intersect] is only applied if both maps explicitly contain the same
     alarm. As both maps have been produced in the same state, the statuses for
     this alarm should be consistent. *)
  try merge ~combine:true intersect s1 s2
  with Inconsistent ->
    Self.fatal ~current:true
      "Inconsistent combination of two alarm maps %a and %a."
      pretty s1 pretty s2

let iter f = function
  | Just m -> M.iter f m
  | AllBut _ -> assert false

let fold f acc = function
  | Just m -> M.fold f m acc
  | AllBut _ -> assert false

let exists test ~default = function
  | Just m -> M.exists test m || default True
  | AllBut m -> M.exists test m || default Unknown

let for_all test ~default = function
  | Just m -> M.for_all test m && default True
  | AllBut m -> M.for_all test m && default Unknown


(* --------------------------------------------------------------------------
                                 Alarms
     ------------------------------------------------------------------------ *)

let emitter = Eva_utils.emitter

(* Printer that shows additional information about temporaries *)
let local_printer: Printer.extensible_printer Lazy.t =
  let open Cil_types in lazy object (self)
    inherit Printer.extensible_printer () as super

    (* Temporary variables for which we want to print more information *)
    val mutable temporaries = Cil_datatype.Varinfo.Set.empty

    method! code_annotation fmt ca =
      temporaries <- Cil_datatype.Varinfo.Set.empty;
      match ca.annot_content with
      | AAssert (_, p) ->
        (* ignore the ACSL name *)
        let p = p.tp_statement.pred_content in
        Format.fprintf fmt "@[<v>@[assert@ %a;@]" self#predicate_node p;
        (* print temporary variables information *)
        if not (Cil_datatype.Varinfo.Set.is_empty temporaries) then begin
          Format.fprintf fmt "@ @[(%t)@]" self#pp_temporaries
        end;
        Format.fprintf fmt "@]";
      | _ -> assert false

    method private pp_temporaries fmt =
      let pp_var fmt vi =
        Format.fprintf fmt "%s from@ @[%s@]" vi.vname (Option.get vi.vdescr)
      in
      Pretty_utils.pp_iter Cil_datatype.Varinfo.Set.iter
        ~pre:"" ~suf:"" ~sep:",@ " pp_var fmt temporaries

    method! logic_var fmt lvi =
      (match lvi.lv_origin with
       | None | Some { vdescr = None }-> ()
       | Some ({ vdescr = Some _ } as vi) ->
         temporaries <- Cil_datatype.Varinfo.Set.add vi temporaries
      );
      super#logic_var fmt lvi
  end

let pr_annot fmt a = (Lazy.force local_printer)#code_annotation fmt a

(* Default behaviour: print one alarm per kinstr. *)
module Alarm_key = Datatype.Pair_with_collections (Cil_datatype.Kinstr) (Alarms)

module Alarm_cache =
  State_builder.Hashtbl (Alarm_key.Hashtbl) (Datatype.Unit)
    (struct
      let name = "Value_messages.Alarm_cache"
      let dependencies = [Self.state]
      let size = 35
    end)

(* Has the given alarm already been emitted by the current analysis?
   For now, alarms emitted at Kglobal are bound to the first statement of the
   main function by the kernel, so we also need to check this case. *)
let already_emitted stmt alarm =
  Alarm_cache.mem (Kstmt stmt, alarm)
  || Kernel_function.is_first_stmt (fst (Globals.entry_point ())) stmt
     && Alarm_cache.mem (Kglobal, alarm)

let loc = function
  | Cil_types.Kglobal -> (* can occur in case of obscure bugs (already happened)
                            with wacky initializers. Module Initial_state of
                            value analysis correctly positions the loc *)
    Current_loc.get ()
  | Cil_types.Kstmt s -> Cil_datatype.Stmt.loc s

let report_alarm ~pos annot msg =
  Self.warning ~wkey:Self.wkey_alarm ~pos ~stacktrace:true
    "@[%s.@ @[<hov 2>%a@]@]"
    msg pr_annot annot

let string_fkind fk = Format.asprintf "%a" Printer.pp_fkind fk

(** Emitting alarms *)

let register_alarm ~pos alarm status str =
  let ki = Position.kinstr pos in
  let status = match status with
    | True -> Property_status.True
    | False ->
      (* We cannot soundly emit a red status on an alarm, because we may have
         emitted a green status earlier on. Thus we store the information
         for logging purposes, and emit an Unknown status. *)
      Red_statuses.add_red_alarm ki alarm;
      Property_status.Dont_know
    | Unknown -> Property_status.Dont_know
  in
  let annot, _is_new =
    Alarms.register ~loc:(loc ki) ~status emitter ki alarm
  in
  (* Report each alarm only once per analysis. The boolean [is_new] returned
     by {{Alarms.register}} is inadequate, as an alarm emitted by another
     plugin or by a previous run of Eva would be considered as not new. *)
  Alarm_cache.memo (fun (_ki,_alarm) -> report_alarm ~pos annot str) (ki, alarm)

let emit_alarm ~pos alarm (status:status) =
  let register_alarm = register_alarm ~pos alarm status in
  match alarm with
  | Alarms.Pointer_comparison (_, _) -> register_alarm "pointer comparison"

  | Alarms.Division_by_zero _ -> register_alarm "division by zero"

  | Alarms.Overflow (kind, _, _, _) ->
    let str = match kind with
      | Alarms.Signed -> "signed overflow"
      | Alarms.Unsigned -> "unsigned overflow"
      | Alarms.Signed_downcast -> "signed downcast"
      | Alarms.Unsigned_downcast -> "unsigned downcast"
      | Alarms.Pointer_downcast -> "pointer downcast"
    in
    register_alarm str

  | Alarms.Float_to_int _ ->
    register_alarm "overflow in conversion from floating-point to integer"

  | Alarms.Invalid_shift (_, Some _) ->
    register_alarm "invalid RHS operand for shift"

  | Alarms.Invalid_shift (_, None) ->
    register_alarm "invalid LHS operand for left shift"

  | Alarms.Memory_access (_, access_kind) ->
    let access = match access_kind with
      | Alarms.For_reading -> "read"
      | Alarms.For_writing -> "write"
    in
    register_alarm (Format.sprintf "out of bounds %s" access)

  | Alarms.Index_out_of_bound _ ->
    register_alarm "accessing out of bounds index"

  | Alarms.Unaligned_pointer _ ->
    register_alarm "unaligned pointer creation"

  | Alarms.Invalid_pointer _ ->
    register_alarm "invalid pointer creation"

  | Alarms.Differing_blocks _ ->
    register_alarm "pointer subtraction"

  | Alarms.Is_nan_or_infinite (_, fkind) ->
    let sfkind = string_fkind fkind in
    register_alarm (Format.sprintf "non-finite %s value" sfkind)

  | Alarms.Is_nan (_, fkind) ->
    let sfkind = string_fkind fkind in
    register_alarm (Format.sprintf "NaN %s value" sfkind)

  | Alarms.Uninitialized _ ->
    register_alarm "accessing uninitialized left-value"

  | Alarms.Dangling _ ->
    register_alarm "accessing left-value that contains escaping addresses"

  | Alarms.Not_separated _ ->
    register_alarm "undefined multiple accesses in expression"

  | Alarms.Overlap _ ->
    register_alarm "partially overlapping lvalue assignment"

  | Alarms.Function_pointer _ ->
    register_alarm  "pointer to function with incompatible type"

  | Alarms.Invalid_bool _ ->
    register_alarm "trap representation of a _Bool lvalue"

let height_alarm = let open Eva_utils in function
    | Alarms.Division_by_zero e
    | Alarms.Index_out_of_bound (e,_)
    | Alarms.Unaligned_pointer (e,_)
    | Alarms.Invalid_pointer e
    | Alarms.Invalid_shift (e,_)
    | Alarms.Overflow (_,e,_,_)
    | Alarms.Float_to_int (e,_,_)
    | Alarms.Function_pointer (e, _)
    | Alarms.Pointer_comparison (None,e) -> height_expr e + 2
    | Alarms.Memory_access (lv,_)
    | Alarms.Dangling lv
    | Alarms.Invalid_bool lv -> height_lval lv + 1
    | Alarms.Uninitialized lv -> height_lval lv
    | Alarms.Pointer_comparison (Some e1, e2) ->
      max (height_expr e1) (height_expr e2) + 2
    | Alarms.Differing_blocks (e1, e2) ->
      max (height_expr e1) (height_expr e2) + 1
    | Alarms.Not_separated (lv1,lv2)
    | Alarms.Overlap (lv1,lv2) -> max (height_lval lv1) (height_lval lv2) + 1
    | Alarms.Is_nan_or_infinite (e, fkind)
    | Alarms.Is_nan (e, fkind) ->
      let trivial = match Cil.(typeOf e).tnode with
        | TFloat fk -> fk = fkind
        | _ -> false
      in
      if trivial then height_expr e else height_expr e + 1

let cmp a1 a2 =
  Datatype.Int.compare (height_alarm (fst a1)) (height_alarm (fst a2))

let remove_redundant_alarms map =
  let filter alarm status =
    match alarm with
    | Alarms.Invalid_pointer expr ->
      let lval = Mem expr, NoOffset in
      let implies alarm s =
        status = s &&
        match alarm with
        | Alarms.Memory_access (lv, _access_kind) ->
          Cil_datatype.LvalStructEq.equal lv lval
        | _ -> false
      in
      not (M.exists implies map)
    | _ -> true
  in
  M.filter filter map

let emit_alarms ~pos map =
  let map = remove_redundant_alarms map in
  let list = M.bindings map in
  let sorted_list = List.sort cmp list in
  List.iter (fun (alarm, status) -> emit_alarm ~pos alarm status) sorted_list;
  if Alarm_cache.length () >= Parameters.StopAtNthAlarm.get ()
  then Self.abort "Stopping at nth alarm"

let emit ~pos = function
  | Just map -> if not (M.is_empty map) then emit_alarms ~pos map
  (* TODO: use GADT to avoid this assert false ? *)
  | AllBut  _ ->
    Self.abort ~current:true ~once:true
      "All alarms may arise: \
       abstract state too imprecise to continue the analysis."
