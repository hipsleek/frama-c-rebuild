(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Abstract_interp
open Cvalue
open Lattice_bounds

let dkey = Self.register_category "malloc" ~level:6
    ~help:"messages from the builtins interpreting dynamic allocations"

let dkey_new = Self.register_category "malloc:new" ~level:3
    ~help:"messages emitted at the creation of new bases"

let dkey_auto_free = Self.register_category "malloc:automatic-free" ~level:4
    ~help:"messages emitted when bases are automatically freed (alloca or VLA)"

let _wkey_malloc =
  Self.register_warn_category "malloc"
    ~help:"warnings related to builtins interpreting dynamic allocation"

let wkey_weak_alloc =
  Self.register_warn_category "malloc:weak"
    ~help:"a same \"weak\" variable is used to represent multiple dynamic \
           allocations, which is rather imprecise"
    ~default:Inactive

let wkey_imprecise_alloc =
  Self.register_warn_category "malloc:imprecise"
    ~help:"a single \"weak\" variable is used to represent all dynamic \
           allocations, which is very imprecise"

(* ---------------------- Dynamically allocated bases ----------------------- *)


module Hptmap_Info = struct
  let initial_values = []
  let dependencies = [ Ast.self ]
end

module Base_hptmap = Hptmap.Make (Base.Base) (Callstack) (Hptmap_Info)

module Dynamic_Alloc_Bases =
  State_builder.Ref
    (Base_hptmap)
    (struct
      let dependencies = [Ast.self] (* TODO: should probably depend on Value
                                       itself *)
      let name = "Value.Builtins_malloc.Dynamic_Alloc_Bases"
      let default () = Base_hptmap.empty
    end)
let () = Ast.add_monotonic_state Dynamic_Alloc_Bases.self

(* -------------------------- Auxiliary functions  -------------------------- *)

let current_call_site () =
  match Callstack.top_callsite (Current_callstack.get_exn ()) with
  | Kglobal -> Cil_datatype.Stmt.dummy
  | Kstmt stmt -> stmt

(* Remove some parts of the callstack:
   - Remove the bottom of the call tree until we get to the call site
     of the call to the first malloc function. The idea is that each of
     these call site correspond to a different use of a malloc function,
     so it is interesting to keep their bases separated. *)
let call_stack_no_wrappers () =
  let cs = Current_callstack.get_exn () in
  let wrappers = Parameters.AllocFunctions.get () in
  let rec bottom_filter = function
    | [] | [_] as stack -> stack
    | (kf,_)::((kf', _):: _ as rest) as stack ->
      if Datatype.String.Set.mem (Kernel_function.get_name kf) wrappers
      && Datatype.String.Set.mem (Kernel_function.get_name kf') wrappers
      then bottom_filter rest
      else stack
  in
  { cs with stack = bottom_filter cs.stack }

let register_malloced_base ~stack b =
  let stack_without_top =
    Option.value ~default:stack (Callstack.pop stack)
  in
  Dynamic_Alloc_Bases.set
    (Base_hptmap.add b stack_without_top (Dynamic_Alloc_Bases.get ()))

let fold_dynamic_bases (f: Base.t -> Callstack.t -> 'a -> 'a) init =
  Base_hptmap.fold f (Dynamic_Alloc_Bases.get ()) init

let is_automatically_deallocated base =
  match base with
  | Base.Allocated (_, (Base.Alloca | Base.VLA), _) -> true
  | Base.Allocated (_, Base.Malloc, _)
  | Base.Var _
  | Base.CLogic_Var _
  | Base.Null -> false

(* Extracts the minimum/maximum sizes (in bytes) for malloc/realloc/calloc,
   respecting the bounds of size_t.
   Note that the value returned for maximum size corresponds to one past
   the last valid index. *)
let extract_size sizev_bytes =
  let max = Bit_utils.max_byte_size () in
  try
    let sizei_bytes = Cvalue.V.project_ival sizev_bytes in
    begin match Ival.min_and_max sizei_bytes with
      | Some smin, Some smax ->
        assert (Z.(geq smin zero));
        smin, Z.min smax max
      | _ -> assert false (* Cil invariant: cast to size_t *)
    end
  with V.Not_based_on_null -> (* size is a garbled mix *)
    Z.zero, max

(* Name of the base that will be given to a malloced variable, determined
   using the callstack. *)
let base_name prefix cs =
  let stmt_line stmt = Cil_datatype.(Stmt.loc stmt |> Fileloc.line) in
  match cs.Callstack.stack with
  | [] ->
    (* Degenerate case *)
    Format.asprintf "__%s_%a" prefix Kernel_function.pretty cs.entry_point
  | (_, callsite) :: qstack ->
    (* Use the whole call-stack to generate the name *)
    let rec loop_full = function
      | [] -> Format.sprintf "_%s" (Kernel_function.get_name cs.entry_point)
      | (kf, line) :: b ->
        let line = stmt_line line in
        let node_str = Format.asprintf "_l%d__%a"
            line Kernel_function.pretty kf
        in
        (loop_full b) ^ node_str
    in
    (* Use only the name of the caller to malloc for the name *)
    let caller =
      let kf = Callstack.top_kf { cs with stack = qstack } in
      Format.asprintf "_%a" Kernel_function.pretty kf
    in
    let full_name = false in
    Format.asprintf "__%s%s_l%d"
      prefix
      (if full_name then loop_full qstack else caller)
      (stmt_line callsite)

type var = Weak | Strong

let create_new_var stack prefix type_base weak =
  let prefix = match weak with
    | Weak -> prefix ^ "_w"
    | Strong -> prefix
  in
  let name = Cabs2cil.fresh_global (base_name prefix stack) in
  let alignas = Machine.Alignof.max () in
  Eva_utils.create_new_var ~alignas name type_base

(* This function adds a "_w" information to a variable. It should be used
   when a variable becomes weak, and supposes that the variable has been
   created by one of the functions of this module. Mutating variables name
   is not a good idea in general, but we take the risk here. *)
let mutate_name_to_weak vi =
  Self.warning ~wkey:wkey_weak_alloc ~current:true ~once:false ~stacktrace:true
    "@[marking variable `%s' as weak@]" vi.vname;
  try
    let prefix, remainder =
      Scanf.sscanf vi.vname "__%s@_%s" (fun s1 s2 -> (s1, s2))
    in
    let name' = Printf.sprintf "__%s_w_%s" prefix remainder in
    vi.vname <- name'
  with End_of_file | Scanf.Scan_failure _ -> ()

(* This type represents the size requested to malloc/realloc and co. *)
type typed_size = {
  min_bytes: Z.t (* minimum size requested, in bytes *);
  max_bytes: Z.t (* maximum size requested, in bytes *);
  elem_typ: typ (* "guessed type" for the elements of the new variable *);
  nb_elems: Z.t option (* number of elements of size [sizeof(elem_typ)].
                                None if [min<>max] *);
}

(* Guess the intended type for the cell returned by malloc, given [sizev ==
   [size_min .. size_max] (in bytes). We look for [T *v = malloc(foo)], then
   check that [size_min] and [size_max] are multiples of [sizeof(T)].
   Note that [sizeof(T)] can be zero (e.g. an empty struct).
   If no information can be found, we use char for the base type. If the size
   cannot change later ([constant_size]), we also compute the number of
   elements that are allocated. *)
(* TODO: this is not perfect because we can have an overflow in computations
   such as [foo * t = malloc (i * sizeof(foo))] *)
let guess_intended_malloc_type stack sizev constant_size =
  let size_min, size_max = extract_size sizev in
  let nb_elems elem_size =
    if constant_size && Z.equal size_min size_max
    then Some Z.(if is_zero elem_size then zero else ediv size_min elem_size)
    else None
  in
  let mk_typed_size t =
    match Ast_types.unroll_node t with
    | TPtr t when not (Ast_types.is_void t) ->
      let s = Z.of_int (Cil.bytesSizeOf t) in
      if Z.(is_zero s ||
            (is_zero (erem size_min s) &&
             is_zero (erem size_max s)))
      then
        { min_bytes = size_min; max_bytes = size_max;
          elem_typ = t; nb_elems = nb_elems s }
      else raise Exit
    | _ -> raise Exit
  in
  try
    match Callstack.top_callsite stack with
    | Kstmt {skind = Instr (Call (Some lv, _, _, _))} ->
      mk_typed_size (Cil.typeOfLval lv)
    | Kstmt {skind = Instr(Local_init(vi, _, _))} -> mk_typed_size vi.vtype
    | _ -> raise Exit
  with Exit | Cil.SizeOfError _ -> (* Default, use char *)
    { min_bytes = size_min; max_bytes = size_max; elem_typ = Cil_const.charType;
      nb_elems = nb_elems Z.one }

(* Helper function to create the best type for a new base.  Builds an
   array type with the appropriate number of elements if needed.  When
   the number of elements cannot be determined, build an array with
   imprecise size. This is not a problem in practice, because in C you
   annot obtain the size of an allocated block, and \block_length
   handles Allocated variables through their validity. *)
let type_from_nb_elems tsize =
  let typ = tsize.elem_typ in
  match tsize.nb_elems with
  | None -> Cil_const.mk_tarray typ None
  | Some nb ->
    if Z.is_one nb
    then typ
    else
      let loc = Current_loc.get () in
      let esize_arr = Cil.kinteger64 ~loc nb in (* [nb] fits in size_t *)
      Cil_const.mk_tarray typ (Some esize_arr)

(* Generalize a type into an array type without size. Useful for variables
   whose size is mutated. *)
let weaken_type typ =
  match Ast_types.unroll typ with
  | { tnode = TArray (_, None) } -> typ
  | { tnode = TArray (typ, Some _) }
  | typ ->
    Cil_const.mk_tarray typ None

(* size for which the base is certain to be valid *)
let size_sure_valid b = match Base.validity b with
  | Base.Invalid | Base.Empty | Base.Unknown (_, None, _) -> Z.zero
  | Base.Known (_, up) | Base.Unknown (_, Some up, _)
  | Base.Variable { Base.min_alloc = up } -> Z.succ up

(* Create a new offsetmap initialized to [bottom] on the entire allocable
   range, with the first [max_alloc] bits set to [v].
   [v] must be an isotropic value. *)
let offsm_with_v v validity max_alloc =
  let size = Bottom.non_bottom (V_Offsetmap.size_from_validity validity) in
  let offsm = V_Offsetmap.create_isotropic ~size V_Or_Uninitialized.bottom in
  (* max_alloc is -1 when allocating an empty base *)
  if Z.(lt max_alloc zero) then
    (* malloc(0) => nothing to uninitialize *)
    offsm
  else (* malloc(i > 0) => uninitialize i bytes *)
    V_Offsetmap.add ~exact:true (Z.zero, max_alloc)
      (v, Z.one, Rel.zero) offsm

(* add [v] as a possible value for the bits [0..max_valid_bits] of
   [base] in [state].
   [v] must be an isotropic value. *)
let add_v v state base max_valid_bits =
  let validity = Base.validity base in
  let offsm = offsm_with_v v validity max_valid_bits in
  let new_offsm =
    try
      let cur = match Model.find_base_or_default base state with
        | `Top -> assert false (* Value never passes Top as state *)
        | `Bottom -> assert false (* offsm_with_v never returns Bottom *)
        | `Value m -> m
      in
      V_Offsetmap.join offsm cur
    with Not_found -> offsm
  in
  Model.add_base base new_offsm state

let add_uninitialized = add_v V_Or_Uninitialized.uninitialized
let add_zeroes = add_v (V_Or_Uninitialized.initialized Cvalue.V.singleton_zero)

(* Applies the possibility of failure when allocating/reallocating a base.
   [ret]: result in case of success (e.g. a new base in case of malloc);
   [orig_state]: state before any allocation, returned in case of failure;
   [state_after_alloc]: state in case the allocation is successful;
   [returns_null]: if given, forces the result to consider/ignore the
   possibility of failure, despite -eva-alloc-returns-null. *)
let wrap_fallible_alloc ?returns_null ret orig_state state_after_alloc =
  let default_returns_null = Parameters.AllocReturnsNull.get () in
  let returns_null = Option.value ~default:default_returns_null returns_null in
  let success = Some ret, state_after_alloc in
  if returns_null
  then
    let failure = Some Cvalue.V.singleton_zero, orig_state in
    [ success ; failure ]
  else [ success ]

let pp_validity fmt (v1, v2) =
  if Z.equal v1 v2
  then Format.fprintf fmt "0..%a" Z.pretty v1
  else Format.fprintf fmt "0..%a/%a" Z.pretty v1 Z.pretty v2


(* --------------------------------- Malloc --------------------------------- *)

(* Create a new variable of size [sizev] with deallocation type [deallocation].
   Returns the new base, and its maximum validity.
   Note that [_state] is not used, but it is present to ensure a compatible
   signature with [alloc_by_stack]. *)
let alloc_fresh weak deallocation prefix sizev _state =
  let stack = call_stack_no_wrappers () in
  let tsize = guess_intended_malloc_type stack sizev (weak = Strong) in
  let type_base = type_from_nb_elems tsize in
  let var = create_new_var stack prefix type_base weak in
  Self.result ~dkey:dkey_new ~current:true ~once:true ~stacktrace:true
    "@[allocating %svariable %a@]"
    (if weak = Weak then "weak " else "") Printer.pp_varinfo var;
  let size_char = Bit_utils.sizeofchar () in
  (* Sizes are in bits *)
  let min_alloc = Z.(pred (mul size_char tsize.min_bytes)) in
  let max_alloc = Z.(pred (mul size_char tsize.max_bytes)) in
  (* NOTE: min_alloc/max_alloc may be -1 if the size is zero *)
  assert Z.(geq min_alloc Z.minus_one);
  assert Z.(geq max_alloc min_alloc);
  (* note that min_alloc may be negative (-1) if the allocated size is 0 *)
  let weak = match weak with Weak -> true | Strong -> false in
  let variable_v = Base.create_variable_validity ~weak ~min_alloc ~max_alloc in
  let validity = Base.Variable variable_v in
  let new_base = Base.register_allocated_var var deallocation validity in
  register_malloced_base ~stack new_base;
  new_base, max_alloc, Builtins.NoCacheCallers


let string_of_region = function
  | Base.Malloc -> "via malloc/calloc/realloc"
  | Base.VLA -> "related to variable-length arrays"
  | Base.Alloca -> "via alloca"

(* Only called when the 'weakest base' needs to be allocated. *)
let create_weakest_base region =
  let stack = { (Current_callstack.get_exn ()) with stack = [] } in
  let type_base = Cil_const.(mk_tarray charType None) in
  let var = create_new_var stack "alloc" type_base Weak in
  Self.warning ~wkey:wkey_imprecise_alloc ~current:true ~once:true
    "allocating a single weak variable for ALL dynamic allocations %s: %a"
    (string_of_region region) Printer.pp_varinfo var;
  let min_alloc = Z.minus_one in
  let max_alloc = Bit_utils.max_bit_address () in
  let variable_v =
    Base.create_variable_validity ~weak:true ~min_alloc ~max_alloc
  in
  let validity = Base.Variable variable_v in
  let new_base = Base.register_allocated_var var region validity in
  register_malloced_base ~stack new_base;
  new_base, max_alloc

module Base_with_Size = Datatype.Pair (Base.Base) (Z)

(* Memoize the creation of the weakest base for [region]. Returns a function
   returning the base, its size, and the suitable cache status: NoCacheCallers
   if the base is new, Cacheable otherwise. *)
let memo_weakest_base name region =
  let module Info = struct
    let name = "Eva.Builtins_malloc." ^ name
    let dependencies = [ Self.state ]
  end in
  let module M = State_builder.Option_ref (Base_with_Size) (Info) in
  fun () ->
    match M.get_option () with
    | Some (base, size) -> base, size, Builtins.Cacheable
    | None ->
      let base, size = create_weakest_base region in
      M.set (base, size);
      base, size, Builtins.NoCacheCallers

let malloc_weakest_base = memo_weakest_base "SingeMalloc" Base.Malloc
let vla_weakest_base = memo_weakest_base "SingleVLA" Base.VLA
let alloca_weakest_base = memo_weakest_base "SingleAlloca" Base.Alloca

(* Returns the single base representing the given region, its size and the
   suitable cache status. *)
let alloc_weakest_base = function
  | Base.Malloc -> malloc_weakest_base ()
  | Base.VLA -> vla_weakest_base ()
  | Base.Alloca -> alloca_weakest_base ()

(* Variables that have been returned by a call to an allocation function
   at this callstack. The first allocated variable is at the top of the
   stack. Currently, the callstacks are truncated according to
   [-eva-alloc-functions]. *)
module MallocedByStack = (* varinfo list Callstack.hashtbl *)
  State_builder.Hashtbl (Callstack.Hashtbl)
    (Datatype.List (Base))
    (struct
      let name = "Value.Builtins_malloc.MallocedByStack"
      let size = 17
      let dependencies = [Ast.self]
    end)
let () = Ast.add_monotonic_state MallocedByStack.self

(* Performs an abstract allocation on an existing allocated variable,
   its validity. If [make_weak], the variable is marked as being weak. *)
let update_variable_validity ?(make_weak=false) base sizev =
  let size_min, size_max = extract_size sizev in
  match base with
  | Base.Allocated (vi, _deallocation, (Base.Variable variable_v)) ->
    if make_weak && (variable_v.Base.weak = false) then
      mutate_name_to_weak vi;
    let min_sure_bits = Z.(pred (mul (of_int 8) size_min)) in
    let max_valid_bits = Z.(pred (mul (of_int 8) size_max)) in
    if not (Z.equal variable_v.Base.min_alloc min_sure_bits) ||
       not (Z.equal variable_v.Base.max_alloc max_valid_bits)
    then begin
      Self.result ~dkey ~current:true ~once:false
        "@[resizing variable `%a'@ (%a) to fit %a@]"
        Printer.pp_varinfo vi
        pp_validity (variable_v.Base.min_alloc, variable_v.Base.max_alloc)
        pp_validity (min_sure_bits, max_valid_bits);
      (* Mutating the type of a varinfo is not exactly a good idea. This is
         probably fine here, because the type of a malloced variable is
         almost never used. *)
      Cil.update_var_type vi (weaken_type vi.vtype);
    end;
    Base.update_variable_validity variable_v
      ~weak:make_weak ~min_alloc:min_sure_bits ~max_alloc:max_valid_bits;
    base, max_valid_bits, Builtins.NoCacheCallers
  | _ -> Self.fatal "base is not Allocated: %a" Base.pretty base

let alloc_by_stack region prefix sizev state =
  let stack = call_stack_no_wrappers () in
  let max_level = Parameters.MallocLevel.get () in
  let all_vars =
    try MallocedByStack.find stack
    with Not_found -> []
  in
  let rec aux nb vars =
    match vars with
    | [] -> (* must allocate a new variable *)
      let b, _, _ as r = alloc_fresh Strong region prefix sizev state in
      MallocedByStack.replace stack (all_vars @ [b]);
      r
    | b :: q ->
      try
        ignore (Model.find_base b state);
        if nb = max_level then begin (* variable already used *)
          update_variable_validity ~make_weak:true b sizev
        end
        else aux (nb+1) q
      with Not_found -> (* Can reuse this (strong) variable *)
        update_variable_validity ~make_weak:false b sizev
  in
  aux 0 all_vars

let choose_base_allocation () =
  match Eva_annotations.get_allocation (current_call_site ()) with
  | Fresh -> alloc_fresh Strong
  | Fresh_weak -> alloc_fresh Weak
  | By_stack -> alloc_by_stack
  | Imprecise -> fun region _ _ _ -> alloc_weakest_base region

let register_malloc ?replace name ?returns_null prefix region =
  let builtin state args =
    let size = match args with
      | [ _, size ] -> size
      | _ -> raise (Builtins.Invalid_nb_of_args 1)
    in
    let alloc_base = choose_base_allocation () in
    let new_base, max_alloc, cacheable = alloc_base region prefix size state in
    let new_state = add_uninitialized state new_base max_alloc in
    let ret = V.inject new_base Ival.zero in
    let c_values = wrap_fallible_alloc ?returns_null ret state new_state in
    let c_clobbered = Base.SetLattice.bottom in
    Builtins.Full { c_values; c_clobbered; c_assigns = None; cacheable; }
  in
  let name = "Frama_C_" ^ name in
  let typ () = Cil_const.voidPtrType, [Machine.sizeof_type ()] in
  Builtins.register_builtin ?replace name builtin ~typ

let () =
  register_malloc ~replace:"malloc" "malloc" "malloc" Base.Malloc;
  register_malloc ~replace:"__fc_vla_alloc" "vla_alloc" "malloc" Base.VLA
    ~returns_null:false;
  register_malloc ~replace:"alloca" "alloca" "alloca" Base.Alloca
    ~returns_null:false

(* --------------------------------- Calloc --------------------------------- *)

let zero_to_max_bytes () = Ival.inject_range
    (Some Z.zero) (Some (Bit_utils.max_byte_size ()))

let alloc_size_ok intended_size =
  try
    let size = Cvalue.V.project_ival intended_size in
    let ok_size = zero_to_max_bytes () in
    if Ival.is_included size ok_size then Alarmset.True
    else if Ival.intersects size ok_size then Alarmset.Unknown
    else Alarmset.False
  with Cvalue.V.Not_based_on_null -> Alarmset.Unknown (* garbled mix in size *)

let calloc_builtin state args =
  let nmemb, sizev =
    match args with
    | [(_, nmemb); (_, size)] -> nmemb, size
    | _ -> raise (Builtins.Invalid_nb_of_args 2)
  in
  let size = Cvalue.V.mul nmemb sizev in
  let size_ok = alloc_size_ok size in
  if size_ok <> Alarmset.True then
    Self.warning ~current:true ~once:true
      "calloc out of bounds: assert(nmemb * size <= SIZE_MAX)";
  let c_clobbered = Base.SetLattice.bottom in
  let c_assigns = None in
  if size_ok = Alarmset.False (* size always overflows *)
  then
    let c_values = [ Some Cvalue.V.singleton_zero, state ] in
    let cacheable = Builtins.Cacheable in
    Builtins.Full { c_values; c_clobbered; c_assigns; cacheable; }
  else
    let alloc_base = choose_base_allocation () in
    let base, max_valid, cacheable = alloc_base Malloc "calloc" size state in
    let new_state = add_zeroes state base max_valid in
    let returns_null =
      if size_ok = Alarmset.Unknown then Some true else None
    in
    let ret = V.inject base Ival.zero in
    let c_values = wrap_fallible_alloc ?returns_null ret state new_state in
    Builtins.Full { c_values; c_clobbered; c_assigns; cacheable; }

let () =
  let name = "Frama_C_calloc" in
  let replace = "calloc" in
  let typ () =
    let sizeof_typ = Machine.sizeof_type () in
    Cil_const.voidPtrType, [ sizeof_typ; sizeof_typ ]
  in
  Builtins.register_builtin ~replace name calloc_builtin ~typ

(* ---------------------------------- Free ---------------------------------- *)

(* Change all references to bases into ESCAPINGADDR into the given state,
   and remove those bases from the state entirely when [exact] holds *)
let free ~exact bases state =
  let changed = ref Memory_zone.bottom in
  (* Uncomment this code to simulate the fact that free "writes" the bases
     it deallocates
     Base_hptmap.iter (fun b ->
      changed := Memory_zone.join !changed (enumerate_bits (loc_of_base b))
     ) bases; *)
  (* No need to remove the freed bases from the state if [exact] is false,
     because they must remain for the 'inexact' case *)
  let state =
    if exact then Base.Hptset.fold Cvalue.Model.remove_base bases state
    else state
  in
  let escaping = bases in
  let on_escaping ~b ~itv ~v:_ =
    let z = Memory_zone.inject b (Int_Intervals.inject_itv itv) in
    changed := Memory_zone.join !changed z
  in
  let within = Base.SetLattice.top in
  let state =
    Locals_scoping.make_escaping ~exact ~escaping ~on_escaping ~within state
  in
  let from_changed =
    let m = Assigns.Memory.(add_binding ~exact empty !changed Deps.bottom) in
    Assigns.{ memory = m; return = Deps.bottom }
  in
  state, (from_changed, if exact then !changed else Memory_zone.bottom)

let freeable arg =
  (* Categorizes the bases in arg *)
  let f base offset (all_ok, one_ok) =
    if Base_hptmap.mem base (Dynamic_Alloc_Bases.get ()) &&
       not (is_automatically_deallocated base)
    then
      all_ok && Ival.is_zero offset,
      one_ok || Ival.contains_zero offset
    else (false, one_ok)
  in
  match Cvalue.V.fold_topset_ok f arg (true, false) with
  | true, true -> True
  | false, true -> Unknown
  | false, false -> False
  | true, false ->
    assert (V.is_bottom arg); True


let resolve_bases_to_free arg =
  (* Categorizes the bases in arg *)
  let f base offset (acc, card, null) =
    let allocated_base = Base_hptmap.mem base (Dynamic_Alloc_Bases.get ()) in
    (* Collect the bases to remove from the memory state.
       Also count the number of freeable bases (including NULL). *)
    if Ival.contains_zero offset
    then begin
      let base_card = match Base.validity base with
        | Base.Variable { Base.weak = true } -> 2
        (* weak validity has "infinite" cardinality; but here we use 2 since
           any value > 1 leads to a weak update anyway *)
        | _ -> 1
      in
      if allocated_base
      then Base.Hptset.add base acc, card + base_card, null
      else if Base.is_null base
      then acc, card + base_card, true
      else acc, card, null
    end
    else acc, card, null
  in
  Cvalue.V.fold_topset_ok f arg (Base.Hptset.empty, 0, false)

let free_aux state ~strong bases_to_remove  =
  (* TODO: reduce on arg if it is an lval *)
  if strong then begin
    Self.debug ~current:true ~dkey "strong free on bases: %a"
      Base.Hptset.pretty bases_to_remove;
    free ~exact:true bases_to_remove state
  end else begin
    Self.debug ~current:true ~dkey "weak free on bases: %a"
      Base.Hptset.pretty bases_to_remove;
    free ~exact:false bases_to_remove state
  end

(* Builtin for [free] function *)
let frama_c_free state actuals =
  match actuals with
  | [ _, arg ] ->
    let bases_to_remove, card_to_remove, _null = resolve_bases_to_free arg in
    let c_clobbered = Base.SetLattice.bottom in
    let cacheable = Builtins.Cacheable in
    if card_to_remove = 0 then
      Builtins.Full { c_values = []; c_clobbered; c_assigns = None; cacheable; }
    else
      let strong = card_to_remove <= 1 in
      let state, changed = free_aux state ~strong bases_to_remove in
      let c_values = [None, state] in
      let c_assigns = Some changed in
      Builtins.Full { c_values; c_clobbered; c_assigns; cacheable; }
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () =
  Builtins.register_builtin ~replace:"free" "Frama_C_free"
    frama_c_free ~typ:(fun () -> (Cil_const.voidType, [Cil_const.voidPtrType]))

(* built-in for [__fc_vla_free] function. By construction, VLA should always
   be mapped to a single base. *)
let frama_c_vla_free state actuals =
  match actuals with
  | [ _, arg ] ->
    let bases_to_remove, _card_to_remove, _null = resolve_bases_to_free arg in
    let state, changed = free_aux state ~strong:true bases_to_remove in
    let c_values = [None, state] in
    let c_clobbered = Base.SetLattice.bottom in
    let c_assigns = Some changed in
    let cacheable = Builtins.Cacheable in
    Builtins.Full { c_values; c_clobbered; c_assigns; cacheable; }
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () =
  Builtins.register_builtin
    ~replace:"__fc_vla_free" "Frama_C_vla_free" frama_c_vla_free
    ~typ:(fun () -> (Cil_const.voidType, [Cil_const.voidPtrType]))

let free_automatic_bases stack state =
  (* free automatic bases that were allocated in the current function *)
  let bases_to_free =
    Base_hptmap.fold (fun base stack' acc ->
        if is_automatically_deallocated base &&
           Callstack.equal stack stack'
        then Base.Hptset.add base acc
        else acc
      ) (Dynamic_Alloc_Bases.get ()) Base.Hptset.empty
  in
  if Base.Hptset.is_empty bases_to_free then state
  else begin
    Self.result ~dkey:dkey_auto_free ~current:true ~once:true
      "freeing automatic bases: %a" Base.Hptset.pretty bases_to_free;
    let state', _changed = free_aux state ~strong:true bases_to_free in
    (* TODO: propagate 'freed' bases for From? *)
    state'
  end

(* -------------------------------- Realloc --------------------------------- *)

(* Note: realloc never fails during read/write operations, hence we can
   always ignore the validity of locations. (We craft them ourselves anyway.)
   The only possible cause of failure is a pointer that was not malloced. *)

(* Auxiliary function for [realloc], that copies the [size] first bytes of
   [b] (or less if [b] is too small) in [src_state], then pastes them in
   [new_base] in [dst_state], which is supposed to be big enough for [size].
   This function always perform weak updates, in case multiple bases are
   copied to [new_base]. *)
let realloc_copy_one size ~src_state ~dst_state new_base b =
  let size_char = Bit_utils.sizeofchar () in
  let size_bits = Z.mul size size_char in
  let up = match Base.validity b with
    | Base.Known (_, up) | Base.Unknown (_, _, up)
    | Base.Variable { Base.max_alloc = up } -> up
    | Base.Invalid | Base.Empty -> Z.zero

  in
  let size_to_copy = Z.min (Z.succ up) size_bits in
  let src = Addresses.Bits.inject b Ival.zero in
  match Cvalue.Model.copy_offsetmap src size_to_copy src_state with
  | `Bottom -> assert false
  | `Value offsetmap ->
    if Z.gt size_to_copy Z.zero then
      Cvalue.Model.paste_offsetmap
        ~from:offsetmap ~dst_addr:new_base ~size:size_to_copy
        ~exact:false dst_state
    else dst_state

(* Auxiliary function for [realloc], that performs the allocation of a new
   variable, and copy the pointers being reallocated inside the new base.
   [size] is the size to realloc. [bases_to_realloc] are the pointers to the
   memory to copy. [null_in_arg] indicates that [realloc] was called with
   [null] in its argument. [weak] indicates which type of variable must
   be created: if [Weak], convergence is ensured using a malloc builtin
   that converges.  If [Strong], a new base is created for each call. *)
let realloc_alloc_copy weak bases_to_realloc null_in_arg sizev state =
  Self.debug ~dkey "bases_to_realloc: %a"
    Base.Hptset.pretty bases_to_realloc;
  assert (not (Model.(equal state bottom || equal state top)));
  let _size_valid, size_max = extract_size sizev in (* bytes everywhere *)
  let base, max_valid, cacheable =
    let prefix = "realloc" in
    match weak with
    | Strong -> alloc_fresh Strong Base.Malloc prefix sizev state
    | Weak -> alloc_by_stack Base.Malloc prefix sizev state
  in
  (* Make sure that [ret] will be present in the result: we bind it at least
     to bottom everywhere *)
  let dst_state = add_uninitialized state base Z.minus_one in
  let ret = V.inject base Ival.zero in
  let addr_bits = Addresses.Bits.of_bytes ret in
  (* get bases to free and copy *)
  let lbases = Base.Hptset.elements bases_to_realloc in
  let dst_state =
    (* uninitialized on all reallocated valid bits *)
    let uninit = V_Or_Uninitialized.uninitialized in
    let offsm = offsm_with_v uninit (Base.validity base) max_valid in
    let offsm =
      if null_in_arg then offsm (* In this case, realloc may copy nothing *)
      else
        (* Compute the maximal size that is guaranteed to be copied across all
           bases *)
        let aux_valid size b = Z.min size (size_sure_valid b) in
        let size_new_loc = Z.mul size_max (Bit_utils.sizeofchar ()) in
        let size_sure_valid = List.fold_left aux_valid size_new_loc lbases in
        (* Replace the bits [0..size_sure_valid] by [bottom]. Those [bottom]
           will be overwritten in the call to [realloc_copy_one]. *)
        if Z.gt size_sure_valid Z.zero then
          V_Offsetmap.add (Z.zero, Z.pred size_sure_valid)
            (V_Or_Uninitialized.bottom, Z.one, Rel.zero) offsm
        else offsm
    in
    Cvalue.Model.paste_offsetmap
      ~from:offsm ~dst_addr:addr_bits ~size:(Z.succ max_valid) ~exact:false
      dst_state
  in
  (* Copy the old bases *)
  let copy_one dst_state b =
    realloc_copy_one size_max ~src_state:state ~dst_state addr_bits b
  in
  let state = List.fold_left copy_one dst_state lbases in
  ret, state, cacheable

(* Auxiliary function for [realloc]. All the bases in [bases] are realloced
   one by one, plus NULL if [null] holds. This function acts as if we had
   first made a disjunction on the pointer passed to [realloc]. *)
let realloc_multiple bases null size state =
  (* this function should never be used with weak allocs *)
  let aux_bases b acc = Base.Hptset.singleton b :: acc in
  let lbases = Base.Hptset.fold aux_bases bases [] in
  (* This function reallocates the base [b] alone, but does not free it.
     We cannot free yet, because [b] would leak in the states corresponding
     to the variables different from [b]. *)
  let realloc_one_base b = realloc_alloc_copy Strong b false size state in
  let join (ret1, st1, c1) (ret2, st2, c2) =
    V.join ret1 ret2,
    Model.join st1 st2,
    if c1 = c2 then c1 else Builtins.NoCacheCallers
  in
  let aux_one_base acc b = join (realloc_one_base b) acc in
  let res = List.fold_left aux_one_base (V.bottom, state, Cacheable) lbases in
  (* Add another base for realloc(NULL) if needed. *)
  if null then
    join res (realloc_alloc_copy Strong Base.Hptset.empty true size state)
  else res

let realloc_imprecise_weakest _bases _null _size state =
  let new_base, max_alloc, cacheable = alloc_weakest_base Base.Malloc in
  let state = add_uninitialized state new_base max_alloc in
  let ret = V.inject new_base Ival.zero in
  ret, state, cacheable

let choose_bases_reallocation () =
  match Eva_annotations.get_allocation (current_call_site ()) with
  | Fresh | Fresh_weak -> realloc_multiple
  | By_stack -> realloc_alloc_copy Weak
  | Imprecise -> realloc_imprecise_weakest

let realloc_builtin_aux state ptr size =
  let bases, card_ok, null = resolve_bases_to_free ptr in
  if card_ok > 0 then
    let realloc = choose_bases_reallocation () in
    let ret, new_state, cacheable = realloc bases null size state in
    (* Maybe the calls above made [ret] weak, and it
       was among the arguments. In this case, do not free it entirely! *)
    let strong = card_ok <= 1 && not Base.(Hptset.exists is_weak bases) in
    (* free old bases. *)
    let new_state, changed = free_aux new_state ~strong bases in
    let c_values = wrap_fallible_alloc ret state new_state in
    let c_clobbered = Builtins.clobbered_set_from_ret new_state ret in
    Builtins.Full { c_values; c_clobbered; c_assigns = Some changed; cacheable; }
  else (* Invalid call. *)
    let c_clobbered = Base.SetLattice.bottom in
    let cacheable = Builtins.Cacheable in
    Builtins.Full { c_values = []; c_clobbered; c_assigns = None; cacheable; }

let realloc_builtin state args =
  let ptr, size =
    match args with
    | [ (_, ptr); (_, size) ] -> ptr, size
    | _ -> raise (Builtins.Invalid_nb_of_args 2)
  in
  realloc_builtin_aux state ptr size

let () =
  let name = "Frama_C_realloc" in
  let replace = "realloc" in
  let typ () = Cil_const.(voidPtrType, [voidPtrType; Machine.sizeof_type ()]) in
  Builtins.register_builtin ~replace name realloc_builtin ~typ

let reallocarray_builtin state args =
  let ptr, nmemb, sizev =
    match args with
    | [ (_, ptr); (_, nmemb); (_, sizev) ] -> ptr, nmemb, sizev
    | _ -> raise (Builtins.Invalid_nb_of_args 3)
  in
  let size = Cvalue.V.mul nmemb sizev in
  let size_ok = alloc_size_ok size in
  if size_ok <> Alarmset.True then
    Self.warning ~current:true ~once:true
      "reallocarray out of bounds: assert(nmemb * size <= SIZE_MAX)";
  if size_ok = Alarmset.False (* size always overflows *)
  then Builtins.Result [Cvalue.V.singleton_zero]
  else
    let valid_size =
      Cvalue.V.narrow (Cvalue.V.inject_ival (zero_to_max_bytes ())) size
    in
    let res = realloc_builtin_aux state ptr valid_size in
    if size_ok = Alarmset.Unknown then
      (* include failure case among possible results *)
      match res with
      | Builtins.Full cr ->
        let c_values = (Some Cvalue.V.singleton_zero, state) :: cr.c_values in
        Builtins.Full { cr with c_values }
      | _ -> assert false (* realloc_builtin_aux always returns Full *)
    else res

let () =
  let name = "Frama_C_reallocarray" in
  let replace = "reallocarray" in
  let typ () =
    Cil_const.voidPtrType,
    [Cil_const.voidPtrType; Machine.sizeof_type (); Machine.sizeof_type ()]
  in
  Builtins.register_builtin ~replace name reallocarray_builtin ~typ

(* ----------------------------- Leak detection ----------------------------- *)

(* Experimental, not to be released, leak detection built-in. *)
(* Check if the base_to_check is present in one of
   the offsetmaps of the state *)
exception Not_leaked
let check_if_base_is_leaked base_to_check state =
  match state with
  | Model.Bottom -> false
  | Model.Top -> true
  | Model.Map m ->
    try
      Cvalue.Model.fold
        (fun base offsetmap () ->
           if not (Base.equal base_to_check base) then
             Cvalue.V_Offsetmap.iter_on_values
               (fun v ->
                  if Addresses.Bytes.may_reach base_to_check
                      (V_Or_Uninitialized.get_v v) then raise Not_leaked)
               offsetmap)
        m
        ();
      true
    with Not_leaked -> false

(* Does not detect leaked cycles within malloc'ed bases.
   The complexity is very far from being optimal. *)
let check_leaked_malloced_bases state _ =
  let alloced_bases = Dynamic_Alloc_Bases.get () in
  Base_hptmap.iter
    (fun base _ -> if check_if_base_is_leaked base state then
        Self.warning ~current:true ~once:true
          "memory leak detected for %a"
          Base.pretty base)
    alloced_bases;
  let c_values = [ None, state ] in
  let c_clobbered = Base.SetLattice.bottom in
  let cacheable = Builtins.NoCacheCallers in
  Builtins.Full { c_values; c_clobbered; c_assigns = None; cacheable; }

let () =
  Builtins.register_builtin "Frama_C_check_leak" check_leaked_malloced_bases
