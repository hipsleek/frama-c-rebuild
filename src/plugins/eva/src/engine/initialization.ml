(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Creation of the initial state of abstract domains. *)

open Cil_types
open Eval
open Eva_ast

type padding_initialization = [
  | `Initialized
  | `Uninitialized
  | `MaybeInitialized
]

(* There are two different options for locals and for globals variables:
   a three-valued parameter of Eva for globals, and a boolean parameter of
   the kernel for locals. Please don't ask. *)
let padding_initialization ~local : padding_initialization =
  if local
  then
    if Kernel.InitializedPaddingLocals.get ()
    then `Initialized else `Uninitialized
  else
    Parameters.InitializationPaddingGlobals.get ()

(* Warn if the size is unknown. *)
let warn_unknown_size vi =
  try
    ignore (Cil.bitsSizeOf vi.vtype);
    false
  with Cil.SizeOfError (s, t)->
    let pp fmt v = Format.fprintf fmt "variable '%a'" Printer.pp_varinfo v in
    Self.warning ~once:true ~current:true
      ~wkey:Self.wkey_unknown_size
      "@[during initialization@ of %a,@ size of@ type '%a'@ cannot be@ \
       computed@ (%s)@]" pp vi Printer.pp_typ t s;
    true

(* A bottom in any part of an initializer results in a bottom for the
   whole initialization. Thus, the following monad raises an exception on a
   bottom case; the exception is caught by the root initialization functions
   to return a proper `Bottom. *)
exception Initialization_failed

let (>>>) t f = match t with
  | `Bottom -> raise Initialization_failed
  | `Value v -> f v

let counter = ref 0


module type Engine_Subset = sig
  include Engine_abstractions_sig.S
  module Transfer_stmt : Engine_sig.Transfer_stmt with type state = Dom.t
end

module Make (Engine: Engine_Subset) = struct

  incr counter;;

  type state = Engine.Dom.t
  module Domain = Engine.Dom

  (* Evaluation in the top state: we do not want a location to depend on
     other globals. *)
  let lval_to_loc lval =
    fst (Engine.Eval.lvaluate ~for_writing:false Domain.top lval)
    >>> fun (_valuation, loc) -> loc

  (* ------------------------- Apply initializer ---------------------------- *)

  (* Conventions:
     - functions in *_var_* act on the entire variables, and receive only
       the corresponding varinfo
     - other functions act on a lvalue, which they directly receive *)

  (* Initializes an entire variable [vi], in particular padding bits,
     according to [local] and [lib_entry] mode. *)
  let initialize_var_padding ~local ~lib_entry vi state =
    let lval = Eva_ast.Build.var vi in
    match padding_initialization ~local with
    | `Uninitialized -> state
    | `Initialized | `MaybeInitialized as i ->
      let initialized = i = `Initialized in
      let init_value =
        if not local && lib_entry
        then Abstract_domain.Top
        else Abstract_domain.Zero
      in
      let location = lval_to_loc lval in
      Domain.initialize_variable lval location ~initialized init_value state

  (* Initializes a volatile lvalue to top. *)
  let initialize_top_volatile lval state =
    let location = lval_to_loc lval in
    let init_value = Abstract_domain.Top in
    Domain.initialize_variable lval location ~initialized:true init_value state

  (* Applies a single initializer, using the standard transfer function on
     assignments. Warns if the results is bottom. *)
  let apply_eva_single_initializer ~source ~pos state lval expr =
    match Engine.Transfer_stmt.assign state ~pos lval expr with
    | `Bottom ->
      if not (Position.is_local pos) then
        Self.warning ~pos ~source ~once:true
          "evaluation of initializer '%a' failed@." Eva_ast.pp_exp expr;
      raise Initialization_failed
    | `Value v -> v

  (* Initializes array [lval] with sequence of characters [seq] in [state].
     Auxiliary function used for string and wide string literals: [zero] is the
     null character and [constant] builds the Eva constant form a character.
     Applies [apply_eva_single_initializer] for each character. *)
  let init_char_array_aux ~source ~pos zero constant lval seq state =
    let _, size = Ast_types.array_elem_type_and_size lval.typ in
    (* Adds [zero] characters to the sequence. *)
    let seq =
      match Option.bind (Cil.constFoldToInt ~machdep:true) size with
      | None -> Seq.append seq (Seq.return zero)
      | Some size ->
        Seq.take (Z.to_int size) (Seq.append seq (Seq.repeat zero))
    in
    (* Initializes i-nth element with character [c]. *)
    let init_element state i c =
      let index_i = Z.of_int i in
      let index_cst = Const (CInt64 (index_i, Machine.sizeof_kind (), None)) in
      let index_exp = Eva_ast_builder.mk_exp index_cst in
      let index = Index (index_exp, NoOffset) in
      let lval = Eva_ast.(add_offset lval index) in
      let expr = Eva_ast_builder.mk_exp (Const (constant c)) in
      apply_eva_single_initializer ~pos ~source state lval expr
    in
    Seq.fold_lefti init_element state seq

  (* Initializes array [lval] from string literal [str] in [state]. *)
  let init_char_array ~source ~pos lval str state =
    if not (Ast_types.is_any_char_array lval.typ) then
      Self.fatal
        "Initialization of %a of type %a with a string literal, \
         which can only be used to initialize a char array."
        Eva_ast_printer.pp_lval lval Printer.pp_typ lval.typ;
    let zero = '\000' in
    let seq = String.to_seq str in
    let constant c = CChr c in
    init_char_array_aux ~source ~pos zero constant lval seq state

  (* Initializes array [lval] from wide string literal [list] in [state]. *)
  let init_wchar_array ~source ~pos lval list state =
    if not (Ast_types.is_wchar_array lval.typ) then
      Self.fatal
        "Initialization of %a of type %a with a wide string literal, \
         which can only be used to initialize a wide char array."
        Eva_ast_printer.pp_lval lval Printer.pp_typ lval.typ;
    let zero = Int64.zero in
    let constant i = CInt64 (Z.of_int64 i, Machine.wchar_kind (), None) in
    let seq = List.to_seq list in
    init_char_array_aux ~source ~pos zero constant lval seq state

  let get_string_literal e =
    match e.node with
    | Lval { node = Var v, NoOffset } ->
      Some (Globals.Vars.get_string_literal v)
    | _ -> None

  (* Applies a single initializer, with the special case of char or wchar arrays
     being initialized with string literals. *)
  let apply_eva_single_initializer_or_str ~source ~pos state lval expr =
    if Ast_types.is_any_char_array lval.typ then begin
      match get_string_literal expr with
      | Some (Str s) -> init_char_array ~source ~pos lval s state
      | None | Some (Wstr _) ->
        Self.fatal "Single init of a char array can only be a string literal"
    end else if Ast_types.is_wchar_array lval.typ then begin
      match get_string_literal expr with
      | Some (Wstr ws) -> init_wchar_array ~source ~pos lval ws state
      | None | Some (Str _) ->
        Self.fatal "Single init of a wchar array can only be a wide string literal"
    end else
      apply_eva_single_initializer ~source ~pos state lval expr

  (* Applies an initializer. If [top_volatile] is true, sets volatile locations
     to top without applying the initializer. Otherwise, lets the standard
     transfer function on assignments handle volatile locations. *)
  let rec apply_eva_initializer ~pos ~top_volatile lval init state =
    if top_volatile && Ast_types.has_qualifier "volatile" lval.typ
    then initialize_top_volatile lval state
    else
      match init with
      | SingleInit (exp, loc) ->
        let source = fst loc in
        apply_eva_single_initializer_or_str ~pos ~source state lval exp
      | CompoundInit (_typ, l) ->
        let doinit state (off, init) =
          let lval = Eva_ast.add_offset lval off in
          apply_eva_initializer ~pos ~top_volatile lval init state
        in
        List.fold_left doinit state l

  (* Field by field initialization of a variable to zero, or top if volatile.
     Very inefficient. *)
  let initialize_var_zero_or_volatile ~pos vi state =
    let loc = Position.loc pos in
    let init = Eva_ast.translate_init (Cil.makeZeroInit ~loc vi.vtype) in
    let lval = Eva_ast.Build.var vi in
    apply_eva_initializer ~pos ~top_volatile:true lval init state

  (* ----------------------- Non Lib-entry mode ----------------------------- *)

  (* Initializes a varinfo, padding bits + optionally an initializer. *)
  let initialize_var_not_lib_entry ~pos ~local vi init state =
    ignore (warn_unknown_size vi);
    let source = fst vi.vdecl in
    let typ = vi.vtype in
    let lval = Eva_ast.Build.var vi in
    let volatile_everywhere = Ast_types.has_qualifier "volatile" typ in
    let state =
      if volatile_everywhere && padding_initialization ~local = `Initialized
      then initialize_top_volatile lval state
      else
        (* Initializes padding bits everywhere (non padding bits are overwritten
           afterwards). *)
        let state = initialize_var_padding vi ~local ~lib_entry:false state in
        (* Initializes everything except padding bits: non-volatile locations
           to zero, volatile locations to top. We only do so if the variable
           must be different from zero somewhere. This is a not-so minor
           optimization. *)
        if padding_initialization ~local = `Initialized &&
           not (Ast_types.is_volatile typ)
        then state
        else initialize_var_zero_or_volatile ~pos vi state
    in
    (* Applies the real initializer on top. *)
    match init with
    | None -> state
    | Some (StrInit (Str s)) -> init_char_array ~source ~pos lval s state
    | Some (StrInit (Wstr a)) -> init_wchar_array ~source ~pos lval a state
    | Some (CInit init) ->
      apply_eva_initializer ~pos ~top_volatile:false lval init state


  (* --------------------------- Lib-entry mode ----------------------------- *)

  (* Special application of an initializer: only non-volatile lval with
     attributes 'const' are initialized. *)
  let rec apply_cil_const_initializer ~pos state lval = function
    | Cil_types.SingleInit exp ->
      let typ_lval = Cil.typeOfLval lval in
      if Ast_types.has_qualifier "const" typ_lval &&
         not (Ast_types.has_qualifier "volatile" typ_lval)
         && not (Cil.is_mutable_or_initialized lval)
      then
        let lval = Eva_ast.translate_lval lval
        and exp = Eva_ast.translate_exp exp
        and source = fst exp.eloc in
        apply_eva_single_initializer_or_str ~pos ~source state lval exp
      else state
    | CompoundInit (typ, l) ->
      if Ast_types.has_qualifier "volatile" typ || not (Ast_types.is_const typ)
      then state (* initializer is not useful *)
      else
        let doinit offset init _typ state =
          let lval = Cil.addOffsetLval offset lval in
          apply_cil_const_initializer ~pos state lval init
        in
        Cil.foldLeftCompound ~implicit:true ~doinit ~ct:typ ~initl:l ~acc:state

  (* Initializes [vi] as if in [-lib-entry] mode. Active when [-lib-entry] is
     set, or when [vi] is extern. [const] initializers, explicit or implicit,
     are taken into account *)
  let initialize_var_lib_entry ~pos vi init state =
    let unknown_size =  warn_unknown_size vi in
    let state =
      if unknown_size then
        (* the type is unknown, initialize everything to Top *)
        let lval = Eva_ast.Build.var vi in
        let loc = lval_to_loc lval in
        let v = Abstract_domain.Top in
        Domain.initialize_variable lval loc ~initialized:true v state
      else
        (* Add padding everywhere. *)
        let state =
          initialize_var_padding vi ~local:false ~lib_entry:true state
        in
        (* Then initialize non-padding bits according to the type. *)
        let kind = Abstract_domain.Global in
        Domain.initialize_variable_using_type kind vi state
    in
    (* If needed, initializes const fields according to the initializer
       (or generate one if there are none). In the first phase, they have been
       set to generic values. This can only happen for variables partially but
       not fully const, as const variables are initialized differently. *)
    if Ast_types.is_const vi.vtype && not (vi.vstorage = Extern)
    then
      let init = match init with
        | None -> Cil.makeZeroInit ~loc:vi.vdecl vi.vtype
        | Some (Cil_types.CInit init) -> init
        | Some (Cil_types.StrInit _) ->
          (* A char array cannot be partially const. *)
          Self.fatal
            "Initializer StrInit for variable %a, which is not a char array"
            Printer.pp_varinfo vi
      in
      apply_cil_const_initializer ~pos state (Cil.var vi) init
    else state


  (* ------------- Adds formal argument of the main function  --------------- *)

  (* Compute values for the formals of [kf] (as if those were variables in
     lib-entry mode) and add them to [state] *)
  let compute_main_formals kf state =
    match kf.fundec with
    | Declaration (_, _, None, _) -> state
    | Declaration (_, _, Some l, _)
    | Definition ({ sformals = l }, _) ->
      if l <> [] && Parameters.InterpreterMode.get ()
      then
        Self.abort "Entry point %a has arguments"
          Kernel_function.pretty kf
      else
        let var_kind = Abstract_domain.Formal kf in
        let state = Domain.enter_scope var_kind l state in
        let init vi state =
          let open Current_loc.Operators in
          let<> UpdatedCurrentLoc = vi.vdecl in
          Domain.initialize_variable_using_type var_kind vi state
        in
        List.fold_right init l state

  (* Use the values supplied in [actuals] for the formals of [kf], and
     bind them in [state] *)
  let add_supplied_main_formals kf actuals state =
    match Engine.Dom.get_cvalue with
    | None -> Self.abort "API function [set_main_args] cannot be \
                          used without the Cvalue domain"
    | Some get_cvalue ->
      let formals = Kernel_function.get_formals kf in
      if (List.length formals) <> List.length actuals then
        Self.abort
          "Incorrect number of arguments for the main function %a \
           provided via the API function [set_main_args]"
          Kernel_function.pretty kf;
      let cvalue_state = get_cvalue state in
      let add_actual state actual formal =
        let actual = Eval_op.offsetmap_of_v ~typ:formal.vtype actual in
        Cvalue.Model.add_base (Base.of_varinfo formal) actual state
      in
      let cvalue_state =
        List.fold_left2 add_actual cvalue_state actuals formals
      in
      let set_domain = Domain.set Cvalue_domain.State.key in
      set_domain (cvalue_state, Locals_scoping.bottom ()) state

  let add_main_formals ?arguments kf state =
    match arguments with
    | None -> compute_main_formals kf state
    | Some actuals -> add_supplied_main_formals kf actuals state


  (* ------------------------ High-level functions -------------------------- *)

  let initialize_local_variable ~pos vi init state =
    let init = Some (CInit init) in
    try `Value (initialize_var_not_lib_entry ~pos ~local:true vi init state)
    with Initialization_failed -> `Bottom

  let is_fully_const vi =
    let frama_c_mutable = Ast_attributes.frama_c_mutable in
    Ast_types.has_qualifier "const" vi.vtype
    && not (Ast_types.has_attribute_memory_block frama_c_mutable vi.vtype)

  let initialize_global_variable ~lib_entry vi init state =
    let open Current_loc.Operators in
    let<> UpdatedCurrentLoc = vi.vdecl in
    Async.yield ();
    Signal.check ();
    let pos = Position.global_init vi in
    let state = Domain.enter_scope Abstract_domain.Global [vi] state in
    if vi.vsource then
      if (lib_entry && not (is_fully_const vi)) || vi.vstorage = Extern then
        initialize_var_lib_entry ~pos vi init.init state
      else
        let init = Option.map Eva_ast.translate_init_or_str init.init in
        initialize_var_not_lib_entry ~pos ~local:false vi init state
    else state

  (* Compute the initial state with all global variable initialized. *)
  let compute_global_state ~lib_entry () =
    Self.debug ~level:2 "Computing globals values";
    let state = Domain.empty () in
    let initialize = initialize_global_variable ~lib_entry in
    try `Value (Globals.Vars.fold_in_file_order initialize state)
    with Initialization_failed -> `Bottom

  (* Dependencies for the Frama-C states containing the initial states
     of Eva: all correctness parameters of Eva, plus the AST itself. We
     cannot use [Self.state] directly, because we do not want to
     depend on the tuning parameters. Previously, we use a more
     fine-grained list, but this lead to bugs. See mantis #2277. *)
  let correctness_deps =
    Ast.self ::
    List.map
      (fun p -> State.get p.Typed_parameter.name)
      Parameters.parameters_correctness

  module InitialState =
    State_builder.Option_ref
      (Bottom.Make_Datatype (Domain))
      (struct
        let name = "Value.Initialization" ^ "(" ^ string_of_int !counter ^ ")"
        let dependencies = correctness_deps
      end)
  let () = Ast.add_monotonic_state InitialState.self

  (* The computation depends on the lib_entry option, which is a correctness
     parameter of the analyzer: the InitialState memoization is thus safely
     cleaned when lib_entry changes. *)
  let global_state ~lib_entry =
    InitialState.memo (compute_global_state ~lib_entry)

  (* The global cvalue state may be supplied by the user. *)
  let supplied_state state cvalue_state =
    if Cvalue.Model.is_reachable cvalue_state
    then
      let cvalue_state = cvalue_state, Locals_scoping.bottom () in
      `Value (Domain.set Cvalue_domain.State.key cvalue_state state)
    else `Bottom

  let print_initial_cvalue_state state =
    let cvalue_state = Engine.Dom.get_cvalue_or_top state in
    (* Do not show string literal nor variables from libc specifications. *)
    let print_base base =
      try
        let vi = Base.to_varinfo base in
        not (Cil.is_in_libc vi.vattr || Ast_info.is_string_literal vi)
      with Base.Not_a_C_variable -> true
    in
    let cvalue_state = Cvalue.Model.filter_base print_base cvalue_state in
    Self.printf ~dkey:Self.dkey_initial_state
      ~header:(fun fmt -> Format.pp_print_string fmt
                  "Values of globals at initialization")
      "@[  %a@]" Cvalue.Model.pretty cvalue_state

  let initial_state_with_formals ?cvalue_state ?arguments ~lib_entry kf =
    let+ init_state =
      match cvalue_state with
      | Some cvalue_state ->
        Self.feedback "Initial state supplied by user";
        let* state = global_state ~lib_entry in
        supplied_state state cvalue_state
      | None ->
        Self.feedback ~dkey:Self.dkey_progress "Computing initial state";
        let state = global_state ~lib_entry in
        Self.feedback ~dkey:Self.dkey_progress "Initial state computed";
        state
    in
    let thread = Thread.(id main) in
    let callstack = Callstack.init ~thread ~entry_point:kf in
    Domain.Store.register_state callstack Initial init_state;
    print_initial_cvalue_state init_state;
    add_main_formals ?arguments kf init_state

end
