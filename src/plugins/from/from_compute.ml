(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

exception Call_did_not_take_place

module Record_From_Callbacks =
  Hook.Build
    (struct
      type t =
        (Kernel_function.t Stack.t) *
        Eva.Assigns.Memory.t Stmt.Hashtbl.t *
        (Kernel_function.t * Eva.Assigns.Memory.t) list Stmt.Hashtbl.t
    end)

module type To_Use =
sig
  val get_from_call : kernel_function -> stmt -> Eva.Assigns.t
  val stmt_request : stmt -> Eva.Results.request
  val keep_base : kernel_function -> Base.t -> bool
  val cleanup_and_save : kernel_function -> Eva.Assigns.t -> Eva.Assigns.t
end

let compute_using_prototype_for_state state kf assigns =
  let varinfo = Kernel_function.get_vi kf in
  let return_deps,deps =
    match assigns with
    | WritesAny ->
      From_parameters.warning "@[no assigns clauses@ for function %a.@]@ \
                               Results will be imprecise."
        Kernel_function.pretty kf;
      From_memory.(top_return, top)
    | Writes assigns ->
      let (rt_typ,_,_,_) = Cil.splitFunctionTypeVI varinfo in
      let input_zone out ins =
        (* Technically out is unused, but there is a signature problem *)
        Eva.Logic_inout.assigns_inputs_to_zone state (Writes [out, ins])
      in
      let treat_assign acc (out, ins) =
        let output =
          Eva.Logic_inout.tlval_to_zones Assigns state Write out.it_content
        in
        match output with
        | None ->
          From_parameters.result
            ~once:true ~current:true "Unable to extract assigns in %a"
            Kernel_function.pretty kf;
          acc
        | Some output ->
          let input_zone = input_zone out ins in
          (* assign clauses do not let us specify address
             dependencies for now, so we assume it is all data
             dependencies *)
          let input_deps = Eva.Deps.data input_zone in
          (* Weak update of the over-approximation of the zones assigned *)
          let acc = Eva.Assigns.Memory.add_binding ~exact:false
              acc output.over input_deps in
          (* Now, perform a strong update on the zones that are guaranteed
                   to be assigned (under-approximation) AND that do not depend
                   on themselves.
                   Note: here we remove an overapproximation from an
             underapproximation to get an underapproximation, which is not
             the usual direction. It works here because diff on non-top
                   zones is an exact operation. *)
          let sure_out_zone =
            Memory_zone.(
              if equal top input_zone then bottom else diff output.under input_zone)
          in
          let acc = Eva.Assigns.Memory.add_binding ~exact:true
              acc sure_out_zone input_deps in
          acc

      in
      let treat_ret_assign acc (out, from) =
        let zone_from = input_zone out from in
        (* assign clauses do not let us specify address dependencies for
           now, so we assume it is all data dependencies *)
        let inputs_deps = Eva.Deps.data zone_from in
        try
          let coffs = Logic_to_c.loc_to_offset out.it_content in
          List.fold_left
            (fun acc coff ->
               let (base,width) = Cil.bitsOffset rt_typ coff in
               let size = Z_or_top.of_int width in
               From_memory.add_to_return
                 ~start:base ~size ~m:acc inputs_deps
            )
            acc coffs
        with Logic_to_c.No_conversion | Cil.SizeOfError _ ->
          From_parameters.result  ~once:true ~current:true
            "Unable to extract a proper offset. \
             Using FROM for the whole \\result";
          let size = Bit_utils.sizeof rt_typ in
          From_memory.add_to_return ~size ~m:acc inputs_deps
      in
      let return_assigns, other_assigns =
        List.fold_left
          (fun (ra,oa) (loc,_ as a) ->
             if Logic_utils.is_result loc.it_content
             then a::ra,oa else ra,a::oa)
          ([],[]) assigns
      in
      let return_assigns =
        match return_assigns with
        | [] when Ast_types.is_void rt_typ ->
          From_memory.default_return
        | [] -> (* \from unspecified. *)
          let size = Bit_utils.sizeof rt_typ in
          From_memory.top_return_size size
        | _ ->
          List.fold_left treat_ret_assign
            From_memory.default_return return_assigns
      in
      return_assigns,
      List.fold_left
        treat_assign Eva.Assigns.Memory.empty other_assigns
  in
  Eva.Assigns.{ return = return_deps; memory = deps }

module ZoneStmtMap = struct

  module Hptmap_Info = struct
    let initial_values = []
    let dependencies = [ Ast.self ]
  end

  include Hptmap.Make (Stmt_Id) (Memory_zone) (Hptmap_Info)

  let join =
    let decide _k z1 z2 = Memory_zone.join z1 z2 in
    join ~cache:(Hptmap_sig.PersistentCache "From_compute.ZoneStmtMap.join")
      ~symmetric:true ~idempotent:true ~decide
end

module Make (To_Use: To_Use) =
struct
  type t' =
    { additional_deps_table : ZoneStmtMap.t;
      (** Additional control dependencies to add to all modified variables,
          coming from the control statements encountered so far (If, Switch).
          The statement information is used to remove the dependencies that
          are no longer useful, when we reach a statement that post-dominates
          the statement that gave rise to the dependency. *)
      additional_deps : Memory_zone.t;
      (** Union of the sets in {!additional_deps_table} *)
      deps_table : Eva.Assigns.Memory.t
      (** dependency table *)
    }

  let call_stack : kernel_function Stack.t = Stack.create ()
  (** Stack of function being processed *)

  (** Recreate the [additional_deps] field from [additional_deps_table] *)
  let rebuild_additional_deps map =
    ZoneStmtMap.fold (fun _ z accz -> Memory_zone.join z accz) map Memory_zone.bottom


  (** given a [Function_Froms.Deps.t], apply [f] on both components and merge
      the result:
        depending directly on an indirect dependency -> indirect,
        depending indirectly on a direct dependency  -> indirect *)
  let merge_deps f deps =
    let open Eva.Deps in
    let ind = f deps.indirect in
    let data = f deps.data in
    let ind = Memory_zone.join data.indirect (to_zone ind) in
    let data = data.data in
    { data = data; indirect = ind }


  (** Bind all the variables of [b] to [Assigned \from \nothing]. This function
      is always called on local variables. We do *not* want to bind a local
      variable [v] to Unassigned, as otherwise we could get some dependencies
      that refer to [v] (when [v] is not guaranteed to be always assigned, or
      for padding in local structs), and that would need to be removed when v
      goes out of scope. Moreover, semantically, [v] *is* assigned (albeit to
      "uninitialized",  which represents an indefinite part of the stack). We
      do not attempts to track this "uninitialized" information in From, as this
      is redundant with the work done by Value -- hence the use of [\nothing].*)
  let bind_locals m b =
    let aux_local acc vi =
      (* Consider that local are initialized to a constant value *)
      Current_loc.with_loc vi.vdecl
        (From_memory.bind_var vi Eva.Deps.bottom) acc
    in
    List.fold_left aux_local m b.blocals

  let unbind_locals m b =
    let aux_local acc vi =
      From_memory.unbind_var vi acc
    in
    List.fold_left aux_local m b.blocals


  let find stmt deps_tbl expr =
    let request = To_Use.stmt_request stmt in
    let pre_trans = Eva.Results.expr_dependencies expr request in
    merge_deps
      (fun d -> Eva.Assigns.Memory.find_precise deps_tbl d) pre_trans

  let lval_to_zone_with_deps stmt lv =
    let request = To_Use.stmt_request stmt in
    Eva.Results.lval_deps lv request

  let lval_to_precise_loc_with_deps stmt ~for_writing lv =
    let request = To_Use.stmt_request stmt in
    let deps = Eva.Results.address_deps lv request in
    let address = Eva.Results.eval_address ~for_writing lv request in
    let loc = Eva.Results.as_precise_loc address
    and exact = Eva.Results.(is_singleton address || is_bottom address) in
    deps, loc, exact

  let empty_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Memory_zone.bottom;
      deps_table = Eva.Assigns.Memory.empty }

  let bottom_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Memory_zone.bottom;
      deps_table = Eva.Assigns.Memory.bottom }

  module Computer = struct

    type t = t'
    let bottom = bottom_from;;

    let callwise_states_with_formals = Stmt.Hashtbl.create 7

    let substitute call_site_froms extra_loc deps =
      let subst_deps = From_memory.substitute call_site_froms deps in
      Eva.Deps.add_indirect subst_deps extra_loc

    let display_one_from fmt v =
      Eva.Assigns.Memory.pretty fmt v.deps_table;
      Format.fprintf fmt "Additional Variable Map : %a@\n"
        ZoneStmtMap.pretty v.additional_deps_table;
      Format.fprintf fmt
        "Additional Variable Map Set : %a@\n"
        Memory_zone.pretty
        v.additional_deps

    let pretty fmt (v: t) =
      display_one_from fmt v

    let transfer_conditional_exp s exp state =
      let additional = find s state.deps_table exp in
      let additional = Eva.Deps.to_zone additional in
      {state with
       additional_deps_table =
         ZoneStmtMap.add s additional state.additional_deps_table;
       additional_deps =
         Memory_zone.join additional state.additional_deps }


    let join_and_is_included new_ old =
      let additional_map, additional_zone, included =
        let mold = old.additional_deps_table in
        let mnew = new_.additional_deps_table in
        let zold = old.additional_deps in
        let m = ZoneStmtMap.join mnew mold in
        if ZoneStmtMap.equal m mold then
          mold, zold, true
        else
          let new_z = Memory_zone.join old.additional_deps new_.additional_deps in
          m, new_z, false
      in
      let map =
        Eva.Assigns.Memory.join new_.deps_table old.deps_table
      in
      let included' =
        Eva.Assigns.Memory.is_included new_.deps_table old.deps_table
      in
      { deps_table = map;
        additional_deps_table = additional_map;
        additional_deps = additional_zone; },
      included && included'

    let join old new_ = fst (join_and_is_included old new_)
    let is_included old new_ = snd (join_and_is_included old new_)

    (** Handle an assignment [lv = ...], the dependencies of the right-hand
        side being stored in [deps_right]. [init] is true for a local
        initialization, in which case the left location is not reduced to its
        valid part for a writing, in order to keep the const local variables. *)
    let transfer_assign stmt ~init lv deps_right state =
      (* The assigned location is [loc], whose address is computed from
         [deps]. *)
      let deps, loc, exact =
        lval_to_precise_loc_with_deps stmt ~for_writing:(not init) lv
      in
      let deps_of_deps = Eva.Assigns.Memory.find state.deps_table deps in
      let all_indirect = Memory_zone.join state.additional_deps deps_of_deps in
      let deps = Eva.Deps.add_indirect deps_right all_indirect in
      let access = if init then Locations.Read else Locations.Write in
      { state with deps_table =
                     Eva.Assigns.Memory.add_binding_precise_loc
                       ~exact access state.deps_table loc deps }

    let transfer_call stmt dest f args _loc state =
      Async.yield ();
      let request = To_Use.stmt_request stmt in
      let called_vinfos = Eva.Results.(eval_callee f request |> default []) in
      let f_deps = Eva.Results.lval_deps (f,NoOffset) request in
      (* dependencies for the evaluation of [f] *)
      let f_deps = Eva.Assigns.Memory.find state.deps_table f_deps in
      let additional_deps =
        Memory_zone.join
          state.additional_deps
          f_deps
      in
      let args_froms =
        List.map
          (fun arg ->
             (* TODO : dependencies on subfields for structs *)
             find stmt state.deps_table arg)
          args
      in
      let states_with_formals = ref [] in
      let do_on kf =
        let called_vinfo = Kernel_function.get_vi kf in
        if Ast_info.start_with_frama_c_builtin called_vinfo.vname then
          state
        else
          let froms_call = To_Use.get_from_call kf stmt in
          let froms_call_table = froms_call.Eva.Assigns.memory in
          if Eva.Assigns.Memory.is_bottom froms_call_table then
            bottom_from
          else
            let formal_args = Kernel_function.get_formals kf in
            let state_with_formals = ref state.deps_table in
            begin try
                List.iter2
                  (fun vi from ->
                     state_with_formals :=
                       From_memory.bind_var
                         vi from !state_with_formals;
                  ) formal_args args_froms;
              with Invalid_argument _ ->
                From_parameters.warning ~once:true ~current:true
                  "variadic call detected. Using only %d argument(s)."
                  (min
                     (List.length formal_args)
                     (List.length args_froms))
            end;
            if not (Record_From_Callbacks.is_empty ())
            then
              states_with_formals :=
                (kf, !state_with_formals) :: !states_with_formals;
            let subst_before_call =
              substitute !state_with_formals additional_deps
            in
            (* From state just after the call,
               but before the result assignment *)
            let deps_after_call =
              let before_call = state.deps_table in
              let call_substituted =
                From_memory.map subst_before_call froms_call_table
              in
              From_memory.compose call_substituted before_call
            in
            let state = {state with deps_table = deps_after_call } in
            (* Treatment for the possible assignment
               of the call result *)
            match dest with
            | None -> state
            | Some lv ->
              let return_from = froms_call.Eva.Assigns.return in
              let deps_ret = subst_before_call return_from in
              let init = Cil.is_mutable_or_initialized lv in
              transfer_assign stmt ~init lv deps_ret state
      in
      let f acc f =
        let p = do_on f in
        match acc with
        | None -> Some p
        | Some acc_memory ->
          Some
            {state with
             deps_table = Eva.Assigns.Memory.join
                 p.deps_table
                 acc_memory.deps_table}
      in
      let result =
        try
          (match List.fold_left f None called_vinfos with
           | None -> state
           | Some s -> s);
        with Call_did_not_take_place -> state
      in
      if not (Record_From_Callbacks.is_empty ())
      then
        Stmt.Hashtbl.replace
          callwise_states_with_formals
          stmt
          !states_with_formals;
      result

    let transfer_instr stmt (i: instr) (state: t) =
      Async.yield ();
      match i with
      | Set (lv, exp, _) ->
        let comp_vars = find stmt state.deps_table exp in
        let init = Cil.is_mutable_or_initialized lv in
        transfer_assign stmt ~init lv comp_vars state
      | Local_init(v, AssignInit i, _) ->
        let rec aux lv i acc =
          let doinit o i _ state = aux (Cil.addOffsetLval o lv) i state in
          match i with
          | SingleInit e ->
            let comp_vars = find stmt acc.deps_table e in
            transfer_assign stmt ~init:true lv comp_vars acc
          | CompoundInit (ct, initl) ->
            (* To avoid a performance issue, do not fold implicit initializers
               of scalar or large arrays. We still use implicit initializers
               for small struct arrays, as this may be more precise in case of
               padding bits. The 100 limit is arbitrary. *)
            let implicit =
              not (Ast_types.is_array ct &&
                   (Ast_types.(is_scalar (direct_element_type ct))
                    || Ast_info.array_size ct > 100z))
            in
            let r = Cil.foldLeftCompound ~implicit ~doinit ~ct ~initl ~acc in
            if implicit then r else
              (* If implicit zero-initializers have been skipped, also mark
                 the entire array as initialized from no dependency (nothing
                 is read by the implicit zero-initializers). *)
              transfer_assign stmt ~init:true lv Eva.Deps.bottom r
        in
        aux (Cil.var v) i state
      | Call (lvaloption,funclv,argl,loc) ->
        transfer_call stmt lvaloption funclv argl loc state
      | Local_init (v, ConsInit(f, args, kind), loc) ->
        Cil.treat_constructor_as_func
          (transfer_call stmt) v f args kind loc state
      | Asm _ | Code_annot _ | Skip _ -> state


    let transfer_guard s e d =
      let request = To_Use.stmt_request s in
      let interpreted_e = Eva.Results.(eval_exp e request |> as_cvalue) in
      let t1 = Ast_types.unroll (Cil.typeOf e) in
      let do_then, do_else =
        if Ast_types.is_integral t1 || Ast_types.is_ptr t1
        then Cvalue.V.contains_non_zero interpreted_e,
             Cvalue.V.contains_zero interpreted_e
        else true, true (* TODO: a float condition is true iff != 0.0 *)
      in
      (if do_then then d else bottom),
      (if do_else then d else bottom)
    ;;

    (* Eliminate additional variables originating from a control-flow branching
       statement closing at [s]. *)
    let eliminate_additional s data =
      let map = data.additional_deps_table in
      let map' =
        ZoneStmtMap.fold
          (fun k _v acc_map ->
             if Dominators.postdominates s k
             then ZoneStmtMap.remove k acc_map
             else acc_map
          ) map map
      in
      if not (map == map') then
        { data with
          additional_deps_table = map';
          additional_deps = rebuild_additional_deps map';
        }
      else data

    let transfer_stmt s data =
      let data = eliminate_additional s data in
      let map_on_all_succs new_data = List.map (fun x -> (x,new_data)) s.succs in
      match s.skind with
      | Instr i -> map_on_all_succs (transfer_instr s i data)

      | If(exp,_,_,_) ->
        let data = transfer_conditional_exp s exp data in
        Dataflows.transfer_if_from_guard transfer_guard s data
      | Switch(exp,_,_,_) ->
        let data = transfer_conditional_exp s exp data in
        Dataflows.transfer_switch_from_guard transfer_guard s data

      | Return _ | Throw _ -> []

      | UnspecifiedSequence _ | Loop _ | Block _
      | Goto _ | Break _ | Continue _
      | TryExcept _ | TryFinally _ | TryCatch _
        -> map_on_all_succs data
    ;;

    (* Filter out unreachable values. *)
    let transfer_stmt s d =
      if Eva.Results.is_reachable s &&
         not (Eva.Assigns.Memory.is_bottom d.deps_table)
      then transfer_stmt s d
      else []

    let doEdge s succ d =
      if Eva.Results.is_reachable succ
      then
        let dt = d.deps_table in
        let opened = Kernel_function.blocks_opened_by_edge s succ in
        let closed = Kernel_function.blocks_closed_by_edge s succ in
        let dt = List.fold_left bind_locals dt opened in
        let dt = List.fold_left unbind_locals dt closed in
        { d with deps_table = dt }
      else
        bottom_from

    (* Filter the outgoing data using doEdge. *)
    let transfer_stmt s d =
      let ds = transfer_stmt s d in
      List.map (fun (succ, d) -> (succ, doEdge s succ d)) ds
    ;;

  end


  (* Remove all local variables and formals from table *)
  let externalize return_stmt kf state =
    let return =
      (match return_stmt.skind with
       | Return (Some ({enode = Lval v}),_) ->
         let zone = lval_to_zone_with_deps return_stmt v in
         let deps = Eva.Assigns.Memory.find_precise state.deps_table zone in
         let size = Bit_utils.sizeof (Cil.typeOfLval v) in
         From_memory.add_to_return ~size deps
       | Return (None,_) ->
         From_memory.default_return
       | _ -> assert false)
    in
    let accept = To_Use.keep_base kf in
    let memory = Eva.Assigns.Memory.filter_base accept state.deps_table in
    Eva.Assigns.{ return; memory }

  let compute_using_cfg kf =
    match kf.fundec with
    | Declaration _ -> assert false
    | Definition (f,_) ->
      if not (Eva.Analysis.save_results kf) then Eva.Assigns.top
      else
        try
          Stack.iter (fun g -> if kf == g then raise Exit) call_stack;
          Stack.push kf call_stack;
          let state =
            { empty_from with
              deps_table = bind_locals empty_from.deps_table f.sbody }
          in
          let module Fenv =
            (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV)
          in
          let module Dataflow_arg = struct
            include Computer
            let init = [(Kernel_function.find_first_stmt kf, state)]
          end
          in
          let module Compute = Dataflows.Simple_forward(Fenv)(Dataflow_arg) in
          let ret_id = Kernel_function.find_return kf in
          if not (Record_From_Callbacks.is_empty ())
          then begin
            From_parameters.feedback "Now calling From callbacks";
            let states =
              Stmt.Hashtbl.create Fenv.nb_stmts
            in
            Compute.iter_on_result (fun k record ->
                Stmt.Hashtbl.add states k record.deps_table);
            Record_From_Callbacks.apply
              (call_stack, states, Dataflow_arg.callwise_states_with_formals)
          end;
          let _popped = Stack.pop call_stack in
          let last_from =
            try
              if Eva.Results.is_reachable ret_id
              then
                externalize
                  ret_id
                  kf
                  Compute.before.(Fenv.to_ordered ret_id)
              else
                raise Not_found
            with Not_found -> begin
                From_parameters.result
                  "Non-terminating function %a (no dependencies)"
                  Kernel_function.pretty kf;
                Eva.Assigns.{
                  return = From_memory.default_return;
                  memory = Eva.Assigns.Memory.bottom
                }
              end
          in
          last_from

        with Exit (* Recursive call *) ->
          {
            return = From_memory.default_return;
            memory = Eva.Assigns.Memory.empty
          }

  let compute_using_prototype kf =
    let state = Eva.Results.(at_start_of kf |> get_cvalue_model) in
    let behaviors = Eva.Logic_inout.valid_behaviors kf state in
    let assigns = Ast_info.merge_assigns behaviors in
    compute_using_prototype_for_state state kf assigns

  let compute_and_return kf =
    let open Current_loc.Operators in
    let<> UpdatedCurrentLoc = Current_loc.get () in
    From_parameters.feedback
      "Computing for function %a%s"
      Kernel_function.pretty kf
      (let s = ref "" in
       Stack.iter
         (fun kf ->
            s := !s^" <-"^(Format.asprintf "%a" Kernel_function.pretty kf))
         call_stack;
       !s);
    Async.yield ();
    let result =
      if Eva.Analysis.use_spec_instead_of_definition kf
      then compute_using_prototype kf
      else compute_using_cfg kf
    in
    let result = To_Use.cleanup_and_save kf result in
    From_parameters.feedback
      "Done for function %a" Kernel_function.pretty kf;
    Async.yield ();
    result

  let compute kf =
    Eva.Analysis.compute ();
    ignore (compute_and_return kf)

end
