(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Runtime Error annotation generation plugin *)

open Cil_types
open Cil_datatype

(* AST inplace visitor for runtime annotation generation *)

(** [kf]: function to annotate
    [flags]: which RTE to generate.
    [register]: the action to perform on each RTE alarm *)
class annot_visitor kf flags on_alarm = object (self)

  inherit Visitor.frama_c_inplace

  val mutable skip_set = Exp.Set.empty
  val mutable skip_initialized_set = Lval.Set.empty

  method private mark_to_skip exp = skip_set <- Exp.Set.add exp skip_set
  method private must_skip exp = Exp.Set.mem exp skip_set

  method private do_initialized () =
    Kernel_function.Set.mem kf flags.Flags.initialized
    && not (Generator.Initialized.is_computed kf)

  method private do_mem_access () =
    flags.Flags.mem_access && not (Generator.Mem_access.is_computed kf)

  method private do_div_mod () =
    flags.Flags.div_mod && not (Generator.Div_mod.is_computed kf)

  method private do_shift () =
    flags.Flags.shift && not (Generator.Shift.is_computed kf)

  method private do_left_shift_negative () =
    flags.Flags.left_shift_negative
    && not (Generator.Left_shift_negative.is_computed kf)

  method private do_right_shift_negative () =
    flags.Flags.right_shift_negative
    && not (Generator.Right_shift_negative.is_computed kf)

  method private do_signed_overflow () =
    flags.Flags.signed_overflow
    && not (Generator.Signed_overflow.is_computed kf)

  method private do_unsigned_overflow () =
    flags.Flags.unsigned_overflow
    && not (Generator.Unsigned_overflow.is_computed kf)

  method private do_signed_downcast () =
    flags.Flags.signed_downcast
    && not (Generator.Signed_downcast.is_computed kf)

  method private do_unsigned_downcast () =
    flags.Flags.unsigned_downcast
    && not (Generator.Unsigned_downcast.is_computed kf)

  method private do_pointer_downcast () =
    flags.Flags.pointer_downcast
    && not (Generator.Pointer_downcast.is_computed kf)

  method private do_float_to_int () =
    flags.Flags.float_to_int && not (Generator.Float_to_int.is_computed kf)

  method private do_finite_float () =
    flags.Flags.finite_float && not (Generator.Finite_float.is_computed kf)

  method private do_pointer_call () =
    flags.Flags.pointer_call && not (Generator.Pointer_call.is_computed kf)

  method private do_pointer_alignment () =
    flags.Flags.pointer_alignment && not (Generator.Pointer_alignment.is_computed kf)

  method private do_pointer_value () =
    flags.Flags.pointer_value && not (Generator.Pointer_value.is_computed kf)

  method private do_bool_value () =
    flags.Flags.bool_value && not (Generator.Bool_value.is_computed kf)

  method private queue_stmt_spec spec =
    let stmt = Option.get (self#current_stmt) in
    Queue.add
      (fun () ->
         let annot = Logic_const.new_code_annotation (AStmtSpec ([], spec)) in
         Annotations.add_code_annot Generator.emitter ~kf stmt annot)
      self#get_filling_actions

  method private generate_assertion: 'a. 'a Rte.alarm_gen -> 'a -> unit =
    fun fgen ->
    let curr_stmt = self#current_stmt in
    let on_alarm ~invalid a =
      match curr_stmt with
      | None -> Options.warning ~current:true
                  "Alarm generated outside any statement:@ %a"
                  Alarms.pretty a
      | Some stmt -> on_alarm stmt ~invalid a
    in
    fgen ~remove_trivial:flags.Flags.remove_trivial ~on_alarm

  (* Do not visit variable declarations, as no alarm should be emitted here,
     and there is no statement to emit an alarm anyway ([generate_assertion]
     or [Alarms.register] would then crash). *)
  method !vvdec _ = Cil.SkipChildren

  method! vstmt s = match s.skind with
    | UnspecifiedSequence l ->
      (* UnspecifiedSequences may contain lvals for side-effects, that
         give rise to spurious assertions *)
      let no_lval = List.map (fun (s, _, _, _, sref) -> s, [], [], [], sref) l in
      let s' = { s with skind = UnspecifiedSequence no_lval } in
      Cil.ChangeDoChildrenPost (s', fun _ -> s)
    | _ -> Cil.DoChildren

  method private check_mem_access ~kind dest =
    if self#do_mem_access () then begin
      Options.debug "lval %a: validity of potential mem access checked\n"
        Printer.pp_lval dest;
      self#generate_assertion (Rte.lval_assertion ~read_only:kind) dest
    end

  method private check_aligned ptr typ =
    if self#do_pointer_alignment () then begin
      Options.debug "exp %a: validity of potential unaligned_pointer checked\n"
        Printer.pp_exp ptr;
      self#generate_assertion Rte.pointer_alignment (ptr, typ)
    end

  method private mark_to_skip_initialized lv =
    skip_initialized_set <- Lval.Set.add lv skip_initialized_set

  method private must_skip_initialized lv =
    (* Will return true only once per mark_to_skip_initialized
       for the same lval *)
    let r = Lval.Set.mem lv skip_initialized_set in
    if r then skip_initialized_set <- Lval.Set.remove lv skip_initialized_set;
    r

  method private check_initialized lval =
    if self#do_initialized () && not (self#must_skip_initialized lval)
    then begin
      Options.debug "lval %a: initialization of potential mem access checked"
        Printer.pp_lval lval ;
      self#generate_assertion Rte.lval_initialized_assertion lval
    end


  method private check_aligned_access lval =
    match lval with
    | Mem e, _ -> self#check_aligned e (Cil.typeOf e)
    | _ -> ()

  method private check_pointer_value ptr =
    if self#do_pointer_value ()
    then self#generate_assertion Rte.pointer_value ptr

  (* assigned left values are checked for valid access *)
  method! vinst = function
    | Set (lval,_,_) ->
      self#check_aligned_access lval ;
      self#check_mem_access ~kind:Alarms.For_writing lval ;
      Cil.DoChildren
    | Call (ret_opt,func,argl,_) ->
      (* Do not emit alarms on Eva builtins such as Frama_C_show_each, that should
         have no effect on analyses. *)
      let is_builtin, is_va_start =
        match func with
        | Var vinfo ->
          let kf = Globals.Functions.get vinfo in
          let frama_b = Ast_info.start_with_frama_c_builtin (Kernel_function.get_name kf)
          in
          let fname = Kernel_function.get_name kf in
          let va_start =
            fname = "__builtin_va_start" || fname = "__builtin_c23_va_start"
          in
          (frama_b, va_start)
        | _ -> (false, false)
      in
      if is_va_start then begin
        match (List.nth argl 0).enode with
        | Lval lv -> self#mark_to_skip_initialized lv
        | _ -> ()
      end ;
      if is_builtin
      then Cil.SkipChildren
      else begin
        Option.iter self#check_aligned_access ret_opt ;
        Option.iter (self#check_mem_access ~kind:Alarms.For_writing) ret_opt ;
        (* Alarm if the call is through a pointer. Done in DoChildrenPost to get a
           more pleasant ordering of annotations. *)
        let do_ptr () =
          if self#do_pointer_call () then
            match func with
            | Mem e -> self#generate_assertion Rte.pointer_call (e, argl)
            | _ -> ()
        in
        Cil.DoChildrenPost (fun res -> do_ptr (); res)
      end
    | Local_init (v,ConsInit(f,args,kind),loc) ->
      let do_call lv _e _args _loc =
        Option.iter (self#check_mem_access ~kind:Alarms.For_writing) lv in
      Cil.treat_constructor_as_func do_call v f args kind loc;
      Cil.DoChildren
    | Local_init (v,AssignInit (SingleInit _),_) ->
      self#check_mem_access ~kind:Alarms.For_writing (Cil.var v) ;
      Cil.DoChildren
    | Local_init (_,AssignInit _,_)
    | Asm _ | Skip _ | Code_annot _ -> Cil.DoChildren

  method! vexpr exp =
    Options.debug "considering exp %a\n" Printer.pp_exp exp;
    match exp.enode with
    | SizeOf _
    | SizeOfE _
    | AlignOf _
    | AlignOfE _ -> Cil.SkipChildren
    | _ ->
      let generate () =
        match exp.enode with
        | BinOp((Div | Mod), lexp, rexp, ty) ->
          (match Ast_types.unroll_node ty with
           | TInt kind ->
             (* add assertion "divisor not zero" *)
             if self#do_div_mod () then
               self#generate_assertion Rte.divmod_assertion rexp;
             if self#do_signed_overflow () && Cil.isSigned kind then
               (* treat the special case of signed division/modulo overflow *)
               let exp = { exp with enode = BinOp (Div, lexp, rexp, ty) } in
               self#generate_assertion Rte.signed_div_assertion (exp, lexp, rexp)
           | TFloat fkind when self#do_finite_float () ->
             self#generate_assertion Rte.finite_float_assertion (fkind,exp);
           | _ -> ())

        | BinOp((Shiftlt | Shiftrt) as op, lexp, rexp,ttype ) ->
          (match Ast_types.unroll_node ttype with
           | TInt kind ->
             (* 0 <= rexp <= width *)
             if self#do_shift () then begin
               let typ = Ast_types.unroll (Cil.typeOf exp) in
               (* Not really a problem of overflow, but almost a similar to self#do_div_mod *)
               self#generate_assertion Rte.shift_width_assertion (rexp, typ);
             end;
             let signed = Cil.isSigned kind in
             (* 0 <= lexp *)
             if signed &&
                (op = Shiftlt && self#do_left_shift_negative ()
                 || op = Shiftrt && self#do_right_shift_negative ())
             then self#generate_assertion Rte.shift_negative_assertion lexp;
             (* Signed or unsigned overflow. *)
             if self#do_signed_overflow () && signed
             || self#do_unsigned_overflow () && not signed
             then
               self#generate_assertion
                 (Rte.shift_overflow_assertion ~signed) (exp, op, lexp, rexp)
           | _ -> ())

        | BinOp((PlusA |MinusA | Mult) as op, lexp, rexp, ttype) ->
          (* may be skipped if the enclosing expression is a downcast to a signed
             type *)
          (match Ast_types.unroll_node ttype with
           | TInt kind when Cil.isSigned kind ->
             if self#do_signed_overflow () && not (self#must_skip exp) then
               self#generate_assertion
                 (Rte.mult_sub_add_assertion ~signed:true)
                 (exp, op, lexp, rexp)
           | TInt kind when not (Cil.isSigned kind) ->
             if self#do_unsigned_overflow () then
               self#generate_assertion
                 (Rte.mult_sub_add_assertion ~signed:false)
                 (exp, op, lexp, rexp)
           | TFloat fkind when self#do_finite_float () ->
             self#generate_assertion Rte.finite_float_assertion (fkind,exp)
           | _ -> ())

        | BinOp((PlusPI | MinusPI), _, _, _)  ->
          self#check_pointer_value exp

        | UnOp(Neg, exp, ty) ->
          (* Note: if unary minus on unsigned integer is to be understood as
             "subtracting the promoted value from the largest value
             of the promoted type and adding one",
             the result is always representable: so no overflow *)
          (match Ast_types.unroll_node ty with
           | TInt kind when Cil.isSigned kind ->
             if self#do_signed_overflow () then
               self#generate_assertion Rte.uminus_assertion exp;
           | TFloat fkind when self#do_finite_float () ->
             self#generate_assertion Rte.finite_float_assertion (fkind,exp)
           | _ -> ())

        | Lval lval ->
          (match Ast_types.unroll_node (Cil.typeOfLval lval) with
           | TPtr _ ->
             (* Note: here we are forced to make these checks because we do not
                control strict aliasing violation, thus a pointer might be
                crafted with indirect operations like copying bytes. Even if at
                some point we support strict aliasing violation checks, there is
                certainly code out there that use -fno-strict-aliasing and that
                would disable the warning. Consequently, these checks would
                remain mandatory when the warning is disabled. *)
             self#check_pointer_value exp ;
             self#check_aligned exp (Cil.typeOf exp) ;
           | TInt IBool when self#do_bool_value () ->
             (* The same remark applies here. *)
             self#generate_assertion Rte.bool_value lval
           | _ -> ());
          self#check_aligned_access lval ;
          self#check_mem_access ~kind:Alarms.For_reading lval ;
          self#check_initialized lval

        | CastE (ty, e) ->
          (match Ast_types.unroll_node ty,  Ast_types.unroll_node (Cil.typeOf e) with
           (* to , from *)
           | TInt _, TPtr _ when self#do_pointer_downcast () ->
             self#generate_assertion Rte.downcast_assertion (ty, e)

           | TPtr _, TInt _ ->
             (* keep cast here, else, we do not have a pointer anymore to
                emit the alarm. *)
             self#check_pointer_value exp ;
             self#check_aligned exp ty

           | TPtr _, TPtr _ ->
             self#check_aligned e ty

           | TInt kind, TInt _ ->
             let signed = Cil.isSigned kind in
             if signed && self#do_signed_downcast ()
             || not signed && self#do_unsigned_downcast ()
             then self#generate_assertion Rte.downcast_assertion (ty, e);
             if signed && self#do_signed_downcast ()
             then self#mark_to_skip e;

           | TInt _, TFloat _ ->
             if self#do_float_to_int () then
               self#generate_assertion Rte.float_to_int_assertion (ty, e)

           | TFloat to_fkind, TFloat from_fkind when
               self#do_finite_float () && Cil.frank to_fkind < Cil.frank from_fkind ->
             self#generate_assertion Rte.finite_float_assertion (to_fkind,exp)
           | _ -> ());
        | Const (CReal(f,fkind,_)) when self#do_finite_float () ->
          begin match classify_float f with
            | FP_normal
            | FP_subnormal
            | FP_zero -> ()
            | FP_infinite
            | FP_nan ->
              self#generate_assertion Rte.finite_float_assertion (fkind,exp)
          end
        | StartOf _ | AddrOf _ ->
          self#check_pointer_value exp
        | UnOp _
        | Const _
        | BinOp _ -> ()
        | SizeOf _
        | SizeOfE _
        | AlignOf _
        | AlignOfE _ -> assert false
      in
      (* Use Cil.DoChildrenPost so that inner expression and lvals are
         checked first. The order of resulting assertions will be better. *)
      Cil.DoChildrenPost (fun new_e -> generate (); new_e)

end

let visit_with_stmt visit kf flags on_alarm stmt element =
  if not (Options.use_eva_results () && Eva_analysis.is_computed kf)
  then
    let visitor = object (self)
      inherit annot_visitor kf flags on_alarm
      initializer self#push_stmt stmt
    end in
    ignore (visit (visitor :> Cil.cilVisitor) element)

(** {2 Iterate over Alarms on Cil elements} *)

type on_alarm = kernel_function -> stmt -> invalid:bool -> Alarms.alarm -> unit

let filter = function None -> Flags.default () | Some flags -> flags

let iter_alarms visit ?flags (on_alarm:on_alarm) kf stmt element =
  visit_with_stmt visit kf (filter flags) (on_alarm kf) stmt element

type 'a iterator =
  ?flags:Flags.t -> on_alarm ->
  Kernel_function.t -> Cil_types.stmt -> 'a -> unit

let iter_lval : lval iterator = iter_alarms Cil.visitCilLval
let iter_exp : exp iterator = iter_alarms Cil.visitCilExpr
let iter_instr : instr iterator = iter_alarms Cil.visitCilInstr
let iter_stmt : stmt iterator = iter_alarms Cil.visitCilStmt

(** {2 Registration} *)

let status ~invalid =
  if invalid then Some Property_status.False_if_reachable else None

let register emitter kf stmt ~invalid alarm =
  let status = status ~invalid in
  Alarms.register emitter ~kf (Kstmt stmt) ?status alarm

(* -------------------------------------------------------------------------- *)
(* --- List Code Annotations                                              --- *)
(* -------------------------------------------------------------------------- *)

let collector () =
  let pool = ref [] in
  let on_alarm stmt ~invalid:_ alarm =
    let ca, _ = Alarms.to_annot (Kstmt stmt) alarm in
    pool := ca :: !pool ;
  in pool , on_alarm

let get_annotations_kf ?flags kf =
  let visit_function kf flags on_alarm =
    if not (Options.use_eva_results () && Eva_analysis.is_computed kf)
    then match kf.fundec with
      | Declaration _ -> ()
      | Definition(f, _) ->
        let visitor = new annot_visitor kf flags on_alarm in
        ignore (Visitor.visitFramacFunction visitor f)
  in
  let pool,on_alarm = collector () in
  visit_function kf (filter flags) on_alarm ;
  !pool

let collect visit flags kf stmt elt =
  let pool,on_alarm = collector () in
  visit_with_stmt visit kf (filter flags) on_alarm stmt elt;
  !pool

let get_annotations_stmt ?flags kf stmt =
  collect Cil.visitCilStmt flags kf stmt stmt

let get_annotations_exp ?flags kf stmt exp =
  collect Cil.visitCilExpr flags kf stmt exp

let get_annotations_lval ?flags kf stmt lv =
  collect Cil.visitCilLval flags kf stmt lv

(** {2 Annotations of kernel_functions for a given type of RTE} *)

(* generates annotation for function kf on the basis of [flags] *)
let annotate ?flags kf =
  let flags = filter flags in
  Options.debug "annotating function %a" Kernel_function.pretty kf;
  (* This reference contains all the RTE statuses that should be positioned
     once this function has been annotated. *)
  let to_update = ref [] in
  (* Check whether there is something to compute + lists all the statuses
     that will be ultimately updated *)
  let comp (_name, set, is_computed) should_compute =
    if should_compute && not (is_computed kf) then begin
      to_update := (fun () -> set kf true) :: !to_update;
      true
    end
    else false
  in
  (* Strict version of ||, because [comp] has side-effects *)
  let (|||) a b = a || b in
  let open Generator in
  let open Flags in
  if comp Initialized.accessor
      (not @@ Kernel_function.Set.is_empty flags.initialized) |||
     comp Mem_access.accessor flags.mem_access |||
     comp Pointer_value.accessor flags.pointer_value |||
     comp Pointer_call.accessor flags.pointer_call |||
     comp Div_mod.accessor flags.div_mod |||
     comp Shift.accessor flags.shift |||
     comp Left_shift_negative.accessor flags.left_shift_negative |||
     comp Right_shift_negative.accessor flags.right_shift_negative |||
     comp Signed_overflow.accessor flags.signed_overflow |||
     comp Signed_downcast.accessor flags.signed_downcast |||
     comp Unsigned_overflow.accessor flags.unsigned_overflow |||
     comp Unsigned_downcast.accessor flags.unsigned_downcast |||
     comp Pointer_downcast.accessor flags.pointer_downcast |||
     comp Float_to_int.accessor flags.float_to_int |||
     comp Finite_float.accessor flags.finite_float |||
     comp Bool_value.accessor flags.bool_value
  then begin
    let visit_function kf flags on_alarm =
      if not (Options.use_eva_results () && Eva_analysis.is_computed kf)
      then match kf.fundec with
        | Declaration _ -> ()
        | Definition(f, _) ->
          Options.feedback ~dkey:Options.dkey_annot
            "annotating function %a" Kernel_function.pretty kf;
          let visitor = new annot_visitor kf flags on_alarm in
          ignore (Visitor.visitFramacFunction visitor f)
    in
    let warn = Options.Warn.get () in
    let on_alarm stmt ~invalid alarm =
      let ca, _ = register Generator.emitter kf stmt ~invalid alarm in
      if warn && invalid then
        Options.warning ~current:true ~once:true "@[guaranteed RTE:@ %a@]"
          Printer.pp_code_annotation ca
    in
    ignore @@ visit_function kf flags on_alarm ;
    List.iter (fun f -> f ()) !to_update;
  end
