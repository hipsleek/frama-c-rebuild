(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Error = struct
  let error ?source ?once ?current =
    Kernel.warning ~wkey:Kernel.wkey_ghost_bad_use ?source ?once ?current

  let cannot_check_ghost_call ?source ?once ?current kf =
    error ?source ?current ?once
      "No definition, nor assigns specification for ghost function '%s'"
      (Kernel_function.get_name kf)

  let invalid_ghost_type_for_varinfo ?source ?once ?current v =
    error ?source ?current ?once
      "Invalid type for '%s': indirection from non-ghost to ghost" v.vname

  let invalid_ghost_type_for_return ?source ?once ?current () =
    error ?source ?once ?current
      "Invalid return type: indirection from non-ghost to ghost"

  let assigns_non_ghost_term ?source ?once ?current t =
    error ?source ?once ?current
      "'%a' is a non-ghost lvalue, it cannot be assigned in ghost code"
      Cil_printer.pp_term t

  let assigns_non_ghost_lvalue ?source ?once ?current lv =
    error ?source ?once ?current
      "'%a' is a non-ghost lvalue, it cannot be assigned in ghost code"
      Cil_printer.pp_lval lv

  let bad_cast_on_return ?source ?once ?current f ret_type lv =
    error ?source ?once ?current
      "Cannot cast return of '%a' from '%a' to '%a'"
      Cil_printer.pp_lhost f
      Cil_printer.pp_typ ret_type
      Cil_printer.pp_typ (Cil.typeOfLval lv)

  let bad_cast ?source ?once ?current exp t =
    error ?source ?once ?current
      "Invalid cast of '%a' from '%a' to '%a'"
      Cil_printer.pp_exp exp
      Cil_printer.pp_typ (Cil.typeOf exp)
      Cil_printer.pp_typ t

  let non_ghost_function_call_in_ghost ?source ?once ?current () =
    error ?source ?once ?current
      "Call to non-ghost function from ghost code is not allowed"

  let function_pointer_call ?source ?once ?current () =
    error ?source ?once ?current
      "Call via a function pointer cannot be checked" ;
end

let is_generated_ret_var vi =
  String.equal vi.vorig_name "__retres"

let get_lvalue = function
  | Set(lv, _, _) | Call(Some(lv), _, _, _) -> Some lv
  | Local_init(vi, _, _) -> Some (Cil.var vi)
  | _ -> None

let rec ghost_term_type t =
  match (Ast_types.unroll_logic t) with
  | Ctype t -> Ast_types.is_ghost t
  | t when Logic_const.is_set_type t ->
    ghost_term_type (Logic_const.type_of_element t)
  | _ -> false

class visitor = object(self)
  inherit Visitor.frama_c_inplace

  method private ghost_incompatible nt ot =
    match Ast_types.unroll_node nt, Ast_types.unroll_node ot with
    | TPtr nt', TPtr ot'
    | TPtr nt', TArray (ot', _) ->
      Ast_types.is_ghost nt' <> Ast_types.is_ghost ot' ||
      self#ghost_incompatible nt' ot'
    | _ ->
      false

  val mutable preferred_loc = None
  method private prefer_loc l = preferred_loc <- Some l
  method private reset_loc () = preferred_loc <- None

  method private ghost_stmt () =
    match self#current_stmt with
    | Some s -> s.ghost
    | _ -> false
  method private in_ghost_section () =
    match self#current_kf with
    | Some kf -> (Kernel_function.get_vi kf).vghost || self#ghost_stmt ()
    | None -> false

  method private bad_ghost_function kf =
    let source = match preferred_loc with
      | None -> fst (Current_loc.get())
      | Some l -> l
    in
    Error.cannot_check_ghost_call ~source ~current:true ~once:true kf

  method! vglob_aux = function
    | GFunDecl(_, vi, _) when vi.vghost ->
      self#prefer_loc (Log.get_current_source ()) ;
      let post g = self#reset_loc() ; g in

      let kf = Globals.Functions.get vi in
      if vi.vghost && vi.vreferenced
         && not (Kernel_function.has_definition kf)
      then begin
        let spec =
          try Annotations.funspec kf
          with _ -> Cil.empty_funspec ()
        in
        if Cil.is_empty_funspec spec then self#bad_ghost_function kf
      end ;
      Cil.DoChildrenPost post
    (* Optimization: only visits ghost globals or globals that may contain
       ghost code. *)
    | GVarDecl (vi, _) when vi.vghost -> Cil.DoChildren
    | GVar _ | GFun _ -> Cil.DoChildren
    | _ -> Cil.SkipChildren

  method! vvdec v =
    let current, source =
      match preferred_loc with
      | Some l -> false, l
      | None -> true, (Log.get_current_source ())
    in
    if not(is_generated_ret_var v)
    && not (Ast_types.is_wellformed_ghost v.vtype) then
      Error.invalid_ghost_type_for_varinfo ~once:true ~current ~source v ;
    if Ast_types.(is_fun (unroll v.vtype)) then begin
      let ftype = Cil.getReturnType (Ast_types.unroll v.vtype) in
      match ftype.tnode with
      | TPtr t when not (Ast_types.is_wellformed_ghost t) ->
        Error.invalid_ghost_type_for_return ~once:true ~current ~source ()
      | _ -> ()
    end ;
    Cil.DoChildren

  method! vinst i =
    if not (self#in_ghost_section ()) then
      (* Non ghost code has already been checked by Cabs2Cil *)
      Cil.DoChildren
    else begin
      let error_if_incompatible lv ret_type f =
        if self#ghost_incompatible (Cil.typeOfLval lv) ret_type then
          Error.bad_cast_on_return ~current:true f ret_type lv
      in
      let error_if_not_writable lv =
        if not (Ast_types.is_ghost (Cil.typeOfLval lv)) then
          Error.assigns_non_ghost_lvalue ~current:true lv
      in
      let failed = match i with
        | Call(_, f, _, _) ->
          begin match Kernel_function.(Option.map get_vi @@ get_called f) with
            | Some fct
              when not (Ast_info.is_frama_c_builtin fct) && not fct.vghost ->
              Error.non_ghost_function_call_in_ghost ~current:true () ; true
            | None ->
              Error.function_pointer_call ~current:true () ; true
            | _ -> false
          end
        | Local_init(_, ConsInit(fct, _, _), _)
          when not (Ast_info.is_frama_c_builtin fct) && not fct.vghost ->
          Error.non_ghost_function_call_in_ghost ~current:true () ; true
        | _ -> false
      in
      if failed then SkipChildren
      else begin
        begin match (get_lvalue i) with
          | Some lv -> begin
              error_if_not_writable lv ;
              match i with
              | Call(_, f, _, _) ->
                let vi =
                  Kernel_function.(get_vi @@ Option.get @@ get_called f) in
                if not (Ast_info.is_frama_c_builtin vi) then
                  error_if_incompatible lv (Cil.getReturnType (Cil.typeOfLhost f)) f
              | Local_init(_, ConsInit(fct, _, _), _) ->
                if not (Ast_info.is_frama_c_builtin fct) then
                  error_if_incompatible lv (Cil.getReturnType fct.vtype) (Var fct)
              | _ -> ()
            end
          (* Note that we do not check "assigns" for a ghost function call since
             the method vglob_aux and vassigns requires ghost functions to have
             either a contract or a definition.
             Thus, if the casts in input are valid:
             - either the assigns specification is valid wrt the types of the
               parameters
             - or the body of the function does not break the typing
          *)
          | _ -> ()
        end ;
        DoChildren
      end
    end

  method! vexpr = function
    | { enode = CastE (t, exp) }
      when self#ghost_incompatible t (Cil.typeOf exp) ->
      Error.bad_cast ~current:true exp t ;
      DoChildren
    | _ -> DoChildren

  method! vassigns assigns =
    if self#in_ghost_section () then begin
      let check_assign = function (id_term, _) ->
        let term = id_term.it_content in
        if not (ghost_term_type term.term_type) then
          Error.assigns_non_ghost_term ~current:true term
      in
      let kf = Option.get (self#current_kf) in
      match assigns with
      | Writes froms -> List.iter check_assign froms
      | WritesAny when not (Kernel_function.has_definition kf) ->
        self#bad_ghost_function kf
      | WritesAny ->
        (* Even without assigns, a definition is enough to check that the
           function does not assigns non-ghost locations. *)
        ()
    end ;
    Cil.DoChildren
end

let checkGhostAccesses f =
  Visitor.visitFramacFileSameGlobals (new visitor) f

let transform_category =
  File.register_code_transformation_category "Ghost Accesses checking"

let () =
  File.add_code_transformation_after_cleanup
    ~after:[Ghost_cfg.transform_category]
    transform_category checkGhostAccesses
