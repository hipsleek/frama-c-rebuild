(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Make_pp
    (P: sig val printer : unit -> Printer_api.extensible_printer_type end) =
struct

  let printer = P.printer

  (** If "-kernel-msg-key printer:debug" flag is provided,
      use Cil_types.pp_* printer instead of normal pretty-printer. *)
  let pick pp_debug pp_normal fmt x =
    if Kernel.is_debug_key_enabled Kernel.dkey_print_debug
    then pp_debug fmt x
    else pp_normal fmt x
  let pp_list fmt = Pretty_utils.pp_list fmt ~sep:"; " ~pre:"[" ~suf:"]" ~empty:"[]"

  let without_annot f fmt x = (printer ())#without_annot f fmt x
  let force_brace f fmt x = (printer ())#force_brace f fmt x
  let pp_varname fmt x = pick Format.pp_print_string (printer ())#varname fmt x
  let pp_constant fmt x = pick Cil_types.pp_constant (printer ())#constant fmt x
  let pp_ikind fmt x = pick Cil_types.pp_ikind (printer ())#ikind fmt x
  let pp_fkind fmt x = pick Cil_types.pp_fkind (printer ())#fkind fmt x
  let pp_storage fmt x = pick Cil_types.pp_storage (printer ())#storage fmt x
  let pp_typ fmt x = pick Cil_types.pp_typ ((printer ())#typ None) fmt x
  let pp_exp fmt x = pick Cil_types.pp_exp (printer ())#exp fmt x
  let pp_vdecl fmt x = pick Cil_types.pp_varinfo (printer ())#vdecl fmt x
  let pp_varinfo fmt x = pick Cil_types.pp_varinfo (printer ())#varinfo fmt x
  let pp_lhost fmt x = pick Cil_types.pp_lhost (printer())#lhost fmt x
  let pp_lval fmt x = pick Cil_types.pp_lval (printer ())#lval fmt x
  let pp_field fmt x = pick Cil_types.pp_fieldinfo (printer())#field fmt x
  let pp_offset fmt x = pick Cil_types.pp_offset (printer ())#offset fmt x
  let pp_init fmt x = pick Cil_types.pp_init (printer ())#init fmt x
  let pp_init_or_str fmt x =
    pick Cil_types.pp_init_or_str (printer())#init_or_str fmt x
  let pp_str_literal fmt x =
    pick Cil_types.pp_str_literal (printer())#str_literal fmt x
  let pp_binop fmt x = pick Cil_types.pp_binop (printer ())#binop fmt x
  let pp_unop fmt x = pick Cil_types.pp_unop (printer ())#unop fmt x
  let pp_attribute fmt x =
    pick
      Cil_types.pp_attribute
      (fun fmt x -> ignore ((printer ())#attribute fmt x))
      fmt x
  let pp_attrparam fmt x = pick Cil_types.pp_attrparam (printer ())#attrparam fmt x
  let pp_attributes fmt x =
    pick (pp_list Cil_types.pp_attribute) (printer ())#attributes fmt x
  let pp_instr fmt x = pick Cil_types.pp_instr (printer ())#instr fmt x
  let pp_label fmt x = pick Cil_types.pp_label (printer ())#label fmt x
  let pp_logic_builtin_label fmt x =
    pick Cil_types.pp_logic_builtin_label (printer ())#logic_builtin_label fmt x
  let pp_logic_label fmt x =
    pick Cil_types.pp_logic_label (printer ())#logic_label fmt x
  let pp_stmt fmt x = pick Cil_types.pp_stmt (printer ())#stmt fmt x
  let pp_block fmt x = pick Cil_types.pp_block (printer ())#block fmt x
  let pp_global fmt x = pick Cil_types.pp_global (printer ())#global fmt x

  (* [pp_file] is used to output code, so never use debug printer here. *)
  let pp_file fmt x = (printer ())#file fmt x

  let pp_relation fmt x = pick Cil_types.pp_relation (printer ())#relation fmt x
  let pp_model_info fmt x =
    pick Cil_types.pp_model_info (printer ())#model_info fmt x
  let pp_term_lval fmt x = pick Cil_types.pp_term_lval (printer ())#term_lval fmt x
  let pp_logic_var fmt x = pick Cil_types.pp_logic_var (printer ())#logic_var fmt x
  let pp_logic_type fmt x =
    pick Cil_types.pp_logic_type ((printer ())#logic_type None) fmt x
  let pp_identified_term fmt x =
    pick Cil_types.pp_identified_term (printer ())#identified_term fmt x
  let pp_term fmt x = pick Cil_types.pp_term (printer ())#term fmt x
  let pp_model_field fmt x =
    pick Cil_types.pp_model_info (printer())#model_field fmt x
  let pp_term_offset fmt x =
    pick Cil_types.pp_term_offset (printer ())#term_offset fmt x
  let pp_predicate_node fmt x =
    pick Cil_types.pp_predicate_node (printer ())#predicate_node fmt x
  let pp_predicate fmt x = pick Cil_types.pp_predicate (printer ())#predicate fmt x
  let pp_toplevel_predicate fmt x =
    pick
      Cil_types.pp_toplevel_predicate
      (fun fmt tp -> (printer())#predicate fmt tp.Cil_types.tp_statement)
      fmt x
  let pp_identified_predicate fmt x =
    pick Cil_types.pp_identified_predicate (printer ())#identified_predicate fmt x
  let pp_code_annotation fmt x =
    pick Cil_types.pp_code_annotation (printer ())#code_annotation fmt x
  let pp_funspec fmt x = pick Cil_types.pp_funspec (printer ())#funspec fmt x
  let pp_behavior fmt x = pick Cil_types.pp_behavior (printer ())#behavior fmt x
  let pp_global_annotation fmt x =
    pick Cil_types.pp_global_annotation (printer ())#global_annotation fmt x
  let pp_decreases fmt x = pick Cil_types.pp_variant (printer ())#decreases fmt x
  let pp_variant fmt x = pick Cil_types.pp_variant (printer ())#variant fmt x
  let pp_from fmt x = pick Cil_types.pp_from ((printer ())#from "assigns") fmt x
  let pp_full_assigns str fmt x =
    pick Cil_types.pp_assigns ((printer ())#assigns str) fmt x
  let pp_assigns fmt x = pp_full_assigns "assigns" fmt x
  let pp_allocation fmt x =
    pick Cil_types.pp_allocation ((printer ())#allocation ~isloop:false) fmt x
  let pp_loop_from fmt x =
    pick Cil_types.pp_from ((printer ())#from "loop assigns") fmt x
  let pp_loop_assigns fmt x =
    pick Cil_types.pp_assigns ((printer ())#assigns "loop assigns") fmt x
  let pp_loop_allocation fmt x =
    pick Cil_types.pp_allocation ((printer ())#allocation ~isloop:true) fmt x
  let pp_post_cond fmt x =
    pick
      Cil_types.(Pretty_utils.pp_pair pp_termination_kind pp_identified_predicate)
      (printer ())#post_cond
      fmt x
  let pp_compinfo fmt x = pick Cil_types.pp_compinfo (printer ())#compinfo fmt x
  let pp_builtin_logic_info fmt x =
    pick Cil_types.pp_builtin_logic_info (printer ())#builtin_logic_info fmt x
  let pp_logic_type_info fmt x =
    pick Cil_types.pp_logic_type_info (printer ())#logic_type_info fmt x
  let pp_logic_ctor_info fmt x =
    pick Cil_types.pp_logic_ctor_info (printer ())#logic_ctor_info fmt x
  let pp_extended fmt x = pick Cil_types.pp_acsl_extension (printer())#extended fmt x
  let pp_short_extended fmt x =
    pick Cil_types.pp_acsl_extension (printer())#short_extended fmt x
  let pp_initinfo fmt x = pick Cil_types.pp_initinfo (printer ())#initinfo fmt x
  let pp_logic_info fmt x = pick Cil_types.pp_logic_info (printer ())#logic_info fmt x
  let pp_logic_constant fmt x =
    pick Cil_types.pp_logic_constant (printer ())#logic_constant fmt x
  let pp_term_lhost fmt x = pick Cil_types.pp_term_lhost (printer ())#term_lhost fmt x
  let pp_fundec fmt x = pick Cil_types.pp_fundec (printer ())#fundec fmt x

end


module Make
    (P: sig class printer: unit -> Printer_api.extensible_printer_type end) =
struct

  module type PrinterClass = sig
    class printer : unit -> Printer_api.extensible_printer_type
  end

  let printer_class_ref =
    ref (module P: PrinterClass)

  let printer_ref = ref None

  module type PrinterExtension = functor (_: PrinterClass) -> PrinterClass

  let set_printer p =
    printer_class_ref := p;
    printer_ref := None

  let update_printer x =
    let module X = (val x: PrinterExtension) in
    let module Cur = (val !printer_class_ref: PrinterClass) in
    let module Updated = X(Cur) in
    set_printer (module Updated: PrinterClass)

  let printer () : Printer_api.extensible_printer_type =
    match !printer_ref with
    | None ->
      let module Printer = (val !printer_class_ref: PrinterClass) in
      let p = new Printer.printer () in
      printer_ref := Some p;
      p#reset ();
      p
    | Some p ->
      p#reset ();
      p

  let current_printer () = !printer_class_ref

  class extensible_printer = P.printer

  include Make_pp(struct let printer = printer end)

end
