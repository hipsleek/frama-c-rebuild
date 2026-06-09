(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let dkey = Wp_parameters.register_category "rte"

module type Option = sig (* hides elements from Parameter_sig.Bool *)
  val get : unit -> bool
  val is_default : unit -> bool
  val name : string
end

type t = {
  name : string ;
  cint : bool ; (* whether the option is impacted by the int arithmetic model *)
  option : (module Option) ;
  status : (module RteGen.Generator.S) ;
}

let configure ~update ~generate kf cint rte =
  let module Option = (val rte.option) in
  let module Generator = (val rte.status) in
  if rte.cint && not @@ Option.get () && generate then
    match cint with
    | Cint.Natural -> (* The option is necessary because of the model *)
      Wp_parameters.warning ~once:true ~current:false
        "-wp-rte and model nat require kernel to warn against %s" rte.name
    | Cint.Machine -> ()
  else
  if not @@ Generator.is_computed kf then
    if Option.get () then begin
      let msg = if generate then "generate" else "missing" in
      Wp_parameters.debug ~dkey "function %a: %s rte for %s"
        Kernel_function.pretty kf msg rte.name ;
      update := true
    end else if not @@ Option.is_default () then begin
      Wp_parameters.warning ~once:true ~current:false
        "-wp-rte can annotate %s because %s is not set"
        rte.name Option.name ;
      (* we want to globally warn on "just-check" mode *)
      update := !update || not generate
    end

module WrapFiniteFloat = struct
  include Kernel.SpecialFloat
  let get () = get () <> "none"
end

let generator =
  [ (* Note: -warn-unaligned-pointer and -rte-pointer-call are not listed here,
       so that we do not warn for missing RTE guards for them. *)
    { name = "memory access" ; cint = false ;
      option = (module RteGen.Options.DoMemAccess) ;
      status = (module RteGen.Generator.Mem_access) } ;
    { name = "division by zero" ; cint = false ;
      option = (module RteGen.Options.DoDivMod) ;
      status = (module RteGen.Generator.Div_mod) } ;
    { name = "signed overflow" ; cint = true ;
      option = (module Kernel.SignedOverflow) ;
      status = (module RteGen.Generator.Signed_overflow) } ;
    { name = "unsigned overflow" ; cint = true ;
      option = (module Kernel.UnsignedOverflow) ;
      status = (module RteGen.Generator.Unsigned_overflow) } ;
    { name = "signed downcast" ; cint = true ;
      option = (module Kernel.SignedDowncast) ;
      status = (module RteGen.Generator.Signed_downcast) } ;
    { name = "unsigned downcast" ; cint = true ;
      option = (module Kernel.UnsignedDowncast) ;
      status = (module RteGen.Generator.Unsigned_downcast) } ;
    { name = "shift" ; cint = true ;
      option = (module RteGen.Options.DoShift) ;
      status = (module RteGen.Generator.Shift) } ;
    { name = "left shift on negative" ; cint = true ;
      option = (module Kernel.LeftShiftNegative) ;
      status = (module RteGen.Generator.Left_shift_negative) } ;
    { name = "right shift on negative" ; cint = false ;
      option = (module Kernel.RightShiftNegative) ;
      status = (module RteGen.Generator.Right_shift_negative) } ;
    { name = "invalid bool value" ; cint = false ;
      option = (module Kernel.InvalidBool) ;
      status = (module RteGen.Generator.Bool_value) } ;
    { name = "pointer downcast" ; cint = false ;
      option = (module Kernel.PointerDowncast) ;
      status = (module RteGen.Generator.Pointer_downcast) } ;
    { name = "invalid pointer" ; cint = false ;
      option = (module Kernel.InvalidPointer) ;
      status = (module RteGen.Generator.Pointer_value) } ;
    { name = "float to int" ; cint = false ;
      option = (module RteGen.Options.DoFloatToInt) ;
      status = (module RteGen.Generator.Float_to_int) } ;
    { name = "special float" ; cint = false ;
      option = (module WrapFiniteFloat) ;
      status = (module RteGen.Generator.Float_to_int) } ;
  ]

(* Initialized is a specific case: it is associated to a set of functions *)

let configure_initialized ~update ~generate kf =
  let module Option = RteGen.Options.DoInitialized in
  (* Note: we do not warn when the function is not mem of the set since there
     are two possibilities for the Option:
     - it is the default: no reason to warn,
     - it is explicitly positioned: we expect that the user correctly set it
  *)
  if Option.mem kf then begin
    let generated = RteGen.Generator.Initialized.is_computed kf in
    if not generated then begin
      let msg = if generate then "generate" else "missing" in
      Wp_parameters.debug ~dkey "function %a: %s rte for initialization"
        Kernel_function.pretty kf msg ;
    end ;
    update := !update || not generated
  end

let print_unsupported ~asked message =
  if asked then
    Wp_parameters.warning ~once:true ~current:false
      "Skipped RTE guards: %s" message

let generate model kf =
  let update = ref false in
  let cint = WpContext.on_context (model,WpContext.Kf kf) Cint.current () in
  List.iter (configure ~update ~generate:true kf cint) generator ;
  configure_initialized ~update ~generate:true kf ;
  if !update then begin
    print_unsupported ~asked:(Kernel.UnalignedPointer.get ())
      "unaligned pointers (\\aligned not supported)" ;
    print_unsupported ~asked:(RteGen.Options.DoPointerCall.get ())
      "invalid function pointer calls (\\valid_function not supported)" ;
    let flags =
      { (RteGen.Flags.default ()) with (* we do not support: *)
        pointer_alignment = false ;    (* - \aligned *)
        pointer_call = false ;         (* - \valid_function *)
      }
    in
    RteGen.Visit.annotate ~flags kf
  end

let generate_all model =
  Wp_parameters.iter_kf (generate model)

let missing_guards model kf =
  let update = ref false in
  let cint = WpContext.on_context (model,WpContext.Kf kf) Cint.current () in
  List.iter (configure ~update ~generate:false kf cint) generator ;
  configure_initialized ~update ~generate:false kf ;
  !update

(* -------------------------------------------------------------------------- *)
