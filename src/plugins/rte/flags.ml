(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Fine Tuning Visitors                                               --- *)
(* -------------------------------------------------------------------------- *)

type t = {
  remove_trivial: bool;
  initialized: Kernel_function.Set.t ;
  mem_access: bool;
  div_mod: bool;
  shift: bool;
  left_shift_negative: bool;
  right_shift_negative: bool;
  signed_overflow: bool;
  unsigned_overflow: bool;
  signed_downcast: bool;
  unsigned_downcast: bool;
  pointer_downcast: bool;
  float_to_int: bool;
  finite_float: bool;
  pointer_call: bool;
  pointer_alignment: bool;
  pointer_value: bool;
  bool_value: bool;
}

let all () = {
  remove_trivial = true;
  initialized =
    Globals.Functions.fold Kernel_function.Set.add Kernel_function.Set.empty;
  mem_access = true;
  div_mod = true;
  shift = true;
  left_shift_negative = true;
  right_shift_negative = true;
  signed_overflow = true;
  unsigned_overflow = true;
  signed_downcast = true;
  unsigned_downcast = true;
  pointer_downcast = true;
  float_to_int = true;
  finite_float = true;
  pointer_call = true;
  pointer_alignment = true;
  pointer_value = true;
  bool_value = true;
}

let none = {
  remove_trivial = false;
  initialized = Kernel_function.Set.empty;
  mem_access = false;
  div_mod = false;
  shift = false;
  left_shift_negative = false;
  right_shift_negative = false;
  signed_overflow = false;
  unsigned_overflow = false;
  signed_downcast = false;
  unsigned_downcast = false;
  pointer_downcast = false;
  float_to_int = false;
  finite_float = false;
  pointer_call = false;
  pointer_alignment = false;
  pointer_value = false;
  bool_value = false;
}

(* Which annotations should be added,
   from local options, or deduced from the options of RTE and the kernel *)

let option get = function None -> get () | Some flag -> flag

let default
    ?remove_trivial
    ?initialized
    ?mem_access
    ?div_mod
    ?shift
    ?left_shift_negative
    ?right_shift_negative
    ?signed_overflow
    ?unsigned_overflow
    ?signed_downcast
    ?unsigned_downcast
    ?pointer_downcast
    ?float_to_int
    ?finite_float
    ?pointer_call
    ?pointer_alignment
    ?pointer_value
    ?bool_value
    () =
  {
    remove_trivial = option (fun () -> not (Options.Trivial.get ())) remove_trivial ;
    initialized = option Options.DoInitialized.get initialized ;
    mem_access = option Options.DoMemAccess.get mem_access ;
    div_mod = option Options.DoDivMod.get div_mod ;
    shift = option Options.DoShift.get shift;
    left_shift_negative = option Kernel.LeftShiftNegative.get left_shift_negative ;
    right_shift_negative = option Kernel.RightShiftNegative.get right_shift_negative ;
    signed_overflow = option Kernel.SignedOverflow.get signed_overflow ;
    unsigned_overflow = option Kernel.UnsignedOverflow.get unsigned_overflow ;
    signed_downcast = option Kernel.SignedDowncast.get signed_downcast ;
    unsigned_downcast = option Kernel.UnsignedDowncast.get unsigned_downcast ;
    pointer_downcast = option Kernel.PointerDowncast.get pointer_downcast ;
    float_to_int = option Options.DoFloatToInt.get float_to_int ;
    finite_float = option (fun () -> Kernel.SpecialFloat.get () <> "none") finite_float ;
    pointer_call = option Options.DoPointerCall.get pointer_call ;
    pointer_alignment = option Kernel.UnalignedPointer.get pointer_alignment;
    pointer_value = option Kernel.InvalidPointer.get pointer_value;
    bool_value = option Kernel.InvalidBool.get bool_value ;
  }

(* -------------------------------------------------------------------------- *)
