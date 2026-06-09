(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type status_accessor =
  string
  * (Cil_types.kernel_function -> bool -> unit)
  * (Cil_types.kernel_function -> bool)

module type S = sig
  val is_computed: kernel_function -> bool
  val set: kernel_function -> bool -> unit
  val accessor: status_accessor
end

let states : State.t list ref = ref []
let accessors : status_accessor list ref = ref []

module Make
    (M:sig
       val name:string
       val parameter: Typed_parameter.t
       val additional_parameters: Typed_parameter.t list
       val kernel_active: unit -> bool
     end)
=
struct

  module H =
    Kernel_function.Make_Table
      (Datatype.Bool)
      (struct
        let name = "RTE.Computed." ^ M.name
        let size = 17
        let dependencies =
          let extract p = State.get p.Typed_parameter.name in
          Ast.self
          :: Options.Trivial.self
          :: List.map extract (M.parameter :: M.additional_parameters)
      end)

  let is_computed =
    (* Nothing to do for functions without body. *)
    let default kf = not (Kernel_function.is_definition kf) in
    fun kf ->
      (* TODO: Ok, this is far from perfect. Since the kernel does not
         centralize alarms management, one might ask RTE whether alarms
         have been emitted even if RTE itself has not been started. In
         this case, if RTE is configured to use Eva results, it checks
         whether Eva emitted these alarms.
      *)
      if M.kernel_active ()
      && Options.use_eva_results ()
      && Eva_analysis.is_computed kf
      then true
      else H.memo default kf

  let set = H.replace
  let self = H.self
  let accessor = M.name, set, is_computed

  let () =
    states := self :: !states;
    accessors := accessor :: !accessors;

end

module Initialized =
  Make
    (struct
      let name = "initialized"
      let parameter = Options.DoInitialized.parameter
      let additional_parameters = [ ]
      let kernel_active () = true
    end)

module Mem_access =
  Make
    (struct
      let name = "mem_access"
      let parameter = Options.DoMemAccess.parameter
      let additional_parameters = [ Kernel.SafeArrays.parameter ]
      let kernel_active () = true
    end)

module Pointer_alignment =
  Make
    (struct
      let name = "pointer_alignment"
      let parameter = Kernel.UnalignedPointer.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.UnalignedPointer.get ()
    end)

module Pointer_value =
  Make
    (struct
      let name = "pointer_value"
      let parameter = Kernel.InvalidPointer.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.InvalidPointer.get ()
    end)

module Pointer_call =
  Make
    (struct
      let name = "pointer_call"
      let parameter = Options.DoPointerCall.parameter
      let additional_parameters = []
      let kernel_active () = true
    end)

module Div_mod =
  Make
    (struct
      let name = "division_by_zero"
      let parameter = Options.DoDivMod.parameter
      let additional_parameters = []
      let kernel_active () = true
    end)

module Shift =
  Make
    (struct
      let name = "shift_value_out_of_bounds"
      let parameter = Options.DoShift.parameter
      let additional_parameters = []
      let kernel_active () = true
    end)

module Left_shift_negative =
  Make
    (struct
      let name = "left_shift_negative"
      let parameter = Kernel.LeftShiftNegative.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.LeftShiftNegative.get()
    end)

module Right_shift_negative =
  Make
    (struct
      let name = "right_shift_negative"
      let parameter = Kernel.RightShiftNegative.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.RightShiftNegative.get()
    end)

module Signed_overflow =
  Make
    (struct
      let name = "signed_overflow"
      let parameter = Kernel.SignedOverflow.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.SignedOverflow.get()
    end)

module Signed_downcast =
  Make
    (struct
      let name = "downcast"
      let parameter = Kernel.SignedDowncast.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.SignedDowncast.get()
    end)

module Unsigned_overflow =
  Make
    (struct
      let name = "unsigned_overflow"
      let parameter = Kernel.UnsignedOverflow.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.UnsignedOverflow.get()
    end)

module Unsigned_downcast =
  Make
    (struct
      let name = "unsigned_downcast"
      let parameter = Kernel.UnsignedDowncast.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.UnsignedDowncast.get()
    end)

module Pointer_downcast =
  Make
    (struct
      let name = "pointer_downcast"
      let parameter = Kernel.PointerDowncast.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.PointerDowncast.get()
    end)

module Float_to_int =
  Make
    (struct
      let name = "float_to_int"
      let parameter = Options.DoFloatToInt.parameter
      let additional_parameters = []
      let kernel_active () = true
    end)


module Finite_float =
  Make
    (struct
      let name = "finite_float"
      let parameter = Kernel.SpecialFloat.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.SpecialFloat.get() <> "none"
    end)

module Bool_value =
  Make
    (struct
      let name = "bool_value"
      let parameter = Kernel.InvalidBool.parameter
      let additional_parameters = []
      let kernel_active () = Kernel.InvalidBool.get()
    end)

(** DO NOT CALL Make AFTER THIS POINT *)

let proxy =
  State_builder.Proxy.create "RTE" State_builder.Proxy.Backward !states

let self = State_builder.Proxy.get proxy

let all_statuses = !accessors

let emitter =
  Emitter.create
    "rte"
    [ Emitter.Property_status; Emitter.Alarm ]
    ~correctness:[ Kernel.SafeArrays.parameter ]
    ~tuning:[]

let get_registered_annotations stmt =
  Annotations.fold_code_annot
    (fun e a acc -> if Emitter.equal e emitter then a ::acc else acc)
    stmt
    []
