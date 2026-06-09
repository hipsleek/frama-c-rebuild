(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Flags for filtering Alarms *)
module Flags = Flags

(** RTE Generator Status & Emitters *)
module Generator = Generator

(** Visitors to iterate over Alarms and/or generate Code-Annotations *)
module Visit = Visit

let compute = Register.compute

(** Options  *)
module Options = struct

  module DoShift = Options.DoShift
  module DoDivMod = Options.DoDivMod
  module DoFloatToInt = Options.DoFloatToInt
  module DoInitialized = Options.DoInitialized
  module DoMemAccess = Options.DoMemAccess
  module DoPointerCall = Options.DoPointerCall
  module Trivial = Options.Trivial

end
