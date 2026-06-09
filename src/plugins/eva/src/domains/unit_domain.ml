(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Static = struct
  module D = struct
    include Datatype.Unit
    type state = t

    let name = "unit"
    let structure = Abstract.Domain.Unit

    let top = ()
    let is_included _ _ = true
    let join _ _ = ()
    let widen _ _ _ _ = ()
    let narrow _ _ = `Value ()
  end

  include D
  include Domain_builder.Complete (D)
end

module Make
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S)
= struct

  include Static
  type value = Value.t
  type location = Loc.location
  type origin

  let eval_top = `Value (Value.top, None), Alarmset.all
  let extract_expr ~oracle:_ _ _ _ = eval_top
  let extract_lval ~oracle:_ _ _ _ _ = eval_top

  let update _ _ = `Value ()
  let assign ~pos:_ _ _ _ _ _ = `Value ()
  let assume ~pos:_ _ _ _ _ = `Value ()
  let start_call ~pos:_ _ _ _ _ = `Value ()
  let finalize_call ~pos:_ _ _ ~pre:_ ~post:_ = `Value ()
  let show_expr _ _ _ _ = ()

  let logic_assign _ _ _ = ()

  let enter_scope _ _ _ = ()
  let leave_scope _ _ _ = ()

  let empty () = ()
  let initialize_variable _ _ ~initialized:_ _ _ = ()
  let initialize_variable_using_type _ _ _  = ()

  let relate _ () = Base.SetLattice.empty
  let overwrite _ ~on:_ ~by:_ = ()
end
