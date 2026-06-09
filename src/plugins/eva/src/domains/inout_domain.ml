(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


type inout = {
  (* over-approximation of the memory locations written by the function *)
  over_outputs: Memory_zone.t;
  (* over-approximation of the memory locations read by the function *)
  over_inputs: Memory_zone.t;
  (* under-approximation of the memory locations written by the function *)
  under_outputs: Memory_zone.t;
  (* over-approximation of the memory locations parts read by the function
     that are parts of its inputs (i.e. that the function has not written
     previously) *)
  operational_inputs: Memory_zone.t;
}


(* Lattice structure for the abstract state above *)
module LatticeInout = struct

  (* Name of the domain *)
  let name = "inout"

  (* Frama-C "datatype" for type [inout] *)
  include Datatype.Make_with_collections(struct
      include Datatype.Serializable_undefined

      type t = inout
      let name = "Eva.Inout_domain.LatticeInout"

      let reprs = [ {
          over_outputs = List.hd Memory_zone.reprs;
          over_inputs = List.hd Memory_zone.reprs;
          under_outputs = List.hd Memory_zone.reprs;
          operational_inputs = List.hd Memory_zone.reprs;
        } ]

      let structural_descr =
        Structural_descr.t_record [|
          Memory_zone.packed_descr;
          Memory_zone.packed_descr;
          Memory_zone.packed_descr;
          Memory_zone.packed_descr;
        |]

      let compare m1 m2 =
        let c = Memory_zone.compare m1.over_outputs m2.over_outputs in
        if c <> 0 then c
        else
          let c = Memory_zone.compare m1.over_inputs m2.over_inputs in
          if c <> 0 then c
          else
            let c = Memory_zone.compare m1.under_outputs m2.under_outputs in
            if c <> 0 then c
            else Memory_zone.compare m1.operational_inputs m2.operational_inputs

      let equal = Datatype.from_compare

      let pretty fmt c =
        Format.fprintf fmt
          "@[<v 2>Over outputs:@ @[<hov>%a@]@]@.\
           @[<v 2>Over inputs:@ @[<hov>%a@]@]@.\
           @[<v 2>Sure outputs:@ @[<hov>%a@]@]@.\
           @[<v 2>Operational inputs:@ @[<hov>%a@]@]"
          Memory_zone.pretty c.over_outputs
          Memory_zone.pretty c.over_inputs
          Memory_zone.pretty c.under_outputs
          Memory_zone.pretty c.operational_inputs

      let hash m =
        Hashtbl.hash (Memory_zone.hash m.over_outputs,
                      Memory_zone.hash m.over_inputs,
                      Memory_zone.hash m.under_outputs,
                      Memory_zone.hash m.operational_inputs)

      let copy c = c

    end)

  (* Initial abstract at the beginning of the computation: nothing written
     or read so far. *)
  let empty = {
    over_outputs = Memory_zone.bottom;
    over_inputs = Memory_zone.bottom;
    under_outputs = Memory_zone.bottom;
    operational_inputs = Memory_zone.bottom;
  }

  (* Top state: everything read or written, nothing written in a sure way *)
  let top = {
    over_outputs = Memory_zone.top;
    over_inputs = Memory_zone.top;
    under_outputs = Memory_zone.bottom;
    operational_inputs = Memory_zone.top;
  }

  (* Join: over-approximation are joined, under-approximation are met. *)
  let join c1 c2 = {
    over_outputs = Memory_zone.join c1.over_outputs c2.over_outputs;
    over_inputs = Memory_zone.join c1.over_inputs c2.over_inputs;
    under_outputs = Memory_zone.meet c1.under_outputs c2.under_outputs;
    operational_inputs = Memory_zone.join c1.operational_inputs c2.operational_inputs;
  }

  (* The memory locations are finite, so the ascending chain property is
     already verified. We simply use a join. *)
  let widen _ _ c1 c2 = join c1 c2

  let narrow c1 c2 =
    `Value
      { over_outputs = Memory_zone.narrow c1.over_outputs c2.over_outputs;
        over_inputs = Memory_zone.narrow c1.over_inputs c2.over_inputs;
        under_outputs = Memory_zone.link c1.under_outputs c2.under_outputs;
        operational_inputs =
          Memory_zone.narrow c1.operational_inputs c2.operational_inputs; }

  (* Inclusion testing: pointwise for over-approximations, counter-pointwise
     for under-approximations *)
  let is_included c1 c2 =
    Memory_zone.is_included c1.over_outputs c2.over_outputs &&
    Memory_zone.is_included c1.over_inputs c2.over_inputs &&
    Memory_zone.is_included c2.under_outputs c1.under_outputs &&
    Memory_zone.is_included c1.operational_inputs c2.operational_inputs

end

module Transfer = struct

  (* Approximations of two consecutive statements [s1; s2], respectively
     abstracted as [c1] and [c2]. The result is immediate, except for
     operational inputs. For those, we subtract from the inputs of [c2]
     the memory locations that have been written in a sure way in [c1],
     then perform the join. *)
  let catenate c1 c2 =
    { over_outputs = Memory_zone.join c1.over_outputs c2.over_outputs;
      over_inputs = Memory_zone.join c1.over_inputs c2.over_inputs;
      under_outputs = Memory_zone.link c1.under_outputs c2.under_outputs;
      operational_inputs =
        Memory_zone.join c1.operational_inputs
          (Memory_zone.diff c2.operational_inputs c1.under_outputs);
    }

  (* Effects of a conditional [if (e)]. [to_z] converts the lvalues present
     in [e] into locations. Nothing is written, the memory locations
     present in [e] are read. *)
  let effects_assume to_z e =
    let inputs = Eva_ast.PreciseDepsOf.zone_of_exp to_z e in
    {
      over_outputs = Memory_zone.bottom;
      over_inputs = inputs;
      under_outputs = Memory_zone.bottom;
      operational_inputs = inputs;
    }

  (* Effects of an assignment [lv = e]. [to_z] converts the lvalues present
     in [lv] and [e] into locations. *)
  let effects_assign to_z lv e =
    let inputs_e = Eva_ast.PreciseDepsOf.zone_of_exp to_z e in
    let inputs_lv =
      Eva_ast.PreciseDepsOf.indirect_zone_of_lval to_z lv.Eval.lval
    in
    let inputs = Memory_zone.join inputs_e inputs_lv in
    let outputs =
      Precise_locs.enumerate_valid_bits Locations.Write lv.Eval.lloc
    in
    let exact_outputs = Precise_locs.cardinal_zero_or_one lv.Eval.lloc in
    {
      over_outputs = outputs;
      over_inputs = inputs;
      under_outputs = if exact_outputs then outputs else Memory_zone.bottom;
      operational_inputs = inputs;
    }

  (* Removes a list of variables from a state. Used to model exiting a
     scope. *)
  let remove_variables vars state =
    let bases =
      List.fold_left
        (fun acc v -> Base.Set.add (Base.of_varinfo v) acc)
        Base.Set.empty vars
    in
    let rm = Memory_zone.filter_base (fun b -> not (Base.Set.mem b bases)) in {
      over_outputs = rm state.over_outputs;
      over_inputs = rm state.over_inputs;
      under_outputs = rm state.under_outputs;
      operational_inputs = rm state.operational_inputs;
    }

end

module Domain = struct

  type state = inout
  type value = Cvalue.V.t
  type location = Precise_locs.precise_location
  type origin

  let value_dependencies = Main_values.cval
  let location_dependencies = Main_locations.ploc

  include (LatticeInout: sig
             include Datatype.S_with_collections with type t = state
             include Abstract_domain.Lattice with type state := state
           end)

  include Domain_builder.Complete (LatticeInout)

  let enter_scope _kind _vars state = state
  let leave_scope _kf vars state = Transfer.remove_variables vars state

  let to_z valuation lv =
    match valuation.Abstract_domain.find_loc lv with
    | `Value loc -> loc.Eval.loc
    | `Top -> Precise_locs.loc_top (* should not occur *)

  let assign ~pos:_ lv e _v valuation state =
    let to_z = to_z valuation in
    let effects = Transfer.effects_assign to_z lv e in
    `Value (Transfer.catenate state effects)

  let assume ~pos:_ e _pos valuation state =
    let to_z = to_z valuation in
    let effects = Transfer.effects_assume to_z e in
    `Value (Transfer.catenate state effects)

  let start_call ~pos:_ _call _recursion _valuation _state =
    `Value LatticeInout.empty

  let finalize_call ~pos:_ _call _recursion ~pre ~post =
    `Value (Transfer.catenate pre post)

  let update _valuation state = `Value state

  (* Memexec *)
  let relate _bases _state = Base.SetLattice.empty

  (* Initial state. Initializers are singletons, so we store nothing. *)
  let empty () = LatticeInout.empty
  let initialize_variable _ _ ~initialized:_ _ state = state
  let initialize_variable_using_type _ _ state  = state

  (* TODO *)
  let logic_assign _assign _location _state = top

  let top_query = `Value (Cvalue.V.top, None), Alarmset.all

  let extract_expr ~oracle:_ _context _state _expr = top_query
  let extract_lval ~oracle:_ _context _state _lv _locs = top_query

  let overwrite bases ~on:state ~by:_ =
    let zone = Memory_zone.of_bases bases in
    { state with over_outputs = Memory_zone.join state.over_outputs zone; }
end

include Domain

let registered =
  let name = "inout"
  and descr = "Infers the inputs and outputs of each function." in
  Abstractions.Domain.register ~name ~descr ~experimental:true (module Domain)
