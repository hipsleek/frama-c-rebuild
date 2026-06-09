(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t = {
  over_inputs: Memory_zone.t;
  over_inputs_if_termination: Memory_zone.t;
  over_logic_inputs: Memory_zone.t;
  under_outputs_if_termination: Memory_zone.t;
  over_outputs: Memory_zone.t;
  over_outputs_if_termination: Memory_zone.t;
}

let pretty_operational_inputs_aux fmt x =
  Format.fprintf fmt "@[<v 2>Operational inputs:@ @[<hov>%a@]@]@ "
    Memory_zone.pretty (x.over_inputs);
  Format.fprintf fmt "@[<v 2>Operational inputs on termination:@ @[<hov>%a@]@]@ "
    Memory_zone.pretty (x.over_inputs_if_termination);
  Format.fprintf fmt "@[<v 2>Sure outputs:@ @[<hov>%a@]@]@ "
    Memory_zone.pretty (x.under_outputs_if_termination);
;;

let pretty_outputs_aux fmt x =
  Format.fprintf fmt "@[<v 2>Over outputs:@ @[<hov>%a@]@]@ "
    Memory_zone.pretty (x.over_outputs);
  Format.fprintf fmt "@[<v 2>Over outputs on termination:@ @[<hov>%a@]@]@ "
    Memory_zone.pretty (x.over_outputs_if_termination);
;;

let wrap_vbox f fmt x =
  Format.fprintf fmt "@[<v>";
  f fmt x;
  Format.fprintf fmt "@]"

let pretty_operational_inputs = wrap_vbox pretty_operational_inputs_aux
let pretty_outputs = wrap_vbox pretty_outputs_aux


include
  (Datatype.Make
     (struct
       include Datatype.Serializable_undefined
       type inout_t = t
       type t = inout_t
       let pretty fmt x =
         Format.fprintf fmt "@[<v>";
         pretty_operational_inputs_aux fmt x;
         pretty_outputs_aux fmt x;
         Format.fprintf fmt "@]"

       let structural_descr =
         let z = Memory_zone.packed_descr in
         Structural_descr.t_record [| z; z; z; z; z; z |]
       let reprs =
         List.map
           (fun z ->
              { over_inputs_if_termination = z;
                under_outputs_if_termination = z;
                over_inputs = z;
                over_logic_inputs = z;
                over_outputs = z;
                over_outputs_if_termination = z;
              }) Memory_zone.reprs
       let name = "Full.tt"
       let hash
           { over_inputs_if_termination = a;
             under_outputs_if_termination = b;
             over_inputs = c;
             over_outputs = d;
             over_outputs_if_termination = e;
             over_logic_inputs = f;
           } =
         Memory_zone.hash a +
         17 * Memory_zone.hash b +
         587 * Memory_zone.hash c +
         1077 * Memory_zone.hash d +
         13119 * Memory_zone.hash e +
         15823 * Memory_zone.hash f
       let equal
           { over_inputs_if_termination = a;
             under_outputs_if_termination = b;
             over_inputs = c;
             over_outputs = d;
             over_outputs_if_termination = e;
             over_logic_inputs = f;
           }
           { over_inputs_if_termination = a';
             under_outputs_if_termination = b';
             over_inputs = c';
             over_outputs = d';
             over_outputs_if_termination = e';
             over_logic_inputs = f';
           } =
         Memory_zone.equal a a'
         && Memory_zone.equal b b'
         && Memory_zone.equal c c'
         && Memory_zone.equal d d'
         && Memory_zone.equal e e'
         && Memory_zone.equal f f'
       let mem_project = Datatype.never_any_project
     end)
   : Datatype.S with type t := t)

let map f v = {
  over_inputs_if_termination = f v.over_inputs_if_termination;
  under_outputs_if_termination = f v.under_outputs_if_termination;
  over_inputs = f v.over_inputs;
  over_logic_inputs = f v.over_logic_inputs;
  over_outputs = f v.over_outputs;
  over_outputs_if_termination = f v.over_outputs_if_termination;
}

let bottom = {
  over_inputs = Memory_zone.bottom;
  over_inputs_if_termination = Memory_zone.bottom;
  over_logic_inputs = Memory_zone.bottom;
  under_outputs_if_termination = Memory_zone.top;
  over_outputs = Memory_zone.bottom;
  over_outputs_if_termination = Memory_zone.bottom;
}

let join c1 c2 = {
  over_inputs = Memory_zone.join c1.over_inputs c2.over_inputs;
  over_inputs_if_termination =
    Memory_zone.join c1.over_inputs_if_termination c2.over_inputs_if_termination;
  over_logic_inputs = Memory_zone.join c1.over_logic_inputs c2.over_logic_inputs;
  over_outputs = Memory_zone.join c1.over_outputs c2.over_outputs;
  over_outputs_if_termination =
    Memory_zone.join c1.over_outputs_if_termination c2.over_outputs_if_termination;
  under_outputs_if_termination =
    Memory_zone.meet c1.under_outputs_if_termination c2.under_outputs_if_termination;
}
