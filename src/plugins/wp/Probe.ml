(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* -------------------------------------------------------------------------- *)
(* --- User-Defined Probes                                                --- *)
(* -------------------------------------------------------------------------- *)

type probe = {
  id : int;
  name : string ;
  stmt : stmt option ;
  loc : location ;
}

let create =
  let id = ref (-1) in
  fun ~loc ?stmt ~name () ->
    incr id; { id = !id ; loc ; stmt ; name }

module S =
struct
  include Datatype.Undefined
  let name = "WP.Conditions.Probe.t"
  let reprs = [{
      loc = Fileloc.unknown; stmt = None; name = ""; id=1
    }]
  type t = probe
  let hash x = x.id
  let equal x y = Int.equal x.id y.id
  let compare x y = Int.compare y.id x.id (* lastly created first (wp) *)
  let pretty fmt p =
    if Wp_parameters.debug_atleast 1 then
      Format.fprintf fmt "%s#%d" p.name p.id
    else
      Format.fprintf fmt "%s" p.name
end

include Datatype.Make_with_collections(S)

(* -------------------------------------------------------------------------- *)
(* --- ACSL Extension                                                     --- *)
(* -------------------------------------------------------------------------- *)

let rec name_of_host = function
  | TVar { lv_name = x } -> x
  | TResult _ -> "result" (* currently not used, but could be one day *)
  | TMem t -> name_of_term t

and name_of_term (t : term) =
  match t.term_name with a::_ -> a | _ ->
  match t.term_node with
  | TLval (lh,_) | TAddrOf (lh,_) | TStartOf (lh,_) -> name_of_host lh
  | _ -> raise Not_found

let annotations stmt =
  let collect _emitter annot acc =
    match annot.annot_content with
    | Cil_types.AExtended (_, _, {
        ext_name="probe" ;
        ext_kind=Ext_terms ps ;
      }) -> List.rev_append ps acc
    | _ -> acc in
  let mk_probe t =
    try name_of_term t, t
    with Not_found ->
      Wp_parameters.abort ~source:(fst t.term_loc)
        "Missing name for probe, use @probe A: term;"
  in List.rev_map mk_probe (Annotations.fold_code_annot collect stmt [])

let parse_probe : Acsl_extension.extension_typer =
  fun context _loc terms ->
  (* use default context of the code-annotation (like an assert clause) *)
  let parse_term = context.type_term context context.pre_state in
  Ext_terms (List.map parse_term terms)

let registered = ref false
let register () =
  if not !registered && Wp_parameters.Probes.get () then
    begin
      registered := true ;
      Acsl_extension.register_code_annot ~plugin:"wp" "probe" parse_probe false ;
    end

let () = Cmdline.run_after_configuring_stage register

(* -------------------------------------------------------------------------- *)
