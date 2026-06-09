open Cil_types
open Cil_const

module type S = Cil_datatype.S_with_collections_pretty with type t = typ

(* Convert a list of elements to a list of all pair combinations of these
   elements. *)
let to_pairs l =
  List.combinations 2 l |>
  List.map (function [ t1; t2 ] -> (t1, t2) | _ -> assert false)

(* If two types are considered equal, their hash should also be equal.
   If this function returns true, it means our compare/hash functions are wrong.
*)
let check (module Typ : S) (t1, t2) =
  let h1 = Typ.hash t1 in
  let h2 = Typ.hash t2 in
  Typ.equal t1 t2 && h1 <> h2

let check_all (module Typ : S) typ_pairs =
  Format.printf "@[<v2>Checking %s@;" Typ.datatype_name;
  match List.filter (check (module Typ)) typ_pairs with
  | [] -> Format.printf "All checks succeeded!@]@\n@."
  | l ->
    let max = 5 in
    Format.printf
      "@[<v2>The following types were considered equal but have \
       different hashes (only showing the %d first occurrences):" max;
    let pp_elem i (t1, t2) =
      if i >= max then raise Exit;
      Format.printf "@;%a, %a" Typ.pretty t1 Typ.pretty t2
    in
    try
      List.iteri pp_elem l;
      Format.printf "@]@]@\n@."
    with _ ->
      Format.printf "@]@]@\n@."

let check_all_datatypes typ_pairs =
  check_all (module Cil_datatype.Typ) typ_pairs;
  check_all (module Cil_datatype.TypByName) typ_pairs;
  check_all (module Cil_datatype.TypNoUnroll) typ_pairs;
  check_all (module Cil_datatype.TypNoAttrs) typ_pairs

(* -------------------------------------------------------------------------- *)
(* --- Tests for attributes                                               --- *)
(* -------------------------------------------------------------------------- *)

let logic_attr = ("const", [])
let type_attr  = ("visibility", [])
let internal_attr = ("fc_stdlib", [])
let ignored_attr = ("a_new_attr", [])

let attrs = [ logic_attr; type_attr; internal_attr; ignored_attr ]

let attrs_combinations =
  List.combinations 1 attrs
  @ List.combinations 2 attrs
  @ List.combinations 3 attrs
  @ [ attrs ]

let mk_types_attrs t =
  List.map (fun attrs -> Ast_types.add_attributes attrs t) attrs_combinations

let attrs_typ = mk_types_attrs intType

(* All types should hash and compare attributes the same way, no need to
   test all kind of nodes at the same time. *)
let pairs_attrs_typ = to_pairs attrs_typ

(* -------------------------------------------------------------------------- *)
(* --- Tests for types                                                    --- *)
(* -------------------------------------------------------------------------- *)

let comp1 = {
  cstruct = true;
  corig_name = "comp_type";
  cname = "comp_type";
  ckey = 42;
  cfields = None;
  cattr = [];
  creferenced = false;
}
let tc1 = mk_tcomp comp1

(* Same name different id *)
let comp2 = {
  cstruct = true;
  corig_name = "comp_type";
  cname = "comp_type";
  ckey = 24;
  cfields = None;
  cattr = [];
  creferenced = false;
}
let tc2 = mk_tcomp comp2

(* Same name and id, different cstruct. Normally only ckey and cname are used
   for compare/hash *)
let comp3 = {
  cstruct = false;
  corig_name = "comp_type";
  cname = "comp_type";
  ckey = 42;
  cfields = None;
  cattr = [];
  creferenced = false;
}
let tc3 = mk_tcomp comp3

let ti = {
  torig_name = "named_type";
  tname = "named_type";
  ttype = intType;
  treferenced = false
}
let tn = mk_tnamed ti

(* Regular nodes, should not pose any problem *)
let pairs_nodes_typ =
  voidType
  :: intType
  :: intPtrType
  :: doubleType
  :: mk_tarray intType None
  :: mk_tfun intType None false
  :: tc1
  :: [ tn ]
  |> to_pairs

(* Make sure we consider types and len correctly for arrays. *)
let pairs_array_typ =
  let loc = Fileloc.unknown in
  let len1 = Some (Cil.one ~loc) in
  let len2 = Some (Cil.integer ~loc 42) in
  mk_tarray intType None
  :: mk_tarray intType len1
  :: mk_tarray intType len2
  :: [ mk_tarray intPtrType len1 ]
  |> to_pairs

(* Simple tests for variadic functions, different parameter configuration
   and also different parameter attributes. Ret types / parameter type do not
   need to be tested here, we already tested regular node comparisons. *)
let pairs_fun_typ =
  mk_tfun intType None true
  :: mk_tfun intType None false
  :: mk_tfun doubleType None false
  :: mk_tfun intType (Some []) false
  :: List.map (fun attrs ->
      mk_tfun intType (Some [("x", intType, attrs)]) false
    ) attrs_combinations
  |> to_pairs

(* structs/unions are compared/hashed using their keys/names, it should never be
   a problem, so we need just a few tests to make sure this is the case.
*)
let pairs_comp_typ = [(tc1, tc2); (tc1, tc3); (tc2, tc3)]

let pairs =
  pairs_attrs_typ
  @ pairs_nodes_typ
  @ pairs_array_typ
  @ pairs_fun_typ
  @ pairs_comp_typ

let () =
  Kernel.(add_debug_keys dkey_print_attrs);
  check_all_datatypes pairs
