(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval



module Make
    (Value: Abstract_value.S)
    (Left: Abstract.Location.Internal with type value = Value.t)
    (Right: Abstract.Location.Internal with type value = Value.t)
= struct
  type value = Value.t
  let structure = Abstract.Location.Node (Left.structure, Right.structure)

  type offset = Left.offset * Right.offset
  type location = Left.location * Right.location

  let top = Left.top, Right.top

  let equal_loc (l, r) (l', r') =
    Left.equal_loc l l' && Right.equal_loc r r'
  let pretty_loc =
    Pretty_utils.pp_pair Left.pretty_loc Right.pretty_loc

  let equal_offset (l, r) (l', r') =
    Left.equal_offset l l' && Right.equal_offset r r'
  let pretty_offset =
    Pretty_utils.pp_pair Left.pretty_offset Right.pretty_offset

  (* TODO: don't know what to do, used max by default *)
  let size (l, r) =
    match Left.size l, Right.size r with
    | `Top, size
    | size, `Top -> size
    | `Value lsize as size, `Value rsize ->
      if Z.equal lsize rsize then size else
        Self.fatal
          "Location product: inconsistent size of the same location \
           represented by %a (size %a) and %a (size %a)."
          Left.pretty_loc l Z.pretty lsize
          Right.pretty_loc r Z.pretty rsize

  let enumerate_valid_bits access (l, r) =
    Memory_zone.narrow
      (Left.enumerate_valid_bits access l)
      (Right.enumerate_valid_bits access r)

  let replace_base subst (l, r) =
    Left.replace_base subst l, Right.replace_base subst r

  let assume_no_overlap ~partial (l1, r1) (l2, r2) =
    let l_truth = Left.assume_no_overlap  ~partial l1 l2 in
    let r_truth = Right.assume_no_overlap ~partial r1 r2 in
    Value_product.narrow_truth_pair ((l1, l2), l_truth) ((r1, r2), r_truth)

  let assume_valid_location ~for_writing ~bitfield (l, r) =
    let l_truth = Left.assume_valid_location  ~for_writing ~bitfield l in
    let r_truth = Right.assume_valid_location ~for_writing ~bitfield r in
    Value_product.narrow_truth (l, l_truth) (r, r_truth)

  let no_offset = Left.no_offset, Right.no_offset

  let forward_field typ varinfo (l, r) =
    Left.forward_field typ varinfo l, Right.forward_field typ varinfo r

  let forward_variable typ varinfo (l, r) =
    let* l = Left.forward_variable  typ varinfo l in
    let* r = Right.forward_variable typ varinfo r in
    `Value (l, r)

  let eval_varinfo varinfo =
    Left.eval_varinfo varinfo, Right.eval_varinfo varinfo

  let backward_variable varinfo (l, r) =
    let* l = Left.backward_variable  varinfo l in
    let* r = Right.backward_variable varinfo r in
    `Value (l, r)

  let backward_field typ varinfo (lo, ro) =
    let* lo = Left.backward_field  typ varinfo lo in
    let* ro = Right.backward_field typ varinfo ro in
    `Value (lo, ro)

  (** Both value abstractions produce a sound value abstraction for the same
      location, so we can narrow their results. *)
  let to_value (l, r) =
    let* vleft = Left.to_value l
    and* vright = Right.to_value r in
    Value.narrow vleft vright

  let forward_index typ v (l, r) =
    Left.forward_index typ v l, Right.forward_index typ v r

  let forward_pointer typ v (lo, ro) =
    let* l = Left.forward_pointer  typ v lo in
    let* r = Right.forward_pointer typ v ro in
    `Value (l, r)

  let backward_pointer v (lo, ro) (l, r) =
    let* (lv, lo) = Left.backward_pointer  v lo l in
    let* (rv, ro) = Right.backward_pointer v ro r in
    let* v = Value.narrow lv rv in
    `Value (v, (lo, ro))

  let backward_index typ ~index ~remaining:(lrem, rrem) (lo, ro) =
    let* (lv, lo) = Left.backward_index  typ ~index ~remaining:lrem lo in
    let* (rv, ro) = Right.backward_index typ ~index ~remaining:rrem ro in
    let* v = Value.narrow lv rv in
    `Value (v, (lo, ro))
end
