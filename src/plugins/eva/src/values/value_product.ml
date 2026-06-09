(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

(* Intersects the truth values [t1] and [t2] coming from [assume_] functions
   from both abstract values. [v1] and [v2] are the initial values leading to
   these truth values, that may be reduced by the assumption. [combine]
   combines values from both abstract values into values of the product. *)
let narrow_any_truth combine (v1, t1) (v2, t2) = match t1, t2 with
  | `Unreachable, _ | _, `Unreachable
  | (`True | `TrueReduced _), `False
  | `False, (`True | `TrueReduced _) -> `Unreachable
  | `False, _ | _, `False -> `False
  | `Unknown v1, `Unknown v2 -> `Unknown (combine v1 v2)
  | (`Unknown v1 | `TrueReduced v1), `True -> `TrueReduced (combine v1 v2)
  | `True, (`Unknown v2 | `TrueReduced v2) -> `TrueReduced (combine v1 v2)
  | (`Unknown v1 | `TrueReduced v1),
    (`Unknown v2 | `TrueReduced v2) -> `TrueReduced (combine v1 v2)
  | `True, `True -> `True

let narrow_truth x y = narrow_any_truth (fun left right -> left, right) x y

let narrow_truth_pair x y =
  let combine (l1, l2) (r1, r2) = (l1, r1), (l2, r2) in
  narrow_any_truth combine x y

module Make
    (Context : Abstract_context.S)
    (Left  : Abstract.Value.Internal with type context = Context.t)
    (Right : Abstract.Value.Internal with type context = Context.t)
= struct

  include Datatype.Pair (Left) (Right)
  type context = Context.t
  let structure = Abstract.Value.Node (Left.structure, Right.structure)

  let pretty_typ typ =
    Pretty_utils.pp_pair ~pre:"@[" ~sep:",@ " ~suf:"@]"
      (Left.pretty_typ typ) (Right.pretty_typ typ)

  let top = Left.top, Right.top

  let is_included (l1, r1) (l2, r2) =
    Left.is_included l1 l2 && Right.is_included r1 r2

  let join (l1, r1) (l2, r2) =
    Left.join l1 l2, Right.join r1 r2

  let narrow (l1, r1) (l2, r2) =
    let+ left  = Left.narrow  l1 l2
    and+ right = Right.narrow r1 r2 in
    left, right

  let zero = Left.zero, Right.zero
  let top_int = Left.top_int, Right.top_int
  let inject_int typ i = Left.inject_int typ i, Right.inject_int typ i

  let assume_non_zero (left, right) =
    let left_truth  = Left.assume_non_zero  left
    and right_truth = Right.assume_non_zero right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_bounded kind bound (left, right) =
    let left_truth  = Left.assume_bounded  kind bound left
    and right_truth = Right.assume_bounded kind bound right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_not_nan ~assume_finite fkind (left, right) =
    let left_truth  = Left.assume_not_nan  ~assume_finite fkind left
    and right_truth = Right.assume_not_nan ~assume_finite fkind right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_pointer (left, right) =
    let left_truth  = Left.assume_pointer  left
    and right_truth = Right.assume_pointer right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_aligned modulo (left, right) =
    let left_truth  = Left.assume_aligned modulo  left
    and right_truth = Right.assume_aligned modulo right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_comparable op (l1, r1) (l2, r2) =
    let left_truth  = Left.assume_comparable  op l1 l2
    and right_truth = Right.assume_comparable op r1 r2 in
    narrow_truth_pair ((l1, l2), left_truth) ((r1, r2), right_truth)

  let constant context expr constant =
    let left  = Left.constant  context expr constant
    and right = Right.constant context expr constant in
    left, right

  let forward_unop context typ unop (left, right) =
    let+ left  = Left.forward_unop  context typ unop left
    and+ right = Right.forward_unop context typ unop right in
    left, right

  let forward_binop context typ binop (l1, r1) (l2, r2) =
    let+ left  = Left.forward_binop  context typ binop l1 l2
    and+ right = Right.forward_binop context typ binop r1 r2 in
    left, right

  let rewrap_integer context range (left, right) =
    let left  = Left.rewrap_integer  context range left
    and right = Right.rewrap_integer context range right in
    left, right

  let forward_cast context ~src_type ~dst_type (left, right) =
    let+ left  = Left.forward_cast context  ~src_type ~dst_type left
    and+ right = Right.forward_cast context ~src_type ~dst_type right in
    left, right

  let resolve_functions (left, right) =
    let list1, b1 = Left.resolve_functions  left  in
    let list2, b2 = Right.resolve_functions right in
    let list = match list1, list2 with
      | `Top, _ -> list2
      | _, `Top -> list1
      | `Value s1, `Value s2 -> `Value (List.filter (fun f -> List.mem f s1) s2)
    in
    list, b1 && b2

  let replace_base substitution (left, right) =
    Left.replace_base substitution left, Right.replace_base substitution right

  let reduce (orig_left, orig_right) left right = match left, right with
    | None, None            -> None
    | Some left, None       -> Some (left, orig_right)
    | None, Some right      -> Some (orig_left, right)
    | Some left, Some right -> Some (left, right)

  let backward_unop context ~typ_arg unop ~arg ~res =
    let on_left  = Left.backward_unop  context ~typ_arg unop in
    let on_right = Right.backward_unop context ~typ_arg unop in
    let+ left  = on_left  ~arg:(fst arg) ~res:(fst res)
    and+ right = on_right ~arg:(snd arg) ~res:(snd res) in
    reduce arg left right

  let backward_binop ctx ~input_type ~resulting_type binop ~left ~right ~result:res =
    let on_left  = Left.backward_binop  ctx ~input_type ~resulting_type binop in
    let on_right = Right.backward_binop ctx ~input_type ~resulting_type binop in
    let+ l1, l2 = on_left  ~left:(fst left) ~right:(fst right) ~result:(fst res)
    and+ r1, r2 = on_right ~left:(snd left) ~right:(snd right) ~result:(snd res) in
    reduce left l1 r1, reduce right l2 r2

  let backward_cast context ~src_typ ~dst_typ ~src_val ~dst_val =
    let on_left  = Left.backward_cast  context ~src_typ ~dst_typ in
    let on_right = Right.backward_cast context ~src_typ ~dst_typ in
    let+ left  = on_left  ~src_val:(fst src_val) ~dst_val:(fst dst_val)
    and+ right = on_right ~src_val:(snd src_val) ~dst_val:(snd dst_val) in
    reduce src_val left right

end
