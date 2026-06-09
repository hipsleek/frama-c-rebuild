(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


module Make (K : Field.S) = struct

  open Linear.Space (K)
  type 'n t = { center : 'n vector ; radius : 'n vector }

  let make center radius = { center ; radius = Matrix.abs radius }
  let zero n = make (Vector.zero n) (Vector.zero n)
  let point v = make v Vector.(size v |> zero)
  let ( + ) l r = make Matrix.(l.center + r.center) Matrix.(l.radius + r.radius)

  let lower { center ; radius } = Matrix.(center - radius)
  let upper { center ; radius } = Matrix.(center + radius)
  let bounds box = lower box, upper box

  let is_included l r =
    let ( < ) = Matrix.all_components_lower_than in
    lower r < lower l && upper l < upper r

  let pretty fmt box =
    let n = Vector.size box.center in
    let pretty i =
      let c = Vector.get i box.center in
      let r = Vector.get i box.radius in
      if Finite.(i != first) then Format.fprintf fmt " ; " ;
      Format.fprintf fmt "%a ± %a" K.pretty c K.pretty r ;
    in
    Format.fprintf fmt "@[[" ;
    Finite.iter pretty n ;
    Format.fprintf fmt "]@]"

end

