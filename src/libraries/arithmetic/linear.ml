(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Nat
open Finite



module Space (Field : Field.S) = struct

  type scalar = Field.scalar

  type ('n, 'm) matrix = { data : scalar Parray.t ; rows : 'n nat ; cols : 'm nat }
  type 'n vector = ('n, zero succ) matrix



  module Matrix = struct

    let index cols i j = i * Nat.to_int cols + j

    let get (type n m) (i : n finite) (j : m finite) (m : (n, m) matrix) =
      let i = Finite.to_int i and j = Finite.to_int j in
      Parray.get m.data (index m.cols i j)

    let set (type n m) i j num (m : (n, m) matrix) : (n, m) matrix =
      let i = Finite.to_int i and j = Finite.to_int j in
      { m with data = Parray.set m.data (index m.cols i j) num }

    let dimensions m = m.rows, m.cols


    type ('n, 'm) formatter = ('n, 'm) matrix Pretty_utils.formatter
    type ('n, 'm) boxing = ('n, 'm) formatter -> ('n, 'm) formatter

    let boxing : type n m. n finite -> n nat -> (n, m) boxing = fun i rows ->
      let i = Finite.to_int i and rows = Nat.to_int rows in
      let pp_vec pp fmt v = Format.fprintf fmt "[%a]" pp v in
      if Stdlib.(i == 0 && rows == 1) then pp_vec
      else if Stdlib.(i == 0) then Unicode.pp_ceil
      else if Stdlib.(i == rows - 1) then Unicode.pp_floor
      else (fun pp fmt v -> Format.fprintf fmt "|%a|" pp v)

    let pp_row_unboxed i fmt m =
      let scalar fmt j = Field.pretty fmt (get i j m) in
      let spacer fmt j = if j != last m.cols then Format.fprintf fmt " ; " in
      let pp_elt j = Format.fprintf fmt "%a%a" scalar j spacer j in
      Finite.iter pp_elt m.cols

    let pretty fmt m =
      let cut fmt i = if i != last m.rows then Format.pp_print_cut fmt () in
      let pp_row i fmt m = boxing i m.rows (pp_row_unboxed i) fmt m in
      let row i = Format.fprintf fmt "@[<h>%a@]%a" (pp_row i) m cut i in
      Finite.iter row m.rows


    let init n m init =
      let rows = Nat.to_int n and cols = Nat.to_int m in
      let t = Parray.init (rows * cols) (fun _ -> Field.zero) in
      let index i j = index m (Finite.to_int i) (Finite.to_int j) in
      let set i j data = Parray.set data (index i j) (init i j) in
      let data = Finite.(fold (fun i t -> fold (set i) m t) n t) in
      { data ; rows = n ; cols = m }

    let zero n m = init n m (fun _ _ -> Field.zero)
    let id n = Finite.fold (fun i m -> set i i Field.one m) n (zero n n)

    let of_array n m rows = init n m @@ fun i j ->
      Field.of_string rows.(Finite.to_int i).(Finite.to_int j)

    let transpose m = init m.cols m.rows (fun j i -> get i j m)

    let equal l r =
      let equal_elt row col = Field.(get row col l = get row col r) in
      let equal_row row = Finite.for_all (equal_elt row) l.cols in
      Finite.for_all equal_row l.rows


    let abs m = { m with data = Parray.map Field.abs m.data }
    let scale k m = { m with data = Parray.map (Field.( * ) k) m.data }
    let ( + ) l r = init l.rows l.cols Field.(fun i j -> get i j l + get i j r)
    let ( - ) l r = init l.rows l.cols Field.(fun i j -> get i j l - get i j r)
    let ( / ) l r = init l.rows l.cols Field.(fun i j -> get i j l / get i j r)

    let ( * ) l r =
      let n = l.rows and m = l.cols and p = r.cols in
      let folder i k j acc = Field.(get i j l * get j k r + acc) in
      let elt i k = Finite.fold (folder i k) m Field.zero in
      init n p elt


    let all_components_lower_than l r =
      let lower i j acc = acc && Field.(get i j l < get i j r) in
      let do_row i = Finite.fold (lower i) l.cols in
      Finite.fold do_row l.rows true

    let norm_inf m =
      let sum j i acc = Field.(abs (get i j m) + acc) in
      let col j = Finite.fold (sum j) m.rows Field.zero in
      let max j res = Field.max res (col j) in
      Finite.fold max m.cols Field.zero

    let norm_one m =
      let sum i j acc = Field.(abs (get i j m) + acc) in
      let row i = Finite.fold (sum i) m.cols Field.zero in
      let max i res = Field.max res (row i) in
      Finite.fold max m.rows Field.zero


    (* Compute inverse matrix using Gaussian elimination. *)
    let rec inverse (matrix : ('n, 'n) matrix) =
      let inverse = id matrix.rows in
      let pivot_row = Finite.first in
      let pivot_col = Finite.first in
      let matrix, inverse = row_echelon matrix inverse pivot_row pivot_col in
      back_propagation matrix inverse

    and row_echelon matrix inverse pivot_row pivot_col =
      let n = matrix.rows in
      let incr k = Finite.(next k |> strengthen n) in
      (* Find the k-th pivot. *)
      let i_max = argmax_col pivot_col pivot_row (Finite.last n) matrix in
      (* If the pivot is at zero in this column, try the next one. *)
      if Field.(get i_max pivot_col matrix = zero) then
        match incr pivot_col with
        | Some pivot_col -> row_echelon matrix inverse pivot_row pivot_col
        | None -> matrix, inverse
      else
        (* Normalize all values in pivot row with the argmax value to compute
           the reduced row echelon form. *)
        let value = get i_max pivot_col matrix in
        let divide col m = Field.(get i_max col m / value) in
        let normalize col m = set i_max col (divide col m) m in
        let matrix  = Finite.fold normalize n matrix  in
        let inverse = Finite.fold normalize n inverse in
        (* Swap the pivot row with the argmax row. *)
        let matrix  = swap_rows pivot_row i_max matrix  in
        let inverse = swap_rows pivot_row i_max inverse in
        (* Stop there if we are at the end. *)
        match incr pivot_row, incr pivot_col with
        | None, _ | _, None -> matrix, inverse
        | Some next_pivot_row, Some next_pivot_col ->
          (* Tools used to update the matrices. *)
          let pivot_value = get pivot_row pivot_col matrix in
          let factor i = Field.(get i pivot_col matrix / pivot_value) in
          let value i j m = Field.(get i j m - factor i * get pivot_row j m) in
          let update_elt i j m = set i j (value i j m) m in
          let update_row start i = Finite.fold (update_elt i) ~start n in
          let for_all_rows_after f = Finite.fold f ~start:next_pivot_row n in
          (* Fill with zeros the lower part of the pivot column. *)
          let to_zero i = set i pivot_col Field.zero in
          let matrix = for_all_rows_after to_zero matrix in
          (* Update all remaining elements on each row below and for each column
           * after the pivot column. Thus all columns in the inverse matrix. *)
          let matrix = for_all_rows_after (update_row next_pivot_col) matrix in
          let inverse = for_all_rows_after (update_row Finite.first) inverse in
          row_echelon matrix inverse next_pivot_row next_pivot_col

    and swap_rows row row' matrix =
      let swap_elt col matrix =
        let elt  = get row  col matrix in
        let elt' = get row' col matrix in
        matrix |> set row col elt' |> set row' col elt
      in Finite.fold swap_elt matrix.cols matrix

    and argmax_col col start stop matrix =
      let max row argmax =
        let argmax_value = Field.abs (get argmax col matrix) in
        let row_value = Field.abs (get row col matrix) in
        if Field.(argmax_value < row_value) then row else argmax
      in Finite.fold max ~start ~stop matrix.rows stop

    and back_propagation matrix inverse =
      let starts = Finite.fold List.cons matrix.rows [] in
      let do_all_steps = List.fold_left back_propagation_step in
      let matrix, inverse = do_all_steps (matrix, inverse) starts in
      if equal matrix (id matrix.rows) then Some inverse else None

    and back_propagation_step (matrix, inverse) pivot =
      let n = matrix.rows in
      let row_substitution row (matrix, inverse) =
        if Finite.(row < pivot) then
          let f = get row pivot matrix in
          let value col m = Field.(get row col m - f * get pivot col m) in
          let update col m = set row col (value col m) m in
          let inverse = Finite.fold update n inverse in
          let matrix = Finite.fold update n matrix in
          (matrix, inverse)
        else (matrix, inverse)
      in Finite.fold row_substitution n (matrix, inverse)

  end



  module Vector = struct

    let init size f =
      let data = Parray.init (Nat.to_int size) (fun _ -> Field.zero) in
      let set i data = Parray.set data (Finite.to_int i) (f i) in
      let data = Finite.fold set size data in
      { data ; rows = size ; cols = Nat.one }

    let of_array size t =
      init size (fun i -> Field.of_string t.(Finite.to_int i))

    let size (type n) (vector : n vector) : n nat = vector.rows
    let repeat n size = init size (fun _ -> n)
    let zero size = repeat Field.zero size

    let get (type n) (i : n finite) (vec : n vector) : scalar =
      Parray.get vec.data (Finite.to_int i)

    let pretty (type n) fmt (vector : n succ vector) =
      Matrix.(pretty fmt (transpose vector))

    let set (type n) (i : n finite) scalar (vec : n vector) : n vector =
      { vec with data = Parray.set vec.data (Finite.to_int i) scalar }

    let norm (type n) (v : n vector) : scalar =
      let max i r = Field.(max (abs (get i v)) r) in
      Finite.fold max (size v) Field.zero

    let max (type n) (l : n vector) (r : n vector) : n vector =
      init l.rows @@ fun i -> Field.max (get i l) (get i r)

    let base (type n) (i : n succ finite) (dimension : n succ nat) =
      zero dimension |> set i Field.one

  end

end
