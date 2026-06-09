(* Invariant computation for the system:
     X = 0.68 * X - 0.68 * Y + E1;
     Y = 0.68 * X + 0.68 * Y + E2;
   with E1 ∈ [-1 .. 1] and E2 ∈ [-1 .. 1]
    and [ X0 ; Y0 ] = [ 1000 ; 200 ] *)

module System = Lti_system.Make (Rational)
open System.Linear
open System

let n = Nat.(succ one)

let state_matrix =
  Matrix.of_array n n
    [| [| "0.68" ; "-0.68" |]
     ; [| "0.68" ;  "0.68" |] |]

let input_matrix =
  Matrix.of_array n n
    [| [| "1" ; "0" |]
     ; [| "0" ; "1" |] |]

let input_space =
  let center = Vector.zero n in
  let radius = Vector.repeat Rational.one n in
  Box.make center radius

let initial_state =
  Vector.of_array n [| "1000" ; "200" |]

let shift =
  Vector.zero n

let () =
  let s = { state_matrix ; input_matrix ; input_space ; initial_state ; shift } in
  let behavior = behavior ~completion_target:0.99 s in
  Format.printf "@[<v>Circle :@,%a@,@]" pretty_behavior behavior

