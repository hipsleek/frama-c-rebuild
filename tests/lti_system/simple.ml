(* Invariant computation for the system:
     X = 1.5 * X - 0.7 * Y + E + 1;
     Y = X + 1;
   with E ∈ [-0.1 .. 0.1]
    and [ X0 ; Y0 ] = [ 0 ; 0 ] *)

module System = Lti_system.Make (Rational)
open System.Linear
open System

let n = Nat.(succ one)
let m = Nat.one

let state_matrix =
  Matrix.of_array n n
    [| [| "1.5" ; "-0.7" |]
     ; [| "1.0" ;  "0.0" |] |]

let input_matrix =
  Vector.of_array n [| "1" ; "0" |]

let input_space =
  let center = Vector.zero m in
  let radius = Vector.repeat (Rational.of_string "0.1") m in
  Box.make center radius

let initial_state =
  Vector.zero n

let shift =
  Vector.repeat Rational.one n

let () =
  let s = { state_matrix ; input_matrix ; input_space ; initial_state ; shift } in
  let behavior = behavior ~completion_target:0.99 s in
  Format.printf "@[<v>Simple :@,%a@,@]" pretty_behavior behavior

