(* Invariant computation for the system:
     X =  1.20 * X - 0.20 * Y - 0.30 * Z + 1.0 * E1 + 0.5 * E3;
     Y =  0.70 * X - 0.30 * Y + 0.60 * Z + 1.0 * E2 - 0.5 * E3;
     Z = -0.07 * X + 0.91 * Y - 0.12 * Z + 0.3 * E1 + 0.2 * E2;
   with E1 ∈ [-99 .. 101], E2 ∈ [-101 .. -99] and E3 ∈ [199 .. 201]
    and [ X0 ; Y0 ; Z0 ] = [ 1000 ; 1000 ; 2000 ]. *)

module System = Lti_system.Make (Rational)
open System.Linear
open System

let n = Nat.(succ (succ one))

let state_matrix =
  Matrix.of_array n n
    [| [|  "1.2"  ; "-0.2"  ; "-0.3"  |]
     ; [|  "0.7"  ; "-0.3"  ;  "0.6"  |]
     ; [| "-0.07" ;  "0.91" ; "-0.12" |] |]

let input_matrix =
  Matrix.of_array n n
    [| [| "1.0"  ; "0.0" ;  "0.5" |]
     ; [| "0.0"  ; "1.0" ; "-0.5" |]
     ; [| "0.3"  ; "0.2" ;  "0.0" |] |]

let input_space =
  let center = Vector.of_array n [| "100" ; "-100" ; "200" |] in
  let radius = Vector.repeat Rational.one n in
  Box.make center radius

let initial_state =
  Vector.of_array n [| "1000" ; "1000" ; "2000" |]

let shift =
  Vector.zero n

let () =
  let s = { state_matrix ; input_matrix ; input_space ; initial_state ; shift } in
  let behavior = behavior ~completion_target:0.80 s in
  Format.printf "@[<v>Random 3D :@,%a@,@]" pretty_behavior behavior

