(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Notations and conventions :
 * - I is the identity matrix ;
 * - A is the system's state matrix ;
 * - B is the system's input matrix ;
 * - S is the system's shift ;
 * - μ is an infinite sequence of inputs ;
 * - C is the center of the inputs box ;
 * - R is the radius of the inputs box ;
 * - Everytime a radius is mentioned, it is always supposed all positive ;
 * - |.| is the componentwise absolute value on matrices and vectors. *)

module Make (K : Field.S) = struct

  (** Preliminary declarations **)

  module Linear = Linear.Space (K)
  module Box = Box.Make (K)
  open Option.Operators
  open Linear

  type 'n box = 'n Box.t


  (** Types specifications **)

  (* A LTI system full specification. *)
  type ('n, 'm) system =
    { state_matrix  : ('n, 'n) matrix
    ; input_matrix  : ('n, 'm) matrix
    ; input_space   : 'm box
    ; shift         : 'n vector
    ; initial_state : 'n vector
    }

  (* Knowledge on LTI systems shared across the module's functions:
   * - [n] is the system's order ;
   * - [center] corresponds to the constant part added at each iteration
   *   and is computed as {m B C + S} ;
   * - [radius] is simply {m |R|}. *)
  type ('n, 'm) knowledge =
    { n : 'n Nat.nat ; center : 'n vector ; radius : 'm vector }

  (* Information on an iteration {m k} of the system:
   * - [state_power] corresponds to the computation {m A^k}.
   * - [perturbations] corresponds to the maximal cumulative contributions of
   *   all previous inputs, which is a box with center and radius computed
   *   respectively as {m ∑ A^t (B C + S)} and {m ∑ |A^t B| |R|},
   *   where {m t} is between {m 0} and {m k - 1}. *)
  type 'n iteration =
    { state_power : ('n, 'n) matrix ; perturbations : 'n box }

  (* Behavior of the system, described as a transition phase of unrolled
   * iterations, and a permanent phase described by an overapproximated box. *)
  type 'n behavior =
    { transition : 'n box list ; permanent : 'n box }


  (** Behavior computation **)

  (* Computes the limit center. The computation is lazy for two reasons:
   * - The result is valid iff {m ρ(A) < 1}, which will eventually be
   *   proven through the limit computation.
   * - Proving {m ρ(A) < 1} comes down to finding a {m q ∈ ℕ} such
   *   as {m ||A^q||₁ < 1}. The limit center can then be computed
   *   as {m (I - A^q)^(-1) (∑ A^t (B C + S))} for {m t} between {m 0}
   *   and {m q - 1}. But, this computation's result is the same for
   *   all {m q} once the necessary condition is proven, so we only
   *   need to compute it as {m (I - A)^(-1) (B C + S)}.
   * Relying on laziness is then a simple way to wait for a proof of
   * the necessary condition {m ρ(A) < 1} and then compute the
   * limit center only once. *)
  let compute_center_limit system knowledge = Lazy.from_fun @@ fun () ->
    let+ limit = Matrix.(inverse (id knowledge.n - system.state_matrix)) in
    Matrix.(limit * knowledge.center)

  (* Computes the systems iterations as a memoized infinite sequence
   * of [iteration] structures. *)
  let compute_iterations s { n ; center ; radius } =
    let zero = { state_power = Matrix.id n ; perturbations = Box.zero n } in
    let compute_next_iteration { state_power ; perturbations } =
      let center = Matrix.(state_power * center) in
      let radius = Matrix.(abs (state_power * s.input_matrix) * radius) in
      let perturbations = Box.(perturbations + make center radius) in
      (* Updating [state_power] at the end as [perturbations] is the sum
       * of all *previous* iterations contributions. *)
      let state_power = Matrix.(s.state_matrix * state_power) in
      { state_power ; perturbations }
    in Seq.(iterate compute_next_iteration zero |> memoize)

  (* Computes a box overapproximating the system's behavior as the iteration
   * goes to infinity, along with the spectral exponent {m q} with which the
   * overapproximation has been computed. The center of this box is computed
   * as described in the [compute_center_limit] function. Its radius is an
   * overapproximation of the supremum for all possible input sequence {m μ}
   * of the limit as {m k} tends toward infinity of {m ∑ A^t B μ_(k - 1 - t)},
   * with {m t} between {m 0} and {m k - 1}.
   * The computation is done as follows:
   * - To prove that {m ρ(A) < 1}, the function searches for a spectral
   *   exponent, i.e a {m q ∈ ℕ} such as {m ||A^q||₁ < 1}.
   * - The infinite sum is then divided in two: a finite sum of the {m q}
   *   first elements and the infinite remaining sum. Indeed, as {m q} grows,
   *   the finite sum becomes a better and better underapproximation of the
   *   limit radius, and the infinite remainder tends toward zero (but in a
   *   potentially non monotonous way).
   * - The infinite remainder is approximated by the computation
   *   {m (I - |A^q|)^(-1) |A^q| (∑ |A^t B| |R|)}.
   * - The radius of the returned overapproximated box is computed as the
   *   finite sum inflated by a factor of {m 1 / completion_target} and is
   *   considered a valid overapproximation if and only if using the remainder
   *   overapproximation would actually be better. It is done that way to
   *   avoid local minimums coming from the non-monotony and that are to
   *   precise to actually find an unrolling stop point later on. *)
  let limit_behavior s ({ n ; _ } as knowledge) completion_target iterations =
    let inflation = K.(one / of_float completion_target) in
    let center_limit = compute_center_limit s knowledge in
    let head seq = Seq.uncons seq |> Option.map fst in
    let limit q { state_power ; perturbations } =
      let () = Async.yield () in
      if K.(Matrix.norm_one state_power < one) then
        let underapprox = perturbations.radius in
        let inflated = Matrix.scale inflation underapprox in
        let abs_power = Matrix.abs state_power in
        let* center_limit = Lazy.force center_limit in
        let* limit_scale = Matrix.(inverse (id n - abs_power)) in
        let remainder = Matrix.(limit_scale * abs_power * underapprox) in
        let overapprox = Matrix.(underapprox + remainder) in
        if Matrix.all_components_lower_than overapprox inflated
        then Some (q, Box.make center_limit inflated)
        else None
      else None
    in Seq.(mapi limit iterations |> filter_map Fun.id |> head)

  (* Searches for the first valid unrolling stop point for a given [limit]
   * found at the exponent [spectral]. A stop point [k] is valid if the
   * system behavior is included in [limit] for iteration [k] and for
   * the [spectral - 1] following iterations. *)
  let search_unrolling_stop spectral limit iterations =
    let exception Found of int in
    let in_limit abst = Box.is_included abst limit in
    let search window n abst =
      let () = Async.yield () in
      match window with
      | None -> if in_limit abst then Some (n, 1) else None
      | Some (start, l) when l = spectral -> raise (Found start)
      | Some (start, l) -> if in_limit abst then Some (start, l + 1) else None
    in
    try ignore (Seq.fold_lefti search None iterations) ; None
    with Found stop -> Some (Seq.take stop iterations |> List.of_seq)

  (* Computation of the system's behavior. No termination guarantee. *)
  let behavior_unbounded completion (s : ('n, 'm) system) =
    let n = Vector.size s.initial_state in
    let radius = Matrix.(abs s.input_space.radius) in
    let center = Matrix.(s.input_matrix * s.input_space.center + s.shift) in
    let knowledge = { n ; radius ; center } in
    let iterations = compute_iterations s knowledge in
    let* spectral, limit = limit_behavior s knowledge completion iterations in
    let remainder it = Matrix.(it.state_power * s.initial_state) in
    let abstraction it = Box.(point (remainder it) + it.perturbations) in
    let iterations = Seq.map abstraction iterations in
    let+ transition = search_unrolling_stop spectral limit iterations in
    { transition ; permanent = limit }

  (* Behavior computation with timeout mechanism. *)
  let behavior ?(timeout = 1.0) ~completion_target =
    if timeout > 0.0 && 0.0 < completion_target && completion_target < 1.0 then
      let start = (Unix.times ()).tms_utime in
      let elapsed_time () = (Unix.times ()).tms_utime -. start in
      let cancel () = if elapsed_time () > timeout then Async.cancel () in
      Async.with_progress cancel @@ fun system ->
      try behavior_unbounded completion_target system
      with Async.Cancel -> None
    else fun _ -> None

  (* Pretty print a behavior. Used for test and debug purposes. *)
  let pretty_behavior fmt = function
    | None -> Unicode.pp_top fmt
    | Some { transition ; permanent } ->
      let lower, upper = Box.bounds permanent in
      let n = Vector.size lower and unrolled = List.length transition in
      let lower fmt i = K.pretty fmt (Vector.get i lower) in
      let upper fmt i = K.pretty fmt (Vector.get i upper) in
      let bounds fmt i = Format.fprintf fmt "[%a .. %a]" lower i upper i in
      let pretty i = Format.fprintf fmt "* %d : %a@ " (Finite.to_int i) bounds i in
      Format.fprintf fmt "@[<v>" ;
      Format.fprintf fmt "Transition duration : %d iterations@ " unrolled ;
      Format.fprintf fmt "State space invariant :@ " ;
      Finite.iter pretty n ;
      Format.fprintf fmt "@]"

end
