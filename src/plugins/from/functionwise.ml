(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)



module Tbl =
  Kernel_function.Make_Table
    (Eva.Assigns)
    (struct
      let name = "Functionwise dependencies"
      let size = 17
      let dependencies = [ Eva.Analysis.self ]
    end)

(* Forward reference to a function computing the from for a given function *)
let force_compute = ref (fun _ -> assert false)

module To_Use = struct
  let stmt_request stmt = Eva.Results.before stmt

  let memo kf =
    Tbl.memo
      (fun kf ->
         !force_compute kf;
         try Tbl.find kf
         with Not_found -> invalid_arg "could not compute dependencies")
      kf

  let get_from_call kf _ = memo kf

  let keep_base kf = (* Eta-expansion required *)
    Eva.Logic_inout.accept_base ~formals:false ~locals:false kf

  let cleanup kf froms =
    if Eva.Assigns.Memory.is_bottom froms.Eva.Assigns.memory
    then froms
    else
      let accept_base =
        Eva.Logic_inout.accept_base ~formals:true ~locals:false kf
      in
      let f b intervs =
        if accept_base b
        then Memory_zone.inject b intervs
        else Memory_zone.bottom
      in
      let joiner = Memory_zone.join in
      let cache = Hptmap_sig.TemporaryCache "from cleanup" in
      let zone_substitution =
        Memory_zone.cached_fold ~cache ~f ~joiner ~empty:Memory_zone.bottom
      in
      let zone_substitution x =
        try
          zone_substitution x
        with Abstract_interp.Error_Top -> Memory_zone.top
      in
      let map_zone = Eva.Deps.map zone_substitution in
      { memory = From_memory.map map_zone froms.memory;
        return = Eva.Deps.map zone_substitution froms.return;
      }

  let cleanup_and_save kf froms =
    let froms = cleanup kf froms in
    Tbl.add kf froms;
    froms
end

module From = From_compute.Make(To_Use)
let () = force_compute := From.compute


let self = Tbl.self

let compute kf = ignore (To_Use.memo kf)

let compute_all () =
  Eva.Analysis.compute () ;
  Callgraph.Uses.iter_in_rev_order @@ fun kf ->
  let is_definition = Kernel_function.is_definition kf in
  if is_definition && Eva.Results.is_called kf then compute kf

let is_computed = Tbl.mem

let get = To_Use.memo

let pretty fmt v =
  From_memory.pretty_with_type (Kernel_function.get_type v) fmt (get v)
