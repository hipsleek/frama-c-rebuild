(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types


class virtual do_it_ = object(self)
  inherit [Memory_zone.t] Cumulative_analysis.cumulative_visitor
  val mutable derefs = Memory_zone.bottom

  method bottom = Memory_zone.bottom

  method result = derefs

  method join new_ =
    derefs <- Memory_zone.join new_ derefs;

  method! vlval (base,_ as lv) =
    begin match base with
      | Var _ -> ()
      | Mem e ->
        let stmt = Option.get self#current_stmt in
        let r = Eva.Results.(before stmt |> eval_exp e |> as_cvalue) in
        let addr = Addresses.Bits.of_bytes r in
        let size = Bit_utils.sizeof_lval lv in
        self#join
          (Locations.enumerate_valid_bits Read (Locations.make addr size))
    end;
    DoChildren

  method compute_funspec (_: kernel_function) =
    Memory_zone.bottom

  method clean_kf_result (_ : kernel_function) (r: Memory_zone.t) = r

end

module Analysis = Cumulative_analysis.Make(
  struct
    let analysis_name ="derefs"

    type t = Memory_zone.t
    module T = Memory_zone

    class virtual do_it = do_it_
  end)

let get_internal = Analysis.kernel_function

let externalize _return fundec x =
  Memory_zone.filter_base
    (fun v -> not (Base.is_formal_or_local v fundec))
    x

module Externals =
  Kernel_function.Make_Table(Memory_zone)
    (struct
      let name = "Inout.Derefs.Externals"
      let dependencies = [ Analysis.Memo.self ]
      let size = 17
    end)

let get_external =
  Externals.memo
    (fun kf ->
       Eva.Analysis.compute ();
       if Kernel_function.is_definition kf then
         try
           externalize
             (Kernel_function.find_return kf)
             (Kernel_function.get_definition kf)
             (get_internal kf)
         with Kernel_function.No_Statement ->
           assert false
       else
         (* assume there is no deref for leaf functions *)
         Memory_zone.bottom)

let compute_external kf = ignore (get_external kf)

let _pretty_internal fmt kf =
  Format.fprintf fmt "@[Derefs (internal) for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Memory_zone.pretty (get_internal kf)

let pretty_external fmt kf =
  Format.fprintf fmt "@[Derefs for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Memory_zone.pretty (get_external kf)
