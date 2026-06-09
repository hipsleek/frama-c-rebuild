(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Compute Init WP *)

module Make(W : Mcfg.S) =
struct

  let compute_global_init wenv filter obj =
    Globals.Vars.fold_in_file_rev_order
      (fun var initinfo obj ->

         if var.vstorage = Extern then obj else
           let do_init = match filter with
             | `All -> true
             | `InitConst -> Cil.isGlobalInitConst var
           in if not do_init then obj
           else
             Current_loc.with_loc var.vdecl
               (W.init wenv var initinfo.init) obj
      ) obj

  let process_global_const wenv obj =
    Globals.Vars.fold_in_file_rev_order
      (fun var _initinfo obj ->
         if Cil.isGlobalInitConst var
         then W.const wenv var obj
         else obj
      ) obj

  (* WP of global initializations. *)
  let process_global_init wenv kf obj =
    if CfgInfos.is_entry_point kf then
      begin
        let obj = W.label wenv None Clabels.init obj in
        compute_global_init wenv `All obj
      end
    else if W.has_init wenv then
      begin
        let obj =
          if Wp_parameters.Init.get ()
          then process_global_const wenv obj else obj in
        let obj = W.use_assigns wenv None WpPropId.mk_init_assigns obj in
        let obj = W.label wenv None Clabels.init obj in
        compute_global_init wenv `All obj
      end
    else
    if Wp_parameters.Init.get ()
    then compute_global_init wenv `InitConst obj
    else obj

end
