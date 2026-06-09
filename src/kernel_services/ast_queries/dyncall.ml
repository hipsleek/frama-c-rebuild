(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Logic_typing
open Logic_ptree
open Cil_datatype

(* -------------------------------------------------------------------------- *)
(* --- Typing                                                             --- *)
(* -------------------------------------------------------------------------- *)

let find_call env loc f =
  try env.find_var f
  with Not_found ->
    env.error loc "Unknown function '%s'" f

let typecheck typing_context loc ps =
  ignore loc ;
  let fs =
    List.map
      (fun p ->
         let loc = p.lexpr_loc in
         match p.lexpr_node with
         | PLvar f ->
           let fv = find_call typing_context loc f in
           Logic_const.term ~loc (TLval(TVar fv,TNoOffset)) fv.lv_type
         | _ ->
           typing_context.error loc "Function name expected for calls"
      ) ps
  in
  Ext_terms fs

(* -------------------------------------------------------------------------- *)
(* --- Recover                                                            --- *)
(* -------------------------------------------------------------------------- *)

let get_call t = match t.term_node with
  | TLval (TVar { lv_origin = Some v } , TNoOffset ) -> Globals.Functions.get v
  | _ -> raise Not_found

let get_calls ecmd bhvs : (string * Kernel_function.t list) list =
  List.fold_right
    (fun bhv calls ->
       let fs = ref [] in
       List.iter
         (function
           | {ext_name; ext_kind = Ext_terms ts} when ext_name = ecmd ->
             fs := !fs @ List.map get_call ts
           | _ -> ())
         bhv.Cil_types.b_extended ;
       let fs = !fs in
       if fs <> [] then (bhv.Cil_types.b_name , fs) :: calls else calls
    ) bhvs []

let pp_calls fmt calls =
  List.iter
    (fun kf -> Format.fprintf fmt "@ %a" Kernel_function.pretty kf)
    calls

(* -------------------------------------------------------------------------- *)
(* --- Dynamic Calls                                                      --- *)
(* -------------------------------------------------------------------------- *)

module Point = Datatype.Pair_with_collections(Datatype.String)(Stmt)
module Calls = Datatype.Pair(Property)(Datatype.List(Kernel_function))
module CInfo =
struct
  let name = "Dyncall.CallPoints"
  let dependencies = [Ast.self]
  let size = 63
end
module CallPoints = State_builder.Hashtbl(Point.Hashtbl)(Calls)(CInfo)

let property ~kf ~bhv ~stmt calls =
  let fact =
    if bhv = Cil.default_behavior_name then
      Format.asprintf "@[<hov 2>call point%a@]"
        pp_calls calls
    else
      Format.asprintf "@[<hov 2>call point%a for %s@]"
        pp_calls calls bhv
  in
  Property.(ip_other fact (OLStmt (kf,stmt)))

(* -------------------------------------------------------------------------- *)
(* --- Detection                                                          --- *)
(* -------------------------------------------------------------------------- *)

let emitter = Emitter.create "Dyncall"
    [ Emitter.Property_status ]
    ~correctness:[]
    ~tuning:[]

class dyncall =
  object(self)
    inherit Visitor.frama_c_inplace

    val mutable count = 0
    val mutable scope = []
    val block_calls = Stack.create ()

    method count = count

    method private kf =
      match self#current_kf with None -> assert false | Some kf -> kf

    method private stmt =
      match self#current_stmt with None -> assert false | Some stmt -> stmt

    method! vfunc _ =
      scope <- [] ;
      DoChildren

    method! vcode_annot ca =
      match ca.annot_content with
      | Cil_types.AExtended
          (bhvs, _,
           ({ext_name = "calls"; ext_kind = Ext_terms calls} as extended)) ->
        if calls <> [] && (scope <> [] || not (Stack.is_empty block_calls))
        then begin
          let bhvs =
            match bhvs with
            | [] -> [ Cil.default_behavior_name ]
            | bhvs -> bhvs
          in
          let debug_calls bhv stmt kfs =
            let source = snd (Stmt.loc stmt) in
            let dkey = Kernel.dkey_dyncalls in
            Kernel.feedback ~source ~dkey
              "@[<hov 2>Calls %s%a@]"
              (if Cil.default_behavior_name <> bhv then "(for " ^ bhv ^ ")"
               else "")
              pp_calls kfs
          in
          let pool = ref [] in (* collect emitted properties *)
          let add_calls_info kf stmt =
            count <- succ count ;
            List.iter
              (fun bhv ->
                 let kfs = List.map get_call calls in
                 debug_calls bhv stmt kfs ;
                 let prop = property ~kf ~bhv ~stmt kfs in
                 pool := prop :: !pool ;
                 CallPoints.add (bhv,stmt) (prop,kfs))
              bhvs
          in
          let kf = self#kf in
          List.iter
            (add_calls_info kf)
            (if scope <> [] then scope else Stack.top block_calls) ;
          if !pool <> [] then
            begin
              let eloc = Property.ELStmt(kf,self#stmt) in
              let annot = Property.ip_of_extended eloc extended in
              Property_status.logical_consequence emitter annot !pool ;
            end
        end;
        SkipChildren
      | _ -> SkipChildren

    method! vspec spec =
      let calls = get_calls "instanceof" spec.Cil_types.spec_behavior in
      if calls <> [] then
        begin
          match self#current_kf with None -> () | Some kf ->
            List.iter
              (fun (bhv,kfs) ->
                 Kernel.(feedback ~dkey:dkey_dyncalls
                           "@[<hov 2>%a for %s instance of%a"
                           Kernel_function.pretty kf bhv pp_calls kfs))
              calls
        end;
      SkipChildren

    method! vstmt_aux s =
      match s.skind with
      | Instr (Call( _ , fct , _ , _ ))
        when Kernel_function.get_called fct = None ->
        if not (Stack.is_empty block_calls) then
          Stack.push (self#stmt :: Stack.pop block_calls) block_calls;
        scope <- self#stmt :: scope ;
        Cil.DoChildrenPost (fun s -> scope <- []; s)
      | Block _ ->
        Stack.push [] block_calls;
        Cil.DoChildrenPost
          (fun s ->
             let calls = Stack.pop block_calls in
             if not (Stack.is_empty block_calls) then
               Stack.push (calls @ Stack.pop block_calls) block_calls;
             s)
      | _ -> Cil.DoChildren

  end

let compute =
  let compute () =
    begin
      Kernel.(feedback ~dkey:dkey_dyncalls "Computing dynamic calls.") ;
      let d = new dyncall in
      Visitor .visitFramacFile (d :> Visitor.frama_c_visitor) (Ast.get()) ;
      let n = d#count in
      if n > 0 then
        Kernel.(feedback ~dkey:dkey_dyncalls "Dynamic call(s): %d." n)
      else
        Kernel.(feedback ~dkey:dkey_dyncalls "No dynamic call.")
    end
  in fst (State_builder.apply_once "Wp.Dyncall.compute"
            [Ast.self] compute)

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let get ?bhv stmt =
  compute () ;
  let get bhv =
    try Some (CallPoints.find (bhv,stmt))
    with Not_found -> None
  in
  match bhv with
  | None -> get Cil.default_behavior_name
  | Some bhv ->
    (match get bhv with
     | None -> get Cil.default_behavior_name
     | result -> result)

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let register_extensions () =
  Acsl_extension.register_code_annot_next_stmt ~plugin:"kernel"
    "calls" typecheck true;
  Acsl_extension.register_behavior ~plugin:"kernel"
    "instanceof" typecheck true

let register_once, _ =
  State_builder.apply_once "Dyncall.register_extensions" [] register_extensions

let () = Cmdline.run_after_early_stage register_once
