(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let is_call_to_sync stmt =
  match stmt.skind with
  | Instr (Call (_, Var vi, _, _))
    when vi.vname = "Frama_C_mthread_sync" -> true
  (* No Local_init possible here, as Frama_C_mthread_sync returns void. *)
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* --- Pretty-printing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let kinstr_to_source = function
  | Kglobal -> None
  | Kstmt stmt -> Some (fst (Cil_datatype.Stmt.loc stmt))


let pretty_succs fmt stmt =
  (Pretty_utils.pp_list ~sep:" "
     (fun fmt s -> Format.fprintf fmt "%d" s.sid)) fmt stmt.succs



(* -------------------------------------------------------------------------- *)
(* --- Stacks                                                             --- *)
(* -------------------------------------------------------------------------- *)

type stack_elt = kernel_function * kinstr

module StackElt = struct
  include Datatype.Pair(Kernel_function)(Cil_datatype.Kinstr)

  let pretty fmt (f, ki) =
    Format.fprintf fmt "@[<hov 2>%s%t@]"
      (Ast_info.Function.get_name f.fundec)
      (fun fmt -> match ki with
         | Kstmt stmt ->
           let loc = Cil_datatype.Stmt.loc stmt in
           Format.fprintf fmt " :: %a" Fileloc.pretty loc
         | Kglobal -> ()
      )

end

type stack = stack_elt list

module Stack = struct

  include Datatype.List(StackElt)

  let pretty =
    Pretty_utils.pp_list ~pre:"@[<hv>" ~sep:" <-@ " ~suf:"@]" StackElt.pretty

  module FunAccessVars =
    State_builder.Option_ref(Cil_datatype.Kf)
      (struct let dependencies = [Ast.self]
        let name = "Stack.FunAccessVars"
      end)

  let fun_access_vars () =
    FunAccessVars.memo (fun () -> Cil_datatype.Kf.dummy)

  let access_to_var stmt : stack_elt = fun_access_vars (), Kstmt stmt
  let is_access_to_var (kf, _) =
    Kernel_function.equal kf (fun_access_vars ())

end
