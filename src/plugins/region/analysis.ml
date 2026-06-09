(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Projectification                                                  --- *)
(* -------------------------------------------------------------------------- *)

module DOMAIN : Datatype.S with type t = Code.domain =
  Datatype.Make
    (struct
      type t = Code.domain
      include Datatype.Undefined
      let name = "Region.Analysis.MEMORY"
      let mem_project = Datatype.never_any_project
      let reprs = [ Memory.create () ]
    end)

module STATE = State_builder.Hashtbl(Kernel_function.Hashtbl)(DOMAIN)
    (struct
      let size = 0
      let name = "Region.Analysis.STATE"
      let dependencies = [Ast.self]
    end)

(* -------------------------------------------------------------------------- *)
(* ---  Memoized Access                                                   --- *)
(* -------------------------------------------------------------------------- *)

let dump = Options.register_category "dump"
    ~help:"Dump regions for each analyzed function"

let find = STATE.find

let get kf =
  try STATE.find kf with Not_found ->
    Options.feedback ~ontty:`Transient
      "Function %a%t" Kernel_function.pretty kf Unicode.pp_ellipsis ;
    let domain = Code.domain kf in
    STATE.add kf domain ;
    if Options.is_debug_key_enabled dump then
      Options.result "@[<v 2>Function %a:%t@]@."
        Kernel_function.pretty kf
        begin fun fmt ->
          List.iter
            begin fun r ->
              Format.pp_print_newline fmt () ;
              Memory.pp_region fmt r ;
            end @@
          Memory.regions domain ;
        end ;
    domain

let compute kf = ignore @@ get kf

let add_hook f = STATE.add_hook_on_change (fun _ -> f())

(* -------------------------------------------------------------------------- *)
