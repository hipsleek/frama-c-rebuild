(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Pasting module. *)

(**  {1 Can be journalized (can be external API)} *)

module MacroIndex: sig
  (** Macro table management. *)

  type scope_t = Sfile | Smodule | Sfunction

  val clear_macro_table : scope_t -> unit
  (** To clear macro table in order to free memory,
      without clearing the result dependencies. *)

end

module SymbolIndex: sig
  (** Symbol table management. *)

  val self : State.t
  (** Dependencies of the result of pasting functions. *)

  val clear_temporary_table: unit -> unit
  (** To clear temporary index table in order to free memory,
      without clearing the result dependencies. *)
end

val paste_global_annot: pfile:string -> pline:int
  -> cfile:Filepath.t -> string -> Cil_types.file -> unit
val paste_fun_spec: Kernel_function.t -> pfile:string -> pline:int
  -> cfile:Filepath.t -> string -> Cil_types.file -> unit
val paste_code_annot:
  Kernel_function.t -> Cil_types.stmt -> pfile:string -> pline:int
  -> cfile:Filepath.t -> string -> Cil_types.file -> unit


(** {1 Not journalized (internal API)} *)

val add_macro: is_global_scope:bool -> string -> Logic_ptree.lexpr -> unit
val add_global_annot: Logic_ptree.decl list -> unit
val add_funspec: Logic_ptree.spec -> Cil_types.location -> unit
val add_annots: ?loop_number:int -> Cil_datatype.Stmt.Set.t -> Cil_types.location -> Logic_ptree.code_annot list -> unit

val set_prop_loc: Filepath.t -> int -> unit
val set_buff_loc: int -> unit
val set_current_module: is_from_file_name:bool -> string -> unit
val init_ast:
  file:Filepath.t -> init_module_from_file_name:bool ->init_typenames:bool ->
  Cil_types.file -> unit
val set_current_function: string*Cil_types.location -> unit
val add_buffer: string -> unit
val buffer: Buffer.t


val paste_post : behav:string -> unit
val paste_at_func_behavior : clause:string -> behav:string -> unit
val paste_at_func: clause:string -> unit
val paste_at_stmt: clause:string -> loop:string -> label:string -> unit
val paste_at_loop: clause:string -> loop:string -> unit
val paste_at_global : clause:string -> unit

(** {1 CIL interface (internal API). } *)

val find_kf: string -> Kernel_function.t
exception Kf_not_found
exception Stmt_not_found of Kernel_function.t
val find_stmt_set_from_sid: ?source:Filepos.t -> int -> Cil_datatype.Stmt.Set.t
val find_stmt_set_from_label: ?source:Filepos.t -> string -> Cil_datatype.Stmt.Set.t
val find_stmt_set_from_return: ?source:Filepos.t -> unit -> Cil_datatype.Stmt.Set.t
val find_stmt_set_from_call_to: ?source:Filepos.t -> Kernel_function.t option -> int -> Cil_datatype.Stmt.Set.t
val find_stmt_set_from_call_number: ?source:Filepos.t -> int -> Cil_datatype.Stmt.Set.t
val find_stmt_set_from_asm_number: ?source:Filepos.t -> int -> Cil_datatype.Stmt.Set.t
val find_loop_stmt_set_from_loop_number: ?source:Filepos.t -> int -> Cil_datatype.Stmt.Set.t
val find_loop_body_set_from_loop_number: ?source:Filepos.t -> int -> Cil_datatype.Stmt.Set.t

val loop_number_attr_name: int -> string
val loop_body_attr_name: int -> string
val hidden_attr: string
