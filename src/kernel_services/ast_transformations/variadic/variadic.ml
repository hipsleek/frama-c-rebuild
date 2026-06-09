(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let category = File.register_code_transformation_category "variadic"

(* Variadic will create prototype and specifications for some variadic
   functions. Since only prototypes are created, the resulting source code isn't
   compilable. This printer will print the original functions, with the replaced
   prototypes in comments beside the instruction. *)
let change_printer =
  let first = ref true in
  fun () ->
    if !first then begin
      first := false;
      let module Printer_class(X: Printer.PrinterClass) = struct
        class printer () = object
          inherit X.printer () as super

          method !instr fmt i =
            match i with
            (* If the instruction calls a function that have been replaced,
               then build an instruction with the old function. *)
            | Call(res, Var vi, args, loc)
              when Replacements.mem vi ->
              let old_vi = Replacements.find vi in
              let old_vi = { vi with vname = old_vi.vname } in
              let old_instr =
                Call(res, Var old_vi, args, loc)
              in
              Format.fprintf fmt "%a /* %s */" super#instr old_instr vi.vname
            (* Otherwise keep the instruction. *)
            | _ ->
              super#instr fmt i
        end
      end in
      Printer.update_printer (module Printer_class: Printer.PrinterExtension)
    end

let translate file =
  if Kernel.VariadicTranslation.get () then begin
    change_printer ();
    Translate.translate_variadics file
  end

let () =
  Cmdline.run_after_extended_stage
    begin fun () ->
      State_dependency_graph.add_dependencies
        ~from:Kernel.VariadicTranslation.self
        [ Ast.self ]
    end;
  Cmdline.run_after_configuring_stage
    begin fun () ->
      File.add_code_transformation_before_cleanup category translate
    end
