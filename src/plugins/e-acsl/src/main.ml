(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Resulting_projects =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Project.Datatype)
    (struct
      let name = "E-ACSL resulting projects"
      let size = 7
      let dependencies = Ast.self :: Options.parameter_states
    end)

let generate_code =
  Resulting_projects.memo
    (fun name ->
       Options.feedback "beginning translation.";
       Temporal.enable (Options.Temporal_validity.get ());
       Options.setup ();
       (* slightly more efficient to copy the project before computing the AST
          if it is not yet computed *)
       let rtl_prj_name = Options.Project_name.get () ^ " RTL" in
       let rtl_prj = Project.create_by_copy ~last:false rtl_prj_name in
       (* force AST computation before copying the project it belongs to *)
       Ast.compute ();
       let copied_prj = Project.create_by_copy ~last:true name in
       Project.on
         copied_prj
         (fun () ->
            (* preparation of the AST does not concern the E-ACSL RTL:
               do it first *)
            Prepare_ast.prepare ();
            Analyses.check_integrity ();
            Memory_tracking.SpecialPointers.initialize ();
            Rtl.link rtl_prj;
            (* the E-ACSL type system ensures the soundness of the generated
               arithmetic operations. Therefore, deactivate the corresponding
               options in order to prevent RTE to generate spurious alarms. *)
            let signed = Kernel.SignedOverflow.get () in
            let unsigned = Kernel.UnsignedOverflow.get () in
            (* we do know that setting these flags does not modify the program's
               semantics: using their unsafe variants is thus safe and preserve
               all emitted property statuses. *)
            Kernel.SignedOverflow.unsafe_set false;
            Kernel.UnsignedOverflow.unsafe_set false;
            let finally () =
              Kernel.SignedOverflow.unsafe_set signed;
              Kernel.UnsignedOverflow.unsafe_set unsigned
            in
            Fun.protect
              ~finally
              (fun () ->
                 Gmp_types.init ();
                 Analyses.preprocess ();
                 Injector.inject ()) ;
            Analyses.check_integrity ();
            (* remove the RTE's results computed from E-ACSL: they are partial
               and associated with the wrong kernel function (the one of the old
               project). *)
            (* [TODO] what if RTE was already computed? To be fixed when
               redoing the RTE management system  *)
            let selection =
              State_selection.union
                (Rte.get_state_selection_with_dependencies ())
                (State_selection.with_dependencies Options.Run.self)
            in
            Project.clear ~selection ~project:copied_prj ();
            Resulting_projects.mark_as_computed ())
         ();
       if not (Options.debug_atleast 1) then Project.remove ~project:rtl_prj ();
       Options.feedback "translation done in project \"%s\"."
         (Options.Project_name.get ());
       copied_prj)

let generate_code =
  Dynamic.register
    ~plugin:"E_ACSL"
    "generate_code"
    (Datatype.func Datatype.string Project.ty)
    generate_code

(* The Frama-C standard library contains specific built-in variables prefixed by
   "__fc_" and declared as extern: they prevent the generated code to be
   linked. Some are used to represent internal states of the specifications and
   should not be printed. Others are used as targets of standard library
   macros like stdout or errno and in that case the original macro should be
   printed instead.
   Moreover, the builtins for VLA allocation and deallocation are specific to
   Frama-C. This printer reprint the original builtins (or nothing for the
   deallocation).

   TODO: could be done by the Frama-C default printer at some points, but since
   the transformation is very specific to E-ACSL it should probably be
   configurable. *)
let change_printer =
  (* not projectified on purpose: this printer change is common to each
     project. *)
  let first = ref true in
  fun () ->
    if !first then begin
      first := false;
      let module Printer_class(X: Printer.PrinterClass) = struct
        class printer () = object
          inherit X.printer () as super

          method !varinfo fmt vi =
            if Functions.Libc.is_vla_alloc_name vi.Cil_types.vname then
              (* Replace VLA allocation with calls to [__builtin_alloca] *)
              Format.fprintf fmt "%s" Functions.Libc.actual_alloca
            else
              let replacement =
                Ast_attributes.find_fc_stdlib_extern_replacement vi.vattr
              in
              match replacement with
              | Some replacement ->
                (* The varinfo is replacing a libc macro, print the replaced
                   name. *)
                Format.pp_print_string fmt replacement
              | None ->
                (* Otherwise use the original printer *)
                super#varinfo fmt vi

          method !instr fmt i =
            match i with
            | Call(_, fct, _, _) when Functions.Libc.is_vla_free fct ->
              (* Nothing to print: VLA deallocation is done automatically when
                 leaving the scope *)
              Format.fprintf fmt "/* ";
              super#instr fmt i;
              Format.fprintf fmt " */"
            | _ ->
              super#instr fmt i

          method !global fmt g =
            let is_vla_builtin vi =
              Functions.Libc.is_vla_alloc_name vi.Cil_types.vname ||
              Functions.Libc.is_vla_free_name vi.Cil_types.vname
            in
            let is_fc_internal (vi : Cil_types.varinfo) =
              vi.vstorage == Cil_types.Extern &&
              Ast_attributes.(contains fc_stdlib_internal vi.vattr)
            in
            match g with
            | GFunDecl (_, vi, _) when is_vla_builtin vi ->
              (* Nothing to print: the VLA builtins don't have an original libc
                 version. *)
              ()
            | GFunDecl (_, vi, _) | GVarDecl (vi, _) when is_fc_internal vi ->
              (* Do not print definitions internal to Frama-C's libc. *)
              ()
            | _ ->
              super#global fmt g
        end
      end in
      Printer.update_printer (module Printer_class: Printer.PrinterExtension)
    end

let main () =
  if Options.Run.get () then begin
    change_printer ();
    ignore (generate_code (Options.Project_name.get ()));
  end

let () = Boot.Main.extend main
