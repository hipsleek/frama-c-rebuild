(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {1 API [ACSL_importer.paste_global_annot].} *)

let paste_global_annot pfile pline cfile s ast =
  Paste.paste_global_annot ~pfile ~pline ~cfile s ast
let paste_global_annot =
  Dynamic.register
    ~plugin:"ACSL_importer"
    "paste_global_annot"
    (Datatype.func
       ~label:("pfile",None) Datatype.string
       (Datatype.func
          ~label:("pline",None) Datatype.int
          (Datatype.func
             ~label:("cfile",None) Filepath.ty
             (Datatype.func
                Datatype.string
                (Datatype.func
                   Cil_datatype.File.ty
                   Datatype.unit)))))
    paste_global_annot

let paste_global_annot
    ?(pfile="ACSL-importer-buffer") ?(pline=1) ?(cfile=Filepath.empty)
    s ast =
  paste_global_annot pfile pline cfile s ast

(** {1 API [ACSL_importer.paste_fun_spec].} *)

let paste_fun_spec kf pfile pline cfile s ast =
  Paste.paste_fun_spec kf ~pfile ~pline ~cfile s ast
let paste_fun_spec =
  Dynamic.register
    ~plugin:"ACSL_importer"
    "paste_fun_spec"
    (Datatype.func
       Kernel_function.ty
       (Datatype.func
          ~label:("pfile",None) Datatype.string
          (Datatype.func
             ~label:("pline",None) Datatype.int
             (Datatype.func
                ~label:("cfile",None) Filepath.ty
                (Datatype.func
                   Datatype.string
                   (Datatype.func
                      Cil_datatype.File.ty
                      Datatype.unit))))))
    paste_fun_spec

let get_cfile kf =
  let glob = Kernel_function.get_global kf in
  let file = Cil_datatype.(Global.loc glob |> Fileloc.path) in
  file

let paste_fun_spec
    kf ?(pfile="ACSL-importer-buffer") ?(pline=1) ?(cfile=(get_cfile kf))
    s ast =
  paste_fun_spec kf pfile pline cfile s ast

(** {1 API [ACSL_importer.paste_code_annot].} *)

let paste_code_annot kf stmt pfile pline cfile s ast =
  Paste.paste_code_annot kf stmt ~pfile ~pline ~cfile s ast

let paste_code_annot =
  Dynamic.register
    ~plugin:"ACSL_importer"
    "paste_code_annot"
    (Datatype.func
       Kernel_function.ty
       (Datatype.func
          Cil_datatype.Stmt.ty
          (Datatype.func
             ~label:("pfile",None) Datatype.string
             (Datatype.func
                ~label:("pline",None) Datatype.int
                (Datatype.func
                   ~label:("cfile",None) Filepath.ty
                   (Datatype.func
                      Datatype.string
                      (Datatype.func
                         Cil_datatype.File.ty
                         Datatype.unit)))))))
    paste_code_annot

let paste_code_annot
    kf stmt ?(pfile="ACSL-importer-buffer")
    ?(pline=1) ?(cfile=(get_cfile kf)) s ast =
  paste_code_annot kf stmt pfile pline cfile s ast

(** {1 API [ACSL_importer.import].} *)

(** Import process. *)
let import ~iDir ast nb pfile =
  Import.import ~iDir ~pfile ~init_typenames:(nb==0) ast ;
  nb+1

let import iDir files ast =
  if not (files = []) then
    begin
      let close_importation () =
        Paste.SymbolIndex.clear_temporary_table () ;
        Logic_env.reset_typenames ();
        (* importation may put additional dependencies between globals.
             Just ask for a reordering at the end of the process.
        *)
        File.reorder_custom_ast ast
        (* File.pretty_ast () *)
      in
      (*        try *)
      let nb = List.fold_left (import ~iDir ast) 0 files in
      close_importation () ;
      (*        with e ->
                  close_importation () ;
                  raise e *)
      Options.feedback "Done: %d file%s.@."
        nb
        (if nb > 1 then "s" else "")
    end

let import files1 files2 ast =
  import
    (Options.Idirs.get ())
    (files1 @ (Options.Import.get ()) @ files2)
    ast;
  Options.set_importation_off ()

let import =
  Dynamic.register
    ~plugin:"ACSL_importer"
    "import"
    (Datatype.func (Datatype.list Datatype.string)
       (Datatype.func (Datatype.list Datatype.string)
          (Datatype.func Cil_datatype.File.ty Datatype.unit)))
    import

(** {1 API [ACSL_importer.import_from_cmdline].} *)

(** Import from the cmdline process. *)
let import_from_cmdline ast =
  Options.debug ~level:2 "Importing..." ;
  import [] [] ast;
  Options.set_importation_off ()

let import_from_cmdline =
  Dynamic.register
    ~plugin:"ACSL_importer"
    "import_from_cmdline"
    (Datatype.func Cil_datatype.File.ty Datatype.unit)
    import_from_cmdline

(** {1 API [ACSL_importer.main].} *)

let dkey = Options.register_category "trace-job"

(** The main entry point. *)
let main ast =
  Options.debug ~level:2 ~dkey "Start ACSL_importer plugin...@." ;
  if Options.is_importation_on () then import_from_cmdline ast ;
  Options.debug ~level:2 ~dkey "Stop ACSL_importer plugin...@."

(** Register the function [main] as a main entry point. *)
let () =
  let main =
    Dynamic.register
      ~plugin:"ACSL_importer"
      "main"
      (Datatype.func Cil_datatype.File.ty Datatype.unit)
      main
  in
  File.add_code_transformation_after_cleanup
    ~deps:[(module Options.Import:Parameter_sig.S);
           (module Options.Run:Parameter_sig.S)]
    ~before:[Unfold_loops.transform] Options.main_import main
