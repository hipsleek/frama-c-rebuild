open Cil_types

include Plugin.Register(
  struct
    let name = "AST diff test"
    let shortname = "adt"
    let help = "Show results of AST diff computation"
  end)

let show_var vi c =
  result "Variable %a: %a"
    Cil_datatype.Varinfo.pretty vi
    Ast_diff.Varinfo.pretty_data c

let show_fun kf c =
  result "Function %a: %a"
    Kernel_function.pretty kf
    Ast_diff.Kernel_function.pretty_data c

let cmp_fun kf1 kf2 =
  Datatype.String.compare
    (Kernel_function.get_vi kf1).vname (Kernel_function.get_vi kf2).vname

let cmp_var v1 v2 =
  let res = Datatype.String.compare v1.vname v2.vname in
  if res <> 0 then res
  else if v1.vglob && v2.vglob then 0
  else if v1.vglob then -1
  else if v2.vglob then 1
  else if v1.vstorage = Static && v2.vstorage = Static then 0
  else if v1.vstorage = Static then -1
  else if v2.vstorage = Static then 1
  else begin
    let prj = Ast_diff.Orig_project.get() in
    let kf1 = Project.on prj Kernel_function.find_defining_kf v1 in
    let kf2 = Project.on prj Kernel_function.find_defining_kf v2 in
    if Option.is_none kf1 || Option.is_none kf2 then
      fatal "Variable %a(%a) is not global but has no associated function"
        Cil_datatype.Varinfo.pretty v1 Fileloc.pretty v1.vdecl;
    cmp_fun (Option.get kf1) (Option.get kf2)
  end

let show_correspondences () =
  if Kernel.AstDiff.get () then begin
    result "Showing correspondences between %s and %s"
      (Project.get_name (Ast_diff.Orig_project.get()))
      (Project.get_name (Project.current()));
    Ast_diff.Varinfo.iter_sorted ~cmp:cmp_var show_var;
    Ast_diff.Kernel_function.iter_sorted ~cmp:cmp_fun show_fun;
  end

let () = Boot.Main.extend show_correspondences
