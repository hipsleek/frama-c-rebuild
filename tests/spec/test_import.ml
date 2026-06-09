open Cil_types
open Logic_typing


module Plugin1 =
  Plugin.Register
    (struct
      let name = "MyPlugin1"
      let shortname = "myplugin1"
      let help = ""
    end)

module Plugin2 =
  Plugin.Register
    (struct
      let name = "MyPlugin2"
      let shortname = "myplugin2"
      let help = ""
    end)

let () = Format.printf "[test-import] Linking.@."

let loader (ctxt: module_builder) (loc: location) (m: string list) =
  begin
    Format.printf "[test-import:%d] Loading %s.@."
      (Fileloc.line loc) (String.concat "::" m) ;
    let t = Cil_const.make_logic_type "t" in
    let check = Cil_const.make_logic_info "check" in
    let x = Cil_const.make_logic_var_formal "x" (Ltype(t,[])) in
    let k = Cil_const.make_logic_var_formal "k" Linteger in
    check.l_profile <- [x;k] ;
    ctxt.add_logic_type loc t ;
    ctxt.add_logic_function loc check ;
  end

let register () =
  Acsl_extension.register_module_importer ~plugin:"myplugin1" "foo" loader;
  Acsl_extension.register_module_importer ~plugin:"myplugin1" "bar" loader;
  Acsl_extension.register_module_importer ~plugin:"myplugin2" "foo" loader;
  if Kernel.GeneralDebug.get () = 1 then
    Acsl_extension.register_module_importer ~plugin:"myplugin1" "foo" loader

let () = Cmdline.run_after_extended_stage register
