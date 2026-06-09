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

let type_foo typing_context _loc l =
  let preds =
    List.map
      (typing_context.type_predicate
         typing_context typing_context.pre_state)
      l
  in
  Cil_types.Ext_preds preds

let catch msg f x =
  try
    Kernel.feedback "Triggering %s :" msg;
    ignore(f x)
  with _ -> ()

let cover_errors () =
  catch "User Error" Logic_env.extension_from "foo";
  catch "Unsupported" (Logic_env.preprocess_extension ~plugin:"myplugin1") "bar"
[@@alert "-acsl_extension_from"]

let cover_warnings () =
  Acsl_extension.register_behavior ~plugin:"myplugin1" "foo" type_foo false;
  Acsl_extension.register_behavior ~plugin:"myplugin1" "unfold" type_foo false

let () =
  Format.printf "[test-extend_errors] Linking.@.";
  Acsl_extension.register_behavior ~plugin:"myplugin1" "foo" type_foo false;
  Acsl_extension.register_behavior ~plugin:"myplugin2" "foo" type_foo false;
  Acsl_extension.register_behavior ~plugin:"myplugin2" "bar" type_foo false;

  if Kernel.GeneralDebug.get () = 1 then begin
    cover_warnings ();
    cover_errors ()
  end
