let plugin_name = "Test"

include Plugin.Register (struct
    let name = plugin_name
    let shortname = String.lowercase_ascii plugin_name
    let help = "A fake plugin for test purpose"
  end)

module TestDecimal = Float (struct
    let option_name = "-test-decimal-option"
    let help = "test option"
    let arg_name = "x"
    let default = 0.0
  end)

let () =
  TestDecimal.set_range ~min:0.0 ~max:1.0;
  TestDecimal.add_aliases ~deprecated:true
    ["-test-deprecated-option"]

module TestDecimalDefaultRange = Float (struct
    let option_name = "-test-default-range"
    let help = "test option"
    let arg_name = "x"
    let default = 0.0
  end)

let run_test () =
  let _range = TestDecimal.get_range () in
  let _range = TestDecimalDefaultRange.get_range () in
  feedback "-test-decimal-option set to %f" (TestDecimal.get ())

let () =
  Boot.Main.extend run_test
