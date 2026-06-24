include Plugin.Register
    (struct
      let name = "HipSleek"
      let shortname = "hipsleek"
      let help = "Verify C programs using the HipSleek separation-logic engine"
    end)

let group = add_group "HipSleek Options"

let () = Parameter_customize.set_group group
module Enable =
  False(struct
    let option_name = "-hipsleek"
    let help = "Run HipSleek on all functions annotated with /* SL ... */ specs"
  end)

let () = Parameter_customize.set_group group
module HipPath =
  String(struct
    let option_name = "-hipsleek-path"
    let help = "Path to the hip.exe binary"
    let arg_name = "path"
    let default = ""
  end)

let () = Parameter_customize.set_group group
module OutputDir =
  String(struct
    let option_name = "-hipsleek-output-dir"
    let help = "Directory for generated .ss files (default: system temp)"
    let arg_name = "dir"
    let default = ""
  end)

let () = Parameter_customize.set_group group
module ProofLog =
  False(struct
    let option_name = "-hipsleek-proof-log"
    let help =
      "Capture HipSleek's ESL proof log and show per-function proof detail \
       (entailments + verdict) in the property description"
  end)
