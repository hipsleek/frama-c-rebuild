open Cil_types

let gen_exits _ _ = [ ]
let status_exits = Property_status.Dont_know

let gen_assigns _ _ = WritesAny
let status_assigns = Property_status.Dont_know

let gen_requires _ _ = [ ]

let gen_allocates _ _ = FreeAllocAny
let status_allocates = Property_status.Dont_know

let run () =
  let generate_spec kf =
    Populate_spec.populate_funspec kf [`Exits; `Assigns; `Requires; `Allocates]
  in
  Globals.Functions.iter generate_spec

let populate () =
  Format.printf "Registering an mode that does nothing@.";
  Populate_spec.register
    ~gen_exits ~status_exits
    ~gen_assigns ~status_assigns
    ~gen_requires
    ~gen_allocates ~status_allocates
    "donothing"

let () = Cmdline.run_after_configuring_stage populate

let () = Boot.Main.extend run
