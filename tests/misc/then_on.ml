module P = Plugin.Register
    (struct
      let name = "Testing plugin"
      let shortname = "test"
      let help = "Just to test -then-on"
    end)
module PrintProj = P.False
    (struct
      let option_name = "-print-proj"
      let help = "Print the name of the project"
    end)
module CreateProj = P.String
    (struct
      let option_name = "-create-proj"
      let arg_name = "project_name"
      let help = "Create a new project named project_name"
      let default = ""
    end)

let main () =
  if CreateProj.is_set () then begin
    let p = Project.create (CreateProj.get ()) in
    P.feedback "Created project %S" (Project.get_name p)
  end;
  if PrintProj.get () then begin
    let p = Project.current () in
    P.feedback "Current project is %S" (Project.get_name p)
  end

let () = Boot.Main.extend main
