class vis prj = object(this)
  inherit Visitor.frama_c_copy prj
  method! vstmt_aux _ =
    this#queueInstr [ Cil_datatype.Instr.dummy ];
    Cil.DoChildren
end

let () =
  Boot.Main.extend (fun () ->
      ignore (File.create_project_from_visitor "A" (new vis)))
