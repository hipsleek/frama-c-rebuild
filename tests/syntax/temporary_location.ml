open Cil_types

class vis = object(_)
  inherit Visitor.frama_c_inplace

  method! vvrbl vi =
    Kernel.result "%s -> %a" vi.vname Fileloc.pretty vi.vdecl;
    Cil.DoChildren

end

let main () =
  Ast.compute ();
  Cil.visitCilFile (new vis :> Cil.cilVisitor) (Ast.get ())

let () =
  Boot.Main.extend main
