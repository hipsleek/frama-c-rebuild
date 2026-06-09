open Cil_types

class pathcrawlerVisitor prj =
  object(self)
    inherit Visitor.frama_c_copy prj

    method! vfunc fundec =
      Format.printf "Considering sspec of function %s@." fundec.svar.vname;
      Format.printf "@[Funspec of %s is@ @['%a'@]@ through visitor@]@."
        fundec.svar.vname
        Printer.pp_funspec fundec.sspec;
      let kf = Globals.Functions.get fundec.svar in
      Format.printf "@[It is@ @['%a'@]@ through get_spec@]@."
        Printer.pp_funspec
        (Annotations.funspec kf);
      DoChildren

    method! vspec sp =
      Format.printf "Considering vspec of function %s@."
        (Kernel_function.get_name (Option.get self#current_kf));
      (match self#current_func with
       | Some fundec ->
         Format.printf "@[Funspec of %s is@ @['%a'@]@ through visitor@]@."
           fundec.svar.vname
           Printer.pp_funspec sp;
         let kf = Globals.Functions.get fundec.svar in
         Format.printf "@[It is@ @['%a'@]@ through get_spec@]@."
           Printer.pp_funspec
           (Annotations.funspec kf);
       | None ->
         Format.printf "@[Function prototype;@ Funspec is@ @['%a'@]@]@."
           Printer.pp_funspec sp;
      );
      DoChildren
  end

let startup () =
  let generate_spec kf =
    Populate_spec.populate_funspec ~do_body:true kf [`Exits]
  in
  Globals.Functions.iter generate_spec;
  Format.printf "Starting visit@.";
  let prj = File.create_project_from_visitor "pcanalyzer"
      (fun prj -> new pathcrawlerVisitor prj)
  in
  Format.printf "End visit@.";
  Project.set_current prj;
;;

let () = Boot.Main.extend startup
