let check ty =
  let vis = object
    inherit Cil.nopCilVisitor
    method! vattr (a,_) =
      if a = "const" then begin
        let error =
          Format.asprintf "Type %a was not cleaned correctly@."
            Cil_printer.pp_typ ty
        in
        failwith error
      end else Cil.SkipChildren
  end
  in
  let ty' = Ast_types.remove_qualifiers_deep ty in
  ignore (Cil.visitCilType vis ty');
  Format.printf "@[<v>%a@;%a@;@;@]" Cil_printer.pp_typ ty Cil_printer.pp_typ ty'

let test ty = check ty

let tbase = Cil_builder.Type.(const int)

let tptr = Cil_builder.Type.ptr tbase

let tarr = Cil_builder.Type.array tbase

let tptr_arr = Cil_builder.Type.ptr tarr

let tarr_ptr = Cil_builder.Type.array tptr

let tptr_ptr = Cil_builder.Type.(ptr (const tptr))

let tfun = Cil_builder.Type.(proto tptr_ptr ArgList.["x",tptr_arr,[]])

let tfun2 = Cil_builder.Type.(proto tptr ArgList.[("x", tptr, []); ("y", tptr_ptr, [])])

let () =
  List.iter test
    [ Cil_builder.Type.cil_typ tbase;
      Cil_builder.Type.cil_typ tptr;
      Cil_builder.Type.cil_typ tarr;
      Cil_builder.Type.cil_typ tptr_arr;
      Cil_builder.Type.cil_typ tarr_ptr;
      Cil_builder.Type.cil_typ tptr_ptr;
      Cil_builder.Type.cil_typ tfun;
      Cil_builder.Type.cil_typ tfun2;
    ]
