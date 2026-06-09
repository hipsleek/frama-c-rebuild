open Cil_types

class test prj = object(self)
  inherit Visitor.frama_c_copy prj
  method private create_f () =
    let f = Cil.emptyFunction "f" in
    f.svar.vdefined <- true;
    let x = Cil.makeFormalVar f "x" Cil_const.intType in
    Cil.setReturnType f Cil_const.intType;
    Queue.add (fun () -> Cil.setFormals f [x])
      self#get_filling_actions;
    f.sbody <-
      Cil.mkBlock
        [Cil.mkStmt ~valid_sid:true
           (Return (Some (Cil.evar x),Fileloc.unknown))];
    Queue.add
      (fun () ->
         Globals.Functions.replace_by_definition
           (Cil.empty_funspec()) f Fileloc.unknown)
      self#get_filling_actions
    ;
    [GFunDecl(Cil.empty_funspec(),f.svar,Fileloc.unknown);
     GFun(f,Fileloc.unknown)]

  method! vglob_aux = function
    | GVar (v,i,loc) ->
      let v'=
        Visitor.visitFramacVarDecl (self:>Visitor.frama_c_visitor) v
      in
      let i'=
        match i.init with
        | None -> { init = None }
        | Some i ->
          { init =
              Some (Visitor.visitFramacInit_or_str
                      (self:>Visitor.frama_c_visitor) v' i) }
      in
      let g = GVar(v',i',loc) in
      Cil.ChangeToPost (g::self#create_f(),fun x -> x)
    | _ -> Cil.DoChildren
end

let run () =
  let vis prj = new test prj in
  ignore (File.create_project_from_visitor "test" vis)

let () = Boot.Main.extend run
