(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Varinfo = Cil_datatype.Varinfo
module Typeinfo = Cil_datatype.Typeinfo
module Compinfo = Cil_datatype.Compinfo
module Enuminfo = Cil_datatype.Enuminfo

let dkey = Sparecode_params.register_category "globs"

let debug format = Sparecode_params.debug ~dkey ~level:2 format

let used_variables = Varinfo.Hashtbl.create 257
let var_init = Varinfo.Hashtbl.create 257
let used_typeinfo = Typeinfo.Hashtbl.create 257
let used_compinfo = Compinfo.Hashtbl.create 257
let used_enuminfo = Enuminfo.Hashtbl.create 257

let clear_tables () =
  Varinfo.Hashtbl.clear used_variables;
  Varinfo.Hashtbl.clear var_init;
  Typeinfo.Hashtbl.clear used_typeinfo;
  Compinfo.Hashtbl.clear used_compinfo;
  Enuminfo.Hashtbl.clear used_enuminfo

class collect_visitor = object (self)

  inherit Visitor.frama_c_inplace

  method! vtype t = match t.tnode with
    | TNamed ti ->
      if Typeinfo.Hashtbl.mem used_typeinfo ti then SkipChildren
      else begin
        debug "add used typedef %s@." ti.tname;
        Typeinfo.Hashtbl.add used_typeinfo ti ();
        ignore (Cil.visitCilType (self:>Cil.cilVisitor) ti.ttype);
        DoChildren
      end
    | TEnum ei ->
      if Enuminfo.Hashtbl.mem used_enuminfo ei then SkipChildren
      else begin
        debug "add used enum %s@." ei.ename;
        Enuminfo.Hashtbl.add used_enuminfo ei (); DoChildren
      end
    | TComp ci ->
      if Compinfo.Hashtbl.mem used_compinfo ci then SkipChildren
      else begin
        debug "add used comp %s@." ci.cname;
        Compinfo.Hashtbl.add used_compinfo ci ();
        List.iter
          (fun f -> ignore (Cil.visitCilType (self:>Cil.cilVisitor) f.ftype))
          (Option.value ~default:[] ci.cfields);
        DoChildren
      end
    | _ -> DoChildren

  method! vvrbl v =
    if v.vglob && not (Varinfo.Hashtbl.mem used_variables v) then begin
      debug "add used var %s@." v.vname;
      Varinfo.Hashtbl.add used_variables v ();
      ignore (Cil.visitCilType (self:>Cil.cilVisitor) v.vtype);
      try
        let init = Varinfo.Hashtbl.find var_init v in
        ignore (Cil.visitCilInit_or_str (self:>Cil.cilVisitor) v init)
      with Not_found -> ()
    end;
    DoChildren

  method! vglob_aux g = match g with
    | GFun (f, _) ->
      debug "add function %s@." f.svar.vname;
      Varinfo.Hashtbl.add used_variables f.svar ();
      Cil.DoChildren
    | GAnnot _ -> Cil.DoChildren
    | GVar (v, init, _) ->
      let _ =
        match init.init with
        | None -> ()
        | Some init ->
          begin
            Varinfo.Hashtbl.add var_init v init;
            if Varinfo.Hashtbl.mem used_variables v then
              (* already used before its initialization (see bug #758) *)
              ignore (Cil.visitCilInit_or_str (self:>Cil.cilVisitor) v init)
          end
      in Cil.SkipChildren
    | GFunDecl _ -> DoChildren
    | _ -> Cil.SkipChildren

end

class filter_visitor prj = object

  inherit Visitor.generic_frama_c_visitor (Visitor_behavior.copy prj)

  method! vglob_aux g =
    match g with
    | GFun (_f, _loc) (* function definition *)
      -> Cil.DoChildren (* keep everything *)
    | GVar (v, _, _) (* variable definition *)
    | GVarDecl (v, _) | GFunDecl (_, v, _) -> (* variable/function declaration *)
      if Varinfo.Hashtbl.mem used_variables v then DoChildren
      else begin
        debug "remove var %s@." v.vname;
        ChangeTo []
      end
    | GType (ti, _loc) (* typedef *) ->
      if Typeinfo.Hashtbl.mem used_typeinfo ti then DoChildren
      else begin
        debug "remove typedef %s@." ti.tname;
        ChangeTo []
      end
    | GCompTag (ci, _loc) (* struct/union definition *)
    | GCompTagDecl (ci, _loc) (* struct/union declaration *) ->
      if Compinfo.Hashtbl.mem used_compinfo ci then DoChildren
      else begin
        debug "remove comp %s@." ci.cname;
        ChangeTo []
      end
    | GEnumTag (ei, _loc) (* enum definition *)
    | GEnumTagDecl (ei, _loc) (* enum declaration *) ->
      if Enuminfo.Hashtbl.mem used_enuminfo ei then DoChildren
      else begin
        debug "remove enum %s@." ei.ename;
        DoChildren (* ChangeTo [] *)
      end
    | _ -> Cil.DoChildren
end

module Result =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Project.Datatype)
    (struct
      let name = "Sparecode without unused globals"
      let size = 7
      let dependencies = [ Ast.self ] (* delayed, see below *)
    end)

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:Result.self
         [ Pdg.Api.self; Inout.self ])

let rm_unused_decl =
  Result.memo
    (fun new_proj_name ->
       clear_tables ();
       let visitor = new collect_visitor in
       Visitor.visitFramacFileSameGlobals visitor (Ast.get ());
       debug "filtering done@.";
       let visitor = new filter_visitor in
       let new_prj = File.create_project_from_visitor new_proj_name visitor in
       let ctx = Parameter_state.get_selection_context () in
       Project.copy ~selection:ctx new_prj;
       new_prj)
