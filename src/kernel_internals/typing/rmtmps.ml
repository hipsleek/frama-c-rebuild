(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

let dkey = Kernel.dkey_rmtmps

open Fun.Operators
open Cil_types

(* Reachability of used data is stored in a table mapping [info] to [bool].
   Note that due to mutability, we need to use our own Hashtbl module which
   uses [Cil_datatype] equality functions. *)
type info =
  | Type of typeinfo
  | Enum of enuminfo
  | Comp of compinfo
  | Var of varinfo

module InfoHashtbl = Hashtbl.Make(struct
    type t = info
    let equal i1 i2 = match i1, i2 with
      | Type t1, Type t2 -> Cil_datatype.Typeinfo.equal t1 t2
      | Enum e1, Enum e2 -> Cil_datatype.Enuminfo.equal e1 e2
      | Comp c1, Comp c2 -> Cil_datatype.Compinfo.equal c1 c2
      | Var v1, Var v2 -> Cil_datatype.Varinfo.equal v1 v2
      | _, _ -> false
    let hash = function
      | Type t -> Cil_datatype.Typeinfo.hash t
      | Enum e -> Cil_datatype.Enuminfo.hash e
      | Comp c -> Cil_datatype.Compinfo.hash c
      | Var v -> Cil_datatype.Varinfo.hash v
  end)

let keepUnused = ref false

let is_reachable t r = try InfoHashtbl.find t r with Not_found -> false

let pp_info fmt = function
  | Type ti -> Format.fprintf fmt "%s" ti.tname
  | Enum ei -> Format.fprintf fmt "%s" ei.ename
  | Comp ci -> Format.fprintf fmt "%s" ci.cname
  | Var vi -> Format.fprintf fmt "%s" vi.vname


(***********************************************************************
 *
 *  Scanning and categorization of pragmas
 *
*)


(* collections of names of things to keep *)
type collection = (string, unit) Hashtbl.t
type keepers = {
  typedefs : collection;
  enums : collection;
  structs : collection;
  unions : collection;
  defines : collection;
}


(* rapid transfer of control when we find a malformed pragma *)
exception Bad_pragma

(* CIL and CCured define several pragmas which prevent removal of
 * various global varinfos.  Here we scan for those pragmas and build
 * up collections of the corresponding varinfos' names.
*)

let categorizePragmas ast =

  (* names of things which should be retained *)
  let keepers = {
    typedefs = Hashtbl.create 1;
    enums = Hashtbl.create 1;
    structs = Hashtbl.create 1;
    unions = Hashtbl.create 1;
    defines = Hashtbl.create 1
  } in

  (* populate these name collections in light of each pragma *)
  let considerPragma =

    let badPragma location pragma =
      Kernel.warning ~source:location "Invalid argument to pragma %s" pragma
    in

    function
    | GPragma (("cilnoremove" as directive, args), (location,_)) ->
      (* a very flexible pragma: can retain typedefs, enums,
       * structs, unions, or globals (functions or variables) *)
      begin
        let processArg arg =
          try
            match arg with
            | AStr specifier ->
              (* isolate and categorize one varinfo name *)
              let collection, name =
                (* Two words denotes a typedef, enum, struct, or
                 * union, as in "type foo" or "enum bar".  A
                 * single word denotes a global function or
                 * variable. *)
                let whitespace = Str.regexp "[ \t]+" in
                let words = Str.split whitespace specifier in
                match words with
                | ["type"; name] ->
                  keepers.typedefs, name
                | ["enum"; name] ->
                  keepers.enums, name
                | ["struct"; name] ->
                  keepers.structs, name
                | ["union"; name] ->
                  keepers.unions, name
                | [name] ->
                  keepers.defines, name
                | _ ->
                  raise Bad_pragma
              in
              Hashtbl.add collection name ()
            | _ ->
              raise Bad_pragma
          with Bad_pragma ->
            badPragma location directive
        in
        List.iter processArg args
      end
    | GFunDecl (_,v, _) -> begin
        (* Look for alias attributes, e.g. Linux modules *)
        match Ast_attributes.filter "alias" v.vattr with
        | [] -> ()  (* ordinary prototype. *)
        | [ ("alias", [AStr othername]) ] ->
          Hashtbl.add keepers.defines othername ()
        | _ ->
          Kernel.fatal ~current:true
            "Bad alias attribute at %a"
            Fileloc.pretty (Current_loc.get ())
      end
    | _ ->
      ()
  in
  Cil.iterGlobals ast considerPragma;
  keepers



(***********************************************************************
 *
 *  Root collection from pragmas
 *
*)


let isPragmaRoot keepers = function
  | GType ({tname = name}, _) ->
    Hashtbl.mem keepers.typedefs name
  | GEnumTag ({ename = name}, _)
  | GEnumTagDecl ({ename = name}, _) ->
    Hashtbl.mem keepers.enums name
  | GCompTag ({cname = name; cstruct = structure}, _)
  | GCompTagDecl ({cname = name; cstruct = structure}, _) ->
    let collection = if structure then keepers.structs else keepers.unions in
    Hashtbl.mem collection name
  | GVar ({vname = name; vattr = attrs}, _, _)
  | GVarDecl ({vname = name; vattr = attrs}, _)
  | GFunDecl (_,{vname = name; vattr = attrs}, _)
  | GFun ({svar = {vname = name; vattr = attrs}}, _) ->
    Hashtbl.mem keepers.defines name ||
    Ast_attributes.contains "used" attrs
  | _ ->
    false



(***********************************************************************
 *
 *  Common root collecting utilities
 *
*)
let hasExportingAttribute funvar =
  let isExportingAttribute = function
    | ("constructor", []) -> true
    | ("destructor", []) -> true
    | _ -> false
  in
  List.exists isExportingAttribute funvar.vattr

(***********************************************************************
 *
 *  Root collection from external linkage
 *
*)


(* Exported roots are those global varinfos which are visible to the
 * linker and dynamic loader.  For variables, this consists of
 * anything that is not "static".  For functions, this consists of:
 * - functions bearing a "constructor" or "destructor" attribute
 * - functions declared extern but not inline
 * - functions declared neither inline nor static
 * - the function named "main"
*)

let isExportedRoot global =
  let name, result, reason = match global with
    | GVar ({vstorage = Static} as v, _, _) when
        Ast_attributes.contains "FC_BUILTIN" v.vattr ->
      v.vname, true, "FC_BUILTIN attribute"
    | GVar ({vstorage = Static; vname}, _, _) -> vname, false, "static variable"
    | GVar (v,_,_) ->
      v.vname, true, "non-static variable"
    | GFun ({svar = v}, _) -> begin
        if hasExportingAttribute v then
          v.vname,true, "constructor or destructor function"
        else if v.vinline && v.vstorage != Extern then
          v.vname, false, "inline function"
        else if v.vstorage = Static then
          v.vname, false, "static function"
        else
          v.vname, true, "other function"
      end
    | GFunDecl(_,v,_) when Ast_attributes.contains "alias" v.vattr ->
      v.vname, true, "has GCC alias attribute"
    | GFunDecl(_,v,_) | GVarDecl(v,_) when
        Ast_attributes.contains "FC_BUILTIN" v.vattr ->
      v.vname, true, "has FC_BUILTIN attribute"
    | GAnnot _ -> "", true, "global annotation"
    | GType (t, _) when
        Ast_attributes.contains "FC_BUILTIN" t.ttype.tattr ->
      t.tname, true, "has FC_BUILTIN attribute"
    | GCompTag (c,_) | GCompTagDecl (c,_) when
        Ast_attributes.contains "FC_BUILTIN" c.cattr ->
      c.cname, true, "has FC_BUILTIN attribute"
    | GEnumTag (e, _) | GEnumTagDecl (e,_) when
        Ast_attributes.contains "FC_BUILTIN" e.eattr ->
      e.ename, true, "has FC_BUILTIN attribute"
    | _ ->
      (Format.asprintf "%a" Cil_types.pp_global global), false,
      "neither fundef nor vardef nor annotation"
  in
  Kernel.debug
    ~dkey "isExportedRoot %s -> %B, %s"  name result reason;
  result



(***********************************************************************
 *
 *  Root collection for complete programs
 *
*)


(* Exported roots are "main()" and functions bearing a "constructor"
 * or "destructor" attribute.  These are the only things which must be
 * retained in a complete program.
*)

let isCompleteProgramRoot global =
  let result = match global with
    | GFun ({svar = {vname = "main"; vstorage = vstorage}}, _) ->
      vstorage <> Static
    | GFun (fundec, _)
      when hasExportingAttribute fundec.svar ->
      true
    | _ ->
      false
  in
  (*  trace (dprintf "complete program root -> %b for %a@!" result d_shortglobal global);*)
  result


(***********************************************************************
 *
 *  Transitive reachability closure from roots
 *
*)


(* This visitor recursively marks all reachable types and variables as used. *)
class markReachableVisitor
    (globalMap: (string, Cil_types.global) Hashtbl.t)
    (currentFunc: Cil_types.fundec option ref)
    (reachable_tbl: bool InfoHashtbl.t)
  = object (self)
    inherit Cil.nopCilVisitor

    method! vglob = function
      | GType (typeinfo, _) ->
        Kernel.debug ~dkey "marking reachable: type %s" typeinfo.tname;
        InfoHashtbl.replace reachable_tbl (Type typeinfo) true;
        DoChildren
      | GCompTag (compinfo, _)
      | GCompTagDecl (compinfo, _) ->
        Kernel.debug ~dkey "marking reachable: comp decl %s" compinfo.cname;
        InfoHashtbl.replace reachable_tbl (Comp compinfo) true;
        DoChildren
      | GEnumTag (enuminfo, _)
      | GEnumTagDecl (enuminfo, _) ->
        Kernel.debug ~dkey "marking reachable: enum decl %s" enuminfo.ename;
        InfoHashtbl.replace reachable_tbl (Enum enuminfo) true;
        DoChildren
      | GVar (varinfo, _, _)
      | GVarDecl (varinfo, _)
      | GFunDecl (_,varinfo, _)
      | GFun ({svar = varinfo}, _) ->
        if not (Ast_attributes.contains "FC_BUILTIN" varinfo.vattr) then
          begin
            Kernel.debug ~dkey "marking reachable: function %s" varinfo.vname;
            InfoHashtbl.replace reachable_tbl (Var varinfo) true;
          end;
        DoChildren
      | GAnnot _ -> DoChildren
      | _ ->
        SkipChildren

    method! vstmt s =
      match s.skind with
      | TryCatch(_,c,_) ->
        List.iter
          (fun (decl,_) ->
             match decl with
             | Catch_exn(v,l) ->
               (* treat all variables declared in exn clause as used. *)
               ignore (self#vvrbl v);
               List.iter (fun (v,_) -> ignore (self#vvrbl v)) l
             | Catch_all -> ())
          c;
        DoChildren
      | _ -> DoChildren

    method! vinst = function
      | Asm (_, tmpls, _, _) when Machine.msvcMode () ->
        (* If we have inline assembly on MSVC, we cannot tell which locals
         * are referenced. Keep them all *)
        (match !currentFunc with
           Some fd ->
           List.iter (fun v ->
               let vre = Str.regexp_string (Str.quote v.vname) in
               if List.exists (fun tmp ->
                   try ignore (Str.search_forward vre tmp 0); true
                   with Not_found -> false)
                   tmpls
               then
                 InfoHashtbl.replace reachable_tbl (Var v) true
             ) fd.slocals
         | _ -> assert false);
        DoChildren
      | _ -> DoChildren

    method! vvrbl v =
      if not (is_reachable reachable_tbl (Var v)) then
        begin
          let name = v.vname in
          if v.vglob then
            Kernel.debug ~dkey "marking transitive use: global %s (%d)" name v.vid
          else
            Kernel.debug ~dkey "marking transitive use: local %s (%d)" name v.vid;

          (* If this is a global, we need to keep everything used in its
           * definition and declarations. *)
          InfoHashtbl.replace reachable_tbl (Var v) true;
          if v.vglob then
            begin
              Kernel.debug ~dkey "descending: global %s (%d)" name v.vid;
              let descend global =
                ignore (Cil.visitCilGlobal (self :> Cil.cilVisitor) global)
              in
              let globals = Hashtbl.find_all globalMap name in
              List.iter descend globals
            end
        end;
      SkipChildren

    method private mark_enum e =
      if not (is_reachable reachable_tbl (Enum e)) then
        begin
          Kernel.debug ~dkey "marking transitive use: enum %s" e.ename;
          InfoHashtbl.replace reachable_tbl (Enum e) true;
          self#visitAttrs e.eattr;
          (* Must visit the value attributed to the enum constants *)
          ignore (Cil.visitCilEnumInfo (self:>Cil.cilVisitor) e);
        end
      else
        Kernel.debug ~dkey "not marking transitive use: enum %s" e.ename;

    method! vexpr e =
      match e.enode with
        Const (CEnum {eihost = ei}) -> self#mark_enum ei; DoChildren
      | _ -> DoChildren

    method! vterm_node t =
      match t with
        TConst (LEnum {eihost = ei}) -> self#mark_enum ei; DoChildren
      | _ -> DoChildren

    method private visitAttrs attrs =
      ignore (Cil.visitCilAttributes (self :> Cil.cilVisitor) attrs)

    method! vtype typ =
      (match typ.tnode with
       | TEnum e ->
         self#visitAttrs typ.tattr;
         self#mark_enum e

       | TComp c ->
         let old = is_reachable reachable_tbl (Comp c) in
         if not old then
           begin
             Kernel.debug ~dkey "marking transitive use: compound %s"
               c.cname;
             InfoHashtbl.replace reachable_tbl (Comp c) true;

             (* to recurse, we must ask explicitly *)
             let recurse f = ignore (self#vtype f.ftype) in
             List.iter recurse (Option.value ~default:[] c.cfields);
             self#visitAttrs typ.tattr;
             self#visitAttrs c.cattr
           end;

       | TNamed ti ->
         let old = (is_reachable reachable_tbl (Type ti)) in
         if not old then
           begin
             Kernel.debug ~dkey "marking transitive use: typedef %s"
               ti.tname;
             InfoHashtbl.replace reachable_tbl (Type ti) true;
             (* recurse deeper into the type referred-to by the typedef *)
             (* to recurse, we must ask explicitly *)
             ignore (self#vtype ti.ttype);
             self#visitAttrs typ.tattr
           end;

       | TVoid | TInt _ | TFloat _ | TBuiltin_va_list ->
         self#visitAttrs typ.tattr
       | TPtr ty -> ignore (self#vtype ty); self#visitAttrs typ.tattr
       | TArray (ty, sz) ->
         ignore (self#vtype ty); self#visitAttrs typ.tattr;
         Option.iter (ignore $ (Cil.visitCilExpr (self:>Cil.cilVisitor))) sz
       | TFun (ty, args, _) ->
         ignore (self#vtype ty);
         Option.iter (List.iter (fun (_,ty,_) -> ignore (self#vtype ty))) args;
         self#visitAttrs typ.tattr
      );
      SkipChildren

    method! vlogic_var_decl lv =
      Kernel.debug ~dkey "markReachable: found LOGIC VAR DECL for: %s (%d)\n" lv.lv_name lv.lv_id;
      DoChildren

    method! vlogic_var_use lv =
      Kernel.debug ~dkey "markReachable: found LOGIC VAR USE for: %s (%d)\n" lv.lv_name lv.lv_id;
      match lv.lv_origin with
      | None -> SkipChildren
      | Some v ->
        if not (is_reachable reachable_tbl (Var v)) then
          begin
            let name = v.vname in
            if v.vglob then
              Kernel.debug ~dkey "marking transitive use for logic var: global %s (%d)" name v.vid
            else
              Kernel.debug ~dkey "marking transitive use for logic var: local %s (%d)" name v.vid;

            (* If this is a global, we need to keep everything used in its
             * definition and declarations. *)
            InfoHashtbl.replace reachable_tbl (Var v) true;
            if v.vglob then
              begin
                Kernel.debug ~dkey "descending: global %s (%d)" name v.vid;
                let descend global =
                  ignore (Cil.visitCilGlobal (self :> Cil.cilVisitor) global)
                in
                let globals = Hashtbl.find_all globalMap name in
                List.iter descend globals
              end
          end;
        SkipChildren
  end


let markReachable isRoot ast reachable_tbl =
  (* build a mapping from global names back to their definitions &
   * declarations *)
  let globalMap = Hashtbl.create 137 in
  let considerGlobal global =
    match global with
    | GFun ({svar = info}, _)
    | GVar (info, _, _)
    | GFunDecl (_,info, _)
    | GVarDecl (info, _) ->
      Hashtbl.add globalMap info.vname global
    | _ ->
      ()
  in
  Cil.iterGlobals ast considerGlobal;

  let currentFunc = ref None in

  (* mark everything reachable from the global roots *)
  let visitor = new markReachableVisitor globalMap currentFunc reachable_tbl in
  let visitIfRoot global =
    if isRoot global then
      begin
        (* trace (dprintf "traversing root global: %a\n" d_shortglobal global);*)
        (match global with
           GFun(fd, _) -> currentFunc := Some fd
         | _ -> currentFunc := None);
        ignore (Cil.visitCilGlobal visitor global)
      end
    else
      (*      trace (dprintf "skipping non-root global: %a\n" d_shortglobal global)*)
      ()
  in
  Cil.iterGlobals ast visitIfRoot

(**********************************************************************
 *
 * Marking of referenced infos
 *
 **********************************************************************)

let global_type_and_name = function
  | GType (t, _) -> "type " ^ t.tname
  | GCompTag (c,_) -> "comp " ^ c.cname
  | GCompTagDecl (c,_) -> "comp decl " ^ c.cname
  | GEnumTag (e, _) -> "enum " ^ e.ename
  | GEnumTagDecl (e,_) -> "enum decl " ^ e.ename
  | GVarDecl(v,_) -> "var decl " ^ v.vname
  | GFunDecl(_,v,_) -> "fun decl " ^ v.vname
  | GVar (v, _, _) -> "var " ^ v.vname
  | GFun ({svar = v}, _) -> "fun " ^ v.vname
  | GAsm _ -> "<asm>"
  | GPragma _ -> "<pragma>"
  | GText _ -> "<text>"
  | GAnnot _ -> "<annot>"

class markReferencedVisitor = object (self)
  inherit Cil.nopCilVisitor

  val dkey = Kernel.dkey_referenced

  val inside_typ : typ Stack.t = Stack.create ()

  method private reference varinfo loc =
    if not (Ast_attributes.contains "FC_BUILTIN" varinfo.vattr) then begin
      Kernel.debug ~dkey "referenced: var/fun %s@." varinfo.vname;
      Kernel.debug ~source:(fst loc) ~dkey "referenced: fun %s" varinfo.vname;
      varinfo.vreferenced <- true;
    end

  method! vglob = function
    | GType (typeinfo, loc) ->
      Kernel.debug ~source:(fst loc) ~dkey "referenced: type %s" typeinfo.tname;
      typeinfo.treferenced <- true;
      DoChildren
    | GCompTag (compinfo, loc)
    | GCompTagDecl (compinfo, loc) ->
      Kernel.debug ~source:(fst loc) ~dkey "referenced: comp %s" compinfo.cname;
      compinfo.creferenced <- true;
      DoChildren
    | GEnumTag (enuminfo, loc)
    | GEnumTagDecl (enuminfo, loc) ->
      Kernel.debug ~source:(fst loc) ~dkey "referenced: enum %s" enuminfo.ename;
      enuminfo.ereferenced <- true;
      DoChildren
    | GVar (varinfo, _, loc) | GFun ({svar = varinfo}, loc) ->
      self#reference varinfo loc;
      DoChildren
    | GVarDecl (varinfo, loc)
    | GFunDecl (_,varinfo, loc) ->
      self#reference varinfo loc;
      SkipChildren
    | _ -> SkipChildren

  method! vtype t =
    match t.tnode with
    | TNamed ti ->
      if not (Stack.is_empty inside_typ) then begin
        Kernel.debug ~current:true ~dkey "referenced: type %s" ti.tname;
        ti.treferenced <- true;
      end;
      DoChildren
    | TComp ci ->
      if not (Stack.is_empty inside_typ) then begin
        Kernel.debug ~current:true ~dkey "referenced: comp %s" ci.cname;
        ci.creferenced <- true;
      end;
      DoChildren
    | TEnum ei ->
      if not (Stack.is_empty inside_typ) then begin
        Kernel.debug ~current:true ~dkey "referenced: enum %s" ei.ename;
        ei.ereferenced <- true;
      end;
      DoChildren
    | TVoid
    | TInt _
    | TFloat _
    | TPtr _
    | TArray _
    | TFun _
    | TBuiltin_va_list -> DoChildren

  method! vexpr e =
    match e.enode with
    | SizeOf t | AlignOf (t, _) | UnOp (_, _, t) | BinOp (_, _, _, t) ->
      Stack.push t inside_typ;
      DoChildrenPost (fun e -> ignore (Stack.pop inside_typ); e)
    | _ -> DoChildren

  method! vvrbl v =
    Kernel.debug ~current:true ~dkey "referenced: var %s" v.vname;
    v.vreferenced <- true;
    SkipChildren

  method! vspec _ = Cil.SkipChildren
  method! vcode_annot _ = Cil.SkipChildren
end

let markReferenced ast =
  Kernel.debug ~dkey "starting markReferenced (AST has %d globals)"
    (List.length ast.globals);
  Cil.visitCilFileSameGlobals (new markReferencedVisitor) ast;
  Kernel.debug ~dkey "finished markReferenced"

(**********************************************************************
 *
 * Marking and removing of unused labels
 *
 **********************************************************************)

(* We prefer a label when it does not require renaming *)
let prefer ~new_label ~old_label =
  old_label = "" ||
  match Logic_typing.builtin_label old_label with
  | None -> false (* the old label already has a good name *)
  | Some _ ->
    match Logic_typing.builtin_label new_label with
    | None -> true (* the old label is a builtin name, the new one is better *)
    | Some _ -> false (* let us keep the old name *)

(* if we're forced to keep a C label whose name match a builtin logic label,
   we'll rename it to ensure no confusion can arise.
   Since the original name is supposed to be unique in the function, and
   users are not supposed to use symbols starting with an underscore, the chosen
   name ought to be unique.
*)
let rename_c_label lab = "__fc_user_label_" ^ lab

(* We keep only one label, preferably one that was not introduced by CIL.
 * Scan a list of labels and return the data for the label that should be
 * kept, and the remaining filtered list of labels *)
let labelsToKeep is_removable ll =
  let rec loop sofar = function
      [] -> sofar, []
    | l :: rest ->
      let newlabel, keepl =
        match l with
        | Case _ | Default _ -> sofar, true
        | Label (ln, loc, isuser) as lab ->
          let lab =
            match Logic_typing.builtin_label ln with
            | None -> lab
            | Some _ -> Label(rename_c_label ln, loc, isuser)
          in
          begin
            match is_removable lab, sofar with
            | true, ("", _) ->
              (* keep this one only if we have no label so far *)
              (ln, lab), false
            | true, _ ->
              sofar, false
            | false, (_, lab') when is_removable lab' ->
              (* this is an original label; prefer it to temporary or
               * missing labels *)
              (ln, lab), false
            | false, (ln', _) when prefer ~new_label:ln ~old_label:ln' ->
              (ln, lab), false
            | false, _ ->
              sofar, false
          end
      in
      let newlabel', rest' = loop newlabel rest in
      newlabel', (if keepl then l :: rest' else rest')
  in
  loop ("", Label("", Fileloc.unknown, false)) ll

class markUsedLabels is_removable (labelMap: (string, unit) Hashtbl.t) =
  let keep_label dest =
    let (ln, _), _ = labelsToKeep is_removable !dest.labels in
    if ln = "" then
      Kernel.fatal "Statement has no label:@\n%a" Cil_printer.pp_stmt !dest ;
    (* Mark it as used *)
    Hashtbl.replace labelMap ln ()
  in
  let keep_label_logic = function
    | FormalLabel _ | BuiltinLabel _ -> ()
    | StmtLabel dest -> keep_label dest
  in
  object
    inherit Cil.nopCilVisitor

    method! vstmt (s: stmt) =
      match s.skind with
        Goto (dest, _) -> keep_label dest; DoChildren
      | _ -> DoChildren

    method! vterm_node t =
      begin
        match t with
        | Tat (_,lab) -> keep_label_logic lab
        | Tapp(_,labs,_) ->
          List.iter keep_label_logic labs
        | _ -> ()
      end;
      DoChildren

    method! vpredicate_node t =
      begin
        match t with
        | Pat (_,lab) -> keep_label_logic lab
        | Papp(_,labs,_) ->
          List.iter keep_label_logic labs
        | _ -> ()
      end;
      DoChildren

    (* No need to go into expressions or types *)
    method! vexpr _ = Cil.SkipChildren
    method! vtype _ = Cil.SkipChildren
  end

class removeUnusedLabels is_removable (labelMap: (string, unit) Hashtbl.t) = object
  inherit Cil.nopCilVisitor

  method! vstmt (s: stmt) =
    let (ln, lab), lrest = labelsToKeep is_removable s.labels in
    s.labels <-
      (if ln <> "" &&
          (Hashtbl.mem labelMap ln || not (is_removable lab))
          (* keep user-provided labels *)
       then (* We had labels *)
         (lab :: lrest)
       else
         lrest);
    DoChildren

  (* No need to go into expressions or instructions *)
  method! vexpr _ = Cil.SkipChildren
  method! vinst _ = Cil.SkipChildren
  method! vtype _ = Cil.SkipChildren
end

(***********************************************************************
 *
 *  Removal of unused varinfos
 *
*)

let label_removable = function
    Label (_,_,user) -> not user
  | Case _ | Default _ -> false

let remove_unused_labels ?(is_removable=label_removable) func =
  (* We also want to remove unused labels. We do it all here, including
   * marking the used labels *)
  let usedLabels:(string, unit) Hashtbl.t = Hashtbl.create 13 in
  ignore
    (Cil.visitCilBlock (new markUsedLabels is_removable usedLabels) func.sbody);
  (* And now we scan again and we remove them *)
  ignore
    (Cil.visitCilBlock (new removeUnusedLabels is_removable usedLabels) func.sbody)

let removeUnmarked isRoot ast reachable_tbl =
  let filterGlobal global =
    match global with
    (* unused global types, variables, and functions are simply removed *)
    | GType (t, _) ->
      is_reachable reachable_tbl (Type t) ||
      Ast_attributes.contains "FC_BUILTIN" t.ttype.tattr
      || isRoot global
    | GCompTag (c,_) | GCompTagDecl (c,_) ->
      is_reachable reachable_tbl (Comp c) ||
      Ast_attributes.contains "FC_BUILTIN" c.cattr || isRoot global
    | GEnumTag (e, _) | GEnumTagDecl (e,_) ->
      is_reachable reachable_tbl (Enum e) ||
      Ast_attributes.contains "FC_BUILTIN" e.eattr || isRoot global
    | GVar (v, _, _) ->
      is_reachable reachable_tbl (Var v) ||
      Ast_attributes.contains "FC_BUILTIN" v.vattr || isRoot global
    | GVarDecl (v, _)
    | GFunDecl (_,v, _)->
      is_reachable reachable_tbl (Var v) ||
      Ast_attributes.contains "FC_BUILTIN" v.vattr ||
      (if isRoot global then true else (Cil.removeFormalsDecl v; false))
    (* keep FC_BUILTIN, as some plug-ins might want to use them later
       for semi-legitimate reasons. *)
    | GFun (func, _) ->
      (* if some generated temp variables are useless, remove them.
         Keep variables that were already present in the code.
      *)
      let filterLocal local =
        if (local.vtemp || local.vstorage = Static) &&
           not (is_reachable reachable_tbl (Var local)) then
          begin
            Kernel.debug ~dkey "removing local: %s" local.vname;
            false
          end else true
      in
      func.slocals <- List.filter filterLocal func.slocals;
      let remove_blocals = object
        inherit Cil.nopCilVisitor
        method! vblock b =
          b.blocals <- List.filter filterLocal b.blocals;
          b.bstatics <- List.filter filterLocal b.bstatics;
          DoChildren
      end
      in
      ((is_reachable reachable_tbl (Var func.svar))
       || Ast_attributes.contains "FC_BUILTIN" func.svar.vattr
       || isRoot global) &&
      (ignore (Cil.visitCilBlock remove_blocals func.sbody);
       remove_unused_labels func;
       true)

    (* all other globals are retained *)
    | _ -> true
  in
  let keptGlobals, removedGlobals = List.partition filterGlobal ast.globals in
  ast.globals <- keptGlobals;
  if Kernel.is_debug_key_enabled dkey then begin
    List.iter (fun rg ->
        Kernel.debug ~dkey "removing global: %s" (global_type_and_name rg)
      ) removedGlobals;
    List.iter (fun rg ->
        begin
          match rg with
          | GFunDecl (_s, vi, _) ->
            begin
              try
                let kf = Globals.Functions.get vi in
                Kernel.debug ~dkey "GFunDecl: %a@." Kernel_function.pretty kf
              with Not_found ->
                Kernel.debug ~dkey
                  "GFunDecl: %a (no associated kernel function)@."
                  Cil_printer.pp_varinfo vi;
            end
          | _ -> ()
        end;
        Kernel.debug ~dkey "kept global %s (%a)" (global_type_and_name rg) Cil_printer.pp_global rg
      ) keptGlobals;
  end


(***********************************************************************
 *
 *  Exported interface
 *
*)

let removeUnused ?(isRoot=isExportedRoot) ast =
  if not !keepUnused then
    begin
      Kernel.debug ~dkey "Removing unused" ;

      (* digest any pragmas that would create additional roots *)
      let keepers = categorizePragmas ast in

      let reachable_tbl = InfoHashtbl.create 43 in
      (* build up the root set *)
      let isRoot global =
        isPragmaRoot keepers global ||
        isRoot global
      in

      (* mark everything reachable from the global roots *)
      markReachable isRoot ast reachable_tbl;

      Kernel.debug ~dkey "reachable_tbl: %t"
        (fun fmt ->
           let elements =
             InfoHashtbl.fold (fun k v acc ->
                 Format.asprintf "%a:%B" pp_info k v :: acc)
               reachable_tbl []
           in
           Format.fprintf fmt "%a"
             (Pretty_utils.pp_list ~sep:"@\n" Format.pp_print_string) elements);

      markReferenced ast;

      (* take out the trash *)
      removeUnmarked isRoot ast reachable_tbl
    end
