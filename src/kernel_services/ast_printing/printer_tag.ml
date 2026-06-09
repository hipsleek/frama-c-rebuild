(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

(* -------------------------------------------------------------------------- *)
(* --- Scope API                                                          --- *)
(* -------------------------------------------------------------------------- *)

type declaration =
  | SEnum of enuminfo
  | SComp of compinfo
  | SType of typeinfo
  | SGlobal of varinfo
  | SFunction of kernel_function
  | SGAnnot of global_annotation

let pp_declaration fmt = function
  | SEnum e ->
    Format.fprintf fmt "enum %s" e.ename
  | SComp { cstruct = true ; cname } ->
    Format.fprintf fmt "struct %s" cname
  | SComp { cstruct = false ; cname } ->
    Format.fprintf fmt "union %s" cname
  | SType t ->
    Format.fprintf fmt "type %s" t.tname
  | SGlobal v ->
    Format.fprintf fmt "global %s" v.vname
  | SFunction kf ->
    Format.fprintf fmt "function %s" @@ Kernel_function.get_name kf
  | SGAnnot (Dfun_or_pred (li, _)) when li.l_type = None ->
    Format.fprintf fmt "predicate %s" li.l_var_info.lv_name
  | SGAnnot (Dfun_or_pred (li, _)) ->
    Format.fprintf fmt "logic %s" li.l_var_info.lv_name
  | SGAnnot (Daxiomatic (name, _, _, _)) ->
    Format.fprintf fmt "axiomatic %s" name
  | SGAnnot (Dmodule (name, _, _, _, _)) ->
    Format.fprintf fmt "module %s" name
  | SGAnnot (Dlemma(name, _, _, _, _, _)) ->
    Format.fprintf fmt "lemma %s" name
  | SGAnnot (Dinvariant (li, _)) ->
    Format.fprintf fmt "invariant %s" li.l_var_info.lv_name
  | SGAnnot (Dtype_annot (li, _)) ->
    Format.fprintf fmt "type invariant %s" li.l_var_info.lv_name
  | SGAnnot (Dtype(lti, _)) ->
    Format.fprintf fmt "logic type %s" lti.lt_name
  | SGAnnot (Dmodel_annot(mi, _)) ->
    Format.fprintf fmt "model %s" mi.mi_name
  | SGAnnot(Dvolatile (ts, _, _, _, _)) ->
    Format.fprintf fmt "volatile %a"
      (Pretty_utils.pp_list ~sep:"," Identified_term.pretty) ts
  | SGAnnot(Dextended(ext, _, _)) ->
    Format.fprintf fmt "%a" Cil_printer.pp_short_extended ext


module Declaration =
  Datatype.Make_with_collections
    (struct
      include Datatype.Undefined
      type t = declaration

      let name = "Printer_tag.Declaration"
      let reprs = List.map (fun g -> SGlobal g) Varinfo.reprs
      let mem_project = Datatype.never_any_project

      let hash = function
        | SEnum e -> Hashtbl.hash( 0, Enuminfo.hash e )
        | SComp c -> Hashtbl.hash( 1, Compinfo.hash c )
        | SType t -> Hashtbl.hash( 2, Typeinfo.hash t )
        | SGlobal g -> Hashtbl.hash( 3, Varinfo.hash g )
        | SFunction kf -> Hashtbl.hash( 4, Kernel_function.hash kf )
        | SGAnnot a -> Hashtbl.hash (5, Global_annotation.hash a)

      let equal a b =
        match a,b with
        | SEnum u, SEnum v -> Enuminfo.equal u v
        | SComp u, SComp v -> Compinfo.equal u v
        | SType u, SType v -> Typeinfo.equal u v
        | SGlobal u, SGlobal v -> Varinfo.equal u v
        | SFunction f, SFunction g -> Kernel_function.equal f g
        | SGAnnot f, SGAnnot g -> Global_annotation.equal f g
        | (SEnum _, _) | (SComp _, _) | (SType _, _) | (SGlobal _, _)
        | (SFunction _, _) | (SGAnnot _, _)-> false

      let compare a b =
        match a,b with
        | SEnum u, SEnum v -> Enuminfo.compare u v
        | SEnum _, _ -> (-1)
        | _, SEnum _ -> (+1)
        | SComp u, SComp v -> Compinfo.compare u v
        | SComp _, _ -> (-1)
        | _, SComp _ -> (+1)
        | SType u, SType v -> Typeinfo.compare u v
        | SType _, _ -> (-1)
        | _, SType _ -> (+1)
        | SGlobal u, SGlobal v -> Varinfo.compare u v
        | SGlobal _, _ -> (-1)
        | _, SGlobal _ -> (+1)
        | SFunction f, SFunction g -> Kernel_function.compare f g
        | SFunction _, _ -> (-1)
        | _, SFunction _ -> (+1)
        | SGAnnot f, SGAnnot g -> Global_annotation.compare f g

      let pretty = pp_declaration
    end)

(* -------------------------------------------------------------------------- *)
(* --- Localizable API                                                    --- *)
(* -------------------------------------------------------------------------- *)

type localizable =
  | PStmt of (kernel_function * stmt)
  | PStmtStart of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PExp of (kernel_function option * kinstr * exp)
  | PTermLval of (kernel_function option * kinstr * Property.t * term_lval)
  | PVDecl of (kernel_function option * kinstr * varinfo)
  | PGlobal of global
  | PIP of Property.t
  | PType of typ

let declaration = function
  | GCompTag(comp,loc) -> GCompTagDecl(comp,loc)
  | GEnumTag(enum,loc) -> GEnumTagDecl(enum,loc)
  | GFunDecl(_,vi,loc) | GVar(vi,_,loc) | GFun({ svar=vi },loc)
    -> GVarDecl(vi,loc)
  | GAsm(_,loc) -> GAsm("…",loc)
  | GText s when String.length s > 20 -> GText(String.sub s 0 20 ^ "…")
  | GPragma((a,_),loc) -> GPragma((a,[]),loc)
  | GAnnot _ -> GText "Global annotation"
  | ( GType _ | GCompTagDecl _ | GEnumTagDecl _
    | GVarDecl _ | GText _ ) as g -> g

let loc_of_global_annotation = function
  | Dfun_or_pred (_, loc) | Dvolatile (_, _, _, _, loc)
  | Daxiomatic (_, _, _, loc) | Dmodule (_, _, _, _, loc)
  | Dtype (_, loc) | Dlemma (_, _, _, _, _, loc)
  | Dinvariant (_, loc) | Dtype_annot (_, loc)
  | Dmodel_annot (_, loc) | Dextended (_, _, loc)
    -> loc

let global_of_declaration decl =
  let global_type namespace name =
    try Globals.Types.global namespace name
    with Not_found -> Kernel.fatal "Unknown %a" pp_declaration decl
  in
  match decl with
  | SGlobal vi -> Ast.def_or_last_decl vi
  | SFunction kf -> Kernel_function.get_global kf
  | SType ti -> global_type Logic_typing.Typedef ti.tname
  | SComp ci ->
    let namespace = if ci.cstruct then Logic_typing.Struct else Union in
    global_type namespace ci.cname
  | SEnum ei -> global_type Logic_typing.Enum ei.ename
  | SGAnnot ga -> GAnnot (ga, loc_of_global_annotation ga)

let pp_global_annotation_decl fmt = function
  | Dfun_or_pred (li, _) | Dinvariant (li, _) | Dtype_annot (li, _) ->
    Logic_info.pretty fmt { li with l_body = LBnone }
  | Daxiomatic (name, _, _, _)
  | Dmodule (name, _, _, _, _)
  | Dlemma (name, _, _, _, _, _) ->
    Format.pp_print_string fmt name
  | Dextended (ext, _, _) ->
    Cil_printer.pp_short_extended fmt ext
  | Dvolatile (ts, r, w, _, _) ->
    let pp name fmt = function
      | None -> ()
      | Some vi -> Format.fprintf fmt " %s %a" name Cil_printer.pp_varinfo vi
    in
    Format.fprintf fmt "%a%a%a"
      (Pretty_utils.pp_list ~sep:", " Cil_printer.pp_identified_term) ts
      (pp "reads") r
      (pp "writes") w
  | Dtype (lt, _) ->
    Logic_type_info.pretty fmt lt
  | Dmodel_annot (mi, _) ->
    Model_info.pretty fmt mi

let pp_localizable fmt = function
  | PVDecl (_, _, vi) -> Printer.pp_vdecl fmt vi
  | PLval (_, _, lval) -> Printer.pp_lval fmt lval
  | PExp  (_, _, expr) -> Printer.pp_exp fmt expr
  | PTermLval (_, _, _, lv) -> Printer.pp_term_lval fmt lv
  | PIP prop -> Description.pp_local fmt prop
  | PGlobal (GAnnot(g,_)) -> pp_global_annotation_decl fmt g
  | PGlobal g -> Printer.pp_global fmt (declaration g)
  | PStmt(_,stmt) | PStmtStart (_, stmt) ->
    Printer.(without_annot pp_stmt) fmt stmt
  | PType t -> Printer.pp_typ fmt t

let pp_ki_loc fmt ki =
  match ki with
  | Kglobal -> (* no location, print 'global' *)
    Format.fprintf fmt "global"
  | Kstmt st -> Filepos.pretty fmt (fst @@ Stmt.loc st)

let pp_debug fmt = function
  | PStmtStart (_, s) ->
    Format.fprintf fmt "LocalizableStart %d (%a)"
      s.sid Fileloc.pretty (Cil_datatype.Stmt.loc s)
  | PStmt (_, s) ->
    Format.fprintf fmt "LocalizableStmt %d (%a)"
      s.sid Fileloc.pretty (Cil_datatype.Stmt.loc s)
  | PLval (_, ki, lv) ->
    Format.fprintf fmt "LocalizableLval %a (%a)"
      Printer.pp_lval lv pp_ki_loc ki
  | PExp (_, ki, lv) ->
    Format.fprintf fmt "LocalizableExp %a (%a)"
      Printer.pp_exp lv pp_ki_loc ki
  | PTermLval (_, ki, _pi, tlv) ->
    Format.fprintf fmt "LocalizableTermLval %a (%a)"
      Printer.pp_term_lval tlv pp_ki_loc ki
  | PVDecl (_, _, vi) ->
    Format.fprintf fmt "LocalizableVDecl %a" Printer.pp_vdecl vi
  | PGlobal g ->
    Format.fprintf fmt "LocalizableGlobal %a" Printer.pp_global g
  | PIP ip ->
    Format.fprintf fmt "LocalizableIP %a" Description.pp_property ip
  | PType typ ->
    Format.fprintf fmt "LocalizableType %a" Printer.pp_typ typ

module Localizable =
  Datatype.Make_with_collections
    (struct
      include Datatype.Undefined
      type t = localizable

      let name = "Printer_tag.Localizable"
      let reprs = List.map (fun g -> PGlobal g) Global.reprs
      let mem_project = Datatype.never_any_project

      let hash = function
        | PStmtStart (_,s) ->
          Hashtbl.hash( 0, Stmt.hash s )
        | PStmt (_,s) ->
          Hashtbl.hash( 1, Stmt.hash s )
        | PLval (_,ki,lv) ->
          Hashtbl.hash( 2, Kinstr.hash ki, Lval.hash lv )
        | PTermLval(_,ki,pi,lv) ->
          Hashtbl.hash( 3, Kinstr.hash ki, Property.hash pi, Term_lval.hash lv)
        | PVDecl(_,_,v) ->
          Hashtbl.hash( 4, Varinfo.hash v )
        | PExp(_,_,e) ->
          Hashtbl.hash( 5, Exp.hash e )
        | PIP ip ->
          Hashtbl.hash( 6, Property.hash ip )
        | PGlobal g ->
          Hashtbl.hash( 7, Global.hash g )
        | PType t ->
          Hashtbl.hash( 8, TypByName.hash t )

      let equal l1 l2 = match l1,l2 with
        | PStmt (_,ki1), PStmt (_,ki2) -> ki1.sid = ki2.sid
        | PStmtStart (_,ki1), PStmtStart (_,ki2) -> ki1.sid = ki2.sid
        | PLval (_,ki1,lv1), PLval (_,ki2,lv2) ->
          Kinstr.equal ki1 ki2 &&
          Lval.equal lv1 lv2
        | PTermLval (_,ki1,pi1,lv1), PTermLval (_,ki2,pi2,lv2) ->
          Kinstr.equal ki1 ki2 &&
          Property.equal pi1 pi2 &&
          Term_lval.equal lv1 lv2
        | PVDecl (_,_,v1), PVDecl (_,_,v2) -> Varinfo.equal v1 v2
        | PExp (_,_,e1), PExp(_,_,e2) -> Exp.equal e1 e2
        | PIP ip1, PIP ip2 -> Property.equal ip1 ip2
        | PGlobal g1, PGlobal g2 -> Global.equal g1 g2
        | PType t1, PType t2 -> TypByName.equal t1 t2
        | (PStmt _ | PStmtStart _ | PLval _ | PExp _ | PTermLval _ | PVDecl _
          | PIP _ | PGlobal _ | PType _), _
          ->  false

      let compare l1 l2 = match l1,l2 with
        | PStmt(_,s1) , PStmt(_,s2) -> Stmt.compare s1 s2
        | PStmt _ , _ -> (-1)
        | _ , PStmt _ -> 1
        | PStmtStart(_,s1) , PStmtStart(_,s2) -> Stmt.compare s1 s2
        | PStmtStart _ , _ -> (-1)
        | _ , PStmtStart _ -> 1
        | PLval (_,k1,lv1), PLval(_,k2,lv2) ->
          let cmp = Kinstr.compare k1 k2 in
          if cmp<>0 then cmp else Lval.compare lv1 lv2
        | PLval _ , _ -> (-1)
        | _ , PLval _ -> 1
        | PTermLval(_,k1,p1,lv1) , PTermLval(_,k2,p2,lv2) ->
          let cmp = Kinstr.compare k1 k2 in
          if cmp<>0 then cmp else
            let cmp = Property.compare p1 p2 in
            if cmp<>0 then cmp else
              Term_lval.compare lv1 lv2
        | PTermLval _ , _ -> (-1)
        | _ , PTermLval _ -> 1
        | PVDecl(_,_,v1) , PVDecl(_,_,v2) -> Varinfo.compare v1 v2
        | PVDecl _ , _ -> (-1)
        | _ , PVDecl _ -> 1
        | PExp(_,_,e1) , PExp(_,_,e2) -> Exp.compare e1 e2
        | PExp _ , _ -> (-1)
        | _ , PExp _ -> 1
        | PIP p1 , PIP p2 -> Property.compare p1 p2
        | PIP _ , _ -> (-1)
        | _ , PIP _ -> 1
        | PType t1, PType t2 -> TypByName.compare t1 t2
        | PType _, _ -> (-1)
        | _, PType _ -> 1
        | PGlobal g1 , PGlobal g2 -> Global.compare g1 g2

      let pretty = pp_localizable

    end)

(* -------------------------------------------------------------------------- *)
(* --- Utility Accessors                                                  --- *)
(* -------------------------------------------------------------------------- *)

let declaration_of_global = function
  | GType(ti, _) -> Some (SType ti)
  | GCompTag(ci, _) | GCompTagDecl (ci, _) -> Some (SComp ci)
  | GEnumTag(ei, _) | GEnumTagDecl (ei, _) -> Some (SEnum ei)
  | GVar(vi, _, _) | GVarDecl(vi, _) -> Some (SGlobal vi)
  | GFun({svar = vi}, _) | GFunDecl(_,vi,_) ->
    Some(SFunction (Globals.Functions.get vi))
  | GAnnot (ga, _) -> Some (SGAnnot ga)
  | GAsm _ | GPragma _ | GText _ -> None

let declaration_of_type t =
  match t.tnode with
  | TVoid | TInt _ | TFloat _ | TPtr _
  | TArray _ | TFun _ | TBuiltin_va_list -> None
  | TNamed ti -> Some (SType ti)
  | TComp  ci -> Some (SComp ci)
  | TEnum  ei -> Some (SEnum ei)

let declaration_of_property ip =
  match Property.get_kf ip with
  | None -> None
  | Some kf -> Some (SFunction kf)

let declaration_of_localizable = function
  | PStmt(kf,_) | PStmtStart(kf,_)
  | PVDecl(Some kf,_,_)
  | PTermLval(Some kf,_,_,_)
  | PLval(Some kf,_,_)
  | PExp(Some kf,_,_)
    -> Some (SFunction kf)
  | PVDecl(None,_,vi) ->
    Some(
      try SFunction(Globals.Functions.get vi)
      with Not_found -> SGlobal vi
    )
  | PGlobal g -> declaration_of_global g
  | PIP p -> declaration_of_property p
  | PTermLval(None,_,_,_)
  | PLval(None,_,_) | PExp(None,_,_)
  | PType _ -> None

let definition_of_type typ =
  let global decl = Some (PGlobal (global_of_declaration decl)) in
  match typ.tnode with
  | TVoid | TInt _ | TFloat _ | TPtr _
  | TArray _ | TFun _ | TBuiltin_va_list -> None
  | TNamed ti -> global (SType ti)
  | TComp ci -> global (SComp ci)
  | TEnum ei -> global (SEnum ei)

let definition_of_localizable = function
  | PLval(kf,ki,(Var vi,NoOffset))
  | PTermLval(kf,ki,_,(TVar { lv_origin = Some vi },TNoOffset)) ->
    if not vi.vsource then
      None (* No definition for variables that do not appear in the AST. *)
    else if vi.vglob then
      let kf = try Some(Globals.Functions.get vi) with Not_found -> None in
      Some (PVDecl(kf,Kglobal,vi))
    else
      Some (PVDecl(kf,ki,vi))
  | PType ty -> definition_of_type ty
  | PGlobal (GType _ | GCompTag _ | GEnumTag _) as loc -> Some loc
  | PStmt _ | PStmtStart _ | PVDecl _
  | PExp _ | PLval _ | PTermLval _ | PGlobal _ | PIP _
    -> None

let name_of_global_annotation = function
  | Dfun_or_pred (li, _)
  | Dinvariant (li, _)
  | Dtype_annot (li, _) ->
    li.l_var_info.lv_name
  | Dvolatile (ts, _, _, _, _) ->
    Format.asprintf "%a"
      (Pretty_utils.pp_list ~sep:", " Identified_term.pretty) ts
  | Daxiomatic (name, _, _, _)
  | Dmodule (name, _, _, _,_)
  | Dlemma (name, _, _, _, _, _) ->
    name
  | Dtype (lti, _) ->
    lti.lt_name
  | Dmodel_annot (mi, _) ->
    mi.mi_name
  | Dextended (ext, _, _) ->
    Format.asprintf "%a" Cil_printer.pp_short_extended ext

let name_of_declaration = function
  | SEnum ei -> ei.ename
  | SComp ci -> ci.cname
  | SType ti -> ti.tname
  | SGlobal vi -> vi.vname
  | SFunction kf -> Kernel_function.get_name kf
  | SGAnnot annot -> name_of_global_annotation annot

let name_of_type t =
  match t.tnode with
  | TVoid | TInt _ | TFloat _ | TPtr _
  | TArray _ | TFun _ | TBuiltin_va_list -> None
  | TNamed ti -> Some ti.tname
  | TComp  ci -> Some ci.cname
  | TEnum  ei -> Some ei.ename

let name_of_global g =
  Option.map name_of_declaration @@ declaration_of_global g

let name_of_localizable = function
  | PVDecl(_,_,vi)
  | PLval(_,_,(Var vi,NoOffset))
  | PTermLval(_,_,_,(TVar { lv_origin = Some vi },TNoOffset)) ->
    Some vi.vname
  | PGlobal g -> name_of_global g
  | PType ty -> name_of_type ty
  | PStmt _ | PStmtStart _
  | PExp _ | PLval _ | PTermLval _ | PIP _
    -> None

let loc_of_declaration = function
  | SGlobal vi -> vi.vdecl
  | SFunction kf -> Kernel_function.get_location kf
  | SGAnnot annot -> loc_of_global_annotation annot
  | decl -> Global.loc (global_of_declaration decl)

let kf_of_localizable = function
  | PLval (kf_opt, _, _)
  | PExp (kf_opt,_,_)
  | PTermLval(kf_opt, _,_,_)
  | PVDecl (kf_opt, _, _) -> kf_opt
  | PStmt (kf, _) | PStmtStart(kf,_) -> Some kf
  | PIP ip -> Property.get_kf ip
  | PGlobal (GFun ({svar = vi}, _) | GFunDecl(_,vi,_)) ->
    Some (Globals.Functions.get vi)
  | PGlobal _ -> None
  | PType _ -> None

let ki_of_localizable = function
  | PLval (_, ki, _)
  | PExp (_, ki, _)
  | PTermLval(_, ki,_,_)
  | PVDecl (_, ki, _) -> ki
  | PStmt (_, st) | PStmtStart(_, st) -> Kstmt st
  | PIP ip -> Property.get_kinstr ip
  | PGlobal _ -> Kglobal
  | PType _ -> Kglobal

let varinfo_of_localizable = function
  | PVDecl (_, _, vi)
  | PLval (_, _, (Var vi, NoOffset))
  | PTermLval (_, _, _, (TVar { lv_origin = Some vi }, TNoOffset))
  | PGlobal (
      GVar (vi, _, _) | GVarDecl (vi, _) |
      GFun ({svar = vi },_) | GFunDecl (_, vi, _)
    ) -> Some vi
  | _ -> None

let loc_of_localizable = function
  | PStmt (_,st) | PStmtStart(_,st)
  | PLval (_,Kstmt st,_) | PExp(_,Kstmt st, _)
  | PTermLval(_,Kstmt st,_,_) ->
    Stmt.loc st
  | PIP ip -> Property.location ip
  | PVDecl (_,_,vi) ->
    if vi.vglob && vi.vsource
    then Global.loc (Ast.def_or_last_decl vi)
    else vi.vdecl
  | PGlobal g -> Global.loc g
  | (PLval _ | PTermLval _ | PExp _) as localize ->
    (match kf_of_localizable localize with
     | None -> Fileloc.unknown
     | Some kf -> Kernel_function.get_location kf)
  | PType _ -> Fileloc.unknown

(* -------------------------------------------------------------------------- *)
(* --- Helper for Globals                                                 --- *)
(* -------------------------------------------------------------------------- *)

let localizable_of_kf kf =
  PVDecl(Some kf,Kglobal,Globals.Functions.get_vi kf)

let localizable_of_stmt stmt =
  PStmtStart (Kernel_function.find_englobing_kf stmt, stmt)

let localizable_of_global g =
  match g with
  | GVarDecl(vi,_) | GVar(vi,_,_) when vi.vglob ->
    PVDecl(None,Kglobal,vi)
  | GFun({ svar = vi },_) | GFunDecl(_,vi,_) ->
    (try PVDecl(Some (Globals.Functions.get vi) , Kglobal, vi)
     with Not_found -> PGlobal g)
  | _ -> PGlobal g

let localizable_of_declaration = function
  | SFunction kf -> localizable_of_kf kf
  | SGlobal vi -> PVDecl(None,Kglobal,vi)
  | SComp _ | SEnum _ | SType _ as decl -> PGlobal (global_of_declaration decl)
  | SGAnnot ga -> PGlobal (GAnnot (ga, loc_of_global_annotation ga))

(* -------------------------------------------------------------------------- *)
(* --- Find localizable at a Filepos.t                            --- *)
(* -------------------------------------------------------------------------- *)

let dkey = Kernel.dkey_pretty_source

module LineToLocalizable =
  Datatype.Hashtbl(Datatype.Int.Hashtbl)(Datatype.Int)
module FileToLines =
  Datatype.Hashtbl(Filepath.Hashtbl)(Filepath)

module MappingLineLocalizable = struct
  module LineToLocalizableAux =
    LineToLocalizable.Make( Datatype.Pair(Fileloc)(Localizable))

  include State_builder.Hashtbl(FileToLines)(LineToLocalizableAux)
      (struct
        let size = 5
        let dependencies = [Ast.self]
        let name = "Pretty_source.line_to_localizable"
      end)
end

class pos_to_localizable =
  object (self)
    inherit Visitor.frama_c_inplace

    (* used to keep track of conditional expressions, to add them to the
       list of relevant localizables *)
    val mutable insideIf = None

    method add_range loc (localizable : localizable) =
      if Fileloc.is_known loc then (
        let p1, p2 = loc in
        if not (Filepath.equal (Filepos.path p1) (Filepos.path p2)) then
          Kernel.debug ~once:true ~dkey
            "Localizable over two files: %a and %a; %a"
            Filepath.pretty (Filepos.path p1)
            Filepath.pretty (Filepos.path p2)
            Localizable.pretty localizable;
        let path = Filepos.path p1 in
        let hpath =
          try MappingLineLocalizable.find path
          with Not_found ->
            let h = LineToLocalizable.create 17 in
            MappingLineLocalizable.add path h;
            h
        in
        for i = Filepos.line p1 to Filepos.line p2 do
          LineToLocalizable.add hpath i (loc, localizable);
        done
      );

    method! vstmt_aux s =
      (* we ignore Block statements, since they tend to overlap existing
         ones which are more precise *)
      let skip = match s.skind with
        | Block _ -> true
        | _ -> false
      in
      if not skip then
        self#add_range (Stmt.loc s) (PStmt (Option.get self#current_kf, s));
      begin
        match s.skind with
        | If (exp, _, _, _) ->
          (* conditional expressions are treated in a special way *)
          insideIf <- Some (Kstmt s);
          ignore (Cil.visitCilExpr (self :> Cil.cilVisitor) exp);
          insideIf <- None
        | _ -> ()
      end;
      Cil.DoChildren

    method! vexpr exp =
      begin
        match insideIf with
        | Some ki ->
          (* expressions inside conditionals have a special treatment *)
          begin
            match exp.enode with
            | Lval lv ->
              (* lvals must be generated differently from other expressions *)
              self#add_range exp.eloc (PLval(self#current_kf, ki, lv))
            | _ ->
              self#add_range exp.eloc (PExp(self#current_kf, ki, exp))
          end
        | None -> ()
      end;
      Cil.DoChildren

    method! vvdec vi =
      if not vi.vglob && not vi.vtemp then
        begin
          match self#current_kf with
          | None -> (* should not happen*) ()
          | Some kf ->
            self#add_range vi.vdecl (PVDecl (Some kf,self#current_kinstr,vi));
        end;
      Cil.DoChildren

    method! vglob_aux g =
      (match g with
       | GFun ({ svar = vi }, loc) ->
         self#add_range loc
           (PVDecl (Some (Globals.Functions.get vi), Kglobal, vi))
       | GVar (vi, _, loc) ->
         self#add_range loc (PVDecl (None, Kglobal, vi))
       | GFunDecl (_, vi, loc) ->
         self#add_range loc
           (PVDecl (Some (Globals.Functions.get vi), Kglobal, vi))
       | GVarDecl (vi, loc) ->
         self#add_range loc (PVDecl (None, Kglobal, vi))
       | _ -> self#add_range (Global.loc g) (PGlobal g)
      );
      Cil.DoChildren
  end

(* Returns [true] if the column [col] is within location [loc]. *)
let location_contains_col (pos_start, pos_end : location) col =
  Filepos.input_column pos_start <= col && col <= Filepos.input_column pos_end

(* Applies several heuristics to try and match the best localizable to a
   given position [pos]. The list [possible_locs] should contain all
   localizables in a given line. If [possible_col] is [true], then we try
   to take column information into account.
   Some heuristics may return an empty list, in which case a fallback is
   later used to return a better choice. *)
let apply_location_heuristics precise_col possible_locs pos =
  let col = Filepos.input_column pos in
  Kernel.debug ~dkey
    "apply_location_heuristics (precise_col:%b): pos: %a, col: %d@\n\
     possible_locs:@ %a"
    precise_col Filepos.pretty pos col
    (Pretty_utils.pp_list ~sep:"@\n"
       (Pretty_utils.pp_pair ~sep:" :: " Fileloc.pretty Localizable.pretty)) possible_locs;
  (* Heuristic 1: we try to obtain a subset of localizables related to a given
     position, or a given column if [precise_col] is true.
     May result in an empty list. *)
  let filter_locs l =
    List.filter (fun (((pos_start, _) as loc'), _) ->
        if precise_col then location_contains_col loc' col
        else pos = pos_start
      ) l
  in
  (* Heuristic 2: prioritize expressions if they are present.
     May result in an empty list. *)
  let exps l =
    List.filter (fun (_, lz) -> match lz with | PExp _ -> true | _ -> false) l
  in
  (* Heuristic 3: when we have more than one match with the exact same location,
     we pick the last one in the list. This will be the first statement that has
     been encountered, and this criterion seems to work well with temporaries
     introduced by Cil. *)
  let last l = match List.rev l with [] -> None | (_, lz) :: _ -> Some lz in
  (* Heuristic 4: when there are no exact locations, we will consider the
     innermost ones, that is, those at the top of the list. *)
  let innermost_in loc l =
    List.filter (fun (loc', _) -> Fileloc.equal loc loc') l
  in
  match possible_locs, filter_locs possible_locs with
  | [], _ -> (* no possible localizables *) None
  | _, (_ :: _ as exact) ->
    (* one or more exact localizables; we prioritize expressions *)
    begin
      match exps exact with
      | [] -> (* no expressions, just take the last localizable *) last exact
      | exps -> (* take the last (usually only) expression *) last exps
    end
  | (loc', _) :: __, [] ->
    (* No exact loc. We consider the innermost statements,
       ie those at the top of the list *)
    let filtered = innermost_in loc' possible_locs in
    last filtered

let pos_to_localizable ?(precise_col=false) pos =
  if not (MappingLineLocalizable.is_computed ()) then (
    let vis = new pos_to_localizable in
    Visitor.visitFramacFile (vis :> Visitor.frama_c_visitor) (Ast.get ());
    MappingLineLocalizable.mark_as_computed ();
  );
  try
    (* Find the mapping from this file to locs-by-line *)
    let hpath = MappingLineLocalizable.find (Filepos.path pos) in
    (* Find the localizable for this line *)
    let all = LineToLocalizable.find_all hpath (Filepos.line pos) in
    match apply_location_heuristics precise_col all pos with
    | Some locz ->
      Kernel.feedback ~dkey "pos: %a -> locz: %a"
        Filepos.pretty pos Localizable.pretty locz;
      Some locz
    | None ->
      Kernel.feedback ~dkey "pos: %a -> NO locz"
        Filepos.pretty pos;
      None
  with
  | Not_found ->
    Kernel.debug ~once:true ~source:pos "no matching localizable found";
    None

(* -------------------------------------------------------------------------- *)
(* --- Printer API                                                        --- *)
(* -------------------------------------------------------------------------- *)

module type INFO =
sig
  val tag : localizable -> string
  val unfold : stmt -> bool
end

(* We delay the creation of the class to execution time, so that all
   pretty-printer extensions get properly registered (as we want to inherit
   from them). The only known solution is to use a functor *)
module BUILD(Info : INFO)(X: Printer.PrinterClass) : Printer.PrinterClass =
struct

  class printer () : Printer.extensible_printer = object(self)

    inherit X.printer () as super

    val mutable current_property = None

    method private current_kinstr =
      match self#current_stmt with
      | None -> Kglobal
      | Some st -> Kstmt st

    method private current_sid =
      match super#current_stmt with
      | None -> assert false
      | Some st -> st.sid

    method private current_kf =
      match super#current_function with
      | None -> None
      | Some fd -> Some (Globals.Functions.get fd)

    val mutable current_ca = None

    val mutable active_behaviors = []

    method private current_behavior_or_loop =
      match current_ca with
        None ->
        let active = Datatype.String.Set.of_list active_behaviors in
        Property.Id_contract (active ,Option.get self#current_behavior)
      | Some ca -> Property.Id_loop ca

    (* When [stmt] is a call, this method "inlines" the preconditions of the
       functions that may be called here, with some context. This way,
       bullets are more precise, etc. *)
    method private preconditions_at_call fmt stmt =
      match stmt.skind with
      | Instr (Call _)
      | Instr (Local_init (_, ConsInit _, _)) ->
        let extract_instance_predicate = function
          | Property.IPPropertyInstance {Property.ii_pred} -> ii_pred
          (* Other cases should not happen, unless a plugin has replaced call
             preconditions. In this case, print nothing but do not crash. *)
          | _ -> raise Not_found
        in
        let extract_predicate = function
          | Property.IPPredicate {Property.ip_pred} -> ip_pred
          | _ -> assert false
        in
        (* Functions called at this point *)
        let called = Statuses_by_call.all_functions_with_preconditions stmt in
        let warn_missing = false in
        let add_by_kf kf acc =
          let ips =
            Statuses_by_call.all_call_preconditions_at ~warn_missing kf stmt
          in
          if ips = [] then acc else (kf, ips) :: acc
        in
        let ips_all_kfs = Kernel_function.Hptset.fold add_by_kf called [] in
        let pp_one fmt (original_p, p) =
          match extract_instance_predicate p with
          | Some pred -> Format.fprintf fmt "@[%a@]" self#requires_aux (p, pred)
          | None ->
            let pred = extract_predicate original_p in
            (* Makes the original predicate non clickable, as it may involve
               the formal parameters which are not in scope at the call site. *)
            Format.fprintf fmt "@[Non transposable: %s@]"
              (Format.asprintf "@[%a@]" self#requires_aux (original_p, pred))
          | exception Not_found -> ()
        in
        let pp_by_kf fmt (kf, ips) =
          Format.fprintf fmt "@[preconditions of %a:@]@ %a"
            Kernel_function.pretty kf
            (Pretty_utils.pp_list ~pre:"" ~sep:"@ " ~suf:"" pp_one) ips
        in
        if ips_all_kfs <> [] then
          Pretty_utils.pp_list ~pre:"@[<v 3>/* " ~sep:"@ " ~suf:" */@]@ "
            pp_by_kf fmt ips_all_kfs
      | _ -> ()

    method! pp_while_head fmt cond =
      let kf = Option.get self#current_kf in
      let stmt = Option.get self#current_stmt in
      Format.fprintf fmt "@{<%s>%a@}"
        (Info.tag (PStmtStart(kf,stmt)))
        super#pp_while_head cond

    method! next_stmt next fmt current =
      if Info.unfold current
      then self#preconditions_at_call fmt current;
      Format.fprintf fmt "@{<%s>%a@}"
        (Info.tag (PStmt (Option.get self#current_kf,current)))
        (super#next_stmt next) current

    method! lval fmt lv =
      match self#current_kinstr with
      | Kglobal -> super#lval fmt lv
      (* Do not highlight the lvals in initializers. *)
      | Kstmt _ as ki ->
        Format.fprintf fmt "@{<%s>"
          (Info.tag (PLval (self#current_kf,ki,lv)));
        (match lv with
         | Var vi, (Field _| Index _ as o) ->
           (* Small hack to be able to click on the arrays themselves
              in the easy cases *)
           self#lval fmt (Var vi, NoOffset);
           self#offset fmt o
         | _ -> super#lval fmt lv
        );
        Format.fprintf fmt "@}"

    method! exp fmt e =
      match e.enode with
      | Lval lv ->
        (* Do not mark immediate l-values as they would not be
               selectable anyway because of the embedded tags of self#lval.
               This is only an optimization. *)
        self#lval fmt lv
      | _ ->
        Format.fprintf fmt "@{<%s>"
          (Info.tag (PExp (self#current_kf,self#current_kinstr,e)));
        super#exp fmt e;
        Format.fprintf fmt "@}"

    method! term_lval fmt lv =
      (* similar to pLval, except that term_lval can appear in specifications
         of functions (ki = None, kf <> None). Initializers are ignored. *)
      if self#current_kinstr = Kglobal && self#current_kf = None then begin
        super#term_lval fmt lv (* Do not highlight the lvals in initializers. *)
      end else begin
        match current_property with
        | None -> (* Also use default printer for this case (possible inside
                     pragmas, for example). *)
          super#term_lval fmt lv
        | Some ip ->
          Format.fprintf fmt "@{<%s>"
            (Info.tag
               (PTermLval (self#current_kf, self#current_kinstr, ip, lv)));
          (match lv with
           | TVar vi, (TField _| TIndex _ as o) ->
             self#term_lval fmt (TVar vi, TNoOffset);
             self#term_offset fmt o
           | _ -> super#term_lval fmt lv
          );
          Format.fprintf fmt "@}"
      end

    method! vdecl fmt vi =
      Format.fprintf fmt "@{<%s>%a@}"
        (Info.tag (PVDecl (self#current_kf, self#current_kinstr, vi)))
        super#vdecl vi

    method private tag_property p =
      current_property <- Some p;
      Info.tag (PIP p)

    method! code_annotation fmt ca =
      match ca.annot_content with
      | AAssert _ | AInvariant _ | AVariant _ ->
        let ip =
          Property.ip_of_code_annot_single
            (Option.get self#current_kf)
            (Option.get self#current_stmt)
            ca
        in
        Format.fprintf fmt "@{<%s>%a@}"
          (self#tag_property ip)
          super#code_annotation ca;
      | AStmtSpec (active,_) | AExtended(active,_,_) ->
        (* tags will be set in the inner nodes. *)
        active_behaviors <- active;
        super#code_annotation fmt ca;
        active_behaviors <- [];
      | AAllocation _
      | AAssigns _  ->
        (* tags will be set in the inner nodes. *)
        current_ca <- Some ca;
        super#code_annotation fmt ca;
        current_ca <- None

    method! typeref t pp fmt x =
      match definition_of_type t with
      | None -> Format.fprintf fmt "@{<%s>%a@}" (Info.tag (PType t)) pp x
      | Some loc -> Format.fprintf fmt "@{<%s>%a@}" (Info.tag loc) pp x

    method! typedef g pp fmt x =
      Format.fprintf fmt "@{<%s>%a@}" (Info.tag (PGlobal g)) pp x

    method! extended fmt ext =
      let loc =
        match self#current_kf with
        | None -> Property.ELGlob
        | Some kf -> Property.e_loc_of_stmt kf self#current_kinstr
      in
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property Property.(ip_of_extended loc ext))
        super#extended ext;

    method private requires_aux fmt (ip, p) =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property ip)
        super#requires p;

    method! requires fmt p =
      let b = Option.get self#current_behavior in
      let ip =
        Property.ip_of_requires
          (Option.get self#current_kf) self#current_kinstr b p
      in
      self#requires_aux fmt (ip, p)

    method! behavior fmt b =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_behavior
              (Option.get self#current_kf)
              self#current_kinstr
              ~active:active_behaviors b))
        super#behavior b

    method! decreases fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_decreases
              (Option.get self#current_kf) self#current_kinstr t))
        super#decreases t;

    method! terminates fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_terminates
              (Option.get self#current_kf) self#current_kinstr t))
        super#terminates t;

    method! complete_behaviors fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_complete
              (Option.get self#current_kf)
              self#current_kinstr
              ~active:active_behaviors
              t))
        super#complete_behaviors t

    method! disjoint_behaviors fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_disjoint
              (Option.get self#current_kf)
              self#current_kinstr
              ~active:active_behaviors
              t))
        super#disjoint_behaviors t

    method! assumes fmt p =
      let b = Option.get self#current_behavior in
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_assumes
              (Option.get self#current_kf) self#current_kinstr b p))
        super#assumes p;

    method! post_cond fmt pc =
      let b = Option.get self#current_behavior in
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_ensures
              (Option.get self#current_kf) self#current_kinstr b pc))
        super#post_cond pc;

    method! assigns s fmt a =
      match
        Property.ip_of_assigns (Option.get self#current_kf) self#current_kinstr
          self#current_behavior_or_loop a
      with
        None -> super#assigns s fmt a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (self#tag_property ip) (super#assigns s) a

    method! from s fmt ((_, f) as from) =
      match f with
      | FromAny -> super#from s fmt from
      | From _ ->
        let ip =
          Option.get
            (Property.ip_of_from
               (Option.get self#current_kf) self#current_kinstr
               self#current_behavior_or_loop from)
        in
        Format.fprintf fmt "@{<%s>%a@}"
          (Info.tag (PIP ip)) (super#from s) from

    method! global_annotation fmt a =
      match Property.ip_of_global_annotation_single a with
      | None ->
        let loc = loc_of_global_annotation a in
        Format.fprintf fmt "@{<%s>%a@}"
          (Info.tag (PGlobal(GAnnot(a,loc))))
          super#global_annotation a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Info.tag (PIP ip)) super#global_annotation a

    method! allocation ~isloop fmt a =
      match
        Property.ip_of_allocation (Option.get self#current_kf) self#current_kinstr
          self#current_behavior_or_loop a
      with
        None -> super#allocation ~isloop fmt a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Info.tag (PIP ip)) (super#allocation ~isloop) a;

    method! stmtkind sattr next fmt sk =
      (* Special tag denoting the start of the statement, WITHOUT any ACSL
         assertion/statement contract, etc. *)
      let s = Option.get self#current_stmt in
      let f = Option.get self#current_kf in
      let tag = Info.tag (PStmtStart(f,s)) in
      Format.fprintf fmt "@{<%s>%a@}" tag (super#stmtkind sattr next) sk

    method! global fmt = function
      | GVar(vi, _, _) | GVarDecl(vi, _) when Ast_info.is_string_literal vi ->
        Format.fprintf fmt "@[%a = %a;@.@]"
          self#vdecl vi self#str_literal (Globals.Vars.get_string_literal vi)
      | g -> super#global fmt g

    initializer force_brace <- true

  end
end

module type Tag =
sig
  val tag : localizable -> string
end

module type S_pp =
sig
  include Printer_api.S_pp
  val with_unfold_precond : (stmt -> bool) ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'a -> unit)
end

module Make(T : Tag) =
struct

  let unfold = ref (fun (_ : stmt) -> false)

  let printer () =
    let pp = Printer.current_printer () in
    let module PP = (val pp: Printer.PrinterClass) in
    let module INFO = struct
      let tag = T.tag
      let unfold s = !unfold s
    end in
    let module TagPrinterClass = BUILD(INFO)(PP) in
    new TagPrinterClass.printer ()

  let with_unfold_precond unfolder f fmt x =
    let stack = !unfold in
    try unfold := unfolder ; f fmt x ; unfold := stack
    with err -> unfold := stack ; raise err

  include Printer_builder.Make_pp(struct let printer = printer end)

end

(* -------------------------------------------------------------------------- *)
