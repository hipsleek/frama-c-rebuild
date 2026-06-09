(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Logic_ptree

type slevel_annotation =
  | SlevelMerge
  | SlevelDefault
  | SlevelLocal of int
  | SlevelFull

type unroll_annotation =
  | UnrollAmount of Cil_types.term
  | UnrollFull
  | UnrollAuto of int

type split_kind = Static | Dynamic
type split_term =
  | Term of Cil_types.term
  | Predicate of Cil_types.predicate
  | ConditionalCases

type flow_annotation =
  | FlowSplit of split_term * split_kind
  | FlowMerge of split_term

type allocation_kind = By_stack | Fresh | Fresh_weak | Imprecise

(* We use two representations for annotations :
   - the high level representation (HL) which is exported from this module
   - the low level representation (Cil) which is used by the kernel to store
     any annotation

   Annotations in this module define the export and import function to go from
   one to another. Then, the parse and print functions works directly on the
   high level representation.

             add  --+
                    |
   ACSL --> parse --+--> HL --> export --> Cil --> import --+--> HL --> print
                                                            |
                                                            +--> get
*)

exception Parse_error

(* Where an Eva directive is applied. *)
type kind =
  | Here (* The directive is applied when encountered. *)
  | Stmt (* The directive has effect on the next statement. *)
  | Loop (* The directive concerns a loop. *)

module type Annotation =
sig
  type t

  val name : string
  val kind : kind
  val parse : typing_context:Logic_typing.typing_context -> lexpr list -> t
  val export : t -> acsl_extension_kind
  val import : acsl_extension_kind -> t
  val print : Format.formatter -> t -> unit
end

module Register (M : Annotation) =
struct
  include M

  let typer typing_context loc args =
    try export (parse ~typing_context args)
    with Parse_error ->
      typing_context.Logic_typing.error loc "Invalid %s directive" name

  let printer _pp fmt lp =
    print fmt (import lp)

  let () =
    let register =
      match kind with
      | Here -> Acsl_extension.register_code_annot
      | Stmt -> Acsl_extension.register_code_annot_next_stmt
      | Loop -> Acsl_extension.register_code_annot_next_loop
    in
    register ~plugin:"eva" name typer ~printer false

  let get stmt =
    let filter_add _emitter annot acc =
      match annot.annot_content with
      | Cil_types.AExtended (_, is_loop_annot, {ext_name=name'; ext_kind})
        when name' = name && is_loop_annot = (kind = Loop) ->
        import ext_kind :: acc
      | _ -> acc
    in
    List.rev (Annotations.fold_code_annot filter_add stmt [])

  let add ~emitter stmt annot =
    let loc = Cil_datatype.Stmt.loc stmt in
    let param = M.export annot in
    let extension =
      Logic_const.new_acsl_extension ~plugin:"eva" name loc false param
    in
    let annot_node = Cil_types.AExtended ([], kind = Loop, extension) in
    let code_annotation = Logic_const.new_code_annotation annot_node in
    Annotations.add_code_annot emitter stmt code_annotation
end


module Slevel = Register (struct
    type t = slevel_annotation

    let name = "slevel"
    let kind = Here

    let parse ~typing_context:_ = function
      | [{lexpr_node = PLvar "default"}] -> SlevelDefault
      | [{lexpr_node = PLvar "merge"}] -> SlevelMerge
      | [{lexpr_node = PLvar "full"}] -> SlevelFull
      | [{lexpr_node = PLconstant (IntConstant i)}] ->
        let i =
          try int_of_string i
          with Failure _ -> raise Parse_error
        in
        if i < 0 then raise Parse_error;
        SlevelLocal i
      | _ -> raise Parse_error

    let export = function
      | SlevelDefault -> Ext_terms [Logic_const.tstring "default"]
      | SlevelMerge -> Ext_terms [Logic_const.tstring "merge"]
      | SlevelLocal i -> Ext_terms [Logic_const.tinteger i]
      | SlevelFull -> Ext_terms [Logic_const.tstring "full"]

    let import = function
      | Ext_terms [{term_node}] ->
        begin match term_node with
          | TConst (LStr "default") -> SlevelDefault
          | TConst (LStr "merge") -> SlevelMerge
          | TConst (LStr "full") -> SlevelFull
          | TConst (Integer (i, _)) -> SlevelLocal (Z.to_int i)
          | _ -> SlevelDefault (* be kind. Someone is bound to write a visitor
                                  that will simplify our term into something
                                  unrecognizable... *)
        end
      | _ -> assert false

    let print fmt = function
      | SlevelDefault -> Format.pp_print_string fmt "default"
      | SlevelMerge -> Format.pp_print_string fmt "merge"
      | SlevelLocal i -> Format.pp_print_int fmt i
      | SlevelFull -> Format.pp_print_string fmt "full"
  end)

module Unroll = Register (struct
    type t = unroll_annotation

    let name = "unroll"
    let kind = Loop

    let parse ~typing_context = function
      | [] -> UnrollFull
      | [t] ->
        let open Logic_typing in
        UnrollAmount
          (typing_context.type_term typing_context typing_context.pre_state t)
      | [{lexpr_node = PLvar "auto"};
         {lexpr_node = PLconstant (IntConstant i)}] ->
        let i = match int_of_string i with
          | i when i >= 0 -> i
          | _i -> raise Parse_error
          | exception Failure _ -> raise Parse_error
        in
        UnrollAuto i
      | _ -> raise Parse_error

    let export = function
      | UnrollFull -> Ext_terms []
      | UnrollAmount t -> Ext_terms [t]
      | UnrollAuto i ->
        Ext_terms [Logic_const.tstring "auto"; Logic_const.tinteger i]

    let import = function
      | Ext_terms [] -> UnrollFull
      | Ext_terms [t] -> UnrollAmount t
      | Ext_terms [
          {term_node = TConst (LStr "auto")};
          {term_node = TConst (Integer (i, _))}] ->
        UnrollAuto (Z.to_int i)
      | _ -> assert false

    let print fmt = function
      | UnrollFull -> ()
      | UnrollAmount t -> Printer.pp_term fmt t
      | UnrollAuto i -> Format.fprintf fmt "auto, %d" i
  end)

module SplitTermAnnotation =
struct
  type t = split_term

  let kind = Here

  let parse ~typing_context:context = function
    | [{lexpr_node = PLvar "\\cases"}] ->
      ConditionalCases
    | [t] ->
      begin
        let open Logic_typing in
        let exception No_term in
        try
          let error _loc _fmt = raise No_term in
          let context = { context with error } in
          let term = context.type_term context context.pre_state t in
          Term term
        with
        | No_term ->
          Predicate (context.type_predicate context context.pre_state t)
        | Logic_to_c.No_conversion ->
          Kernel.warning ~wkey:Kernel.wkey_annot_error ~once:true ~current:true
            "split/merge expressions must be valid expressions; ignoring";
          raise Parse_error
      end
    | _ -> raise Parse_error

  let export = function
    | Term term -> Ext_terms [term]
    | Predicate pred -> Ext_preds [pred]
    | ConditionalCases -> Ext_terms [ Logic_const.tstring "\\cases" ]

  let import = function
    | Ext_terms [{term_node=TConst (LStr "\\cases")}] -> ConditionalCases
    | Ext_terms [term] -> Term term
    | Ext_preds [pred] -> Predicate pred
    | _ -> assert false

  let print fmt = function
    | Term term -> Printer.pp_term fmt term
    | Predicate pred -> Printer.pp_predicate fmt pred
    | ConditionalCases -> Format.pp_print_string fmt "\\cases"
end

module Split = Register (struct
    include SplitTermAnnotation
    let name = "split"
  end)

module Merge = Register (struct
    include SplitTermAnnotation
    let name = "merge"
  end)

module DynamicSplit = Register (struct
    include SplitTermAnnotation
    let name = "dynamic_split"
  end)

let get_slevel_annot stmt =
  try Some (List.hd (Slevel.get stmt))
  with Failure _ -> None

let get_unroll_annot stmt = Unroll.get stmt

let get_flow_annot stmt =
  List.map (fun a -> FlowSplit (a, Static)) (Split.get stmt) @
  List.map (fun a -> FlowSplit (a, Dynamic)) (DynamicSplit.get stmt) @
  List.map (fun a -> FlowMerge a) (Merge.get stmt)


let add_slevel_annot = Slevel.add

let add_unroll_annot = Unroll.add

let add_flow_annot ~emitter stmt flow_annotation =
  let f, annot =
    match flow_annotation with
    | FlowSplit (annot, Static) -> Split.add, annot
    | FlowSplit (annot, Dynamic) -> DynamicSplit.add, annot
    | FlowMerge annot -> Merge.add, annot
  in
  f ~emitter stmt annot


module Subdivision = Register (struct
    type t = int
    let name = "subdivide"
    let kind = Stmt

    let parse ~typing_context:_ = function
      | [{lexpr_node = PLconstant (IntConstant i)}] ->
        let i =
          try int_of_string i
          with Failure _ -> raise Parse_error
        in
        if i < 0 then raise Parse_error;
        i
      | _ -> raise Parse_error

    let export i = Ext_terms [Logic_const.tinteger i]
    let import = function
      | Ext_terms [{term_node = TConst (Integer (i, _))}] -> Z.to_int i
      | _ -> assert false

    let print fmt i = Format.pp_print_int fmt i
  end)

let get_subdivision_annot = Subdivision.get
let add_subdivision_annot = Subdivision.add


module Allocation = struct
  let of_string = function
    | "by_stack"   -> Some By_stack
    | "fresh"      -> Some Fresh
    | "fresh_weak" -> Some Fresh_weak
    | "imprecise"  -> Some Imprecise
    | _            -> None

  let to_string = function
    | By_stack   -> "by_stack"
    | Fresh      -> "fresh"
    | Fresh_weak -> "fresh_weak"
    | Imprecise  -> "imprecise"

  include Register (struct
      type t = allocation_kind
      let name = "eva_allocate"
      let kind = Stmt

      let parse ~typing_context:_ = function
        | [{lexpr_node = PLvar s}] -> Option.get ~exn:Parse_error (of_string s)
        | _ -> raise Parse_error

      let export alloc_kind =
        Ext_terms [Logic_const.tstring (to_string alloc_kind)]

      let import = function
        | Ext_terms [{term_node}] ->
          (* Be kind and return By_stack by default. Someone is bound to write a
             visitor that will simplify our term into something unrecognizable. *)
          begin match term_node with
            | TConst (LStr s) -> Option.value ~default:By_stack (of_string s)
            | _ -> By_stack
          end
        | _ -> assert false

      let print fmt alloc_kind =
        Format.pp_print_string fmt (to_string alloc_kind)
    end)

  let get stmt =
    match get stmt with
    | [] -> Option.get (of_string (Parameters.AllocBuiltin.get ()))
    | [x] -> x
    | x :: _ ->
      Self.warning ~current:true ~once:true
        "Several eva_allocate annotations at the same statement; selecting %s\
         and ignoring the others." (to_string x);
      x
end

let get_allocation = Allocation.get


module ArraySegmentation = Register (struct
    type t = Cil_types.varinfo * Cil_types.offset * Cil_types.exp list
    let name = "array_partition"
    let kind = Here

    let convert = function
      | {term_node =  TLval (TVar {lv_origin=Some vi}, toffset)} :: tbounds ->
        begin try
            let offset = Logic_to_c.term_offset_to_offset toffset
            and bounds = List.map (Logic_to_c.term_to_exp ?result:None) tbounds
            in
            Some (vi, offset, bounds)
          with
            Logic_to_c.No_conversion -> None
        end
      | _ -> None

    let parse ~typing_context:context lexprs =
      let open Logic_typing in
      let l = List.map (context.type_term context context.pre_state) lexprs in
      Option.get ~exn:Parse_error (convert l)

    let import = function
      | Ext_terms l -> Option.get (convert l)
      | _ -> assert false

    let export (vi, offset, bounds) =
      let lv = Cil.cvar_to_lvar vi
      and toffset = Logic_utils.offset_to_term_offset offset
      and tbounds = List.map Logic_utils.expr_to_term bounds in
      let tlval = TVar lv, toffset in
      let tarray = Logic_const.term (TLval tlval) (Cil.typeOfTermLval tlval) in
      Ext_terms (tarray :: tbounds)

    let print fmt (vi,offset,bounds) =
      Format.fprintf fmt "%a, %a"
        Cil_printer.pp_lval (Var vi, offset)
        (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_exp) bounds
  end)


type array_segmentation = ArraySegmentation.t
let add_array_segmentation = ArraySegmentation.add
let read_array_segmentation ext = ArraySegmentation.import ext.ext_kind


module DomainScope = Register (struct
    type t = string * Cil_types.varinfo list
    let name = "eva_domain_scope"
    let kind = Here

    let parse ~typing_context:context =
      let parse_domain = function
        | {lexpr_node = PLvar v} -> v
        | _ -> raise Parse_error
      and parse_var = function
        | {lexpr_node = PLvar v} ->
          begin match context.Logic_typing.find_var v with
            | {lv_origin=Some vi} -> vi
            | _ -> raise Parse_error
            | exception Not_found ->
              Kernel.warning ~wkey:Kernel.wkey_annot_error
                ~once:true ~current:true
                "cannot find variable %s at this point" v;
              raise Parse_error
          end
        | _ -> raise Parse_error
      in
      function
      | domain :: vars ->
        parse_domain domain, List.map parse_var vars
      | _ -> raise Parse_error

    let import = function
      | Ext_terms ({term_node=TConst (LStr domain)} :: vars) ->
        let import_var = function
          | {term_node=TLval (TVar {lv_origin=Some vi}, TNoOffset)} -> vi
          | _ -> assert false
        in
        domain, List.map import_var vars
      | _ -> assert false

    let export (domain, vars) =
      let export_var vi =
        Logic_const.tvar (Cil.cvar_to_lvar vi)
      in
      Ext_terms (Logic_const.tstring domain :: List.map export_var vars)

    let print fmt (domain, vars) =
      Format.fprintf fmt "%s, %a"
        domain
        (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_varinfo) vars
  end)


type domain_scope = DomainScope.t
let add_domain_scope = DomainScope.add
let read_domain_scope ext = DomainScope.import ext.ext_kind
