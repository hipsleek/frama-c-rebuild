(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* -------------------------------------------------------------------------- *)
(* --- Annotation Generator                                               --- *)
(* -------------------------------------------------------------------------- *)

module Ltype = Cil_datatype.Logic_type_ByName
module Stmts = Cil_datatype.Stmt.Set
module Exp = Cil_builder.Exp
type visitor = Visitor.frama_c_visitor

type pred =
  | True
  | False
  | Pand of pred * pred
  | Por of pred * pred
  | Eval of Exp.exp
  | Pcall of string * Exp.exp list

let pand (a : pred) (b : pred) : pred =
  match a,b with
  | False,_ | _,False -> False
  | True,c | c,True -> c
  | _ -> Pand(a,b)

let por (a : pred) (b : pred) : pred =
  match a,b with
  | True,_ | _,True -> True
  | False,c | c,False -> c
  | _ -> Por(a,b)

let rec has_profile (vs : logic_var list) (ts : term list) =
  match vs, ts with
  | [],[] -> true
  | [],_ | _,[] -> false
  | lv::vs, t::ts ->
    Ltype.equal lv.lv_type t.term_type && has_profile vs ts

let matches_params (ts : term list) (fn : logic_info) =
  fn.l_labels = [] && has_profile fn.l_profile ts

let predicate ~loc ?(name=[]) (p : pred) : predicate =
  let rec aux = function
    | True -> Logic_const.ptrue
    | False -> Logic_const.pfalse
    | Pand(a,b) -> Logic_const.pand ~loc (aux a, aux b)
    | Por(a,b) -> Logic_const.por ~loc (aux a, aux b)
    | Eval e -> Exp.cil_pred ~loc e
    | Pcall(f,es) ->
      let ts = List.map (Exp.cil_term ~loc) es in
      let ls = Logic_env.find_all_logic_functions f in
      match List.find_opt (matches_params ts) ls with
      | None -> Self.fatal "[Export] Unknown predicate '%s'" f
      | Some li -> Logic_const.papp ~loc (li,[],ts)
  in { (aux p) with pred_name = name }

let error (err : Results.error) : pred =
  match err with
  | Top | DisabledDomain -> True
  | Bottom -> False

(* -------------------------------------------------------------------------- *)
(* --- Ivalues                                                            --- *)
(* -------------------------------------------------------------------------- *)

let iequal (exp : Exp.exp) (k : Z.t) : pred =
  Eval Exp.( exp == of_integer k )

let imin (exp : Exp.exp) (ival : Ival.t) : pred =
  match Ival.min_int ival with
  | None -> True
  | Some k -> Eval Exp.( of_integer k <= exp )

let imax (exp : Exp.exp) (ival : Ival.t) : pred =
  match Ival.max_int ival with
  | None -> True
  | Some k -> Eval Exp.( exp <= of_integer k )

let sparse = function
  | [] -> false
  | [_] -> true
  | x::xs ->
    let rec continuous x = function
      | [] -> true
      | y::ys -> Z.equal (Z.succ x) y && continuous y ys
    in not @@ continuous x xs

let ival (exp : Exp.exp) (ival : Ival.t) : pred =
  match Ival.project_small_set ival with
  | Some vs when sparse vs ->
    List.fold_left (fun w v -> por w (iequal exp v)) False vs
  | _ -> pand (imin exp ival) (imax exp ival)

(* -------------------------------------------------------------------------- *)
(* --- Fvalues                                                            --- *)
(* -------------------------------------------------------------------------- *)

let fNaN (exp : Exp.exp) (isNaN : bool) : pred =
  if isNaN then Pcall("\\is_NaN",[exp]) else False

let fmin ~kind (exp : Exp.exp) (a : Fval.F.t) : pred =
  if Fval.F.is_finite a then
    Eval Exp.( of_cfloat ~kind (Fval.F.to_float a) <= exp )
  else True

let fmax ~kind (exp : Exp.exp) (b : Fval.F.t) : pred =
  if Fval.F.is_finite b then
    Eval Exp.( exp <= of_cfloat ~kind (Fval.F.to_float b) )
  else True

let frange ~kind (exp : Exp.exp) = function
  | None -> False
  | Some(a,b) -> pand (fmin ~kind exp a) (fmax ~kind exp b)

let fval kind (exp : Exp.exp) (fval : Fval.t) : pred =
  let range,isNaN = Fval.min_and_max fval in
  por (fNaN exp isNaN) (frange ~kind exp range)

(* -------------------------------------------------------------------------- *)
(* --- Values                                                             --- *)
(* -------------------------------------------------------------------------- *)

let domain lv value =
  let exp = Exp.of_lval lv in
  let typ = Cil.typeOfLval lv in
  match Ast_types.unroll_node typ with
  | TInt _ | TEnum _ ->
    Results.as_ival value |> Result.fold ~error ~ok:(ival exp)
  | TFloat kind ->
    Results.as_fval value |> Result.fold ~error ~ok:(fval kind exp)
  | _ -> True

(* -------------------------------------------------------------------------- *)
(* --- Evaluation                                                        --- *)
(* -------------------------------------------------------------------------- *)

let export_value ~loc ?name lv request =
  Results.eval_lval lv request
  |> domain lv
  |> predicate ?name ~loc

(* -------------------------------------------------------------------------- *)
(* --- Instructions                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Lvs = Cil_datatype.LvalStructEq.Set

class collector =
  object(self)
    inherit Visitor.frama_c_inplace
    val mutable marked = Lvs.empty
    val mutable collected : lval list = []

    method private add : 'a. lval -> 'a Cil.visitAction =
      fun lv ->
      if Lvs.mem lv marked then Cil.SkipChildren else
        begin
          marked <- Lvs.add lv marked ;
          DoChildrenPost(fun r -> collected <- lv :: collected ; r)
        end

    method flush = collected

    method! vlval lv = self#add lv
    method! vterm_lval lv =
      begin
        match Logic_to_c.term_lval_to_lval lv with
        | exception Logic_to_c.No_conversion -> Cil.DoChildren
        | lv -> self#add lv
      end

    method add_lhs lv =
      ignore @@ Visitor.visitFramacOffset (self :> visitor) (snd lv) ;
      match fst lv with Var _ -> () | Mem e -> self#add_expr e

    method add_expr e =
      ignore @@ Visitor.visitFramacExpr (self :> visitor) e

    method add_instr = function
      | Set(lv,e,_) ->
        self#add_lhs lv ;
        self#add_expr e
      | Call(lvopt,f,es,_) ->
        Option.iter self#add_lhs lvopt ;
        ignore (self#add (f,NoOffset)) ;
        List.iter self#add_expr es
      | instr ->
        ignore @@ Visitor.visitFramacInstr (self :> visitor) instr

  end

let collect stmt =
  let acc = new collector in
  begin
    match stmt.skind with
    (* Instructions *)
    | Instr instr -> acc#add_instr instr
    (* Branching expressions *)
    | Return (Some e,_) | If(e,_,_,_) | Switch(e,_,_,_) -> acc#add_expr e
    (* Others *)
    | Return(None,_) -> ()
    | Goto _ | Break _ | Continue _
    | Loop _ | Block _ | UnspecifiedSequence _
    | Throw _ | TryCatch _ | TryFinally _ | TryExcept _
      -> ()
  end ;
  acc#flush

let export_stmt ?callstack ?name stmt =
  let request =
    let r = Results.before stmt in
    match callstack with
    | None -> r
    | Some c -> Results.in_callstack c r in
  List.map (predicate ?name ~loc:(Cil_datatype.Stmt.loc stmt)) @@
  if Results.is_empty request then [False] else
    List.fold_left
      (fun ps lv ->
         let p = domain lv @@ Results.eval_lval lv request in
         if p <> True then p :: ps else ps
      ) [] (collect stmt)

let is_dead stmt = Results.is_empty @@ Results.before stmt

(* -------------------------------------------------------------------------- *)
(* --- Annotation Generator                                               --- *)
(* -------------------------------------------------------------------------- *)

let emitter = Eva_utils.export_emitter

let generator () : visitor =
  object(self)
    inherit Visitor.frama_c_inplace
    val mutable dead = Stmts.empty (* annotated as dead *)

    method! vlval _ = Cil.SkipChildren
    method! vexpr _ = Cil.SkipChildren

    method !vstmt_aux stmt =
      match self#current_kf with
      | None -> Cil.SkipChildren
      | Some kf ->
        if not @@ List.for_all (fun s -> Stmts.mem s dead) stmt.preds then
          begin
            List.iter
              (Annotations.add_assert emitter ~kf stmt)
              (export_stmt stmt) ;
            Annotations.iter_code_annot
              (fun e ca ->
                 if Emitter.equal e emitter then
                   List.iter
                     (fun ip ->
                        Property_status.emit Analysis.emitter ~hyps:[] ip True
                     ) (Property.ip_of_code_annot kf stmt ca)
              ) stmt ;
          end ;
        if is_dead stmt
        then ( dead <- Stmts.add stmt dead ; Cil.SkipChildren )
        else Cil.DoChildren

  end

(* -------------------------------------------------------------------------- *)
(* --- Annotation Removal                                                 --- *)
(* -------------------------------------------------------------------------- *)

let cleaner () : visitor =
  object(self)
    inherit Visitor.frama_c_inplace

    method! vlval _ = Cil.SkipChildren
    method! vexpr _ = Cil.SkipChildren

    method !vstmt_aux stmt =
      match self#current_kf with
      | None -> Cil.SkipChildren
      | Some kf ->
        Annotations.iter_code_annot
          (fun e ca ->
             if Emitter.equal e emitter then
               Annotations.remove_code_annot e ~kf stmt ca
          ) stmt ;
        Cil.DoChildren

  end

(* -------------------------------------------------------------------------- *)
(* --- Command Line Option                                                --- *)
(* -------------------------------------------------------------------------- *)

let main () =
  let generator = lazy
    begin
      Analysis.compute () ;
      let ast = Ast.get () in
      let cleaner = cleaner () in
      Self.feedback ~ontty:`Transient "Cleaning annotations..." ;
      Visitor.visitFramacFile cleaner ast ;
      generator ()
    end
  in Parameters.Annot.iter
    begin fun kf ->
      let generator = Lazy.force generator in
      if Kernel_function.has_definition kf then
        if Results.are_available kf then
          let fundec = Kernel_function.get_definition kf in
          Self.feedback ~level:4 "Annotate %a" Kernel_function.pretty kf ;
          ignore @@ Visitor.visitFramacFunction generator fundec
        else
          Self.warning "Can not annotate %a (no available results)"
            Kernel_function.pretty kf
      else
        Self.warning "Can not annotate %a (no definition)"
          Kernel_function.pretty kf
    end

let () = Boot.Main.extend main

(* -------------------------------------------------------------------------- *)
