(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_datatype


type binop =
  | Plus | Minus | Mult | Div | Mod
  | Lt | Gt | Le | Ge | Eq | Ne (* arithmetic comparison *)

type exp =
  {
    enode : exp_node;
    rtes : rte list;
    origin : term option
  }

and exp_node =
  | True
  | False
  | Integer of {ity : Analyses_types.number_ty; n : Z.t}
  | BinOp of binop_node
  | Lval of lval
  | SizeOf of typ
  | Coerce of {coerce_to : typ; coerced : exp}

and binop_node = {ity : Number_ty.t; binop : binop; op1 : exp; op2 : exp}

and lhost =
  | Var of varinfo
  | Mem of exp

and lval = lhost * offset

and offset =
  | NoOffset
  | Field of fieldinfo * offset
  | Index of exp * offset

and rte = {rnode : exp_node; rorigin : predicate}

let of_bool = function
  | true -> True
  | false -> False

module Pretty = struct
  open Format

  let pp_binop fmt b =
    fprintf fmt "%s"
      (match b with
       | Plus -> "+"
       | Minus -> "-"
       | Mult -> "*"
       | Div -> "/"
       | Mod -> "%"
       | Lt -> "<"
       | Gt -> ">"
       | Le -> "<="
       | Ge -> ">="
       | Eq -> "=="
       | Ne -> "!=")

  let rec pp_lhost fmt = function
    | Var vi -> Printer.pp_varinfo fmt vi
    | Mem exp -> fprintf fmt "*@[%a@]" pp_exp exp

  and pp_lval fmt (host, offset) =
    pp_lhost fmt host;
    pp_offset fmt offset

  and pp_offset fmt = function
    | NoOffset -> Printer.pp_offset fmt NoOffset
    | Field (fi, o) ->
      fprintf fmt ".%a%a" Printer.pp_field fi pp_offset o
    | Index (e, o) ->
      fprintf fmt "[%a]%a" pp_exp e pp_offset o

  and pp_exp fmt {enode} = pp_exp_node fmt enode

  and pp_exp_node fmt = function
    | True -> fprintf fmt "true"
    | False -> fprintf fmt "false"
    | Integer {ity; n} ->
      fprintf fmt "@[%a@]@ :@ @[%a@]" Z.pretty n Analyses_types.pp_number_ty ity;
    | BinOp {binop; op1; op2} ->
      fprintf fmt "@[%a@]@ %a@ @[%a@]" pp_exp op1 pp_binop binop pp_exp op2
    | Lval lval -> pp_lval fmt lval
    | SizeOf ty -> fprintf fmt "SizeOf(@[%a])" Printer.pp_typ ty
    | Coerce {coerce_to = ty; coerced = exp} ->
      fprintf fmt "Coerce(@[%a@],@ @[%a@])" Printer.pp_typ ty pp_exp exp

  let pp_rtes fmt rtes =
    let pp_rte fmt rte = fprintf fmt "%a" pp_exp_node rte.rnode in
    Pretty_utils.pp_list ~pre:"[" ~suf:"]" ~sep:";@ " pp_rte fmt rtes

end


module Optimization = struct

  module Aux = struct
    let modulo_coerce e1 e2 =
      let under_coerce e = match e.enode with
        | Coerce {coerced = exp} -> exp
        | _ -> e
      in
      (under_coerce e1).enode, (under_coerce e2).enode
  end

  let plus ~ity e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, _ when Z.is_zero z1 -> Some e2.enode
    | _, Integer {n = z2} when Z.is_zero z2 -> Some e1.enode
    | Integer {n = z1}, Integer {n = z2} -> Some (Integer {n = Z.add z1 z2; ity})
    | _ -> None

  let minus ~ity e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, Integer {n = z2} when Z.is_zero z1 ->
      Some (Integer {n = Z.neg z2; ity})
    | _, Integer {n = z2} when Z.is_zero z2 -> Some e1.enode
    | Integer {n = z1}, Integer {n = z2} -> Some (Integer {n = Z.sub z1 z2; ity})
    | _ -> None

  let mult ~ity e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, _ when Z.is_zero z1 -> Some (Integer {n = Z.zero; ity})
    | _, Integer {n = z2} when Z.is_zero z2 -> Some (Integer {n = Z.zero; ity})
    | Integer {n = z1}, _ when Z.is_one z1 -> Some e2.enode
    | _, Integer {n = z2} when Z.is_one z2 -> Some e1.enode
    | Integer {n = z1}, Integer {n = z2} -> Some (Integer {n = Z.mul z1 z2; ity})
    | _ -> None

  let div ~ity e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, _ when Z.is_zero z1 -> Some (Integer {n = Z.zero; ity})
    | Integer {n = z1}, Integer {n = z2} when not (Z.is_zero z2) ->
      Some (Integer {n = Z.ediv z1 z2; ity})
    | _ -> None

  let modulo ~ity e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, _ when Z.is_zero z1 -> Some (Integer {n = Z.zero; ity})
    | Integer {n = z1}, Integer {n = z2} when not (Z.is_zero z2) ->
      Some (Integer {n = Z.erem z1 z2; ity})
    | _ -> None

  let lt e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, Integer {n = z2} -> Some (of_bool @@ Z.lt z1 z2)
    | _ -> None

  let gt e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, Integer {n = z2} -> Some (of_bool @@ Z.gt z1 z2)
    | _ -> None

  let le e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, Integer {n = z2} -> Some (of_bool @@ Z.leq z1 z2)
    | _ -> None

  let ge e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, Integer {n = z2} -> Some (of_bool @@ Z.geq z1 z2)
    | _ -> None

  let eq e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, Integer {n = z2} -> Some (of_bool @@ Z.equal z1 z2)
    | _ -> None

  let ne e1 e2 =
    match Aux.modulo_coerce e1 e2 with
    | Integer {n = z1}, Integer {n = z2} -> Some (of_bool @@ not @@ Z.equal z1 z2)
    | _ -> None
end

module Exp_node = struct
  let of_binop ~bop ~ity =
    let open Optimization in
    match bop with
    | Plus -> plus ~ity
    | Minus -> minus ~ity
    | Mult -> mult ~ity
    | Div -> div ~ity
    | Mod -> modulo ~ity
    | Lt -> lt
    | Gt -> gt
    | Le -> le
    | Ge -> ge
    | Eq -> eq
    | Ne -> ne
end

module Exp = struct
  module Aux = struct
    let of_exp_node ?origin enode = {enode; rtes = []; origin}
  end

  let lval ?origin lval = Aux.of_exp_node ?origin @@ Lval lval
  let integer ~origin ~ity n = Aux.of_exp_node ~origin @@ Integer {ity; n}
  let sizeof ~origin ty = Aux.of_exp_node ~origin @@ SizeOf ty
  let rte rte = Aux.of_exp_node ?origin:None rte.rnode

  let mk_true ?origin () = Aux.of_exp_node ?origin True
  let mk_false ?origin () = Aux.of_exp_node ?origin False

  let binop ?origin bop ity e1 e2 =
    let org = BinOp {binop = bop; ity; op1 = e1; op2 = e2} in
    let res = if Options.O.get () < 1 then org
      else match Exp_node.of_binop ~bop ~ity e1 e2 with
        | Some e ->
          Options.debug ~dkey:Options.Dkey.interlang_print_opt ~level:3
            "@[%a@] => @[%a@]"
            Pretty.pp_exp_node org Pretty.pp_exp_node e;
          e
        | None -> org
    in Aux.of_exp_node ?origin res

  let coerce ?origin ~coerce_to exp =
    let origin =
      try List.find Option.is_some [origin; exp.origin]
      with Not_found -> None
    in
    match exp with
    | {enode = Coerce c; origin} as exp -> (* collapse stacked coercions *)
      {exp with origin; enode = Coerce {c with coerce_to}}
    | exp -> Aux.of_exp_node ?origin @@ Coerce {coerce_to; coerced = exp}
end

module Rte = struct
  let make p e = {rnode = e.enode; rorigin = p}
end

module Lhost = struct
  let var ?name vi =
    let name = Option.value ~default:vi.vname name in
    Var {vi with vorig_name = name}

  let mem e = Mem e
end

module Helpers = struct
  let attach_rtes rtes e = {e with rtes = e.rtes @ rtes}
  let is_div_or_mod = function
    | (Div | Mod) -> true | _ -> false
end
