(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let dkey = Options.Dkey.rte

(** [Guards] stores the pairs ([term],[predicate list]) created during the
    analysis. *)
module Guards =
struct

  module Terms = Misc.Id_term.Hashtbl

  let tbl = Terms.create 10

  let add t pred =
    if not @@ Logic_utils.is_trivially_true pred then
      match Terms.find_opt tbl t with
      | Some preds -> Terms.replace tbl t (pred :: preds)
      | None -> Terms.add tbl t [pred]

  let apply ~default t f =
    match Terms.find_opt tbl t with
    | Some preds -> f preds
    | _ -> default

  let remove t = Terms.remove tbl t

  let mem t = Terms.mem tbl t

  let clear () = Terms.clear tbl

  let pretty fmt () =
    let pp_data fmt d =
      Pretty_utils.pp_list
        ~pre:"[" ~suf:"]" ~sep:";@ " Printer.pp_predicate fmt d
    in
    Terms.pretty
      ~item:(format_of_string "%a --> %a") Printer.pp_term pp_data fmt tbl
end

module Flags =
struct

  let removes_trivial () = Options.O.get () > 0

  (** [needs_div_mod ()] @return:
      - [true] if the option [-rte-div] from the RTE plugin is used (default);
      - [false] if the option [-rte-no-div] from the RTE plugin is used. *)
  let needs_div_mod () =
    if RteGen.Options.DoDivMod.is_set ()
    then RteGen.Options.DoDivMod.get ()
    else true

end

(** The module [Undefined_behaviours] contains functions that makes a guard for
    each kind of undefined behavior listed below:
    - division by zero *)
module Undefined_behaviours =
struct

  let preprocess_guard guard =
    Logic_normalizer.preprocess_predicate guard;
    Bound_variables.preprocess_predicate guard

  (** [div_by_zero ~loc divider] creates the predicate that checks if [divider]
      is not equal to [Z.zero]. The guard does not contain directly [divider]
      but a copy of it. *)
  let div_by_zero ~loc divider =
    let smart = Flags.removes_trivial () in
    let divider_cpy = Smart_term.copy ~smart divider in
    let pred =
      Smart_predicate.prel
        ~smart
        ~loc
        ~names:["division by zero"]
        Rneq
        divider_cpy
        (Logic_const.tint Z.zero)
    in
    preprocess_guard pred;
    pred

end

let rte_visitor =
  object(self)

    inherit E_acsl_visitor.visitor dkey

    (** [add_div_mod orig divider] adds an entry for [orig] if [divider] can
        be equal to zero. *)
    method private add_div_mod ~orig divider =
      if Flags.needs_div_mod () then
        Guards.add orig
          (Undefined_behaviours.div_by_zero ~loc:orig.term_loc divider)

    method !vterm t =
      begin match t.term_node with
        | TBinOp ((Div | Mod),_,divider) -> self#add_div_mod ~orig:t divider
        | _ -> ()
      end;
      Cil.DoChildren

    method !vpredicate p =
      begin match p.pred_content with
        | Paligned (_,v) -> self#add_div_mod ~orig:v v
        | _ -> ()
      end;
      Cil.DoChildren
  end

let preprocess ast =
  if Options.O.get () < 3
  then begin
    ignore @@ rte_visitor#visit_file ast;
    Options.feedback ~dkey:dkey "Result of the RTE analysis.%!";
    Options.feedback ~dkey:dkey "%a%!" Guards.pretty ()
  end else
    Options.feedback ~dkey:dkey "Skip the RTE analysis.%!"

let iter_on_guards t f = Guards.apply ~default:() t (List.iter f)

let fold_guards_il ~default t f =
  Guards.apply ~default t @@ List.fold_left (fun x y -> f y x) default

let fold_guards_old ~default t f =
  (* [collect t] returns the RTE guards associated to [t] and its sub-terms. *)
  let collect t =
    let preds = ref [] in
    let collector =
      object
        inherit Visitor.frama_c_inplace
        method! vterm t =
          Guards.apply ~default:() t (fun p -> preds := p @ !preds; Guards.remove t);
          match t.term_node with
          (* warning: we do not retrieve RTE guards from [Tif] sub-terms *)
          | Tif _ -> Cil.SkipChildren
          | _ -> Cil.DoChildren
      end in
    if Guards.mem t then ignore @@ Visitor.visitFramacTerm collector t;
    !preds
  in
  List.fold_left (fun x y -> f y x) default (collect t)

let clear () = Guards.clear ()
