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

open Cabs

let nextident = ref 0
let getident () =
  nextident := !nextident + 1;
  !nextident

let cabslu = Fileloc.unknown

module Comments =
struct
  module MapDest = struct
    include Datatype.List(Datatype.Pair(Filepos)(Datatype.String))
    let fast_equal (_:t) (_:t) = false
  end
  module MyTable =
    Rangemap.Make
      (Filepos)
      (MapDest)
  module MyState =
    State_builder.Ref
      (MyTable)
      (struct
        let name = "Cabshelper.Comments"
        let dependencies = [ ]
        (* depends from File.self and Ast.self which add
           the dependency themselves. *)
        let default () = MyTable.empty
      end)
  let self = MyState.self

  (* What matters is the beginning of the comment. *)
  let add (first,last) comment =
    let state = MyState.get () in
    let acc = try MyTable.find first state with Not_found -> [] in
    MyState.set ((MyTable.add first ((last,comment)::acc)) state)

  let get (first,last) =
    Kernel.debug ~dkey:Kernel.dkey_comments
      "Searching for comments between positions %a and %a@."
      Filepos.pretty first Filepos.pretty last;
    if not (Filepos.is_known first) || not (Filepos.is_known last)
    then begin
      Kernel.debug ~dkey:Kernel.dkey_comments "skipping dummy position@.";
      []
    end else
      let r = MyTable.fold_range
          (fun pos ->
             match Filepos.compare first pos with
             | n when n > 0 -> Rangemap.Below
             | 0 -> Rangemap.Match
             | _ ->
               if Filepos.compare pos last <= 0 then
                 Rangemap.Match
               else
                 Rangemap.Above)
          (fun _ comments acc -> acc @ List.rev_map snd comments)
          (MyState.get ())
          []
      in
      Kernel.debug ~dkey:Kernel.dkey_comments "%d results@." (List.length r);
      r

  let iter f =
    MyTable.iter
      (fun first comments ->
         List.iter (fun (last,comment) -> f (first,last) comment) comments)
      (MyState.get())

  let fold f acc =
    MyTable.fold
      (fun first comments acc ->
         List.fold_left
           (fun acc (last,comment) -> f (first,last) comment acc) acc comments)
      (MyState.get()) acc

end

(*********** HELPER FUNCTIONS **********)

let missingFieldDecl loc = (Cil.missingFieldName, JUSTBASE, [], loc)

let rec isStatic = function
    [] -> false
  | (SpecStorage STATIC) :: _ -> true
  | _ :: rest -> isStatic rest

let rec isExtern = function
    [] -> false
  | (SpecStorage EXTERN) :: _ -> true
  | _ :: rest -> isExtern rest

let rec isInline = function
    [] -> false
  | SpecInline :: _ -> true
  | _ :: rest -> isInline rest

let rec isTypedef = function
    [] -> false
  | SpecTypedef :: _ -> true
  | _ :: rest -> isTypedef rest


let get_definitionloc (d : definition) : cabsloc =
  match d with
  | FUNDEF(_,_, _, l, _) -> l
  | DECDEF(_,_, l) -> l
  | TYPEDEF(_, l) -> l
  | ONLYTYPEDEF(_, l) -> l
  | GLOBASM(_, l) -> l
  | PRAGMA(_, l) -> l
  | STATIC_ASSERT (_, _, l) -> l
  | LINKAGE (_, l, _) -> l
  | GLOBANNOT({Logic_ptree.decl_loc = l }::_) -> l
  | GLOBANNOT [] -> assert false

let get_statementloc (s : statement) : cabsloc =
  begin
    match s.stmt_node with
    | NOP(_, loc) -> loc
    | COMPUTATION(_,loc) -> loc
    | BLOCK(_,loc,_) -> loc
    | IF(_,_,_,loc) -> loc
    | WHILE(_,_,_,loc) -> loc
    | DOWHILE(_,_,_,loc) -> loc
    | FOR(_,_,_,_,_,loc) -> loc
    | BREAK(loc) -> loc
    | CONTINUE(loc) -> loc
    | RETURN(_,loc) -> loc
    | SWITCH(_,_,loc) -> loc
    | CASE(_,_,loc) -> loc
    | CASERANGE(_,_,_,loc) -> loc
    | DEFAULT(_,loc) -> loc
    | LABEL(_,_,loc) -> loc
    | GOTO(_,loc) -> loc
    | COMPGOTO (_, loc) -> loc
    | DEFINITION d -> get_definitionloc d
    | ASM(_,_,_,loc) -> loc
    | (CODE_SPEC (_,l) |CODE_ANNOT (_,l)) -> l
    | THROW(_,l) -> l
    | TRY_CATCH(_,_,l) -> l
  end


let explodeStringToInts (s: string) : int64 list =
  let rec allChars i acc =
    if i < 0 then acc
    else allChars (i - 1) (Int64.of_int (Char.code (String.get s i)) :: acc)
  in
  allChars (-1 + String.length s) []

let valueOfDigit chr =
  let int_value =
    match chr with
    '0'..'9' -> (Char.code chr) - (Char.code '0')
    | 'a'..'z' -> (Char.code chr) - (Char.code 'a') + 10
    | 'A'..'Z' -> (Char.code chr) - (Char.code 'A') + 10
    | _ -> Kernel.fatal "not a digit"
  in
  Int64.of_int int_value


let d_cabsloc fmt cl =
  Format.fprintf fmt "%a" Filepos.pretty (fst cl)

type attr_test = Normal | Test
let state_stack = Stack.create ()
let () = Stack.push Normal state_stack
let push_attr_test () = Stack.push Test state_stack
let pop_attr_test () = ignore (Stack.pop state_stack)
let is_attr_test () = Stack.top state_stack = Test

let mk_behavior ?(name=Cil.default_behavior_name) ?(assumes=[]) ?(requires=[])
    ?(post_cond=[]) ?(assigns=Logic_ptree.WritesAny) ?(allocation=Logic_ptree.FreeAllocAny)  ?(extended=[]) ()
  =
  { Logic_ptree.b_name = name;
    b_assumes = assumes; (* must be always empty for default_behavior_name *)
    b_requires = requires;
    b_assigns = assigns ;
    b_allocation = allocation ;
    b_post_cond = post_cond ;
    b_extended = extended;
  }

let mk_asm_templates =
  let buf = Buffer.create 100 in
  let rec outer res = function
    | [] when res = [] && Buffer.length buf = 0 -> [""]
    | [] when Buffer.length buf = 0 -> List.rev res
    | [] ->
      let res = List.rev @@ Buffer.contents buf :: res in
      Buffer.clear buf;
      res
    | str :: tail -> tail |> outer @@ inner res str 0
  and inner res template i =
    if i < String.length template then
      let c = String.get template i in
      Buffer.add_char buf c;
      if c = '\n' then
        if i < String.length template - 1 then
          match String.get template @@ i + 1 with
          | '\t' ->
            Buffer.add_char buf '\t';
            let res = Buffer.contents buf :: res in
            Buffer.clear buf;
            inner res template @@ i + 2
          | c ->
            let res = Buffer.contents buf :: res in
            Buffer.clear buf;
            Buffer.add_char buf c;
            inner res template @@ i + 2
        else
          let res = Buffer.contents buf :: res in
          Buffer.clear buf;
          res
      else inner res template @@ i + 1
    else res in
  outer []
