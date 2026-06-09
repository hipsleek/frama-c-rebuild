(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Datatypes for analyses types *)

open Cil_types
open Cil_datatype
open Analyses_types

module Annotation_kind =
  Datatype.Make
    (struct
      type t = annotation_kind
      let name = "E_ACSL.Annotation_kind"
      let reprs = [ Assertion ]
      include Datatype.Undefined

      let pretty fmt akind =
        match akind with
        | Assertion -> Format.fprintf fmt "Assertion"
        | Precondition -> Format.fprintf fmt "Precondition"
        | Postcondition -> Format.fprintf fmt "Postcondition"
        | Invariant -> Format.fprintf fmt "Invariant"
        | Variant -> Format.fprintf fmt "Variant"
        | RTE -> Format.fprintf fmt "RTE"
    end)

module Pred_or_term =
  Datatype.Make_with_collections
    (struct
      type t = pred_or_term

      let name = "E_ACSL.Pred_or_term"

      let reprs =
        let reprs =
          List.fold_left
            (fun reprs t -> PoT_term t :: reprs)
            []
            Misc.Id_term.reprs
        in
        List.fold_left
          (fun reprs p -> PoT_pred p :: reprs)
          reprs
          Predicate.reprs

      include Datatype.Undefined

      let compare _ _ =
        (* see [Misc.Id_term.compare] *)
        Kernel.fatal "Pred_or_term: comparison undefined (and undefinable)"

      let equal pot1 pot2 =
        match pot1, pot2 with
        | PoT_pred p1, PoT_pred p2 -> PredicateStructEq.equal p1 p2
        | PoT_term t1, PoT_term t2 -> Misc.Id_term.equal t1 t2
        | _ -> false

      let hash = function
        | PoT_pred p -> 7 * PredicateStructEq.hash p
        | PoT_term t -> 97 * Misc.Id_term.hash t

      let pretty fmt = function
        | PoT_pred p -> Printer.pp_predicate fmt p
        | PoT_term t -> Printer.pp_term fmt t


    end)

(** [Ext_logic_label] associates a statement to a label when necessary. For
    instance, the label `Old` is associated with its contract statement to
    distinguish two `Old` annotations in the same function. On the contrary, the
    `Pre` label does not have an associated statement because this label
    represents the same location for all contracts in the same function. *)
module Ext_logic_label: sig
  include Datatype.S_with_collections with type t = logic_label * stmt option

  val get: kinstr -> logic_label -> logic_label * stmt option
  (** @return an extended logic label from a [kinstr] and a [logic_label]. *)

end = struct

  include Datatype.Pair_with_collections
      (Logic_label)
      (Datatype.Option_with_collections
         (Stmt))

  (* Override [pretty] to print a more compact representation of
     [Ext_logic_label] for debugging purposes. *)
  let pretty fmt (label, from_stmt_opt) =
    match from_stmt_opt with
    | Some from_stmt ->
      Format.fprintf fmt "%a from stmt %d at %a"
        Logic_label.pretty label
        from_stmt.sid
        Fileloc.pretty (Stmt.loc from_stmt)
    | None ->
      Format.fprintf fmt "%a"
        Logic_label.pretty label

  let get kinstr label =
    let from_stmt_opt =
      match kinstr, label with
      | Kglobal, _
      | Kstmt _, (BuiltinLabel (Pre | Here | Init)
                 | FormalLabel _ | StmtLabel _) ->
        None
      | Kstmt _, BuiltinLabel (LoopCurrent | LoopEntry) ->
        (* [None] for now because these labels are unsupported, but the
           statement before the loop and the first statement of the loop should
           probably be used once they are supported. *)
        Error.print_not_yet
          (Format.asprintf "label %a" Printer.pp_logic_label label);
        None
      | Kstmt s, BuiltinLabel (Old | Post) -> Some s
    in
    label, from_stmt_opt

end

(** Basic printer for a [kinstr]. Contrary to [Cil_datatype.Kinstr.pretty], the
    stmt of [Kstmt] is not printed. *)
let basic_pp_kinstr fmt kinstr =
  Format.fprintf fmt "%s"
    (match kinstr with
     | Kglobal -> "Kglobal"
     | Kstmt _ -> "Kstmt")

(** Basic comparison for two [kinstr], i.e. two [Kstmt] are always equal
    regardless of the statement value (contrary to [Cil_datatype.Kinstr.compare]
    where two [Kstmt] are compared with their included statement's [sid]). *)
let basic_kinstr_equal kinstr1 kinstr2 =
  match kinstr1, kinstr2 with
  | Kglobal, Kglobal | Kstmt _, Kstmt _ -> true
  | _ -> false

(** Basic hash function for a [kinstr], i.e. contrary to
    [Cil_datatype.Kinstr.hash] the statement of the [Kstmt] is not considered
    for the hash. *)
let basic_kinstr_hash kinstr =
  match kinstr with
  | Kglobal -> 1 lsl 29
  | Kstmt _ -> 1 lsl 31

module At_data = struct
  let create ?error kf kinstr lscope pot label =
    { kf; kinstr; lscope; pot; label; error }

  include Datatype.Make_with_collections
      (struct
        type t = at_data
        let name = "E_ACSL.At_data"

        let reprs =
          List.fold_left
            (fun acc kf ->
               List.fold_left
                 (fun acc kinstr ->
                    List.fold_left
                      (fun acc pot ->
                         List.fold_left
                           (fun acc label ->
                              create kf kinstr Lscope.empty pot label :: acc)
                           acc
                           Logic_label.reprs)
                      acc
                      Pred_or_term.reprs)
                 acc
                 Kinstr.reprs)
            []
            Kf.reprs

        include Datatype.Undefined

        let compare _ _ =
          (* see [Misc.Id_term.compare] *)
          Kernel.fatal "At_data: comparison undefined (and undefinable)"

        let equal
            {kf = kf1; kinstr = kinstr1; lscope = ls1; pot = pot1; label = l1}
            {kf = kf2; kinstr = kinstr2; lscope = ls2; pot = pot2; label = l2} =
          Kf.equal kf1 kf2 &&
          basic_kinstr_equal kinstr1 kinstr2 &&
          Lscope.D.equal ls1 ls2 &&
          Pred_or_term.equal pot1 pot2 &&
          let elabel1 = Ext_logic_label.get kinstr1 l1 in
          let elabel2 = Ext_logic_label.get kinstr2 l2 in
          Ext_logic_label.equal elabel1 elabel2

        let hash { kf; kinstr; lscope; pot; label } =
          let elabel = Ext_logic_label.get kinstr label in
          Hashtbl.hash
            (Kf.hash kf,
             basic_kinstr_hash kinstr,
             Lscope.D.hash lscope,
             Pred_or_term.hash pot,
             Ext_logic_label.hash elabel)

        let pretty fmt { kf; kinstr; lscope; pot; label } =
          let elabel = Ext_logic_label.get kinstr label in
          Format.fprintf fmt "@[(%a, %a, %a, %a, %a)@]"
            Kf.pretty kf
            basic_pp_kinstr kinstr
            Lscope.D.pretty lscope
            Pred_or_term.pretty pot
            Ext_logic_label.pretty elabel
      end)
end

module Ival_datatype =
struct
  include
    Datatype.Make_with_collections
      (struct
        type t = ival
        let name = "E_ACSL.Interval.t"
        let reprs = [ Float (FFloat, Some 0.); Rational; Real; Nan ]
        include Datatype.Undefined

        let compare i1 i2 =
          if i1 == i2 then 0
          else
            match i1, i2 with
            | Ival i1, Ival i2 ->
              Ival.compare i1 i2
            | Float (k1, f1), Float (k2, f2) ->
              (* faster to compare a kind than a float *)
              let n = Stdlib.compare k1 k2 in
              if n = 0 then Stdlib.compare f1 f2 else n
            | Ival _, (Float _ | Rational | Real | Nan)
            | Float _, (Rational | Real | Nan)
            | Rational, (Real | Nan)
            | Real, Nan ->
              -1
            | Nan, (Ival _ | Float _ | Rational | Real)
            | Real, (Ival _ | Float _ | Rational)
            | Rational, (Ival _ | Float _)
            | Float _, Ival _ ->
              1
            | Rational, Rational | Real, Real | Nan, Nan ->
              assert false

        let equal = Datatype.from_compare

        let hash = function
          | Ival i -> 7 * Ival.hash i
          | Float(k, f) -> 17 * Hashtbl.hash f + 97 * Hashtbl.hash k
          | Rational -> 787
          | Real -> 1011
          | Nan -> 1277

        let pretty fmt = function
          | Ival i -> Ival.pretty fmt i
          | Float(_, Some f) -> Format.pp_print_float fmt f
          | Float(fk, None) -> Printer.pp_fkind fmt fk
          | Rational -> Format.pp_print_string fmt "Rational"
          | Real -> Format.pp_print_string fmt "Real"
          | Nan -> Format.pp_print_string fmt "NaN"

      end)

  let is_included i1 i2 =
    match i1, i2 with
    | Ival i1, Ival i2 -> Ival.is_included i1 i2
    | Float(_k1, _f1), Float(_k2, _f2) -> assert false
    | Rational, (Rational | Real)
    | Real, Real
    | Nan, Nan -> true
    | Ival _, (Float _ | Rational | Real | Nan)
    | Float _, (Ival _ | Rational | Real | Nan)
    | Rational, (Ival _ | Float _ | Nan)
    | Real, (Ival _ | Float _ | Rational | Nan)
    | Nan, (Ival _ | Float _ | Rational | Real) -> false
end
(* Profiles of functions are the interval ranges of their arguments. For
   memoization purposes, we need need them as keys of hashtables, even though
   they are implemented as maps. Functions typically do not have many arguments
   so it is acceptable to do so.*)
module Profile =
struct
  let rec make args ival =
    match args,ival with
    | [],[] -> Logic_var.Map.empty
    | x::args, i::ival -> Logic_var.Map.add x i (make args ival)
    | [], _::_ | _::_, [] -> assert false

  include
    Datatype.Make_with_collections
      (struct
        include Datatype.Undefined

        type t = ival Logic_var.Map.t

        let equal = Logic_var.Map.equal Ival_datatype.equal
        let compare = Logic_var.Map.compare Ival_datatype.compare

        let mem_project = Datatype.never_any_project
        let copy m =  Logic_var.Map.fold Logic_var.Map.add m Logic_var.Map.empty
        let hash m =
          Logic_var.Map.fold
            (fun v i h -> h + Logic_var.hash v + Ival_datatype.hash i)
            m
            0
        let reprs =
          let v = List.hd Logic_var.reprs in
          let i = List.hd Ival_datatype.reprs in
          [ Logic_var.Map.add v i Logic_var.Map.empty ]
        let structural_descr = Structural_descr.t_abstract
        let rehash = Datatype.identity
        let name = "E-ACSL.Profile"

        let pretty fmt m =
          let first = ref true in
          let pp_vi v i =
            if !first
            then first := false
            else Format.fprintf fmt " ";
            Format.fprintf fmt "%a:%a"
              Logic_var.pretty v Analyses_types.pp_ival i
          in
          Logic_var.Map.iter pp_vi  m

      end)

  let is_empty = Logic_var.Map.is_empty

  let empty = Logic_var.Map.empty

  let is_included p q =
    Logic_var.Map.for_all
      (fun lv ival_p ->
         match Logic_var.Map.find_opt lv q with
         | None -> false
         | Some ival_q -> Ival_datatype.is_included ival_p ival_q)
      p
end

module Id_term_in_profile =
  Datatype.With_hashtbl
    (Datatype.Pair
       (Misc.Id_term)
       (Profile))

(* Environment to handle recursive functions: this environment stores the logic
   functions that we have already started inferring along with their
   profiles. This is necessary for the fixpoint algorithm. *)
module LFProf =
  Datatype.Pair_with_collections (Cil_datatype.Logic_info) (Profile)

module Logic_env
= struct
  type t = { profile : Profile.t;
             let_quantif_bind : Profile.t}

  (* forward reference to meet of intervals *)
  let ival_meet_ref
    : (ival -> ival -> ival) ref
    = ref (fun _i1 _i2 -> Extlib.mk_labeled_fun "ival_meet_ref")

  let add env x i =
    { env with let_quantif_bind = Logic_var.Map.add  x i env.let_quantif_bind}

  let empty =
    {profile = Logic_var.Map.empty;
     let_quantif_bind = Logic_var.Map.empty}

  let make profile =
    { profile = profile;
      let_quantif_bind = Logic_var.Map.empty }

  let find env x =
    try Logic_var.Map.find x env.let_quantif_bind
    with Not_found ->
      Logic_var.Map.find x env.profile

  let get_profile env = env.profile

  let refine env x ival =
    let update = function
      | None -> raise Not_found
      | Some i -> Some (!ival_meet_ref i ival)
    in
    let new_lq_bind =
      try Logic_var.Map.update x update env.let_quantif_bind
      with Not_found ->
      match Logic_var.Map.find_opt x env.profile with
      | Some i ->
        (* The profile must remain unchanged, so if the variable is bound in
           the profile, we add the refined interval in the other bindings,
           which are checked first when finding the interval *)
        Logic_var.Map.add x (!ival_meet_ref i ival) env.let_quantif_bind
      | None -> env.let_quantif_bind
    in
    {env with let_quantif_bind = new_lq_bind}

  let pretty fmt {profile; let_quantif_bind} =
    Format.fprintf fmt "@[profile: %a@]@[<v 2>let_quantif_bind: %a@]"
      Profile.pretty profile
      Profile.pretty let_quantif_bind
end

(* Imperative environment to perform fixpoint algorithm for recursive
   functions. This environment store four pieces of information associated
   to every logic_info:
   - the current profile in which the interval for the logic_info is inferred.
   - the current interval that it is inferred to.
   - a map associating to each parameter all the arguments in their profiles
     that this parameter has been called with up until now.
   - the depth of calls to the fixpoint algorithm associated to the logic_info.
     When this depth reaches 0, the entry corresponding to the logic_info is
     cleared thus avoiding unification between two independent calls to the same
     logic function, which is unsafe.

   The third argument is used so that whenever a parameter of a logic_info is
   unified by widening with a new value, the interval inference algorithm
   updates all the arguments that this parameter have been called with, to
   assign the new interval to it *)
module LF_env
= struct

  let tbl = Logic_info.Hashtbl.create 17

  let clear () = Logic_info.Hashtbl.clear tbl

  let find_profile li =
    let profile, _, _, _ = Logic_info.Hashtbl.find tbl li in profile

  let find_ival li =
    let _, ival, _, _ = Logic_info.Hashtbl.find tbl li in ival

  let find_profile_ival li =
    let profile, ival, _, _ = Logic_info.Hashtbl.find tbl li in profile, ival

  let find_args li =
    let _, _, args, _ = Logic_info.Hashtbl.find tbl li in args

  let build_map ~profile li args map =
    List.fold_left2
      (fun map lv t ->
         Logic_var.Map.update
           lv
           (fun m -> match m with
              | Some tbl ->
                Misc.Id_term.Hashtbl.replace tbl t profile; Some tbl
              | None ->
                let tbl = Misc.Id_term.Hashtbl.create 9 in
                Misc.Id_term.Hashtbl.add tbl t profile;
                Some tbl)
           map)
      map
      li.l_profile
      args

  let add ~logic_env li args_ival ival args =
    let profile = Logic_env.get_profile logic_env in
    let map, n =
      match Logic_info.Hashtbl.find_opt tbl li with
      | Some (_,_,map,n) -> map, n
      | None -> Logic_var.Map.empty, 0
    in
    let map = build_map ~profile li args map in
    Logic_info.Hashtbl.replace tbl li (args_ival, ival, map, n+1)

  let update_ival li ival =
    let profile, _, args_list, n = Logic_info.Hashtbl.find tbl li in
    Logic_info.Hashtbl.replace tbl li (profile, ival, args_list, n + 1)

  let decrease li =
    let profile, ival, args_list, n = Logic_info.Hashtbl.find tbl li in
    if(n - 1 <= 0) then
      Logic_info.Hashtbl.remove tbl li
    else
      Logic_info.Hashtbl.replace tbl li (profile, ival, args_list, n - 1)

  exception Recursive

  let contain li = object
    inherit Visitor.frama_c_inplace

    method! vpredicate p =
      match p.pred_content with
      | Papp (li_app,_,_) when Cil_datatype.Logic_info.equal li li_app ->
        raise Recursive;
      | _ -> Cil.DoChildren

    method! vterm t =
      match t.term_node with
      | Tapp(li_app,_,_) when Cil_datatype.Logic_info.equal li li_app ->
        raise Recursive
      | _ -> Cil.DoChildren

    method! vlogic_type _ = Cil.SkipChildren
  end

  let is_rec li =
    match li.l_body with
    | LBpred p ->
      (try ignore (Visitor.visitFramacPredicate (contain li) p); false
       with Recursive -> true)
    | LBterm t ->
      (try ignore (Visitor.visitFramacTerm (contain li) t); false
       with Recursive -> true)
    | LBreads _ | LBnone -> false
    | LBinductive _ -> Error.not_yet "Inductive"

end

module Number_ty =
  Datatype.Make_with_collections
    (struct
      type t = number_ty
      let name = "E_ACSL.Typing.t"
      let reprs = [ Gmpz; Real; Nan; C_integer IInt ]
      include Datatype.Undefined

      let compare ty1 ty2 =
        if ty1 == ty2 then 0
        else
          match ty1, ty2 with
          | C_integer i1, C_integer i2 ->
            if i1 = i2 then 0
            else if Cil.intTypeIncluded i1 i2 then -1 else 1
          | C_float f1, C_float f2 ->
            Stdlib.compare f1 f2
          | (C_integer _ | C_float _ | Gmpz | Rational | Real), Nan
          | (C_integer _ | C_float _ | Gmpz | Rational ), Real
          | (C_integer _ | C_float _ | Gmpz), Rational
          | (C_integer _ | C_float _), Gmpz
          | C_integer _, C_float _ ->
            -1
          | (C_float _ | Gmpz | Rational | Real | Nan), C_integer _
          | (Gmpz | Rational | Real | Nan), C_float _
          | (Rational | Real | Nan), Gmpz
          | (Real | Nan), Rational
          | Nan, Real ->
            1
          | Gmpz, Gmpz
          | Rational, Rational
          | Real, Real
          | Nan, Nan ->
            assert false

      let equal = Datatype.from_compare

      let hash = function
        | C_integer ik -> 7 * Hashtbl.hash ik
        | C_float fk -> 97 * Hashtbl.hash fk
        | Gmpz -> 787
        | Rational -> 907
        | Real -> 1011
        | Nan -> 1277

      let pretty fmt = function
        | C_integer k -> Printer.pp_ikind fmt k
        | C_float k -> Printer.pp_fkind fmt k
        | Gmpz -> Format.pp_print_string fmt "Gmpz"
        | Rational -> Format.pp_print_string fmt "Rational"
        | Real -> Format.pp_print_string fmt "Real"
        | Nan -> Format.pp_print_string fmt "Nan"
    end)
