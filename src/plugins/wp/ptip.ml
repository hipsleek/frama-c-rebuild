(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Conditions
open Lang.F
module F = Lang.F
module Env = Plang.Env
module Imap = Qed.Intmap
type 'a printer = Format.formatter -> 'a -> unit

type v_fold = [ `Auto | `Visible | `Hidden ]
type v_term = [ v_fold | `Shared | `Name of string ]

type part = Term | Goal | Step of step

[@@@ warning "-32"]

let pp_part fmt = function
  | Term -> Format.fprintf fmt "Term"
  | Goal -> Format.fprintf fmt "Goal"
  | Step s -> Format.fprintf fmt "Step #%d" s.id

let pp_term fmt e = Format.fprintf fmt "E%03d" (F.QED.id e)

let pp_target fmt = function
  | None -> Format.pp_print_string fmt "-"
  | Some e -> Format.fprintf fmt "T%03d" (F.QED.id e)

let pp_fold fmt u = Format.pp_print_string fmt
    ( match u with `Auto -> "auto" | `Fold -> "fold" | `Unfold -> "unfold" )

[@@@ warning "+32"]

(* -------------------------------------------------------------------------- *)
(* --- Focus                                                              --- *)
(* -------------------------------------------------------------------------- *)

class autofocus =
  object(self)

    val mutable autofocus = true

    (* Term Visibility (forced by user) *)
    val mutable vterm : v_term Tmap.t = Tmap.empty

    (* Step Visibility (forced by user) *)
    val mutable vstep : v_fold Imap.t = Imap.empty

    (* Focused Terms ; lastly selected at head *)
    val mutable focusring = []
    val mutable target = F.e_true

    (* Memoization of focused terms and steps occurrence *)
    val mutable occurs_term : bool Tmap.t = Tmap.empty
    val mutable occurs_step : bool Imap.t = Imap.empty

    (* Currently displayed sequent *)
    val mutable sequent : Conditions.sequent option = None

    method clear =
      begin
        sequent <- None ;
        self#reset ;
      end

    method reset =
      begin
        focusring <- [] ;
        vterm <- Tmap.empty ;
        vstep <- Imap.empty ;
        self#clear_cache ;
      end

    method private clear_cache =
      begin
        occurs_term <- Tmap.empty ;
        occurs_step <- Imap.empty ;
      end

    method private clear_steps =
      occurs_step <- Imap.empty

    (* --- Environment --- *)

    method env =
      let env = Env.create () in
      Tmap.iter
        (fun t v -> match v with
           | `Auto -> ()
           | `Hidden -> Env.define env "..." t
           | `Visible -> Env.unfold env t
           | `Shared ->
             let base = F.basename t in
             let sanitizer = Plang.sanitizer in
             Env.define env (Env.fresh env ~sanitizer base) t
           | `Name x ->
             Env.define env x t)
        vterm ; env

    (* --- Term Occurrence --- *)

    method private occurs_term e =
      try Tmap.find e occurs_term
      with Not_found ->
        let occurs =
          try
            if List.memq e focusring then raise Exit ;
            if e != F.e_true && e == target then raise Exit ;
            F.lc_iter (fun e -> if self#occurs_term e then raise Exit) e ;
            false
          with Exit -> true
        in occurs_term <- Tmap.add e occurs occurs_term ; occurs

    method private occurs_seq seq =
      try
        Conditions.iter
          (fun s -> if self#occurs_step s then raise Exit) seq ;
        false
      with Exit -> true

    method private occurs_state s =
      try
        Mstate.iter
          (fun _m v -> if self#occurs_term v then raise Exit) s ;
        false
      with Exit -> true

    method private occurs_step step =
      try step.id < 0 (* defensive *) || Imap.find step.id occurs_step
      with Not_found ->
        let occurs =
          match step.condition with
          | When _ -> true
          | State s -> self#occurs_state s
          | Probe(_,t) -> self#occurs_term t
          | Init p | Have p | Type p | Core p ->
            self#occurs_term (F.e_prop p)
          | Branch(p,sa,sb) ->
            self#occurs_term (F.e_prop p)
            || self#occurs_seq sa
            || self#occurs_seq sb
          | Either cs ->
            List.exists self#occurs_seq cs
        in occurs_step <- Imap.add step.id occurs occurs_step ; occurs

    (* --- Term Visibility --- *)

    method set_term t = function
      | `Auto ->
        if Tmap.mem t vterm then
          (vterm <- Tmap.remove t vterm ; self#clear_cache)
      | v ->
        let same =
          try v = Tmap.find t vterm
          with Not_found -> false in
        if not same then
          (vterm <- Tmap.add t v vterm ; self#clear_cache)

    method get_term t = try Tmap.find t vterm with Not_found -> `Auto

    method set_target e = target <- e
    method clear_target = target <- F.e_true

    method focus ~extend e =
      if F.lc_closed e then
        begin
          let ring = if extend
            then (List.filter (fun e0 -> e0 != e) focusring)
            else [] in
          focusring <- e :: ring ;
          self#clear_cache ;
        end

    method unfocus e =
      begin
        focusring <- List.filter (fun e0 -> e0 != e) focusring ;
        self#clear_cache ;
      end

    method unfocus_last =
      begin match focusring with
        | [] -> ()
        | _::es -> focusring <- es ; self#clear_cache
      end

    method is_selected e = match focusring with e0::_ -> e0 == e | [] -> false
    method is_focused e = List.memq e focusring
    method is_visible e = if autofocus then self#occurs_term e else true
    method is_targeted e = autofocus && self#occurs_term e

    method set_autofocus flag =
      autofocus <- flag ;
      if flag then self#clear_cache else self#reset

    method get_autofocus = autofocus

    method is_autofocused =
      autofocus && Tmap.is_empty vterm

    (* --- Hypotheses Management --- *)

    method set_step s = function
      | `Auto ->
        if Imap.mem s.id vstep then
          (vstep <- Imap.remove s.id vstep ; self#clear_steps)
      | v ->
        let same =
          try v = Imap.find s.id vstep
          with Not_found -> false in
        if not same then
          (vstep <- Imap.add s.id v vstep ; self#clear_steps)

    method get_step s =
      try Imap.find s.id vstep
      with Not_found -> `Auto

    method is_visible_step (s : step) =
      match self#get_step s with
      | `Auto -> if autofocus then self#occurs_step s else true
      | `Visible -> true
      | `Hidden -> false

    method locate a =
      match sequent with
      | None -> Tactical.Empty
      | Some (hs,goal) ->
        if F.is_subterm a (F.e_prop goal)
        then Tactical.(Inside(Goal goal,a))
        else
          let pool = ref Tactical.Empty in
          let lookup_term s a t =
            if F.is_subterm a t then
              begin
                pool := Tactical.(Inside(Step s,a));
                raise Exit;
              end in
          let lookup_pred s a p = lookup_term s a (F.e_prop p) in
          let rec lookup_sequence a hs =
            (*TODO: staged iter *)
            Conditions.iter
              (fun step ->
                 match step.condition with
                 | (Type p | Init p | Have p | When p | Core p)
                   -> lookup_pred step a p
                 | Probe(_,t) -> lookup_term step a t
                 | Branch(p,sa,sb) ->
                   lookup_pred step a p ;
                   lookup_sequence a sa ;
                   lookup_sequence a sb ;
                 | Either cs ->
                   List.iter (lookup_sequence a) cs
                 | State _ -> ()
              ) hs in
          (try lookup_sequence a hs with Exit -> ()) ;
          !pool

    (* ---- Global ----- *)

    method set_sequent (s : sequent) =
      let updated = match sequent with None -> true | Some s0 -> s0 != s in
      if updated then
        begin
          sequent <- Some s ;
          Conditions.index s ;
          vstep <- Imap.empty ;
          self#clear_cache ;
        end ;
      updated

  end

(* -------------------------------------------------------------------------- *)
(* --- Term Engine                                                        --- *)
(* -------------------------------------------------------------------------- *)

class type term_wrapper =
  object
    method wrap : term printer -> term printer
  end

class type term_selection =
  object
    method is_focused : term -> bool
    method is_visible : term -> bool
    method is_targeted : term -> bool
  end

class plang
    ~(terms : #term_wrapper)
    ~(focus : #term_wrapper)
    ~(target : #term_wrapper)
    ~(autofocus : #term_selection)
  =
  object(self)
    inherit Pcond.state as super

    method! shareable e = autofocus#is_targeted e || super#shareable e

    val mutable tgt = F.e_true
    method set_target t = tgt <- t
    method clear_target = tgt <- F.e_true

    method private wrap pp fmt e =
      if e != F.e_true && e == tgt then
        target#wrap pp fmt e
      else
      if autofocus#is_focused e then
        focus#wrap pp fmt e
      else
      if F.lc_closed e then
        terms#wrap pp fmt e
      else
        pp fmt e

    method! pp_at fmt lbl =
      Format.fprintf fmt "@{<wp:label>@@%a@}" super#pp_label lbl
    method! pp_label fmt lbl =
      Format.fprintf fmt "@{<wp:label>%a@}" super#pp_label lbl
    method! pp_var fmt x =
      Format.fprintf fmt "@{<wp:var>%s@}" x
    method! pp_flow fmt e = self#wrap super#pp_flow fmt e
    method! pp_atom fmt e = self#wrap super#pp_atom fmt e
  end

(* -------------------------------------------------------------------------- *)
(* --- Condition Engine                                                   --- *)
(* -------------------------------------------------------------------------- *)

class type part_marker =
  object
    method wrap : part printer -> part printer
    method mark : 'a. part -> 'a printer -> 'a printer
  end

class type step_selection =
  object
    method is_visible : F.term -> bool
    method is_visible_step : step -> bool
  end

class pcond
    ~(parts : #part_marker)
    ~(target : #part_marker)
    ~(autofocus : #step_selection)
    ~(plang : #Pcond.state) =
  object(self)
    inherit Pcond.seqengine plang as super

    (* All displayed entries *)
    val mutable domain = Vars.empty
    val mutable ellipsed = false
    val mutable tgt : part = Term (* empty *)

    method set_target p = tgt <- p

    (* Step Visibility Management *)

    method visible step =
      autofocus#is_visible_step step ||
      match tgt with
      | Term | Goal -> false
      | Step s -> s.id = step.id

    method private domain seq =
      Conditions.iter
        (fun step ->
           if self#visible step && not (Vars.subset step.vars domain)
           then
             begin
               match step.condition with
               | State _ -> ()
               | Probe(_,t) ->
                 domain <- Vars.union (F.vars t) domain
               | Have p | Init p | Core p | When p | Type p ->
                 domain <- Vars.union (F.varsp p) domain
               | Branch(p,a,b) ->
                 domain <- Vars.union (F.varsp p) domain ;
                 self#domain a ; self#domain b
               | Either cs -> List.iter self#domain cs
             end
        ) seq

    (* local-variable marking ; not hover/clickable marks *)
    method! mark (m : F.marks) s = if self#visible s then super#mark m s

    method! pp_step fmt step =
      if self#visible step then
        begin
          ellipsed <- false ;
          match tgt with
          | Step { condition = State _ } -> super#pp_step fmt step
          | Step s when s == step ->
            target#mark (Step step) super#pp_step fmt step
          | _ ->
            parts#mark (Step step) super#pp_step fmt step
        end
      else
        ( if not ellipsed then Format.fprintf fmt "@ [...]" ; ellipsed <- true )

    method! pp_goal fmt goal =
      match tgt with
      | Goal ->
        target#mark Goal super#pp_goal fmt goal
      | _ ->
        parts#mark Goal super#pp_goal fmt goal

    method! pp_block ~clause fmt seq =
      try
        Conditions.iter
          (fun step ->
             if self#visible step then
               raise Exit)
          seq ;
        Format.fprintf fmt "@ %a { ... }" self#pp_clause clause
      with Exit ->
        begin
          ellipsed <- false ;
          super#pp_block ~clause fmt seq ;
          ellipsed <- false ;
        end

    (* Global Call *)

    method! set_sequence hyps =
      domain <- Vars.empty ;
      super#set_sequence hyps ;
      if self#get_state then
        begin
          self#domain hyps ;
          plang#set_domain domain ;
        end

  end

(* -------------------------------------------------------------------------- *)
(* --- Sequent Engine                                                     --- *)
(* -------------------------------------------------------------------------- *)

type target = part * F.term option
type focus = [ `Transient | `Select | `Focus | `Extend | `Reset ]

class pseq
    ~(autofocus:#autofocus)
    ~(plang:#plang)
    ~(pcond:#pcond) =
  object(self)
    val mutable demon = []
    val mutable sequent = Conditions.empty , F.p_true
    val mutable selected_term = None
    val mutable selected_part = Term

    method reset =
      selected_term <- None ;
      selected_part <- Term ;
      autofocus#reset

    method get_focus_mode = autofocus#get_autofocus
    method set_focus_mode = autofocus#set_autofocus

    method get_ce_mode = pcond#get_ce_mode
    method set_ce_mode ce = pcond#set_ce_mode ce

    method update_ce_models (po: Wpo.t) =
      let models = Hashtbl.create 7 in
      List.iter
        begin fun (p, r) ->
          if not @@ Probe.Map.is_empty r.VCS.prover_model then
            Hashtbl.add models p r.VCS.prover_model
        end
        (Wpo.get_results po) ;
      pcond#update_ce_models models

    method get_state_mode = pcond#get_state
    method set_state_mode = pcond#set_state
    method set_unmangled m = pcond#set_state (not m)

    method set_iformat = plang#set_iformat
    method get_iformat = plang#get_iformat

    method set_rformat = plang#set_rformat
    method get_rformat = plang#get_rformat

    method selected =
      begin
        self#highlight self#selection ;
        List.iter (fun f -> f ()) demon ;
      end

    method on_selection f =
      demon <- demon @ [f]

    method private convert part term =
      let inside clause t =
        if F.p_bool t == Tactical.head clause
        then Tactical.(Clause clause)
        else Tactical.(Inside(clause,t))
      in
      match part , term with
      | Term , None -> Tactical.Empty
      | Goal , None -> Tactical.(Clause(Goal(snd sequent)))
      | Step s , None -> Tactical.(Clause(Step s))
      | Term , Some t -> autofocus#locate t
      | Goal , Some t -> inside Tactical.(Goal (snd sequent)) t
      | Step s , Some t -> inside Tactical.(Step s) t

    method target = selected_part, selected_term
    method resolve (p,t) = self#convert p t
    method selection = self#convert selected_part selected_term

    method unselect =
      begin
        let p = selected_part in selected_part <- Term ;
        let t = selected_term in selected_term <- None ;
        autofocus#unfocus_last ; p,t
      end

    method restore ~(focus:focus) (p,t) =
      begin
        selected_part <- p ;
        selected_term <- t ;
        let selected =
          match focus with
          | `Transient -> false
          | `Select -> true
          | `Focus -> Option.iter (autofocus#focus ~extend:false) t ; true
          | `Extend -> Option.iter (autofocus#focus ~extend:true) t ; false
          | `Reset -> autofocus#reset ; true
        in if selected then self#selected
      end

    method selection_to_target = function
      | Tactical.Empty | Tactical.Compose _ | Tactical.Multi _ ->
        Term, None
      | Tactical.Clause(Goal p) -> Goal, Some (F.e_prop p)
      | Tactical.Clause(Step s as clause) ->
        Step s, Some (F.e_prop @@ Tactical.head clause)
      | Tactical.Inside(Goal _,t) -> Goal, Some t
      | Tactical.Inside(Step s,t) -> Step s, Some t

    method set_selection sel =
      let current = self#selection in
      if not @@ Tactical.equal current sel then
        let target = self#selection_to_target sel in
        self#restore ~focus:`Focus target

    method highlight tgt =
      match tgt with
      | Tactical.Empty | Tactical.Compose _ | Tactical.Multi _ ->
        begin
          pcond#set_target Term ;
          plang#clear_target ;
          autofocus#clear_target ;
        end
      | Tactical.Inside (_,t) ->
        begin
          pcond#set_target Term ;
          plang#set_target t ;
          autofocus#set_target t ;
        end
      | Tactical.Clause (Tactical.Goal _) ->
        begin
          pcond#set_target Goal ;
          plang#clear_target ;
          autofocus#clear_target ;
        end
      | Tactical.Clause (Tactical.Step s) ->
        begin
          pcond#set_target (Step s) ;
          plang#clear_target ;
          autofocus#clear_target ;
        end

    method pp_term fmt e = plang#pp_sort fmt e
    method pp_pred fmt p = plang#pp_pred fmt p

    method pp_selection fmt = function
      | Tactical.Empty -> Format.fprintf fmt " - "
      | Tactical.Compose(Tactical.Range(a,b)) ->
        Format.fprintf fmt "%d..%d" a b
      | sel -> self#pp_term fmt (Tactical.selected sel)

    method sequent = sequent

    method pp_sequent fmt s =
      sequent <- s ;
      if autofocus#set_sequent s then
        begin
          selected_term <- None ;
          selected_part <- Term ;
        end ;
      let env = autofocus#env in
      if pcond#get_state then Env.set_indexed_vars env ;
      pcond#pp_esequent env fmt s

    method pp_goal fmt w =
      Format.fprintf fmt "@{<wp:clause>Goal@} %a:@\n" Wpo.pp_title w ;
      let _,sequent = Wpo.compute w in
      self#pp_sequent fmt sequent

    method pp_step fmt s =
      pcond#pp_step fmt s

  end

(* -------------------------------------------------------------------------- *)
