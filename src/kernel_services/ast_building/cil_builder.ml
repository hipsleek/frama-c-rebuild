(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

exception BuildError of string

let error format =
  Format.kasprintf (fun s -> raise (BuildError s)) format


(* --- Types --- *)

module Type =
struct
  exception NotACType
  open Cil_types

  type empty = |

  type ('value,'shape) morphology =
    | Uninitializable : (empty,empty) morphology
    | Single : ('value,'value) morphology
    | Listed : ('value,'shape) typ -> ('value,'shape list) morphology
    | Record : (Cil_types.fieldinfo -> 'a -> 'value) -> ('value,'a) morphology

  and ('value,'shape) typ = ('value,'shape) morphology * Cil_types.logic_type

  open Cil_const

  (* Logic types *)

  let of_ltyp t = Single, t
  let integer = Single, Linteger
  let real = Single, Lreal

  (* C base types *)

  let of_ctyp t  = Single, Ctype t
  let void       = Single, Ctype voidType
  let bool       = Single, Ctype boolType
  let char       = Single, Ctype charType
  let schar      = Single, Ctype scharType
  let uchar      = Single, Ctype ucharType
  let int        = Single, Ctype intType
  let uint       = Single, Ctype uintType
  let short      = Single, Ctype shortType
  let ushort     = Single, Ctype ushortType
  let long       = Single, Ctype longType
  let ulong      = Single, Ctype ulongType
  let longlong   = Single, Ctype longLongType
  let ulonglong  = Single, Ctype ulongLongType
  let float      = Single, Ctype floatType
  let double     = Single, Ctype doubleType
  let longdouble = Single, Ctype longDoubleType

  let ptr = function
    | _, Ctype t -> Single, Ctype (mk_tptr t)
    | _, _ -> raise NotACType

  let array ?size = function
    | (_,Ctype t) as typ ->
      let to_exp = Cil.integer ~loc:Fileloc.unknown in
      let size = Option.map to_exp size in
      Listed typ,
      Ctype (mk_tarray t size)
    | _, _ -> raise NotACType

  let structure compinfo f =
    Record f, Ctype (mk_tcomp compinfo)

  module ArgList = struct
    type t =
      | [] : t
      | Variadic : t
      | (::) : (string * ('v,'s) typ * attributes) * t -> t
  end

  let proto (rt : ('vr, 'sr) typ) (args : ArgList.t) : (empty, empty) typ =
    let rt = match rt with
      | _, Ctype ({ tnode = TArray _}) ->
        error "function cannot return an array"
      | _, Ctype t -> t
      | _ -> raise NotACType
    in
    let rec fold_args (args : ArgList.t) acc =
      match args with
      | [] -> List.rev acc, false
      | Variadic -> List.rev acc, true
      | (name, t, attrs) :: tail ->
        match t with
        | _, Ctype ({ tnode = TArray(elt,_) } as t ) ->
          fold_args tail ((name, { t with tnode = TPtr elt }, attrs) :: acc)
        | _, Ctype t ->
          fold_args tail ((name, t, attrs) :: acc)
        | _ ->
          raise NotACType
    in
    let args, is_va = fold_args args [] in
    Uninitializable, Ctype (mk_tfun rt (Some args) is_va)

  (* Attributes *)

  let attribute (s, t) name params =
    match t with
    | Ctype t ->
      let tattr = Ast_attributes.add (name, params) t.tattr in
      s, Ctype { t with tattr }
    | _ -> raise NotACType

  let const typ = attribute typ "const" []
  let stdlib_generated typ = attribute typ Ast_attributes.fc_stdlib_generated []


  (* Conversion *)

  let cil_typ = function
    | _, Ctype ty -> ty
    | _, _ -> raise NotACType

  let cil_logic_type (_,t) = t
end


(* --- Variables scoping --- *)

module Scope =
struct
  type id = int64

  let new_id : unit -> id =
    let last_id = ref Int64.zero in
    fun () ->
      last_id := Int64.(add !last_id one);
      !last_id

  module IdMap = Map.Make (Int64)

  type t = Cil_types.varinfo IdMap.t

  exception OutOfScope of string

  let empty : t = IdMap.empty
  let add : id -> Cil_types.varinfo -> t -> t = IdMap.add

  let resolve (scope : t) (id : id) (name : string) : Cil_types.varinfo =
    match IdMap.find_opt id scope with
    | Some vi -> vi
    | None -> raise (OutOfScope name)
end


(* --- C & Logic expressions builder --- *)

module Exp =
struct
  include Type

  (*
    This module exports polymorphic variant to simulate subtyping.
    It uses regular variant internally though, instead of using only the
    polymorphic variant, as
    1. it simplifies greatly the .mli since most of the types don't have
       to be exposed ; it also greatly simplifies mistyping errors for the user
    2. recursive polymorphic variants do not allow inclusion of one into another
    3. it is much easier to type the program with regular variants
  *)

  type label = Cil_types.logic_label

  type const' =
    | Int of int
    | Integer of Z.t
    | CilConstant of Cil_types.constant
  and var' =
    | CilVar of Cil_types.varinfo
    | NewVar of Scope.id * string * Cil_types.typ
  and lhost' =
    | CilLhost of Cil_types.lhost
    | Var of var'
    | Mem of exp'
  and lval' =
    | CilLval of Cil_types.lval
    | Lhost of lhost'
    | Result
    | Index of lval' * exp'
    | Field of lval' * Cil_types.fieldinfo
    | FieldNamed of lval' * string
  and exp' =
    | CilExp of Cil_types.exp
    | CilExpCopy of Cil_types.exp
    | CilTerm of Cil_types.term
    | Lval of lval'
    | Const of const'
    | Range of exp' option * exp' option
    | Unop of Cil_types.unop * exp'
    | Binop of Cil_types.binop * exp' * exp'
    | Cast of Cil_types.logic_type * exp'
    | Addr of lval'
    | App of Cil_types.logic_info * label list * exp' list
    | Pred of pred'
  and pred' =
    | ObjectPointer of label * exp'
    | Valid of label * exp'
    | ValidRead of label * exp'
    | Initialized of label * exp'
    | Dangling of label * exp'
    | Allocable of label * exp'
    | Freeable of label * exp'
    | Fresh of label * label * exp' * exp'
  and init' =
    | CilInit of Cil_types.init
    | SingleInit of exp'
    | ArrayInit of Cil_types.typ * init' list
    | StructInit of Cil_types.typ * (Cil_types.fieldinfo * init') list (* ordered *)

  type const = [ `const of const' ]
  type var = [ `var of var' ]
  type lhost = [ var | `lhost of lhost' ]
  type lval = [  lhost | `lval of lval' ]
  type exp = [ const | lval | `exp of exp' ]
  type init = [ exp | `init of init']

  (* Pretty printing *)

  let rec pretty_const fmt = function
    | Int i -> Format.pp_print_int fmt i
    | Integer i -> Z.pretty fmt i
    | CilConstant c -> Printer.pp_constant fmt c
  and pretty_var fmt = function
    | CilVar vi -> Printer.pp_varinfo fmt vi
    | NewVar (_id,name,_typ) -> Format.pp_print_string fmt name
  and pretty_lhost fmt = function
    | CilLhost h -> Printer.pp_lhost fmt h
    | Var v -> pretty_var fmt v
    | Mem e -> Format.fprintf fmt "*(%a)" pretty_exp e
  and pretty_lval fmt = function
    | CilLval lv -> Printer.pp_lval fmt lv
    | Lhost h -> pretty_lhost fmt h
    | Result -> Format.fprintf fmt "%s" "\result"
    | Index (lv,e) -> Format.fprintf fmt "%a[%a]" pretty_lval lv pretty_exp e
    | Field (lv,fi) ->
      Format.fprintf fmt "%a.%s" pretty_lval lv fi.Cil_types.fname
    | FieldNamed (lv,s) -> Format.fprintf fmt "%a.%s" pretty_lval lv s
  and pretty_exp fmt = function
    | CilExp e -> Printer.pp_exp fmt e
    | CilExpCopy e -> Printer.pp_exp fmt e
    | CilTerm t -> Printer.pp_term fmt t
    | Lval lv -> pretty_lval fmt lv
    | Const c -> pretty_const fmt c
    | Range (o1,o2) ->
      Format.fprintf fmt "(%a .. %a)" pretty_exp_opt o1 pretty_exp_opt o2
    | Unop (op,e) -> Format.fprintf fmt "%a%a" Printer.pp_unop op pretty_exp e
    | Binop (op,e1,e2) ->
      Format.fprintf fmt "(%a %a %a)"
        pretty_exp e1
        Printer.pp_binop op
        pretty_exp e2
    | Cast (lty, e) ->
      Format.fprintf fmt "(%a)%a" Printer.pp_logic_type lty pretty_exp e
    | Addr lv -> Format.fprintf fmt "&%a" pretty_lval lv
    | App (li,labels,args) -> pretty_app fmt (li.l_var_info.lv_name,labels,args)
    | Pred pred ->
      pretty_pred fmt pred
  and pretty_exp_opt fmt o =
    Option.iter (pretty_exp fmt) o
  and pretty_app fmt (name,labels,args) =
    Format.fprintf fmt "%s{%a}(%a)"
      name
      (Pretty_utils.pp_list ~sep:",@ " Printer.pp_logic_label) labels
      (Pretty_utils.pp_list ~sep:",@ " pretty_exp) args
  and pretty_pred fmt = function
    | ObjectPointer (l,e) -> pretty_app fmt ("object_pointer",[l],[e])
    | Valid (l,e) -> pretty_app fmt ("valid",[l],[e])
    | ValidRead (l,e) -> pretty_app fmt ("valid_read",[l],[e])
    | Initialized (l,e) -> pretty_app fmt ("initialized",[l],[e])
    | Dangling (l,e) -> pretty_app fmt ("dangling",[l],[e])
    | Allocable (l,e) -> pretty_app fmt ("allocable",[l],[e])
    | Freeable (l,e) -> pretty_app fmt ("freeable",[l],[e])
    | Fresh (l1,l2,e1,e2) -> pretty_app fmt ("fresh",[l1;l2],[e1;e2])
  and pretty_init fmt = function
    | CilInit init -> Printer.pp_init fmt init
    | SingleInit e -> pretty_exp fmt e
    | ArrayInit (_,l) ->
      Format.fprintf fmt "{%a}" (Pretty_utils.pp_list ~sep:",@ " pretty_init) l
    | StructInit (_,l) ->
      Format.fprintf fmt "{%a}" (Pretty_utils.pp_list ~sep:",@ " pretty_init) @@
      List.map snd l

  let pretty fmt = function
    | `none -> ()
    | `const c -> pretty_const fmt c
    | `var v -> pretty_var fmt v
    | `lhost h -> pretty_lhost fmt h
    | `lval lv -> pretty_lval fmt lv
    | `exp e -> pretty_exp fmt e
    | `init i -> pretty_init fmt i
    | `mem e -> Format.fprintf fmt "*(%a)" pretty_exp e

  (* Depolymorphize *)

  let harden_const c =
    match (c :> const) with
    | `const const -> const

  let harden_var v =
    match (v :> var) with
    | `var var -> var

  let harden_lhost h =
    match (h :> lhost) with
    | #var as var -> Var (harden_var var)
    | `lhost h -> h

  let harden_lval lv =
    match (lv :> lval) with
    | #lhost as h -> Lhost (harden_lhost h)
    | `lval lval -> lval

  let harden_lval_opt = function
    | `none -> None
    | #lval as lval -> Some (harden_lval lval)

  let harden_exp e =
    match (e :> exp) with
    | #const as const -> Const (harden_const const)
    | #lval as lval -> Lval (harden_lval lval)
    | `exp exp -> exp

  let harden_exp_opt = function
    | `none -> None
    | #exp as exp -> Some (harden_exp exp)

  let harden_exp_list l =
    List.map harden_exp l

  let harden_init i =
    match (i :> init) with
    | #exp as exp -> SingleInit (harden_exp exp)
    | `init init -> init

  (* None *)

  let none = `none

  (* Labels *)

  let here = Cil_types.(BuiltinLabel Here)
  let old = Cil_types.(BuiltinLabel Old)
  let pre = Cil_types.(BuiltinLabel Pre)
  let post = Cil_types.(BuiltinLabel Post)
  let loop_entry = Cil_types.(BuiltinLabel LoopEntry)
  let loop_current = Cil_types.(BuiltinLabel LoopCurrent)
  let program_init = Cil_types.(BuiltinLabel Init)

  (* Constants *)

  let of_int i = `const (Int i)
  let of_integer i = `const (Integer i)
  let of_constant c = `const (CilConstant c)

  let zero = of_int 0
  let one = of_int 1

  let mk_cint kind iv =
    let iv, _ = Cil.truncateInteger64 kind iv in
    Cil_types.CInt64(iv,kind,None)

  let mk_cfloat kind fv =
    let fv = Floating_point.round_if_single_precision kind fv in
    Cil_types.CReal(fv, kind, None)

  let of_cint ?(kind=Cil_types.IInt) iv =
    `const (CilConstant (mk_cint kind iv))

  let of_cfloat ?(kind=Cil_types.FDouble) fv =
    `const (CilConstant (mk_cfloat kind fv))

  (* Lvalues *)

  let var v = `var (CilVar v)
  let of_lval lv = `lval (CilLval lv)

  let of_lhost h = `lhost (CilLhost h)

  (* Expressions *)

  let of_exp e = `exp (CilExp e)
  let of_exp_copy e = `exp (CilExpCopy e)
  let of_exp_list l = List.map of_exp l
  let unop op e = `exp (Unop (op, harden_exp e))
  let neg e = unop Cil_types.Neg e
  let lognot e = unop Cil_types.LNot e
  let bwnot e = unop Cil_types.BNot e
  let binop op e1 e2 = `exp (Binop (op, harden_exp e1, harden_exp e2))
  let add e1 e2 = binop Cil_types.PlusA e1 e2
  let succ e = add e one
  let add_int e i = add e (of_int i)
  let sub e1 e2 = binop Cil_types.MinusA e1 e2
  let mul e1 e2 = binop Cil_types.Mult e1 e2
  let div e1 e2 = binop Cil_types.Div e1 e2
  let modulo e1 e2 = binop Cil_types.Mod e1 e2
  let shiftl e1 e2 = binop Cil_types.Shiftlt e1 e2
  let shiftr e1 e2 = binop Cil_types.Shiftrt e1 e2
  let lt e1 e2 = binop Cil_types.Lt e1 e2
  let gt e1 e2 = binop Cil_types.Gt e1 e2
  let le e1 e2 = binop Cil_types.Le e1 e2
  let ge e1 e2 = binop Cil_types.Ge e1 e2
  let eq e1 e2 = binop Cil_types.Eq e1 e2
  let ne e1 e2 = binop Cil_types.Ne e1 e2
  let bwand e1 e2 = binop Cil_types.BAnd e1 e2
  let bwor e1 e2 = binop Cil_types.BOr e1 e2
  let bwxor e1 e2 = binop Cil_types.BXor e1 e2
  let logand e1 e2 = binop Cil_types.LAnd e1 e2
  let logor e1 e2 = binop Cil_types.LOr e1 e2
  let cast' t e = `exp (Cast (Cil_types.Ctype t, harden_exp e))
  let cast (_,t) e = `exp (Cast (t, harden_exp e))
  let addr lv = `exp (Addr (harden_lval lv))
  let mem e = `lhost (Mem (harden_exp e))
  let index lv e = `lval (Index (harden_lval lv, harden_exp e))
  let field lv fi = `lval (Field (harden_lval lv, fi))
  let fieldnamed lv s = `lval (FieldNamed (harden_lval lv, s))

  (* Expression lists *)

  exception EmptyList

  let reduce f l =
    match (l :> exp list) with
    | [] -> raise EmptyList
    | h :: t -> List.fold_left f h t

  let logand_list l = reduce logand l
  let logor_list l = reduce logor l

  (* Term specific *)

  let result = `lval Result
  let term t = `exp (CilTerm t)
  let range e1 e2 = `exp (Range (harden_exp_opt e1, harden_exp_opt e2))
  let whole = `exp (Range (None, None))
  let whole_right = `exp (Range (Some (Const (Int 0)), None))
  let app logic_info labels args =
    `exp (App (logic_info, labels, harden_exp_list args))

  let object_pointer ?(at=here) e =
    `exp (Pred (ObjectPointer (at, harden_exp e)))
  let valid ?(at=here) e = `exp (Pred (Valid (at, harden_exp e)))
  let valid_read ?(at=here) e = `exp (Pred (ValidRead (at, harden_exp e)))
  let initialized ?(at=here) e = `exp (Pred (Initialized (at, harden_exp e)))
  let dangling ?(at=here) e = `exp (Pred (Dangling (at, harden_exp e)))
  let allocable ?(at=here) e = `exp (Pred (Allocable (at, harden_exp e)))
  let freeable ?(at=here) e = `exp (Pred (Freeable (at, harden_exp e)))
  let fresh l1 l2 e1 e2 =
    `exp (Pred (Fresh  (l1, l2, harden_exp e1, harden_exp e2)))

  (* Initializations *)

  let of_init i = `init (CilInit i)
  let compound t l =
    match t.Cil_types.tnode with
    | TArray _ ->
      `init (ArrayInit (t, List.map harden_init l))
    | TComp comp ->
      let field_init field init =
        field, harden_init init
      in
      `init (StructInit (t, List.map2 field_init (Option.get comp.cfields) l ))
    | _ -> invalid_arg "compound: type must be a C array, struct or union"

  let rec values : type a. (init, a) typ -> a -> [> init] =
    fun ty x ->
    match ty with
    | Single, Ctype _ -> x
    | Listed sub, Ctype t-> compound t (List.map (values sub) x)
    | Record f, Ctype (Cil_types.{ tnode = TComp comp } as t) ->
      let field_init field =
        field, harden_init (f field x)
      in
      `init (StructInit (t, List.map field_init (Option.get comp.cfields)))
    | Record _, _ ->
      (* invariant: Record initializers can only be associated with C structures
         or unions.  *)
      assert false
    | _, _ -> raise NotACType

  (* Operators *)

  let (+), (-), ( * ), (/), (%) = add, sub, mul, div, modulo
  let (<<), (>>) = shiftl, shiftr
  let (<), (>), (<=), (>=), (==), (!=) = lt, gt, le, ge, eq, ne
  let (--) = range
  let (.@[]) = index

  (* Convert *)

  exception LogicInC of exp
  exception CInLogic of exp
  exception NotATerm of exp
  exception NotAPredicate of exp
  exception NotAFunction of Cil_types.logic_info
  exception Typing_error of string
  exception OutOfScope = Scope.OutOfScope

  let typing_error s =
    raise (Typing_error s)

  let get_field ci s =
    try
      Cil.getCompField ci s
    with Not_found ->
      typing_error ("no field " ^ s ^ " in " ^ ci.Cil_types.cname)

  let get_result = function Ok e -> e | Error (_, msg) -> typing_error msg

  let rec build_constant = function
    | CilConstant const -> const
    | Int i -> mk_cint IInt (Z.of_int i)
    | Integer i -> mk_cint IInt i

  and build_var ~scope = function
    | CilVar vi -> vi
    | NewVar (vid, name,_typ) -> Scope.resolve scope vid name

  and build_lhost ~scope ~loc = function
    | CilLhost h -> h
    | Var v -> Cil_types.Var (build_var ~scope v)
    | Mem e ->
      let e' = build_exp ~scope ~loc e in
      fst (Cil.mkMem ~addr:e' ~off:Cil_types.NoOffset)

  and build_lval ~scope ~loc = function
    | Result as lv -> raise (LogicInC (`lval lv))
    | CilLval lval -> lval
    | Lhost h ->
      let h' = build_lhost ~scope ~loc h in
      (h', NoOffset)
    | Index (lv, e) ->
      let (host, offset) as lv' = build_lval ~scope ~loc lv
      and e' = build_exp ~scope ~loc e in
      begin match Ast_types.unroll_node (Cil.typeOfLval lv') with
        | TArray _ ->
          let offset' = Cil_types.Index (e', NoOffset) in
          host, Cil.addOffset offset' offset
        | TPtr _ ->
          let base = Cil.new_exp ~loc (Lval lv') in
          let addr_res = Cil.mkBinOp ~loc Cil_types.PlusPI base e' in
          let addr = get_result addr_res in
          Cil.mkMem ~addr ~off:Cil_types.NoOffset
        | _ -> typing_error "trying to index an lvalue which is not an array \
                             or a pointer"
      end
    | (Field (lv,_) | FieldNamed (lv,_)) as e ->
      let (host, offset) as lv' = build_lval ~scope ~loc lv in
      let host', offset', ci =
        match Ast_types.unroll_deep_node (Cil.typeOfLval lv') with
        | TComp ci -> host, offset, ci
        | TPtr { tnode = TComp ci } ->
          Mem (Cil.new_exp ~loc (Lval lv')), Cil_types.NoOffset, ci
        | _ -> typing_error "trying to get a field of an lvalue which is not \
                             of composite type or pointer to a composite type"
      in
      let f = match e with
        | Field (_lv,f) -> f
        | FieldNamed (_lv,s) -> get_field ci s
        | _ -> assert false
      in
      let offset'' = Cil_types.(Field (f, NoOffset)) in
      host', Cil.addOffset offset'' offset'

  and build_exp ~scope ~loc = function
    | CilTerm _ | Range _ | App _ | Pred _ as e -> raise (LogicInC (`exp e))
    | CilExp exp -> exp
    | CilExpCopy exp -> Cil.copy_exp exp
    | Const c->
      Cil.new_exp ~loc (Cil_types.Const (build_constant c))
    | Lval lval ->
      Cil.new_exp ~loc (Cil_types.Lval (build_lval ~scope ~loc lval))
    | Unop (op,e) ->
      let e' = build_exp ~scope ~loc e in
      let oldt = Cil.typeOf e' in
      let newt = Cil.integralPromotion oldt in
      Cil.(new_exp ~loc (Cil_types.UnOp (op, mkCastT ~oldt ~newt e', oldt)))
    | Binop (op,e1,e2) ->
      let is_pointer_type e =
        Ast_types.is_ptr (Cil.typeOf e)
      in
      let e1' = build_exp ~scope ~loc e1
      and e2' = build_exp ~scope ~loc e2 in
      let op' = match op with (* Normalize operation *)
        | PlusA when is_pointer_type e1' -> Cil_types.PlusPI
        | MinusA when is_pointer_type e1' -> Cil_types.MinusPI
        | PlusPI when not (is_pointer_type e1') -> Cil_types.PlusA
        | MinusPI when not (is_pointer_type e1') -> Cil_types.MinusA
        | op -> op
      in
      get_result (Cil.mkBinOp ~loc op' e1' e2')
    | Cast (Cil_types.Ctype newt, e) ->
      Cil.mkCast ~force:false ~newt (build_exp ~scope ~loc e)
    | Cast _ ->
      raise NotACType
    | Addr lv ->
      Cil.mkAddrOrStartOf ~loc (build_lval ~scope ~loc lv)

  (* restyp is the type of result *)
  let rec build_term_lval ~scope ~loc ~restyp = function
    | Result -> Cil_types.(TResult (Option.get restyp), TNoOffset)
    | CilLval lv -> Logic_utils.lval_to_term_lval lv
    | Lhost h ->
      let h' = build_term_lhost ~scope ~loc ~restyp h in
      h', TNoOffset
    | Index (tlv, t) ->
      let (host, offset) as tlv' = build_term_lval ~scope ~loc ~restyp tlv
      and t' = build_term ~scope ~loc ~restyp t in
      let lty = Cil.typeOfTermLval tlv' in
      begin match Ast_types.unroll_logic lty with
        | Ctype { tnode = TArray _ } ->
          let offset' = Cil_types.(TIndex (t', TNoOffset)) in
          host, Logic_const.addTermOffset offset' offset
        | Ctype { tnode = TPtr _ } ->
          let base = Logic_const.term ~loc (TLval tlv') lty in
          let addr = Logic_const.term ~loc (TBinOp (PlusPI,base,t')) lty in
          TMem addr, TNoOffset
        | _ -> typing_error "trying to index a term lvalue which is not a C \
                             array or a C pointer"
      end
    | (Field (tlv,_) | FieldNamed (tlv,_)) as t ->
      let (host, offset) as tlv' = build_term_lval ~scope ~loc ~restyp tlv in
      let lty = match Ast_types.unroll_logic (Cil.typeOfTermLval tlv') with
        | Ctype cty -> Cil_types.Ctype (Ast_types.unroll_deep cty)
        | lty -> lty
      in
      let host', offset', ci = match lty with
        | Ctype { tnode = TComp ci } -> host, offset, ci
        | Ctype { tnode = TPtr { tnode = TComp ci } } ->
          TMem (Logic_const.term ~loc (Cil_types.TLval tlv') lty), TNoOffset, ci
        | _ -> typing_error "trying to get a field of an lvalue which is not \
                             of composite type or pointer to a composite type"
      in
      let f = match t with
        | Field (_lv,f) -> f
        | FieldNamed (_lv,s) -> get_field ci s
        | _ -> assert false
      in
      let offset'' = Cil_types.(TField (f, TNoOffset)) in
      host', Logic_const.addTermOffset offset'' offset'

  and build_term_lhost ~scope ~loc ~restyp = function
    | CilLhost h -> Logic_utils.host_to_term_lhost h
    | Var v ->
      Cil_types.(TVar (Cil.cvar_to_lvar (build_var ~scope v)))
    | Mem t ->
      let t' = build_term ~scope ~loc ~restyp t in
      Cil_types.TMem t'


  and build_term ~scope ~loc ~restyp = function
    | Const (CilConstant c) ->
      Logic_utils.expr_to_term ~coerce:false @@
      Cil.new_exp ~loc (Cil_types.Const c)
    | CilExp exp | CilExpCopy exp ->
      Logic_utils.expr_to_term ~coerce:true exp
    | Pred _ as e ->
      raise (NotATerm (`exp e))
    | CilTerm term -> term
    | Const (Int i) ->
      Logic_const.tinteger ~loc i
    | Const (Integer i) ->
      Logic_const.tint ~loc i
    | Lval lval ->
      let tlval = build_term_lval ~scope ~loc ~restyp lval in
      Logic_const.term ~loc Cil_types.(TLval tlval) (Cil.typeOfTermLval tlval)
    | Unop (op,t) ->
      let t' = build_term t ~scope ~loc ~restyp in
      let ty = t'.Cil_types.term_type in
      (* TODO: type conversion *)
      Logic_const.term ~loc Cil_types.(TUnOp (op,t')) ty
    | Binop (op,t1,t2) ->
      let t1' = build_term ~scope ~loc ~restyp t1
      and t2' = build_term ~scope ~loc ~restyp t2 in
      let ty = t1'.Cil_types.term_type in
      let op' = match op with (* Normalize operation *)
        | PlusA when Logic_utils.isLogicPointer t1' -> Cil_types.PlusPI
        | MinusA when Logic_utils.isLogicPointer t1' -> Cil_types.MinusPI
        | PlusPI when not (Logic_utils.isLogicPointer t1') -> Cil_types.PlusA
        | MinusPI when not (Logic_utils.isLogicPointer t1') -> Cil_types.MinusA
        | op -> op
      in
      (* TODO: type conversion *)
      Logic_const.term ~loc Cil_types.(TBinOp (op',t1',t2')) ty
    | Cast (Ctype ct, t) ->
      let t' = build_term ~scope ~loc ~restyp t in
      Logic_utils.mk_cast ~loc ct t'
    | Cast (ty, t) ->
      let t' = build_term ~scope ~loc ~restyp t in
      Logic_utils.numeric_coerce ty t'
    | Addr lval ->
      let tlval = build_term_lval ~scope ~loc ~restyp lval in
      let ty = Cil.typeOfTermLval tlval in
      Logic_utils.mk_logic_AddrOf ~loc tlval ty
    | Range (t1,t2) ->
      let t1' = Option.map (build_term ~scope ~loc ~restyp) t1
      and t2' = Option.map (build_term ~scope ~loc ~restyp) t2 in
      Logic_const.trange ~loc (t1',t2')
    | App (logic_info, labels, args) ->
      let ty = match logic_info.l_type with
        | None -> raise (NotAFunction logic_info)
        | Some ty -> ty
      in
      let args' = List.map (build_term ~scope ~loc ~restyp) args in
      Logic_const.term ~loc (Tapp (logic_info, labels, args')) ty

  and build_relation e = function
    | Cil_types.Lt -> Cil_types.Rlt
    | Cil_types.Gt -> Cil_types.Rgt
    | Cil_types.Le -> Cil_types.Rle
    | Cil_types.Ge -> Cil_types.Rge
    | Cil_types.Eq -> Cil_types.Req
    | Cil_types.Ne -> Cil_types.Rneq
    | _ -> raise (NotAPredicate (`exp e))

  and build_pred_node ~scope ~loc ~restyp = function
    | Unop (Cil_types.LNot, p) ->
      let p' = build_pred ~scope ~loc ~restyp p in
      Cil_types.Pnot p'
    | Binop (Cil_types.LAnd, p1, p2) ->
      let p1' = build_pred ~scope ~loc ~restyp p1
      and p2' = build_pred ~scope ~loc ~restyp p2 in
      Cil_types.Pand (p1',p2')
    | Binop (Cil_types.LOr, p1, p2) ->
      let p1' = build_pred ~scope ~loc ~restyp p1
      and p2' = build_pred ~scope ~loc ~restyp p2 in
      Cil_types.Por (p1',p2')
    | Binop (binop, t1, t2) as e ->
      let rel = build_relation e binop
      and t1' = build_term ~scope ~loc ~restyp t1
      and t2' = build_term ~scope ~loc ~restyp t2 in
      Cil_types.Prel (rel, t1', t2')
    | Const _ | CilExp _ | CilExpCopy _  | CilTerm _
    | Lval _ | Unop _ | Cast _ | Addr _ | Range _ as e ->
      raise (NotAPredicate (`exp e))
    | App (logic_info, labels, args) ->
      let args' = List.map (build_term ~scope ~loc ~restyp) args in
      Cil_types.Papp (logic_info, labels, args')
    | Pred (ObjectPointer (l, t)) ->
      Cil_types.Pobject_pointer (l, build_term ~scope ~loc ~restyp t)
    | Pred (Valid (l, t)) ->
      Cil_types.Pvalid (l, build_term ~scope ~loc ~restyp t)
    | Pred (ValidRead (l, t)) ->
      Cil_types.Pvalid_read (l, build_term ~scope ~loc ~restyp t)
    | Pred (Initialized (l, t)) ->
      Cil_types.Pinitialized (l, build_term ~scope ~loc ~restyp t)
    | Pred (Dangling (l, t)) ->
      Cil_types.Pdangling (l, build_term ~scope ~loc ~restyp t)
    | Pred (Allocable (l, t)) ->
      Cil_types.Pallocable (l, build_term ~scope ~loc ~restyp t)
    | Pred (Freeable (l, t)) ->
      Cil_types.Pfreeable (l, build_term ~scope ~loc ~restyp t)
    | Pred (Fresh (l1, l2, t1, t2)) ->
      let t1' = build_term ~scope ~loc ~restyp t1
      and t2' = build_term ~scope ~loc ~restyp t2 in
      Cil_types.Pfresh (l1, l2, t1', t2')

  and build_pred ~scope ~loc ~restyp t =
    Logic_const.unnamed ~loc (build_pred_node ~scope ~loc ~restyp t)

  let rec build_init ~scope ~loc = function
    | CilInit init -> init
    | SingleInit e ->
      Cil_types.SingleInit (build_exp ~scope ~loc e)
    | ArrayInit (typ,l) ->
      let index i = Cil_types.(Index (Cil.integer ~loc i, NoOffset)) in
      let initl =
        List.mapi (fun i sub -> index i, build_init ~scope ~loc sub) l
      in
      Cil_types.CompoundInit (typ, initl)
    | StructInit (typ,l) ->
      let field fi = Cil_types.(Field (fi,NoOffset)) in
      let initl =
        List.map (fun (fi,sub) -> field fi, build_init ~scope ~loc sub) l
      in
      Cil_types.CompoundInit (typ, initl)


  (* Export *)

  let cil_logic_label label = label
  let cil_constant c = build_constant (harden_const c)
  let cil_varinfo v = build_var ~scope:Scope.empty (harden_var v)
  let cil_lhost ~loc h = build_lhost ~scope:Scope.empty ~loc (harden_lhost h)
  let cil_lval ~loc lv = build_lval ~scope:Scope.empty ~loc (harden_lval lv)
  let cil_lval_opt ~loc lv =
    Option.map (build_lval ~scope:Scope.empty ~loc) (harden_lval_opt lv)
  let cil_exp ~loc e = build_exp ~scope:Scope.empty ~loc (harden_exp e)
  let cil_exp_opt ~loc e =
    Option.map (build_exp ~scope:Scope.empty ~loc) (harden_exp_opt e)
  let cil_exp_list ~loc l = List.map (cil_exp ~loc) l
  let cil_term_lval ~loc ?restyp lv =
    build_term_lval ~scope:Scope.empty ~loc ~restyp (harden_lval lv)
  let cil_term_lhost ~loc ?restyp h =
    build_term_lhost ~scope:Scope.empty ~loc ~restyp (harden_lhost h)
  let cil_term ~loc ?restyp e =
    build_term ~scope:Scope.empty ~loc ~restyp (harden_exp e)
  let cil_iterm ~loc ?restyp e =
    Logic_const.new_identified_term (cil_term ~loc ?restyp e)
  let cil_pred ~loc ?restyp e =
    build_pred ~scope:Scope.empty ~loc ~restyp (harden_exp e)
  let cil_ipred ~loc ?restyp e =
    Logic_const.new_predicate (cil_pred ~loc ?restyp e)
  let cil_init ~loc i = build_init ~scope:Scope.empty ~loc (harden_init i)

  let cil_typeof (`var v) =
    match v with
    | CilVar vi -> vi.Cil_types.vtype
    | NewVar (_id,_name,typ) -> typ
end


(* --- Pure builder --- *)

module Pure =
struct
  include Exp

  exception DeclarationOutsideOfFunction

  type ghost = NoGhost | Ghost

  type call' = lhost' * exp' list

  type instr' =
    | CilInstr of Cil_types.instr
    | Skip
    | Assign of lval' * exp'
    | Call of lval' option * call'
    | Local of var' * init' option * ghost
    | LocalCopy of Cil_types.varinfo * var' * init' option * ghost

  type stmt' =
    | CilStmt of Cil_types.stmt
    | CilStmtkind of Cil_types.stmtkind
    | Instr of instr'
    | Sequence of stmt' list
    | Block of stmt' list
    | GhostSection of stmt'
    | If of exp' * block * block

  and block = stmt' list * Cil_types.attributes

  type call = [ `call of call' ]
  type instr = [ call | `instr of instr' ]
  type stmt = [ instr | `stmt of stmt' ]

  (* Depolymorphize *)

  let harden_call c =
    match (c :> call) with
    | `call call -> call

  let harden_instr i =
    match (i :> instr) with
    | `call call -> Call (None, call)
    | `instr instr -> instr

  let harden_stmt s =
    match (s :> stmt) with
    | #instr as instr -> Instr (harden_instr instr)
    | `stmt stmt -> stmt

  let harden_block l attributes : block =
    List.map harden_stmt l, attributes

  (* Build *)

  let of_instr i = `instr (CilInstr i)
  let skip = `instr Skip
  let assign dest = function
    | #exp as src -> `instr (Assign (harden_lval dest, harden_exp src))
    | #call as call -> `instr (Call (Some (harden_lval dest), harden_call call))
  let incr dest = `instr (Assign (harden_lval dest, harden_exp (add dest one)))
  let call_ptr callee args = `call (harden_lhost callee, harden_exp_list args)
  let call callee args = call_ptr (var (Kernel_function.get_vi callee)) args
  let of_stmtkind sk = `stmt (CilStmtkind sk)
  let of_stmt s = `stmt (CilStmt s)
  let of_stmts l = `stmt (Sequence (List.map (fun s -> CilStmt s) l))
  let sequence l = `stmt (Sequence (List.map harden_stmt l))
  let block l = `stmt (Block (List.map harden_stmt l))
  let ghost s = `stmt (GhostSection (harden_stmt s))

  let if_ ?(ghost_else=false) cond ~then_ ~else_ =
    let else_attributes =
      if ghost_else
      then [(Ast_attributes.frama_c_ghost_else,[])]
      else []
    in
    `stmt (If (
        harden_exp cond,
        harden_block then_ [],
        harden_block else_ else_attributes))

  let local' ?(ghost=false) ?init typ name =
    let var = NewVar (Scope.new_id (), name, typ) in
    let ghost = if ghost then Ghost else NoGhost in
    let instr = Local (var, Option.map harden_init init, ghost) in
    `var var, `instr instr

  let local ?ghost ?init ty name =
    let init = Option.map (values ty) init in
    local' ?ghost ?init (cil_typ ty) name

  let local_copy ?(ghost=false) ?(suffix="_tmp") v =
    let name, typ, vi =
      match harden_var v with
      | NewVar (_id, name, typ) -> name, typ, None
      | CilVar vi -> vi.vname, vi.vtype, Some vi
    in
    let var = NewVar (Scope.new_id (), name ^ suffix, typ) in
    let ghost = if ghost then Ghost else NoGhost in
    let instr = match vi with
      | None -> Local (var, None, ghost)
      | Some vi -> LocalCopy (vi, var, None, ghost)
    in
    `var var, `instr instr

  (* Convert *)

  (* block: refers to the innermost englobing block where locals must be added
     fundec: refers to the owner function where locals must also be added *)

  let build_local_definition ~scope ~loc ~block ~fundec v init ghost copied_vi =
    let vi, scope = match v with
      | CilVar vi -> vi, scope
      | NewVar (id, name, typ) ->
        let temp = false and global = false and formal = false in
        let ghost = match ghost with Ghost -> true | NoGhost -> false in
        let vi =
          match copied_vi with
          | None -> Cil.makeVarinfo ~temp ~ghost global formal name typ
          | Some vi -> Cil.copyVarinfo vi name
        in
        let block, fundec = match block, fundec with
          | Some block, Some fundec -> block, fundec
          | None, _ | _, None -> raise DeclarationOutsideOfFunction
        in
        (* Register the variable *)
        Cil.refresh_local_name fundec vi;
        vi.vdecl <- loc;
        fundec.slocals <- fundec.slocals @ [vi];
        block.Cil_types.blocals <- vi :: block.Cil_types.blocals;
        vi, Scope.add id vi scope
    in
    (* Initialization *)
    let initialization =
      match init with
      | None -> Cil_types.Skip loc
      | Some init ->
        vi.vdefined <- true;
        let local_init = Cil_types.AssignInit (build_init ~scope ~loc init) in
        Cil_types.Local_init (vi, local_init, loc)
    in
    initialization, scope

  let build_instr ~scope ~loc ~block ~fundec = function
    | CilInstr i -> i, scope
    | Skip ->
      Cil_types.Skip (loc), scope
    | Assign (dest,src) ->
      let dest' = build_lval ~scope ~loc dest
      and src' = build_exp ~scope ~loc src in
      let src' = Cil.mkCast ~newt:(Cil.typeOfLval dest') src' in
      Cil_types.Set (dest', src', loc), scope
    | Call (dest,(callee,args)) ->
      let dest' = Option.map (build_lval ~scope ~loc) dest
      and callee' = build_lhost ~scope ~loc callee
      and args' = List.map (build_exp ~scope ~loc) args in
      Cil_types.Call (dest', callee', args', loc), scope
    | Local (v, init, ghost) ->
      build_local_definition ~scope ~loc ~block ~fundec v init ghost None
    | LocalCopy (vi, v, init, ghost) ->
      build_local_definition ~scope ~loc ~block ~fundec v init ghost (Some vi)

  let rec build_stmtkind ~scope ~loc ~ghost ~block ~fundec = function
    | CilStmtkind sk -> sk, scope
    | Instr i ->
      let instr, scope = build_instr ~scope ~loc ~block ~fundec i in
      Cil_types.Instr instr, scope
    | If (exp, then_stmts, else_stmt) ->
      Cil_types.If (
        build_exp ~scope ~loc exp,
        build_block ~scope ~loc ~ghost ~fundec then_stmts,
        build_block ~scope ~loc ~ghost ~fundec else_stmt,
        loc
      ),
      scope
    | Sequence s | Block s ->
      Cil_types.Block (build_block ~scope ~loc ~ghost ~fundec (s,[])), scope
    | (CilStmt _)  as s | GhostSection s ->
      let ghost = true in
      Cil_types.Block (build_block ~scope ~loc ~ghost ~fundec ([s],[])), scope

  and build_stmtlist_rev ~scope ~loc ~ghost ~block ~fundec acc l =
    let add_one (acc,scope) stmt =
      match stmt with
      | CilStmt s -> s :: acc, scope
      | Sequence s -> (* do not build a block if unnecessary *)
        build_stmtlist_rev ~scope ~loc ~ghost ~block ~fundec acc s
      | GhostSection stmt -> (* do not build a block if unnecessary *)
        build_stmtlist_rev ~scope ~loc ~ghost:true ~block ~fundec acc [stmt]
      | _ ->
        let stmtkind, scope =
          build_stmtkind ~scope ~loc ~ghost ~block ~fundec stmt
        in
        match stmtkind with
        | Instr (Cil_types.Skip _) -> acc, scope (* Filter skips out *)
        | stmtkind ->
          Cil.mkStmt ~ghost stmtkind :: acc, scope
    in
    List.fold_left add_one (acc,scope) l

  and build_stmt ~scope ~loc ~ghost ~block ~fundec = function
    | CilStmt s -> s, scope
    | GhostSection s -> build_stmt ~scope ~loc ~ghost:true ~block ~fundec s
    | stmt ->
      let stmtkind, scope =
        build_stmtkind ~scope ~loc ~ghost ~block ~fundec stmt
      in
      Cil.mkStmt ~ghost stmtkind, scope

  and build_block ~scope ~loc ~ghost ~fundec (l, attributes) =
    let block = Cil.mkBlock [] in
    let bstmts, _scope =
      build_stmtlist_rev ~scope ~loc ~ghost ~block:(Some block) ~fundec [] l
    in
    block.battrs <- attributes;
    block.bstmts <- List.rev bstmts;
    block


  (* Export *)

  let top_block =
    Option.map (fun fundec -> fundec.Cil_types.sbody)

  let cil_instr ?into:fundec ~loc i =
    let scope = Scope.empty and block = top_block fundec in
    fst (build_instr ~scope ~loc ~block ~fundec (harden_instr i))
  let cil_stmtkind ?into:fundec ~loc s =
    let scope = Scope.empty and block = top_block fundec in
    fst (build_stmtkind ~scope ~loc ~block ~fundec ~ghost:false (harden_stmt s))
  let cil_stmt ?into:fundec ~loc s =
    let scope = Scope.empty and block = top_block fundec in
    fst (build_stmt ~scope ~loc ~block ~fundec ~ghost:false (harden_stmt s))


  (* Operators *)

  let (:=) = assign
  let (+=) lv e = assign lv (add lv e)
  let (-=) lv e = assign lv (sub lv e)

  let (let+) (var, stmt) f = stmt :: f var
  let (and+) var1 var2 = (var1, var2)
end


(* --- Stateful builder --- *)

module Stateful () =
struct
  include Exp

  type stmt =
    | Label of Cil_types.label
    | CilStmt of Cil_types.stmt
    | CilStmtkind of Cil_types.stmtkind
    | CilInstr of Cil_types.instr

  type scope =
    {
      loc: Cil_types.location;
      scope_type: scope_type;
      ghost: bool;
      mutable stmts: stmt list; (* In reverse order *)
      mutable vars: Cil_types.varinfo list; (* In reverse order *)
    }
  and scope_type =
    | Block
    | IfThen of {ifthen_exp: Cil_types.exp}
    | IfThenElse of {
        ifthenelse_exp: Cil_types.exp;
        then_block: Cil_types.block;
        ghost_else: bool;
      }
    | Switch of {switch_exp: Cil_types.exp}
    | Function of {fundec: Cil_types.fundec}


  (* Conversion to Cil *)

  let build_instr_list l =
    let rev_build_one acc = function
      | Label _ | CilStmt _ | CilStmtkind _ ->
        error "the statement is not an instruction"
      | CilInstr instr -> instr :: acc
    in
    List.fold_left rev_build_one [] l

  let build_stmt_list { loc ; ghost ; stmts } =
    let rev_build_one acc = function
      | Label l ->
        begin match acc with
          | [] -> (* No generated statement to attach the label to *)
            let stmt = Cil.mkEmptyStmt ~ghost ~loc () in
            stmt.Cil_types.labels <- [l];
            stmt :: acc
          | stmt :: _ -> (* There is a statement to attach the label to *)
            stmt.Cil_types.labels <- l :: stmt.Cil_types.labels;
            acc
        end
      | CilStmt stmt ->
        stmt :: acc
      | CilStmtkind sk ->
        Cil.mkStmt ~ghost sk :: acc
      | CilInstr instr ->
        Cil.mkStmt ~ghost (Cil_types.Instr instr) :: acc
    in
    List.fold_left rev_build_one [] stmts

  let build_block b =
    let block = Cil.mkBlock (build_stmt_list  b) in
    block.Cil_types.blocals <- List.rev b.vars;
    block

  let build_stmtkind b =
    let block = build_block b in
    match b.scope_type with
    | Block ->
      Cil_types.Block block
    | IfThen { ifthen_exp } ->
      Cil_types.If (ifthen_exp, block, Cil.mkBlock [], b.loc)
    | IfThenElse { ifthenelse_exp; then_block; ghost_else } ->
      if ghost_else then
        block.battrs <- [(Ast_attributes.frama_c_ghost_else,[])];
      Cil_types.If (ifthenelse_exp, then_block, block, b.loc)
    | Switch { switch_exp } ->
      let open Cil_types in
      (* Cases are only allowed in the current block by the case function *)
      let contains_case stmt =
        List.exists (function Case _ -> true | _ -> false) stmt.labels
      in
      let case_stmts = List.filter contains_case block.bstmts in
      Cil_types.Switch (switch_exp, block, case_stmts , b.loc)
    | Function _ ->
      error "the function block is not convertible to Cil_types.stmtkind"


  (* State management *)

  let owner: Cil_types.fundec option ref = ref None

  let reset_owner () =
    owner := None

  let set_owner o =
    if Option.is_some !owner then
      error "already in a function context";
    owner := Some o

  let get_owner () =
    match !owner with
    | None -> error "function context not set"
    | Some fundec -> fundec


  let stack : scope list ref = ref []

  let pretty_stack fmt =
    let pretty_stack_type fmt b =
      match b.scope_type with
      | Block -> Format.pp_print_string fmt "block"
      | IfThen _ -> Format.pp_print_string fmt "if-then"
      | IfThenElse _ -> Format.pp_print_string fmt "if-then-else"
      | Switch _ -> Format.pp_print_string fmt "switch"
      | Function _ -> Format.pp_print_string fmt "function"
    in
    Pretty_utils.pp_list ~pre:"[@[" ~sep:";@," ~last:"@]]"
      pretty_stack_type fmt !stack

  let check_empty () =
    if !stack <> [] then
      error "some contexts have not been closed: %t" pretty_stack !stack

  let check_not_empty () =
    if !stack = [] then
      error "only a finish_* function can close all contexts"

  let top () =
    match !stack with
    | [] -> error "not in an opened context"
    | state :: _ -> state

  let push state =
    let parent_ghost = match !stack with
      | [] -> false
      | s :: _ -> s.ghost
    in
    stack := { state  with ghost = parent_ghost || state.ghost } :: !stack;
    Kernel.debug ~dkey:Kernel.dkey_cil_builder "push onto %t" pretty_stack

  let pop () =
    Kernel.debug ~dkey:Kernel.dkey_cil_builder "pop from %t" pretty_stack;
    match !stack with
    | [] -> error "not in an opened context"
    | hd :: tail ->
      stack := tail;
      hd

  let finish () =
    reset_owner ();
    match !stack with
    | [] -> error "not in an opened context"
    | [b] -> stack := []; b
    | _ :: _ :: _ -> error "all contexts have not been closed"

  let append_stmt b s =
    b.stmts <- s :: b.stmts

  let append_instr b i =
    append_stmt b (CilInstr i)

  let append_local v =
    let fundec = get_owner () and b = top () in
    fundec.Cil_types.slocals <- fundec.Cil_types.slocals @ [v];
    b.vars <- v :: b.vars

  let current_loc () =
    match !stack with
    | [] -> Fileloc.unknown
    | state :: _ -> state.loc


  (* Statements *)

  let of_stmt s =
    let b = top () in
    append_stmt b (CilStmt s)

  let of_stmts l =
    List.iter of_stmt l

  let of_stmtkind sk =
    let b = top () in
    append_stmt b (CilStmtkind sk)

  let break () =
    let loc = current_loc () in
    of_stmtkind (Cil_types.Break loc)

  let return exp =
    let loc = current_loc () in
    of_stmtkind (Cil_types.Return (cil_exp_opt ~loc exp, loc))


  (* Blocks *)

  let new_block ?(loc=current_loc ()) ?(ghost=false) scope_type =
    {
      loc;
      scope_type;
      ghost;
      stmts = [];
      vars = [];
    }

  let extract_ifthen_block b =
    match b.scope_type with
    | IfThen {ifthen_exp} -> ifthen_exp
    | _ -> error "not in an opened if-then-else context"

  let extract_switch_block b =
    match b.scope_type with
    | Switch {switch_exp} -> switch_exp
    | _ -> error "not in a opened switch context"

  let extract_function_block b =
    match b.scope_type with
    | Function {fundec} -> fundec
    | _ -> error "not in a opened function context"

  let open_function ?(loc=current_loc ()) ?ghost ?vorig_name name =
    check_empty ();
    let vorig_name = Option.value ~default:name vorig_name in
    let fundec = Cil.emptyFunction vorig_name in
    fundec.svar.vdecl <- loc;
    fundec.svar.vname <- name;
    set_owner fundec;
    push (new_block ~loc ?ghost (Function {fundec}));
    `var (CilVar fundec.Cil_types.svar)

  let open_block ?(loc=current_loc ()) ?into ?ghost () =
    Option.iter set_owner into;
    push (new_block ~loc ?ghost Block)

  let open_ghost ?(loc=current_loc ()) ?into () =
    open_block ~loc ?into ~ghost:true ()

  let open_switch ?(loc=current_loc ()) ?into exp =
    Option.iter set_owner into;
    let switch_exp = cil_exp ~loc exp in
    push (new_block ~loc (Switch {switch_exp}))

  let open_if ?(loc=current_loc ()) ?into exp =
    Option.iter set_owner into;
    let ifthen_exp = cil_exp ~loc exp in
    push (new_block ~loc (IfThen {ifthen_exp}))

  let open_else ?(ghost=false) () =
    let b = pop () in
    let ifthenelse_exp = extract_ifthen_block b in
    let then_block = build_block b in
    push (new_block (IfThenElse {ifthenelse_exp; then_block; ghost_else=ghost}))

  let close () =
    let above = pop () in
    check_not_empty ();
    of_stmtkind (build_stmtkind above) (* add the block to the parent *)

  let finish_block () =
    let b = finish () in
    match build_stmtkind b with
    | Cil_types.Block b -> b
    | _ -> error "not in an opened simple block context"

  let finish_instr_list ?scope () =
    let b = finish () in
    begin match scope, b.vars with
      | None, [] -> () (* Nothing to do *)
      | Some block, vars ->
        block.Cil_types.blocals <- List.rev vars @ block.Cil_types.blocals
      | None, _ :: _ ->
        error "a scope must be provided to insert local variables"
    end;
    build_instr_list b.stmts

  let finish_stmt () =
    let b = finish () in
    Cil.mkStmt ~ghost:b.ghost (build_stmtkind b)

  let finish_function ?(register=true) () =
    let b = finish () in
    let fundec = extract_function_block b in
    let open Cil_types in
    let vi = fundec.svar and spec = fundec.sspec in
    fundec.sbody <- build_block b;
    vi.vdefined <- true;
    vi.vghost <- b.ghost;
    if register then begin
      Globals.Functions.replace_by_definition spec fundec b.loc;
      let keepSwitch = Kernel.KeepSwitch.get () in
      Cfg.prepareCFG ~keepSwitch fundec;
      Cfg.cfgFun fundec;
    end;
    GFun (fundec,b.loc)

  let finish_declaration ?(register=true) () =
    let b = finish () in
    let fundec = extract_function_block b in
    let open Cil_types in
    let vi = fundec.svar and spec = fundec.sspec in
    if b.stmts <> [] then
      error "there must not be any built statements";
    vi.vdefined <- false;
    vi.vghost <- b.ghost;
    if register then begin
      Globals.Functions.replace_by_declaration spec vi b.loc;
    end;
    GFunDecl (spec, vi, b.loc)

  let case exp =
    let b = top () in
    let _ = extract_switch_block b in
    let loc = b.loc in
    let label = Cil_types.Case (cil_exp ~loc exp, loc) in
    append_stmt b (Label label)


  (* Functions *)

  let get_return_type () =
    let fundec = get_owner () in
    Cil.getReturnType fundec.svar.vtype

  let set_return_type' ty =
    let fundec = get_owner () in
    Cil.setReturnType fundec ty

  let set_return_type ty =
    set_return_type' (cil_typ ty)

  let add_attribute attr =
    let fundec = get_owner () in
    fundec.svar.vattr <- Ast_attributes.add attr fundec.svar.vattr

  let add_new_attribute attr params =
    add_attribute (attr, params)

  let add_stdlib_generated () =
    add_new_attribute Ast_attributes.fc_stdlib_generated []


  (* Behaviors *)

  let current_behavior () =
    let b = top () in
    let fundec = extract_function_block b in
    match fundec.sspec.spec_behavior with
    | [] ->
      let default_behavior = Cil.mk_behavior () in
      fundec.sspec.spec_behavior <- [default_behavior];
      default_behavior
    | bhv :: _ -> bhv

  type source = [exp | `indirect of exp]

  let indirect src =
    match (src :> source ) with
    | #exp as it | `indirect it -> `indirect it

  let append_assigns new_assigns =
    let b = current_behavior () in
    let previous_assigns = match b.b_assigns with
      | WritesAny -> []
      | Writes l -> l
    in
    b.b_assigns <- Writes (previous_assigns @ new_assigns)

  let assigns dests sources =
    let open Cil_types in
    let restyp = get_return_type ()
    and loc = current_loc () in
    let map_source src =
      match (src :> source) with
      | #Exp.exp as e ->
        cil_iterm ~loc ~restyp e
      | `indirect e ->
        let t = cil_term ~loc ~restyp e in
        let t' = { t with term_name = "indirect" :: t.term_name } in
        Logic_const.new_identified_term t'
    and map_dest dst =
      cil_iterm ~loc ~restyp dst
    in
    let dests' = List.map map_dest dests
    and sources' = List.map map_source sources in
    let new_assigns = List.map (fun dst -> dst, From sources') dests' in
    append_assigns new_assigns

  let infer_assigns () =
    let fundec = get_owner ()
    and b = current_behavior () in
    let assigns = Infer_assigns.from_prototype_vi fundec.svar in
    b.b_assigns <- Writes assigns

  let requires pred =
    let open Cil_types in
    let b = current_behavior ()
    and restyp = get_return_type ()
    and loc = current_loc () in
    b.b_requires <- b.b_requires @ [cil_ipred ~loc ~restyp pred]

  let ensures pred =
    let open Cil_types in
    let b = current_behavior ()
    and restyp = get_return_type ()
    and loc = current_loc () in
    b.b_post_cond <- b.b_post_cond @ [Normal, cil_ipred ~loc ~restyp pred]


  (* Variables *)

  let local' ?(ghost=false) ?init typ name =
    let fundec = get_owner () and b = top () in
    let ghost = ghost || b.ghost in
    let loc = current_loc () in
    let v = Cil.makeLocalVar ~insert:false ~ghost ~loc fundec name typ in
    begin match init with
      | None -> ()
      | Some init ->
        let local_init = Cil_types.AssignInit (cil_init ~loc init) in
        append_instr b (Cil_types.Local_init (v, local_init, loc));
        v.vdefined <- true
    end;
    append_local v;
    `var (CilVar v)

  let local ?ghost ?init ty name =
    let init = Option.map (values ty) init in
    local' ?ghost ?init (cil_typ ty) name

  let local_copy ?(ghost=false) ?(suffix="_tmp") v =
    let name, typ, vi =
      match harden_var v with
      | NewVar (_id, name, typ) -> name, typ, None
      | CilVar vi -> vi.vname, vi.vtype, Some vi
    in
    let name = name ^ suffix in
    let fundec = get_owner () and b = top () in
    let ghost = ghost || b.ghost in
    let v =
      match vi with
      | None ->
        let temp = false and global = false and formal = false in
        Cil.makeVarinfo ~temp ~ghost global formal name typ
      | Some vi ->
        Cil.copyVarinfo vi name
    in
    v.vghost <- v.vghost || ghost;
    Cil.refresh_local_name fundec v;
    append_local v;
    `var (CilVar v)

  let parameter ?(ghost=false) ?(attributes=[]) typ name =
    let fundec = get_owner () and b = top () in
    let ghost = ghost || b.ghost in
    let loc = current_loc () in
    let v = Cil.makeFormalVar ~ghost ~loc fundec name typ in
    v.Cil_types.vattr <- attributes;
    `var (CilVar v)


  (* Instructions *)

  let of_instr i =
    let b = top () in
    append_instr b i

  let assign lval exp =
    let loc = current_loc () in
    let lval' = cil_lval ~loc lval
    and exp' = cil_exp ~loc exp in
    of_instr (Cil_types.Set (lval', exp', loc))

  let incr lval =
    assign lval (add lval (of_int 1))

  let call_ptr dest callee args =
    let loc = current_loc () in
    let dest' = cil_lval_opt ~loc dest
    and callee' = cil_lhost ~loc callee
    and args' = cil_exp_list ~loc args in
    of_instr (Cil_types.Call (dest', callee', args', loc))

  let call res callee args =
    let callee' = Kernel_function.get_vi callee in
    call_ptr res (var callee') args

  let pure exp =
    let loc = current_loc () in
    let exp' = cil_exp ~loc exp in
    match local' (Cil.typeOf exp') "tmp" ~init:(Exp.of_exp exp') with
    | `var (CilVar v) ->
      v.vdescr <- Some (Format.asprintf "%a" !Cil.pp_exp_ref exp')
    | _ -> assert false

  let skip () =
    let loc = current_loc () in
    of_instr (Cil_types.Skip (loc))


  (* Operators *)

  let (:=) = assign
  let (+=) lv e = assign lv (add lv e)
  let (-=) lv e = assign lv (sub lv e)
end
