(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_types
module Error = Translation_error

(**************************************************************************)
(************************* Calls to builtins ******************************)
(**************************************************************************)

let apply_on_var ~loc funname e =
  let prefix =
    let ty = Cil.typeOf e in
    if Gmp_types.Z.is_t ty then "__gmpz_"
    else if Gmp_types.Q.is_t ty then "__gmpq_"
    else Options.fatal "expected GMP type instead of %a" Printer.pp_typ ty
  in
  Smart_stmt.rtl_call ~loc ~prefix funname [ e ]

let init ~loc e = apply_on_var "init" ~loc e
let clear loc e = apply_on_var "clear" ~loc e

exception Longlong of ikind

let get_set_suffix_and_arg res_ty e =
  let ty = Cil.typeOf e in
  let exp_number_ty = Typing.number_ty_of_typ ~post:true ty in
  let res_number_ty = Typing.number_ty_of_typ ~post:true res_ty in
  let args_uisi e =
    if Gmp_types.Z.is_t res_ty then [ e ]
    else begin
      assert (Gmp_types.Q.is_t res_ty);
      [ e; Cil.one ~loc:e.eloc ]
    end
  in
  match (exp_number_ty, res_number_ty) with
  | Gmpz, Gmpz | Rational, Rational -> "", [ e ]
  | Gmpz, Rational -> "_z", [ e ]
  | Rational, Gmpz -> "_q", [ e ]
  | C_integer IChar, _ ->
    (if Machine.char_is_unsigned () then "_ui"
     else "_si"),
    args_uisi e
  | C_integer (IBool | IUChar | IUInt | IUShort | IULong), _ ->
    "_ui", args_uisi e
  | C_integer (ISChar | IShort | IInt | ILong), _ -> "_si", args_uisi e
  | C_integer (ILongLong | IULongLong as ikind), _ -> raise (Longlong ikind)
  | C_integer (IInt128 | IUInt128 as ikind), _ ->
    raise (Longlong ikind) (* TODO: use a more specific exception? *)
  | C_float (FFloat | FFloat32 | FFloat64 | FDouble), _ -> "_d", [ e ]
  (* FFloat is a strict subset of FDouble (modulo exceptional numbers)
     Hence, calling [set_d] for both of them is sound.
     HOWEVER: the machdep MUST NOT be vulnerable to double rounding
     [TODO] check the statement above *)
  | C_float FLongDouble, _ -> Error.not_yet "creating gmp from long double"
  | Gmpz, _ | Rational, _ | Real, _ | Nan, _ ->
    match Ast_types.unroll_node ty with
    | TPtr { tnode = TInt IChar } ->
      "_str",
      (* decimal base for the number given as string *)
      [ e; Cil.integer ~loc:e.eloc 10 ]
    | _ ->
      Options.fatal "expected char* instead of type %a" Printer.pp_typ ty

let generic_assign ~loc fname lv ev e =
  let ty = Cil.typeOf ev in
  if Gmp_types.Z.is_t ty || Gmp_types.Q.is_t ty then begin
    let suf, args = get_set_suffix_and_arg ty e in
    Smart_stmt.rtl_call ~loc ~prefix:"" (fname ^ suf) (ev :: args)
  end else
    Smart_stmt.assigns ~loc:e.eloc ~result:lv e

let assign ~loc lv ev e =
  let fname =
    let ty = Cil.typeOf ev in
    if Gmp_types.Z.is_t ty then "__gmpz_set"
    else if Gmp_types.Q.is_t ty then "__gmpq_set"
    else ""
  in
  try generic_assign ~loc fname lv ev e
  with Longlong _ ->
    Error.not_yet "quantification over long long and requiring GMP"

let init_set ~loc lv ev e =
  let mpz_init_set fname =
    try generic_assign ~loc fname lv ev e
    with
    | Longlong IULongLong ->
      (match e.enode with
       | Lval elv ->
         assert (Gmp_types.Z.is_t (Cil.typeOf ev));
         let call =
           Smart_stmt.rtl_call ~loc
             ~prefix:""
             "__gmpz_import"
             [ ev;
               Cil.one ~loc;
               Cil.one ~loc;
               Cil.sizeOf ~loc Cil_const.ulongLongType;
               Cil.zero ~loc;
               Cil.zero ~loc;
               Cil.mkAddrOf ~loc elv ]
         in
         Smart_stmt.block_stmt (Cil.mkBlock [ init ~loc ev; call ])
       | _ ->
         Error.not_yet "unsigned long long expression requiring GMP")
    | Longlong ILongLong ->
      Error.not_yet "long long requiring GMP"
  in
  let ty = Cil.typeOf ev in
  if Gmp_types.Z.is_t ty then
    mpz_init_set "__gmpz_init_set"
  else if Gmp_types.Q.is_t ty then
    Smart_stmt.block_stmt
      (Cil.mkBlock [ init ~loc ev; assign ~loc lv ev e ])
  else
    mpz_init_set ""

module Z = struct

  let name_arith_bop = function
    | PlusA -> "__gmpz_add"
    | MinusA -> "__gmpz_sub"
    | Mult -> "__gmpz_mul"
    | Div -> "__gmpz_tdiv_q"
    | Mod -> "__gmpz_tdiv_r"
    | BAnd -> "__gmpz_and"
    | BOr -> "__gmpz_ior"
    | BXor -> "__gmpz_xor"
    | Shiftlt -> "__gmpz_mul_2exp"
    | Shiftrt -> "__gmpz_tdiv_q_2exp"
    | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr | PlusPI | MinusPI
    | MinusPP as bop ->
      Options.fatal
        "Operation '%a' not supported on GMP integers"
        Printer.pp_binop bop

  let new_var ~loc ?scope ?name env kf t mk_stmts =
    let _, e, env =
      Env.new_var
        ~loc
        ?scope
        ?name
        env
        kf
        t
        (Gmp_types.Z.t ())
        (fun v e -> init ~loc e :: mk_stmts v e)
    in
    e, env

  let create ~loc ?name t_opt env kf e =
    let _, e, env =
      Env.new_var
        ~loc
        ?name
        env
        kf
        t_opt
        (Gmp_types.Z.t ())
        (fun vi vi_e -> [ init_set ~loc (Cil.var vi) vi_e e ])
    in
    e, env

  let add_cast ~loc ?name env kf ty e =
    let fname, new_ty =
      if Cil.isSignedInteger ty then "__gmpz_get_si", Cil_const.longType
      else "__gmpz_get_ui", Cil_const.ulongType
    in
    let _, e, env =
      Env.new_var
        ~loc
        ?name
        env
        kf
        None
        new_ty
        (fun v _ ->
           [ Smart_stmt.rtl_call ~loc
               ~result:(Cil.var v)
               ~prefix:""
               fname
               [ e ] ])
    in
    e, env

  let binop ~loc t_opt bop env kf e1 e2 =
    let name = name_arith_bop bop in
    let mk_stmts _ e = [ Smart_stmt.rtl_call ~loc
                           ~prefix:""
                           name
                           [ e; e1; e2 ] ] in
    let name = Misc.name_of_binop bop in
    let e, env = new_var ~loc ~name env kf t_opt mk_stmts in
    e,env

  let cmp ~loc name t_opt bop env kf e1 e2 =
    let _, e, env = Env.new_var
        ~loc
        env
        kf
        t_opt
        ~name
        Cil_const.intType
        (fun v _ ->
           [ Smart_stmt.rtl_call ~loc
               ~result:(Cil.var v)
               ~prefix:""
               "__gmpz_cmp"
               [ e1; e2 ] ])
    in
    Cil.new_exp ~loc (BinOp(bop, e, Cil.zero ~loc, Cil_const.intType)), env

end

module Q = struct
  let name_arith_bop = function
    | PlusA -> "__gmpq_add"
    | MinusA -> "__gmpq_sub"
    | Mult -> "__gmpq_mul"
    | Div -> "__gmpq_div"
    | Mod | Lt | Gt | Le | Ge | Eq | Ne | BAnd | BXor | BOr | LAnd | LOr
    | Shiftlt | Shiftrt | PlusPI | MinusPI | MinusPP as bop ->
      Options.fatal
        "Operation '%a' not supported on GMP rationals"
        Printer.pp_binop bop

  exception Not_a_decimal of string
  exception Is_a_float

  (* The possible float suffixes (ISO C 6.4.4.2) are lLfF.
     dD is a GNU extension accepted by Frama-C (only!) in the logic *)
  let float_suffixes = [ 'f'; 'F'; 'l'; 'L'; 'd'; 'D' ]

  (* Computes the fractional representation of a decimal number.
     Does NOT perform reduction.
     Example: [dec_to_frac "43.567"] evaluates to ["43567/1000"]
     Complexity: Linear
     Original Author: Frédéric Recoules

     It iterates **once** over [str] during which three cases are distinguished,
     example for "43.567":
     Case1: pre: no '.' has been found yet ==> copy current char into buf
      buf: | 4 |   |   |   |   |   |   |   |   |   |   |   |
           | 4 | 3 |   |   |   |   |   |   |   |   |   |   |
     Case2: mid: current char is '.' ==> put "/1" into buf at [(length str) - 1]
      buf: | 4 | 3 |   |   |   | / | 1 |   |   |   |   |   |
     Case3: post: a '.' was found ==> put current char in numerator AND '0' in den
      buf: | 4 | 3 | 5 |   |   | / | 1 | 0 |   |   |   |   |
           | 4 | 3 | 5 | 6 |   | / | 1 | 0 | 0 |   |   |   |
           | 4 | 3 | 5 | 6 | 7 | / | 1 | 0 | 0 | 0 |   |   | *)
  let decimal_to_fractional str =
    let rec post str len buf len' i =
      if i = len then
        Bytes.sub_string buf 0 len'
      else
        match String.unsafe_get str i with
        | c when '0' <= c && c <= '9' ->
          Bytes.unsafe_set buf (i - 1) c;
          Bytes.unsafe_set buf len' '0';
          post str len buf (len' + 1) (i + 1)
        | c when List.mem c float_suffixes ->
          (* [JS] a suffix denoting a C type is possible *)
          assert (i = len - 1);
          raise Is_a_float
        | _ ->
          raise (Not_a_decimal str)
    in
    let mid buf len =
      Bytes.unsafe_set buf (len - 1) '/';
      Bytes.unsafe_set buf len '1'
    in
    let rec pre str len buf i =
      if i = len then
        str
      else
        match String.unsafe_get str i with
        | '-' when i = 0 ->
          Bytes.unsafe_set buf i '-';
          pre str len buf (i + 1)
        | '.' ->
          mid buf len;
          post str len buf (len + 1) (i + 1)
        | c when '0' <= c && c <= '9' ->
          Bytes.unsafe_set buf i c;
          pre str len buf (i + 1)
        | c when List.mem c float_suffixes ->
          (* [JS] a suffix denoting a C type is possible *)
          assert (i = len - 1);
          raise Is_a_float
        | _ ->
          raise (Not_a_decimal str)
    in
    let strlen = String.length str in
    let buflen =
      (* The fractional representation is at most twice as lengthy
         as the decimal one. *)
      2 * strlen
    in
    try pre str strlen (Bytes.create buflen) 0
    with Is_a_float -> str (* just left it unchanged *)

  (* ACSL considers strings written in decimal expansion to be reals.
     Yet GMPQ considers them to be double:
     they MUST be converted into fractional representation. *)
  let normalize_str str =
    try
      decimal_to_fractional str
    with Invalid_argument _ ->
      Error.not_yet "number not written in decimal expansion"

  let create ~loc ?name t_opt env kf e =
    let ty = Cil.typeOf e in
    if Gmp_types.Q.is_t ty then
      e, env
    else
      let _, e, env =
        Env.new_var
          ~loc
          ?name
          env
          kf
          t_opt
          (Gmp_types.Q.t ())
          (fun vi vi_e -> [ init_set ~loc (Cil.var vi) vi_e e ])
      in
      e, env

  let cast_to_z ~loc:_ ?name:_ _env e =
    assert (Gmp_types.Q.is_t (Cil.typeOf e));
    Error.not_yet "reals: cast from R to Z"

  let add_cast ~loc ?name env kf ty e =
    (* TODO: The best solution would actually be to directly write all the
       needed functions as C builtins then just call them here depending on the
       situation at hand. *)
    assert (Gmp_types.Q.is_t (Cil.typeOf e));
    let get_double e env =
      let _, e, env =
        Env.new_var
          ~loc
          ?name
          env
          kf
          None
          Cil_const.doubleType
          (fun v _ ->
             [ Smart_stmt.rtl_call ~loc
                 ~result:(Cil.var v)
                 ~prefix:""
                 "__gmpq_get_d"
                 [ e ] ])
      in
      e, env
    in
    match Ast_types.unroll_node ty with
    | TFloat FLongDouble ->
      (* The biggest floating-point type we can extract from GMPQ is double *)
      Error.not_yet "R to long double"
    | TFloat FDouble ->
      get_double e env
    | TFloat FFloat ->
      (* No "get_float" in GMPQ, but fortunately, [float] \subset [double].
         HOWEVER: going through double as intermediate step might be unsound
         since it could cause double rounding. See: [Boldo2013, Sec 2.2]
         https://hal.inria.fr/hal-00777639/document *)
      let e, env = get_double e env in
      Options.warning
        ~once:true "R to float: double rounding might cause unsoundness";
      Cil.mkCastT ~force:false ~oldt:Cil_const.doubleType ~newt:ty e, env
    | TInt IULongLong ->
      (* The biggest C integer type we can extract from GMP is ulong *)
      Error.not_yet "R to unsigned long long"
    | TInt _ ->
      (* 1) Cast R to Z using cast_to_z
         2) Extract ulong from Z
         3) Potentially cast ulong to ty *)
      Error.not_yet "R to Int"
    | _ ->
      Error.not_yet "R to <typ>"

  let new_var ~loc ?scope ?name env kf t_opt mk_stmts =
    let _, e, env = Env.new_var
        ~loc
        ?scope
        ?name
        env
        kf
        t_opt
        (Gmp_types.Q.t ())
        (fun v e -> init ~loc e :: mk_stmts v e)
    in
    e, env

  let binop ~loc t_opt bop env kf e1 e2 =
    let name = name_arith_bop bop in
    let e1, env = create ~loc None env kf e1 in
    let e2, env = create ~loc None env kf e2 in
    let mk_stmts _ e = [ Smart_stmt.rtl_call ~loc
                           ~prefix:""
                           name
                           [ e; e1; e2 ] ] in
    let name = Misc.name_of_binop bop in
    let e, env = new_var ~loc ~name env kf t_opt mk_stmts in
    e, env

  let cmp ~loc name t_opt bop env kf e1 e2 =
    let fname = "__gmpq_cmp" in
    let e1, env = create ~loc None env kf e1 in
    let e2, env = create ~loc None env kf e2 in
    let _, e, env =
      Env.new_var
        ~loc
        env
        kf
        t_opt
        ~name
        Cil_const.intType
        (fun v _ ->
           [ Smart_stmt.rtl_call ~loc
               ~result:(Cil.var v)
               ~prefix:""
               fname
               [ e1; e2 ] ])
    in
    Cil.new_exp ~loc (BinOp(bop, e, Cil.zero ~loc, Cil_const.intType)), env

end

let () =
  Env.gmp_clear_ref := clear
