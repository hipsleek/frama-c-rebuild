open Cabs

[@@@warning "-32-27"]

let loc = Fileloc.unknown

let gen_int_type =
  Crowbar.(choose [
      const Tint;
      const Tlong;
      const Tunsigned;
    ])

let gen_type =
  Crowbar.(choose [
      gen_int_type;
      const Tfloat;
      const Tdouble;
    ])

let mk_exp expr_node = { expr_loc = loc; expr_node }

let needs_int_unary = function
  | NOT | BNOT -> true
  | _ -> false

let gen_unary_op =
  Crowbar.(choose [
      const NOT;
      const BNOT;
      const MINUS;
      const PLUS;
    ])

(* NB: we don't generate shifts and division/modulo operands to avoid
   undefined operations. Overflows alarms are deactivated as well. *)

let needs_int_binary = function
  | AND | OR | BAND | BOR | XOR -> true
  | _ -> false

let gen_binary_op =
  let open Crowbar in
  choose [
    const AND;
    const OR;
    const BAND;
    const BOR;
    const XOR;
    const ADD;
    const SUB;
    const MUL;
    const EQ;
    const NE;
    const LT;
    const GT;
    const LE;
    const GE;
  ]



(* int32 generator as the default machdep is 32 bit.
   Moreover, we only generate positive integers here, as negative ones are
   supposed to be given by unary -
*)
let gen_constant =
  let open Crowbar in
  choose [
    map [ range 4 ]
      (fun i -> mk_exp (CONSTANT (CONST_INT (string_of_int i))));
    map [ float ]
      (fun f ->
         let up = 4.0 in
         let f = if f < 0.0 || Float.is_nan f then 0. else f in
         let f = if f > up then up else f in
         mk_exp (CONSTANT (CONST_FLOAT (string_of_float f))))
  ]

let mk_cast t e = mk_exp (CAST (([SpecType t],JUSTBASE), SINGLE_INIT e))

let protected_cast t e =
  let max = mk_exp (CONSTANT(CONST_INT("255"))) in
  let min =
    match t with
    | Tunsigned -> mk_exp(CONSTANT(CONST_INT("0")))
    | _ ->  mk_exp (UNARY(MINUS,max))
  in
  let maxr = mk_cast t max in
  let minr = mk_cast t min in
  mk_exp(
    QUESTION(
      mk_exp(BINARY(GE,e,min)),
      mk_exp(QUESTION(mk_exp(BINARY(LE,e,max)),e,maxr)),
      minr))

let gen_cast t e =
  let e = protected_cast t e in
  mk_exp (CAST (([SpecType t],JUSTBASE), SINGLE_INIT e))

let gen_unary t op e =
  let e = if needs_int_unary op then gen_cast t e else e in
  mk_exp (UNARY (op,e))

let gen_binary t op e1 e2 =
  let e1,e2 =
    if needs_int_binary op then
      gen_cast t e1, gen_cast t e2
    else e1,e2
  in
  mk_exp (BINARY (op,e1,e2))

let gen_question c et ef =
  mk_exp (QUESTION (c,et,ef))

let rec gen_expr_l n =
  if n <= 0 then lazy gen_constant
  else lazy (
    let open Crowbar in
    choose [
      gen_constant;
      map [ gen_int_type; gen_unary_op; gen_expr (n-1)] gen_unary;
      map [ gen_int_type; gen_binary_op; gen_expr (n-1); gen_expr (n-1) ] gen_binary;
      map [ gen_expr (n-1); gen_expr (n-1); gen_expr (n-1)] gen_question;
      map [ gen_type; gen_expr (n-1) ] gen_cast;
    ])
and gen_expr n = Crowbar.unlazy (gen_expr_l n)

let gen_cabs typ expr =
  let expr = protected_cast typ expr in
  (Filepath.empty,
   [ false,
     DECDEF(
       None,
       ([SpecType typ],
        [("a",
          ARRAY(JUSTBASE,[],{ expr_loc = loc; expr_node = NOTHING}),[],loc),
         COMPOUND_INIT [NEXT_INIT, SINGLE_INIT expr]]),
       loc);
     false,
     DECDEF(None,([SpecType Tint],[("result", JUSTBASE,[],loc),NO_INIT]),loc);
     false,
     FUNDEF(
       None,([SpecType Tvoid],("f", PROTO(JUSTBASE,[],[],false),[],loc)),
       { blabels = [];
         battrs = [];
         bstmts = [
           { stmt_ghost = false;
             stmt_node =
               DEFINITION(
                 DECDEF(
                   None,
                   ([SpecType typ], [("x",JUSTBASE,[],loc),SINGLE_INIT expr]),
                   loc))};
           { stmt_ghost = false;
             stmt_node =
               COMPUTATION(
                 mk_exp(
                   BINARY(
                     ASSIGN,
                     mk_exp (VARIABLE "result"),
                     mk_exp (
                       BINARY(
                         EQ,
                         mk_exp (VARIABLE "x"),
                         mk_exp(
                           INDEX(
                             mk_exp (VARIABLE "a"),
                             mk_exp (CONSTANT (CONST_INT "0")))))))), loc)}
         ]
       },
       loc,loc)])

let () = Project.set_current (Project.create "my_project")

let run typ expr =
  Project.clear ();
  let cabs = gen_cabs typ expr in
  Kernel.SignedOverflow.off ();
  Kernel.SignedDowncast.off ();
  Kernel.UnsignedOverflow.off ();
  Kernel.UnsignedDowncast.off ();
  Kernel.UnsignedOverflow.off ();
  Kernel.set_warn_status Kernel.wkey_decimal_float Log.Winactive;
  Eva.Parameters.Verbose.set 0;
  (* otherwise, we must load scope in addition to eva. *)
  Dynamic.Parameter.Bool.off "-eva-remove-redundant-alarms" ();
  Errorloc.clear_errors ();
  let cil =
    try Cabs2cil.convFile cabs
    with exn ->
      Crowbar.failf "@[<v2>Failed to typecheck cabs: %s@\n%a@]@."
        (Printexc.to_string exn)
        Cprint.printFile cabs
  in
  if Errorloc.had_errors () then begin
    Crowbar.failf "@[<v2>Failed to typecheck cabs (had errors)@\n%a@]@."
      Cprint.printFile cabs
  end;
  File.init_cil();
  File.prepare_cil_file cil;
  Kernel.MainFunction.set "f";
  Eva.Analysis.compute ();
  let kf = Globals.Functions.find_by_name "f" in
  let r = Globals.Vars.find_from_astinfo "result" Cil_types.Global in
  let ret = Kernel_function.find_return kf in
  let expr = Cil.evar ~loc r in
  let v1 = Eva.Results.(before ret |> eval_exp expr |> as_cvalue) in
  let itv =
    try Cvalue.V.project_ival v1
    with exn ->
      Crowbar.failf "@[<v2>Eva analysis did not reduce to a constant: %s@\n%t@]@."
        (Printexc.to_string exn)
        (fun fmt -> File.pretty_ast ~fmt ())
  in
  if not (Ival.is_one itv) then begin
    Crowbar.failf "@[<v2>Const fold did not reduce to identical value:@\n%t@]@."
      (fun fmt -> File.pretty_ast ~fmt ())
  end

let f () = Crowbar.add_test ~name:"constfold" [gen_type; gen_expr 2] run

let () = Crowbar_utils.run "constfold" f
