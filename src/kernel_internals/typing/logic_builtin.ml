(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

open Cil_types

let add = Logic_env.add_builtin_logic_function_gen
    Logic_utils.is_same_builtin_profile

let register builtin =
  Logic_env.Builtins.extend (fun () -> add builtin)

let float_type = Ctype Cil_const.floatType
let float32_type = Ctype Cil_const.float32Type
let float64_type = Ctype Cil_const.float64Type
let double_type = Ctype Cil_const.doubleType
let long_double_type = Ctype Cil_const.longDoubleType
let object_ptr = Ctype Cil_const.voidPtrType
let fun_ptr =
  Ctype Cil_const.(mk_tptr (mk_tfun voidType None false))

let polymorphic_type name = name, Lvar name

let init =
  let called = ref false in
  (* Since hooks are not projectified this function must be added exactly
     once per session, otherwise we might end up with several built-ins with
     the same name.
  *)
  fun () ->
    if !called then (fun () -> ())
    else begin
      called:=true;
      fun () ->
        (* types *)
        (*
          let tvar v = new_identified_term (tvar v) in
         *)
        let set =
          { lt_name = "set"; lt_params=["elem"]; lt_def=None; lt_attr=[] }
        in
        let typetag =
          { lt_name = "typetag"; lt_params = []; lt_def = None; lt_attr = [] }
        in
        let sign =
          {lt_name = "sign"; lt_params = []; lt_def = None; lt_attr = [] }
        in
        let float_format =
          {lt_name = "float_format"; lt_params = []; lt_def = None; lt_attr=[]}
        in
        let rounding_mode =
          {lt_name = "rounding_mode"; lt_params = []; lt_def = None; lt_attr=[]}
        in
        List.iter (fun x -> Logic_env.add_builtin_logic_type x.lt_name x)
          [ set; typetag; sign; float_format; rounding_mode ];
        (* constructors *)
        List.iter
          (fun (typename, constrs) ->
             let l =
               List.map
                 (fun cname ->
                    let c =
                      { ctor_name = cname; ctor_type = typename;
                        ctor_params = [] }
                    in
                    Logic_env.add_builtin_logic_ctor cname c;
                    c)
                 constrs
             in
             typename.lt_def <- Some (LTsum l))
          [ sign , [ "\\Positive"; "\\Negative"] ;
            float_format, [ "\\Single"; "\\Double"; "\\Quad" ] ;
            rounding_mode, [ "\\Up"; "\\Down"; "\\ToZero"; "\\NearestAway";
                             "\\NearestEven" ];
          ];
        (* logic types used by the builtins *)
        let a_name, a_type = polymorphic_type "a" in
        let sign = Ltype(sign,[]) in
        let float_format = Ltype(float_format,[]) in
        let rounding_mode = Ltype(rounding_mode,[]) in
        let set_of_integer = Ltype (set, [Linteger]) in
        let set_of_a_type = Ltype (set, [a_type]) in
        let void_array = Cil_builder.Type.(cil_logic_type (array void)) in
        (* "\list" logic type with its constructors *)
        let list =
          { lt_name="\\list"; lt_params=[a_name]; lt_def=None; lt_attr=[]}
        in
        Logic_env.add_builtin_logic_type list.lt_name list ;
        let list_of_a_type = Ltype (list, [a_type]) in
        let nil =
          { ctor_name = "\\Nil"; ctor_type = list; ctor_params = [] }
        in
        let cons = {
          ctor_name = "\\Cons";
          ctor_type = list;
          ctor_params = [a_type; list_of_a_type] }
        in
        let ctors = [nil ; cons] in
        List.iter
          (fun c -> Logic_env.add_builtin_logic_ctor c.ctor_name c) ctors ;
        list.lt_def <- Some (LTsum ctors);
        let _list_of_integer = Ltype (list, [Linteger]) in
        let l_label = FormalLabel "L" in
        (* predicates *)
        List.iter
          (fun (f,tparams,labels,params)  ->
             add { bl_name = f; bl_params = tparams; bl_profile = params;
                   bl_type = None; bl_labels = labels})
          [ "\\is_finite", [], [], ["x", float_type] ;
            "\\is_finite", [], [], ["x", float32_type] ;
            "\\is_finite", [], [], ["x", float64_type] ;
            "\\is_finite", [], [], ["x", double_type] ;
            "\\is_finite", [], [], ["x", long_double_type] ;
            "\\is_infinite", [], [], ["x", float_type] ;
            "\\is_infinite", [], [], ["x", float32_type] ;
            "\\is_infinite", [], [], ["x", float64_type] ;
            "\\is_infinite", [], [], ["x", double_type] ;
            "\\is_infinite", [], [], ["x", long_double_type] ;
            "\\is_NaN", [], [], ["x", float_type] ;
            "\\is_NaN", [], [], ["x", float32_type] ;
            "\\is_NaN", [], [], ["x", float64_type] ;
            "\\is_NaN", [], [], ["x", double_type] ;
            "\\is_NaN", [], [], ["x", long_double_type] ;
            "\\is_minus_infinity", [], [], ["x", float_type] ;
            "\\is_minus_infinity", [], [], ["x", float32_type] ;
            "\\is_minus_infinity", [], [], ["x", float64_type] ;
            "\\is_minus_infinity", [], [], ["x", double_type] ;
            "\\is_minus_infinity", [], [], ["x", long_double_type] ;
            "\\is_plus_infinity", [], [], ["x", float_type] ;
            "\\is_plus_infinity", [], [], ["x", float32_type] ;
            "\\is_plus_infinity", [], [], ["x", float64_type] ;
            "\\is_plus_infinity", [], [], ["x", double_type] ;
            "\\is_plus_infinity", [], [], ["x", long_double_type] ;
            "\\le_float", [], [], ["x", float_type; "y", float_type];
            "\\lt_float", [], [], ["x", float_type; "y", float_type];
            "\\ge_float", [], [], ["x", float_type; "y", float_type];
            "\\gt_float", [], [], ["x", float_type; "y", float_type];
            "\\eq_float", [], [], ["x", float_type; "y", float_type];
            "\\ne_float", [], [], ["x", float_type; "y", float_type];
            "\\le_float32", [], [], ["x", float32_type; "y", float32_type];
            "\\lt_float32", [], [], ["x", float32_type; "y", float32_type];
            "\\ge_float32", [], [], ["x", float32_type; "y", float32_type];
            "\\gt_float32", [], [], ["x", float32_type; "y", float32_type];
            "\\eq_float32", [], [], ["x", float32_type; "y", float32_type];
            "\\ne_float32", [], [], ["x", float32_type; "y", float32_type];
            "\\le_float64", [], [], ["x", float64_type; "y", float64_type];
            "\\lt_float64", [], [], ["x", float64_type; "y", float64_type];
            "\\ge_float64", [], [], ["x", float64_type; "y", float64_type];
            "\\gt_float64", [], [], ["x", float64_type; "y", float64_type];
            "\\eq_float64", [], [], ["x", float64_type; "y", float64_type];
            "\\ne_float64", [], [], ["x", float64_type; "y", float64_type];
            "\\le_double", [], [], ["x", double_type; "y", double_type];
            "\\lt_double", [], [], ["x", double_type; "y", double_type];
            "\\ge_double", [], [], ["x", double_type; "y", double_type];
            "\\gt_double", [], [], ["x", double_type; "y", double_type];
            "\\eq_double", [], [], ["x", double_type; "y", double_type];
            "\\ne_double", [], [], ["x", double_type; "y", double_type];
            "\\no_overflow_single", [], [], ["m", rounding_mode; "x", Lreal] ;
            "\\no_overflow_double", [], [], ["m", rounding_mode; "x", Lreal] ;
            "\\subset", [a_name], [], ["s1", set_of_a_type;
                                       "s2", set_of_a_type];
            "\\pointer_comparable", [], [l_label], [("p1", object_ptr);
                                                    ("p2", object_ptr)];
            "\\pointer_comparable", [], [l_label], [("p1", fun_ptr);
                                                    ("p2", fun_ptr)];
            "\\pointer_comparable", [], [l_label], [("p1", fun_ptr);
                                                    ("p2", object_ptr)];
            "\\pointer_comparable", [], [l_label], [("p1", object_ptr);
                                                    ("p2", fun_ptr)];
          ];
        (* functions *)
        List.iter
          (fun (f,tparams,params,ret_type)  ->
             add { bl_name = f; bl_params = tparams; bl_profile = params;
                   bl_type = Some ret_type; bl_labels = []})
          [
            "\\min", [], ["x",Linteger;"y",Linteger], Linteger ;
            "\\max", [], ["x",Linteger;"y",Linteger], Linteger ;
            "\\min", [], ["x",Lreal;"y",Lreal], Lreal ;
            "\\max", [], ["x",Lreal;"y",Lreal], Lreal ;

            "\\abs", [], ["x",Linteger], Linteger ;
            "\\labs", [], ["x",Linteger], Linteger ;
            "\\abs", [], ["x",Lreal], Lreal ;
            "\\fabs", [], ["x",Lreal], Lreal ;

            "\\sqrt", [], ["x",Lreal], Lreal ;

            "\\ceil", [], ["x",Lreal], Linteger ;
            "\\floor", [], ["x",Lreal], Linteger ;
            "\\truncate", [], ["x",Lreal], Linteger ;

            (* transcendental functions *)
            "\\exp", [], ["x",Lreal], Lreal ;
            "\\log", [], ["x",Lreal], Lreal ;
            "\\log10", [], ["x",Lreal], Lreal ;

            "\\e", [], [], Lreal ; (* \log(\e) == 1 *)
            "\\pi", [], [], Lreal ;

            "\\cos", [], ["x",Lreal], Lreal ;
            "\\sin", [], ["x",Lreal], Lreal ;
            "\\tan", [], ["x",Lreal], Lreal ;

            "\\cosh", [], ["x",Lreal], Lreal ;
            "\\sinh", [], ["x",Lreal], Lreal ;
            "\\tanh", [], ["x",Lreal], Lreal ;

            "\\acos", [], ["x",Lreal], Lreal ;
            "\\asin", [], ["x",Lreal], Lreal ;
            "\\atan", [], ["x",Lreal], Lreal ;

            "\\hypot", [], ["x",Lreal;"y",Lreal], Lreal ;
            "\\atan2", [], ["x",Lreal;"y",Lreal], Lreal ;
            "\\pow", [], ["x",Lreal;"y",Lreal], Lreal ;
            "\\fmod", [], ["x",Lreal;"y",Lreal], Lreal ;

            "sqrt", [], ["x",double_type], double_type ;
            "atan2", [], ["x",double_type;"y",double_type], double_type ;
            "pow", [], ["x",double_type;"y",double_type], double_type ;
            "fmod", [], ["x",double_type;"y",double_type], double_type ;

            "sqrtf", [], ["x",float_type], float_type ;
            "atan2f", [], ["x",float_type;"y",float_type], float_type ;
            "powf", [], ["x",float_type;"y",float_type], float_type ;
            "fmodf", [], ["x",float_type;"y",float_type], float_type ;

            (* TODO ?
             * div() frexp() ldexp()
             * ldiv() modf() modf()
            *)

            "\\sum", [], ["min",Linteger;
                          "max", Linteger;
                          "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\sum", [], ["min",Linteger;
                          "max", Linteger;
                          "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\product", [], ["min",Linteger;
                              "max", Linteger;
                              "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\product", [], ["min",Linteger;
                              "max", Linteger;
                              "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\min", [], ["min",Linteger;
                          "max", Linteger;
                          "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\min", [], ["min",Linteger;
                          "max", Linteger;
                          "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\max", [], ["min",Linteger;
                          "max", Linteger;
                          "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\max", [], ["min",Linteger;
                          "max", Linteger;
                          "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\numof", [], ["min",Linteger;
                            "max", Linteger;
                            "f",(Larrow ([Linteger],Lboolean))], Linteger ;


            (* for floats special values *)

            "\\round_float", [],
            ["f", float_format; "m", rounding_mode; "x", Lreal], Lreal ;

            "\\sign", [], ["x",float_type], sign ;
            "\\sign", [], ["x",float32_type], sign ;
            "\\sign", [], ["x",float64_type], sign ;
            "\\sign", [], ["x",double_type], sign ;
            "\\sign", [], ["x",long_double_type], sign ;

            "\\model", [], ["x",float_type], Lreal;
            "\\model", [], ["x",float32_type], Lreal;
            "\\model", [], ["x",float64_type], Lreal;
            "\\model", [], ["x",double_type], Lreal;
            (*"\\model", [], ["x",long_double_type], Lreal;*)

            "\\exact", [], ["x",float_type], Lreal;
            "\\exact", [], ["x",float32_type], Lreal;
            "\\exact", [], ["x",float64_type], Lreal;
            "\\exact", [], ["x",double_type], Lreal;
            (*"\\exact", [], ["x",long_double_type], Lreal;*)

            "\\total_error", [], ["x",float_type], Lreal;
            "\\total_error", [], ["x",float32_type], Lreal;
            "\\total_error", [], ["x",float64_type], Lreal;
            "\\total_error", [], ["x",double_type], Lreal;
            (*"\\total_error", [], ["x",long_double_type], Lreal;*)

            "\\round_error", [], ["x",float_type], Lreal;
            "\\round_error", [], ["x",float32_type], Lreal;
            "\\round_error", [], ["x",float64_type], Lreal;
            "\\round_error", [], ["x",double_type], Lreal;
            (*"\\round_error", [], ["x",long_double_type], Lreal;*)

            "\\relative_error", [], ["x",float_type], Lreal;
            "\\relative_error", [], ["x",float32_type], Lreal;
            "\\relative_error", [], ["x",float64_type], Lreal;
            "\\relative_error", [], ["x",double_type], Lreal;
            (*"\\relative_error", [], ["x",long_double_type], Lreal;*)

            "\\round_float", [],
            ["m",  rounding_mode; "x", Lreal], float_type;
            "\\round_double", [],
            ["m", rounding_mode ; "x", Lreal], double_type;
            "\\plus_infinity", [], [], float_type;
            "\\minus_infinity", [], [], float_type;
            "\\NaN", [], [], float_type;
            (*"\\round_quad", [],
               ["m",  rounding_mode; "x", Lreal], long_double_type;*)

            "\\neg_float",[],["x",float_type], float_type;
            "\\add_float",[],["x",float_type;"y",float_type], float_type;
            "\\sub_float",[],["x",float_type;"y",float_type], float_type;
            "\\mul_float",[],["x",float_type;"y",float_type], float_type;
            "\\div_float",[],["x",float_type;"y",float_type], float_type;

            "\\neg_float32",[],["x",float32_type], float32_type;
            "\\add_float32",[],["x",float32_type;"y",float32_type], float32_type;
            "\\sub_float32",[],["x",float32_type;"y",float32_type], float32_type;
            "\\mul_float32",[],["x",float32_type;"y",float32_type], float32_type;
            "\\div_float32",[],["x",float32_type;"y",float32_type], float32_type;

            "\\neg_float64",[],["x",float64_type], float64_type;
            "\\add_float64",[],["x",float64_type;"y",float64_type], float64_type;
            "\\sub_float64",[],["x",float64_type;"y",float64_type], float64_type;
            "\\mul_float64",[],["x",float64_type;"y",float64_type], float64_type;
            "\\div_float64",[],["x",float64_type;"y",float64_type], float64_type;

            "\\neg_double",[],["x",double_type], double_type;
            "\\add_double",[],["x",double_type;"y",double_type], double_type;
            "\\sub_double",[],["x",double_type;"y",double_type], double_type;
            "\\mul_double",[],["x",double_type;"y",double_type], double_type;
            "\\div_double",[],["x",double_type;"y",double_type], double_type;

            "\\min", [], ["s", set_of_integer], Linteger;
            "\\max", [], ["s", set_of_integer], Linteger;

            "\\nth", [a_name],
            ["l", list_of_a_type; "n", Linteger], a_type;
            "\\length", [a_name],
            ["l", list_of_a_type], Linteger;
            "\\length", [], ["a", void_array], Linteger;
            "\\concat", [a_name],
            ["l1", list_of_a_type; "l2", list_of_a_type], list_of_a_type;
            "\\repeat", [a_name],
            ["l", list_of_a_type; "n", Linteger], list_of_a_type;

          ]
    end
