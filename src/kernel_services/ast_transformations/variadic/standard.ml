(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Va_types
module Typ = Extends.Typ

let pp_prototype name fmt tparams =
  Format.fprintf fmt "%s(%a)"
    name
    (Pretty_utils.pp_list ~sep:", " Cil_printer.pp_typ) tparams

let pp_overload name fmt l =
  let prototypes = List.map fst l in
  Pretty_utils.pp_list ~sep:"@\n" (pp_prototype name) fmt prototypes


(* State to store the number of fallback functions generated for a particular
   function name. This number is used to generate a unique function name. *)
module Fallback_counts =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Datatype.Int)
    (struct
      let size = 17
      let name = "fallback_counts"
      let dependencies =
        [ Kernel.VariadicTranslation.self; Kernel.VariadicStrict.self ]
    end)

(* ************************************************************************ *)
(* Call translation                                                         *)
(* ************************************************************************ *)

exception Translate_call_exn of varinfo

(* Extended integer types (e.g. int8_t, uint_least16_t, int_fast32_t)
   do not have their own character modifiers, but instead use macros that are
   converted into "standard" modifiers (e.g. "%hhd", "%hu", "%d", etc.).
   Therefore, we cannot enforce their types the same way as for e.g. size_t,
   which has its own modifier. We weaken the check, allowing a different name
   but still requiring same size and signedness. *)
let extended_integer_typenames =
  ["int8_t"; "uint8_t"; "int_least8_t"; "uint_least8_t";
   "int_fast8_t"; "uint_fast8_t";
   "int16_t"; "uint16_t"; "int_least16_t"; "uint_least16_t";
   "int_fast16_t"; "uint_fast16_t";
   "int32_t"; "uint32_t"; "int_least32_t"; "uint_least32_t";
   "int_fast32_t"; "uint_fast32_t";
   "int64_t"; "uint64_t"; "int_least64_t"; "uint_least64_t";
   "int_fast64_t"; "uint_fast64_t"]

let is_extended_integer_type t =
  match t.tnode with
  | TNamed ti -> List.mem ti.tname extended_integer_typenames
  | _ -> false

let integral_rep ikind =
  Cil.bitsSizeOfInt ikind, Cil.isSigned ikind

let expose t =
  Ast_types.remove_attributes_for_c_cast (Ast_types.unroll t)

(* From most permissive to least permissive *)
type castability = Strict      (* strictly allowed by the C standard *)
                 | Tolerated   (* tolerated in practice *)
                 | NonPortable (* non-portable minor deviation *)
                 | NonStrict   (* only allowed in non-strict mode *)
                 | Never       (* never allowed *)

let can_cast given expected =
  let t1 = expose given
  and t2 = expose expected in
  match t1.tnode, t2.tnode with
  | _, _ when Cil_datatype.Typ.equal t1 t2 -> Strict
  | (TInt i1 | TEnum {ekind=i1}),
    (TInt i2 | TEnum {ekind=i2}) ->
    if integral_rep i1 <> integral_rep i2 ||
       not (Cil_datatype.Attributes.equal t1.tattr t2.tattr) then
      Never
    else if is_extended_integer_type given then
      Tolerated
    else if i1 = i2 then
      NonPortable
    else
      NonStrict
  | TPtr _, TPtr _ -> Strict
  | _, _ -> Never

let does_fit exp typ =
  match Cil.constFoldToInt exp, Ast_types.unroll_node typ with
  | Some i, (TInt ekind | TEnum {ekind}) ->
    Cil.fitsInInt ekind i
  | _ -> false

(* Variant of [pp_typ] which details the underlying type for enums *)
let pretty_typ fmt t =
  match Ast_types.unroll_node t with
  | TEnum ei ->
    Format.fprintf fmt "%a (%a)" Cil_printer.pp_typ t
      Cil_printer.pp_typ (Cil_const.mk_tint ei.ekind)
  | _ -> Cil_printer.pp_typ fmt t

(* cast the i-th argument exp to paramtyp *)
let cast_arg i paramtyp exp =
  let argtyp = Cil.typeOf exp in
  if not (does_fit exp paramtyp) then
    begin match can_cast argtyp paramtyp with
      | Strict | Tolerated -> ()
      | (NonPortable | NonStrict) when not (Kernel.VariadicStrict.get ()) -> ()
      | NonPortable ->
        Kernel.warning ~current:true ~wkey:Kernel.wkey_typing
          "Possible portability issues with enum type for argument %d \
           (use -no-variadic-strict to avoid this warning)."
          (i + 1)
      | NonStrict | Never ->
        Kernel.warning ~current:true ~wkey:Kernel.wkey_typing
          "Incorrect type for argument %d. \
           The argument will be cast from %a to %a."
          (i + 1)
          pretty_typ argtyp pretty_typ paramtyp
    end;
  Cil.mkCast ~check:false ~force:false ~newt:paramtyp exp


(* cast a list of args to the tparams list of types and remove unused args *)
let match_args ~callee tparams args =
  (* Remove unused arguments *)
  let paramcount = List.length tparams
  and argcount = List.length args in
  if argcount > paramcount  then
    Kernel.warning ~current:true ~wkey:Kernel.wkey_typing
      "Too many arguments: expected %d, given %d. \
       Superfluous arguments will be removed."
      paramcount argcount
  else if argcount < paramcount then (
    Kernel.warning ~current:true ~wkey:Kernel.wkey_typing
      "Not enough arguments: expected %d, given %d."
      paramcount argcount;
    raise (Translate_call_exn callee)
  );

  (* Translate params *)
  let new_args, unused_args = List.break paramcount args in
  List.mapi2 cast_arg tparams new_args, unused_args


(* translate a call by applying argument matching/pruning and changing
   callee *)
let match_call ~builder new_callee new_tparams args =
  let module Build = (val builder : Builder.S) in

  let new_args, unused_args = match_args ~callee:new_callee new_tparams args in
  Build.start_translation ();
  Build.(List.iter pure (of_exp_list unused_args));
  Build.(translated_call (var new_callee) (of_exp_list new_args))


let check_available_name env name =
  not @@ Environment.mem_global env name
  && not @@ Environment.mem_function env name

let get_next_name env id name =
  let rec aux id =
    let next_name = name ^ (string_of_int id) in
    if check_available_name env next_name then
      next_name, id
    else
      aux (id + 1)
  in
  aux id

(* ************************************************************************ *)
(* Fallback calls                                                           *)
(* ************************************************************************ *)

let fallback_fun_call ~builder ~callee env vf args =
  let module Build = (val builder : Builder.S) in

  (* Choose function name *)
  let name = callee.vname in
  let vorig_name = callee.vorig_name in
  let count = try Fallback_counts.find name + 1 with Not_found -> 1 in
  let new_name, new_count = get_next_name env count (name ^ "_fallback_") in
  Fallback_counts.replace name new_count;

  (* Start building the function *)
  let loc = vf.vf_decl.vdecl in
  let new_callee = Build.open_function ~loc ~vorig_name new_name in

  (* Set function return type and attributes *)
  let ret_typ, params, _, attrs = Cil.splitFunctionType vf.vf_original_type in
  Build.set_return_type' ret_typ;
  List.iter Build.add_attribute attrs;
  Build.add_stdlib_generated ();

  (* Add parameters *)
  let fixed_params_count = Typ.params_count vf.vf_original_type in
  let gen_param_name =
    let i = ref (-1) in
    fun () ->
      incr i;
      "param" ^ string_of_int !i
  in
  let add_static_param (name,typ,attributes) =
    let name =
      if String.equal name ""
      then gen_param_name ()
      else name
    in
    ignore (Build.parameter ~attributes typ name)
  and add_variadic_param e =
    let typ = Cil.typeOf e in
    let name = gen_param_name () in
    ignore (Build.parameter typ name)
  in
  List.iter add_static_param (Option.get params);
  List.iter add_variadic_param (List.drop fixed_params_count args);

  (* Add guessed specifications *)
  Build.infer_assigns ();

  (* Finish building the function *)
  Build.finish_declaration ();

  (* Store the translation *)
  Replacements.add (Build.cil_varinfo new_callee) vf.vf_decl;

  (* Translate the call *)
  Build.start_translation ();
  Kernel.result ~current:true ~dkey:Kernel.dkey_variadic
    "Fallback translation of call %s to a call to the specialized version %a. \
     Generating default assigns that may be inaccurate."
    vf.vf_decl.vorig_name Cil_printer.pp_varinfo (Build.cil_varinfo new_callee);
  Build.(translated_call new_callee (List.map of_exp args))


(* ************************************************************************ *)
(* Aggregator calls                                                         *)
(* ************************************************************************ *)

let find_null exp_list =
  List.find_index (fun e -> Cil.isZero (Cil.constFold false e)) exp_list


let aggregator_call ~builder aggregator vf args =
  let module Build = (val builder : Builder.S) in

  let {a_target; a_pos; a_type; a_param} = aggregator in
  let name = vf.vf_decl.vorig_name
  and tparams = Typ.params_types a_target.vtype
  and pname, ptyp = a_param in

  (* Check argument count *)
  let argcount = List.length args
  and paramcount = List.length tparams in
  if argcount < paramcount then begin
    Kernel.warning ~current:true ~wkey:Kernel.wkey_typing
      "Not enough arguments: expected %d, given %d."
      paramcount argcount;
    raise (Translate_call_exn vf.vf_decl);
  end;

  (* Compute the size of the aggregation *)
  let size = match a_type with
    | EndedByNull ->
      match find_null (List.drop a_pos args) with
      | Some i -> i  + 1
      | None ->
        Kernel.warning ~current:true
          "Failed to find a sentinel (NULL pointer) in the argument list.";
        raise (Translate_call_exn vf.vf_decl);
  in

  (* Convert arguments *)
  let tparams_left = List.take a_pos tparams in
  let tparams_right = List.drop (a_pos + 1) tparams in
  let tparams_new = List.init size (fun _ -> ptyp)  in
  let new_tparams = tparams_left @ tparams_new @ tparams_right in
  let new_args, unused_args = match_args ~callee:vf.vf_decl new_tparams args in

  (* Split the arguments *)
  let args_left, args_rem = List.break a_pos new_args in
  let args_middle, args_right = List.break size args_rem in

  (* Create the call code  *)
  Kernel.result ~current:true ~dkey:Kernel.dkey_variadic
    "Translating call to %s to a call to %s."
    name a_target.vorig_name;
  let pname = if pname = "" then "param" else pname in

  Build.start_translation ();
  let init = match args_middle with (* C standard forbids arrays of size 0 *)
    | [] -> [Build.(of_init (Cil.makeZeroInit ~loc ptyp))]
    | l -> List.map Build.of_exp l
  in
  let size = List.length init in
  let vaggr = Build.(local (array ~size (of_ctyp ptyp)) pname ~init) in
  let new_args = args_left @ [Build.(cil_exp ~loc (addr vaggr))] @ args_right in
  let new_args,_ = match_args ~callee:vf.vf_decl tparams new_args in
  Build.(List.iter pure (of_exp_list unused_args));
  Build.(translated_call (var a_target) (of_exp_list new_args))


(* ************************************************************************ *)
(* Overloads calls                                                          *)
(* ************************************************************************ *)

let rec check_arg_matching expected given =
  match Ast_types.unroll_node given, Ast_types.unroll_node expected with
  | (TInt _ | TEnum _), (TInt _ | TEnum _) -> true
  | TPtr _, _ when Ast_types.is_void_ptr expected -> true
  | TPtr t1, TPtr t2 -> check_arg_matching t1 t2
  | _, _ -> not (Cil.need_cast given expected)


let rec check_call_matching tparams targs =
  match tparams, targs with
  | [], [] -> true
  | [], _
  (* too many args: this is allowed by the standard (the extra arguments
     are ignored), but in practice this leads to disambiguation issues in
     some cases (e.g. last argument is 0 instead of NULL), so we prefer to
     be strict *)
  (* Not enough input args *)
  | _, [] -> false
  | a1 :: l1, a2 :: l2 ->
    check_arg_matching a1 a2 &&
    check_call_matching l1 l2


let filter_matching_prototypes overload args =
  (* Find suitable candidates for this call *)
  let targs = List.map Cil.typeOf args in
  let check (tparams, _vi) = check_call_matching tparams targs in
  List.filter check overload


let overloaded_call ~builder overload vf args =
  let name = vf.vf_decl.vorig_name in

  (* Find the matching prototype *)
  let tparams, new_callee =
    match filter_matching_prototypes overload args with
    | [] -> (* No matching prototype *)
      Kernel.warning ~current:true ~wkey:Kernel.wkey_prototype
        "@[No matching prototype found for this call to %s.@.\
         Expected candidates:@.\
         @[<v>       %a@]@.\
         Given arguments:@.\
         @[<v>       %a@]"
        name (pp_overload name) overload
        (pp_prototype name) (List.map Cil.typeOf args);
      raise (Translate_call_exn vf.vf_decl);
    | [(tparams,vi)] -> (* Exactly one matching prototype *)
      tparams, vi
    | l -> (* Several matching prototypes *)
      Kernel.warning ~current:true ~wkey:Kernel.wkey_prototype
        "Ambiguous call to %s. Matching candidates are: \
         %a"
        name
        (pp_overload name) l;
      raise (Translate_call_exn vf.vf_decl);
  in

  (* Store the translation *)
  Replacements.add new_callee vf.vf_decl;

  (* Rebuild the call *)
  Kernel.result ~current:true ~dkey:Kernel.dkey_variadic
    "Translating call to the specialized version %a."
    (pp_prototype name) tparams;
  match_call ~builder new_callee tparams args



(* ************************************************************************ *)
(* Format functions calls                                                   *)
(* ************************************************************************ *)

(* --- Specification building --- *)

let rec static_string env a = match a.enode with
  | StartOf(Var s, NoOffset) ->
    (match Environment.find_global_init env s.vname with
     | None -> None
     | Some (StrInit (Str s)) -> Some (Format_string.String s)
     | Some (StrInit (Wstr s)) -> Some (Format_string.WString s)
     | Some (CInit _) | exception Not_found -> None)
  | CastE (_, e) -> static_string env e
  | _ -> None

let find_global env name =
  try
    Some (Environment.find_global env name)
  with Not_found ->
    Kernel.warning ~once:true ~wkey:Kernel.wkey_libc_framac
      "Unable to locate global %s which should be in the Frama-C LibC. \
       Correct specifications can't be generated."
      name;
    None

let find_predicate name =
  match Logic_env.find_all_logic_functions name with
  | f :: _q -> f (* TODO: should we warn in case of overloading? *)
  | [] ->
    Kernel.warning ~once:true ~wkey:Kernel.wkey_libc_framac
      "Unable to locate ACSL predicate %s which should be in the Frama-C LibC. \
       Correct specifications can't be generated."
      name;
    raise Not_found

let find_field env structname fieldname =
  try
    let compinfo = Environment.find_struct env structname in
    Cil.getCompField compinfo fieldname
  with Not_found ->
    Kernel.warning ~once:true
      "Unable to locate %s field %s."
      structname fieldname;
    raise Not_found

let find_predicate_by_width typ narrow_name wide_name =
  match Ast_types.unroll_deep_node typ with
  | TPtr { tnode = TInt IChar } -> find_predicate narrow_name
  | TPtr t when
      (* drop attributes to remove 'const' qualifiers and fc_stdlib attributes *)
      Cil_datatype.Typ.equal
        (Cil.typeDeepDropAllAttributes (Ast_types.unroll_deep t))
        (Machine.wchar_type ()) ->
    find_predicate wide_name
  | _ ->
    Kernel.warning ~current:true ~wkey:Kernel.wkey_typing
      "expected single/wide character pointer type, got %a (%a, unrolled %a)"
      Cil_printer.pp_typ typ
      Cil_types.pp_typ typ
      Cil_types.pp_typ (Ast_types.unroll_deep typ);
    raise Not_found

let valid_read_string typ =
  find_predicate_by_width typ "valid_read_string" "valid_read_wstring"

let valid_read_nstring typ =
  find_predicate_by_width typ "valid_read_nstring" "valid_read_nwstring"

let format_length typ =
  find_predicate_by_width typ "format_length" "wformat_length"

let build_specialized_fun ~builder env vf format_fun tvparams =
  let open Format_types in
  let module Build = (val builder : Builder.S) in

  (* Choose function name *)
  let name = vf.vf_decl.vorig_name in
  let id = vf.vf_specialization_count + 1 in
  let new_name, new_count = get_next_name env id (name ^ "_va_") in
  vf.vf_specialization_count <- new_count;

  (* Start building the function *)
  let loc = vf.vf_decl.vdecl in
  let funvar = Build.open_function ~loc ~vorig_name:name new_name in

  (* Set function return type and attributes *)
  let ret_typ, params, _, attrs = Cil.splitFunctionType vf.vf_original_type in
  Build.set_return_type' ret_typ;
  List.iter Build.add_attribute attrs;
  Build.add_stdlib_generated ();

  (* Add parameters *)
  let fresh_param_name =
    let counter = ref 0 in
    fun () ->
      let p = "param" ^ string_of_int !counter in
      incr counter;
      p
  in
  let add_static_param (name,typ,attributes) =
    (* create a new name for parameters which does not have one *)
    let name = if name = "" then fresh_param_name () else name in
    Build.parameter ~attributes typ name
  and add_variadic_param (typ,_dir) =
    let typ = if Ast_types.is_integral typ then
        Cil.integralPromotion typ
      else
        typ
    in
    Build.parameter typ (fresh_param_name ())
  in
  let sformals = List.map add_static_param (Option.get params)
  and vformals = List.map add_variadic_param tvparams
  in


  (* Spec *)
  let sources = ref [] and dests = ref [] in
  let add_source ?(indirect=false) s =
    let s = (s :> Build.source) in
    sources := (if indirect then Build.indirect s else s) :: !sources
  and add_dest d =
    dests := (d :> Build.exp) :: !dests
  in
  (* Add the lval to the list of sources/dests *)
  let add_lval ~indirect lval = function
    | (`ArgIn | `ArgInArray _) -> add_source ~indirect lval
    | (`ArgOut | `ArgOutArray) -> add_dest lval
    | `ArgInOut -> add_source ~indirect lval; add_dest lval
  in
  let add_var ?pos ~indirect (v : Build.var) dir =
    let ty = Build.cil_typeof v in
    (* Use the appropriate logical lval *)
    let lval = match dir with
      | `ArgIn -> (v :> Build.lval)
      | (`ArgInArray _ | `ArgOutArray) -> Build.(index v whole_right)
      | (`ArgOut | `ArgInOut) -> Build.(mem v)
    in
    add_lval ~indirect lval dir;
    (* Build requires/ensures *)
    try match dir with
      | `ArgInArray None ->
        Build.(requires (app (valid_read_string ty) [here] [v]))

      | `ArgInArray (Some precision) ->
        let nterm = match precision with
          | PStar ->
            let size_arg = List.nth vformals (Option.get pos - 1) in
            Build.(cast integer size_arg)
          | PInt n ->
            Build.of_int n
        in
        Build.(requires (app (valid_read_nstring ty) [here] [(v :> Build.exp) ; nterm]))

      | `ArgOut ->
        Build.(requires (valid v));
        Build.(ensures (initialized v))

      | _ -> ()
    with Not_found -> () (* Predicate not found *)
  in


  (* Build variadic parameter source/dest list *)
  let l = List.combine vformals tvparams in
  List.iteri (fun pos (v,(_,dir)) -> add_var ~indirect:false ~pos v dir) l;

  (* Add format source and additional parameters *)
  let fmt = List.nth sformals format_fun.f_format_pos in
  add_var ~indirect:true fmt (`ArgInArray None);

  (* Add buffer source/dest *)
  let add_stream v =
    (* assigns stream->__fc_FILE_data
         \from stream->__fc_FILE_data, __fc_FILE_id *)
    try
      let f_data = find_field env "__fc_FILE" "__fc_FILE_data"
      and f_id = find_field env "__fc_FILE" "__fc_FILE_id" in
      add_lval ~indirect:false Build.(field v f_data) `ArgInOut;
      add_lval ~indirect:true Build.(field v f_id) `ArgIn
    with Not_found ->
      add_var ~indirect:false v `ArgInOut
  in

  (* Add a bounded buffer *)
  let add_buffer buffer size =
    add_var ~indirect:true size `ArgIn;
    (* this is an snprintf-like function; compute and add its precondition:
       \valid(s + (0..n-1)) || \valid(s + (0..format_length(format)-1)) *)
    let valid_range length =
      Build.(valid (buffer + (zero -- (length - one))))
    in
    let left_pred = valid_range size in
    try
      let flen = format_length (Build.cil_typeof buffer) in
      let right_pred = Build.(valid_range (app flen [here] [fmt])) in
      Build.(requires (logor left_pred right_pred))
    with
    | Not_found -> Build.requires left_pred
    | Build.NotAFunction li ->
      Kernel.abort ~current:true
        "%a should be a logic function, not a predicate"
        Cil_printer.pp_logic_var li.l_var_info
  in

  begin match format_fun.f_buffer, format_fun.f_kind with
    | StdIO, ScanfLike ->
      begin match find_global env "__fc_stdin" with
        | Some vi -> add_stream (Build.var vi)
        | None -> ()
      end
    | StdIO, PrintfLike ->
      begin match find_global env "__fc_stdout" with
        | Some vi -> add_stream (Build.var vi)
        | None -> ()
      end
    | Arg (i, _), ScanfLike ->
      add_var ~indirect:true (List.nth sformals i) (`ArgInArray None)
    | Arg (i, size_pos), PrintfLike ->
      add_var ~indirect:true (List.nth sformals i) (`ArgOutArray);
      begin match size_pos with
        | Some n ->
          add_buffer (List.nth sformals i) (List.nth sformals n)
        | None -> ()
      end
    | Stream i, _ ->
      add_stream (List.nth sformals i)
    | File i, _ ->
      let file = List.nth sformals i in
      add_var ~indirect:true file `ArgIn;
    | Syslog, _ -> ()
  end;

  (* assign \result \from indirect:sources *)
  if not (Ast_types.is_void ret_typ) then
    Build.(assigns [result] (List.map indirect !sources));
  (* assigns dests \from sources *)
  Build.assigns !dests !sources;

  (* Build the default behaviour *)
  Build.finish_declaration ();
  Build.cil_varinfo funvar


(* --- Call translation --- *)

let format_of_ikind = function
  | IBool -> Some `hh, `u
  | IChar -> None, `c
  | ISChar -> Some `hh, `d
  | IUChar -> Some `hh, `u
  | IInt -> None, `d
  | IUInt -> None, `u
  | IShort -> Some `h, `d
  | IUShort -> Some `h, `u
  | ILong -> Some `l, `d
  | IULong -> Some `l, `u
  | ILongLong -> Some `ll, `d
  | IULongLong -> Some `ll, `u
  | IInt128 | IUInt128 -> Kernel.fatal "128-bit integers not allowed in formats"

let format_of_fkind k = function
  | FFloat   -> None, `f
  | FFloat32 -> None, `f32
  | FFloat64 -> None, `f64
  | FDouble ->
    (match k with
     | Format_types.PrintfLike -> None, `f
     | ScanfLike -> Some `l, `f)
  | FLongDouble -> Some `L, `f

let rec format_of_type vf k t =
  match t.tnode with
  | TInt ikind | TEnum {ekind = ikind} -> format_of_ikind ikind
  | TFloat fkind -> format_of_fkind k fkind
  | TPtr _ ->
    (* technically, we might still want to write/read the actual pointer,
       but this is not the most likely possibility. *)
    if Ast_types.is_char_ptr t then
      None, `s
    else
      None, `p
  | TNamed {tname; ttype} ->
    (match tname with
     | "size_t" -> Some `z, `u
     | "ptrdiff_t" -> Some `t, `d
     | "intmax_t" -> Some `j, `d
     | "uintmax_t" -> Some `j, `u
     (* not really standard, but that's what glibc does. *)
     | "ssize_t" -> Some `z, `d
     | _ -> format_of_type vf k ttype
    )
  (* in the case of a scanf-like function, it might happen
     that we pass a void* whose actual type is coherent with
     the format string itself, but this can't really be checked
     here.
  *)
  | TVoid -> raise (Translate_call_exn vf.vf_decl)

  (* these cases should not happen anyway *)
  | TComp _
  | TFun _
  | TArray _
  | TBuiltin_va_list -> raise (Translate_call_exn vf.vf_decl)

let infer_format_from_args vf format_fun args =
  let args = List.drop (format_fun.f_format_pos + 1) args in
  let f_format (l,s) =
    Format_types.(
      Specification
        { f_flags = [];
          f_field_width = None;
          f_precision = None;
          f_length_modifier = l;
          f_conversion_specifier = s;
          f_capitalize = false;
        })
  in
  let s_format (l,s) =
    Format_types.(
      Specification
        { s_assignment_suppression = false;
          s_field_width = None;
          s_length_modifier = l;
          s_conversion_specifier = s;
        }
    )
  in
  let treat_one_arg arg =
    let t = Cil.typeOf arg in
    let t =
      match format_fun.f_kind with
      | PrintfLike -> t
      | ScanfLike ->
        if not (Ast_types.is_ptr t) then begin
          let source = fst arg.eloc in
          Kernel.warning ~source ~wkey:Kernel.wkey_typing
            "Expecting pointer as parameter of scanf function. \
             Argument %a has type %a"
            Cil_printer.pp_exp arg Cil_printer.pp_typ t;
          raise (Translate_call_exn vf.vf_decl);
        end;
        Ast_types.direct_pointed_type t
    in
    format_of_type vf format_fun.f_kind t
  in
  let format = List.map treat_one_arg args in
  match format_fun.f_kind with
  | PrintfLike -> Format_types.FFormat (List.map f_format format)
  | ScanfLike -> Format_types.SFormat (List.map s_format format)

let format_fun_call ~builder env format_fun vf args =
  (* Extract the format if possible *)
  let format =
    try
      let format_arg = List.nth args format_fun.f_format_pos in
      match static_string env format_arg with
      | None ->
        Kernel.warning ~current:true
          ~wkey:Kernel.wkey_variadic_format_nonliteral
          "Call to function %s with non-static format argument: \
           assuming that parameters are coherent with the format, and that \
           no %%n specifiers are present in the actual string."
          vf.vf_decl.vorig_name;
        infer_format_from_args vf format_fun args
      | Some s -> Format_parser.parse_format format_fun.f_kind s
    with
    | Format_parser.Invalid_format -> raise (Translate_call_exn vf.vf_decl)
  in

  (* Try to type expected parameters if possible *)
  let find_typedef = Environment.find_type env in
  let tvparams =
    try
      Format_typer.type_format ~find_typedef format
    with Format_typer.Type_not_found (type_name) ->
      Kernel.warning ~current:true
        "Unable to find type %s in the source code which should be used in \
         this call:@ no specification will be generated.@ \
         Note that due to cleanup, the type may have been defined in the \
         original code but not used anywhere.@.\
         Did you include <stdint.h> ?" type_name;
      raise (Translate_call_exn vf.vf_decl)
  in

  (* Create the new callee *)
  let new_callee = build_specialized_fun ~builder env vf format_fun tvparams in

  (* Store the translation *)
  Replacements.add new_callee vf.vf_decl;

  (* Translate the call *)
  Kernel.result ~current:true ~dkey:Kernel.dkey_variadic
    "Translating call to %s to a call to the specialized version %s."
    vf.vf_decl.vorig_name new_callee.vname;
  let tparams = Typ.params_types new_callee.vtype in
  match_call ~builder new_callee tparams args
