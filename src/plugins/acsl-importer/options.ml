(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Plug-in states and options. *)

include Plugin.Register
    (struct
      let name = "ACSL importer"
      let shortname = "acsl-import"
      let help = "external ACSL files importer"
    end)
let dkey = register_category "trace-options"

(** {1 Messages and warning categories} *)

let annot_error ?source fmt =
  Kernel.warning ~wkey:Kernel.wkey_annot_error ?source fmt

let annot_warning ?source ~raising fmt =
  Kernel.logwith (fun _ -> raising ()) ~wkey:Kernel.wkey_annot_error ?source fmt

let wkey_integer_cast = register_warn_category "annot:integer-cast"

(** {1 Plug-in options.} *)

module TypeOnly =
  False(struct
    let option_name = "-acsl-import-type-only"
    let help = "parses and types the ACSL files without imports"
  end)

let continue_after_typing () = not (TypeOnly.get ())

module ParseOnly =
  False(struct
    let option_name = "-acsl-import-parse-only"
    let help = "parses the ACSL files without typing nor imports"
  end)

let continue_after_parsing () = not (ParseOnly.get ())

module Import =
  String_list(struct
    let option_name = "-acsl-import"
    let arg_name = "f1,...,fn"
    let help = "ACSL files to import"
  end)

module KeepUnusedSymbols =
  False(struct
    let option_name = "-acsl-import-keep-unused-symbols"
    let help = "keeps unused C symbols"
  end)
let () = KeepUnusedSymbols.add_update_hook (fun _ v -> Rmtmps.keepUnused := v)

module AddonEnsuresAndExits =
  False(struct
    let option_name = "-acsl-import-addon-ensures-and-exits"
    let help = "adds ensures_and_exits extension clause"
  end)

module Idirs =
  String_list(struct
    let option_name = "-acsl-import-include-dirs"
    let arg_name = "d1,...,dn"
    let help = "directories for searching ACSL files to include"
  end)

module AddonIntegerCast =
  False
    (struct
      let option_name = "-acsl-import-addon-integer-cast"
      let help =
        "elucidates implicit and explicit casts from integers to C integral \
         types"
    end)

module Run =
  True
    (struct
      let option_name = "-acsl-import-run"
      let help =
        "runs the plugin (other options just configure its parameters)"
    end)

module UnroolLoopCondition =
  False
    (struct
      let option_name = "-acsl-import-unroll-loop-conditions"
      let help =
        "unrolls statements related to loop conditions"
    end)
let is_unroll_loop_condition_on = UnroolLoopCondition.get

let split_value s =
  let completely = "completely" in
  let rx2 = Str.regexp "@" in
  let b, s = match (try
                      Str.bounded_split_delim rx2 s 3
                    with _ -> failwith "cannot split from separator '@'") with
  | [ "" ; "" ] -> failwith "no directive"
  | [ total ; n ] when total = completely -> true, n
  | [ total ] when total = completely -> true, ""
  | [ n ] -> false, n
  | [  ] -> failwith "empty string"
  | _ -> failwith "too much directive separator '@'"
  in b, if s = "" then -1 else (try int_of_string s with _ -> failwith ("invalid unrolling number:" ^ s))

module UnroolLoopFunctionLevel =
  String_map
    (struct
      include Datatype.Pair(Datatype.Bool)(Datatype.Int)
      let of_string s =
        try
          debug ~level:2 ~dkey "Parsing value for \"-acsl-import-ulevel-spec=%s\"" s;
          let (b, n) as v  = split_value s
          in debug ~level:2 ~dkey  "-> unencoded value: %b, %d." b n;
          v
        with
        | Failure why -> raise (Cannot_build ( why))
      let to_string = function
        | (false,n) -> string_of_int n
        | (true,n) -> "completely@"^ string_of_int n
    end
    )
    (struct
      let option_name = "-acsl-import-ulevel-spec"
      let arg_name = "spec1,...,specs"
      let help = "an unrolling specification <m@f:tag@n> adds a 'loop unfold \"tag\", <n>;' to the loop of the function <f> of occurrence <m>.\n \
                  An unrolling specification <c@f:tag@n> adds a 'loop unfold \"tag\", <n>;' to all loops of category <c> of the function <f> where allowed loop categories are: 'while', 'for' and 'do-while'.\n \
                  The specification is considered as a set of elementary specifications: spec1,...,specs.\n \
                  Categories, function names and loop occurrence numbers can be omitted. \
                  The priority ordering used for choosing the (\"tag\", unrolling value <n>) pair is: m@f:tag@n > c@f:tag@n > f:tag@n > c:tag@n > :tag@n.\n \
                  The default value for optional tags is the empty string which leads to add a loop pragmas without tags.\n \
                  Nothing is done for loops having already a clause 'loop unfold ...'."
      let default = Datatype.String.Map.empty
    end)

let is_unroll_loop_pragma_on = UnroolLoopFunctionLevel.is_empty

let find_ulevel_spec loop_category loop_num fct_name =
  debug ~level:2 ~dkey "Find -acsl-import-ulevel-spec for %S loop #%d of function %S." loop_category loop_num fct_name;
  let (_, times) as spec =
    let total = ref false
    and times = ref (-1)
    in (try
          List.iter (fun f ->
              (try
                 let b, n =
                   let key = f () in
                   debug ~level:2 ~dkey  "-> keys=%S." key;
                   UnroolLoopFunctionLevel.find key
                 in debug ~level:2 ~dkey  "-> found %b, %d." b n;
                 if b then total := true;
                 if !times < 0 then times := n;
               with Not_found -> ()) ;
              if !total && !times >= 0 then raise Not_found)
            [ (fun () -> (string_of_int loop_num) ^ "@" ^ fct_name) ;
              (fun () -> loop_category ^ "@" ^ fct_name);
              (fun () -> fct_name) ;
              (fun () -> loop_category) ;
              (fun () -> "")] ;
        with Not_found -> ()) ;
    debug ~level:2 ~dkey  "Found finally %b, %d." !total !times;
    !total, !times
  in if times < 0 then raise Not_found ;
  spec

let is_importation_on () = (not (Import.is_empty ()) && Run.get () )

let set_importation_off () = Import.set []

let emitter =
  Emitter.create
    "ACSL Importer"
    [ Emitter.Global_annot; Emitter.Code_annot ]
    ~correctness:[]
    ~tuning:[]

let main_import =
  File.register_code_transformation_category "acsl importer"

let aux_import =
  File.register_code_transformation_category "acsl importer transform"
