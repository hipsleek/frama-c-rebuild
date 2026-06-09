(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let () = Plugin.is_share_visible ()
module P = Plugin.Register
    (struct
      let name = "E-ACSL"
      let shortname = "e-acsl"
      let help = "Executable ANSI/ISO C Specification Language --- runtime \
                  assertion checker generator"
    end)
include P

module Run =
  False
    (struct
      let option_name = "-e-acsl"
      let help = "generate a new project where E-ACSL annotations are \
                  translated to executable C code"
    end)

module Project_name =
  String
    (struct
      let option_name = "-e-acsl-project"
      let help = "the name of the generated project is <prj> \
                  (default to \"e-acsl\")"
      let default = "e-acsl"
      let arg_name = "prj"
    end)

module Valid =
  False
    (struct
      let option_name = "-e-acsl-valid"
      let help = "translate annotation which have been proven valid"
    end)

module Gmp_only =
  False
    (struct
      let option_name = "-e-acsl-gmp-only"
      let help = "always use GMP integers instead of C integral types"
    end)

module Temporal_validity =
  False
    (struct
      let option_name = "-e-acsl-temporal-validity"
      let help = "enable temporal analysis in valid annotations"
    end)

module Validate_format_strings =
  False
    (struct
      let option_name = "-e-acsl-validate-format-strings"
      let help = "enable runtime validation of stdio.h format functions"
    end)

module Replace_libc_functions =
  False
    (struct
      let option_name = "-e-acsl-replace-libc-functions"
      let help = "replace some libc functions (such as strcpy) with built-in\
                  RTL alternatives"
    end)

module Full_mtracking =
  False
    (struct
      let option_name = "-e-acsl-full-mtracking"
      let help = "maximal memory-related instrumentation"
    end)
let () = Full_mtracking.add_aliases ~deprecated:true [ "-e-acsl-full-mmodel" ]

module Builtins =
  String_set
    (struct
      let option_name = "-e-acsl-builtins"
      let arg_name = ""
      let help = "C functions which can be used in the E-ACSL specifications"
    end)

module Assert_print_data =
  True
    (struct
      let option_name = "-e-acsl-assert-print-data"
      let help = "print data contributing to the failed assertion along with \
                  the runtime error message"
    end)

module Concurrency =
  False
    (struct
      let option_name = "-e-acsl-concurrency"
      let help = "activate the concurrency support of E-ACSL. The option \
                  implies -e-acsl-full-mtracking."
    end)

module Functions =
  Kernel_function_set
    (struct
      let option_name = "-e-acsl-functions"
      let arg_name = "f1, ..., fn"
      let help = "only annotations in functions f1, ..., fn are checked at \
                  runtime"
    end)

module Instrument =
  Kernel_function_set
    (struct
      let option_name = "-e-acsl-instrument"
      let arg_name = "f1, ..., fn"
      let help = "only instrument functions f1, ..., fn. \
                  Be aware that runtime verdicts may become partial."
    end)

module Interlang =
  False
    (struct
      let option_name = "-e-acsl-interlang"
      let help = "try compilation based on intermediate language"
    end)

module Interlang_force =
  False
    (struct
      let option_name = "-e-acsl-interlang-force"
      let help = "crash if interlang compilation fails"
    end)


module O =
  Int
    (struct
      let default = 2
      let option_name = "-e-acsl-O"
      let arg_name = "n"
      let help = "Level of optimisation (defaults to 2). \
                  0 - No optimisation. \
                  1 - Constant-time optimisations. \
                  2 - Moderate-cost optimisations. \
                  3 - Potentially expensive optimisations, that may exploit \
                  undefined behaviours in specification."
    end)
let () = O.set_range ~min:0 ~max:3

module Widening_arguments_base =
  Int
    (struct
      let default = 1
      let option_name = "-e-acsl-widening-arguments-base"
      let arg_name = "n"
      let help = "widening strategy for arguments of recursive functions."
    end)
let () = Widening_arguments_base.set_range ~min:0 ~max:2

module Widening_arguments =
  String_map
    (Value_int)
    (struct
      let default = Datatype.String.Map.empty
      let option_name = "-e-acsl-widening-arguments"
      let arg_name = ""
      let help = "widening strategy for arguments of functions on a case by case \
                  basis."
    end)

module Widening_output_base =
  Int
    (struct
      let default = 1
      let option_name = "-e-acsl-widening-output-base"
      let arg_name = "n"
      let help = "wideining strategy for output of recursive functions."
    end)
let () = Widening_output_base.set_range ~min:0 ~max:2

module Widening_output =
  String_map
    (Value_int)
    (struct
      let default = Datatype.String.Map.empty
      let option_name = "-e-acsl-widening-output"
      let arg_name = ""
      let help = "widening strategy for output of recursive functions on a case
      by case basis."
    end)

let parameter_states =
  [ Valid.self;
    Gmp_only.self;
    Full_mtracking.self;
    Builtins.self;
    Temporal_validity.self;
    Validate_format_strings.self;
    Functions.self;
    Instrument.self;
    Widening_arguments_base.self;
    Widening_arguments.self;
    Widening_output.self;
    Widening_output_base.self ]

let emitter =
  Emitter.create
    "E_ACSL"
    [ Emitter.Code_annot; Emitter.Funspec ]
    ~correctness:[ Functions.parameter;
                   Instrument.parameter;
                   Validate_format_strings.parameter;
                   Temporal_validity.parameter ]
    ~tuning:[ Gmp_only.parameter;
              Valid.parameter;
              Replace_libc_functions.parameter;
              Full_mtracking.parameter;
              Widening_output_base.parameter;
              Widening_arguments.parameter;
              Widening_output_base.parameter;
              Widening_output.parameter ]

let must_visit () = Run.get ()

module Dkey = struct
  let prepare = register_category "preparation"
  let logic_normalizer = register_category "analysis:logic_normalizer"
  let bound_variables = register_category "analysis:bound_variables"
  let rte = register_category "analysis:rte"
  let inductive =
    let help = "extraction of an executable form from \
                (certain forms of) inductive predicate definitions" in
    register_category ~help "analysis:inductive"
  let interval = register_category "analysis:interval_inference"
  let mtracking = register_category "analysis:memory_tracking"
  let typing = register_category "analysis:typing"
  let labels = register_category "analysis:labels"
  let translation = register_category "translation"
  let env = register_category "translation:env"
  let interlang_translation =
    let help = "translation from the intermediate language to Cil" in
    register_category ~help "interlang:translation"
  let interlang_not_covered =
    let help = "encountered constructs unsupported by indirect compilation scheme" in
    register_category ~help "interlang:not_covered"
  let interlang_print_opt =
    let help = "prints a comparison with non-optimized expressions" in
    register_category ~help "interlang:print_opt"
end

let setup ?(rtl=false) () =
  (* Variadic translation *)
  if Kernel.VariadicTranslation.get () then begin
    if rtl then
      (* If we are translating the RTL project, then we need to deactivate the
         variadic translation. Indeed since we are translating the RTL in
         isolation, we do not now if the variadic functions are used by the
          user project and we cannot monomorphise them accordingly. *)
      Kernel.VariadicTranslation.off ()
    else if Validate_format_strings.get () then begin
      if Ast.is_computed () then
        abort
          "The variadic translation is incompatible with E-ACSL option \
           '%s'.@ Please use option '-no-variadic-translation'."
          Validate_format_strings.option_name;
      warning ~once:true "deactivating variadic translation";
      Kernel.VariadicTranslation.off ()
    end
  end;
  (* Concurrency support *)
  if Concurrency.get () then begin
    if Full_mtracking.is_set () && not (Full_mtracking.get ()) then
      abort
        "The memory tracking dataflow analysis is incompatible@ \
         with the concurrency support of E-ACSL.@ \
         Please use option '-e-acsl-full-mtracking'.";
    if not rtl && not (Full_mtracking.is_set ()) then
      feedback
        "Due to the large number of function pointers in concurrent@ \
         code, the memory tracking dataflow analysis is deactivated@ \
         when activating the concurrency support of E-ACSL.";
    Full_mtracking.on ();
    if Temporal_validity.get () then
      abort
        "The temporal analysis in valid annotations is incompatible@ \
         with the concurrency support of E-ACSL.@ \
         Please use '-e-acsl-no-temporal-validity' or '-e-acsl-no-concurrency'@ \
         to deactivate one or the other.";
    if rtl then
      Kernel.CppExtraArgs.add "-DE_ACSL_CONCURRENCY_PTHREAD"
  end;
  (* Additional kernel options while parsing the RTL project. *)
  if rtl then begin
    Kernel.KeepUnusedFunctions.set "none";
    Kernel.CppExtraArgs.add
      (Format.asprintf " -DE_ACSL_MACHDEP=%s" (Kernel.Machdep.get ()));
  end
