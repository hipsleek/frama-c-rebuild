(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Self

(* Dependencies to kernel options *)
let kernel_parameters_correctness = [
  Kernel.MainFunction.parameter;
  Kernel.LibEntry.parameter;
  Kernel.AbsoluteValidRange.parameter;
  Kernel.InitializedPaddingLocals.parameter;
  Kernel.SafeArrays.parameter;
  Kernel.UnspecifiedAccess.parameter;
  Kernel.SignedOverflow.parameter;
  Kernel.UnsignedOverflow.parameter;
  Kernel.LeftShiftNegative.parameter;
  Kernel.RightShiftNegative.parameter;
  Kernel.SignedDowncast.parameter;
  Kernel.UnsignedDowncast.parameter;
  Kernel.PointerDowncast.parameter;
  Kernel.SpecialFloat.parameter;
  Kernel.InvalidBool.parameter;
  Kernel.InvalidPointer.parameter;
  Kernel.UnalignedPointer.parameter;
]

let parameters_correctness = ref Typed_parameter.Set.empty
let parameters_tuning = ref Typed_parameter.Set.empty
let add_dep p =
  let state = State.get p.Typed_parameter.name in
  State_builder.Proxy.extend [state] Self.proxy
let add_correctness_dep p =
  if Typed_parameter.Set.mem p !parameters_correctness then
    Kernel.abort "adding correctness parameter %a twice"
      Typed_parameter.pretty p;
  add_dep p;
  parameters_correctness := Typed_parameter.Set.add p !parameters_correctness
let add_precision_dep p =
  if Typed_parameter.Set.mem p !parameters_tuning then
    Kernel.abort "adding tuning parameter %a twice"
      Typed_parameter.pretty p;
  add_dep p;
  parameters_tuning := Typed_parameter.Set.add p !parameters_tuning

let () = List.iter add_correctness_dep kernel_parameters_correctness

module Eva =
  False
    (struct
      let option_name = "-eva"
      let help = "Run the Eva analysis."
    end)
let () = Eva.add_aliases ~deprecated:true ["-val"]

let domains = add_group "Abstract Domains"
let precision_tuning = add_group "Precision vs. time"
let initial_context = add_group "Initial Context"
let performance = add_group "Results memoization vs. time"
let interpreter = add_group "Deterministic programs"
let alarms = add_group "Propagation and alarms "
let malloc = add_group "Dynamic allocation"

(* -------------------------------------------------------------------------- *)
(* --- Eva domains                                                        --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group domains
module Domains =
  Filled_string_set
    (struct
      let option_name = "-eva-domains"
      let arg_name = "d1,...,dn"
      let help = "Enable a list of analysis domains."
      let default = Datatype.String.Set.of_list ["cvalue"]
    end)
let () = add_precision_dep Domains.parameter

let remove_domain name =
  Domains.set (Datatype.String.Set.filter ((!=) name) (Domains.get ()))

(* For backward compatibility, creates an invisible option for the domain [name]
   that sets up -eva-domains with [name]. To be removed one day. *)
let create_domain_option name =
  let option_name =
    match name with
    | "apron-box" -> "-eva-apron-box"
    | "apron-octagon" -> "-eva-apron-oct"
    | "apron-polka-loose" -> "-eva-polka-loose"
    | "apron-polka-strict" -> "-eva-polka-strict"
    | "apron-polka-equality" -> "-eva-polka-equalities"
    | _ -> "-eva-" ^ name ^ "-domain"
  in
  let module Input = struct
    let option_name = option_name
    let help = "Use the " ^ name ^ " domain of eva."
    let default = name = "cvalue"
  end in
  Parameter_customize.set_group domains;
  Parameter_customize.is_invisible ();
  let module Parameter = Bool (Input) in
  Parameter.add_set_hook
    (fun _old _new ->
       warning "Option %s is deprecated. Use -eva-domains %s%s instead."
         option_name (if _new then "" else "-") name;
       if _new then Domains.add name else remove_domain name)

let () = Parameter_customize.set_group performance
module NoResultsDomain =
  String_set
    (struct
      let option_name = "-eva-no-results-domain"
      let arg_name = "domains"
      let help = "Do not record the states of some domains during the analysis."
    end)
let () = add_dep NoResultsDomain.parameter

(* List ((name, descr), priority) of available domains. *)
let domains_ref : ((string * string) * int) list ref = ref []

(* Sort domains by decreasing priority. *)
let sorted_domains () =
  let cmp_domain ((name1, _), prio1) ((name2, _), prio2) =
    let b = prio2 - prio1 in
    if b <> 0 then b else Stdlib.String.compare name1 name2
  in
  List.fast_sort cmp_domain !domains_ref |> List.map fst

(* Print registered domain names by decreasing priority. *)
let pp_domain_names fmt =
  let names = sorted_domains () |> List.map fst in
  Pretty_utils.pp_list ~sep:", " Format.pp_print_string fmt names

(* Help message for the -eva-domains option, with the list of currently
   available domains. *)
let domains_help () =
  Format.asprintf
    "Enable a list of analysis domains. Available domains are: %t. \
     Use -eva-domains help to print a short description of each domain."
    pp_domain_names

(* Prints the list of available domains with their description. *)
let print_domains_and_exit () =
  let pp_dom fmt (name, descr) =
    Format.fprintf fmt "%-20s @[%t@]" name
      (fun fmt -> Format.pp_print_text fmt descr)
  in
  feedback ~level:0
    "List of available domains:@,%a"
    (Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@," ~suf:"@]" pp_dom)
    (sorted_domains ());
  raise Cmdline.Exit

(* Registers a new domain. Updates the help message of -eva-domains. *)
let register_domain ~name ~descr ~priority =
  create_domain_option name;
  domains_ref := ((name, descr), priority) :: !domains_ref;
  Cmdline.replace_option_help
    Domains.option_name ~plugin:"eva" ~group:domains (domains_help ())

(* Checks that a domain has been registered. *)
let check_domain option_name domain =
  if domain = "help" || domain = "list"
  then print_domains_and_exit ()
  else if not (List.exists (fun ((name, _), _) -> name = domain) !domains_ref)
  then
    abort "invalid domain %S for option %s.@\nAvailable domains are: %t"
      domain option_name pp_domain_names

let () =
  let hook option_name = fun _old domains ->
    Datatype.String.Set.iter (check_domain option_name) domains
  in
  Domains.add_set_hook (hook Domains.name);
  NoResultsDomain.add_set_hook (hook NoResultsDomain.name)

let () = Parameter_customize.set_group domains
module DomainsFunction =
  Make_multiple_map
    (struct
      include Datatype.String
      let of_string str = check_domain "-eva-domains-function" str; [ str ]
      let to_string str = str
    end)
    (struct
      include Domain_mode.Function_Mode
      let of_string str =
        try of_string str
        with Invalid_argument msg -> raise (Cannot_build msg)
    end)
    (struct
      let option_name = "-eva-domains-function"
      let help = "Enable a domain only for the given functions. \
                  <d:f+> enables the domain [d] from function [f] \
                  (the domain is enabled in all functions called from [f]). \
                  <d:f-> disables the domain [d] from function [f]."
      let arg_name = "d:f"
      let default = Datatype.String.Map.empty
      let dependencies = []
    end)
let () = add_precision_dep DomainsFunction.parameter

let enabled_domains () =
  let domains = Domains.get () in
  let domains_by_fct = DomainsFunction.get () in
  List.filter
    (fun (name, _) -> Datatype.String.Set.mem name domains
                      || Datatype.String.Map.mem name domains_by_fct)
    (sorted_domains ())

let () = Parameter_customize.set_group domains
module EqualityCall =
  String
    (struct
      let option_name = "-eva-equality-through-calls"
      let help = "Propagate equalities through function calls (from the caller \
                  to the called function): none, only equalities between formal \
                  parameters and concrete arguments, or all. "
      let default = "formals"
      let arg_name = "none|formals|all"
    end)
let () = EqualityCall.set_possible_values ["none"; "formals"; "all"]
let () = add_precision_dep EqualityCall.parameter

let () = Parameter_customize.set_group domains
module EqualityCallFunction =
  Kernel_function_map
    (struct
      include Datatype.String
      let of_string = function
        | "none" | "formals" | "all" as x -> x
        | _ -> raise (Cannot_build "must be 'none', 'formals' or 'all'.")
      let to_string s = s
    end)
    (struct
      let option_name = "-eva-equality-through-calls-function"
      let help = "Propagate equalities through calls to specific functions. \
                  Overrides -eva-equality-call."
      let default = Kernel_function.Map.empty
      let arg_name = "f:none|formals|all"
    end)
let () = add_precision_dep EqualityCallFunction.parameter

let () = Parameter_customize.set_group domains
module OctagonCall =
  Bool
    (struct
      let option_name = "-eva-octagon-through-calls"
      let help = "Propagate relations inferred by the octagon domain \
                  through function calls. Disabled by default: \
                  the octagon analysis is intra-procedural, starting \
                  each function with an empty octagon state, \
                  and losing the octagons inferred at the end. \
                  The interprocedural analysis is more precise but slower."
      let default = false
    end)
let () = add_precision_dep OctagonCall.parameter

let () = Parameter_customize.set_group domains
module TaintAuto =
  False
    (struct
      let option_name = "-eva-taint-auto"
      let help = "Automatically taint the function parameters of \
                  user input based functions (scanf, fgets, etc)."
    end)
let () = add_precision_dep TaintAuto.parameter

let () = Parameter_customize.set_group domains
let () = Parameter_customize.is_invisible ()
module TaintSingletons =
  True
    (struct
      let option_name = "-eva-taint-singletons"
      let help = "By default, variables may be tainted by the taint domain \
                  regardless of whether they have a single value. \
                  Use -eva-no-taint-singletons to never taint such variables. \
                  This may be unsound in presence of some state partitioning \
                  (such as split annotations)."
    end)
let () = add_precision_dep TaintSingletons.parameter

let () = Parameter_customize.set_group domains
module SecureFlow =
  False
    (struct
      let option_name = "-eva-secure-flow"
      let help = "Perform secure-flow analysis to prove non-interference \
                  properties: emit warnings whenever low-security public data \
                  may depend on high-security private data. Public and private \
                  data must be specified using corresponding custom attributes."
    end)
let () = add_correctness_dep SecureFlow.parameter

let () = Parameter_customize.set_group domains
module NumerorsInteraction =
  String
    (struct
      let option_name = "-eva-numerors-interaction"
      let help = "Define how the numerors domain infers the absolute and the \
                  relative errors:\n\
                  - relative: the relative is deduced from the absolute;\n\
                  - absolute: the absolute is deduced from the relative;\n\
                  - none: absolute and relative are computed separately;\n\
                  - both: reduced product between absolute and relative."
      let default = "both"
      let arg_name = "relative|absolute|none|both"
    end)
let () =
  NumerorsInteraction.set_possible_values ["relative"; "absolute"; "none"; "both"]
let () = add_precision_dep NumerorsInteraction.parameter

let () = Parameter_customize.set_group domains
module TracesUnrollLoop =
  Bool
    (struct
      let option_name = "-eva-traces-unroll-loop"
      let help = "Specify if the traces domain should unroll the loops."
      let default = true
    end)
let () = add_precision_dep TracesUnrollLoop.parameter

let () = Parameter_customize.set_group domains
module TracesUnifyLoop =
  Bool
    (struct
      let option_name = "-eva-traces-unify-loop"
      let help = "Specify if all the instances of a loop should try \
                  to share theirs traces."
      let default = false
    end)
let () = add_precision_dep TracesUnifyLoop.parameter

let () = Parameter_customize.set_group domains
module TracesDot = Filepath
    (struct
      let option_name = "-eva-traces-dot"
      let arg_name = "FILENAME"
      let file_kind = "DOT"
      let existence = Fclib.Filepath.Indifferent
      let help = "Output to the given filename the Cfg in dot format."
    end)

let () = Parameter_customize.set_group domains
module TracesProject = Bool
    (struct
      let option_name = "-eva-traces-project"
      let help = "Try to convert the Cfg into a program in a new project."
      let default = false
    end)

let () = Parameter_customize.set_group domains
module MultidimSegmentLimit = Int
    (struct
      let option_name = "-eva-multidim-segment-limit"
      let arg_name = "N"
      let help = "Limit the number of segments in the abstraction of arrays."
      let default = 8
    end)
let () = MultidimSegmentLimit.set_range ~min:3 ~max:max_int
let () = add_precision_dep MultidimSegmentLimit.parameter

let () = Parameter_customize.set_group domains
module MultidimDisjunctiveInvariants = False
    (struct
      let option_name = "-eva-multidim-disjunctive-invariants"
      let help = "Try to infer structures disjunctive invariants."
    end)
let () = add_precision_dep MultidimDisjunctiveInvariants.parameter

let () = Parameter_customize.set_group domains
let () = Parameter_customize.is_invisible ()
module MultidimFastImprecise = False
    (struct
      let option_name = "-eva-multidim-fast-imprecise"
      let help = "Makes the multidim domain faster but less precise: \
                  the domain can lose more information when joining states."
    end)
let () = add_precision_dep MultidimFastImprecise.parameter

(* -------------------------------------------------------------------------- *)
(* --- Performance options                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group performance
module NoResultsFunction =
  Fundec_set
    (struct
      let option_name = "-eva-no-results-function"
      let arg_name = "f"
      let help = "Do not record the values obtained for the statements of \
                  function f"
    end)
let () = add_dep NoResultsFunction.parameter

let () = Parameter_customize.set_group performance
module ResultsAll =
  True
    (struct
      let option_name = "-eva-results"
      let help = "Record values for each of the statements of the program."
    end)
let () = add_dep ResultsAll.parameter

let () = Parameter_customize.set_group performance
module JoinResults =
  Bool
    (struct
      let option_name = "-eva-join-results"
      let help = "Precompute consolidated states once Eva is computed"
      let default = true
    end)

(* ------------------------------------------------------------------------- *)
(* --- Non-standard alarms                                               --- *)
(* ------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group alarms
module UndefinedPointerComparisonPropagateAll =
  False
    (struct
      let option_name = "-eva-undefined-pointer-comparison-propagate-all"
      let help = "If the target program appears to contain undefined pointer \
                  comparisons, propagate both outcomes {0; 1} in addition to \
                  the emission of an alarm"
    end)
let () = add_correctness_dep UndefinedPointerComparisonPropagateAll.parameter

let () = Parameter_customize.set_group alarms
module WarnPointerComparison =
  Enum
    (struct
      type t = [ `All | `Pointer | `None ]
      let option_name = "-eva-warn-undefined-pointer-comparison"
      let help = "Warn on all pointer comparisons, on comparisons where \
                  the arguments have pointer type (default), or never warn"
      let default = `Pointer
      let values = [ `All, "all"; `Pointer, "pointer"; `None, "none" ]
    end)
let () = add_correctness_dep WarnPointerComparison.parameter

let () = Parameter_customize.set_group alarms
module WarnSignedConvertedDowncast =
  False
    (struct
      let option_name = "-eva-warn-signed-converted-downcast"
      let help = "Signed downcasts are decomposed into two operations: \
                  a conversion to the signed type of the original width, \
                  then a downcast. Warn when the downcast may exceed the \
                  destination range."
    end)
let () = add_correctness_dep WarnSignedConvertedDowncast.parameter


let () = Parameter_customize.set_group alarms
module WarnPointerSubtraction =
  True
    (struct
      let option_name = "-eva-warn-pointer-subtraction"
      let help =
        "Warn when subtracting two pointers that may not be in the same \
         allocated block, and return the pointwise difference between the \
         offsets. When unset, do not warn but generate imprecise offsets."
    end)
let () = add_correctness_dep WarnPointerSubtraction.parameter

let () = Parameter_customize.set_group alarms
let () = Parameter_customize.is_invisible ()
module IgnoreRecursiveCalls =
  False
    (struct
      let option_name = "-eva-ignore-recursive-calls"
      let help = "Deprecated."
    end)
let () =
  IgnoreRecursiveCalls.add_set_hook
    (fun _old _new ->
       warning
         "@[Option -eva-ignore-recursive-calls has no effect.@ Recursive calls \
          can be unrolled@ through option -eva-unroll-recursive-calls,@ or their \
          specification is used@ to interpret them.@]")

let () = Parameter_customize.set_group alarms
let () = Parameter_customize.argument_may_be_fundecl ();
module WarnCopyIndeterminate =
  Kernel_function_set
    (struct
      let option_name = "-eva-warn-copy-indeterminate"
      let arg_name = "f | @all"
      let help =
        "Warn when a statement copies a value that may be indeterminate \
         (uninitialized, containing an escaping address, or infinite/NaN \
         floating-point value). \
         Set by default; can be deactivated for function 'f' by '=-f', \
         or for all functions by '=-@all'."
    end)
let () = add_correctness_dep WarnCopyIndeterminate.parameter
let () = WarnCopyIndeterminate.Category.(set_default (all ()))

let () = Parameter_customize.set_group alarms
module ReduceOnLogicAlarms =
  False
    (struct
      let option_name = "-eva-reduce-on-logic-alarms"
      let help = "Force reductions by a predicate to ignore logic alarms \
                  emitted while the predicate is evaluated (experimental)"
    end)
let () = add_correctness_dep ReduceOnLogicAlarms.parameter

let () = Parameter_customize.set_group alarms
module InitializedLocals =
  False
    (struct
      let option_name = "-eva-initialized-locals"
      let help = "Local variables enter in scope fully initialized. \
                  Only useful for the analysis of programs buggy w.r.t. \
                  initialization."
    end)
let () = add_correctness_dep InitializedLocals.parameter

(* ------------------------------------------------------------------------- *)
(* --- Initial context                                                   --- *)
(* ------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group initial_context
module ContextDepth =
  Int
    (struct
      let option_name = "-eva-context-depth"
      let default = 2
      let arg_name = "n"
      let help = "Use <n> as the depth of the default context for Eva. \
                  (defaults to 2)"
    end)
let () = ContextDepth.set_range ~min:0 ~max:max_int
let () = add_correctness_dep ContextDepth.parameter

let () = Parameter_customize.set_group initial_context
module ContextWidth =
  Int
    (struct
      let option_name = "-eva-context-width"
      let default = 2
      let arg_name = "n"
      let help = "Use <n> as the width of the default context for Eva. \
                  (defaults to 2)"
    end)
let () = ContextWidth.set_range ~min:1 ~max:max_int
let () = add_correctness_dep ContextWidth.parameter

let () = Parameter_customize.set_group initial_context
module ContextValidPointers =
  False
    (struct
      let option_name = "-eva-context-valid-pointers"
      let help = "Only allocate valid pointers until context-depth, \
                  and then use NULL (defaults to false)"
    end)
let () = add_correctness_dep ContextValidPointers.parameter

let () = Parameter_customize.set_group initial_context
module InitializationPaddingGlobals =
  Enum
    (struct
      type t =   [ `Initialized | `Uninitialized | `MaybeInitialized ]
      let option_name = "-eva-initialization-padding-globals"
      let help = "Specify how padding bits are initialized inside global \
                  variables. Possible values are <yes> (padding is fully \
                  initialized), <no> (padding is completely uninitialized), or \
                  <maybe> (padding may be uninitialized). Default is <yes>."
      let default = `Initialized
      let values =
        [ `Initialized, "yes";
          `Uninitialized, "no" ;
          `MaybeInitialized, "maybe" ]
    end)
let () = add_correctness_dep InitializationPaddingGlobals.parameter

(* ------------------------------------------------------------------------- *)
(* --- Tuning                                                            --- *)
(* ------------------------------------------------------------------------- *)

(* --- Iteration strategy --- *)

type descending_strategy = NoIteration | FullIteration | ExitIteration

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.is_invisible ()
module DescendingIteration =
  Enum
    (struct
      let option_name = "-eva-descending-iteration"
      let help = "Experimental. After hitting a postfix point, try to improve \
                  the precision with either a <full> iteration or an iteration \
                  from loop head to exit paths (<exits>) or do not try anything \
                  (<no>). Default is <no>."
      type t = descending_strategy
      let default = NoIteration
      let values =
        [ NoIteration, "no";
          FullIteration, "full";
          ExitIteration, "exits" ]
    end)
let () = add_precision_dep DescendingIteration.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.is_invisible ()
module HierarchicalConvergence =
  False
    (struct
      let option_name = "-eva-hierarchical-convergence"
      let help = "Experimental and unsound. Separate the convergence process \
                  of each level of nested loops. This implies that the \
                  convergence of inner loops will be completely recomputed when \
                  doing another iteration of the outer loops."
    end)
let () = add_precision_dep HierarchicalConvergence.parameter

let () = Parameter_customize.set_group precision_tuning
module WideningDelay =
  Int
    (struct
      let default = 3
      let option_name = "-eva-widening-delay"
      let arg_name = "n"
      let help =
        "Do not widen before the <n>-th iteration (defaults to 3)"
    end)
let () = WideningDelay.set_range ~min:1 ~max:max_int
let () = add_precision_dep WideningDelay.parameter

let () = Parameter_customize.set_group precision_tuning
module WideningPeriod =
  Int
    (struct
      let default = 2
      let option_name = "-eva-widening-period"
      let arg_name = "n"
      let help =
        "After the first widening, widen each <n> iterations (defaults to 2)"
    end)
let () = WideningPeriod.set_range ~min:1 ~max:max_int
let () = add_precision_dep WideningPeriod.parameter

let () = Parameter_customize.set_group precision_tuning
module RecursiveUnroll =
  Int
    (struct
      let default = 0
      let option_name = "-eva-unroll-recursive-calls"
      let arg_name = "n"
      let help = "Unroll <n> recursive calls before using the specification of \
                  the recursive function to interpret the calls."
    end)
let () = RecursiveUnroll.set_range ~min:0 ~max:max_int
let () = add_precision_dep RecursiveUnroll.parameter

(* --- Partitioning --- *)

let () = Parameter_customize.set_group precision_tuning
module SLevel =
  Zero
    (struct
      let option_name = "-eva-slevel"
      let arg_name = "n"
      let help =
        "Superpose up to <n> states when unrolling control flow. \
         The larger n, the more precise and expensive the analysis \
         (defaults to 0)"
    end)
let () = SLevel.set_range ~min:0 ~max:max_int
let () = add_precision_dep SLevel.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module SlevelFunction =
  Kernel_function_map
    (Value_int)
    (struct
      let option_name = "-eva-slevel-function"
      let arg_name = "f:n"
      let help = "Override slevel with <n> when analyzing <f>"
      let default = Kernel_function.Map.empty
    end)
let () = add_precision_dep SlevelFunction.parameter

let () = Parameter_customize.set_group precision_tuning
module SlevelMergeAfterLoop =
  Kernel_function_set
    (struct
      let option_name = "-eva-slevel-merge-after-loop"
      let arg_name = "f | @all"
      let help =
        "When set, the different execution paths that originate from the body \
         of a loop are merged before entering the next execution."
    end)
let () = add_precision_dep SlevelMergeAfterLoop.parameter

let () = Parameter_customize.set_group precision_tuning
module MinLoopUnroll =
  Int
    (struct
      let option_name = "-eva-min-loop-unroll"
      let arg_name = "n"
      let default = 0
      let help =
        "Unroll <n> loop iterations for each loop, regardless of the slevel \
         settings and the number of states already propagated. \
         Can be overwritten on a case-by-case basis by loop unroll annotations."
    end)
let () = add_precision_dep MinLoopUnroll.parameter
let () = MinLoopUnroll.set_range ~min:0 ~max:max_int

let () = Parameter_customize.set_group precision_tuning
module AutoLoopUnroll =
  Int
    (struct
      let option_name = "-eva-auto-loop-unroll"
      let arg_name = "n"
      let default = 0
      let help = "Limit of the automatic loop unrolling: all loops whose \
                  number of iterations can be easily bounded by <n> \
                  are completely unrolled."
    end)
let () = add_precision_dep AutoLoopUnroll.parameter
let () = AutoLoopUnroll.set_range ~min:0 ~max:max_int

let () = Parameter_customize.set_group precision_tuning
module DefaultLoopUnroll =
  Int
    (struct
      let option_name = "-eva-default-loop-unroll"
      let arg_name = "n"
      let default = 100
      let help =
        "Define the default limit for loop unroll annotations that do \
         not explicitly provide a limit."
    end)
let () = add_precision_dep DefaultLoopUnroll.parameter
let () = DefaultLoopUnroll.set_range ~min:0 ~max:max_int

let () = Parameter_customize.set_group precision_tuning
module HistoryPartitioning =
  Int
    (struct
      let option_name = "-eva-partition-history"
      let arg_name = "n"
      let default = 0
      let help =
        "Keep states distinct as long as the <n> last branching in their \
         traces are also distinct. (A value of 0 deactivates this feature)"
    end)
let () = add_precision_dep HistoryPartitioning.parameter
let () = HistoryPartitioning.set_range ~min:0 ~max:max_int

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module HistoryPartitioningFunction =
  Kernel_function_map
    (Value_int)
    (struct
      let option_name = "-eva-partition-history-function"
      let arg_name = "f:n"
      let help = "Override partition-history with <n> when analyzing <f>"
      let default = Kernel_function.Map.empty
    end)
let () = add_precision_dep HistoryPartitioningFunction.parameter

let () = Parameter_customize.set_group precision_tuning
module ValuePartitioning =
  String_set
    (struct
      let option_name = "-eva-partition-value"
      let help = "Partition the space of reachable states according to the \
                  possible values of the global(s) variable(s) V."
      let arg_name = "V"
    end)
let () = add_precision_dep ValuePartitioning.parameter

let use_global_value_partitioning vi =
  ValuePartitioning.add vi.Cil_types.vname

let () = Parameter_customize.set_group precision_tuning
module SplitLimit =
  Int
    (struct
      let option_name = "-eva-split-limit"
      let arg_name = "N"
      let default = 100
      let help = "Prevent split annotations or -eva-partition-value from \
                  enumerating more than N cases"
    end)
let () = add_precision_dep SplitLimit.parameter
let () = SplitLimit.set_range ~min:0 ~max:max_int

let () = Parameter_customize.set_group precision_tuning
module InterproceduralSplits =
  False
    (struct
      let option_name = "-eva-interprocedural-splits"
      let help = "Keep partitioning splits through function returns"
    end)
let () = add_precision_dep InterproceduralSplits.parameter

let () = Parameter_customize.set_group precision_tuning
module InterproceduralHistory =
  False
    (struct
      let option_name = "-eva-interprocedural-history"
      let help = "Keep partitioning history through function returns"
    end)
let () = add_precision_dep InterproceduralHistory.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module SplitReturnFunction =
  Kernel_function_map
    (Split_strategy)
    (struct
      let option_name = "-eva-split-return-function"
      let arg_name = "f:full|f:auto|f:N|f:"
      let help = "Override the global option -eva-split-return at the return \
                  of function <f>. <f:> reverts to the the default strategy \
                  where all states are joined at the return point of <f>."
      let default = Kernel_function.Map.empty
    end)
let () = add_precision_dep SplitReturnFunction.parameter

let () = Parameter_customize.set_group precision_tuning
module SplitReturn =
  Custom
    (Split_strategy)
    (struct
      let option_name = "-eva-split-return"
      let arg_name = "full|auto|N"
      let default = Split_strategy.NoSplit
      let help =
        "Control the propagation of states separated by option -eva-slevel \
         at the return of a function. \
         By default, all inferred states at a return point are joined into one \
         single state, which is fast but imprecise. \
         With 'full', all inferred states are propagated back to the callsite, \
         which is costly but precise. \
         With 'auto', states inferred at a return point are automatically \
         joined or kept separate according to the function return code. \
         With a number <N>, keep states distinct only according to the value \
         of the predicate \\result == N."
    end)
let () = add_precision_dep SplitReturn.parameter

(* --- Misc --- *)

let () = Parameter_customize.set_group precision_tuning
module ILevel =
  Int
    (struct
      let option_name = "-eva-ilevel"
      let default = 8 (* Must be synchronized with Int_set.small_cardinal. *)
      let arg_name = "n"
      let help =
        "Sets of integers are represented as sets up to <n> elements. \
         Above, intervals with congruence information are used \
         (defaults to 8, must be above 2)"
    end)
let () = add_precision_dep ILevel.parameter
let () = ILevel.add_update_hook (fun _ i -> Int_set.set_small_cardinal i)
let () = ILevel.set_range ~min:2 ~max:max_int

let builtins = ref Datatype.String.Set.empty
let register_builtin name = builtins := Datatype.String.Set.add name !builtins
let unregister_builtin name =
  builtins := Datatype.String.Set.remove name !builtins
let mem_builtin name = Datatype.String.Set.mem name !builtins

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module BuiltinsOverrides =
  Kernel_function_map
    (struct
      include Datatype.String
      let of_string name =
        if not (mem_builtin name) then
          abort "option '-eva-builtin': undeclared builtin '%s'@.\
                 declared builtins: @[%a@]"
            name
            (Pretty_utils.pp_list ~sep:",@ " Format.pp_print_string)
            (Datatype.String.Set.elements !builtins);
        name
      let to_string name = name
    end)
    (struct
      let option_name = "-eva-builtin"
      let arg_name = "f:ffc"
      let help = "When analyzing function <f>, try to use Frama-C builtin \
                  <ffc> instead. \
                  Fall back to <f> if <ffc> cannot handle its arguments."
      let default = Kernel_function.Map.empty
    end)
let () = add_correctness_dep BuiltinsOverrides.parameter

(* Exported in Eva.mli. *)
let use_builtin key name =
  if mem_builtin name
  then BuiltinsOverrides.add (key, name)
  else raise Not_found

let () = Parameter_customize.set_group precision_tuning
module BuiltinsAuto =
  True
    (struct
      let option_name = "-eva-builtins-auto"
      let help = "When set, builtins will be used automatically to replace \
                  known C functions"
    end)
let () = add_correctness_dep BuiltinsAuto.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.set_negative_option_name ""
module BuiltinsList =
  False
    (struct
      let option_name = "-eva-builtins-list"
      let help = "List existing builtins, and which functions they \
                  are automatically associated to (if any)"
    end)

let () = Parameter_customize.set_group precision_tuning
module SubdivideNonLinear =
  Zero
    (struct
      let option_name = "-eva-subdivide-non-linear"
      let arg_name = "n"
      let help =
        "Improve precision when evaluating expressions in which a variable \
         appears multiple times, by splitting its value at most n times. \
         Defaults to 0."
    end)
let () = SubdivideNonLinear.set_range ~min:0 ~max:max_int
let () = add_precision_dep SubdivideNonLinear.parameter

let () = Parameter_customize.set_group precision_tuning
module SubdivideNonLinearFunction =
  Kernel_function_map
    (Value_int)
    (struct
      let option_name = "-eva-subdivide-non-linear-function"
      let arg_name = "f:n"
      let help = "Override the global option -eva-subdivide-non-linear with <n>\
                  when analyzing the function <f>."
      let default = Kernel_function.Map.empty
    end)
let () = add_precision_dep SubdivideNonLinearFunction.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module UseSpec =
  Kernel_function_set
    (struct
      let option_name = "-eva-use-spec"
      let arg_name = "f1,..,fn"
      let help = "Use the ACSL specification of the functions instead of \
                  their definitions"
    end)
let () = add_correctness_dep UseSpec.parameter

let () = Parameter_customize.set_group precision_tuning
module SkipLibcSpecs =
  True
    (struct
      let option_name = "-eva-skip-stdlib-specs"
      let help = "Skip ACSL specifications on functions originating from the \
                  standard library of Frama-C, when their bodies are evaluated"
    end)
let () = add_precision_dep SkipLibcSpecs.parameter


let () = Parameter_customize.set_group precision_tuning
module RmAssert =
  True
    (struct
      let option_name = "-eva-remove-redundant-alarms"
      let help = "After the analysis, try to remove redundant alarms, \
                  so that the user needs to inspect fewer of them"
    end)
let () = add_precision_dep RmAssert.parameter

let () = Parameter_customize.set_group precision_tuning
module Memexec =
  True
    (struct
      let option_name = "-eva-memexec"
      let help = "Speed up analysis by not recomputing functions already \
                  analyzed in the same context. \
                  Callstacks for which the analysis has not been recomputed \
                  are incorrectly shown as dead in the GUI."
    end)

let () = Parameter_customize.set_group precision_tuning
module ArrayPrecisionLevel =
  Int
    (struct
      let default = 200
      let option_name = "-eva-plevel"
      let arg_name = "n"
      let help = "Use <n> as the precision level for arrays accesses. \
                  Array accesses are precise as long as the interval for the \
                  index contains less than n values. (defaults to 200)"
    end)
let () = ArrayPrecisionLevel.set_range ~min:0 ~max:max_int
let () = add_precision_dep ArrayPrecisionLevel.parameter
let () = ArrayPrecisionLevel.add_update_hook
    (fun _ v -> Offsetmap.set_plevel v)

(* ------------------------------------------------------------------------- *)
(* --- Messages                                                          --- *)
(* ------------------------------------------------------------------------- *)

(* Export verbose option generated when creating the Eva plugin. *)
module Verbose = Self.Verbose

let () = Parameter_customize.set_group messages
module ShowPerf =
  False
    (struct
      let option_name = "-eva-show-perf"
      let help = "Compute and show a summary of the time spent analyzing \
                  function calls"
    end)

let () = Parameter_customize.set_group messages
module Flamegraph =
  Filepath
    (struct
      let option_name = "-eva-flamegraph"
      let arg_name = "file"
      let file_kind = "Text for flamegraph"
      let existence = Fclib.Filepath.Indifferent
      let help = "Dump a summary of the time spent analyzing function calls \
                  in a format suitable for the Flamegraph tool \
                  (http://www.brendangregg.com/flamegraphs.html)"
    end)


let () = Parameter_customize.set_group messages
module ShowSlevel =
  Int
    (struct
      let option_name = "-eva-show-slevel"
      let default = 100
      let arg_name = "n"
      let help = "Period for showing consumption of the allotted slevel during \
                  analysis"
    end)
let () = ShowSlevel.set_range ~min:1 ~max:max_int

let () = Parameter_customize.set_group messages
module PrintCallstacks =
  False
    (struct
      let option_name = "-eva-print-callstacks"
      let help = "When printing a message, also show the current call stack"
    end)
let () =
  let set_hook _old_enabled enabled =
    warning "Option -eva-print-callstacks is now deprecated.@ \
             Use -eva-msg-key callstacks instead.";
    if enabled
    then Self.(add_debug_keys dkey_callstacks)
    else Self.(del_debug_keys dkey_callstacks)
  in
  PrintCallstacks.add_set_hook set_hook

let () = Parameter_customize.set_group messages
module ReportRedStatuses =
  Filepath
    (struct
      let option_name = "-eva-report-red-statuses"
      let arg_name = "filename"
      let file_kind = "CSV"
      let existence = Fclib.Filepath.Indifferent
      let help = "Output the list of \"red properties\" in a csv file of the \
                  given name. These are the properties which were invalid for \
                  some states. Their consolidated status may not be invalid, \
                  but they should often be investigated first."
    end)

let () = Parameter_customize.set_group messages
module StatisticsFile =
  Filepath
    (struct
      let option_name = "-eva-statistics-file"
      let arg_name = "file.csv"
      let file_kind = "CSV"
      let existence = Fclib.Filepath.Indifferent
      let help = "Dump some internal statistics about the analysis"
    end)


(* ------------------------------------------------------------------------- *)
(* --- Interpreter mode                                                  --- *)
(* ------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group interpreter
module InterpreterMode =
  False
    (struct
      let option_name = "-eva-interpreter-mode"
      let help = "Stop at first call to a library function, if main() has \
                  arguments, on undecided branches"
    end)

let () = Parameter_customize.set_group interpreter
module StopAtNthAlarm =
  Int(struct
    let option_name = "-eva-stop-at-nth-alarm"
    let default = max_int
    let arg_name = "n"
    let help = "Abort the analysis when the nth alarm is emitted."
  end)
let () = StopAtNthAlarm.set_range ~min:0 ~max:max_int

(* -------------------------------------------------------------------------- *)
(* --- Ugliness required for correctness                                  --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.is_invisible ()
module CorrectnessChanged =
  Int (struct
    let option_name = "-eva-new-initial-state"
    let default = 0
    let arg_name = "n"
    let help = ""
  end)
let () = add_correctness_dep CorrectnessChanged.parameter

(* Changing the user-supplied initial state (or the arguments of main) through
   the API does reset the state of Eva, but *not* the property statuses set by
   Eva. Currently, statuses can only depend on command-line parameters.
   We use the dummy one above to force a reset when needed. *)
let change_correctness = CorrectnessChanged.incr

(* -------------------------------------------------------------------------- *)
(* --- Eva options                                                        --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group precision_tuning
module EnumerateCond =
  Bool
    (struct
      let option_name = "-eva-enumerate-cond"
      let help = "Activate reduce_by_cond_enumerate."
      let default = true
    end)
let () = add_precision_dep EnumerateCond.parameter


let () = Parameter_customize.set_group precision_tuning
module OracleDepth =
  Int
    (struct
      let option_name = "-eva-oracle-depth"
      let help = "Maximum number of successive uses of the oracle by the domain \
                  for the evaluation of an expression. Set 0 to disable the \
                  oracle."
      let default = 2
      let arg_name = ""
    end)
let () = OracleDepth.set_range ~min:0 ~max:max_int
let () = add_precision_dep OracleDepth.parameter

let () = Parameter_customize.set_group precision_tuning
module ReductionDepth =
  Int
    (struct
      let option_name = "-eva-reduction-depth"
      let help = "Maximum number of successive backward reductions that the \
                  domain may initiate."
      let default = 4
      let arg_name = ""
    end)
let () = ReductionDepth.set_range ~min:0 ~max:max_int
let () = add_precision_dep ReductionDepth.parameter


(* -------------------------------------------------------------------------- *)
(* --- Dynamic allocation                                                 --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group malloc
module AllocBuiltin =
  String
    (struct
      let option_name = "-eva-alloc-builtin"
      let help = "Select the behavior of allocation builtins. \
                  By default, they use up to [-eva-mlevel] bases \
                  for each callstack (<by_stack>). They can also \
                  use one <imprecise> base for all allocations, \
                  create a <fresh> strong base at each call, \
                  or create a <fresh_weak> base at each call."
      let default = "by_stack"
      let arg_name = "imprecise|by_stack|fresh|fresh_weak"
    end)
let () = add_precision_dep AllocBuiltin.parameter
let () =
  AllocBuiltin.set_possible_values
    ["imprecise"; "by_stack"; "fresh"; "fresh_weak"]

let () = Parameter_customize.set_group malloc
module AllocFunctions =
  Filled_string_set
    (struct
      let option_name = "-eva-alloc-functions"
      let arg_name = "f1,...,fn"
      let help = "Control call site creation for dynamically allocated bases. \
                  Dynamic allocation builtins use the call sites of \
                  malloc/calloc/realloc to know \
                  where to create new bases. This detection does not work for \
                  custom allocators or wrappers on top of them, unless they \
                  are listed here. \
                  By default, contains malloc, calloc and realloc."
      let default = Datatype.String.Set.of_list ["malloc"; "calloc"; "realloc"]
    end)
let () = AllocFunctions.add_aliases ["-eva-malloc-functions"]

let () = Parameter_customize.set_group malloc
module AllocReturnsNull=
  True
    (struct
      let option_name = "-eva-alloc-returns-null"
      let help = "Memory allocation built-ins (malloc, calloc, realloc) are \
                  modeled as nondeterministically returning a null pointer"
    end)
let () = add_correctness_dep AllocReturnsNull.parameter

let () = Parameter_customize.set_group malloc
module MallocLevel =
  Int
    (struct
      let option_name = "-eva-mlevel"
      let default = 0
      let arg_name = "m"
      let help = "Set to [m] the number of precise dynamic allocations \
                  besides the initial one, for each callstack (defaults to 0)"
    end)
let () = MallocLevel.set_range ~min:0 ~max:max_int
let () = add_precision_dep MallocLevel.parameter

(* -------------------------------------------------------------------------- *)
(* --- Annotations Generator options                                      --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group messages
module Annot =
  Kernel_function_set
    (struct
      let option_name = "-eva-annot"
      let arg_name = "f"
      let help =
        "Populate the specified functions with assertions \
         representing the range of values computed by Eva \
         on l-values read by the code, when available."
    end)

(* -------------------------------------------------------------------------- *)
(* --- Deprecated options and aliases                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group alarms
let () = Parameter_customize.is_invisible ()
module AllRoundingModesConstants =
  False
    (struct
      let option_name = "-eva-all-rounding-modes-constants"
      let help = "Deprecated. Take into account the possibility of constants \
                  not being converted to the nearest representable value, \
                  or being converted to higher precision"
    end)
let () = add_correctness_dep AllRoundingModesConstants.parameter
let () =
  AllRoundingModesConstants.add_set_hook
    (fun _old _new ->
       warning "Option -eva-all-rounding-modes-constants is now deprecated.@ \
                Please contact us if you need it.")

let () = Parameter_customize.set_group messages
let () = Parameter_customize.is_invisible ()
module ValShowProgress =
  False
    (struct
      let option_name = "-eva-show-progress"
      let help = "Deprecated: use -eva-msg-key=progress instead."
    end)
let () =
  let hook _previous enabled =
    let prefix = if enabled then "+" else "-" in
    warning "Option -eva%s-show-progress is deprecated. \
             Please use -eva-msg-key=%sprogress instead."
      (if enabled then "" else "-no") prefix;
    Self.Message_category.set (prefix ^ "progress");
  in
  ValShowProgress.add_set_hook hook

let () = Parameter_customize.set_group messages
let () = Parameter_customize.is_invisible ()
module ForcePrintSummary =
  False
    (struct
      let option_name = "-eva-force-print-summary"
      let help = "Deprecated: use -eva-msg-key=summary instead."
    end)
let () =
  let hook _previous enabled =
    let prefix = if enabled then "+" else "-" in
    warning "Option -eva%s-force-print-summary is deprecated. \
             Please use -eva-msg-key=%ssummary instead."
      (if enabled then "" else "-no") prefix;
    Self.Message_category.set (prefix ^ "summary");
  in
  ForcePrintSummary.add_set_hook hook

let deprecated_aliases : ((module Parameter_sig.S) * string) list =
  [ (module SLevel), "-slevel"
  ; (module SlevelFunction), "-slevel-function"
  ; (module NoResultsFunction), "-no-results-function"
  ; (module ResultsAll), "-results"
  ; (module JoinResults), "-val-join-results"
  ; (module AllRoundingModesConstants), "-all-rounding-modes-constants"
  ; (module UndefinedPointerComparisonPropagateAll), "-undefined-pointer-comparison-propagate-all"
  ; (module WarnPointerComparison), "-val-warn-undefined-pointer-comparison"
  ; (module WarnSignedConvertedDowncast), "-val-warn-signed-converted-downcast"
  ; (module WarnPointerSubtraction), "-val-warn-pointer-subtraction"
  ; (module IgnoreRecursiveCalls), "-val-ignore-recursive-calls"
  ; (module WarnCopyIndeterminate), "-val-warn-copy-indeterminate"
  ; (module ReduceOnLogicAlarms), "-val-reduce-on-logic-alarms"
  ; (module InitializedLocals), "-val-initialized-locals"
  ; (module ContextDepth), "-context-depth"
  ; (module ContextWidth), "-context-width"
  ; (module ContextValidPointers), "-context-valid-pointers"
  ; (module InitializationPaddingGlobals), "-val-initialization-padding-globals"
  ; (module WideningDelay), "-wlevel"
  ; (module SlevelMergeAfterLoop), "-val-slevel-merge-after-loop"
  ; (module SplitReturnFunction), "-val-split-return-function"
  ; (module SplitReturn), "-val-split-return"
  ; (module ILevel), "-val-ilevel"
  ; (module BuiltinsOverrides), "-val-builtin"
  ; (module BuiltinsAuto), "-val-builtins-auto"
  ; (module BuiltinsList), "-val-builtins-list"
  ; (module SubdivideNonLinear), "-val-subdivide-non-linear"
  ; (module UseSpec), "-val-use-spec"
  ; (module SkipLibcSpecs), "-val-skip-stdlib-specs"
  ; (module RmAssert), "-remove-redundant-alarms"
  ; (module Memexec), "-memexec-all"
  ; (module ArrayPrecisionLevel), "-plevel"
  ; (module ValShowProgress), "-val-show-progress"
  ; (module ShowPerf), "-val-show-perf"
  ; (module Flamegraph), "-val-flamegraph"
  ; (module ShowSlevel), "-val-show-slevel"
  ; (module PrintCallstacks), "-val-print-callstacks"
  ; (module InterpreterMode), "-val-interpreter-mode"
  ; (module StopAtNthAlarm), "-val-stop-at-nth-alarm"
  ; (module AllocFunctions), "-val-malloc-functions"
  ; (module AllocReturnsNull), "-val-alloc-returns-null"
  ; (module MallocLevel), "-val-mlevel"
  ]

let add_deprecated_alias ((module P: Parameter_sig.S), name) =
  P.add_aliases ~visible:false ~deprecated:true [name]

let () = List.iter add_deprecated_alias deprecated_aliases


(* -------------------------------------------------------------------------- *)
(* --- Meta options                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Precision =
  Int
    (struct
      let option_name = "-eva-precision"
      let arg_name = "n"
      let default = -1
      let help = "Meta-option that automatically sets up some Eva parameters \
                  for a quick configuration of an analysis, \
                  from 0 (fastest but rather imprecise analysis) \
                  to 11 (accurate but potentially slow analysis)."
    end)
let () = Precision.set_range ~min:(-1) ~max:11
let () = add_precision_dep Precision.parameter

(* Sets a parameter [P] to [t], unless it has already been set by any other
   means. *)
let set (type t) (module P: Parameter_sig.S with type t = t) =
  (* Last value set by this function. *)
  let previous = ref None in
  let set_value value = P.set value; previous := Some value in
  let equal_current t = P.equal (P.get ()) t in
  (* We avoid overwriting a parameter already set, except if the current value
     is equal to !previous, the last value set by this function — in which case
     the parameter has probably been set by this function and not by the user. *)
  let is_unchanged () =
    Option.fold !previous ~none:false ~some:equal_current
  in
  fun ~default t ->
    let already_set = P.is_set () && not (is_unchanged ()) in
    if not already_set then
      if default then P.clear () else set_value t;
    let str = Typed_parameter.get_value P.parameter in
    let str = match P.parameter.Typed_parameter.accessor with
      | Typed_parameter.String _ -> "\'" ^ str ^ "\'"
      | _ -> str
    in
    let dkey = dkey_precision_settings in
    printf ~dkey "    option %s %sset to %s%s." P.name
      (if already_set then "already " else "") str
      (if already_set && not (equal_current t) then " (not modified)"
       else if P.is_default () then " (default value)" else "")

(* List of configure functions to be called for -eva-precision. *)
let configures = ref []

(* Binds the parameter [P] to the function [f] that gives the parameter value
   for a precision n. *)
let bind (type t) (module P: Parameter_sig.S with type t = t) f =
  let set = set (module P) in
  configures := (fun n -> set ~default:(n < 0) (f n)) :: !configures

let domains n =
  let (<+>) domains (x, name) = if n >= x then name :: domains else domains in
  [ "cvalue" ]
  <+> (1, "symbolic-locations")
  <+> (2, "equality")
  <+> (3, "gauges")
  <+> (5, "octagon")

(*  power             0    1   2   3    4    5    6    7    8     9    10    11 *)
let slevel_power = [| 0;  10; 20; 35;  60; 100; 160; 250; 500; 1000; 2000; 5000 |]
let ilevel_power = [| 8;  12; 16; 24;  32;  48;  64; 128; 192;  256;  256;  256 |]
let plevel_power = [| 10; 20; 40; 70; 100; 150; 200; 300; 500;  700; 1000; 2000 |]
let auto_unroll =  [| 0;  16; 32; 64;  96; 128; 192; 256; 384;  512;  768; 1024 |]

let get array n = if n < 0 then 0 else array.(n)

let () =
  bind (module MinLoopUnroll) (fun n -> max 0 (n - 7));
  bind (module AutoLoopUnroll) (get auto_unroll);
  bind (module WideningDelay) (fun n -> 1 + n / 2);
  bind (module HistoryPartitioning) (fun n -> (n - 1) / 5);
  bind (module SLevel) (get slevel_power);
  bind (module ILevel) (get ilevel_power);
  bind (module ArrayPrecisionLevel) (get plevel_power);
  bind (module SubdivideNonLinear) (fun n -> n * 20);
  bind (module RmAssert) (fun n -> n > 0);
  bind (module Domains) (fun n -> Datatype.String.Set.of_list (domains n));
  bind (module SplitReturn) (fun n -> if n > 3 then SplitAuto else NoSplit);
  bind (module EqualityCall) (fun n -> if n > 4 then "formals" else "none");
  bind (module OctagonCall) (fun n -> n > 6);
  ()

let set_analysis n =
  let dkey = dkey_precision_settings in
  feedback ~dkey "Option %s %i detected, \
                  automatic configuration of the analysis:" Precision.name n;
  List.iter ((|>) n) (List.rev !configures)

let configure_precision () =
  if Precision.is_set () then set_analysis (Precision.get ())

(* -------------------------------------------------------------------------- *)
(* --- Freeze parameters. MUST GO LAST                                    --- *)
(* -------------------------------------------------------------------------- *)

let parameters_correctness =
  Typed_parameter.Set.elements !parameters_correctness
let parameters_tuning =
  Typed_parameter.Set.elements !parameters_tuning
