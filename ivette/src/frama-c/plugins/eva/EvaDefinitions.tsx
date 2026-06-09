/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import * as Forms from 'dome/layout/forms';
import { computationStateType } from 'frama-c/plugins/eva/api/analysis';

export type KeyVal<A> = {[key: string]: A}

export interface buttonFieldInfo {
  value: boolean,
  label: string,
}

export type ButtonList = KeyVal<buttonFieldInfo>

export interface OptionsButtonProps {
  name: string,
  label: string,
  fieldState: Forms.FieldState<KeyVal<boolean>>,
}

export type RadioList = KeyVal<string>

export interface RadioFieldListProps {
  classeName?: string;
  state: Forms.FieldState<string>;
  values: RadioList;
  fieldProps: Forms.GenericFieldProps;
}

export interface ButtonFieldListProps {
  classeName?: string;
  state: Forms.FieldState<KeyVal<boolean>>;
  fieldProps: Forms.GenericFieldProps;
}

export type fieldsName =
  "-main" |
  "-lib-entry" |
  "-eva-precision" |
  "-eva-domains" |
  "-eva-equality-through-calls" |
  "-eva-octagon-through-calls" |
  "-eva-multidim-disjunctive-invariants" |
  "-eva-auto-loop-unroll" |
  "-eva-min-loop-unroll" |
  "-eva-widening-delay" |
  "-eva-widening-period" |
  "-eva-slevel" |
  "-eva-split-return" |
  "-eva-ilevel" |
  "-eva-plevel" |
  "-eva-partition-history" |
  "-eva-subdivide-non-linear" |
  "-eva-alloc-builtin" |
  "-eva-alloc-returns-null" |
  "-eva-mlevel" |
  "-eva-context-depth" |
  "-eva-context-width" |
  "-eva-context-valid-pointers" |
  "-warn-signed-overflow" |
  "-warn-unsigned-overflow" |
  "-warn-signed-downcast" |
  "-warn-unsigned-downcast" |
  "-warn-pointer-downcast" |
  "-warn-special-float" |
  "-warn-invalid-pointer" |
  "-warn-invalid-bool" |
  "-warn-left-shift-negative" |
  "-warn-right-shift-negative"

export interface EvaField {
  label: string,
  step?: number,
  min?: number,
  max?: number,
  optionRadio?: KeyVal<string>,
  /* eslint-disable-next-line @typescript-eslint/no-explicit-any */
  state: Forms.FieldState<any>
}

export type EvaFormProps =  {[key in fieldsName]: EvaField};

/* ************************************************************************ *
 * Option Eva Forms
 * ************************************************************************ */
export const fieldHelp: {[key in fieldsName]: string} = {
  "-main":
"use <f> as entry point for analysis. See \"-lib-entry\" \n \
if this is not for a complete application. Defaults to main",
  "-lib-entry":
"run analysis for an incomplete application e.g. an API call.\n \
See the -main option to set the entry point",
  "-eva-precision":
"Meta-option that automatically sets up some Eva parameters\n \
for a quick configuration of an analysis,\n \
from 0 (fastest but rather imprecise analysis)\n \
to 11 (accurate but potentially slow analysis).",
  "-eva-domains": "Enable a list of analysis domains.",
  "-eva-equality-through-calls":
"Propagate equalities through function calls (from the caller\n \
to the called function): none, only equalities between formal\n \
parameters and concrete arguments, or all.",
  "-eva-octagon-through-calls":
"Propagate relations inferred by the octagon domain\n \
through function calls. Disabled by default:\n \
the octagon analysis is intra-procedural, starting\n \
each function with an empty octagon state,\n \
and losing the octagons inferred at the end.\n \
The interprocedural analysis is more precise but slower.",
  "-eva-multidim-disjunctive-invariants":
"Try to infer structures disjunctive invariants.",
  "-eva-auto-loop-unroll":
"Limit of the automatic loop unrolling: all loops whose\n \
number of iterations can be easily bounded by <n>\n \
are completely unrolled.",
  "-eva-min-loop-unroll":
"Unroll <n> loop iterations for each loop, regardless of the slevel\n \
settings and the number of states already propagated.\n \
Can be overwritten on a case-by-case basis by loop unroll annotations.",
  "-eva-widening-delay":
"Do not widen before the <n>-th iteration (defaults to 3)",
  "-eva-widening-period":
"After the first widening, widen each <n> iterations (defaults to 2)",
  "-eva-slevel":
"Superpose up to <n> states when unrolling control flow.\n \
The larger n, the more precise and expensive the analysis\n \
(defaults to 0)",
  "-eva-split-return":
"Split return states of function <f> according to \
\\result == n and \\result != n",
  "-eva-ilevel":
"Sets of integers are represented as sets up to <n> elements.\n \
Above, intervals with congruence information are used\n \
(defaults to 8, must be above 2)",
  "-eva-plevel":
"Use <n> as the precision level for arrays accesses.\n \
Array accesses are precise as long as the interval for the\n \
index contains less than n values. (defaults to 200)",
  "-eva-partition-history":
"Keep states distinct as long as the <n> last branching in their\n \
traces are also distinct. (A value of 0 deactivates this feature)",
  "-eva-subdivide-non-linear":
"Improve precision when evaluating expressions in which a variable\n \
appears multiple times, by splitting its value at most n times.\n \
Defaults to 0.",
  "-eva-alloc-builtin":
"Select the behavior of allocation builtins.\n \
By default, they use up to [-eva-mlevel] bases\n \
for each callstack (<by_stack>). They can also\n \
use one <imprecise> base for all allocations,\n \
create a <fresh> strong base at each call,\n \
or create a <fresh_weak> base at each call.",
  "-eva-alloc-returns-null":
"Memory allocation built-ins (malloc, calloc, realloc) are\n \
modeled as nondeterministically returning a null pointer",
  "-eva-mlevel":
"Set to [m] the number of precise dynamic allocations\n \
besides the initial one, for each callstack (defaults to 0)",
  "-eva-context-depth":
"Use <n> as the depth of the default context for Eva.\n \
(defaults to 2)",
  "-eva-context-width":
"Use <n> as the width of the default context for Eva.\n \
(defaults to 2)",
  "-eva-context-valid-pointers":
"Only allocate valid pointers until context-depth,\n \
and then use NULL (defaults to false)",
  "-warn-signed-overflow":
"generate alarms for signed operations that overflow.",
  "-warn-unsigned-overflow":
"generate alarms for unsigned operations that overflow",
  "-warn-signed-downcast":
"generate alarms when signed downcasts may exceed the\n \
destination range",
  "-warn-unsigned-downcast":
"generate alarms when unsigned downcasts may exceed the\n \
destination range",
  "-warn-pointer-downcast":
"generate alarms when a pointer is converted into an integer\n \
but may not be in the range of the destination type.",
  "-warn-special-float":
"generate alarms when special floats are produced: never,\n \
only on NaN, or on infinite floats and NaN (by default).",
  "-warn-invalid-pointer":
"generate alarms when invalid pointers are created.",
  "-warn-invalid-bool":
"generate alarms when trap representations are read from\n \
_Bool lvalues.",
  "-warn-left-shift-negative":
"generate alarms for signed left shifts on negative values.",
  "-warn-right-shift-negative":
"generate alarms for signed right shifts on negative values.",
};

export const fieldsAlwaysVisible:fieldsName[] = [
  "-main",
  "-lib-entry",
  "-eva-precision",
  "-eva-domains",
  "-eva-auto-loop-unroll",
  "-eva-slevel",
  "-eva-split-return",
  "-eva-partition-history",
  "-eva-ilevel",
];

export const formEvaDomains: KeyVal<boolean> = {
  'cvalue': false,
  'equality': false,
  'symbolic-locations': false,
  'octagon': false,
  'gauges': false,
  'multidim': false,
  'bitwise': false,
  'taint': false,
};

export const formWarnSpecialFloat: KeyVal<string> = {
  'none': 'None',
  'nan': 'NaN',
  'non-finite': 'NonFinite',
};

export const formEvaSplitReturn: KeyVal<string> = {
  '': 'None',
  'full': 'Full',
  'auto': 'Auto'
};

export const formEvaEqualityCall: KeyVal<string> = {
  'none': 'None',
  'formals': 'Formals',
  'all': 'All'
};

export const formEvaAllocBuiltin: KeyVal<string> = {
  'imprecise': 'Imprecise',
  'by_stack': 'ByStack',
  'fresh': 'Fresh',
  'fresh_weak': 'FreshWeak',
};

export const domainsToKeyVal = (value: string): KeyVal<boolean> => {
  const domains = { ...formEvaDomains };
  const array = value.split(',');
  for (const domain of array) { domains[domain] = true; }
  return domains;
};

export const KeyValToDomains = (value: KeyVal<boolean>):string => {
  return Object.entries(value).reduce(function (acc: string, cur) {
    if(cur[1]) return (acc + "," + cur[0]);
    else return acc;
  }, "cvalue");
};

export function buttonListEquality(
  a: KeyVal<boolean>, b: KeyVal<boolean>
): boolean {
  const _a = Object.entries(a);
  const _b = Object.entries(b);

  if(_a.length !== _b.length) return false;
  for (const [aKey, aValue] of _a.values()) {
    const bProperty = _b.find(([bkey, ]) => bkey === aKey);
    if(bProperty === undefined) return false;

    const [, bValue] = bProperty;
    if(aValue !== bValue) return false;
  }
  return true;
}

export function domainsEquality(
  a: string, b: string
): boolean {
  return buttonListEquality(
    domainsToKeyVal(a),
    domainsToKeyVal(b)
  );
}

/* ************************************************************************ *
 * Eva Status
 * ************************************************************************ */
export interface EvaStatusInfos {
  message: string;
  title: string;
  icon: string;
}

export type EvaStatus = {
  [key in computationStateType | "undefined"]: EvaStatusInfos
}

export const evaBasicStatus: EvaStatus = {
  'undefined': {
    message: "Eva state undefined.",
    title: "No communication established with the Frama-C server.",
    icon: "CROSS",
  },
  'not_computed': {
    message: "No Eva analysis performed.",
    title: "No Eva analysis has been run yet.",
    icon: "CROSS",
  },
  'computing': {
    message: "Eva analysis in progress…",
    title: "The Eva analysis is currently ongoing.",
    icon: "SPINNER",
  },
  'computed': {
    message: "Eva analysis successfully completed.",
    title: "The Eva analysis has completed successfully.",
    icon: "CHECK",
  },
  'aborted': {
    message: "Eva analysis aborted.",
    title:
      "The Eva analysis has been prematurely aborted by an internal error "+
      "or a user interruption : "+
      "the displayed results will be incomplete.",
    icon: "WARNING",
  },
};
