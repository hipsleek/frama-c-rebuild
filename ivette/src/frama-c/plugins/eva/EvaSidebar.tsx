/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import * as Forms from 'dome/layout/forms';
import { GlobalState, useGlobalState } from 'dome/data/states';
import * as Ivette from 'ivette';
import { useServerField, State, useSyncValue } from 'frama-c/states';
import * as Eva from 'frama-c/plugins/eva/api/analysis';
import * as Params from 'frama-c/kernel/api/parameters';
import * as EvaDef from 'frama-c/plugins/eva/EvaDefinitions';
import { EvaFormOptions } from 'frama-c/plugins/eva/components/Form';
import EvaTools from './components/Tools';

const startComputing = new GlobalState<number>(0);

/** Management of the EVA analyse start timestamp */
function useSyncStartComputing(): void {
  const [start, setStart] = useGlobalState(startComputing);
  const status = useSyncValue(Eva.computationState);
  React.useEffect(() => {
    // If start ≠ 0, this means the counter has already started.
    // It does not restart during a hot reload, for example.
    if(status !== "computing") setStart(0);
    else if(start === 0) setStart(Date.now());
  }, [status, start, setStart]);
}

export function useStartComputing(): number {
  const [start, ] = useGlobalState(startComputing);
  return start;
}

export function EvaSideBar(): JSX.Element {
  useSyncStartComputing();
  const remote = Forms.useController();

  function useField<A>(state: State<A>, defaultValue: A) : Forms.FieldState<A> {
    return Forms.useBuffer(remote, useServerField(state, defaultValue));
  }

  const main = useField(Params.main, "");
  const libEntry = useField(Params.libEntry, false);
  const precision = useField(Params.evaPrecision, 0);

  const domains = useField(Params.evaDomains, "cvalue");
  const domainsFiltered = Forms.useFilter(
    domains,
    EvaDef.domainsToKeyVal,
    EvaDef.KeyValToDomains,
    EvaDef.formEvaDomains
  );
  const EqualityCall = useField(Params.evaEqualityThroughCalls, "none");
  const OctagonCall = useField(Params.evaOctagonThroughCalls, false);
  const MultidimDisjunctive =
    useField(Params.evaMultidimDisjunctiveInvariants, false);

  const AutoLoopUnroll = useField(Params.evaAutoLoopUnroll, 0);
  const MinLoopUnroll = useField(Params.evaMinLoopUnroll, 0);
  const WideningDelay = useField(Params.evaWideningDelay, 0);
  const WideningPeriod = useField(Params.evaWideningPeriod, 0);

  const sLevel = useField(Params.evaSlevel, 0);
  const SplitReturn = useField(Params.evaSplitReturn, "none");
  const PartitionHistory = useField(Params.evaPartitionHistory, 0);

  const iLevel = useField(Params.evaIlevel, 0);
  const pLevel = useField(Params.evaPlevel, 0);
  const Subdivide = useField(Params.evaSubdivideNonLinear, 0);

  const AllocBuiltin = useField(Params.evaAllocBuiltin, "");
  const AllocReturnsNull = useField(Params.evaAllocReturnsNull, false);
  const mLevel = useField(Params.evaMlevel, 0);

  const ContextDepth = useField(Params.evaContextDepth, 0);
  const ContextWidth = useField(Params.evaContextWidth, 0);
  const ContextValidPointers = useField(Params.evaContextValidPointers, false);

  const warnSignedOverflow = useField(Params.warnSignedOverflow, true);
  const warnUnsignedOverflow = useField(Params.warnUnsignedOverflow, false);
  const warnSignedDowncast = useField(Params.warnSignedDowncast, false);
  const warnUnsignedDowncast = useField(Params.warnUnsignedDowncast, false);
  const warnPointerDowncast = useField(Params.warnPointerDowncast, true);
  const warnSpecialFloat = useField(Params.warnSpecialFloat, "non-finite");
  const warnInvalidPointer = useField(Params.warnInvalidPointer, false);
  const warnInvalidBool = useField(Params.warnInvalidBool, true);
  const warnLeftShiftNegative = useField(Params.warnLeftShiftNegative, true);
  const warnRightShiftNegative = useField(Params.warnRightShiftNegative, false);

  const evaFields : EvaDef.EvaFormProps = {
    "-main": {
      label: "Main",
      state: main
    },
    "-lib-entry": {
      label: "LibEntry",
      state: libEntry
    },
    "-eva-precision": {
      label: "Precision",
      step: 1, min: -1, max: 11,
      state: precision
    },
    "-eva-domains": {
      label: "Domains",
      state: domainsFiltered
    },
    "-eva-equality-through-calls": {
      label: "Equality through function call",
      optionRadio: EvaDef.formEvaEqualityCall,
      state: EqualityCall
    },
    "-eva-octagon-through-calls": {
      label: "Octagon through function call",
      state: OctagonCall
    },
    "-eva-multidim-disjunctive-invariants": {
      label: "Multidim disjunctive invariant inference",
      state: MultidimDisjunctive
    },
    "-eva-auto-loop-unroll": {
      label: "Automatic loop unrolling",
      step: 20, min: 0, max: 1024,
      state: AutoLoopUnroll
    },
    "-eva-min-loop-unroll": {
      label: "Minimum loop unrolling",
      step: 1, min: 0, max: 10,
      state: MinLoopUnroll
    },
    "-eva-widening-delay": {
      label: "Widening delay",
      step: 1, min: 1, max: 10,
      state: WideningDelay
    },
    "-eva-widening-period": {
      label: "Widening period",
      step: 1, min: 1, max: 10,
      state: WideningPeriod
    },
    "-eva-slevel": {
      label: "Slevel",
      step: 10, min: 0, max: 10000,
      state: sLevel
    },
    "-eva-split-return": {
      label: "Split return",
      optionRadio: EvaDef.formEvaSplitReturn,
      state: SplitReturn
    },
    "-eva-ilevel": {
      label: "Ilevel",
      step: 10, min: 0, max: 1024,
      state: iLevel
    },
    "-eva-plevel": {
      label: "PLevel",
      step: 100, min: 0, max: 5000,
      state: pLevel
    },
    "-eva-partition-history": {
      label: "History partitioning",
      step: 1, min: 0, max: 5,
      state: PartitionHistory
    },
    "-eva-subdivide-non-linear": {
      label: "Subdivision on non-linear expressions",
      step: 20, min: 0, max: 1024,
      state: Subdivide
    },
    "-eva-alloc-builtin": {
      label: "Allocation builtin behavior",
      optionRadio: EvaDef.formEvaAllocBuiltin,
      state: AllocBuiltin
    },
    "-eva-alloc-returns-null": {
      label: "Allocation return null",
      state: AllocReturnsNull
    },
    "-eva-mlevel": {
      label: "Mlevel",
      step: 1, min: 0, max: 20,
      state: mLevel
    },
    "-eva-context-depth": {
       label: "Initial context depth",
       step: 1, min: 0, max: 10,
       state: ContextDepth
    },
    "-eva-context-width": {
       label: "Initial context width",
       step: 1, min: 1, max: 1024,
       state: ContextWidth
    },
    "-eva-context-valid-pointers": {
       label: "Validity of initial pointers",
       state: ContextValidPointers
    },
    "-warn-signed-overflow": {
      label: "Signed overflow",
      state: warnSignedOverflow
    },
    "-warn-unsigned-overflow": {
      label: "Unsigned overflow",
      state: warnUnsignedOverflow
    },
    "-warn-signed-downcast": {
      label: "Signed downcast",
      state: warnSignedDowncast
    },
    "-warn-unsigned-downcast": {
      label: "Unsigned downcast",
      state: warnUnsignedDowncast
    },
    "-warn-pointer-downcast": {
      label: "Pointer downcast",
      state: warnPointerDowncast
    },
    "-warn-special-float": {
      label: "Special float",
      optionRadio: EvaDef.formWarnSpecialFloat,
      state: warnSpecialFloat
    },
    "-warn-invalid-pointer": {
      label: "Invalid pointer",
      state: warnInvalidPointer
    },
    "-warn-invalid-bool": {
      label: "Invalid _Bool",
      state: warnInvalidBool
    },
    "-warn-left-shift-negative": {
      label: "Negative left shift",
      state: warnLeftShiftNegative
    },
    "-warn-right-shift-negative": {
      label: "Negative right shift",
      state: warnRightShiftNegative
    },
  };

  return (
    <>
      <EvaTools
        remote={remote}
        iconSize={18}
      />
      <EvaFormOptions
        fields={evaFields}
      />
    </>
  );
}

Ivette.registerSidebar({
  id: 'frama-c.plugins.eva_sidebar',
  label: 'EVA',
  icon: 'APPLE',
  title: 'Eva',
  children: <EvaSideBar />,
});
