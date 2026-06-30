/* ************************************************************************ */
/*                                                                          */
/*   HipSleek Proof panel                                                    */
/*                                                                          */
/*   Shows the HipSleek proof obligations for the selected function,         */
/*   grouped by kind and tied to the C source line they come from:          */
/*     - Precondition (PRE), Field access (BIND), Recursive-call (PRE_REC),  */
/*       Postcondition (POST)                                                */
/*   Each group is collapsible; rows show the C line, a proved/unproved      */
/*   badge, and the (decluttered) entailment. Data is served by the OCaml    */
/*   request `plugins.hipsleek.getProofInfo`.                                */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import * as Ivette from 'ivette';
import * as States from 'frama-c/states';
import { getProofInfo, obligation } from './api';
import './style.css';

const KIND_LABEL: { [k: string]: string } = {
  PRE: 'Precondition',
  BIND: 'Field access (dereference safe)',
  PRE_REC: 'Recursive-call precondition',
  POST: 'Postcondition',
  ASSERT: 'Assertion',
};

// Stable display order; unknown kinds appended in encounter order.
const KIND_ORDER = ['PRE', 'BIND', 'PRE_REC', 'POST', 'ASSERT'];

function groupByKind(obls: obligation[]): [string, obligation[]][] {
  const map = new Map<string, obligation[]>();
  obls.forEach((o) => {
    const a = map.get(o.kind) ?? [];
    a.push(o);
    map.set(o.kind, a);
  });
  const kinds = Array.from(map.keys()).sort((a, b) => {
    const ia = KIND_ORDER.indexOf(a); const ib = KIND_ORDER.indexOf(b);
    return (ia < 0 ? 99 : ia) - (ib < 0 ? 99 : ib);
  });
  return kinds.map((k) => [k, map.get(k) ?? []]);
}

function ObligationRow(props: { o: obligation, active: boolean }): JSX.Element {
  const { o, active } = props;
  const where = o.cline > 0 ? `line ${o.cline}` : `.ss ${o.line}`;
  return (
    <div className={active ? 'hipsleek-row active' : 'hipsleek-row'}>
      <span className="hipsleek-loc">{where}</span>
      <span className={o.proved ? 'hipsleek-tag ok' : 'hipsleek-tag bad'}>
        {o.proved ? 'proved' : 'unproved'}
      </span>
      <span className="hipsleek-entail">{o.entail}</span>
    </div>
  );
}

function KindGroup(
  props: { kind: string, obls: obligation[], selectedLine: number },
): JSX.Element {
  const { kind, obls, selectedLine } = props;
  const label = KIND_LABEL[kind] ?? kind;
  const ok = obls.filter((o) => o.proved).length;
  const allOk = ok === obls.length;
  const hasActive =
    selectedLine > 0 && obls.some((o) => o.cline === selectedLine);
  // Open if unproved obligations exist, or the selected source line lives here.
  return (
    <details className="hipsleek-group" open={!allOk || hasActive}>
      <summary>
        <span className="hipsleek-group-label">{label}</span>
        <span className={allOk ? 'hipsleek-count ok' : 'hipsleek-count bad'}>
          {ok}/{obls.length} proved
        </span>
      </summary>
      {obls.map((o, i) => (
        <ObligationRow
          key={i}
          o={o}
          active={selectedLine > 0 && o.cline === selectedLine}
        />
      ))}
    </details>
  );
}

function HipSleekProof(): JSX.Element {
  const { scope, marker } = States.useCurrentLocation();
  const { kind, name } = States.useDeclaration(scope);
  const info = States.useRequestStable(getProofInfo, scope);
  // C source line of the current selection, so obligations from that line are
  // highlighted as the user clicks around the Source Code / AST views.
  const selectedLine = States.useMarker(marker).sloc?.line ?? 0;

  if (kind !== 'FUNCTION')
    return (
      <div className="hipsleek-proof hipsleek-empty">
        Select a function to see its HipSleek proof.
      </div>
    );

  const verdict = info.verdict || 'UNKNOWN';
  const vclass =
    verdict === 'SUCCESS' ? 'ok'
      : (verdict === 'FAIL' || verdict === 'ERROR') ? 'bad'
        : 'unknown';
  const groups = groupByKind(info.obligations);

  return (
    <div className="hipsleek-proof">
      <div className="hipsleek-header">
        <span className="hipsleek-fn">{name}</span>
        <span className={`hipsleek-verdict ${vclass}`}>{verdict}</span>
      </div>
      {info.obligations.length === 0 ? (
        <div className="hipsleek-empty">
          No proof obligations (run with <code>-hipsleek-proof-log</code>).
        </div>
      ) : (
        groups.map(([k, obls]) => (
          <KindGroup key={k} kind={k} obls={obls} selectedLine={selectedLine} />
        ))
      )}
      {info.fidelity.length > 0 && (
        <div className="hipsleek-fidelity">
          <div className="hipsleek-fidelity-title">⚠ translation fidelity</div>
          <ul>{info.fidelity.map((w, i) => <li key={i}>{w}</li>)}</ul>
        </div>
      )}
    </div>
  );
}

Ivette.registerGroup({
  id: 'fc.hipsleek',
  label: 'HipSleek',
});

Ivette.registerComponent({
  id: 'fc.hipsleek.proof',
  label: 'HipSleek Proof',
  title: 'HipSleek proof obligations for the selected function',
  preferredPosition: 'BD',
  children: <HipSleekProof />,
});

// One-click layout: original C source (top-left), normalized AST (bottom-left),
// and the HipSleek proof obligations (right).
Ivette.registerView({
  id: 'fc.hipsleek.view',
  label: 'HipSleek',
  layout: {
    A: 'fc.kernel.sourcecode',
    B: 'fc.kernel.astview',
    CD: 'fc.hipsleek.proof',
  },
});
