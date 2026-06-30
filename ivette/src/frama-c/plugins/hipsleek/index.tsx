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
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';
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

function ObligationRow(
  props: { o: obligation, active: boolean, onSelect: (line: number) => void },
): JSX.Element {
  const { o, active, onSelect } = props;
  const where = o.cline > 0 ? `line ${o.cline}` : `.ss ${o.line}`;
  const clickable = o.cline > 0;
  return (
    <div
      className={
        `hipsleek-row${active ? ' active' : ''}${clickable ? ' clickable' : ''}`
      }
      title={clickable ? `Reveal line ${o.cline} in the source` : undefined}
      onClick={clickable ? () => onSelect(o.cline) : undefined}
    >
      <span className="hipsleek-loc">{where}</span>
      <span className={o.proved ? 'hipsleek-tag ok' : 'hipsleek-tag bad'}>
        {o.proved ? 'proved' : 'unproved'}
      </span>
      <span className="hipsleek-entail">{o.entail}</span>
    </div>
  );
}

function KindGroup(
  props: {
    kind: string, obls: obligation[], selectedLine: number,
    onSelect: (line: number) => void,
  },
): JSX.Element {
  const { kind, obls, selectedLine, onSelect } = props;
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
          onSelect={onSelect}
        />
      ))}
    </details>
  );
}

// Shared "active C line" selection used by both HipSleek panels, so the Source
// Code view, the .ss view and the obligation list all highlight the same C line:
//  - markerLine: the selected marker's source line (covers code lines / AST);
//  - revealLine: an explicitly-clicked C line published via revealSourceLine,
//    needed for lines with no AST marker (e.g. /*[SL]*/ spec comments).
// The reveal is cleared whenever the marker moves, so real selections win.
// onSelect publishes the clicked C line (highlighting all three views) and, for
// lines that do have a marker, also moves the kernel selection (syncs the AST).
function useClineSelection(file: string, marker: States.Marker): {
  selectedLine: number,
  onSelect: (line: number) => void,
} {
  const markerLine = States.useMarker(marker).sloc?.line ?? 0;
  const reveal = States.useRevealSourceLine();
  const revealLine = reveal && reveal.file === file ? reveal.line : 0;
  const selectedLine = revealLine || markerLine;
  React.useEffect(() => { States.clearRevealSourceLine(); }, [marker]);
  const onSelect = React.useCallback((line: number) => {
    if (file === '' || line <= 0) return;
    States.revealSourceLine({ file, line });
    Server.send(Ast.getMarkerAt, { file, line, column: 0 })
      .then((m) => { if (m) States.setSelected(m); })
      .catch(() => { /* no marker at that position: ignore */ });
  }, [file]);
  return { selectedLine, onSelect };
}

function HipSleekProof(): JSX.Element {
  const { scope, marker } = States.useCurrentLocation();
  const decl = States.useDeclaration(scope);
  const { kind, name } = decl;
  const info = States.useRequestStable(getProofInfo, scope);
  const file = decl.source?.file ?? '';
  const { selectedLine, onSelect } = useClineSelection(file, marker);

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
          <KindGroup
            key={k} kind={k} obls={obls}
            selectedLine={selectedLine} onSelect={onSelect}
          />
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

// Shows the generated HipSleek .ss ("HIP core") for the selected function: the
// separation-logic program the plugin feeds to hip.exe (the data the proof
// obligations and verdict are about). Each line is clickable and linked to its
// C source line, so clicking a line highlights the matching source line and
// obligations (and vice-versa) — like clicking through the AST.
function HipSleekCore(): JSX.Element {
  const { scope, marker } = States.useCurrentLocation();
  const decl = States.useDeclaration(scope);
  const { kind, name } = decl;
  const info = States.useRequestStable(getProofInfo, scope);
  const file = decl.source?.file ?? '';
  const { selectedLine, onSelect } = useClineSelection(file, marker);

  if (kind !== 'FUNCTION')
    return (
      <div className="hipsleek-proof hipsleek-empty">
        Select a function to see its generated <code>.ss</code> (HIP core).
      </div>
    );

  // Drop trailing blank lines; ssClines is aligned to info.ss split on '\n'.
  const lines = info.ss === '' ? [] : info.ss.replace(/\n+$/, '').split('\n');

  // HipSleek verdict shown as a colored status circle next to the name.
  const verdict = info.verdict || 'UNKNOWN';
  const vclass =
    verdict === 'SUCCESS' ? 'ok'
      : (verdict === 'FAIL' || verdict === 'ERROR') ? 'bad'
        : 'unknown';

  return (
    <div className="hipsleek-proof hipsleek-core">
      <div className="hipsleek-header">
        <span className={`hipsleek-circle ${vclass}`} title={verdict} />
        <span className="hipsleek-fn">{name}</span>
        <span className="hipsleek-core-tag">generated .ss</span>
      </div>
      {lines.length === 0 ? (
        <div className="hipsleek-empty">
          No generated <code>.ss</code> (run with <code>-hipsleek</code>).
        </div>
      ) : (
        <div className="hipsleek-ss">
          {lines.map((ln, i) => {
            const cl = info.ssClines[i] ?? 0;
            const clickable = cl > 0;
            const active = clickable && cl === selectedLine;
            return (
              <div
                key={i}
                className={
                  `hipsleek-ss-line${active ? ' active' : ''}`
                  + `${clickable ? ' clickable' : ''}`
                }
                title={clickable ? `C line ${cl}` : undefined}
                onClick={clickable ? () => onSelect(cl) : undefined}
              >
                <span className="hipsleek-ss-num">{i + 1}</span>
                <span className="hipsleek-ss-code">{ln === '' ? ' ' : ln}</span>
              </div>
            );
          })}
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

Ivette.registerComponent({
  id: 'fc.hipsleek.core',
  label: 'HipSleek Core (.ss)',
  title: 'Generated HipSleek .ss program for the selected function',
  preferredPosition: 'BL',
  children: <HipSleekCore />,
});

// One-click layout: original C source (top-left), the generated HipSleek .ss
// (bottom-left), and the HipSleek proof obligations (right).
Ivette.registerView({
  id: 'fc.hipsleek.view',
  label: 'HipSleek',
  layout: {
    A: 'fc.kernel.sourcecode',
    B: 'fc.hipsleek.core',
    CD: 'fc.hipsleek.proof',
  },
});
