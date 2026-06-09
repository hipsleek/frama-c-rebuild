/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Sandbox Ivette Component.                                          --- */
/* --- Only appears in DEVEL mode.                                        --- */
/* --- Please, keep it empty.                                             --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import * as Ctrl from 'dome/controls/buttons';
import * as Disp from 'dome/controls/displays';
import * as Box from 'dome/layout/boxes';
import { QSplit, QPane } from 'dome/layout/qsplit';
import { registerSandbox } from 'ivette';

function Quarter(props: {
  value?: string,
  setValue: (v: string | undefined) => void,
}): JSX.Element {
  const onChange = (s?: string): void => props.setValue(s ? s : undefined);
  return (
    <Ctrl.SelectMenu value={props.value ?? ''} onChange={onChange}>
      <option value=''>-</option>
      <option value='A'>A</option>
      <option value='B'>B</option>
      <option value='C'>C</option>
      <option value='D'>D</option>
      <option value='E'>E</option>
    </Ctrl.SelectMenu>
  );
}

function Pane(props: { id: string, background: string }): JSX.Element {
  const { id, background } = props;
  const css: React.CSSProperties = {
    width: '100%',
    height: '100%',
    textAlign: 'center',
    background,
  };
  return (
    <QPane id={id}><div style={css}>{id}</div></QPane>
  );
}

const round = (r: number): number => Math.round(r * 100) / 100;

function QSplitSandbox(): JSX.Element {
  const [HTOP, setHTOP] = React.useState(0.5);
  const [HBOTTOM, setHBOTTOM] = React.useState(0.5);
  const [V, setV] = React.useState(0.5);
  const [A, setA] = React.useState<string | undefined>('A');
  const [B, setB] = React.useState<string | undefined>('B');
  const [C, setC] = React.useState<string | undefined>('C');
  const [D, setD] = React.useState<string | undefined>('D');
  const setPosition = React.useCallback(
    (hTop: number, hBottom: number, v: number) => {
      setHTOP(hTop);
      setHBOTTOM(hBottom);
      setV(v);
  }, [setHTOP, setHBOTTOM, setV]);
  const reset = (): void => {
    setPosition(0.5, 0.5, 0.5);
    setA('A');
    setB('B');
    setC('C');
    setD('D');
  };
  const clear = (): void => {
    setPosition(0.5, 0.5, 0.5);
    setA(undefined);
    setB(undefined);
    setC(undefined);
    setD(undefined);
  };
  return (
    <Box.Vfill>
      <Box.Hfill>
        <Ctrl.Button icon='RELOAD' label='Reset' onClick={reset} />
        <Ctrl.Button icon='TRASH' label='Clear' onClick={clear} />
        <Box.Space />
        <Disp.LCD>
          HTOP={round(HTOP)} HBOTTOM={round(HBOTTOM)} V={round(V)}
        </Disp.LCD>
        <Box.Space />
        <Quarter value={A} setValue={setA} />
        <Quarter value={B} setValue={setB} />
        <Quarter value={C} setValue={setC} />
        <Quarter value={D} setValue={setD} />
      </Box.Hfill>
      <QSplit A={A} B={B} C={C} D={D} HTOP={HTOP} HBOTTOM={HBOTTOM} V={V}
        setPosition={setPosition}>
        <Pane id='A' background='lightblue' />
        <Pane id='B' background='lightgreen' />
        <Pane id='C' background='#8282db' />
        <Pane id='D' background='coral' />
        <Pane id='E' background='red' />
      </QSplit>
    </Box.Vfill >
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.qsplit',
  label: 'Q-Splitters',
  children: <QSplitSandbox />,
});

/* -------------------------------------------------------------------------- */
