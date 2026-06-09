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
/* -------------------------------------------------------------------------- */

import React from 'react';
import { Label } from 'dome/controls/labels';
import { LCD } from 'dome/controls/displays';
import { Button } from 'dome/controls/buttons';
import * as Box from 'dome/layout/boxes';
import * as DnD from 'dome/dnd';
import { registerSandbox } from 'ivette';
import './sandbox.css';

const source = (id: string, d: DnD.Dragging): string => {
  const dx = d.dragX - d.rootX;
  const dy = d.dragY - d.rootY;
  return `${id} ${dx}:${dy}`;
};

const target = (id: string, d: DnD.Dropping): string => {
  const dx = d.dropX;
  const dy = d.dropY;
  const m = d.meta ? '+' : '?';
  return `${id}${m} ${dx}:${dy}`;
};

interface BlobProps {
  id: string;
  dnd: DnD.DnD;
  setState: (s: string) => void;
}

function Source(props: BlobProps): JSX.Element {
  const { id, dnd, setState } = props;
  return (
    <DnD.DragSource
      className='sandbox-item'
      styleDragging={{ background: 'lightgreen' }}
      dnd={dnd}
      onStart={() => setState(id)}
      onDrag={(d) => setState(source(id, d))}
      onStop={() => setState('--')}
    >
      Blob #{id}
    </DnD.DragSource>
  );
}

function Target(props: BlobProps): JSX.Element {
  const { id, dnd, setState } = props;
  return (
    <DnD.DropTarget
      className='sandbox-item'
      dnd={dnd}
      onDrop={() => setState(id)}
      onDropIn={(d) => setState(target(id, d))}
      onDropOut={() => setState('--')}
    >
      Zone #{id}
    </DnD.DropTarget>
  );
}

function Item({ id }: { id: string }): JSX.Element {
  return <DnD.Item className='sandbox-item' id={id}>Item {id}</DnD.Item>;
}

function UseDnD(): JSX.Element {
  const dnd = DnD.useDnD();
  const [items, setItems] = React.useState<string[]>([]);
  const [source, setSource] = React.useState('--');
  const [target, setTarget] = React.useState('--');
  const onReset = (): void => {
    setItems([]);
    setSource('--');
    setTarget('--');
  };
  return (
    <Box.Vfill>
      <Box.Hbox>
        <Button label='Reset' onClick={onReset} />
        <LCD label={source} />
        <LCD label={target} />
      </Box.Hbox>
      <Box.Hbox>
        <Box.Vbox>
          <Label label='Controlled list' />
          <DnD.List items={items} setItems={setItems}>
            <Item id='A' />
            <Item id='B' />
            <Item id='C' />
            <Item id='D' />
            <Item id='E' />
          </DnD.List>
        </Box.Vbox>
        <Box.Vbox>
          <Label label='Uncontrolled list' />
          <DnD.List>
            <Item id='A' />
            <Item id='B' />
            <Item id='C' />
            <Item id='D' />
            <Item id='E' />
          </DnD.List>
        </Box.Vbox>
        <Box.Vbox>
          <Label label='Draggable Blobs' />
          <Source id='A' dnd={dnd} setState={setSource} />
          <Source id='B' dnd={dnd} setState={setSource} />
          <Source id='C' dnd={dnd} setState={setSource} />
        </Box.Vbox>
        <Box.Vbox>
          <Label label='Droppable Blobs' />
          <Target id='X' dnd={dnd} setState={setTarget} />
          <Target id='Y' dnd={dnd} setState={setTarget} />
          <Target id='Z' dnd={dnd} setState={setTarget} />
        </Box.Vbox>
      </Box.Hbox>
    </Box.Vfill >
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.usednd',
  label: 'Drag & Drop',
  children: <UseDnD />,
});

/* -------------------------------------------------------------------------- */
