/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Regions
// --------------------------------------------------------------------------

import React from 'react';
import * as Dome from 'dome';
import * as Tools from 'dome/frame/toolbars';
import { Label } from 'dome/controls/labels';
import { LCD } from 'dome/controls/displays';
import { IconButton } from 'dome/controls/buttons';
import { showSaveFile } from 'dome/dialogs';
import { writeFile } from 'dome/system';
import { Vfill, Vbox } from 'dome/layout/boxes';
import * as Ivette from 'ivette';
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Region from './api';
import { MemoryView } from './memory';
import { AccessList, Attributes } from './access';
import './style.css';

function saveModel(model: string, fct: string): void {
  showSaveFile({
    label: 'Save',
    title: 'Save DOT model for current memory map',
    path: Server.getPath(`region-${fct}.dot`),
    filters: [{ name: 'Graphviz File', extensions: ["dot"] }],
  }).then(file => {
    if (file) writeFile(file, model);
  });
}

function RegionAnalys(): JSX.Element {
  const [kf, setKf] = React.useState<States.Scope>();
  const [kfName, setName] = React.useState<string>();
  const [pinned, setPinned] = React.useState(false);
  const [running, setRunning] = React.useState(false);
  const [selected, setSelected] = React.useState<Region.node>();
  const [model, setModel] = React.useState('');
  const setComputing = Dome.useProtected(setRunning);
  const { scope, marker } = States.useCurrentLocation();
  const { kind, name } = States.useDeclaration(scope);
  const regions = States.useRequestStable(Region.regions, kf);
  const localized = States.useRequestStable(Region.localize, marker);
  const filter = selected ?? localized;
  const region = regions.find(r => r.node === filter);
  const { descr: label } = States.useMarker(marker);
  React.useEffect(() => {
    if (!pinned && kind === 'FUNCTION' && scope !== kf) {
      setKf(scope);
      setName(name);
    } else if (!Server.isRunning()) {
      setKf(undefined);
      setName(undefined);
      setPinned(false);
    }
  }, [pinned, kind, name, scope, kf]);
  async function compute(): Promise<void> {
    try {
      setComputing(true);
      await Server.send(Region.compute, kf);
    } finally {
      setComputing(false);
    }
  }
  const fct = kfName ?? '---';
  return (
    <>
      <Tools.ToolBar>
        <Label label='Function' />
        <LCD className='wp-region-lcd' label={fct} />
        <Tools.Button
          icon={running ? 'EXECUTE' : 'MEDIA.PLAY'}
          title='Run region analysis on the selected function'
          disabled={running}
          visible={kf !== undefined && regions.length === 0}
          onClick={compute}
        />
        <IconButton
          icon='PIN'
          display={kf !== undefined}
          title='Keep focus on current function'
          selected={pinned}
          onClick={() => setPinned(!pinned)}
        />
        <IconButton
          icon='DOWNLOAD'
          display={kfName !== undefined && model !== ''}
          title='Save DOT model'
          onClick={() => saveModel(model, fct)}
        />
      </Tools.ToolBar>
      <Vfill>
        <MemoryView regions={regions}
          label={label}
          localized={localized}
          selected={selected}
          onSelection={setSelected}
          onModelChanged={setModel} />
      </Vfill>
      <Vbox>
        <Attributes region={region} />
        <AccessList region={region} selection={marker} />
      </Vbox>
    </>
  );
}

Ivette.registerComponent({
  id: 'fc.region.main',
  label: 'Region Analysis',
  preferredPosition: 'BD',
  children: <RegionAnalys />,
});

// --------------------------------------------------------------------------
