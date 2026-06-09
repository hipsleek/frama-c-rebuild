/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import * as Dome from 'dome';
import * as Display from 'ivette/display';
import { showHelp } from 'dome/help';
import { FieldState, TextField, useState } from 'dome/layout/forms';
import { Label } from 'dome/controls/labels';
import { closeModal, Modal, showModal } from 'dome/dialogs';
import { IconButton } from 'dome/controls/buttons';
import { Button, ButtonGroup } from 'dome/frame/toolbars';
import { Hbox } from 'dome/layout/boxes';
import { Icon } from 'dome/controls/icons';

import * as Server from 'frama-c/server';
import * as Ast from 'frama-c/kernel/api/ast';
import * as ASTview from 'frama-c/kernel/ASTview';
import * as Locations from 'frama-c/kernel/Locations';
import { getWritesLval, getReadsLval } from 'frama-c/plugins/studia/api/studia';

import './style.css';
import { evaNeeded, EvaReady } from '../eva/components/AnalysisStatus';

type access = 'Reads' | 'Writes';

async function computeStudiaSelection(
  kind: access,
  marker: Ast.marker,
  descr: string,
  onError: (err: string) => void
): Promise<void> {
  const request = kind === 'Reads' ? getReadsLval : getWritesLval;
  const data = await Server.send(request, marker).catch(onError);
  const markers = data?.direct ?? [];
  if (markers.length > 0) {
    const label = (kind === 'Reads' ? 'Reads of ' : 'Writes to ') + `${descr}`;
    const access = kind === 'Reads' ? 'accessing' : 'modifying';
    const title =
      `Statements ${access} the memory location pointed by ${descr}.`;
    Locations.setSelection({
      plugin: 'Studia', label, title, markers,
    });
  } else {
    const label = `No ${kind.toLowerCase()} to ${descr}`;
    Locations.setSelection({
      plugin: 'Studia', label, markers: []
    });
  }
}

function handleError(err: string): void {
  Display.showWarning({ label: 'Studia Failure', title: `Error (${err})` });
}

/** Builds the Studia entries in the contextual menu about a given marker.  */
export function buildMenu(
  menu: Dome.PopupMenuItem[],
  attr: Ast.markerAttributesData,
): void {
  function addSubMenu(submenu: Dome.PopupMenuItem[]): void {
    const helpItem = {
      label: 'Help',
      onClick: () => showHelp('eva-studia'),
    };
    submenu.push(helpItem);
    menu.push({ label: 'Studia', submenu });
  }
  const { marker, kind } = attr;
  switch (kind) {
    case 'LVAL':
    case 'DVAR':
    case 'LVAR':
      addSubMenu([
        {
          label: `Select reads`,
          onClick: () => evaNeeded(() =>
            computeStudiaSelection('Reads', marker, attr.descr, handleError)
          )
        },
        {
          label: `Select writes`,
          onClick: () => evaNeeded(() =>
            computeStudiaSelection('Writes', marker, attr.descr, handleError)
          )
        }
      ]);
      return;
    case 'STMT':
      menu.push({ label: 'Studia…', onClick: () => showModalStudia(attr) });
      return;
  }
}

ASTview.registerMarkerMenuExtender(buildMenu);

/* -------------------------------------------------------------------------- */
/* --- Modal                                                              --- */
/* -------------------------------------------------------------------------- */

interface ModalTextFieldProps {
  attr: Ast.markerAttributesData;
}

function ModalStudiaSearch(props: ModalTextFieldProps) : React.JSX.Element {
  const { attr } = props;
  const state = useState('');
  const value = state.value;
  const [akind, setAkind] = React.useState<access>('Reads');
  const [error, setError] = React.useState<string | undefined>();

  const onValidate = React.useCallback(async (p: string) => {
    const data = { stmt: attr.marker, term: p };
    const marker = await Server.send(Ast.parseLval, data).catch(setError);
    if (marker) {
      closeModal();
      computeStudiaSelection(akind, marker, p, setError);
    }
  }, [akind, attr.marker, setError]);

  const helpButton =
    <IconButton
      icon='HELP' size={15}
      title='Open Studia documentation'
      onClick={() => showHelp('eva-studia')}
    />;

  const readOrWrite = (akind === 'Reads') ? "read" : "write";

  return <Modal
    className='modal-studia'
    label={'Studia'}
    actions={helpButton}
  >
    <EvaReady>
      <div>
        <Label>
          Find all statements that may {readOrWrite} the given lvalue:
        </Label>
        <Hbox>
          <ButtonGroup>
            <Button
              label='Reads of'
              selected={akind === 'Reads'}
              onClick={() => setAkind('Reads')}
            />
            <Button
              label='Writes to'
              selected={akind === 'Writes'}
              onClick={() => setAkind('Writes')}
            />
          </ButtonGroup>
          <TextField
            label=''
            latency={0}
            autoFocus={true}
            state={state as FieldState<string | undefined>}
            onKeyDown={(e) => { if (e.key === "Enter") onValidate(value); }}
          />
          <Button
            label='Search'
            onClick={() => onValidate(value)}
          />
        </Hbox>
        { error &&
          <Hbox>
            <Icon id='WARNING' kind='warning' />
            <span>{error}</span>
          </Hbox> }
      </div>
    </EvaReady>
  </Modal>;
}

async function showModalStudia(attr: Ast.markerAttributesData): Promise<void> {
  showModal(<ModalStudiaSearch attr={attr} />);
}

/* -------------------------------------------------------------------------- */
