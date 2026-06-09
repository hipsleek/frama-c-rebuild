/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import * as Dome from 'dome';
import { useSyncState, useSyncValue } from 'frama-c/states';
import * as EvaParams from 'frama-c/kernel/api/parameters';
import * as EvaTaint from 'frama-c/plugins/eva/api/taint';
import { registerSidebar } from 'ivette';
import * as Toolbars from 'dome/frame/toolbars';
import { Label } from 'dome/controls/labels';
import { Checkbox, IconButton } from 'dome/controls/buttons';
import * as AnalysisStatus from 'frama-c/plugins/eva/components/AnalysisStatus';
import { domainsToKeyVal } from 'frama-c/plugins/eva/EvaDefinitions';
import { SidebarTitle } from 'dome/frame/sidebars';

// --------------------------------------------------------------------------
// --- Globals selection
// ---------------------------------------------------------------------

const pinnedMessageId = 'EvaFilterTaint';

function addTaintMessage(names: string[], remove: () => void): void {
  const pinnedMessageButton =
    <IconButton
      icon='TRASH'
      title='Clear taint selection: show all taints'
      onClick={remove}
    />;
  const message =
    names.length === 0
    ? `No taint is currently shown`
    : names.length === 1
    ? `Only taint "${names[0]}" is currently shown`
    : `Only taints ${names.map(n => `"${n}"`).join(', ')} are currently shown`;
  const pinnedMessage: Toolbars.PinnedMessage = {
    id: pinnedMessageId,
    message,
    actions: pinnedMessageButton
  };
  Toolbars.addPinnedMessage(pinnedMessage);
}

function delTaintMessage(): void {
  Toolbars.delPinnedMessage(pinnedMessageId);
}

function Taints({ taintNames }: { taintNames: string[] }): React.JSX.Element {
  /* [current] is the set of currently selected taints. */
  const [current = taintNames, setCurrent] =
    useSyncState(EvaTaint.currentTaints);

  React.useEffect(() => {
    /* Note that [current] should always be a subset of taintNames. */
    const allTaintsSelected =
      current.length >= taintNames.length
      && taintNames.every((name) => current.includes(name));
    if (allTaintsSelected)
      delTaintMessage();
    else
      addTaintMessage(current, () => setCurrent(taintNames));
  }, [current, setCurrent, taintNames]);

  const onSelection = React.useCallback((v: string) => {
    if (current.includes(v))
      setCurrent(current.filter(n => n !== v));
    else
      setCurrent([...current, v]);
  }, [current, setCurrent]);

  const onContextMenu = React.useCallback(() => {
    const items: Dome.PopupMenuItem[] = [
      { label: 'Select all', onClick: () => setCurrent(taintNames) },
      { label: 'Deselect all', onClick: () => setCurrent([]) }
    ];
    Dome.popupMenu(items);
  }, [setCurrent, taintNames]);

  return (<>
    <SidebarTitle label='Taints' >
      <Toolbars.Button
        icon='TUNINGS'
        title='Configure selection'
        onClick={() => onContextMenu()}
      />
    </SidebarTitle>
    <div className="globals-scrollable-area eva-taint-list">
      {taintNames.map((name) => {
        const selected = current.includes(name);
        return (
          <div key={name} className="eva-taint-row">
            <Checkbox
              label={name}
              title={name}
              value={selected}
              onChange={() => onSelection(name)}
            />
            <span
              aria-hidden
              className="eva-taint-marker"
              style={{ visibility: selected ? 'visible' : 'hidden' }}
            />
          </div>
        );
      })
      }
    </div>
  </>);
}

function NoTaintsMessage(
  { taintDomainEnabled }: { taintDomainEnabled: boolean }
): React.JSX.Element {
  const status = taintDomainEnabled ? 'computed' : 'not_computed';
  const title = taintDomainEnabled
    ? 'No taint results available.'
    : 'Taint analysis is disabled.';
  const message = taintDomainEnabled
    ? 'Nothing to display about taint analysis.'
    : 'Enable the taint domain and rerun Eva.';

  return (
    <div className={"eva-status eva-status-" + status}>
      <div className="eva-status-content">
        <div className="eva-status-message">{title}</div>
        <AnalysisStatus.StatusIcon size={50} status={status} />
        <Label className="eva-status-timer">{message}</Label>
      </div>
    </div>
  );
}

function TaintSidebar(): JSX.Element {
  const evaDomainsValue = useSyncValue(EvaParams.evaDomains);
  const evaDomains = React.useMemo(
    () => evaDomainsValue ?? 'cvalue',
    [evaDomainsValue]
  );
  const taintDomainEnabled = React.useMemo(
    () => Boolean(domainsToKeyVal(evaDomains).taint),
    [evaDomains]
  );
  const taintNamesValue = useSyncValue(EvaTaint.taintNames);
  const taintNames = React.useMemo(
    () => taintNamesValue ?? [],
    [taintNamesValue]
  );

  return (
    <AnalysisStatus.EvaReady>
      {taintNames.length > 0
        ? <Taints taintNames={taintNames} />
        : <NoTaintsMessage taintDomainEnabled={taintDomainEnabled} />}
    </AnalysisStatus.EvaReady>
  );
}

registerSidebar({
  id: 'fc.eva.filter.taints',
  label: 'Taints',
  icon: 'DROP.EMPTY',
  title: 'Taints',
  children: <TaintSidebar />
});

// --------------------------------------------------------------------------
