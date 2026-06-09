/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import { Modal, showMessageBox, showModal } from 'dome/dialogs';
import { alpha } from 'dome/data/compare';
import { LSplit } from 'dome/layout/splitters';
import * as Toolbar from 'dome/frame/toolbars';
import { GlobalState } from 'dome/data/states';

import * as Server from 'frama-c/server';
import * as Params from 'frama-c/kernel/api/parameters';

import { OptionsForms, useRemotes } from './forms';
import { OptionsSidebar } from './sidebar';
import './style.css';
import { Debug } from 'dome/system';

const D = new Debug('Options');

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

/** left and right form selected => [left, right] */
export type SelectedPlugins = [string, string];
/** recording of elements modified by the user */
export type IsSetElement = Record<string, boolean>;
/** Parameters by section */
export type SectionParams = [string, Params.parameter[]];

interface PContext {
  params: Record<string, SectionParams[]>;
  isSetElement: IsSetElement;
  addPluginsSet: (value: IsSetElement) => void;
}

interface PContextById extends Omit<PContext, 'params'> {
  params: SectionParams[];
}

const PLUGINSCONTEXT =
  React.createContext<PContext | undefined>(undefined);

export function usePluginsContextById(id: string): PContextById {
  const context = React.useContext(PLUGINSCONTEXT);
  if (!context) {
    throw new Error("usePluginsContext must be used in <Provider>");
  }
  const { params, isSetElement, addPluginsSet } = context;
  return { params: params[id], isSetElement, addPluginsSet };
}


// --------------------------------------------------------------------------
// --- Options
// --------------------------------------------------------------------------

const defaultSelected: SelectedPlugins = ['kernel', ''];

/** Number of forms modified */
export const countFormsModified = new GlobalState<number>(0);

export default function Options(): React.JSX.Element | null {
  React.useEffect(() => countFormsModified.setValue(0), []);

  /** Remotes */
  const remotesState = useRemotes();
  const [remotes,] = remotesState;

  /** Selected plugins */
  const selectedState = React.useState<SelectedPlugins>(defaultSelected);

  /** List of plugins */
  const [plugins, setPlugins] = React.useState<Params.plugin[]>([]);
  React.useEffect(() => {
    const fetchPlugins = async (): Promise<void> => {
      const plugins = await Server.send(Params.getPlugins, {});
      setPlugins(plugins.sort((a, b) => alpha(a.name, b.name)));
    };
    if(Server.isRunning()) fetchPlugins();
    else Server.onReady(fetchPlugins);
  }, []);

  /** List of plugins set, true if plugin contains a field set by the user */
  const [isSetElement, setIsSetElement] = React.useState<IsSetElement>({});
  const addPluginsSet = React.useCallback((value: IsSetElement) => {
    setIsSetElement(prev => ({ ...prev, ...value })); }, [setIsSetElement]);

  /** Set of parameters (grouped by section) for each plugin name */
  const [params, setParams] =
    React.useState<Record<string, SectionParams[]>>({});
  React.useEffect(() => {
    const fetchParams = async (id: string): Promise<void> => {
      try {
        const params = await Server.send(Params.getPluginParameters, id);
        /** Initial check if field 'isSet' for sidebar items */
        if(params.find(plugin => plugin[1].find(param => param.isSet)))
          addPluginsSet({ [id]: true });
        const sortedParams = params.sort((a, b) => alpha(a[0], b[0]));
        setParams(v => ({ ...v, [id]: sortedParams }));
      } catch (err) {
        D.warn("Error on getPluginParameters: ", id, err);
      }
    };
    plugins.map(p => fetchParams(p.name));
  }, [plugins, addPluginsSet]);

  const paramsNb = Object.keys(params).length;
  if (paramsNb !== plugins.length || paramsNb <= 0) return null;

  return (
    <PLUGINSCONTEXT.Provider value={{ params, isSetElement, addPluginsSet }}>
      <div className='framac-options'>
        <LSplit settings="frama-c.options" unfold={true}>
          <OptionsSidebar
            selectedState={selectedState}
            plugins={plugins}
            remotes={remotes}
            isSetElement={isSetElement}
          />
          <OptionsForms
            selectedState={selectedState}
            remotesState={remotesState}
            />
        </LSplit>
      </div>
    </PLUGINSCONTEXT.Provider>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Frama-C Options Modal                                              --- */
/* -------------------------------------------------------------------------- */

async function onClose(): Promise<boolean> {
  if(countFormsModified.getValue() <= 0) return true;
  const confirm = await showMessageBox({
      block: true,
      buttons: [
      { label: 'Cancel' },
      { label: 'Ok', value: true }
    ],
    message: 'Close Frama-C parameters?',
    details: 'Any parameter changes that have not been applied will be lost.'
  });
  return !!confirm;
}

export function showOptionsModal(): void {
  showModal(
    <Modal className='modal-framac-options' label='Frama-C Parameters'>
      <Options/>
    </Modal>,
    onClose
  );
}

export function ButtonOptions(): React.JSX.Element {
  return <Toolbar.Button
    icon='SETTINGS'
    onClick={showOptionsModal}
    title='Open Frama-C parameters'
  />;
}
