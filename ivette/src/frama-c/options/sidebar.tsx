/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import { Item, SideBar, SidebarTitle } from 'dome/frame/sidebars';
import { LED } from 'dome/controls/displays';
import * as Forms from 'dome/layout/forms';

import * as Params from 'frama-c/kernel/api/parameters';

import { countFormsModified, IsSetElement, SelectedPlugins } from '.';
import { recordRemotes } from './forms';
import { HelpButton } from 'dome/help';

// --------------------------------------------------------------------------
// --- Sidebar
// --------------------------------------------------------------------------

interface SideBarItemProps {
  plugin: Params.plugin;
  selected: SelectedPlugins;
  isSet: boolean;
  remote: Forms.BufferController;
  onSelection: (e: React.MouseEvent<Element, MouseEvent>) => void
}

function SidebarItem(props: SideBarItemProps): React.JSX.Element {
  const { plugin, selected, isSet, remote, onSelection } = props;
  const controller = Forms.useController(remote);
  const isModified = controller.hasReset() || controller.hasCommit();
  const init = React.useRef(true);

  React.useEffect(() => {
    if(init.current) init.current = false;
    else {
      const current = countFormsModified.getValue();
      if(isModified) countFormsModified.setValue(current + 1);
      else countFormsModified.setValue(current - 1);
    }
  }, [isModified, init]);

  return (
    <Item
      key={plugin.name}
      title={plugin.help}
      label={plugin.name}
      selected={selected[0] === plugin.name || selected[1] === plugin.name}
      onSelection={(e) => onSelection(e)}
    >
      {isModified && <LED status='warning' title='Pending modification'/>}
      {isSet && <LED status='active' title='Modified fields'/>}
      {selected[0] === plugin.name && '(left)'}
      {selected[1] === plugin.name && '(right)'}
    </Item>
  );
}

interface SideBarProps {
  plugins: Params.plugin[];
  isSetElement: IsSetElement;
  selectedState: [
    SelectedPlugins,
    React.Dispatch<React.SetStateAction<SelectedPlugins>>
  ];
  remotes: recordRemotes;
}

export function OptionsSidebar(props: SideBarProps): React.JSX.Element {
  const { selectedState, isSetElement, plugins, remotes } = props;
  const [selected, setSelected] = selectedState;

  const onSelection = React.useCallback(
    (e: React.MouseEvent, p: Params.plugin) => {
    setSelected(v => {
      if(e.ctrlKey) {
        if(p.name === v[0]) return v;
        else if(p.name === v[1]) return [v[1], v[0]];
        else return [p.name, v[1]];
      } else {
        if(p.name === v[1]) return v;
        else if(p.name === v[0]) return [v[1], v[0]];
        else return [v[0], p.name];
      }
    });
  }, [setSelected]);

  return (
    <SideBar>
      <SidebarTitle label='Plugins'>
        <HelpButton id="framac-parameters" size={14} />
      </SidebarTitle>
      <div className="globals-scrollable-area">
        { plugins.map(p => <SidebarItem key={p.name}
            plugin={p}
            isSet={isSetElement[p.name]}
            onSelection={(e: React.MouseEvent) => onSelection(e, p)}
            selected={selected}
            remote={remotes[p.name]} />
          )
        }
      </div>
    </SideBar>
  );
}
