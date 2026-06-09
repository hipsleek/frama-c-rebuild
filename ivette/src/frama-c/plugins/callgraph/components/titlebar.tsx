/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import * as Ivette from 'ivette';

import { IconButton } from 'dome/controls/buttons';
import { Button, ButtonGroup, Inset } from 'dome/frame/toolbars';
import * as Themes from 'dome/themes';
import { Pattern } from 'dome/text/markdown';
import { Dropdown } from 'dome/dialogs';

import doc from '../callgraph.md?raw';
import { ModeDisplay } from '../definitions';
import { IThreeStateButton, ThreeStateButton, TThreesButtonState
} from './buttons';

/* -------------------------------------------------------------------------- */
/* --- Callgraph titlebar component                                       --- */
/* -------------------------------------------------------------------------- */

interface CallgraphTitleBarProps {
  /** filtering menu to filtering nodes */
  contextFctFilter: React.JSX.Element,
  /** automatic graph centering */
  autoCenterState: [boolean, () => void],
  /** automatic selection */
  autoSelectState: [boolean, () => void]
}

export function CallgraphTitleBar(props: CallgraphTitleBarProps): JSX.Element {
  const { autoCenterState, autoSelectState, contextFctFilter } = props;
  const [ autoCenter, flipAutoCenter ] = autoCenterState;
  const [ autoSelect, flipAutoSelect] = autoSelectState;

  return (
    <Ivette.TitleBar help="callgraph">
      <Dropdown
        control={<IconButton icon='FILTER'
            title={`Filter functions appearing in the graph`}
          />}
      >{contextFctFilter}</Dropdown>
      <Inset />
      <IconButton
        icon={"TARGET"}
        onClick={flipAutoCenter}
        kind={autoCenter ? "positive" : "default"}
        title={"Move the camera to show each node after each render"}
      />
      <IconButton
        icon={"PIN"}
        onClick={flipAutoSelect}
        kind={autoSelect ? "positive" : "default"}
        title={"Automatically select node of the function selected in AST"}
      />
      <Inset />
    </Ivette.TitleBar>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Callgraph documentation                                            --- */
/* -------------------------------------------------------------------------- */

/** Pattern used for callgraph documentation */
const TSButtonTag: Pattern = {
  pattern: /\[button-displaymode\]/g,
  replace: (key: number, match?: RegExpExecArray) => {
    return match ? <span key={key}>{DocShowNodesButton()}</span> : null;
  }
};

/** Pattern used for callgraph documentation */
const selectButtonTag: Pattern = {
  pattern: /\[button-select\]/g,
  replace: (key: number, match?: RegExpExecArray) => {
    return match ? <Button key={key} label="Select" title={`Nodes selection`}/>
      : null;
  }
};

export const docCallgraph = {
  id: 'callgraph',
  content: doc,
  patterns: [selectButtonTag, TSButtonTag]
};

interface ShowNodesButtonProps {
  displayModeState: [ModeDisplay, (newValue: ModeDisplay) => void],
  selectedParentsState: TThreesButtonState,
  selectedChildrenState: TThreesButtonState,
}

function ShowNodesButton(props: ShowNodesButtonProps): JSX.Element {
  const {
    displayModeState, selectedParentsState, selectedChildrenState
  } = props;
  const [ displayMode, setDisplayMode] = displayModeState;

  return (
    <ButtonGroup>
      <Button
        label='all'
        title='show all nodes'
        selected={displayMode === 'all'}
        onClick={() => setDisplayMode("all")}
        />
      <Button
        label='linked'
        title='only show nodes linked to the selected ones'
        selected={displayMode === 'linked'}
        onClick={() => setDisplayMode("linked")}
        />
      <Button
        label='selected'
        title='only show selected nodes, their parents and their childrens'
        selected={displayMode === 'selected'}
        onClick={() => setDisplayMode("selected")}
        />
      { displayMode === "selected" ? (
          <>
            <ThreeStateButton
              label={"Parents"}
              title={"Choose how many parents you want to see."}
              buttonState={selectedParentsState}
              />
            <ThreeStateButton
              label={"Children"}
              title={"Choose how many children you want to see."}
              buttonState={selectedChildrenState}
              />
          </>
        ) : <></>
      }
    </ButtonGroup>
  );
}

export function DocShowNodesButton(): JSX.Element {
  const displayModeState = React.useState<ModeDisplay>("all");
  const selectedParentsState = React.useState<IThreeStateButton>(
      { active: false, max: false, value: 1 });
  const selectedChildrenState = React.useState<IThreeStateButton>(
      { active: true, max: true, value: 1 });
  const [ displayMode, ] = displayModeState;
  const [ parent, ] = selectedParentsState;
  const [ children, ] = selectedChildrenState;

  const style = Themes.useStyle();
  const infosStyle = { color: style.getPropertyValue('--text-highlighted') };

  function getDocSelected(
    parent: IThreeStateButton,
    children: IThreeStateButton
  ):JSX.Element {
    function getDocTSB(name: string, tsb: IThreeStateButton):string {
      return !tsb.active ? '' :
        tsb.max ? ` all their ${name}` :
          tsb.value > 0 ?
            (tsb.value+' level'+(tsb.value > 1 ? 's':'')+` of their ${name}`):
            "";
    }
    const p = getDocTSB('parents', parent);
    const c = getDocTSB('children', children);

    return (
      <div style={infosStyle}>
        Show selected nodes { (p || c) && " with " }
        { p }{ p && c && " and " }{ c }
        { !p && !c && " only " }.
      </div>
    );
  }

  const docAll = <div style={infosStyle}>Show all nodes.</div>;
  const docLinked = <div style={infosStyle}>
    Show only nodes linked to at least one other node.
    </div>;
  const docSelected = getDocSelected(parent, children);

  return (
    <>
      <ShowNodesButton
        displayModeState={displayModeState}
        selectedParentsState={selectedParentsState}
        selectedChildrenState={selectedChildrenState}
      />
      { displayMode === 'all' ? docAll :
        displayMode === 'linked' ? docLinked :
        docSelected
      }
    </>
  );
}
