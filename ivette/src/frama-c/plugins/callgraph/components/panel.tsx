/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import * as Dome from 'dome';
import { Label } from 'dome/controls/labels';
import { Panel as DPanel, ListElement } from 'dome/frame/panel';
import { ButtonGroup, Button } from 'dome/frame/toolbars';
import { LED } from 'dome/controls/displays';
import { Icon } from 'dome/controls/icons';

import { decl } from 'frama-c/kernel/api/ast';
import { statusData } from 'frama-c/kernel/api/properties';
import { onContextMenu, EFilterType } from 'frama-c/kernel/Properties';
import * as EvaAst from 'frama-c/plugins/eva/api/ast';
import * as States from 'frama-c/states';
import {
  renderTaint, useEvaPropertiesFilter, useKindPropertiesFilter, useStatusFilter
} from 'frama-c/kernel/Properties';

import { CGData, SelectedNodes } from "../definitions";
import { IconButton } from 'dome/controls/buttons';
import { Hbox } from 'dome/layout/boxes';


/* -------------------------------------------------------------------------- */
/* --- Callgraph Panel component                                          --- */
/* -------------------------------------------------------------------------- */

interface PanelFilterParams {
  contextMenuStatus: Dome.PopupMenuItem[];
  contextMenuKind: Dome.PopupMenuItem[];
  contextMenuEva: Dome.PopupMenuItem[];
}

function getPanelFilters(params: PanelFilterParams): JSX.Element {
  const { contextMenuStatus, contextMenuKind, contextMenuEva } = params;

  return (
    <>
      <ButtonGroup>
        <Button
          label="Status"
          title={`Select visible status`}
          onClick={() => Dome.popupMenu(contextMenuStatus)}
          />
        <Button
          icon={'TUNINGS'}
          title={`Select visible status`}
          onClick={() => onContextMenu(EFilterType.STATUS)}
          />
      </ButtonGroup>

      <ButtonGroup>
        <Button
          label="Kind"
          title={`Select visible kind`}
          onClick={() => Dome.popupMenu(contextMenuKind)}
          />
        <Button
          icon={'TUNINGS'}
          title={`Select visible status`}
          onClick={() => onContextMenu(EFilterType.KIND)}
          />
      </ButtonGroup>

      <Button
          label="Eva"
          title={`Select visible Eva properties`}
          onClick={() => Dome.popupMenu(contextMenuEva)}
          />
    </>
  );
}

interface IGetElementParams {
  graph?: CGData;
  id?: string;
  properties: statusData[];
  evaProperties: EvaAst.propertiesData[];
  style: CSSStyleDeclaration;
  showStatus: (status: statusData) => boolean;
  showKind: (status: statusData) => boolean;
  showEva: (status: statusData) => boolean;
}

function getElement(
  params: IGetElementParams
): JSX.Element | undefined {
  const { graph, id, properties, evaProperties,
    style, showStatus, showKind, showEva } = params;

  if (!id || !graph) return undefined;

  const allProperties = properties.map((elt) => {
    const evap = evaProperties.find((evaps) => evaps.key === elt.key);
    return { ...elt, ...evap };
  });

  const error = allProperties
    .filter(showStatus)
    .filter(showKind)
    .filter(showEva)
    .map((elt) => {
      const priority = elt?.priority;
      const cssVarName ='--status-'+elt.status.replaceAll("_", '-');
      const color = style.getPropertyValue(cssVarName);

      return (
        <div
          key={elt.key}
          className="cg-panel-error"
          title={elt.descr}
          onClick={() => {
            States.setCurrentLocation({ scope: elt.scope, marker: elt.key });
          }}
        >
          {LED({
              title: "status = "+elt.status+"\nkind = "+elt.kind,
              style: { background: color },
            })
          }
          { priority && <Icon id="ATTENTION" className='cg-panel-priority'/> }
          <Label>{elt.predicate ?? (elt.status+" : "+elt.kind)}</Label>
        </div>
      );
    }
  );

  const taint = evaProperties.filter(
    (ps) => ps.taint === "direct_taint" || ps.taint === "indirect_taint"
  ).map(
    (ps) => {
      const taintIcon = renderTaint({ name: ps.taint });
      return !taintIcon ? null : React.cloneElement(taintIcon, {
        key: ps.key,
        onClick: () => {
          const scope = properties.find((elt) => elt.key === ps.key)?.scope;
          if(scope) States.setCurrentLocation({ scope: scope, marker: ps.key });
        }
      });
    }
  );

  const node = graph?.nodes.find((elt) => elt.id === id);
  const content: JSX.Element =  (
    <div key={id} className='cg-panel-element'>
      <div
        className='cg-panel-element-name'
        onClick={() => States.setCurrentLocation({ scope: id as decl })}
      >
        {node?.label || ""}
      </div>
      <div className='cg-panel-element-content'>
        { taint.length > 0 &&
          <div className='cg-panel-taint'>
            <Label>Taint :</Label>
            {taint}
          </div>
        }
        { error.length > 0 &&
          <div className="cg-panel-errors" key={id}>{error}</div>
        }
      </div>
    </div>
  );


  if (id) return content;
  return undefined;
}

interface IGetElementListParams {
  selectedNodes: SelectedNodes;
  graph?: CGData;
  properties: statusData[];
  evaProperties: EvaAst.propertiesData[];
  selected?: number;
  nodes?: number;
  links?: number;
  tainted?: boolean;
  style: CSSStyleDeclaration;
  showStatus: (status: statusData) => boolean;
  showKind: (status: statusData) => boolean;
  showEva: (status: statusData) => boolean;
}

function getElementList(
  params: IGetElementListParams
): JSX.Element[] {
  const { graph, selectedNodes, properties, evaProperties,
    style, showStatus, showKind, showEva } = params;

  const list: JSX.Element[] = [];

  selectedNodes.forEach((elt: string) => {
    const propsKeys: string[] = [];
    const prs = properties.filter((prop) => {
      if(prop.scope === elt) {
        propsKeys.push(prop.key);
      }
      return prop.scope === elt;
    });
    const evaps = evaProperties.filter((evaprop) =>
      propsKeys.includes(evaprop.key));
    const newElement = getElement({
      graph: graph,
      id: elt,
      properties: prs,
      evaProperties: evaps,
      style,
      showStatus,
      showKind,
      showEva,
    });
    if(newElement) list.push(newElement);
  });

  return list;
}

/* -------------------------------------------------------------------------- */
/* --- Panel content                                                      --- */
/* -------------------------------------------------------------------------- */
interface PanelContentProps {
  graphData: CGData;
  selectedNodes: SelectedNodes;
  tainted: number;
  properties: statusData[];
  evaProperties: EvaAst.propertiesData[];
  style: CSSStyleDeclaration;
  visibleNodes: number;
  visibleLinks: number;
  panelVisibleState: [boolean, () => void];
}

export function Panel(
  props: PanelContentProps
): JSX.Element {
  const { graphData, selectedNodes, tainted,
    properties, evaProperties, style,
    visibleNodes, visibleLinks, panelVisibleState } = props;
  const countNodes = graphData.nodes.length;
  const countLinks = graphData.links.length;
  const countSelected = selectedNodes.size;
  const [ panelVisible, flipPanelVisible ] = panelVisibleState;

  const [ positionDefault, flipPositionDefault ] =
    Dome.useFlipSettings("ivette.callgraph.panel.position.default", true);
  const { contextMenu: contextMenuStatus,
    show: showStatus } = useStatusFilter();
  const { contextMenu: contextMenuKind,
    show: showKind } = useKindPropertiesFilter();
  const { contextMenu: contextMenuEva,
    show: showEva } = useEvaPropertiesFilter();

  return (
    <DPanel
      position={ positionDefault ? 'right' : 'left'}
      visible={panelVisible}
      label={`${visibleNodes || 0} / ${visibleLinks || 0} ( Nodes / links )`}
      title={`Total ${countNodes} / ${countLinks}`}
      actions={
        <Hbox>
          <IconButton
            icon={positionDefault ? 'ANGLE.LEFT' : 'ANGLE.RIGHT'}
            title={"Change the side of the panel"}
            onClick={flipPositionDefault}
            />
          <IconButton
            icon={'CROSS'}
            size={13}
            title={"Hide panel"}
            onClick={flipPanelVisible}
            />
        </Hbox>
        }
    >
      <>
        <Label
          label={
            countSelected.toString()+" node"+
            (countSelected > 1 ? "s" : "")+" selected"
          }
        />
        { tainted > 0 ?
          <Label
          label='Taint legend:'
          >
            {
              <>
              {renderTaint({ name: "direct_taint", descr: "direct_taint" })}
              {renderTaint({ name: "indirect_taint", descr: "indirect_taint" })}
              </>
            }
          </Label> : <></>
          }
        <Label className='cg-filter-panel'>
          {getPanelFilters({
            contextMenuStatus,
            contextMenuKind,
            contextMenuEva
          })}
        </Label>
        <ListElement>
          { getElementList({
            graph: graphData,
            selectedNodes,
            properties,
            evaProperties,
            nodes: countNodes,
            links: countLinks,
            tainted: tainted > 0,
            style,
            showKind, showStatus, showEva
          }).map((elt) => elt )
          }
        </ListElement>
      </>
    </DPanel>
  );
}
