/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { Edge, Node } from 'dome/graph/graph';
import * as EvaStats from 'frama-c/plugins/eva/api/stats';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';
import {
  NodeObject as NodeObject3D,
  LinkObject as LinkObject3D,
} from 'react-force-graph-3d';

import { IThreeStateButton } from "./components/buttons";

export type ModeDisplay = "all" | "linked" | "selected"
export type SelectedNodes = Set<string>
export type SetSelectedNodes = (s: SelectedNodes | string[]) => void
export type SelectedNodesState = [SelectedNodes, SetSelectedNodes]

export function useSelectedNodes(): SelectedNodesState {
  const [selected, setSelected] = React.useState<SelectedNodes>(new Set());
  const update = React.useCallback((newSet: SelectedNodes | string[])
  : void => { setSelected(new Set(newSet)); }, []);

  return [selected, update];
}

export interface CGNode extends Node {
  /** Coverage of the Eva analysis */
  coverage?: { reachable: number, dead: number };
  /** Alarms raised by the Eva analysis by category */
  alarmCount?: EvaStats.alarmEntry[];
  /** Alarms statuses emitted by the Eva analysis */
  alarmStatuses?: EvaStats.statusesEntry;
  /** Taint status */
  taintStatus?: States.Tag[];
  /** is Recursive function */
  isRecursive?: boolean;
}

export interface CGLink extends Edge {}

export interface CGData {
  nodes: CGNode[];
  links: CGLink[];
}

type nodeType = "parents" | "children";

export function getSourceId(link: LinkObject3D<CGNode, CGLink>): string {
  return typeof link.source === 'string' ?
    link.source : (link.source as NodeObject3D<CGNode>).id;
}

export function getTargetId(link: LinkObject3D<CGNode, CGLink>): string {
  return typeof link.target === 'string' ?
    link.target : (link.target as NodeObject3D<CGNode>).id;
}

export function getIDFromLink(link: LinkObject3D<CGNode, CGLink>)
: {sourceId: string, targetId: string} {
  return { sourceId: getSourceId(link), targetId: getTargetId(link) };
}

export function getNodeVisibility(
  links: CGLink[],
  mode: ModeDisplay,
  id: string,
  successor: string[],
  predecessors: string[]
): boolean {
  switch(mode) {
    case "linked": return links.some((elt: CGLink) => (
        id === getSourceId(elt) || id === getTargetId(elt)));
    case "selected":
      return successor.includes(id) || predecessors.includes(id);
    case "all": return true;
  }
}

function removeCycle(toTraited: string[], ids: string[]): string[] {
  const ret: string[] = [];
  for (const elt of toTraited) {
    if(!ids.includes(elt)) ret.push(elt);
  }
  return ret;
}

function getNextNodes(links: CGLink[], type: nodeType, ids: string[])
: string[] {
  const ret: string[] = [];
  for (const elt of links) {
    const { sourceId, targetId } = getIDFromLink(elt);

    if(type === "children" && ids.includes(sourceId))
        ret.push(targetId);
    else if (type === "parents" && ids.includes(targetId))
        ret.push(sourceId);
  }
  return ret;
}

function getNodes(
  links: CGLink[],
  selectedNodes: SelectedNodes,
  type: nodeType,
  depth?: number
): string[] {
  let ids: string[] = Array.from(selectedNodes);
  if (depth === 0) return ids;
  let nodes = ids;
  let i = 0;
  do {
    const news = getNextNodes(links, type, nodes)
      .map((elt) => nodes.includes(elt) ? "" : elt);
    nodes = removeCycle(news, ids);
    ids = ids.concat(news);
    i++;
  } while(nodes.length > 0 && (depth === undefined || i < depth));

  return ids;
}

function getDepth(v: IThreeStateButton): number | undefined {
  return v.active ? (v.max ? undefined : (v.value ? v.value : 0)) : 0;
}

export function getSuccessor(
  links: CGLink[],
  selectedNodes: SelectedNodes,
  selectedChildren: IThreeStateButton
): string[] {
  return getNodes(
    links, selectedNodes, "children", getDepth(selectedChildren)
  );
}
export function getPredecessors(
  links: CGLink[],
  selectedNodes: SelectedNodes,
  selectedParents: IThreeStateButton
): string[] {
   return getNodes( links, selectedNodes, "parents", getDepth(selectedParents));
}

export function onNodeClickMultiSelect(
  selectedNodesState: [SelectedNodes, SetSelectedNodes],
  id: string,
  event: MouseEvent | React.MouseEvent
): void {
  const [selectedNodes, SetSelectedNodes] = selectedNodesState;

  const s = new Set(selectedNodes);
  if (event.ctrlKey) { // multi-selection
    s.has(id) ? s.delete(id) : s.add(id);
  } else if (event.altKey) {
    States.setCurrentScope(id as Ast.decl);
    return;
  } else { // single-selection
    s.clear();
    s.add(id);
  }
  SetSelectedNodes(s);
}

/** Links */
export function getLinkColor(
  node: LinkObject3D<CGNode, CGLink>,
  selectedNodes: SelectedNodes,
  style: CSSStyleDeclaration
): string {
  const { sourceId, targetId } = getIDFromLink(node);
  let color = "grey";
  const isDst = selectedNodes.has(targetId);
  const isSrc = selectedNodes.has(sourceId);

  if(isDst && isSrc)
    color = style.getPropertyValue('--graph-ed-color-green');
  else if(isDst)
    color = style.getPropertyValue('--graph-ed-color-red');
  else if(isSrc)
    color = style.getPropertyValue('--graph-ed-color-blue');
  return color;
}

export function getLinkVisibility(
  node: LinkObject3D<CGNode, CGLink>,
  displayMode: ModeDisplay,
  predecessors: string[],
  successor: string[],
): boolean {
  const { sourceId, targetId } = getIDFromLink(node);
  switch(displayMode) {
    case "selected":
      return Boolean(
        (successor.includes(sourceId) || predecessors.includes(sourceId)) &&
        (successor.includes(targetId) || predecessors.includes(targetId))
      );
    case "linked":
    case "all":
    default: return true;
  }
}

export function getLinkWidth(
  node: LinkObject3D<CGNode, CGLink>,
  selectedNodes: SelectedNodes,
  linkThickness: number
): number {
  const { sourceId, targetId } = getIDFromLink(node);
  return (selectedNodes.has(sourceId) || selectedNodes.has(targetId)) ?
    (linkThickness + 1):
    linkThickness;
}
