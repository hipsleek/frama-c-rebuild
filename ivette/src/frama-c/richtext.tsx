/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Frama-C Utilities
// --------------------------------------------------------------------------

/**
 * @packageDocumentation
 * @module frama-c/richtext
 */

import React from 'react';
import * as Dome from 'dome';
import * as States from 'frama-c/states';
import * as KernelData from 'frama-c/kernel/api/data';
import * as Ast from 'frama-c/kernel/api/ast';
import { getMarkerMenuItems } from './kernel/ASTview';
import { classes } from 'dome/misc/utils';

// --------------------------------------------------------------------------
// --- Kernel Text Utilities
// --------------------------------------------------------------------------

/** Unstructured text contents */
export function textToString(text: KernelData.text): string {
  if (text===null) return '';
  if (typeof(text)==='string') return text;
  // documented to be faster than map & join
  let buffer='';
  // skip tag mark
  for(let k=1; k<text.length; k++)
    buffer += textToString(text[k]);
  return buffer;
}

// --------------------------------------------------------------------------
// --- Text Tag Tree
// --------------------------------------------------------------------------

/** Tag Tree */
export type Tags = readonly Tag[];
export type Tag = {
  tag: string,
  offset: number,
  endOffset: number,
  children: Tags
};

export type TagIndex = Map<string, Tags>;

export function contains(a: Tag, b: Tag): boolean {
  return a.offset <= b.offset && b.endOffset <= a.endOffset;
}

/** Extract a Tag forest from a text. */
export function textToTags(
  text: KernelData.text,
  filter?: (tag: Tag) => boolean,
): {
    index: TagIndex,
    tags: Tags
} {
  const walk =
    (buffer: Tag[], offset: number, text: KernelData.text): number => {
      if (text===null) return offset;
      if (typeof(text)==='string') return offset + text.length;
      let endOffset = offset;
      const t0 = text[0];
      if (t0 && typeof(t0)==='string') {
        const tag = typeof(t0)==='string' ? t0 : '';
        const children: Tag[] = [];
        for(let k=1; k<text.length; k++)
          endOffset = walk(children, endOffset, text[k]);
        const tg = { tag, offset, endOffset, children };
        if (!filter || filter(tg)) {
          const tgs = index.get(tag);
          if (tgs===undefined) index.set(tag, [tg]); else tgs.push(tg);
          buffer.push(tg);
        } else {
          children.forEach(t => buffer.push(t));
        }
      } else {
        for(let k=1; k<text.length; k++)
          endOffset = walk(buffer, endOffset, text[k]);
      }
      return endOffset;
    };
  const index = new Map<string, Tag[]>();
  const tags : Tag[] = [];
  walk(tags, 0, text);
  return { index, tags };
}

// --------------------------------------------------------------------------
// --- Text Tag Index
// --------------------------------------------------------------------------

/** Lookup for the top-most tag containing the offset */
export function findChildTag(tags: Tags, offset: number) : Tag | undefined
{
  let p = 0;
  let q = tags.length - 1;
  if (q < p) return undefined;
  let a = tags[p];
  let b = tags[q];
  if (offset < a.offset) return undefined;
  if (offset > b.endOffset) return undefined;
  if (offset <= a.endOffset) return a;
  if (b.offset <= offset) return b;
  // @invariant Range:  0 <= p <= q < tags.length;
  // @invariant Tags:   a = tags[p] /\ b = tags[q];
  // @invariant Offset: a.endOffset < offset < b.offset;
  // @variant q-p;
  for(;;) {
    const d = q-p;
    if (d <= 1) return undefined;
    const r = Math.floor(p + d / 2);
    const c = tags[r];
    if (offset < c.offset) { b = c; q = r; continue; }
    if (c.endOffset < offset) { a = c; p = r; continue; }
    return c;
  }
}

/** Lookup for the deepest tag containing the offset and satisfying the
   filtering condition. */
export function findTag(
  tags: Tags,
  offset: number,
  filter?: (tag: Tag) => boolean,
): Tag | undefined {
  type Result = Tag | undefined;
  const lookup = (tags: Tags, res: Result): Result => {
    const r = findChildTag(tags, offset);
    if (r && (!filter || filter(r)))
      return lookup(r.children, r);
    else
      return res;
  };
  return lookup(tags, undefined);
}

// --------------------------------------------------------------------------
// --- Lightweight Text Renderer
// --------------------------------------------------------------------------

export type Modifier = 'NORMAL' | 'DOUBLE' | 'META';

export interface MarkerProps {
  marker: string;
  onSelected?: (marker: string, meta: Modifier) => void;
  onHovered?: (marker: string | undefined) => void;
  onContextMenu?: (marker: string) => void;
  children?: React.ReactNode;
}

export function Marker(props: MarkerProps): JSX.Element {
  const { marker, onSelected, onHovered, onContextMenu, children } = props;
  const className = classes(
    (onSelected || onHovered) && 'kernel-text-marker'
  );
  const onDoubleClick = (): void => {
    onSelected && onSelected(marker, 'DOUBLE');
  };
  const onClick = (evt: React.MouseEvent): void => {
    evt.stopPropagation();
    onSelected && onSelected(marker, evt.altKey ? 'META' : 'NORMAL');
  };
  const onRightClick = (evt: React.MouseEvent): void => {
    evt.stopPropagation();
    onContextMenu && onContextMenu(marker);
  };
  return (
    <span
      className={className}
      onClick={onClick}
      onDoubleClick={onDoubleClick}
      onContextMenu={onRightClick}
      onMouseEnter={() => onHovered && onHovered(marker)}
      onMouseLeave={() => onHovered && onHovered(undefined)}
    >
      {children}
    </span>
  );
}

export interface TextProps {
  text: KernelData.text;
  onSelected?: (marker: string, meta: Modifier) => void;
  onHovered?: (marker: string | undefined) => void;
  onContextMenu?: (marker: string) => void;
  className?: string;
}

export function Text(props: TextProps): JSX.Element {
  const className = classes('kernel-text', 'dome-text-code', props.className);
  const makeContents = (text: KernelData.text): React.ReactNode => {
    if (Array.isArray(text)) {
      const tag = text[0];
      const marker = tag && typeof (tag) === 'string';
      const array = marker ? text.slice(1) : text;
      const contents = React.Children.toArray(array.map(makeContents));
      if (marker)
        return (
          <Marker
            marker={tag}
            onSelected={props.onSelected}
            onHovered={props.onHovered}
            onContextMenu={props.onContextMenu}
          >
            {contents}
          </Marker>
        );
      return <>{contents}</>;
    }
    return text;
  };
  return <div className={className}>{makeContents(props.text)}</div>;
}

// --------------------------------------------------------------------------
// --- Text for AST markers
// --------------------------------------------------------------------------

export function selectMarker(marker: Ast.marker, modifier: Modifier): void {
  switch (modifier) {
    case 'NORMAL':
      States.setMarked(marker);
      break;
    case 'META':
      States.setMarked(marker, true);
      break;
    case 'DOUBLE':
      States.setSelected(marker);
      break;
  }
}

export interface MarkerTextProps {
  text: KernelData.text;
  onSelected?: (marker: Ast.marker, meta: Modifier) => void;
  onHovered?: (marker: Ast.marker | undefined) => void;
  className?: string;
}

/** Text with AST markers. Markers are automatically hovered on mouseover
    and selected on click. This behavior can be changed by providing
    callbacks onSelected and onHovered. */
export function MarkerText(props: MarkerTextProps): JSX.Element {
  const onSelected = (m: string, modifier: Modifier): void => {
    const marker = Ast.jMarker(m);
    if (props.onSelected)
      props.onSelected(marker, modifier);
    else
      selectMarker(marker, modifier);
  };
  const onHovered = (m: string | undefined): void => {
    const marker = m === undefined ? m : Ast.jMarker(m);
    if (props.onHovered)
      props.onHovered(marker);
    else
      States.setHovered(marker);
  };
  const onContextMenu = (m: string): void => {
    const marker = Ast.jMarker(m);
    const items: Dome.PopupMenuItem[] = getMarkerMenuItems(marker);
    Dome.popupMenu(items);
  };
  return  <Text {...props} onSelected={onSelected} onHovered={onHovered}
                           onContextMenu={onContextMenu} />;
}


// --------------------------------------------------------------------------
