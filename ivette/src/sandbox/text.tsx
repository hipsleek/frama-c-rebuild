/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Sandbox Testing of RichText                                        --- */
/* --- Only appears in DEVEL mode.                                        --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import * as Dome from 'dome';
import { ToolBar, Filler } from 'dome/frame/toolbars';
import { Code } from 'dome/controls/labels';
import { Button, IconButton } from 'dome/controls/buttons';
import {
  TextView,
  TextProxy,
  TextBuffer,
  Position,
  emptySelection,
  Decoration,
} from 'dome/text/richtext';
import { registerSandbox } from 'ivette';

/* -------------------------------------------------------------------------- */
/* --- Use Text                                                           --- */
/* -------------------------------------------------------------------------- */

function UseText(): JSX.Element {
  const [useLines, flipUseLines] = Dome.useFlipState(true);
  const [useCurrent, flipUseCurrent] = Dome.useFlipState(true);
  const [readOnly, flipReadOnly] = Dome.useFlipState(false);
  const [useProxy, flipUseProxy] = Dome.useFlipState(false);
  const [changed, setChanged] = React.useState(false);
  const [changes, setChanges] = React.useState(0);
  const [length, setLength] = React.useState(0);
  const [lines, setLines] = React.useState(1);
  const [s, onSelection] = React.useState(emptySelection);
  const [v, onViewport] = React.useState(emptySelection);
  const [h, onHover] = React.useState<Position | null>(null);
  const [evt, setEvent] = React.useState('');
  const proxy = React.useMemo(() => new TextProxy(), []);
  const buffer = React.useMemo(() => new TextBuffer(), []);
  const text = useProxy ? proxy : buffer;
  const updateProxy = React.useCallback(
    () => {
      const { length, toLine } = text.range();
      setChanged(true);
      setChanges((n) => 1+n);
      setLength(length);
      setLines(toLine);
    }, [text]);
  const push = React.useCallback(() => {
    const n = Math.random();
    text.append(`ADDED${n}\n`);
  }, [text]);
  const onChange = Dome.useDebounced(updateProxy, 200);
  const [decorations, setDecorations] = React.useState<Decoration[]>([]);
  const inconsistent = decorations.length > 0 && changed;

  const clearDecorations = React.useCallback(() => {
    setChanged(false);
    setDecorations([]);
  }, []);

  const clearText = React.useCallback(() => {
    setChanged(false);
    setDecorations([]);
    text.clear();
  }, [text]);

  const addDecoration = React.useCallback(() => {
    setChanged(false);
    setDecorations([...decorations, {
      offset: s.offset,
      length: s.length,
      className: 'decoration',
      title: 'Decorated'
    }]);
  }, [decorations, s]);

  const addLineDecoration = React.useCallback(() => {
    setChanged(false);
    setDecorations([...decorations, {
      line: s.fromLine,
      className: 'line-decoration',
      title: 'Line Decorated',
    }]);
  }, [decorations, s]);

  const addGutterDecoration = React.useCallback(() => {
    setChanged(false);
    setDecorations([...decorations, {
      line: s.fromLine,
      gutter: '*',
      title: 'Gutter Mark',
    }]);
  }, [decorations, s]);

  const isLine = s.fromLine === s.toLine && s.toLine <= lines;
  const isRange = s.length > 0 && s.offset + s.length <= length;
  const allDecorations = React.useMemo(() => {
    if (h===null) return decorations;
    return [...decorations, { line: h.line, className: 'hover' }];
  }, [decorations, h]);

  const clearEvent = React.useCallback(() => setEvent(''), []);
  const triggerCancelEvent = Dome.useDebounced(clearEvent, 1200);

  const onClick = React.useCallback(
    (pos: Position | null, evt: MouseEvent) => {
      const name =
        evt.altKey ? 'Alt-Click' :
        evt.ctrlKey ? 'Ctrl-Click' :
        evt.metaKey ? 'Meta-Click' :
        'Click';
      setEvent(`${name} ${pos ? pos.offset : 'null'}`);
      triggerCancelEvent();
    }, [triggerCancelEvent]);

  const onPopup = React.useCallback(
    (pos: Position | null) => {
      setEvent(`Popup ${pos ? pos.offset : 'null'}`);
      triggerCancelEvent();
    }, [triggerCancelEvent]);

  const onDoubleClick = React.useCallback(
    (pos: Position | null) => {
      setEvent(`Double-Click ${pos ? pos.offset : 'null'}`);
      triggerCancelEvent();
    }, [triggerCancelEvent]);

  const onGutter = React.useCallback(
    (pos: Position | null) => {
      setEvent(`Gutter-Click L${pos ? pos.line : 'null'}`);
      triggerCancelEvent();
    }, [triggerCancelEvent]);

  return (
    <>
      <ToolBar>
        <Button
          icon="ITEMS.LIST"
          selected={useLines}
          title={'Line Numbers'}
          onClick={flipUseLines}
        />
        <Button
          icon="TERMINAL"
          selected={useCurrent}
          title={'Show Current Line'}
          onClick={flipUseCurrent}
        />
        <Button
          icon={readOnly ? 'LOCK' : 'EDIT'}
          title={readOnly ? 'Read Only' : 'Editable'}
          onClick={flipReadOnly}
        />
        <Button
          icon={useProxy ? 'DISPLAY' : 'SAVE'}
          title={useProxy ? 'Use TextProxy' : 'Use TextBuffer (persistent)'}
          onClick={flipUseProxy}
        />
        <Filler/>
        <Code
          icon={inconsistent ? 'WARNING' : undefined}
          title={inconsistent ? 'Iconsistent (modified text)' : undefined}
          label={`Decorations: ${decorations.length}`}
        />
        <IconButton
          enabled={isLine}
          icon="CIRC.INFO"
          title="Add Gutter Decoration"
          onClick={addGutterDecoration}
        />
        <IconButton
          enabled={isLine}
          icon="CIRC.CHECK"
          title="Add Line Decoration"
          onClick={addLineDecoration}
        />
        <IconButton
          enabled={isRange}
          icon="CIRC.PLUS"
          title="Add Decoration"
          onClick={addDecoration}
        />
        <IconButton
          enabled={decorations.length > 0}
          kind={inconsistent ? 'negative' : 'default'}
          icon="CIRC.CLOSE"
          title="Clear Decorations"
          onClick={clearDecorations} />
        <Button label="Push" onClick={push} />
        <Button label="Clear" kind='negative' onClick={clearText}  />
      </ToolBar>
      <TextView
        text={text}
        readOnly={readOnly}
        onChange={onChange}
        onSelection={onSelection}
        onHover={onHover}
        onClick={onClick}
        onPopup={onPopup}
        onGutter={onGutter}
        onDoubleClick={onDoubleClick}
        onViewport={onViewport}
        decorations={allDecorations}
        lineNumbers={useLines}
        showCurrentLine={useCurrent}
      />
      <ToolBar>
        <Code label={`Length ${length}`} />
        <Code label={`Lines ${lines}`} />
        <Code label={`Offset ${s.offset}-${s.offset + s.length}`} />
        <Code label={`Line ${s.fromLine}-${s.toLine}`} />
        <Code label={`View ${v.fromLine}-${v.toLine}`} />
        <Code label={`Hover ${h ? h.offset : '-'}:${h ? h.line : '-'}`} />
        <Filler />
        <Code>{evt}</Code>
        <Code>{`Changes: ${changes}`}</Code>
      </ToolBar>
    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.richtext',
  label: 'Rich Text',
  children: <UseText />,
});

/* -------------------------------------------------------------------------- */
