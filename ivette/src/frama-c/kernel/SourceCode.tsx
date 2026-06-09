/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import Path from 'path';

import * as Dome from 'dome';
import * as System from 'dome/system';
import * as Editor from 'dome/text/editor';
import * as Labels from 'dome/controls/labels';
import * as Settings from 'dome/data/settings';
import * as Buttons from 'dome/controls/buttons';
import * as Dialogs from 'dome/dialogs';
import * as Toolbars from 'dome/frame/toolbars';

import * as Ivette from 'ivette';
import * as Preferences from 'ivette/prefs';

import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';

// -----------------------------------------------------------------------------
//  Utility types and functions
// -----------------------------------------------------------------------------

interface Position { line: number, column: number }

// Recovering the cursor position as a line and a column.
function getCursorPosition(view: Editor.View): Position {
  const pos = view?.state.selection.main;
  if (!view || !pos) return { line: 1, column: 1 };
  const line = view.state.doc.lineAt(pos.from);
  const column = pos.from - line.from;
  return { line: line.number, column };
}

// Function launching the external editor at the currently selected position.
async function openSourceFile(
  cmd: string,
  file: string,
  pos: Position
): Promise<void> {
  if (file === '') return;
  const args = cmd
    .replace('%s', file)
    .replace('%n', pos.line.toString())
    .replace('%c', pos.column.toString())
    .split(' ');
  const prog = args.shift(); if (!prog) return;
  System.spawn(prog, args).catch(() => {
    const filename = Path.basename(file);
    Dialogs.showMessageBox({
      kind: 'error',
      message: `File ${filename} could not be opened with ${prog}`,
      details: (
        `\nThe command "${prog} ${args}" failed.\n\n`
        + 'This command can be changed in the preferences.'
      ),
      buttons: [{ label: "Ok" }],
    });
  });
}

// Toplevel Declaration Markers
function isToplevelDecl(kind: Ast.markerKind): boolean {
  switch (kind) {
    case 'DFUN':
    case 'DECLARATION':
      return true;
    default:
      return false;
  }
}

// -----------------------------------------------------------------------------
//  Selection Events
// -----------------------------------------------------------------------------

type SourceCursor = { file: string, line: number, column: number };
type SourceCallback = ((pos: Position) => void) | null;

const noCursor: SourceCursor = { file: '', line: 0, column: 0 };
const OnClick = Editor.createField<SourceCallback>(null);
const OnControlClick = Editor.createField<SourceCallback>(null);
const OnContextMenu = Editor.createField<SourceCallback>(null);

// This events handler takes care of mouse events
const EventHandlers = createEventHandlers();
function createEventHandlers(): Editor.Extension {
  const deps = {
    onClick: OnClick,
    onControlClick: OnControlClick,
    onContextMenu: OnContextMenu
  };
  return Editor.createEventHandler(deps, {
    // Control Click
    mouseup: ({ onClick, onControlClick }, view, event) => {
      const pos = getCursorPosition(view);
      if (onClick !== null) onClick(pos);
      if (event.ctrlKey && onControlClick !== null) {
        onControlClick(pos);
      }
    },
    // Context Menu
    contextmenu: ({ onContextMenu }, view) => {
      if (onContextMenu !== null) {
        const pos = getCursorPosition(view);
        onContextMenu(pos);
      }
    }
  });
}

// -----------------------------------------------------------------------------
//  Source File Contents
// -----------------------------------------------------------------------------

const Source = Editor.createTextField<string>('', (s: string) => s);

// System request to read the source file.
function useSourceFileContents(file: string | undefined): string {
  const req = React.useMemo(() => {
    if (!file) return Promise.resolve('');
    return System.readFile(file);
  }, [file]);
  const { result } = Dome.usePromise(req);
  return result ?? '';
}

// -----------------------------------------------------------------------------
//  Source Code component
// -----------------------------------------------------------------------------

// Necessary extensions.
const extensions: Editor.Extension[] = [
  Source,
  Editor.GotoLine,
  Editor.Search,
  Editor.ReadOnly,
  Editor.Selection,
  Editor.LineNumbers,
  Editor.LanguageHighlighter,
  Editor.HighlightActiveLine,
  OnClick,
  OnContextMenu,
  OnControlClick,
  EventHandlers,
];

// The component in itself.
export default function SourceCode(): JSX.Element {
  const [fontSize] = Settings.useGlobalSettings(Preferences.EditorFontSize);
  const [command] = Settings.useGlobalSettings(Preferences.EditorCommand);
  const { view, Component } = Editor.Editor(extensions);
  const selectedMarker = States.useSelected();
  const { sloc, kind } = States.useMarker(selectedMarker);
  const [floc, setFloc] = React.useState<Ast.source | undefined>();
  const server = Server.useStatus();
  const isTop = isToplevelDecl(kind);
  React.useEffect(() => {
    if (server !== 'ON') setFloc(undefined);
    else if (sloc !== undefined) setFloc(sloc);
  }, [sloc, server]);
  const file = floc?.file;
  const filename = floc?.base;
  const selectedMarkerLine = floc?.line ?? 0;
  const source = useSourceFileContents(file);
  const [cursor, setCursor] = React.useState<SourceCursor>(noCursor);
  const markerAtCursor = States.useRequestResponse(Ast.getMarkerAt, cursor);
  const { sloc: slocAtCursor } = States.useMarker(markerAtCursor);

  const toggleSearchPanel = React.useCallback(() => {
    if (view) Editor.toggleSearchPanel(view);
  }, [view]);

  const openFile = React.useCallback(() => {
    if (file) openSourceFile(command, file, getCursorPosition(view));
  }, [ command, file, view ]
  );

  const menuPopup = React.useCallback(() => {
    if (file && command)
      Dome.popupMenu([{
        label: 'Open file in external editor',
        onClick: openFile,
      }]);
  }, [file, command, openFile] );

  const onClick = React.useCallback((pos: Position) => {
    if (!file) return;
    const { line, column } = pos;
    setCursor({ file, line, column });
  }, [file]);

  React.useEffect(() => { if (!file) setCursor(noCursor); }, [file]);
  React.useEffect(() => Source.set(view, source), [view, source]);
  React.useEffect(() => OnClick.set(view, onClick), [view, onClick]);
  React.useEffect(() => OnContextMenu.set(view, menuPopup), [view, menuPopup]);
  React.useEffect(() => OnControlClick.set(view, openFile), [view, openFile]);

  React.useEffect(() => {
    if (source.length > 0 && selectedMarkerLine > 0)
      Editor.selectLine(view, selectedMarkerLine, isTop, true);
  }, [view, source, isTop, selectedMarkerLine]);

  React.useEffect(() => {
    if (cursor.file === slocAtCursor?.file
      && cursor.line === slocAtCursor?.line)
      States.setSelected(markerAtCursor);
  }, [cursor, markerAtCursor, slocAtCursor]);

  return (
    <>
      <Ivette.TitleBar help="framac-source-code">
        <Buttons.IconButton
          icon="DUPLICATE"
          visible={!!file}
          onClick={openFile}
          title='Open file in external editor'
        />
        <Labels.Code title={file}>{filename}</Labels.Code>
        <Toolbars.Filler />
        <Buttons.IconButton
          icon="SEARCH"
          enabled={!!file}
          onClick={toggleSearchPanel}
          title='Search in source code'
        />
      </Ivette.TitleBar>
      <Component style={{ fontSize: `${fontSize}px` }} />
    </>
  );
}

// -----------------------------------------------------------------------------
