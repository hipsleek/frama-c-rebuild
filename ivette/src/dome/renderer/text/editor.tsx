/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import { EditorState, StateField, Facet, Extension } from '@codemirror/state';
import { Annotation, Transaction, RangeSet } from '@codemirror/state';
import { EditorSelection, Text, Prec } from '@codemirror/state';

import { EditorView, ViewPlugin, ViewUpdate } from '@codemirror/view';
import { Decoration, DecorationSet } from '@codemirror/view';
import { DOMEventMap as EventMap } from '@codemirror/view';
import { GutterMarker, gutter } from '@codemirror/view';
import { Tooltip, showTooltip } from '@codemirror/view';
import { lineNumbers, keymap } from '@codemirror/view';
import { searchKeymap, search, openSearchPanel, closeSearchPanel,
  searchPanelOpen, gotoLine } from '@codemirror/search';

import { parser } from '@lezer/cpp';
import { tags } from '@lezer/highlight';
import { SyntaxNode } from '@lezer/common';
import * as Language from '@codemirror/language';

import './style.css';

export type { Extension } from '@codemirror/state';
export { GutterMarker } from '@codemirror/view';
export { Decoration } from '@codemirror/view';
export { RangeSet } from '@codemirror/state';



// -----------------------------------------------------------------------------
// Ranges definition and interface
// -----------------------------------------------------------------------------

// Type definition.
export type Range = { from: number, to: number };

// Checks if the start of the range x belongs in y.
export function startInto(x: Range, y: Range): boolean {
  return y.from <= x.from && x.from < y.to;
}

// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
//  CodeMirror state's extensions types definitions
// -----------------------------------------------------------------------------

// Helper types definitions.
export type View = EditorView | null;
export type Set<A> = (view: View, value: A) => void;
export type Get<A> = (state: EditorState | undefined) => A;
export type IsUpdated = (update: ViewUpdate) => boolean;
export interface Struct<S> { structure: S, extension: Extension }
export interface Value<A> { init: A, get: Get<A> }
export interface Data<A, S> extends Value<A>, Struct<S> { isUpdated: IsUpdated }

// Event handlers type definition.
export type Handler<I, E> = (i: I, v: EditorView, e: E) => void;
export type Handlers<I> = { [e in keyof EventMap]?: Handler<I, EventMap[e]> };

// A Field is a data added to the CodeMirror internal state that can be
// modified by the outside world and used by plugins. The typical use case is
// when one needs to inject information from the server into the CodeMirror
// component. A Field exposes a getter and a setter that handles all React's
// hooks shenanigans. It also exposes a StateField, a CodeMirror data structure
// representing the internal state's part responsible of the data. This
// structure is exposed for two reasons. The first one is that it contains the
// extension that must be added to the CodeMirror instantiation. The second one
// is that it is needed during the Aspects creation's process.
export interface Field<A> extends Data<A, StateField<A>> { set: Set<A> }

// An Aspect is a data associated with an editor state and computed by combining
// data from several fields. A typical use case is if one needs a data that
// relies on a server side information (like a synchronized array) which must be
// recomputed each time the selection (which is a field but is also an internal
// information of CodeMirror) is changed. An Aspect exposes a getter that
// handles all React's hooks shenanigans and an extension that must be added to
// the CodeMirror initial configuration.
export type Aspect<A> = Data<A, Facet<A, A>>;

// State extensions and Aspects have to declare their dependencies, i.e. the
// Field and Aspects they rely on to perform their computations. Dependencies
// are declared as a record mapping names to either a Field or an Aspect. This
// is needed to be able to give the dependencies values to the computing
// functions in a typed manner. However, an important point to take into
// consideration is that the extensions constructors defined below cannot
// actually typecheck without relying on type assertions. It means that if you
// declare an extension's dependencies after creating the extension, it will
// crash at execution time. So please, check that every dependency is declared
// before being used.
export type Dict = Record<string, unknown>;
export type Dependency<A> = Field<A> | Aspect<A>;
export type Dependencies<I extends Dict> = { [K in keyof I]: Dependency<I[K]> };

// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
//  Internal types and helpers
// -----------------------------------------------------------------------------

// Type aliases to shorten internal definitions.
type Dep<A> = Dependency<A>;
type Deps<I extends Dict> = Dependencies<I>;
type Combine<Output> = (l: readonly Output[]) => Output;
type Pred<I extends Dict> = (d: Dep<I[typeof k]>, k: string) => boolean;
type Mapper<I extends Dict, A> = (d: Dep<I[typeof k]>, k: string) => A;
type Transform<I extends Dict> = Mapper<I, unknown>;

// Helper function used to map a function over Dependencies.
function mapDeps<I extends Dict, A>(deps: Deps<I>, fn: Mapper<I, A>): A[] {
  return Object.keys(deps).map((k) => fn(deps[k], k));
}

// Helper function used to check if at least one dependency satisfied a
// given predicate.
function existsDeps<I extends Dict>(deps: Deps<I>, fn: Pred<I>): boolean {
  return Object.keys(deps).find((k) => fn(deps[k], k)) !== undefined;
}

// Helper function used to transform a Dependencies will keeping its structure.
function transformDeps<I extends Dict>(deps: Deps<I>, tr: Transform<I>): Dict {
  return Object.fromEntries(Object.keys(deps).map(k => [k, tr(deps[k], k)]));
}

// Helper function retrieving the current values associated to each dependencies
// in a given editor state. They are returned as a Dict instead of the precise
// type because of TypeScript subtyping shenanigans that prevent us to correctly
// type the returned record. Thus, a type assertion has to be used.
function inputs<I extends Dict>(ds: Deps<I>, s: EditorState | undefined): Dict {
  return transformDeps(ds, (d) => d.get(s));
}

// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
//  CodeMirror state's extensions
// -----------------------------------------------------------------------------

// Several extensions constructors are provided. Each one of them encapsulates
// its dependencies if needed. This means that adding an extension to the
// CodeMirror's initial configuration will also add all its dependencies'
// extensions, and thus recursively. However, for now, there is no systemic way
// to check that the fields at the root of the dependencies tree are updated by
// a component. This means you have to verify, by hands, that every field is
// updated when needed by your component. It may be possible to compute a
// function that asks for a value for each fields in the dependencies tree and
// does the update for you, thus forcing you to actually update every fields.
// But it seems hard to define and harder to type.

// A Field is simply declared using an initial value. However, to be able to
// use it, you must add its extension (obtained through <field.extension>) to
// the CodeMirror initial configuration. If determining equality between
// values of the given type cannot be done using (===), an equality test can be
// provided through the optional parameters <equal>. Providing an equality test
// for complex types can help improve performances by avoiding recomputing
// extensions depending on the field.
export function createField<A>(init: A): Field<A> {
  const annot = Annotation.define<A>();
  const create = (): A => init;
  type Update<A> = (current: A, transaction: Transaction) => A;
  const update: Update<A> = (current, tr) => tr.annotation(annot) ?? current;
  const field = StateField.define<A>({ create, update });
  const get: Get<A> = (state) => state?.field(field) ?? init;
  const set: Set<A> = (v, a) => v?.dispatch({ annotations: annot.of(a) });
  const isUpdated: IsUpdated = (update) =>
    update.transactions.find((tr) => tr.annotation(annot)) !== undefined;
  return { init, get, set, structure: field, extension: field, isUpdated };
}

// An Aspect is declared using its dependencies and a function. This function's
// input is a record containing, for each key of the dependencies record, a
// value of the type of the corresponding field. The function's output is a
// value of the aspect's type.
export function createAspect<I extends Dict, O>(
  deps: Dependencies<I>,
  fn: (input: I) => O,
): Aspect<O> {
  const enables = mapDeps(deps, (d) => d.extension);
  const init = fn(transformDeps(deps, (d) => d.init) as I);
  const combine: Combine<O> = (l) => l.length > 0 ? l[l.length - 1] : init;
  const facet = Facet.define<O, O>({ combine, enables });
  const get: Get<O> = (state) => state?.facet(facet) ?? init;
  const convertedDeps = mapDeps(deps, (d) => d.structure);
  const compute: Get<O> = (s) => fn(inputs(deps, s) as I);
  const extension = facet.compute(convertedDeps, compute);
  const isUpdated: IsUpdated = (update) =>
    existsDeps(deps, (d) => d.isUpdated(update));
  return { init, get, structure: facet, extension, isUpdated };
}

// A Decorator is an extension that adds decorations to the CodeMirror's
// document, i.e. it tags subpart of the document with CSS classes. See the
// CodeMirror's documentation on Decoration for further details.
export function createDecorator<I extends Dict>(
  deps: Dependencies<I>,
  fn: (inputs: I, state: EditorState) => DecorationSet
): Extension {
  const enables = mapDeps(deps, (d) => d.extension);
  const get = (s: EditorState): DecorationSet => fn(inputs(deps, s) as I, s);
  class S { s: DecorationSet = RangeSet.empty; }
  class D extends S { update(u: ViewUpdate): void { this.s = get(u.state); } }
  const decorations = (d: D): DecorationSet => d.s;
  return enables.concat(ViewPlugin.fromClass(D, { decorations }));
}

// A Gutter is an extension that adds decorations (bullets or any kind of
// symbol) in a gutter in front of document's lines. See the CodeMirror's
// documentation on GutterMarker for further details.
export function createGutter<I extends Dict>(
  deps: Dependencies<I>,
  className: string,
  line: (inputs: I, block: Range, view: EditorView) => GutterMarker | null
): Extension {
  const enables = mapDeps(deps, (d) => d.extension);
  const extension = gutter({
    class: className,
    lineMarkerChange: (u) => existsDeps(deps, (d) => d.isUpdated(u)),
    lineMarker: (view, block) => {
      return line(inputs(deps, view.state) as I, block, view);
    }
  });
  return enables.concat(extension);
}

// A Tooltip is an extension that adds decorations as a floating DOM element
// above or below some text. See the CodeMirror's documentation on Tooltip
// for further details.
export function createTooltip<I extends Dict>(
  deps: Dependencies<I>,
  fn: (input: I) => Tooltip | Tooltip[] | undefined,
): Extension {
  const { structure, extension } = createAspect(deps, fn);
  const show = showTooltip.computeN([structure], st => {
    const tip = st.facet(structure);
    if (tip === undefined) return [null];
    if ('length' in tip) return tip;
    return [tip];
  });
  return [extension, show];
}

// An Event Handler is an extension responsible of performing a computation each
// time a DOM event (like <mouseup> or <contextmenu>) happens.
export function createEventHandler<I extends Dict>(
  deps: Dependencies<I>,
  handlers: Handlers<I>,
): Extension {
  const enables = mapDeps(deps, (d) => d.extension);
  const domEventHandlers = Object.fromEntries(Object.keys(handlers).map((k) => {
    const h = handlers[k] as Handler<I, typeof k>;
    const fn = (e: typeof k, v: EditorView): void =>
      h(inputs(deps, v.state) as I, v, e);
    return [k, fn];
  }));
  return enables.concat(EditorView.domEventHandlers(domEventHandlers));
}

// A View updater is an extension that allows to modify the view each time a
// dependency is updated. For example, one could use this to change the cursor
// position when a Data is updated by the outside world.
export function createViewUpdater<I extends Dict>(
  deps: Dependencies<I>,
  fn: (input: I, view: View) => void,
): Extension {
  const enables = mapDeps(deps, (d) => d.extension);
  const listener = EditorView.updateListener.of((u) => {
    if(!existsDeps(deps, (d) =>  d.isUpdated(u))) return;
    const get = (b: boolean): EditorState => b ? u.state : u.startState;
    const state: <X>(d: Dep<X>) => EditorState = (d) => get(d.isUpdated(u));
    const inputs = transformDeps(deps, (d) => d.get(state(d))) as I;
    fn(inputs, u.view);
  });
  return enables.concat(listener);
}

export type Priority = 'lowest' | 'low' | 'default' | 'high' | 'highest';
export function setPriority(extension: Extension, p: Priority): Extension {
  switch(p) {
    case 'lowest': return Prec.lowest(extension);
    case 'low': return Prec.low(extension);
    case 'default': return Prec.default(extension);
    case 'high': return Prec.high(extension);
    case 'highest': return Prec.highest(extension);
  }
}

// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
//  Code highlighting and parsing
// -----------------------------------------------------------------------------

// Plugin specifying how to highlight the code. The theme is handled by the CSS.
const Highlight = Language.syntaxHighlighting(Language.HighlightStyle.define([
  { tag: tags.comment, class: 'cm-comment' },
  { tag: tags.typeName, class: 'cm-type' },
  { tag: tags.number, class: 'cm-number' },
  { tag: tags.controlKeyword, class: 'cm-keyword' },
  { tag: tags.definition(tags.variableName), class: 'cm-def' },
]));

// A language provider based on the [Lezer C++ parser], extended with
// highlighting and folding information. Only comments can be folded.
// (Source: https://github.com/lezer-parser/cpp)
const comment = (t: SyntaxNode): Range => ({ from: t.from + 2, to: t.to - 2 });
const folder = Language.foldNodeProp.add({ BlockComment: comment });
const stringPrefixes = [ "L", "u", "U", "u8", "LR", "UR", "uR", "u8R", "R" ];
const cppLanguage = Language.LRLanguage.define({
  parser: parser.configure({ props: [ folder ] }),
  languageData: {
    commentTokens: { line: "//", block: { open: "/*", close: "*/" } },
    indentOnInput: /^\s*(?:case |default:|\{|\})$/,
    closeBrackets: { stringPrefixes },
  }
});

// This extension enables all the language highlighting features.
export const LanguageHighlighter: Extension =
  [Highlight, new Language.LanguageSupport(cppLanguage)];

// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
//  Standard extensions and commands
// -----------------------------------------------------------------------------

export const ReadOnly = EditorState.readOnly.of(true);

export const toggleSearchPanel = (view: EditorView) : void => {
  if (searchPanelOpen(view.state))
    closeSearchPanel(view);
  else
    openSearchPanel(view);
};

const SearchAlternativeKey = [{ key: 'Alt-f', run: openSearchPanel }];
const SearchKeymap = searchKeymap.slice(1).concat(SearchAlternativeKey);
export const Search : Extension = [ search(), keymap.of(SearchKeymap) ];

const GotoKeymap = [{ key: 'Alt-g', run: gotoLine }];
export const GotoLine : Extension = [ search(), keymap.of(GotoKeymap) ];

export const Selection = createSelectionField();
function createSelectionField(): Field<EditorSelection> {
  const cursor = EditorSelection.cursor(0);
  const field = createField<EditorSelection>(EditorSelection.create([cursor]));
  const set: Set<EditorSelection> = (view, selection) => {
    view?.dispatch({ selection });
    field.set(view, selection);
  };
  const updater = EditorView.updateListener.of((update) => {
    if (update.selectionSet) field.set(update.view, update.state.selection);
  });
  return { ...field, set, extension: [field.extension, updater] };
}

export type RangeCallback = (offset: number, endOffset: number) => void;
export const OnSelection = createOnSelectionField();
function createOnSelectionField(): Field<RangeCallback|null> {
  const field = createField<RangeCallback|null>(null);
  const set: Set<RangeCallback|null> = (view, fn) => {
    field.set(view, fn);
  };
  const updater = EditorView.updateListener.of((update) => {
    if (update.selectionSet) {
      const view = update.view;
      const fn = field.get(view.state);
      if (fn !== null) {
        const { from: offset, to: endOffset } = view.state.selection.main;
        fn(offset, endOffset);
      }
    }
  });
  return { ...field, set, extension: [field.extension, updater] };
}

export const Document = createDocumentField();
function createDocumentField(): Field<Text> {
  const field = createField<Text>(Text.empty);
  const set: Set<Text> = (view, text) => {
    const selection = { anchor: 0 };
    const length = view?.state.doc.length;
    const changes = { from: 0, to: length, insert: text };
    view?.dispatch({ changes, selection });
    field.set(view, text);
  };
  const updater = EditorView.updateListener.of((update) => {
    if (update.docChanged) field.set(update.view, update.state.doc);
  });
  return { ...field, set, extension: [field.extension, updater] };
}

// Create a text field that updates the CodeMirror document when set.
export type ToString<A> = (text: A) => string;
export function createTextField<A>(init: A, toString: ToString<A>): Field<A> {
  const field = createField<A>(init);
  const set: Set<A> = (view, text) => {
    const selection = { anchor: 0 };
    const length = view?.state.doc.length;
    const changes = { from: 0, to: length, insert: toString(text) };
    view?.dispatch({ changes, selection });
    field.set(view, text);
  };
  return { ...field, set };
}

// An extension displaying line numbers in a gutter. Does not display anything
// if the document is empty.
export const LineNumbers = createLineNumbers();
function createLineNumbers(): Extension {
  return lineNumbers({
    formatNumber: (lineNo, state) => {
      if (state.doc.length === 0) return '';
      return lineNo.toString();
    }
  });
}

// An extension highlighting the active line.
export const HighlightActiveLine = createHighlightActiveLine();
function createHighlightActiveLine(): Extension {
  const highlight = Decoration.line({ class: 'cm-active-line' });
  return createDecorator({}, (_, state) => {
    if (state.doc.length === 0) return RangeSet.empty;
    const { from } = state.doc.lineAt(state.selection.main.from);
    const deco = highlight.range(from, from);
    return RangeSet.of(deco);
  });
}

// An extension handling the folding of foldable nodes. For example, If used
// with the language highlighter defined above, it will provides interactions
// to fold comments only.
export const FoldGutter = createFoldGutter();
function createFoldGutter(): Extension {
  return Language.foldGutter();
}

// Folds all the foldable nodes of the given view.
export function foldAll(view: View): void {
  if (view !== null) Language.foldAll(view);
}

// Unfolds all the foldable nodes of the given view.
export function unfoldAll(view: View): void {
  if (view !== null) Language.unfoldAll(view);
}

function isVisible(view: View, line: number): boolean {
  if (!view || view.state.doc.lines < line) return false;
  const doc = view.state.doc;
  const top = view.documentTop;
  const rect = view.dom.getBoundingClientRect();
  const topVisibleBlock = view.lineBlockAtHeight(rect.top - top);
  const topVisibleLine = doc.lineAt(topVisibleBlock.to).number;
  const bottomVisibleBlock = view.lineBlockAtHeight(rect.bottom - top);
  const bottomVisibleLine = doc.lineAt(bottomVisibleBlock.from).number;
  return (topVisibleLine < line && line < bottomVisibleLine);
}

// Move to the given line. The indexation starts at 1.
export function selectLine(
  view: View,
  line: number,
  atTop: boolean,
  focus = true,
): void {
  if (!view || view.state.doc.lines < line) return;
  const doc = view.state.doc;
  const { from: anchor } = doc.line(Math.max(line, 1));
  if (focus) view.dispatch({ selection: { anchor } });
  if (isVisible(view, line)) return;
  const verticalScroll = atTop ? 'start' : 'center';
  const effects = EditorView.scrollIntoView(anchor, { y: verticalScroll });
  view.dispatch({ effects });
}

// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
//  Editor component
// -----------------------------------------------------------------------------

export interface EditorComponentProps { style?: React.CSSProperties }
export type EditorComponent = (props: EditorComponentProps) => JSX.Element;
export interface Editor { view: View; Component: EditorComponent }

export function Editor(extensions: Extension[]): Editor {
  const parent = React.useRef(null);
  const editor = React.useRef<View>(null);
  const Component: EditorComponent = React.useCallback((props) => {
    return <div className='cm-global-box' style={props.style} ref={parent} />;
  }, [parent]);
  React.useEffect(() => {
    if (!parent.current) return;
    const state = EditorState.create({ extensions });
    const eview = new EditorView({ state, parent: parent.current });
    editor.current = eview;
    return () => {
      eview.destroy();
      editor.current = null;
    };
  }, [parent, extensions]);
  return { view: editor.current, Component };
}

// -----------------------------------------------------------------------------
