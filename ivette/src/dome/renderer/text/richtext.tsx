/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import _ from 'lodash';
import React, { CSSProperties } from 'react';
import { classes } from 'dome/misc/utils';
import * as CS from '@codemirror/state';
import * as CM from '@codemirror/view';
import { Change, diffLines } from 'diff';
import { Button } from 'dome/controls/buttons';

/* -------------------------------------------------------------------------- */
/* --- Basic Definitions                                                  --- */
/* -------------------------------------------------------------------------- */

export interface Offset { offset: number; }
export interface Range extends Offset { length: number }
export interface Position extends Offset { line: number }
export interface Selection extends Range { fromLine: number, toLine: number }

export const emptyPosition : Position = { offset: 0, line: 1 };
export const emptyRange : Range = { offset: 0, length: 0 };
export const emptySelection : Selection =
  { ...emptyRange, fromLine: 1, toLine: 1 };

export function byDepth(a : Range, b : Range): number
{
  return (a.length - b.length) || (b.offset - a.offset);
}

export function byOffset(a : Offset, b : Offset): number
{
  return (a.offset - b.offset);
}

export type View = CM.EditorView | null;

const clipRange = (a: number, value: number, b: number): number => {
  if (value < a) return a;
  if (value > b) return b;
  return value;
};

/* -------------------------------------------------------------------------- */
/* --- Text View Updates                                                  --- */
/* -------------------------------------------------------------------------- */

function appendContents(view: CM.EditorView, text: string): void {
  const length = view.state.doc.length;
  view.dispatch({ changes: { from: length, insert: text } });
}

function dispatchContents(view: CM.EditorView, text: string | CS.Text): void {
  const length = view.state.doc.length;
  view.dispatch({ changes: { from: 0, to: length, insert: text } });
}

function dispatchReplace(view: CM.EditorView, rg: Range, text: string): void {
  const docLength = view.state.doc.length;
  const { offset, length } = rg;
  const endOffset = offset + length;
  if (0 <= offset && offset <= endOffset && endOffset <= docLength)
    view.dispatch({ changes: { from: offset, to: endOffset, insert: text } });
}

function dispatchScroll(view: CM.EditorView, rg: Range): void {
  const length = view.state.doc.length;
  const anchor = clipRange(0, rg.offset, length);
  const head = clipRange(anchor, anchor + rg.length, length);
  view.dispatch({ scrollIntoView: true, selection: { anchor, head } });
}

class DiffBuffer {
  private readonly changes : CS.ChangeSpec[] = [];
  private offset = 0;
  private added = '';
  private removed = 0;

  constructor() { this.add = this.add.bind(this); }

  private commit(forward=0): void {
    const { changes, offset, added, removed } = this;
    if (added || removed) {
      const nextOffset = offset + removed;
      changes.push({ from: offset, to: nextOffset, insert: added });
      this.offset = nextOffset + forward;
      this.added = '';
      this.removed = 0;
    } else
      if (forward) this.offset += forward;
  }

  add(c : Change): void {
    if (c.added) this.added += c.value;
    if (c.removed) this.removed += c.value.length;
    if (!c.added && !c.removed) this.commit(c.value.length);
  }

  flush(): CS.ChangeSpec {
    this.commit();
    return this.changes;
  }

}

function updateContents(view: CM.EditorView, newText: string): void {
  const buffer = new DiffBuffer();
  diffLines(view.state.doc.toString(), newText).forEach(buffer.add);
  const changes = buffer.flush();
  view.dispatch({ changes });
}

/* -------------------------------------------------------------------------- */
/* --- Text Proxy                                                         --- */
/* -------------------------------------------------------------------------- */

/** Text proxy to a RichText component.

   This class can be used as a proxy to the content of a {RichText} component,
   provided such a component has been associated with the proxy.

   Methods of the class are no-ops when there is no associated view, and at most
   one component shall be associated with a given Text buffer at the same time.

   <b>Warning:</n> do not access proxy's methods during React component
   rendering since they would not be synchronized with further changes from
   document or editor view. Rather, those methods shall be invoked from
   React and event callbacks.

   All methods are bound to `this`.  */
export class TextProxy {

  // --- Private part

  protected scrolled : Range | null = null;
  protected proxy : View = null;

  constructor() {
    this.range = this.range.bind(this);
    this.clear = this.clear.bind(this);
    this.append = this.append.bind(this);
    this.toString = this.toString.bind(this);
    this.setContents = this.setContents.bind(this);
    this.connect = this.connect.bind(this);
  }

  /** @ignore */
  connect(newView: View): void {
    this.proxy = newView;
    const range = this.scrolled;
    if (range) {
      if (newView) dispatchScroll(newView, range);
      this.scrolled = null;
    }
  }

  /** @ignore */
  protected toText(): CS.Text | null {
    const view = this.proxy;
    return view ? view.state.doc : null;
  }

  // --- Public part

  /** Full document range. Remark: empty documents still have 1 (empty) line. */
  range(): Selection {
    const doc = this.toText();
    if (doc === null) return emptySelection;
    return { offset: 0, length: doc.length, fromLine: 1, toLine: doc.lines };
  }

  /** Returns 1 also when disconnected. */
  lineAt(offset: number): number {
    if (offset < 0) return -1;
    const doc = this.toText();
    if (!doc) return -1;
    const length = doc.length;
    if (offset > length) return -1;
    return doc.lineAt(offset).number;
  }

  /** Returns empty range when disconnected. */
  lineRange(line: number): Range {
    const doc = this.toText();
    if (!doc) return emptyRange;
    const { from: offset, to: endOffset } = doc.line(line);
    return { offset, length: endOffset - offset };
  }

  /** Remove all text from document. */
  clear(): void {
    const view = this.proxy;
    if (view) dispatchContents(view, CS.Text.empty);
  }

  /** Full document contents. */
  toString(slice ?: Range): string {
    const doc = this.toText();
    if (!doc) return '';
    if (slice) {
      const { offset, length } = slice;
      return doc.sliceString(offset, offset + length);
    } else
      return doc.toString();
  }

  /** Appends to end of document. */
  append(...values: unknown[]): void {
    if (values.length > 0) {
      const data = values.join(' ');
      const view = this.proxy;
      if (view) appendContents(view, data);
    }
  }

  /** Appends to end of document. */
  setContents(data: string): void {
    const view = this.proxy;
    if (view) dispatchContents(view, data);
  }

  replaceContents(range: Range, data = ''): void {
    const view = this.proxy;
    if (view) dispatchReplace(view, range, data);
  }

  /** Uses diff changes instead of replacing the entire view's contents. */
  updateContents(data: string): void {
    const view = this.proxy;
    if (view) updateContents(view, data);
  }

  /** Makes the editor scroll to the given range, when connected. */
  scrollTo(range: Range): void {
    const view = this.proxy;
    if (view) dispatchScroll(view, range);
    else this.scrolled = range;
  }

}

/* -------------------------------------------------------------------------- */
/* --- Text Buffer                                                        --- */
/* -------------------------------------------------------------------------- */

const NewLine = /(\r\n|\r|\n)/;
function textOf(text: string): CS.Text {
  return CS.Text.of(text.split(NewLine));
}

/** Text buffer extends a text proxy by making the contents persistent.

   Contents is kept in sync with the associated view, and is still maintained or
   updated when the view is unmounted.

   All methods are bound to `this`. */
export class TextBuffer extends TextProxy {

  // --- Private part (we avoid unnecessary conversions from/to text)
  // --- Invariant: only one of proxy, text or contents holds data

  private text = CS.Text.empty;
  private contents : string | undefined = undefined;

  /** @ignore */
  connect(newView: View): void {
    const oldView = this.proxy;
    if (oldView) {
      this.proxy = null;
      this.text = oldView.state.doc;
      // invariant preserved
      const { from: offset, to: endOffset } = oldView.state.selection.main;
      this.scrolled = { offset, length: endOffset - offset };
    }
    if (newView) {
      const newData = this.contents ?? this.text;
      this.proxy = newView;
      this.text = CS.Text.empty;
      this.contents = undefined;
      // invariant established
      dispatchContents(newView, newData);
      const range = this.scrolled;
      if (range) {
        dispatchScroll(newView, range);
        this.scrolled = null;
      }
    }
  }

  /** @ignore */
  protected toText(): CS.Text {
    // Data is in Proxy
    const view = this.proxy;
    if (view) return view.state.doc;
    // Data is in Text
    const contents = this.contents;
    if (contents===undefined) return this.text;
    // Data is in Contents
    const text = textOf(contents);
    this.text = text;
    this.contents = undefined;
    // --- invariant established
    return text;
  }

  // --- Specific parts

  clear(): void {
    const view = this.proxy;
    if (view) dispatchContents(view, CS.Text.empty);
    else {
      this.text = CS.Text.empty;
      this.contents = undefined;
      // invariant established
    }
  }

  toString(slice?: Range): string {
    const contents = this.contents;
    if (contents !== undefined) {
      if (slice) {
        const { offset, length } = slice;
        return contents.substring(offset, offset + length);
      } else return contents;
    } else return super.toString(slice);
  }

  append(...values: unknown[]): void {
    if (values.length > 0) {
      const data = values.join(' ');
      const view = this.proxy;
      if (view) { appendContents(view, data); }
      else {
        const state = CS.EditorState.create({ doc: this.toText() });
        const offset = state.doc.length;
        const changes = { from: offset, insert: data };
        const transaction = state.update({ changes });
        this.text = transaction.state.doc;
        this.contents = undefined;
        // invariant established
      }
    }
  }

  setContents(data: string): void {
    const view = this.proxy;
    if (view) dispatchContents(view, data);
    else {
      this.contents = data;
      this.text = CS.Text.empty;
      // invariant established
    }
  }

  replaceContents(range: Range, data = ''): void {
    const view = this.proxy;
    if (view) dispatchReplace(view, range, data);
    else {
      const { offset, length } = range;
      const endOffset = offset + length;
      const state = CS.EditorState.create({ doc: this.toText() });
      const changes = { from: offset, to: endOffset, insert: data };
      const transaction = state.update({ changes });
      this.text = transaction.state.doc;
      // invariant preserved
    }
  }

  updateContents(data: string): void {
    const view = this.proxy;
    if (view) updateContents(view, data);
    else {
      this.contents = data;
      this.text = CS.Text.empty;
      // invariant established
    }
  }

}

/* -------------------------------------------------------------------------- */
/* --- Code Mirror Extensions                                             --- */
/* -------------------------------------------------------------------------- */

class Extension {
  readonly extension : CS.Extension[] = [];
  pack(ext : CS.Extension): void { this.extension.push(ext); }
}

class Field<A> extends Extension {
  readonly field : CS.StateField<A>;
  private readonly annot : CS.AnnotationType<A>;

  constructor(init: A) {
    super();
    const annot = CS.Annotation.define<A>();
    const field = CS.StateField.define<A>({
      create: () => init,
      update: (fd: A, tr: CS.Transaction): A => {
        const newValue = tr.annotation(annot);
        return newValue !== undefined ? newValue : fd;
      }
    });
    this.annot = annot;
    this.field = field;
    this.pack(field);
  }

  dispatch(view: View, value: A): void {
    view?.dispatch({ annotations: this.annot.of(value) });
  }

}

class Option extends Extension {

  private readonly spec: CS.Extension;
  private readonly comp = new CS.Compartment();

  constructor(extension: CS.Extension) {
    super();
    this.spec = extension;
    this.pack(this.comp.of([]));
  }

  dispatch(view: View, active: boolean): void {
    if (view !== null && active !== undefined) {
      const effects = this.comp.reconfigure(active ? this.spec : []);
      view.dispatch({ effects });
    }
  }

}

/* -------------------------------------------------------------------------- */
/* --- Read Only                                                          --- */
/* -------------------------------------------------------------------------- */

const ReadOnly = new Field(false);

ReadOnly.pack(CS.EditorState.readOnly.from(ReadOnly.field));

/* -------------------------------------------------------------------------- */
/* --- Change Listener                                                    --- */
/* -------------------------------------------------------------------------- */

export type Callback = () => void;

const OnChange = new Field<Callback|null>(null);

OnChange.pack(
  CM.EditorView.updateListener.computeN(
    [OnChange.field],
    (state) => {
      const callback = state.field(OnChange.field);
      if (callback !== null)
        return [
          (updates: CM.ViewUpdate) => {
            if (!updates.changes.empty) callback();
          }
        ];
      return [];
    }
));

/* -------------------------------------------------------------------------- */
/* --- Selection Builder                                                  --- */
/* -------------------------------------------------------------------------- */

type cmrange = { from: number, to: number };

function selection(doc: CS.Text, range: cmrange) : Selection {
  const { from: offset, to: endOffset } = range;
  const fromLine = doc.lineAt(offset).number;
  const toLine = doc.lineAt(endOffset).number;
  return { offset, length: endOffset - offset, fromLine, toLine };
}

/* -------------------------------------------------------------------------- */
/* --- Selection Change Listener                                          --- */
/* -------------------------------------------------------------------------- */

export type SelectionCallback = (S: Selection) => void;

const OnSelect = new Field<SelectionCallback|null>(null);

OnSelect.pack(
  CM.EditorView.updateListener.computeN(
    [OnSelect.field],
    (state) => {
      const callback = state.field(OnSelect.field);
      if (callback !== null)
        return [
          (updates: CM.ViewUpdate) => {
            const oldSel = updates.startState.selection.main;
            const newSel = updates.state.selection.main;
            const doc = updates.state.doc;
            if (!newSel.eq(oldSel)) callback(selection(doc, newSel));
        }];
      return [];
    }
));

/* -------------------------------------------------------------------------- */
/* --- Viewport Change Listener                                           --- */
/* -------------------------------------------------------------------------- */

const Viewport = new Field<SelectionCallback|null>(null);

Viewport.pack(
  CM.EditorView.updateListener.computeN(
    [Viewport.field],
    (state) => {
      const callback = state.field(Viewport.field);
      if (callback !== null)
        return [
          (updates: CM.ViewUpdate) => {
            if (updates.viewportChanged) {
              const sel = updates.view.viewport;
              const doc = updates.state.doc;
              callback(selection(doc, sel));
            }
        }];
      return [];
    }
));

/* -------------------------------------------------------------------------- */
/* --- Hovering Listener                                                  --- */
/* -------------------------------------------------------------------------- */

export type MouseCallback = (pos: Position | null, evt: MouseEvent) => void;

function getPosition(evt: MouseEvent, view: CM.EditorView): Position | null
{
  const { x, y } = evt;
  const offset = view.posAtCoords({ x, y }, false);
  const line = view.state.doc.lineAt(offset);
  const p = view.coordsAtPos(line.from);
  const q = view.coordsAtPos(line.to);
  if (p !== null && q !== null) {
    const left = Math.trunc(p.left);
    const right = Math.trunc(q.right + 0.5);
    const top = Math.trunc(p.top);
    const bottom = Math.trunc(q.bottom + 0.5);
    const ok = left <= x && x <= right && top <= y && y <= bottom;
    if (ok) return { offset, line: line.number };
  }
  return null;
}

class MouseCallbackField extends Field<MouseCallback|null> {
  constructor() {
    super(null);
    this.callback = this.callback.bind(this);
  }

  callback(evt: MouseEvent, view: CM.EditorView): boolean {
    const fn = view.state.field(this.field);
    if (fn) {
      const pos = getPosition(evt, view);
      if (fn) fn(pos, evt);
    }
    return false;
  }
}

const OnClick = new MouseCallbackField();
const OnPopup = new MouseCallbackField();
const OnHover = new MouseCallbackField();
const OnDouble = new MouseCallbackField();
const OnGutter = new MouseCallbackField();

const gutterEventHandlers = {
  click: (view: CM.EditorView, block: CM.BlockInfo, evt: Event) => {
    const fn = view.state.field(OnGutter.field);
    if (fn) {
      const offset = block.from;
      const line = view.state.doc.lineAt(offset).number;
      fn({ offset, line }, evt as MouseEvent);
    }
    return false;
  }
};

const MouseEvents : CS.Extension = [
  OnClick,
  OnHover,
  OnPopup,
  OnDouble,
  OnGutter,
  CM.EditorView.domEventHandlers({
    click: (evt: MouseEvent, view: CM.EditorView) => {
      OnClick.callback(evt, view);
      if (evt.detail > 1) OnDouble.callback(evt, view);
      return false;
    },
    contextmenu: OnPopup.callback,
    mousemove: _.debounce(OnHover.callback, 10),
  }),
];

/* -------------------------------------------------------------------------- */
/* --- Decorations                                                        --- */
/* -------------------------------------------------------------------------- */

export interface MarkDecoration extends Range {

  /** The class of the decoration. */
  className?: string;

  /** The tooltip title of the decoration. */
  title?: string;

  /**
     Whether the decoration shall extend to characters inserted
     at start end end positions. Defaults to false.
   */
  inclusive?: boolean;
}

export interface LineDecoration {

  /** The line number to be decorated. */
  line: number;

  /** The class of the decoration. */
  className?: string;

  /** The tooltip title of the decoration. */
  title?: string;

}

export interface GutterDecoration extends LineDecoration {

  /** The gutter text mark (shall be few chatracters long). */
  gutter: string;

}

export type Decoration = MarkDecoration | LineDecoration | GutterDecoration;
export type Decorations =
  undefined | null | Decoration | readonly Decorations[];

/* -------------------------------------------------------------------------- */
/* --- Decoration Builder                                                 --- */
/* -------------------------------------------------------------------------- */

function isDecoration(d : Decorations) : d is Decoration
{
  return d !== undefined && d !== null && !Array.isArray(d);
}

function isMarkDecoration(d : Decoration) : d is MarkDecoration
{
  // eslint-disable-next-line no-prototype-builtins
  return d.hasOwnProperty("offset") && d.hasOwnProperty("length");
}

function isLineDecoration(d : Decoration) : d is LineDecoration
{
  // eslint-disable-next-line no-prototype-builtins
  return d.hasOwnProperty("line") && !d.hasOwnProperty("gutter");
}

function isGutterDecoration(d : Decoration) : d is GutterDecoration
{
  // eslint-disable-next-line no-prototype-builtins
  return d.hasOwnProperty("line") && d.hasOwnProperty("gutter");
}

/* -------------------------------------------------------------------------- */
/* --- Decorations Cache                                                  --- */
/* -------------------------------------------------------------------------- */

const DecorationCache : Map<string, CM.Decoration> = new Map();

function markDecoration(spec: MarkDecoration): CM.Decoration {
  const { className='', title='', inclusive=false } = spec;
  const key = `M${className}@T${title}@I{inclusive}`;
  let mark = DecorationCache.get(key);
  if (!mark) {
    const attributes = title ? { title } : undefined;
    mark = CM.Decoration.mark({
      'class': className,
      attributes,
      inclusive
    });
    DecorationCache.set(key, mark);
  }
  return mark;
}

function lineDecoration(spec: LineDecoration): CM.Decoration {
  const { className='', title='' } = spec;
  const key = `L${className}@T${title}`;
  let line = DecorationCache.get(key);
  if (!line) {
    const attributes = title ? { title } : undefined;
    line = CM.Decoration.line({
      'class': className,
      attributes,
    });
    DecorationCache.set(key, line);
  }
  return line;
}

/* -------------------------------------------------------------------------- */
/* --- Gutter Cache                                                       --- */
/* -------------------------------------------------------------------------- */

class GutterMark extends CM.GutterMarker {
  private spec: GutterDecoration;

  constructor(spec: GutterDecoration) {
    super();
    this.spec = spec;
  }

  toDOM(): Node {
    const {  gutter, className, title } = this.spec;
    const textNode = document.createTextNode(gutter);
    if (!className && !title) return textNode;
    const span = document.createElement("span");
    span.appendChild(textNode);
    if (className) span.className = className;
    if (title) span.title = title;
    return span;
  }

}

const GutterCache : Map<string, GutterMark> = new Map();

function gutterMark(spec: GutterDecoration) : CM.GutterMarker {
  const { gutter, className='', title='' } = spec;
  const key = `G${gutter}@C${className}@T${title}`;
  let marker = GutterCache.get(key);
  if (!marker) {
    marker = new GutterMark(spec);
    GutterCache.set(key, marker);
  }
  return marker;
}

/* -------------------------------------------------------------------------- */
/* --- Decorations Builder                                                --- */
/* -------------------------------------------------------------------------- */

class DecorationsBuilder {

  private ranges : CS.Range<CM.Decoration>[] = [];
  private gutters : CS.Range<CM.GutterMarker>[] = [];
  protected readonly doc : CS.Text;

  constructor(doc: CS.Text) {
    this.doc = doc;
    this.addSpec = this.addSpec.bind(this);
  }

  addMark(spec: MarkDecoration): void {
    const { offset, length } = spec;
    if (offset < 0 || length < 1) return;
    const endOffset = offset + length;
    if (endOffset > this.doc.length) return;
    this.ranges.push(markDecoration(spec).range(offset, endOffset));
  }

  addLine(spec: LineDecoration): void {
    const { line } = spec;
    if (line < 1) return;
    if (line > this.doc.lines) return;
    const offset = this.doc.line(line).from;
    this.ranges.push(lineDecoration(spec).range(offset));
  }

  addGutter(spec: GutterDecoration): void {
    const { line } = spec;
    if (line < 1) return;
    if (line > this.doc.lines) return;
    const offset = this.doc.line(line).from;
    this.gutters.push(gutterMark(spec).range(offset));
  }

  addSpec(spec : Decorations): void {
    if (isDecoration(spec)) {
      if (isMarkDecoration(spec)) this.addMark(spec);
      if (isLineDecoration(spec)) this.addLine(spec);
      if (isGutterDecoration(spec)) this.addGutter(spec);
    } else
    if (Array.isArray(spec))
      spec.forEach(this.addSpec);
  }

  getRanges(): CS.RangeSet<CM.Decoration> {
    return CS.RangeSet.of(this.ranges, true);
  }

  getGutters(): CS.RangeSet<CM.GutterMarker> {
    return CS.RangeSet.of(this.gutters, true);
  }

}

/* -------------------------------------------------------------------------- */
/* --- Decorations                                                        --- */
/* -------------------------------------------------------------------------- */

interface Decorator {
  spec : Decorations;
  ranges : CM.DecorationSet;
  gutters : CS.RangeSet<CM.GutterMarker>;
}

const DecoratorSpec = CS.Annotation.define<Decorations>();

const DecoratorState = CS.StateField.define<Decorator>({

  create: () => ({
    spec: null,
    ranges: CS.RangeSet.empty,
    gutters: CS.RangeSet.empty,
  }),

  update(value: Decorator, tr: CS.Transaction) {
    const newSpec : Decorations = tr.annotation(DecoratorSpec) ?? null;
    if (newSpec !== null) {
      const builder = new DecorationsBuilder(tr.newDoc);
      builder.addSpec(newSpec);
      return {
        spec: newSpec,
        ranges: builder.getRanges(),
        gutters: builder.getGutters(),
      };
    }
    if (tr.docChanged)
      return {
        spec: value.spec,
        ranges: value.ranges.map(tr.changes),
        gutters: value.gutters.map(tr.changes),
      };
    return value;
  },

});

function dispatchDecorations(view: View, spec: Decorations): void {
  view?.dispatch({ annotations: DecoratorSpec.of(spec) });
}

const Decorations: CS.Extension = [
  DecoratorState,
  CM.EditorView.decorations.from(DecoratorState, ({ ranges }) => ranges),
  OnGutter,
  CM.gutter({
    markers: (view) => view.state.field(DecoratorState).gutters,
    domEventHandlers: gutterEventHandlers,
  }),
];

/* -------------------------------------------------------------------------- */
/* --- Line Numbers                                                       --- */
/* -------------------------------------------------------------------------- */

const LineNumbers = new Option(
  CM.lineNumbers({ domEventHandlers: gutterEventHandlers })
);

/* -------------------------------------------------------------------------- */
/* --- Active Line                                                        --- */
/* -------------------------------------------------------------------------- */

const ActiveLine = new Option([
  CM.highlightActiveLine(),
  CM.highlightActiveLineGutter(),
]);

/* -------------------------------------------------------------------------- */
/* --- Editor View                                                        --- */
/* -------------------------------------------------------------------------- */

function createView(parent: Element): CM.EditorView {
  const extensions : CS.Extension[] = [
    LineNumbers,
    ActiveLine,
    ReadOnly,
    OnChange,
    OnSelect,
    Viewport,
    MouseEvents,
    Decorations,
  ];
  const state = CS.EditorState.create({ extensions });
  return new CM.EditorView({ state, parent });
}

/* -------------------------------------------------------------------------- */
/* --- Rich Text Component                                                --- */
/* -------------------------------------------------------------------------- */

export interface TextViewProps {
  text?: TextProxy | string;
  readOnly?: boolean;
  onChange?: Callback;
  selection?: Range;
  onViewport?: SelectionCallback;
  onSelection?: SelectionCallback;
  onClick?: MouseCallback;
  onPopup?: MouseCallback;
  onHover?: MouseCallback;
  onGutter?: MouseCallback;
  onDoubleClick?: MouseCallback;
  decorations?: Decorations;
  lineNumbers?: boolean;
  showCurrentLine?: boolean;
  display?: boolean;
  visible?: boolean;
  scrollToBottom?: boolean;
  className?: string;
  style?: CSSProperties;
}

export function TextView(props: TextViewProps) : JSX.Element {
  const [view, setView] = React.useState<View>(null);

  // --- Text Proxy
  const { text } = props;
  React.useEffect(() => {
    if (text instanceof TextProxy) {
      text.connect(view);
      if (view) return () => text.connect(null);
    }
    if (typeof(text)==='string' && view) {
      dispatchContents(view, text);
    }
    return undefined;
  }, [text, view]);

  // ---- Fields Props
  const {
    onClick = null,
    onPopup = null,
    onHover = null,
    onGutter = null,
    onChange = null,
    readOnly = false,
    onViewport: onReview = null,
    onSelection: onSelect = null,
    onDoubleClick: onDouble = null,
    lineNumbers: lines = false,
    showCurrentLine: active = false,
  } = props;
  React.useEffect(() => OnClick.dispatch(view, onClick), [view, onClick]);
  React.useEffect(() => OnPopup.dispatch(view, onPopup), [view, onPopup]);
  React.useEffect(() => OnHover.dispatch(view, onHover), [view, onHover]);
  React.useEffect(() => OnDouble.dispatch(view, onDouble), [view, onDouble]);
  React.useEffect(() => OnGutter.dispatch(view, onGutter), [view, onGutter]);
  React.useEffect(() => OnChange.dispatch(view, onChange), [view, onChange]);
  React.useEffect(() => OnSelect.dispatch(view, onSelect), [view, onSelect]);
  React.useEffect(() => Viewport.dispatch(view, onReview), [view, onReview]);
  React.useEffect(() => ReadOnly.dispatch(view, readOnly), [view, readOnly]);
  React.useEffect(() => ActiveLine.dispatch(view, active), [view, active]);
  React.useEffect(() => LineNumbers.dispatch(view, lines), [view, lines]);

  // ---- Decorations
  const { decorations: decors = null } = props;
  React.useEffect(() => dispatchDecorations(view, decors), [view, decors]);

  // ---- Selection
  const { selection } = props;
  React.useEffect(() => {
    if (selection && view) dispatchScroll(view, selection);
  }, [view, selection]);

  // ---- Mount & Unmount Editor
  const [nodeRef, setRef] = React.useState<Element | null>(null);
  React.useEffect(() => {
    if (!nodeRef) return;
    const view = createView(nodeRef);
    setView(view);
    return () => { setView(null); view.destroy(); };
  }, [nodeRef]);

  /**
   * Check if editor is scroll to the bottom
   * isBottom is true if the last line of the document is in memory.
   * isBottom=true does not mean that the last line is visible.
   */
  const [isBottom, setIsBottom] = React.useState(false);
  React.useEffect(() => {
    if (!view || !props.scrollToBottom) return;
    const setBottom = (view: View): void => {
      if(view) {
        const lineAt = (v: number): number => view.state.doc.lineAt(v).number;
        setIsBottom(lineAt(view.state.doc.length) <= lineAt(view.viewport.to));
      }
    };

    view.scrollDOM.addEventListener("scroll", () => setBottom(view));
    return () => {
      view.scrollDOM.removeEventListener("scroll", () => setBottom(view));
    };
  }, [view, props.scrollToBottom, setIsBottom]);

  // ---- Editor DIV
  const { visible=true, display=true } = props;
  const className = classes(
    'cm-global-box',
    !display && 'dome-erased',
    !visible && 'dome-hidden',
    props.className,
  );
  return <>
    <div className={className} style={props.style} ref={setRef} />
    { props.scrollToBottom &&
        <Button
          icon='ANGLE.DOWN'
          title='Scroll to bottom'
          display={!isBottom}
          style={{ position: 'absolute', bottom: '40px', right: '20px' }}
          onClick={() => {
            if(view) view.dispatch({
              effects: CM.EditorView.scrollIntoView(
                view.state.doc.length, { y: "end" }
              )
            });
          }}
        />
    }
  </>;
}

/* -------------------------------------------------------------------------- */
