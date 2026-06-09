/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import { jOption } from 'dome/data/json';
import {
  MarkDecoration, GutterDecoration, Decorations,
  Position, Range, TextProxy, TextView,
} from 'dome/text/richtext';

import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as RichText from 'frama-c/richtext';
import type { text } from 'frama-c/kernel/api/data';
import * as TIP from 'frama-c/plugins/wp/api/tip';

/* -------------------------------------------------------------------------- */
/* --- Sequent Decorations                                                --- */
/* -------------------------------------------------------------------------- */

type Node = TIP.node | undefined;
type Location = { part?: RichText.Tag, term?: RichText.Tag };
type Target = { selection?: Range, gutters: GutterDecoration[] };

class Sequent {
  private readonly contents: string;
  private readonly tags: RichText.Tags;
  private readonly style: MarkDecoration[];
  private readonly index: RichText.TagIndex;

  constructor(jtext: text) {
    this.contents = RichText.textToString(jtext);
    this.style = [];
    const addStyle = (tg: RichText.Tag, className: string): void => {
      const { offset, endOffset } = tg;
      const length = endOffset - offset;
      this.style.push({ className, offset, length });
    };
    const filter = (t: RichText.Tag): boolean => {
      switch(t.tag) {
        case 'wp:comment':
        case 'wp:property':
          addStyle(t, 'cm-comment');
          return false;
        case 'wp:stmt':
        case 'wp:clause':
        case 'wp:warning':
          addStyle(t, 'cm-keyword');
          return false;
        case 'wp:label':
          addStyle(t, 'cm-def');
          return false;
        case 'wp:var':
          addStyle(t, 'cm-type');
          return false;
        case 'wp:focus':
          addStyle(t, 'cm-multiple-code');
          return false;
        case 'wp:target':
          addStyle(t, 'cm-selected-code');
          return false;
      }
      return t.tag.startsWith('#');
    };
    const { index, tags } = RichText.textToTags(jtext, filter);
    this.tags = tags;
    this.index = index;
  }

  get text(): string {
    return this.contents;
  }

  get decorations(): Decorations {
    return this.style;
  }

  find(tg: string | undefined) : RichText.Tags {
    return tg ? this.index.get(tg) ?? [] : [];
  }

  select(part?: TIP.part, term?: TIP.term): RichText.Tag | undefined {
    const ptags = this.find(part);
    if (!ptags) return;
    const etags = this.find(term);
    if (etags) {
      const tg = etags.find(e => ptags.find(p => RichText.contains(p, e)));
      if (tg) return tg;
    }
    return ptags[0];
  }

  target(proxy: TextProxy, part?: TIP.part, term?: TIP.term): Target {
    const tag = this.select(part, term);
    const gutters: GutterDecoration[] = [];
    if (tag) {
      const selection: Range = { offset: tag.offset, length: 0 };
      const lineP = proxy.lineAt(tag.offset+1);
      const lineQ = proxy.lineAt(tag.endOffset);
      if (0 <= lineP && 0 <= lineQ)
        for (let line = lineP; line <= lineQ; line++)
          gutters.push({ className: 'wp-gutter-part', gutter: '|', line });
      return { selection, gutters };
    } else
      return { selection: undefined, gutters };
  }

  locate(position: number): Location {
    const isPart = ({ tag }: RichText.Tag): boolean => {
      return tag === '#goal' || tag.startsWith('#s');
    };
    const part = RichText.findTag(this.tags, position, isPart);
    if (!part) return {};
    const term = RichText.findTag(part.children, position);
    return { part, term };
  }

  hover(pos: Position | null): Decorations {
    if (pos) {
      const { part, term } = this.locate(pos.offset);
      const range = term ?? part;
      if (range) {
        const { offset, endOffset } = range;
        const length = endOffset - offset;
        return { className: 'wp cm-hovered-code', offset, length };
      }
    }
    return null;
  }

}

/* -------------------------------------------------------------------------- */
/* --- Sequent View                                                       --- */
/* -------------------------------------------------------------------------- */

export interface GoalViewProps {
  node: Node;
  locked: boolean;
  showce: boolean;
  autofocus: boolean;
  unmangled: boolean;
  iformat: TIP.iformat;
  rformat: TIP.rformat;
}

export function GoalView(props: GoalViewProps): JSX.Element {
  const { node, locked } = props;
  const jtext = States.useRequestStable(TIP.printSequent, props);
  const { part, term } = States.useRequestStable(TIP.getSelection, node);
  const proxy = React.useMemo(() => new TextProxy(), []);
  const sequent = React.useMemo(() => new Sequent(jtext), [jtext]);
  React.useEffect(() => proxy.updateContents(sequent.text), [proxy, sequent]);
  const { selection, gutters } = React.useMemo(
    () => sequent.target(proxy, part, term),
    [sequent, proxy, part, term]
  );
  const focus : Decorations =
    selection && { className: 'wp-selected', ...selection };
  const [hover, setHover] = React.useState<Decorations>(null);
  const onHover = React.useCallback((pos: Position | null) => {
    setHover(sequent.hover(pos));
  }, [sequent]);
  const onClick = React.useCallback((pos: Position | null) => {
    setHover(null);
    if (node && pos) {
      const loc = sequent.locate(pos.offset);
      const part = jOption(TIP.jPart)(loc.part?.tag);
      const term = jOption(TIP.jTerm)(loc.term?.tag);
      if (part || term) {
        Server.send(TIP.setSelection, { node, part, term });
        return;
      }
    } // otherwise
    Server.send(TIP.clearSelection, node);
  }, [sequent, node]);
  return (
    <TextView
      readOnly
      className='wp'
      text={proxy}
      decorations={[hover, focus, sequent.decorations, gutters]}
      onHover={onHover}
      onClick={locked ? undefined : onClick}
    />
  );
}

/* -------------------------------------------------------------------------- */
