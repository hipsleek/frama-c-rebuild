/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import ReactMarkdown, { Options } from 'react-markdown';
import remarkCustomHeaderId from 'remark-custom-header-id';

import { getResourcePath } from 'dome';
import * as Themes from 'dome/themes';
import { classes } from 'dome/misc/utils';
import { Icon, jIconKind, IconKind as _IconKind  } from 'dome/controls/icons';
import {
  CodeBlock, atomOneDark, atomOneLight
} from "react-code-blocks";
import {
  jLEDstatus, LED, LEDstatus as _LEDstatus
} from 'dome/controls/displays';

export interface Pattern {
  pattern: RegExp,
  replace: (key: number, match?: RegExpExecArray) => JSX.Element | null
}

/**
 * iconTag allows you to replace the tag with an {@link Icon}.
 *
 * `[icon-<id>(-<kind | color>)?]` :
 *
 * * Id : case-insensitive, consult [Icon Gallery](../../doc/guides/icons.md)
 * * kind : {@link _IconKind}
 * * color : Hexa or html
 *
 * @example
 * [icon-tunings-#FF0000] : this tag will be replaced by a red TUNINGS icon
 */

export const iconTag: Pattern = {
  pattern: /(\[icon-([^-\]]+)(-([^\]]+))?\])/g,
  replace: (key: number, match?: RegExpExecArray) => {
    if(!match) return null;
    const kind = jIconKind(match[4]);
    const color = !kind ? match[4] : undefined;
    return <Icon key={key}
     id={match[2].toUpperCase()}
     kind={kind}
     fill={color}
    />;
  }
};

/**
 * ledTag allows you to replace the tag with an {@link LED}.
 *
 * `[led-<status>]` : {@link _LEDstatus}
 * */
export const ledTag: Pattern = {
  pattern: /\[led-([^\]]+)\]/g,
  replace: (key: number, match?: RegExpExecArray) => {
    if(!match) return null;
    const status = jLEDstatus(match[1]);
    return <LED key={key} status={status} />;
  }
};

const defaultPatterns = [iconTag, ledTag];

// --------------------------------------------------------------------------
// --- Replacement function
// --------------------------------------------------------------------------

/**
 * Replace all tag in children.
 * This function doesn't replace any tags added by a previous replacement.
 *
 * Patterns added to the table will be processed to make replacements
 * in the markdown file. Markdown component provides two patterns,
 * iconTag and ledTag
 */
function replaceTags(
  children: React.ReactNode,
  patterns?: Pattern[],
): React.ReactNode {
  if(!patterns || patterns.length < 1) return children;

  const buffer: React.ReactNode[] = [];
  let counter = 0;
  const childrenTab = React.Children.toArray(children);

  const makeReplace = (text: string, index: number): void => {
    if(index >= patterns.length) {
      buffer.push(text);
      return;
    }
    const { pattern, replace } = patterns[index];

    let match;
    let lastIndex = 0;
    while ((match = pattern.exec(text)) !== null) {
      if (match.index > lastIndex) {
        makeReplace(text.slice(lastIndex, match.index), index+1);
      }
      buffer.push(replace(counter++, match));
      lastIndex = pattern.lastIndex;
    }
    if (lastIndex < text.length) {
      makeReplace(text.slice(lastIndex), index+1);
    }
  };

  /** makeReplace is applied if child is a string,
   * otherwise it is pushed into the buffer
   */
  childrenTab.forEach((child) => {
    if (typeof child === 'string') {
      return makeReplace(child, 0);
    }
    return buffer.push(child);
  });

  return buffer;
}

// --------------------------------------------------------------------------
// --- Markdown component
// --------------------------------------------------------------------------

export interface MarkdownProps {
  /** classes for Markdown component */
  className?: string;
  /** Tab of patterns */
  patterns?: Pattern[];
  /** scroll to the chosen id */
  scrollTo?: string;
  /** On link click */
  onLinkClick?: (id: string) => void
  /** check link: return true if id existe */
  checkLink?: (id: string) => boolean
  /** Children */
  children?: string | null;
}

export function Markdown(
  props: MarkdownProps
): JSX.Element {
  const { className, scrollTo, checkLink, onLinkClick,
    patterns = [], children } = props;
  const mdPatterns = patterns.concat(defaultPatterns);

  const theme = Themes.useColorTheme()[0];
  const markdownClasses = classes(
    "dome-xMarkdown", "dome-pages", className
  );
  let liKey: number = 0;

  const scroll = (id: string): void => {
    const elt = document.getElementById(id);
    if(elt) elt.scrollIntoView({ behavior: "smooth" });
  };

  React.useEffect(() => { if(scrollTo) scroll(scrollTo); }, [scrollTo]);

  const options: Options = {
    className: markdownClasses,
    remarkPlugins: [remarkCustomHeaderId],
  };
  options.components = {
    p: ({ children }) => <div>{replaceTags(children, mdPatterns)}</div>,
    li: ({ children }) => {
      return <li key={liKey++}>{replaceTags(children, mdPatterns)}</li>;
    },
    /** Uses codeBlock if ``` is used in markdown with a language,
     *  otherwise the code-inline class is added */
    code: ({ className, children }) => {
      if (className && className.includes("language-")
        && typeof children === "string"
      ) {
        const language = className.split("language-")[1];
        return <CodeBlock
          text={children}
          language={language}
          showLineNumbers={false}
          theme={theme === 'dark' ? atomOneDark : atomOneLight}
        />;
      }
      return <code className='code-inline'>{children}</code>;
    },
    /** Change the behavior of links on anchors */
    a: ({ href, children }) => {
      if (href && href.startsWith("#")) {
        const id = href.slice(1);
        const className = checkLink && !checkLink(id) ?
          "invalid-link" : undefined;
        return ( <a href={href}
          className={className}
          onClick={(e) => {
            e.preventDefault();
            if(onLinkClick) onLinkClick(id);
            else scroll(id);
          }}>{children}</a>
        );
      }
      return <a href={href}>{children}</a>;
    },
    img: ({ src, alt }) => {
      const newSrc = src ? getResourcePath(src) : undefined;
      return <img src={newSrc} alt={alt} style={{ maxWidth: '100%' }} />;
    }
  };

  return <ReactMarkdown {...options}>{ children }</ReactMarkdown>;
}

/* -------------------------------------------------------------------------- */
