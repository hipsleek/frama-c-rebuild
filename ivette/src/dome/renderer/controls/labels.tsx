/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Labels
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/controls/labels
*/

import React, { LegacyRef } from 'react';
import { classes } from 'dome/misc/utils';
import { Icon } from './icons';
import './style.css';

// --------------------------------------------------------------------------
// --- Generic Label
// --------------------------------------------------------------------------

export type IconKind =
  'default' | 'disabled' | 'warning' | 'positive' | 'negative' | 'selected';

/** Labels support forwarding refs to their inner [<label/>] element. */
export interface LabelProps {
  /** Text of the label. Prepend to other children elements. */
  label?: string;
  /** Icon identifier. Displayed on the left side of the label. */
  icon?: string;
  /** Tool-tip description. */
  title?: string;
  /** Icon kind. */
  kind?: IconKind,
  /** Icon spinning */
  spinning?: boolean;
  /** Additional class. */
  className?: string;
  /** Additional class for icon. */
  iconClassName?: string;
  /** Additional style. */
  style?: React.CSSProperties;
  /** If `false`, do not display the label. Default to `true`. */
  display?: boolean;
  /** Additional content of the `<label/>` element. */
  children?: React.ReactNode;
  /** Html tag `<input />` element. */
  htmlFor?: string;
  /** Click event callback. */
  onClick?: (evt: React.MouseEvent) => void;
  /** Click event callback. */
  onDoubleClick?: (evt: React.MouseEvent) => void;
  /** Right-click event callback. */
  onContextMenu?: (evt: React.MouseEvent) => void;
}

const makeLabel = (className: string) =>
  function Label(
    props: LabelProps,
    ref: LegacyRef<HTMLLabelElement> | undefined
  ): JSX.Element {
    const { display = true, kind = 'default' } = props;
    const allClasses = classes(
      className,
      !display && 'dome-control-erased',
      props.className,
    );
    const iconClass = classes(
      'dome-xIcon-' + kind,
      props.iconClassName,
    );
    return (
      <label
        ref={ref}
        className={allClasses}
        title={props.title}
        style={props.style}
        onClick={props.onClick}
        onDoubleClick={props.onDoubleClick}
        onContextMenu={props.onContextMenu}
        htmlFor={props.htmlFor}
      >
        {props.icon &&
          <Icon title={props.title}
                id={props.icon}
                className={iconClass}
                spinning={props.spinning}
          />}
        {props.label}
        {props.children}
      </label>
    );
  };

// --------------------------------------------------------------------------
// --- CSS Classes
// --------------------------------------------------------------------------

const LABEL = 'dome-xLabel dome-text-label';
const TITLE = 'dome-xLabel dome-text-title';
const DESCR = 'dome-xLabel dome-text-descr';
const TDATA = 'dome-xLabel dome-text-data';
const TCODE = 'dome-xLabel dome-text-code';
const TCELL = 'dome-xLabel dome-text-cell';
const TITEM = 'dome-xLabel dome-text-item';

// --------------------------------------------------------------------------
// --- Components
// --------------------------------------------------------------------------

/** Simple labels. */
export const Label = React.forwardRef(makeLabel(LABEL));

/** Title and headings. */
export const Title = React.forwardRef(makeLabel(TITLE));

/** Description, textbook content. */
export const Descr = React.forwardRef(makeLabel(DESCR));

/** Selectable textual information. */
export const Data = React.forwardRef(makeLabel(TDATA));

/** Selectable inlined source-code content. */
export const Code = React.forwardRef(makeLabel(TCODE));

/** Selectable inlined source-code content with default cursor. */
export const Cell = React.forwardRef(makeLabel(TCELL));

/** Non-selectable inlined source-code content with default cursor. */
export const Item = React.forwardRef(makeLabel(TITEM));

// --------------------------------------------------------------------------
