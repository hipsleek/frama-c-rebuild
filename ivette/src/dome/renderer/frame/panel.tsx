/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
  This package allows us to add a panel inside positioned elements.

  It provides some components to create the panel content:
  * ListElement
  * Text
  * Actions

  @packageDocumentation
  @module dome/frame/Panel
 */

import React from 'react';
import { classes } from 'dome/misc/utils';
import { Hbox } from 'dome/layout/boxes';
import { Label } from 'dome/controls/labels';


/* --------------------------------------------------------------------------*/
/* --- Panel Container                                                       */
/* --------------------------------------------------------------------------*/

export type PanelPosition = 'top' | 'bottom' | 'left' | 'right';

interface PanelProps {
  /** Label. */
  label?: string;
  /** Icon. */
  icon?: string;
  /** Title. */
  title?: string;
  /** Actions : Add in the panel title */
  actions?: React.JSX.Element;
  /** Additional class. */
  className?: string;
  /** Position to displayed the panel. Default 'tr' */
  position?: PanelPosition;
  /** Defaults to `true`. */
  visible?: boolean;
  /** Defaults to `true`. */
  display?: boolean;
  /** Panel children. */
  children: JSX.Element;
}

export const Panel = (props: PanelProps): JSX.Element => {
  const { label, icon, title, actions, visible = true, display = true,
    className, position = 'right' } = props;

  const classNames = classes(
    'dome-xPanel',
    'dome-xPanel-'+position,
    visible ? 'dome-xPanel-open' : 'dome-xPanel-close',
    !display && 'dome-control-erased',
    className,
  );

  return (
    <div className={classNames}>
      { (label || icon || actions) &&
      <Hbox className={'dome-xPanelTitle'}>
        <Label icon={icon} label={label} title={title}/>
        { actions}
      </Hbox>
      }
      { props.children }
    </div>
  );
};

/* --------------------------------------------------------------------------*/
/* --- Panel List                                                            */
/* --------------------------------------------------------------------------*/
export interface ElementProps {
  /** Selection state. */
  selected?: boolean;
  /** Selection callback. */
  onSelection?: () => void;
  /** Item element. */
  children?: JSX.Element;
}

export function Element(props: ElementProps): JSX.Element {
  const { selected = true, onSelection, children } = props;

  const classNames = classes(
    'dome-xPanel-element',
    selected ? 'dome-active' : 'dome-inactive',
  );
  return (
    <div
      className={classNames}
      onClick={onSelection}
    >
      {children}
    </div>
  );
}

interface ListElementProps {
  children: JSX.Element[];
}

export function ListElement(props: ListElementProps): JSX.Element {
  return (
    <div className='dome-xPanel-list'>
      {props.children}
    </div>
  );
}
