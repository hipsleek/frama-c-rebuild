/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- LEDs, LCD, meters, etc.
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/controls/displays
 */

import React from 'react';
import { classes } from 'dome/misc/utils';
import { Icon } from './icons';
import { LabelProps } from './labels';
import './style.css';

// --------------------------------------------------------------------------
// --- LCD
// --------------------------------------------------------------------------

/** Button-like label. */
export function LCD(props: LabelProps): JSX.Element {
  const className = classes(
    'dome-xButton dome-xBoxButton dome-text-code dome-xButton-lcd ',
    props.className,
  );
  return (
    <label
      className={className}
      title={props.title}
      style={props.style}
    >
      {props.icon && <Icon id={props.icon} />}
      {props.label}
      {props.children}
    </label>
  );
}

// --------------------------------------------------------------------------
// --- Led
// --------------------------------------------------------------------------

export const LEDStatusList = [
  'active', 'inactive', 'positive', 'negative', 'warning'
] as const;

export type LEDstatus = typeof LEDStatusList[number] | undefined;

export function jLEDstatus(js : string) : LEDstatus {
  return LEDStatusList.find(elt => elt === js);
}

export interface LEDprops {
  /**
     Led status:
     - `'inactive'`: off (default)
     - `'active'`: blue color
     - `'positive'`: green color
     - `'negative'`: red color
     - `'warning'`: orange color
   */
  status?: LEDstatus;
  /** Blinking Led (default: `false`). */
  blink?: boolean;
  /** Tooltip text. */
  title?: string;
  /** Additional CSS class. */
  className?: string;
  /** Additional CSS style. */
  style?: React.CSSProperties;
}

export const LED = (props: LEDprops): JSX.Element => {
  const className = classes(
    'dome-xButton-led',
    `dome-xButton-led-${props.status || 'inactive'}`,
    props.blink && 'dome-xButton-blink',
    props.className,
  );
  return (
    <div className={className} title={props.title} style={props.style} />
  );
};

// --------------------------------------------------------------------------
// --- Metter
// --------------------------------------------------------------------------

export interface MeterProps {
  /** Additional CSS class. */
  className?: string;
  /** Additional CSS style. */
  style?: React.CSSProperties;
  /** Disabled control. */
  /** Meter value. Undefined means disabled. */
  value: number; /** default is undefined */
  min?: number;  /** default is 0.0 */
  low?: number;  /** default is 0.0 */
  high?: number; /** default is 1.0 */
  max?: number;  /** default is 1.0 */
  optimum?: number | 'LOW' | 'MEDIUM' | 'HIGH'; /** default is undefined */
}

export const Meter = (props: MeterProps): JSX.Element => {
  const { className, style, value, optimum, ...ms } = props;
  const min = props.min ?? 0.0;
  const max = props.max ?? 1.0;
  const low = props.low ?? min;
  const high = props.high ?? max;
  const theClass = classes('dome-xMeter', className);
  let opt: number | undefined;
  if (value !== undefined)
    switch (optimum) {
      case 'LOW': opt = (min + low) / 2; break;
      case 'MEDIUM': opt = (low + high) / 2; break;
      case 'HIGH': opt = (high + max) / 2; break;
      default: opt = optimum;
    }
  const mv = value === undefined ? min : Math.min(max, Math.max(min, value));
  return (
    <meter
      className={theClass}
      style={style}
      value={mv}
      optimum={opt}
      {...ms} />
  );
};

// --------------------------------------------------------------------------
