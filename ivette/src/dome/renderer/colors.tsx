/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
   @packageDocumentation
   @module dome/colors
 */

import React from 'react';
import { useStyle, useColorTheme } from 'dome/themes';

export enum EColor {
  DEFAULT = "default",
  WHITE = 'white',
  LIGHTGREY = 'lightgrey',
  GREY = 'grey',
  DARK = 'dark',
  PRIMARY = 'primary',
  SELECTED = 'selected',
  GREEN = 'green',
  ORANGE = 'orange',
  RED = 'red',
  YELLOW = 'yellow',
  BLUE = 'blue',
  PINK = 'pink'
}

export type TColor = `${EColor}`

type TColorVal = {
  [key in EColor]: string
}

export interface IHookColors {
  BGCOLOR: TColorVal;
  SGCOLOR: TColorVal;
  FGCOLOR: TColorVal;
  EDCOLOR: TColorVal;
}

type TColorCategory = 'bg'|'fg'|'sg'|'ed';

export function useColor(): IHookColors  {
  const style = useStyle();
  const [theme, ] = useColorTheme();

  const enum2Tcolor = (callback: (elt: EColor) => string): TColorVal => {
    return {
      ...(Object.fromEntries(
        Object.values(EColor).map((val: EColor) => [ val, callback(val)])
      )),
    } as TColorVal;
  };

  function getColorType(type: TColorCategory): TColorVal {
    return enum2Tcolor(
      (elt) => style.getPropertyValue('--graph-'+type+'-color-'+elt)
    );
  }

  const colors = React.useMemo(() => {
    return {
      // node background colors
      BGCOLOR: getColorType('bg'),
      // // cluster background colors
      SGCOLOR: getColorType('sg'),
      //  foreground colors
      FGCOLOR: getColorType('fg'),
      // // edge colors
      EDCOLOR: getColorType('ed'),
    };
  },
    /** style is dependent on theme but it is not used directly */
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [theme]
  );
  return colors;
}

/**
 * Each RGB component of the color is modified by :
 * - adding a percentage of the (255 - component value) for lighten
 * - removing a percentage of the component value for darken.
 *
 * A negative percentage darkens and a positive percentage lightens the color.
 *
 * @param hex color to transform
 * @param amount percentage in [-100, 100]
 * @returns new hexadecimal color
*/
export function transformColor(hex: string, amount: number): string {
  const percentage =  Math.max(-100, Math.min(100, amount));

  function hexToRgb(hex: string): [number, number, number] {
    hex = hex.replace('#', '');
    const bigint = parseInt(hex, 16);
    return [(bigint >> 16) & 255, (bigint >> 8) & 255, bigint & 255];
  }
  function rgbToHex(r: number, g: number, b: number): string {
    return `#${((1 << 24) + (r << 16) + (g << 8) + b)
      .toString(16).slice(1).toUpperCase()}`;
  }
  function newColor(color: number): number {
    const base = percentage < 0 ? color : (255 - color);
    return color + Math.floor(base * percentage / 100);
  }

  const [r, g, b] = hexToRgb(hex).map(color => newColor(color));
  return rgbToHex(r, g, b);
}
