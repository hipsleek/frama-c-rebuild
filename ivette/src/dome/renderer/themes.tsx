/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Global Color Theme Management
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/themes
 */

import React from 'react';
import * as Dome from 'dome';
import * as Settings from 'dome/data/settings';
import { State } from 'dome/data/states';
import { ipcRenderer } from 'electron';

/* -------------------------------------------------------------------------- */
/* --- Global Settings                                                    --- */
/* -------------------------------------------------------------------------- */

export type ColorTheme = 'dark' | 'light';
export type ColorSettings = 'dark' | 'light' | 'system';

export const jColorTheme =
  (th: string | undefined): ColorTheme => (th === 'dark' ? 'dark' : 'light');
export const jColorSettings =
  (th: string | undefined): ColorSettings => {
    switch (th) {
      case 'light':
      case 'dark':
      case 'system':
        return th;
      default:
        return 'system';
    }
  };

const ColorThemeSettings = new Settings.GString('dome-color-theme', 'system');
const NativeThemeUpdated = new Dome.Event('dome.theme.updated');
ipcRenderer.on('dome.theme.updated', () => NativeThemeUpdated.emit());

async function getNativeTheme(): Promise<ColorTheme> {
  const th = await ipcRenderer.invoke('dome.ipc.theme');
  return jColorTheme(th);
}

/* -------------------------------------------------------------------------- */
/* --- Color Theme Hooks                                                  --- */
/* -------------------------------------------------------------------------- */

export function useColorTheme(): [ColorTheme, (upd: ColorSettings) => void] {
  Dome.useUpdate(NativeThemeUpdated);
  const { result: current } = Dome.usePromise(getNativeTheme());
  const [pref, setTheme] = Settings.useGlobalSettings(ColorThemeSettings);
  return [current ?? jColorTheme(pref), setTheme];
}

export function useColorThemeSettings(): State<ColorSettings> {
  const [pref, setTheme] = Settings.useGlobalSettings(ColorThemeSettings);
  return [jColorSettings(pref), setTheme];
}

export function useStyle(): CSSStyleDeclaration {
  const [theme, ] = useColorTheme();
  const style = React.useMemo(() => getComputedStyle(document.body),
    /** style is dependent on theme but it is not used directly */
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [theme]
  );
  return style;
}
/* -------------------------------------------------------------------------- */
