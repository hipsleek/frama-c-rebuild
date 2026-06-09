/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* eslint-disable no-console */

/**
   ## Dome Application (Main Process)

   This module manages the electron main-process of the application.
   Your application will launch on `Dome.start()`:

   @example
   // src/main/index.js:
   import * as Dome from 'dome' ;
   Dome.start();

   @packageDocumentation
   @module dome(main)
*/

import _ from 'lodash';
import fs from 'fs';
import path from 'path';
import {
  BrowserWindow,
  BrowserWindowConstructorOptions,
  IpcMainEvent,
  Rectangle,
  app,
  dialog,
  ipcMain,
  nativeTheme,
  nativeImage,
  shell,
} from 'electron';
import installExtension from 'electron-devtools-installer';
import { REACT_DEVELOPER_TOOLS } from 'electron-devtools-installer';
import SYS, * as System from 'dome/system';

// --------------------------------------------------------------------------
// --- Main Window Web Navigation
// --------------------------------------------------------------------------

import * as Menubar from './menubar';

// --------------------------------------------------------------------------
// --- System Helpers
// --------------------------------------------------------------------------

function fstat(p: string): fs.Stats | undefined {
  try {
    return fs.statSync(p);
  } catch (_error) {
    return undefined;
  }
}

// --------------------------------------------------------------------------
// --- Helpers
// --------------------------------------------------------------------------

const LOCAL = process.env.DOME_LOCAL;

/** Development mode flag */
export const { DEVEL } = System;

/** System platform */
export const { platform } = System;

// --------------------------------------------------------------------------
// --- Native Theme
// --------------------------------------------------------------------------

ipcMain.handle('dome.ipc.theme', () => {
  return nativeTheme.shouldUseDarkColors ? 'dark' : 'light';
});

nativeTheme.on('updated', () => {
  broadcast('dome.theme.updated');
});

function setNativeTheme(theme: string | undefined): void {
  switch (theme) {
    case 'dark':
    case 'light':
    case 'system':
      nativeTheme.themeSource = theme;
      return;
    default:
      console.warn('[dome] unknown theme', theme);
  }
}

// --------------------------------------------------------------------------
// --- Settings
// --------------------------------------------------------------------------

type json =
  | undefined | null | boolean | number | string
  | { [key: string]: json };
type Store = { [key: string]: json };
type Frame = { x: number, y: number, width: number, height: number };

function loadSettings(file: string): Store {
  try {
    if (!fstat(file))
      return {};
    const text = fs.readFileSync(file, { encoding: 'utf8' });
    return { ...JSON.parse(text) };
  } catch (err) {
    console.error('[Dome] Unable to load settings', file, err);
    return {};
  }
}

function saveSettings(file: string, data: Store = {}): void {
  try {
    const text = JSON.stringify(data, undefined, DEVEL ? 2 : 0);
    fs.writeFileSync(file, text, { encoding: 'utf8' });
  } catch (err) {
    console.error('[Dome] Unable to save settings', file, err);
  }
}

// --------------------------------------------------------------------------
// --- Global Settings
// --------------------------------------------------------------------------

let GlobalSettings = {}; // Current Dictionary

const APP_DIR = app.getPath('userData');
const PATH_WINDOW_SETTINGS = path.join(APP_DIR, 'WindowSettings.json');
const PATH_GLOBAL_SETTINGS = path.join(APP_DIR, 'GlobalSettings.json');

const CLI_OPTION_SETTINGS = {
  name: "--settings",
  defaultValue: "DEFAULT",
} as const;

function saveGlobalSettings(): void {
  try {
    if (!fstat(APP_DIR)) fs.mkdirSync(APP_DIR);
    saveSettings(PATH_GLOBAL_SETTINGS, GlobalSettings);
  } catch (err) {
    console.error('[Dome] Unable to save global settings', err);
  }
}

function obtainGlobalSettings(): Store {
  if (_.isEmpty(GlobalSettings)) {
    GlobalSettings = loadSettings(PATH_GLOBAL_SETTINGS);
  }
  return GlobalSettings;
}

// --------------------------------------------------------------------------
// --- Window Settings & Frames
// --------------------------------------------------------------------------

interface Handle {
  primary: boolean; // Primary window
  window: BrowserWindow; // Also prevents Gc
  frame: Electron.Rectangle | undefined; // Window frame
  devtools: boolean; // Developer tools visible
  reloaded: boolean; // Reloaded window
  config: string; // Path to config file
  settings: Store; // Current settings
  storage: Store; // Local storage
}

const WindowHandles = new Map<number, Handle>(); // Indexed by *webContents* id

function jInt(v: json): number {
  return _.toSafeInteger(v);
}

function jFrame(obj: json | Rectangle): Frame | undefined {
  if (obj && typeof (obj) === 'object')
    return {
      x: jInt(obj.x),
      y: jInt(obj.y),
      width: jInt(obj.width),
      height: jInt(obj.height),
    };
  return undefined;
}

function jStore(obj: json): Store {
  return obj !== null && typeof (obj) === 'object' ? obj : {};
}

function saveWindowConfig(handle: Handle): void {
  const frame = jFrame(handle.frame);
  const configData: Store = {
    frame,
    settings: handle.settings,
    storage: handle.storage,
    devtools: handle.devtools,
  };

  if (process.argv.indexOf(CLI_OPTION_SETTINGS.name) === -1) {
    saveSettings(handle.config, configData);
  }
}

function windowSyncSettings(event: IpcMainEvent): void {
  const handle = WindowHandles.get(event.sender.id);
  event.returnValue = {
    globals: obtainGlobalSettings(),
    storage: handle && handle.storage,
    settings: handle && handle.settings,
  };
}

ipcMain.on('dome.ipc.settings.sync', windowSyncSettings);

function applyThemeSettings(settings: Store): void {
  const theme = settings['dome-color-theme'];
  if (typeof (theme) === 'string') setNativeTheme(theme);
}

// --------------------------------------------------------------------------
// --- Patching Settings
// --------------------------------------------------------------------------

type Patch = { key: string; value: json };

function applyPatches(data: Store, args: Patch[]): void {
  args.forEach(({ key, value }) => {
    if (value === null) {
      delete data[key];
    } else {
      data[key] = value;
    }
  });
}

function applyWindowSettings(event: IpcMainEvent, args: Patch[]): void {
  const handle = WindowHandles.get(event.sender.id);
  if (handle) {
    applyPatches(handle.settings, args);
    if (DEVEL) saveWindowConfig(handle);
  }
}

function applyStorageSettings(event: IpcMainEvent, args: Patch[]): void {
  const handle = WindowHandles.get(event.sender.id);
  if (handle) {
    applyPatches(handle.storage, args);
    if (DEVEL) saveWindowConfig(handle);
  }
}

function applyGlobalSettings(event: IpcMainEvent, args: Patch[]): void {
  const settings: Store = obtainGlobalSettings();
  applyPatches(settings, args);
  applyThemeSettings(settings);
  BrowserWindow.getAllWindows().forEach((w: BrowserWindow) => {
    const contents = w.webContents;
    if (contents.id !== event.sender.id) {
      contents.send('dome.ipc.settings.broadcast', args);
    }
  });
  if (DEVEL) saveGlobalSettings();
}

ipcMain.on('dome.ipc.settings.window', applyWindowSettings);
ipcMain.on('dome.ipc.settings.global', applyGlobalSettings);
ipcMain.on('dome.ipc.settings.storage', applyStorageSettings);

// --------------------------------------------------------------------------
// --- Renderer-Process Communication
// --------------------------------------------------------------------------

function broadcast(event: string, ...args: unknown[]): void {
  BrowserWindow.getAllWindows().forEach((w) => {
    w.webContents.send(event, ...args);
  });
}

// --------------------------------------------------------------------------
// --- Window Activities
// --------------------------------------------------------------------------

let appName = 'Dome';
const MODIFIED = '(*) ';

/**
   Sets application window name
 */
export function setName(title: string): void {
  appName = title;
}

function setTitle(event: IpcMainEvent, title: string): void {
  const handle = WindowHandles.get(event.sender.id);
  if (handle) handle.window.setTitle(title || appName);
}

function setModified(event: IpcMainEvent, modified: boolean): void {
  const handle = WindowHandles.get(event.sender.id);
  if (handle) {
    const w = handle.window;
    if (platform === 'macos')
      w.setDocumentEdited(modified);
    else {
      let title = w.getTitle();
      if (title.startsWith(MODIFIED))
        title = title.substring(MODIFIED.length);
      if (modified)
        title = MODIFIED + title;
      w.setTitle(title);
    }
  }
}

ipcMain.on('dome.ipc.window.title', setTitle);
ipcMain.on('dome.ipc.window.modified', setModified);

function getURL(): string {
  if (DEVEL && process.env['ELECTRON_RENDERER_URL']) {
    const url = process.env['ELECTRON_RENDERER_URL'];
    console.log('[Dome] DEVEL - Loading URL', url);
    return url;
  }
  if (LOCAL) {
    const url = `file://${path.join(__dirname, '../renderer/index.html')}`;
    console.log('[Dome] LOCAL - Loading URL', url);
    return url;
  }
  const url = `file://${path.join(__dirname, '../renderer/index.html')}`;
  console.log('[Dome] PROD - Loading URL', url);
  return url;
}

function navigateURL(sender: Electron.WebContents) {
  return (event: Electron.Event, url: string) => {
    event.preventDefault();
    const href = new URL(url);
    const main = new URL(getURL());
    if (href.origin === main.origin) {
      sender.send('dome.ipc.href', url);
    } else {
      shell.openExternal(url);
    }
  };
}

// --------------------------------------------------------------------------
// --- Lookup for config file
// --------------------------------------------------------------------------

function lookupConfig(pwd = '.'): string {
  const wdir = path.resolve(pwd);
  let cwd = wdir;
  const cfg = `.${appName.toLowerCase().replace(' ', '-')}.local`;
  for (; ;) {
    const here = path.join(cwd, cfg);
    if (fstat(here)) return here;
    const up = path.dirname(cwd);
    if (up === cwd) break;
    cwd = up;
  }
  const home = path.resolve(app.getPath('home'));
  const user = wdir.startsWith(home) ? wdir : home;
  return path.join(user, cfg);
}

// --------------------------------------------------------------------------
// --- Browser Window SetUp
// --------------------------------------------------------------------------

function createBrowserWindow(
  primary: boolean,
  config: BrowserWindowConstructorOptions,
  argv?: string[],
  wdir?: string,
): BrowserWindow {

  const isAppWindow = argv !== undefined && wdir !== undefined;
  const browserArguments = isAppWindow
    ? SYS.WINDOW_APPLICATION_ARGV
    : SYS.WINDOW_PREFERENCES_ARGV;
  console.log('[Dome] Browser Arguments', browserArguments);

  const iconPath = path.join(__dirname, '../../static/icon.png');
  const appIcon = nativeImage.createFromPath(iconPath);

  const options: BrowserWindowConstructorOptions = {
    width: 900,
    height: 670,
    show: false,
    backgroundColor: '#f0f0f0',
    icon: appIcon,
    webPreferences: {
      contextIsolation: false,
      nodeIntegration: true,
      sandbox: false,
      additionalArguments: [browserArguments],
      preload: path.join(__dirname, '../preload/index.js'),
    },
    ...config,
  };

  let configFile = PATH_WINDOW_SETTINGS;
  if (argv && argv.indexOf(CLI_OPTION_SETTINGS.name) >= 0) {
    const settingsIdx = argv.indexOf(CLI_OPTION_SETTINGS.name);
    const settings = argv[settingsIdx + 1];
    if (settings !== CLI_OPTION_SETTINGS.defaultValue) {
      configFile = argv[settingsIdx + 1];
    }
    argv = argv.slice(0, settingsIdx).concat(argv.slice(settingsIdx + 2));
  } else if (isAppWindow) {
    configFile = lookupConfig(wdir);
  }

  console.log('[Dome] Loading config file', configFile);
  const configData = loadSettings(configFile);

  const frame = jFrame(configData.frame);
  const settings = jStore(configData.settings);
  const storage = jStore(configData.storage);
  const devtools = !!configData.devtools;

  if (frame) {
    options.x = frame.x;
    options.y = frame.y;
    options.width = frame.width;
    options.height = frame.height;
  }

  const theWindow = new BrowserWindow(options);
  const { webContents } = theWindow;
  const wid = webContents.id;

  const handle: Handle = {
    primary,
    window: theWindow,
    config: configFile,
    reloaded: false,
    frame,
    settings,
    storage,
    devtools,
  };

  // Keep the window reference (prevent garbage collection)
  WindowHandles.set(wid, handle);

  // Emitted when the window is closed.
  theWindow.on('closed', () => {
    saveWindowConfig(handle);
    // Dereference the window object (allow garbage collection)
    WindowHandles.delete(wid);
  });

  // Disable security warnings (unless build)
  if (DEVEL || LOCAL)
    process.env.ELECTRON_DISABLE_SECURITY_WARNINGS = 'true';

  // Load Finished
  const showWindow = () : void => {
    console.log('[Dome] Window ready');
    if (DEVEL || LOCAL)
      process.env.ELECTRON_DISABLE_SECURITY_WARNINGS = 'false';
    if (DEVEL && devtools) webContents.openDevTools();
    theWindow.show();
  };

  // `ready-to-show` event doesn't always fire on wayland; use `did-finish-load`
  // event on the web content instead, as it is similar enough.
  // Cf https://github.com/electron/electron/issues/48859
  if (process.platform === 'linux'
      && app.commandLine.getSwitchValue('ozone-platform') === 'wayland') {
    theWindow.webContents.once('did-finish-load', showWindow);
  } else {
    theWindow.once('ready-to-show', showWindow);
  }

  // Focus Management
  theWindow.on('focus', () => webContents.send('dome.ipc.focus', true));
  theWindow.on('blur', () => webContents.send('dome.ipc.focus', false));

  // URL Navigation
  webContents.on('will-navigate', navigateURL(webContents));
  webContents.on('did-navigate-in-page', navigateURL(webContents));

  // Application Startup
  webContents.on('did-finish-load', () => {
    if (!handle.reloaded) {
      handle.reloaded = true;
    } else {
      broadcast('dome.ipc.reload');
    }
    webContents.send('dome.ipc.command', argv, wdir);
  });

  // Emitted when the window want's to close.
  const closeHandler = function (event: Electron.Event): void {
    // Do not call this handler in a cycle; the next close event will forcibly
    // close the window
    theWindow.off('close', closeHandler);
    // Do not close the window yet
    event.preventDefault();
    // Save state
    handle.frame = theWindow.getBounds();
    handle.devtools = webContents.isDevToolsOpened();
    // Start closing process
    webContents.send('dome.ipc.closing', wid);
  };

  theWindow.on('close', closeHandler);

  // Keep track of frame positions (in DEVEL)
  if (DEVEL) {
    const saveFrame = _.debounce(() => {
      handle.frame = theWindow.getBounds();
      handle.devtools = webContents.isDevToolsOpened();
      saveWindowConfig(handle);
    }, 300);
    theWindow.on('resize', saveFrame);
    theWindow.on('moved', saveFrame);
  }

  theWindow.loadURL(getURL()).catch(err =>
    console.error("Cannot load window URL", err)
  );


  return theWindow;
}

ipcMain.on('dome.ipc.closing.done', (_event, wid: number) => {
  const handle = WindowHandles.get(wid);
  if (handle !== undefined) handle.window.close();
});

// --------------------------------------------------------------------------
// --- Application Window(s) & Command Line
// --------------------------------------------------------------------------

interface Cmd { wdir: string; argv: string[] }

function stripElectronArgv(cmd: Cmd): Cmd {
  const devel = import.meta.env.MODE === "development";
  const wdir = devel ? cmd.argv[2] : cmd.wdir;
  let slice = 3;
  if (!LOCAL && !devel) {
    slice = 1;
  } else if (LOCAL) {
    slice = 2;
  }
  const argv = cmd.argv
    .slice(slice)
    .filter((p) => !!p && p !== "--no-sandbox");
  return { wdir, argv };
}

function createPrimaryWindow(): void {
  // Initialize Menubar
  Menubar.install();

  const cwd = process.cwd();
  const wdir = cwd === '/' ? app.getPath('home') : cwd;
  const cmd = stripElectronArgv({ wdir, argv: process.argv });

  // Reset Settings if the associated argument is provided
  const settingsIdx = cmd.argv.indexOf(CLI_OPTION_SETTINGS.name);
  if (settingsIdx >= 0) {
    const settings = cmd.argv[settingsIdx + 1];
    if (settings === CLI_OPTION_SETTINGS.defaultValue) {
      restoreAllDefaultSettings();
    }
  }

  // Initialize Theme
  const globals = obtainGlobalSettings();
  applyThemeSettings(globals);

  // Create Window
  createBrowserWindow(true, { title: appName }, cmd.argv, cmd.wdir);
}

let appCount = 1;

function createSecondaryWindow(
  _event: Electron.Event,
  chromiumArgv: string[],
  wdir: string,
): void {
  const argStart = "--second-instance=";
  let argString = chromiumArgv.find(a => a.startsWith(argStart));
  if (argString) {
    argString = argString.substring(argStart.length);
    const electronArgv = JSON.parse(argString);
    const cmd = stripElectronArgv({ wdir, argv: electronArgv });
    const title = `${appName} #${++appCount}`;
    createBrowserWindow(false, { title }, cmd.argv, cmd.wdir);
  }
}

function createDesktopWindow(): void {
  const wdir = app.getPath('home');
  const title = `${appName} #${++appCount}`;
  createBrowserWindow(false, { title }, [], wdir);
}

// --------------------------------------------------------------------------
// --- Activate Windows (macOS)
// --------------------------------------------------------------------------

function activateWindows(): void {
  let isFocused = false;
  let toFocus: BrowserWindow | undefined;
  BrowserWindow.getAllWindows().forEach((w) => {
    w.show();
    if (w.isFocused()) isFocused = true;
    else if (!toFocus) toFocus = w;
  });
  if (!isFocused) {
    if (toFocus) toFocus.focus();
    else {
      // No focusable nor focused window
      createDesktopWindow();
    }
  }
}

// --------------------------------------------------------------------------
// --- Settings Window
// --------------------------------------------------------------------------

let PreferenceWindow: BrowserWindow | undefined;

function showSettingsWindow(): void {
  if (!PreferenceWindow)
    PreferenceWindow = createBrowserWindow(
      false, {
      title: `${appName} Preferences`,
      width: 375,
      height: 270,
      fullscreen: false,
      maximizable: false,
      minimizable: false,
    });
  PreferenceWindow.setMenuBarVisibility(false);
  PreferenceWindow.show();
  PreferenceWindow.on('closed', () => { PreferenceWindow = undefined; });
}

function restoreDefaultSettings(): void {
  GlobalSettings = {};
  nativeTheme.themeSource = 'system';
  if (DEVEL) saveGlobalSettings();

  WindowHandles.forEach((handle) => {
    // Keep frame for user comfort
    handle.settings = {};
    handle.devtools = handle.window.webContents.isDevToolsOpened();
    if (DEVEL) saveWindowConfig(handle);
  });

  broadcast('dome.ipc.settings.defaults');
}

/**
 * Resets the Global setting file and delete the Window setting file
 * (which will be recreated in a default state when needed)
 */
function restoreAllDefaultSettings(): void {
  GlobalSettings = {};
  nativeTheme.themeSource = 'system';
  saveGlobalSettings();
  try {
    if (fs.existsSync(PATH_WINDOW_SETTINGS)) {
      fs.rmSync(PATH_WINDOW_SETTINGS);
    }
  } catch (error) {
    console.warn(error);
  }
}

ipcMain.on('dome.menu.settings', showSettingsWindow);
ipcMain.on('dome.menu.defaults', restoreDefaultSettings);
ipcMain.on('dome.app.paths', (event) => {
  event.returnValue = {
    'home': app.getPath('home'),
    'desktop': app.getPath('desktop'),
    'documents': app.getPath('documents'),
    'downloads': app.getPath('downloads'),
    'temp': app.getPath('temp'),
  };
});

// --------------------------------------------------------------------------
// --- Main Application Starter
// --------------------------------------------------------------------------

let isQuitting = false;

/** Starts the main process. */
export function start(): void {

  app.on(
    'certificate-error',
    (event, _webContents, _url, _error, _certificate, callback) => {
      event.preventDefault();
      callback(true);
    }
  );

  // Workaround to recover the original commandline of a second instance
  // after chromium messes with the argument order.
  // See https://github.com/electron/electron/issues/20322 for more details.
  app.commandLine.appendSwitch('second-instance', JSON.stringify(process.argv));

  // Ensures second instance triggers the main one
  if (!app.requestSingleInstanceLock()) app.quit();

  // Change default locale
  app.commandLine.appendSwitch('lang', 'en');

  app.on('ready', createPrimaryWindow); // Wait for Electron init
  app.on('activate', activateWindows); // Mac OSX response to dock
  app.on('second-instance', createSecondaryWindow);

  // Listen to application events
  app.whenReady().then(() => {
    if (DEVEL) {
      installExtension(REACT_DEVELOPER_TOOLS)
        .then((name) => console.log(`[Dome] Added Extension:  ${name}`))
        .catch((err) => console.warn('[Dome] Extension error: ', err));
    }
  });

  // Configuring macOS for exiting
  app.on('before-quit', () => {
    isQuitting = true;
  });

  // At-exit callbacks
  app.on('will-quit', () => {
    saveGlobalSettings();
    System.doExit();
  });

  // On macOS the menu bar stays active until the user explicitly quits.
  // On other systems, automatically quit when all windows are closed.
  // Warning: when no event handler is registered, the app automatically
  // quit when all windows are closed.
  app.on('window-all-closed', () => {
    if (isQuitting || System.platform !== 'macos') app.quit();
  });

}

// --------------------------------------------------------------------------
// --- MenuBar Management
// --------------------------------------------------------------------------

/**
   Define a custom main window menu.
*/
export function addMenu(label: string): void {
  Menubar.addMenu(label);
}

/**
   Define a custom menu item.
*/
export function addMenuItem(spec: Menubar.CustomMenuItemSpec): void {
  Menubar.addMenuItem(spec);
}

/**
   Update a menu item.
*/
export function setMenuItem(spec: Menubar.CustomMenuItem): void {
  Menubar.setMenuItem(spec);
}

function isPrimary(evt: IpcMainEvent): boolean {
  const h = WindowHandles.get(evt.sender.id);
  return h ? h.primary : false;
}

ipcMain.on('dome.ipc.menu.addmenu', (evt, label) =>
  isPrimary(evt) && Menubar.addMenu(label)
);
ipcMain.on('dome.ipc.menu.delsubmenu', (evt, label) =>
  isPrimary(evt) && Menubar.delSubMenu(label)
);
ipcMain.on('dome.ipc.menu.addmenuitem', (evt, spec) =>
  isPrimary(evt) && Menubar.addMenuItem(spec)
);
ipcMain.on('dome.ipc.menu.setmenuitem', (_evt, spec) =>
  // Always update menu items
  Menubar.setMenuItem(spec)
);

// --------------------------------------------------------------------------
// --- Dialogs Management
// --------------------------------------------------------------------------

ipcMain.handle(
  'dome.dialog.showMessageBox',
  (_evt, props) => dialog.showMessageBox(props),
);

ipcMain.handle(
  'dome.dialog.showMessageBoxSync',
  (evt, props) => {
    const browser = BrowserWindow.fromId(evt.sender.id);
    if(browser) return dialog.showMessageBoxSync(browser, props);
    else return dialog.showMessageBoxSync(props);
  }
);

ipcMain.handle(
  'dome.dialog.showOpenDialog',
  (_evt, props) => dialog.showOpenDialog(props),
);

ipcMain.handle(
  'dome.dialog.showSaveDialog',
  (_evt, props) => dialog.showSaveDialog(props),
);

// --------------------------------------------------------------------------
