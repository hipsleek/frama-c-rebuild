/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* --------------------------------------------------------------------------*/
/* --- Frama-C MENU                                                       ---*/
/* --------------------------------------------------------------------------*/

import * as Dome from 'dome';
import * as Dialogs from 'dome/dialogs';
import { showHelp } from 'dome/help';
import * as Display from 'ivette/display';
import * as Server from 'frama-c/server';
import * as Services from 'frama-c/kernel/api/services';
import * as Project from 'frama-c/kernel/api/project';
import * as Ast from 'frama-c/kernel/api/ast';
import * as States from 'frama-c/states';
import * as Projects from 'frama-c/kernel/Projects';
import { showAboutModal, showCreditsModal } from './help';
import { showOptionsModal } from './options';

const cFilter = {
  name: 'C source files',
  extensions: ['c', 'i', 'h'],
};
const allFilter = {
  name: 'all',
  extensions: ['*'],
};

async function parseFiles(files: string[]): Promise<void> {
  await Server.send(Ast.setFiles, files);
  await Server.send(Ast.compute, {});
  Display.showMessage('Source files parsed.');
  return;
}

async function setFiles(): Promise<void> {
  const files = await Dialogs.showOpenFiles({
    title: 'Select C source files',
    filters: [cFilter, allFilter],
  });
  if (files) {
    await parseFiles(files);
    States.clearHistory();
  }
  return;
}

async function addFiles(): Promise<void> {
  const dialog = Dialogs.showOpenFiles({
    title: 'Add C source files',
    filters: [cFilter, allFilter],
  });
  const request = Server.send(Ast.getFiles, {});
  const [oldFiles, newFiles] = await Promise.all([request, dialog]);
  if (newFiles) {
    const files = oldFiles ? oldFiles.concat(newFiles) : newFiles;
    parseFiles(files);
  }
  return;
}

async function reparseFiles(): Promise<void> {
  const files = await Server.send(Ast.getFiles, {});
  if (files) {
    await Server.send(Ast.setFiles, []);
    parseFiles(files);
  }
  return;
}

async function loadSession(): Promise<void> {
  const file = await Dialogs.showOpenFile({ title: 'Load a saved session' });
  const error = await Server.send(Services.load, file);
  States.clearHistory();
  if (error) {
    await Dialogs.showMessageBox({
      message: 'An error has occurred when loading the file',
      details: `File: ${file}\nError: ${error}`,
      kind: 'error',
      buttons: [{ label: 'Cancel' }],
    });
  }
  return;
}

async function saveSession(): Promise<void> {
  const title = 'Save the current session';
  const file = await Dialogs.showSaveFile({ title });
  const error = await Server.send(Services.save, file);
  if (error) {
    await Dialogs.showMessageBox({
      message: 'An error has occurred when saving the session',
      kind: 'error',
      buttons: [{ label: 'Cancel' }],
    });
  }
  return;
}

function addFileMenuItems(): void {
  Dome.addMenuItem({
    menu: 'File',
    label: 'Set source files…',
    id: 'file_set',
    onClick: setFiles,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'File',
    label: 'Add source files…',
    id: 'file_add',
    onClick: addFiles,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'File',
    label: 'Reparse',
    id: 'file_reparse',
    onClick: reparseFiles,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'File',
    id: 'file_separator',
    kind: 'separator',
  });
  Dome.addMenuItem({
    menu: 'File',
    label: 'Load session…',
    id: 'file_load',
    onClick: loadSession,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'File',
    label: 'Save session…',
    id: 'file_save',
    onClick: saveSession,
    kind: 'normal',
  });
}

function addHelpMenuItems(): void {
  Dome.addMenuItem({
    menu: 'Help',
    label: 'Documentation',
    id: 'help_documentation',
    key: 'Cmd+H',
    onClick: showHelp,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'Help',
    id: 'help_separator',
    kind: 'separator',
  });
  Dome.addMenuItem({
    menu: 'Help',
    label: 'About',
    id: 'help_about',
    onClick: showAboutModal,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'Help',
    label: 'Credits',
    id: 'help_credits',
    onClick: showCreditsModal,
    kind: 'normal',
  });
}

function addEditMenuItems(): void {
  Dome.addMenuItem({
    menu: 'Edit',
    label: 'Frama-C Parameters',
    id: 'frama_c_options',
    onClick: showOptionsModal,
    kind: 'normal',
    key: 'Cmd+P'
  });
  Dome.addMenuItem({
    menu: 'Edit',
    id: 'edit_params_separator',
    kind: 'separator',
  });
}

async function duplicateCurrentProject(): Promise<void> {
  const current = await Server.send(Project.getCurrent, null);
  Projects.duplicateProject(current);
}

async function deleteCurrentProject(): Promise<void> {
  const current = await Server.send(Project.getCurrent, null);
  Projects.removeProject(current);
}

async function renameCurrentProject(): Promise<void> {
  const current = await Server.send(Project.getCurrent, null);
  Projects.renameProject(current);
}

export function addProjectSubMenu(others?: Dome.MenuItemProps[]): void {
  Dome.addMenuItem({
    menu: 'Project',
    label: 'New project',
    id: 'project_new',
    onClick: Projects.newProject,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'Project',
    label: 'Load project',
    id: 'project_load',
    onClick: Projects.loadProject,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'Project',
    label: 'Duplicate current project',
    id: 'project_duplicate_current',
    onClick: duplicateCurrentProject,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'Project',
    label: 'Delete current project',
    id: 'project_delete_current',
    onClick: deleteCurrentProject,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'Project',
    label: 'Rename current project',
    id: 'project_rename_current',
    onClick: renameCurrentProject,
    kind: 'normal',
  });
  Dome.addMenuItem({
    menu: 'Project',
    id: 'project_separator',
    kind: 'separator'
  });
  others?.forEach(e => Dome.addMenuItem(e));
}

export function init(): void {
  addFileMenuItems();
  addHelpMenuItems();
  addEditMenuItems();
  Dome.addMenu('Project');
  addProjectSubMenu();
}

/* --------------------------------------------------------------------------*/
