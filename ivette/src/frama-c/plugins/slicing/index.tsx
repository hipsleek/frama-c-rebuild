/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import * as Dome from 'dome';
import * as Dialogs from 'dome/dialogs';
import * as Server from 'frama-c/server';
import * as ASTview from 'frama-c/kernel/ASTview';

import * as Ast from 'frama-c/kernel/api/ast';
import * as Slicing from 'frama-c/plugins/slicing/api/slicing';
import * as Eva from 'frama-c/plugins/eva/api/analysis';

import { evaNeeded } from '../eva/components/AnalysisStatus';

function showInfo(message: string, details: string): void {
  const buttons = [{ label: 'Ok' }];
  const kind = 'info';
  Dialogs.showMessageBox({ block: true, message, details, kind, buttons });
}

function showError(message: string, details: string): void {
  const buttons = [{ label: 'Cancel' }];
  const kind = 'error';
  Dialogs.showMessageBox({ block: true, message, details, kind, buttons });
}

function showSuccess(project: [string, number] | void): void {
  if (project) {
    const [name, id] = project;
    const details =
      `Sliced code has been generated in a new project `
      + `named '${name}' (id: ${id}).`;
    showInfo('Slicing successful', details);
  }
  else
    showError('Slicing failure', 'Unexpected error from the Slicing plug-in.');
}

type slicingRequest = Server.ExecRequest<Ast.marker, [string, number]>;

async function checkEvaStatus() : Promise<boolean> {
  const evaStatus = await Server.send(Eva.getComputationState, []);
  switch (evaStatus) {
    case 'not_computed':
      showInfo('Eva analysis required',
        'Please run an Eva analysis, for instance via the dedicated sidebar,'
        + ' before using the Slicing plug-in.');
      return false;
    case 'computing':
      showInfo('Eva analysis in progress',
        'Please wait for the Eva analysis to complete.');
      return false;
    case 'aborted': case 'computed':
      return true;
  }
}

async function callSlicing(
  slicingRequest: slicingRequest,
  marker: Ast.marker
): Promise<void> {
  const eva = await checkEvaStatus();
  if (eva)
    Server.send(slicingRequest, marker).then(showSuccess);
}

/* Builds the Slicing entries in the contextual menu about a statement. */
function buildMenu(
  menu: Dome.PopupMenuItem[],
  attr: Ast.markerAttributesData,
): void {

  const submenu: Dome.PopupMenuItem[] = [];

  function addItem(label: string, slicingRequest: slicingRequest): void {
    submenu.push({
      label,
      onClick: () => evaNeeded(() => callSlicing(slicingRequest, attr.marker))
    });
  }

  switch (attr.kind) {
    case 'STMT': case 'EXP': case 'LVAL': case 'LVAR': case 'LFUN':
      addItem('Slice effects of statement', Slicing.sliceStmt);
      addItem('Slice accessibility of statement', Slicing.sliceStmtCtrl);
      break;
  }
  switch (attr.kind) {
    case 'LFUN': case 'DFUN':
      addItem('Slice effects of function', Slicing.sliceCallsTo);
      addItem('Slice entrance into function', Slicing.sliceCallsInto);
      addItem('Slice returned value of function', Slicing.sliceResult);
      break;
    case 'LVAL': case 'LVAR':
      addItem('Slice lvalue', Slicing.sliceLval);
      addItem('Slice read accesses of lvalue', Slicing.sliceLvalReads);
      addItem('Slice write accesses of lvalue', Slicing.sliceLvalWrites);
      break;
  }

  if (submenu.length > 0) menu.push({ label: 'Slicing', submenu });
}

ASTview.registerMarkerMenuExtender(buildMenu);
