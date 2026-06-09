/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Frama-C Registry                                                   --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import * as Ivette from 'ivette';

import History from 'frama-c/kernel/History';
import { GlobalDeclarations, GlobalByFiles } from 'frama-c/kernel/Globals';
import { Projects } from  'frama-c/kernel/Projects';
import ASTview from 'frama-c/kernel/ASTview';
import ASTinfo from 'frama-c/kernel/ASTinfo';
import SourceCode from 'frama-c/kernel/SourceCode';
import PivotTable from 'frama-c/kernel/PivotTable';
import Locations from 'frama-c/kernel/Locations';
import Properties from 'frama-c/kernel/Properties';
import { RecordingLogs, ServerLogs } from 'frama-c/kernel/ServerLogs';
import { ButtonOptions } from 'frama-c/options';

import 'frama-c/kernel/style.css';

import * as Menu from 'frama-c/menu';

Menu.init();

/* -------------------------------------------------------------------------- */
/* --- Frama-C Tools                                                      --- */
/* -------------------------------------------------------------------------- */

Ivette.registerSidebar({
  id: 'fc.kernel.globals',
  label: 'AST',
  icon: 'DUPLICATE',
  title: 'Global Declarations (AST)',
  children: <GlobalDeclarations />
});

Ivette.registerSidebar({
  id: 'fc.kernel.files',
  label: 'Files',
  icon: 'FOLDER',
  title: 'Files',
  children: <GlobalByFiles />
});

Ivette.registerSidebar({
  id: 'fc.kernel.projects',
  label: 'Projects',
  icon: 'PROJECT',
  title: 'Projects',
  children: <Projects />
});

Ivette.registerToolbar({
  id: 'ivette.parameters',
  children: <ButtonOptions />
});

Ivette.registerToolbar({
  id: 'ivette.history',
  children: <History />
});

Ivette.registerStatusbar({
  id: 'fc.kernel.recordinglogs',
  children: <RecordingLogs />
});

/* -------------------------------------------------------------------------- */
/* --- Frama-C Kernel Groups                                              --- */
/* -------------------------------------------------------------------------- */

Ivette.registerGroup({
  id: 'fc.kernel',
  label: 'Frama-C',
});

Ivette.registerComponent({
  id: 'fc.server.serverlogs',
  label: 'Server Logs',
  title: 'Frama-C server output logs',
  children: <ServerLogs />
});

Ivette.registerComponent({
  id: 'fc.kernel.astinfo',
  label: 'Inspector',
  title: 'Contextual information on selected AST elements',
  children: <ASTinfo />
});

Ivette.registerComponent({
  id: 'fc.kernel.astview',
  label: 'AST',
  title: 'Normalized C/ACSL Source Code',
  children: <ASTview />,
});

Ivette.registerComponent({
  id: 'fc.kernel.sourcecode',
  label: 'Source Code',
  title: 'C/ACSL Source Code',
  children: <SourceCode />,
});

Ivette.registerComponent({
  id: 'fc.kernel.locations',
  label: 'Locations',
  title: 'Selected list of locations',
  children: <Locations />,
});

Ivette.registerComponent({
  id: 'fc.kernel.properties',
  label: 'Properties',
  title: 'Status of ACSL Properties',
  children: <Properties />,
});

Ivette.registerComponent({
  id: 'fc.kernel.pivottable',
  label: 'Pivot Table',
  title: 'Pivot Table',
  children: <PivotTable />,
});

/* -------------------------------------------------------------------------- */
/* --- Frama-C Views                                                      --- */
/* -------------------------------------------------------------------------- */

Ivette.registerView({
  id: 'fc.kernel.source',
  label: 'Source Code',
  defaultView: true,
  layout: {
    A: 'fc.kernel.astview',
    B: 'fc.kernel.sourcecode',
    CD: 'fc.kernel.astinfo',
  }
});

Ivette.registerView({
  id: 'fc.kernel.properties',
  label: 'Properties',
  layout: {
    A: 'fc.kernel.astview',
    B: 'fc.kernel.sourcecode',
    CD: 'fc.kernel.properties',
  }
});

Ivette.registerView({
  id: 'fc.kernel.pivot-table',
  label: 'Pivot Table',
  layout: { 'ABCD': 'fc.kernel.pivottable' },
});

/* -------------------------------------------------------------------------- */
