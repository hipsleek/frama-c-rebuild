/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import * as Ivette from 'ivette';

import GraphComponent from './graph';
import TreeComponent from './tree';

import { Pattern } from 'dome/text/markdown';
import Legend from './legend';
import doc from './doc.md?raw';

Ivette.registerGroup({
  id: 'fc.dive',
  label: 'Dive Plugin',
});

Ivette.registerComponent({
  id: 'fc.dive.graph',
  label: 'Dive Dataflow Graph',
  title: 'Data dependency graph according to an Eva analysis.',
  children: <GraphComponent />,
});

Ivette.registerComponent({
  id: 'fc.dive.tree',
  label: 'Dive Dataflow Tree',
  title: 'Data dependency tree according to an Eva analysis.',
  children: <TreeComponent />,
});

Ivette.registerView({
  id: 'fc.dive.dataflow',
  label: 'Dive Dataflow',
  layout: {
    A: 'fc.kernel.astview',
    B: 'fc.dive.graph',
    C: 'fc.kernel.properties',
    D: 'fc.kernel.locations',
  }
});

// --------------------------------------------------------------------------
// --- help
// --------------------------------------------------------------------------

const legendTag: Pattern = {
  pattern: /\[legend\]/g,
  replace: (key: number, match?: RegExpExecArray) => {
    return match ? <Legend key={key} /> : null;
  }
};

Ivette.registerDocChapter({ id: "dive", content: doc, patterns: [legendTag] });
