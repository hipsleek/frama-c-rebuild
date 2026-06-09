/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Eva Values
// --------------------------------------------------------------------------

import * as Ivette from 'ivette';
import doc from './doc.md?raw';
import './valuetable';
import './Summary';
import './Coverage';
import './DomainStates';
import './EvaSidebar';
import './Flamegraph';
import './MThread';
import './style.css';
import './Taint';

// --------------------------------------------------------------------------
// --- help
// --------------------------------------------------------------------------

Ivette.registerDocChapter({ id: "eva", content: doc });

// --------------------------------------------------------------------------
// --- Export Component
// --------------------------------------------------------------------------

Ivette.registerGroup({
  id: 'fc.eva',
  label: 'Eva Plugin'
});

Ivette.registerView({
  id: 'fc.eva.summary',
  label: 'Eva Summary',
  layout: {
    'A': 'fc.eva.summary',
    'B': 'fc.eva.coverage',
    'C': 'fc.kernel.messages',
    'D': 'fc.eva.flamegraph',
  },
});

Ivette.registerView({
  id: 'fc.eva.values',
  label: 'Eva Values',
  layout: {
    'A': 'fc.kernel.astview',
    'B': 'fc.kernel.astinfo',
    'CD': 'fc.eva.values',
  }
});

Ivette.registerView({
  id: 'fc.eva.mthread',
  label: 'Eva MThread',
  layout: {
    'A': 'fc.kernel.astview',
    'B': 'fc.kernel.locations',
    'CD': 'fc.eva.mthread' }
});


// --------------------------------------------------------------------------
