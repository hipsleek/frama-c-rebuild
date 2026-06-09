/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* Currently Cytoscape.use emits an error when a library is already loaded.
This prevents Hot Module Reloading for modules where Cytescope.use is used.
Grouping all Cytoscape plugins registrations here solves the problem. */

import Cytoscape from 'cytoscape';

import CxtMenu from 'cytoscape-cxtmenu';
import Popper from 'cytoscape-popper';

// Layouts
import Dagre from 'cytoscape-dagre';
import Cola from 'cytoscape-cola';
import CoseBilkent from 'cytoscape-cose-bilkent';
import Klay from 'cytoscape-klay';

Cytoscape.use(Popper);
Cytoscape.use(CxtMenu);

Cytoscape.use(Dagre);
Cytoscape.use(Cola);
Cytoscape.use(CoseBilkent);
Cytoscape.use(Klay);
