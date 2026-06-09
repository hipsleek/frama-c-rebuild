/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Electron main-process entry-point for Dome.
// --------------------------------------------------------------------------

/*
   Template of ./src/main/index.ts
   Imported from $(DOME)/template/main.ts

   The call to Dome.start() will initialize the Dome application
   and create the main window that will run the renderer process
   from `src/renderer/index.js`.

   You may add your own code to be run in the Electron main-process
   before or after the call to `Dome.start()`.
*/

import * as Dome from 'dome/main';
Dome.setName('Frama-C GUI');
Dome.start();
