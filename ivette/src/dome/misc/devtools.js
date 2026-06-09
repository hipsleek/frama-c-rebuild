/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Dummy clone of electron-dev-installer
// --------------------------------------------------------------------------

// No need to export dummy identifiers (undefined is ok)

export const REACT_DEVELOPER_TOOLS = undefined ;

// Shall not be used in non-development mode
export default function installExtension(_id,_force) {
  return Promise.resolve();
}
