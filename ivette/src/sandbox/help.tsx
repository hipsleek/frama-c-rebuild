/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */


import React from 'react';
import { registerSandbox } from 'ivette';
import { HelpButton } from 'dome/help';

/* -------------------------------------------------------------------------- */
/* --- Sandbox help                                                       --- */
/* -------------------------------------------------------------------------- */

function SandboxHelp(): JSX.Element {
  const style = {
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    width: '100%',
    height: '100%',
    fontSize: '1.5em'
  };

  return (
    <>
      <div style={style}>
        <>
          Click the help button to display help : here
          <HelpButton id='sandbox' size={18} />
          or on the toolbar
        </>
      </div>
    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.help',
  label: 'Help',
  preferredPosition: 'ABCD',
  help: 'sandbox',
  children: <SandboxHelp />,
});

// --------------------------------------------------------------------------
