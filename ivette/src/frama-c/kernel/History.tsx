/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* --------------------------------------------------------------------------*/
/* --- Frama-C Selection History                                          ---*/
/* --------------------------------------------------------------------------*/

import React from 'react';
import * as Toolbar from 'dome/frame/toolbars';
import * as States from 'frama-c/states';

export default function History(): JSX.Element {
  const history = States.useHistory();
  const prev = history.prev[0]?.scope;
  const next = history.next[0]?.scope;
  const { label: prevLabel } = States.useDeclaration(prev);
  const { label: nextLabel } = States.useDeclaration(next);
  const prevTitle = prevLabel || 'Previous location';
  const nextTitle = nextLabel || 'Next location';
  return (
    <Toolbar.ButtonGroup>
      <Toolbar.Button
        icon="ANGLE.LEFT"
        onClick={States.gotoPrev}
        enabled={history.prev.length > 0}
        title={prevTitle}
      />
      <Toolbar.Button
        icon="ANGLE.RIGHT"
        onClick={States.gotoNext}
        enabled={history.next.length > 0}
        title={nextTitle}
      />
    </Toolbar.ButtonGroup>
  );
}

/* --------------------------------------------------------------------------*/
