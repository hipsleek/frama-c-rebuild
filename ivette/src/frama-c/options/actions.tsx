/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import * as Forms from 'dome/layout/forms';
import { Hbox } from 'dome/layout/boxes';
import { Button } from 'dome/frame/toolbars';

// --------------------------------------------------------------------------
// --- Actions
// --------------------------------------------------------------------------

export function Remote({ remote, onChange }: {
    remote?: Forms.BufferController;
    onChange?: () => void;
  }
): React.JSX.Element | null {
  if(!remote) return null;

  const countErrors = remote.getErrors();
  remote.resetNotified();

  const syncFromFC = (): void => {
    remote.reset();
    onChange && onChange();
  };
  const syncToFC = (): void => {
    remote.commit();
    onChange && onChange();
  };

  return (
    <Hbox className='actions'>
      <Button
        label='Reset'
        icon="RELOAD"
        title="Reset form"
        disabled={!remote.hasReset()}
        onClick={syncFromFC}
        />
      <Button
        label='Apply'
        icon="PUSH"
        title={"Apply changes"
          +
          (countErrors > 0 ?
          " : "+String(countErrors)+" error(s) in the form" : ""
          )
        }
        kind={countErrors > 0 ? "warning" : "default"}
        disabled={!remote.hasCommit()}
        onClick={syncToFC}
      />
    </Hbox>
  );
}
