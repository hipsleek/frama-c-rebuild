/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { IconButton } from 'dome/controls/buttons';
import { Hbox } from 'dome/layout/boxes';
import * as Forms from 'dome/layout/forms';
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Eva from 'frama-c/plugins/eva/api/analysis';
import { EvaStatus } from 'frama-c/plugins/eva/components/AnalysisStatus';
import { SidebarTitle } from 'dome/frame/sidebars';
import { HelpButton } from 'dome/help';

export let evaComputationValue:
  Eva.computationStateType | undefined = undefined;

export interface EvaToolsProps {
  remote: Forms.BufferController;
  iconSize: number;
}

export default function EvaTools(
  props: EvaToolsProps
): JSX.Element {
  const { remote, iconSize } = props;

  const evaComputed = States.useSyncValue(Eva.computationState);

  React.useEffect(() => { evaComputationValue = evaComputed; }, [evaComputed]);

  const countErrors = remote.getErrors();
  remote.resetNotified();

  const startAnalysis = (): void => {
    setTimeout(() => {
      if(!remote.hasReset()) Server.send(Eva.compute, null);
      else startAnalysis();
    }, 150);
  };

  const compute = (): void => {
    if(remote.hasReset()) remote.commit();
    startAnalysis();
  };
  const abort = (): void => { Server.send(Eva.abort, null); };
  const clear = (): void => { Server.send(Eva.clear, null); };
  const syncFromFC = (): void => { remote.reset(); };
  const syncToFC = (): void => { remote.commit(); };

  return (
    <SidebarTitle label='Parameters of Eva Analysis' className='eva-tools'>
      <Hbox className='eva-tools-actions'>
        <IconButton
          icon="MEDIA.PLAY"
          title="Apply changes and launch Eva analysis"
          size={iconSize}
          disabled={evaComputed === "computing"}
          onClick={compute}
        />
        <IconButton
          icon="MEDIA.STOP"
          title="Abort Eva analysis"
          size={iconSize}
          disabled={evaComputed !== "computing"}
          onClick={abort}
        />
        <IconButton
          icon="CIRC.CLOSE"
          title="Clear Eva results, including alarms and statuses"
          size={iconSize}
          enabled={evaComputed !== "computing"}
          onClick={clear}
        />
        <IconButton
          icon="RELOAD"
          title="Reset form"
          size={iconSize}
          disabled={!remote.hasReset()}
          onClick={syncFromFC}
          />
        <IconButton
          icon="PUSH"
          title={"Apply changes"
            +
            (countErrors > 0 ?
            " : "+String(countErrors)+" error(s) in the form" : ""
            )
          }
          size={iconSize}
          kind={countErrors > 0 ? "warning" : "default"}
          disabled={!remote.hasCommit()}
          onClick={syncToFC}
        />
      </Hbox>
      <Hbox className='eva-tools-status'>
        <EvaStatus iconSize={18} />
        <HelpButton id='eva' size={18} />
      </Hbox>
    </SidebarTitle>
  );
}
