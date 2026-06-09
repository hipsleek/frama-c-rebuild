/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import { Icon } from 'dome/controls/icons';
import { Label } from 'dome/controls/labels';
import { classes } from 'dome/misc/utils';
import { Button } from 'dome/controls/buttons';
import { closeModal, Modal, showModal } from 'dome/dialogs';

import * as Eva from 'frama-c/plugins/eva/api/analysis';
import { evaBasicStatus } from 'frama-c/plugins/eva/EvaDefinitions';
import { useSyncValue } from 'frama-c/states';
import * as Server from 'frama-c/server';
import { useStartComputing } from '../EvaSidebar';

interface EvaReadyProps {
  children: React.ReactNode;
  showChildrenForComputingStatus?: boolean;
}

interface EvaStatusProp {
  iconSize?: number; // default size for titlebar
  showStatus?: Eva.computationStateType[]; // all status shown by default
}

interface StatusIconProp {
  size: number;
  status?: Eva.computationStateType;
}

export function StatusIcon(props: StatusIconProp):JSX.Element {
  const { size, status } = props;
  const infosStatus = evaBasicStatus[status || "undefined"];

  return (
    <Icon
      id={infosStatus.icon}
      title={infosStatus.title}
      className={"eva-status-icon eva-"+status}
      size={size}
    />
  );
}

function EvaLaunchButton(): JSX.Element | null {
  return (
    <Button
      icon="MEDIA.PLAY"
      label="Run Eva analysis"
      title={"Start an Eva analysis. \n"
        + "The Eva sidebar allows changing some analysis parameters."}
      onClick={() => Server.send(Eva.compute, null)}
    />
  );
}

export function EvaStatus(props: EvaStatusProp): JSX.Element | null {
  const { iconSize = 12, showStatus } = props;
  const status = useSyncValue(Eva.computationState);

  if(!showStatus || status && showStatus?.includes(status)) {
    return <StatusIcon size={iconSize} status={status} />;
  } else return null;
}

function timeToString(time: number): string {
  const totalSeconds = Math.floor(time / 1000);

  const hours = Math.floor(totalSeconds / 3600);
  const minutes = Math.floor((totalSeconds % 3600) / 60);
  const seconds = totalSeconds % 60;

  const strSeconds = seconds.toString().padStart(2, '0');
  const strMinutes = minutes.toString().padStart(2, '0');
  const strTime = `${strMinutes}:${strSeconds}`;

  return hours > 0 ? `${hours}:${strTime}` : strTime;
}

function Timer({ start }: {start: number}): React.JSX.Element | null {
  const [time, setTime] = React.useState(0);

  const className = classes(
    'eva-status-timer',
    time < 10000 && 'eva-status-timer-hide'
  );

  React.useEffect(() => {
    if(start === 0) return;
    const interval = setInterval(() => setTime(Date.now() - start), 1000);
    return () => clearInterval(interval);
  }, [start]);

  return <Label className={className}>{timeToString(time)}</Label>;
}

function EvaStatusPanel(): JSX.Element {
  const start = useStartComputing();
  const status = useSyncValue(Eva.computationState);
  const infosStatus = evaBasicStatus[status || "undefined"];

  return (
    <div className={"eva-status eva-status-"+status}>
      <div className='eva-status-content'>
        <div className="eva-status-message">{infosStatus.message}</div>
        { status === 'not_computed' && <EvaLaunchButton /> }
        <StatusIcon size={50} status={status} />
        <Timer start={start}/>
      </div>
    </div>
  );
}

export function EvaReady(props: EvaReadyProps): JSX.Element {
  const { showChildrenForComputingStatus = false, children } = props;
  const status = useSyncValue(Eva.computationState);
  const showChildren = Boolean(
    status === "aborted" || status === "computed" ||
    (showChildrenForComputingStatus && status === "computing")
  );
  if(showChildren) return <>{children}</>;
  else return <EvaStatusPanel/>;
}

/* -------------------------------------------------------------------------- */
/* --- Modal                                                              --- */
/* -------------------------------------------------------------------------- */

function EvaModal({ callback }: { callback: () => void }): React.JSX.Element {
  const status = useSyncValue(Eva.computationState);

  /* The callback function is only called if the Eva modal window is still
   * open at the end of the analysis.
   * Closing the modal window is equivalent to abandoning the action. */
  React.useEffect(() => {
    if(status === 'computed') {
      callback();
      closeModal();
    }
  }, [status, callback]);

  return (
    <Modal className='modal-eva' label="Eva analysis required" >
      <div className={"eva-status eva-status-"+status}>
        <div className='eva-status-content'>
          <EvaStatusPanel />
          { status === "computing" &&
            <div className='eva-status-message'>
              The requested action will be executed once the analysis is
              complete. <br />
              Closing this window cancels the action, but not the Eva analysis.
            </div>
          }
        </div>
      </div>
    </Modal>
  );
}

function showEvaModal(callback: () => void): void {
  showModal(<EvaModal callback={callback} />);
}

export async function evaNeeded(callback: () => void): Promise<void> {
  const status = await Server.send(Eva.getComputationState, []);
  if(status !== "computed" && status !== "aborted")
    showEvaModal(callback);
  else
    callback();
}
