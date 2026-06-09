/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Display logs emitted by Server
// --------------------------------------------------------------------------

import * as React from 'react';

import * as Display from 'ivette/display';
import { IconButton } from 'dome/controls/buttons';
import { Cell } from 'dome/controls/labels';
import { Page } from 'dome/text/pages';
import * as Editor from 'dome/text/editor';
import * as Arrays from 'dome/table/arrays';
import { Table, Column, Renderer } from 'dome/table/views';
import { BSplit } from 'dome/layout/splitters';
import { Hbox, Scroll, Vbox } from 'dome/layout/boxes';
import { GlobalState, useGlobalState } from 'dome/data/states';
import { json } from 'dome/data/json';
import * as Compare from 'dome/data/compare';
import { duration } from 'dome/misc/format';

import { RqKind } from 'frama-c/server';

// --------------------------------------------------------------------------
// --- Logs types
// --------------------------------------------------------------------------

export enum typeLog {
  /** Signal emitted by server. */
  SIGNAL = 'SIGNAL',
  /** Signal sent to the server. */
  REQUEST = 'REQUEST',
  /** Message information. */
  FEEDBACK = 'FEEDBACK',
}

export enum status {
  /** Request is waiting to be resolved. */
  PENDING = 'PENDING',
  /** Request is resolved. */
  RESOLVED = 'RESOLVED',
  /** Request is rejected. */
  REJECTED = 'REJECTED',
  /** Request is in error. */
  ERROR = 'ERROR',
  /** New signal ON. */
  SIGON = 'SIGON',
  /** Signal is OFF. */
  SIGOFF = 'SIGOFF'
}

enum LogFlow {
  /** Pending request, Signal ON or Feedback. */
  IN = 'IN',
  /** Response of a specific Request or Signal. */
  OUT = 'OUT',
}

/** Server log type. */
interface Log {
  /** Row identifier. */
  key: number;
  /** The precise time of log registration. */
  timestamp: number;
  /** Gap time between a selected log and the other logs. */
  delta?: number;
  /** Type of log. */
  type: typeLog;
  /** The log name. */
  name: string;
  /** Id of request. */
  rid?: string;
  /** The request kind (GET, SET, EXEC). */
  kind?: RqKind;
  /** Log status */
  status?: status;
  /** The parameters or data stored in request. */
  params?: json;
}

interface RequestInfo {
  rid: string;
  kind?: RqKind;
  name?: string;
  param?: json;
  statut: status;
}

// --------------------------------------------------------------------------
// --- Server Logs Compact Model
// --------------------------------------------------------------------------

export const RECORDING = new GlobalState<boolean>(false);

class LogModel extends Arrays.CompactModel<number, Log> {
  private logs: Log[] = [];
  private logKey = 0;
  private recording = false;

  constructor() {
    super((log: Log) => log.key);
    RECORDING.on((v) => {
      if (v && !this.recording) this.clear();
      this.recording = v;
    });
  }

  /** Empty Compact Model and reset key values. */
  clear(): void {
    this.logKey = 0;
    super.clear();
  }

  /** Register a new log in Server Logs Table. */
  addLog(log: Log): void {
    if (this.recording) {
      this.logs.push(log);
      this.flushLogs();
    }
  }

  /** Find the corresponding request with it's id. */
  findNameByRid(rid: string): string {
    const data = this.getArray().concat(this.logs);
    const log = data.filter(log => log.rid === rid);
    const lastLog = log.pop();
    return lastLog ? lastLog.name : '';
  }

  /** Register a new request log. */
  registerRequest(request: RequestInfo): void {
    if (!this.recording) return;
    const { rid, kind, name, param, statut } = request;
    const log: Log = {
      key: this.logKey++,
      rid,
      timestamp: Date.now(),
      delta: 0,
      type: typeLog.REQUEST,
      kind: kind,
      name: name ?? this.findNameByRid(rid),
      params: param,
      status: statut
    };
    this.addLog(log);
  }

  /** Register a new signal log. */
  registerSignal(id: string, statut: status): void {
    if (!this.recording) return;
    const log: Log = {
      key: this.logKey++,
      timestamp: Date.now(),
      delta: 0,
      type: typeLog.SIGNAL,
      name: id,
      status: statut
    };
    this.addLog(log);
  }

  /** Register a new feedback log. */
  registerFeedback(msg: string): void {
    if (!this.recording) return;
    const log: Log = {
      key: this.logKey++,
      timestamp: Date.now(),
      delta: 0,
      type: typeLog.FEEDBACK,
      name: msg
    };
    this.addLog(log);
  }

  /** Update Compact Model with current logs.  */
  flushLogs(): void {
    if (this.logs.length > 0) {
      this.updateData(this.logs);
      this.reload();
      this.logs = [];
    }
  }

}

/** Main log compact model to register new logs emitted by the server*/
export const logModel: LogModel = new LogModel();

// -------------------------------------------------------------------------
// --- Table columns
// -------------------------------------------------------------------------

const renderCell: Renderer<string> =
  (text: string): JSX.Element => (<Cell title={text}> {text} </Cell>);
const renderType: Renderer<typeLog> = (t: typeLog): JSX.Element => {
  switch (t) {
    case typeLog.FEEDBACK:
      return <Cell title={'Feedback'}>{'INFO'}</Cell>;
    case typeLog.REQUEST:
      return <Cell title={'Request'}>{'RQ'}</Cell>;
    case typeLog.SIGNAL:
      return <Cell title={'Signal'}>{'SIG'}</Cell>;
  }
};

const renderTime: Renderer<string> = (date: string): JSX.Element => {
  const d = new Date(date);
  const h = d.getHours().toString().padStart(2, '0');
  const mn = d.getMinutes().toString().padStart(2, '0');
  const s = d.getSeconds().toString().padStart(2, '0');
  const ms = d.getMilliseconds().toString().padStart(3, '0');
  const newTime = `${h}:${mn}:${s}.${ms}`;
  return <Cell title={newTime}> {newTime} </Cell>;
};

const renderDelta = (delta: number): JSX.Element => {
  const text =
    delta > 0 ? `+${duration(delta)}` :
      delta < 0 ? `-${duration(-delta)}` :
        '0';
  return <Cell>{text}</Cell>;
};

const LogColumns = (): JSX.Element => (
  <>
    <Column
      id="key"
      label="#"
      title="Line number"
      visible={false}
      width={27}
      render={renderCell}
    />
    <Column
      id="timestamp"
      label="Timestamp"
      width={90}
      title="Registration time"
      render={renderTime}
    />
    <Column
      id="delta"
      label="Delta time"
      align='right'
      width={60}
      title="Gap time between a selected log and other logs"
      render={renderDelta}
    />
    <Column
      id="type"
      label="Type"
      title="Log type (Feedback, Request, Signal)"
      width={50}
      align='center'
      render={renderType}
    />
    <Column
      id="rid"
      label="RQ ID"
      title="Request ID"
      width={55}
      align='center'
      render={renderCell}
    />
    <Column
      id="kind"
      label="Kind"
      title="Request kind (GET, SET, EXEC)"
      width={50}
      align='center'
      visible={false}
      render={renderCell}
    />
    <Column
      id="status"
      label="Status"
      title="Request status"
      width={80}
      align='center'
      render={renderCell}
    />
    <Column
      id="name"
      label="Name"
      title="Log name"
      fill
      render={renderCell}
    />
  </>
);

// -------------------------------------------------------------------------
// --- Displays related log
// -------------------------------------------------------------------------

/** Find request log. */
const findRequestLog = (log: Log, logs: Log[]): Log | undefined => {
  if (log.status === status.PENDING) {
    return logs.find(
      (l) =>
        l.type === typeLog.REQUEST &&
        l.status !== status.PENDING &&
        l.rid === log.rid &&
        l.name === log.name &&
        l.key > log.key
    );
  } else {
    return logs
      .filter(
        (l) =>
          l.type === typeLog.REQUEST &&
          l.status === status.PENDING &&
          l.rid === log.rid &&
          l.name === log.name &&
          l.key < log.key
      )
      .pop();
  }
};

/** Find signal log. */
const findSignalLog = (log: Log, logs: Log[]): Log | undefined => {
  if (log.status === status.SIGON) {
    return logs.find(
      (l) =>
        l.type === typeLog.SIGNAL &&
        l.status === status.SIGOFF &&
        l.name === log.name &&
        l.key > log.key
    );
  } else {
    return logs
      .filter(
        (l) =>
          l.type === typeLog.SIGNAL &&
          l.status === status.SIGON &&
          l.name === log.name &&
          l.key < log.key
      )
      .pop();
  }
};

/** Find corresponding log if it exists. */
const findLog = (log?: Log): Log | undefined => {
  if (!log) return;

  const logs = logModel.getArray();

  switch (log.type) {
    case typeLog.REQUEST:
      return findRequestLog(log, logs);
    case typeLog.SIGNAL:
      return findSignalLog(log, logs);
    default:
      return undefined;
  }
};

// -------------------------------------------------------------------------
// --- Request Data or Parameters in Editor
// -------------------------------------------------------------------------

const extensions: Editor.Extension[] = [
  Editor.ReadOnly,
  Editor.Selection,
  Editor.LineNumbers,
  Editor.LanguageHighlighter,
  Editor.HighlightActiveLine,
];

/** Displays a simple view of the request data or parameters.  */
function ParamViewer({ log }: { log: Log }): JSX.Element {
  const { view, Component } = Editor.Editor(extensions);
  const [_content, setContent] = React.useState<string>();

  React.useEffect(() => {
    const newContent = JSON.stringify(log.params, null, 2);
    setContent(newContent);
    const Source = Editor.createTextField<string>('', (s: string) => s);
    Source.set(view, newContent);
  }, [view, log]);

  return <Component />;
}

// -------------------------------------------------------------------------
// --- IN & OUT Navigation Tabs
// -------------------------------------------------------------------------

const getLogType = (log?: Log): LogFlow => {
  if (!log || log.type === typeLog.FEEDBACK)
    return LogFlow.IN;

  switch (log.status) {
    case status.PENDING:
      return LogFlow.IN;
    case status.SIGON:
      return LogFlow.IN;
    default:
      return LogFlow.OUT;
  }
};

interface TabProps {
  label: string;
  active: boolean;
  onClick: () => void;
}

function Tab({ label, active, onClick }: TabProps): JSX.Element {
  return (
    <div
      className={`server-logs-tab ${active && 'server-logs-tab-active'}`}
      onClick={onClick}
    >
      {label}
    </div>
  );
}

interface InOutTabsProps {
  log?: Log;
  relatedLog?: Log;
  selectLog: (s?: Log) => void;
}

function InOutTabs(props: InOutTabsProps): JSX.Element {
  const { log, relatedLog, selectLog } = props;

  const type = getLogType(log);
  const activeTab: LogFlow = type;

  const handleClick = (flow: LogFlow): void => {
    if (flow !== activeTab) selectLog(relatedLog);
  };

  return relatedLog ? (
    <>
      <Tab
        label='IN'
        active={activeTab === LogFlow.IN}
        onClick={() => handleClick(LogFlow.IN)}
      />
      <Tab
        label='OUT'
        active={activeTab === LogFlow.OUT}
        onClick={() => handleClick(LogFlow.OUT)}
      />
    </>
  ) : (
    <Tab
      label={type}
      active={activeTab === type}
      onClick={() => handleClick(type)}
    />
  );
}

// -------------------------------------------------------------------------
// --- Bottom Panel
// -------------------------------------------------------------------------
interface LogPanelProps {
  selectedLog?: Log;
  selectLog: (s?: Log) => void;
}

function LogPanel({ selectedLog, selectLog }: LogPanelProps): JSX.Element {

  const relatedLog = findLog(selectedLog);

  return (
    <Vbox style={{ height: '100%' }}>
      <Hbox style={{ alignItems: 'center' }}>
        <InOutTabs
          log={selectedLog}
          relatedLog={relatedLog}
          selectLog={selectLog}
        />
        <IconButton
          style={{ marginLeft: '5px' }}
          icon="CROSS"
          title="Close"
          size={14}
          onClick={() => selectLog(undefined)}
        />
      </Hbox>
      {selectedLog?.type === typeLog.REQUEST ? (
        <ParamViewer log={selectedLog} />
      ) : (
        <Scroll>
          <Page className="message-page">
            {`${selectedLog?.type} : ${selectedLog?.name}`}
          </Page>
        </Scroll>
      )}
    </Vbox>
  );
}

/** Calculate the time delta between a specific log and the other logs. */
function computeDelta(log?: Log): void {
  if (!log || !log.kind) return;
  const logs = logModel.getArray();
  const newLogs = logs.map(l => {
    const diff = l.timestamp - log.timestamp;
    const delta = diff / 1000;
    return { ...l, delta };
  });
  logModel.replaceAllDataWith(newLogs);
}

// -------------------------------------------------------------------------
// --- Logs Recording Button
// -------------------------------------------------------------------------

export function RecordingLogs(): JSX.Element {
  const { active } = Display.useComponentStatus('fc.server.serverlogs');
  const [isRecording] = useGlobalState(RECORDING);
  const onClick = (): void => RECORDING.setValue(!isRecording);
  return (
    <IconButton
      kind={isRecording ? 'negative' : 'default'}
      icon="MEDIA.HALT"
      display={active}
      onClick={onClick}
      title='Stop recording logs'
    />
  );
}

// -------------------------------------------------------------------------
// --- Server Logs Table
// -------------------------------------------------------------------------

const byLog: Compare.ByFields<Log> = {
  key: Compare.number,
  timestamp: Compare.defined(Compare.number),
  type: Compare.string,
  rid: Compare.defined(Compare.string),
  kind: Compare.defined(Compare.string),
  status: Compare.defined(Compare.byEnum(status))
};

export function ServerLogs(): JSX.Element {
  const [recording] = useGlobalState(RECORDING);

  const [model] = React.useState(() => {
    const m = logModel;
    m.setOrderingByFields(byLog);
    return m;
  });
  const [selectedLog, selectLog] = React.useState<Log>();
  React.useEffect(() => model.flushLogs(), [model, recording]);
  React.useEffect(() => computeDelta(selectedLog), [selectedLog]);

  return (
    <BSplit
      settings="ivette.serverlogs.filterSplit"
      defaultPosition={210}
      unfold={selectedLog !== undefined}
    >
      <Table<number, Log>
        model={logModel}
        sorting={logModel}
        selection={selectedLog?.key}
        onSelection={selectLog}
        settings="ivette.serverlogs.table"
      >
        <LogColumns />
      </Table>
      <LogPanel
        selectedLog={selectedLog}
        selectLog={selectLog}
      />
    </BSplit>
  );
}

// --------------------------------------------------------------------------
