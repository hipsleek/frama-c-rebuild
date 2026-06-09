/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Managing Errors
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/errors
*/

import React, { ReactNode } from 'react';
import { DEVEL, Debug } from 'dome';
import { Label } from 'dome/controls/labels';
import { Button } from 'dome/controls/buttons';

const D = new Debug('Dome');

// --------------------------------------------------------------------------
// --- Error Boundaries
// --------------------------------------------------------------------------

/**
   Alternative renderer in case of error.
   @param reload - callback for re-rendering the faulty component
 */
export interface ErrorRenderer {
  (error: unknown, info: unknown, reload: () => void): JSX.Element;
}

export interface CatchProps {
  /** Name of the error boundary. */
  label?: string;
  /** Alternative renderer callback in case of errors. */
  onError?: JSX.Element | ErrorRenderer;
  children: ReactNode;
}

export interface CatchState {
  error?: unknown;
  info?: unknown;
}

/* eslint-disable react/prop-types */

/**
   React Error Boundaries.
 */
export class Catch extends React.Component<CatchProps, CatchState, unknown> {

  constructor(props: CatchProps) {
    super(props);
    this.state = {};
    this.logerr = this.logerr.bind(this);
    this.reload = this.reload.bind(this);
  }

  componentDidCatch(error: unknown, info: unknown): void {
    if (DEVEL) {
      const { label='Error' } = this.props;
      D.error(label, ': ', error, info);
    }
  }

  static getDerivedStateFromError(error: unknown, info: unknown): CatchState {
    return { error, info };
  }

  logerr(): void {
    const { error, info } = this.state;
    D.error('Caught error:', error, info);
  }

  reload(): void {
    this.setState({ error: undefined, info: undefined });
  }

  render(): JSX.Element {
    const { error, info } = this.state;
    const { onError, label = 'Error' } = this.props;
    if (error) {
      if (typeof onError === 'function')
        return onError(error, info, this.reload);
      return (
        <div>
          <Button
            icon="WARNING"
            kind="warning"
            title={typeof (error) === 'string' ? error : undefined}
            onClick={this.logerr}
          />
          <Button icon="RELOAD" onClick={this.reload} />
          <Label>{label}</Label>
        </div>
      );
    }
    return (<>{this.props.children}</>);
  }
}

// --------------------------------------------------------------------------
