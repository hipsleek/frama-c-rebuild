/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { Meter } from 'dome/controls/displays';

export interface Coverage {
  reachable: number;
  dead: number;
}

export interface CoverageProps {
  coverage: Coverage;
}

export function percent(coverage: Coverage): string {
  const n = coverage.reachable + coverage.dead;
  if (n > 0) {
    const r = coverage.reachable;
    if (r > 0) {
      const q = r / n;
      return `${(q * 100).toFixed(1)}%`;
    } else return '0%';
  } else return 'n/a';
}

export default function CoverageMeter(props: CoverageProps): JSX.Element {
  const { reachable, dead } = props.coverage;
  const total = reachable + dead;

  return (
    <Meter
      min={0}
      max={total}
      low={0.50 * total}
      high={0.85 * total}
      optimum='HIGH'
      value={reachable}
    />
  );
}
