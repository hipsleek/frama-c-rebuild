/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* --------------------------------------------------------------------------*/
/* --- Ivette Extensions                                                  ---*/
/* --------------------------------------------------------------------------*/

import React from 'react';
import * as Dome from 'dome';

/* --------------------------------------------------------------------------*/
/* --- Extension Elements                                                 ---*/
/* --------------------------------------------------------------------------*/

const UPDATED = new Dome.Event('ivette.updated');

export interface ElementProps {
  id: string;
  rank?: number;
  children?: React.ReactNode;
}

export class ElementRack<A extends ElementProps> {

  private readonly items = new Map<string, A>();

  register(elt: A): void {
    this.items.set(elt.id, elt);
    UPDATED.emit();
  }

  getElement(id: string): A | undefined { return this.items.get(id); }
  getElements(): A[] { return Array.from(this.items.values()); }

}

export function useElement<A extends ElementProps>(
  E: ElementRack<A>, id: string | undefined
): A | undefined
{
  Dome.useUpdate(UPDATED);
  return id ? E.getElement(id) : undefined;
}

export function useElements<A extends ElementProps>(
  E: ElementRack<A>
): A[] {
  Dome.useUpdate(UPDATED);
  return E.getElements();
}

export function useChildren<A extends ElementProps>(
  E: ElementRack<A>
): React.ReactNode {
  const elements = useElements(E);
  return React.Children.toArray(elements.map((e) => e.children));
}

/* -------------------------------------------------------------------------- */
