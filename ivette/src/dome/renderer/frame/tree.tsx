/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
  This package provide components to displayed a tree

  @packageDocumentation
  @module dome/frame/Tree
*/

import React from 'react';
import { classes, styles } from 'dome/misc/utils';
import { Label } from 'dome/controls/labels';
import { IconButton } from 'dome/controls/buttons';
import { Actions } from 'dome/layout/forms';

/* --------------------------------------------------------------------------*/
/* --- Tree                                                                  */
/* --------------------------------------------------------------------------*/

export type FoldIconPosition = 'left' | 'right';

type TreeContext = {
  depth: number;
  heightSticky: number;
} & Omit<TreeProps, 'children' | 'className'>;

const CONTEXT_DEFAULT: TreeContext = {
  foldButtonPosition: 'left',
  depth: 0,
  heightSticky: 0,
};
const CONTEXT = React.createContext<TreeContext>(CONTEXT_DEFAULT);

function useContext(props: Partial<TreeContext>): TreeContext {
  const Parent = React.useContext(CONTEXT);
  return {
    unfoldAll: props.unfoldAll,
    setUnfoldAll: props.setUnfoldAll,
    foldButtonPosition: props.foldButtonPosition || Parent.foldButtonPosition,
    selected: props.selected || Parent.selected,
    depth: props.depth || Parent.depth,
    heightSticky: props.heightSticky || Parent.heightSticky,
    onClick: props.onClick,
  };
}

interface TreeProps {
  className?: string;
  unfoldAll?: boolean ; /* default false */
  setUnfoldAll?: (v: boolean|undefined) => void;
  foldButtonPosition?: FoldIconPosition;
  selected?: string;
  sticky?: boolean;
  onClick?: (id: string) => void;
  children?: React.ReactNode ; /* only nodes */
}

export function Tree(props: TreeProps): JSX.Element {
  const {
      unfoldAll, setUnfoldAll, foldButtonPosition,
      selected, sticky, onClick, className
    } = props;

  const context = useContext({
    unfoldAll: unfoldAll,
    setUnfoldAll: setUnfoldAll,
    onClick: onClick,
    foldButtonPosition: foldButtonPosition,
    selected,
  });
  const treeClass = classes(
    'dome-xTree',
    sticky && 'dome-xTree-sticky',
    className);

  return (
    <CONTEXT.Provider value={context}>
      <div className={treeClass}>
        <div className='dome-xTree-nodes'>
          { props.children }
        </div>
      </div>
    </CONTEXT.Provider>
  );
}

export interface NodeProps {
  id: string;
  icon?: string;
  label?: string;
  title?: string;
  actions?: React.ReactNode;
  children?: React.ReactNode;
}

export function Node(props: NodeProps): JSX.Element {
  const { id, icon, label, title, actions, children } = props;

  const context = React.useContext(CONTEXT);
  const countChildren = React.Children.count(children);
  const hasSubTree = countChildren > 0;

  const ref = React.useRef<HTMLDivElement>(null);
  const [height, setHeight] = React.useState(0);
  React.useEffect(() => {
    if (ref.current) setHeight(ref.current.offsetHeight);
  }, [context]);

  const [ unfold, setUnfold ] = React.useState(
    context.unfoldAll !== undefined ? context.unfoldAll : false);
  const flipUnfold = (): void => {
    setUnfold(v => !v);
    if (context.setUnfoldAll !== undefined) context.setUnfoldAll(undefined);
  };

  React.useEffect(() => {
    if(context.unfoldAll !== undefined) setUnfold(context.unfoldAll);
  }, [context.unfoldAll]);

  const className = classes(
    'dome-xTree-node',
    hasSubTree ? "dome-xTree-has-subtree" : "",
    unfold ? 'dome-xTree-show-children' : 'dome-xTree-hide-children',
    context.selected === id && 'dome-xTree-selected'
  );
  const classSubtree = classes(
    'dome-xTree-subtree',
    unfold ? 'dome-xTree-subtree-visible' : 'dome-xTree-subtree-hidden'
  );
  const topDepth = (context.heightSticky).toString()+'px';
  const zIndex = (100 - context.depth);
  const style = styles(context.depth > 0 && {
    marginLeft: `10px`,
    top: topDepth,
    zIndex: zIndex
  });

  /** ************************************************************ */

  const foldIconPosition = context?.foldButtonPosition || 'left';
  const foldIcon = <IconButton
      className='dome-xTree-folding-button'
      style={{ visibility: hasSubTree ? 'visible' : 'hidden' }}
      icon={ "ANGLE.DOWN" }
      onClick={() => flipUnfold()}
    />;

  return (
    <CONTEXT.Provider value={{
      ...context,
      depth: context.depth+1,
      heightSticky: context.heightSticky+height
    }}>
      <div>
        <div ref={ref} className={className} style={style}
          onClick={() => context.onClick ? context.onClick(id) : flipUnfold() }
        >
            <div>
              { foldIconPosition === 'left' && foldIcon }
              <Label icon={icon} title={title}
                label={label}
              ></Label>
            </div>
            <Actions>
              { actions && actions }
              { foldIconPosition === 'right' && foldIcon }
            </Actions>
        </div>
        { hasSubTree &&
          <div className={classSubtree} style={style}>
            { children }
          </div>
        }
      </div>
    </CONTEXT.Provider>
  );
}
