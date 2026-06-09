/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Sandbox Testing of Tree                                            --- */
/* --- Only appears in DEVEL mode.                                        --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import { registerSandbox } from 'ivette';

import { Tree, Node, FoldIconPosition } from 'dome/frame/tree';
import { Panel } from 'dome/frame/panel';
import { IconButton } from 'dome/controls/buttons';

import './style.css';
import { Label } from 'dome/controls/labels';

interface SboxNode {
  id: string;
  label: string;
  subTree: SboxNode[];
}

type SboxTree = SboxNode[];

interface NodesProps {
  tree: SboxTree;
  multiSelectedState: [string[], React.Dispatch<React.SetStateAction<string[]>>]
}

/* -------------------------------------------------------------------------- */
/* --- Use Panel                                                          --- */
/* -------------------------------------------------------------------------- */

interface sboxActions {
  id: string;
  multiSelectedState: [string[], React.Dispatch<React.SetStateAction<string[]>>]
}

function Actions(props: sboxActions): JSX.Element {
  const { id, multiSelectedState } = props;
  const [ multiSelected, setMultiSelected ] = multiSelectedState;
  const [selectedItem, setSelectedItem] =
    React.useState(multiSelected.includes(id));

  return <IconButton
    icon="CHECK"
    kind={selectedItem ? "warning" : "default"}
    onClick={() => {
      setMultiSelected((val) => {
        const index = val.findIndex(elt => elt === id);
        setSelectedItem(index === -1);
        if (index === -1) return [...val, id];
        else return [...val.splice(index, 1)];
      });
    }}
  />;
}

function getSubTree(
  tree: SboxTree,
  multiSelectedState: [string[], React.Dispatch<React.SetStateAction<string[]>>]
): React.ReactNode {
  return tree.length > 0
    ? <Nodes tree={tree} multiSelectedState={multiSelectedState}/>
    : null;
}

function Nodes(props: NodesProps): React.ReactNode {
  const { tree, multiSelectedState } = props;
  return tree.map(({ id, label, subTree }) =>
    <Node
      key={id}
      id={id}
      label={label}
      actions={
        <Actions key={id} id={id} multiSelectedState={multiSelectedState}/>
      }
      >{ getSubTree(subTree, multiSelectedState) }</Node>
  );
}

function SandboxTree(): JSX.Element {
  const [ unfoldAll, setUnfoldAll ] = React.useState<boolean|undefined>(false);
  const [ selected, setSelected ] = React.useState<string | undefined>();
  const multiSelectedState = React.useState<string[]>([]);
  const [ multiSelected, ] = multiSelectedState;
  const [ foldIconPosition, setFoldIconPosition ] =
    React.useState<FoldIconPosition>('right');

  const allNodes = React.useMemo(() => {
    return tree.map(({ id, label, subTree }) =>
      <Node
        key={id}
        id={id}
        label={label}
        actions={
          <Actions key={id} id={id} multiSelectedState={multiSelectedState} />
        }
      >{ getSubTree(subTree, multiSelectedState) }</Node>
    ); }, [multiSelectedState]);

    const panelActions = <IconButton
        icon={foldIconPosition === 'left' ? 'ANGLE.RIGHT' : 'ANGLE.LEFT'}
        title='Change side of the fold button'
        onClick={() =>
          setFoldIconPosition(val => val === 'left' ? 'right' : 'left')}
        style={{ marginLeft: '15px' }}
      />;

  return (
    <>
      <div style={{ position: 'relative', height: '100%' }}>
        <Panel
          label={`Sandbox Tree: ${multiSelected.length} selected`}
          actions={panelActions}
          position='left'
          display={true}
        >
          <>
            <div className='sandbox-tree-control'>
              <Label
                label={`${selected ? `node ${selected}` : 'no node' } selected`}
              />
              { setUnfoldAll &&
                <div className='dome-xTree-actions'>
                  <IconButton
                    icon={ "CHEVRON.EXPAND" }
                    title="Unfold all"
                    disabled={unfoldAll}
                    size={14}
                    onClick={() => setUnfoldAll(true)}
                  />
                  <IconButton
                    icon={ "CHEVRON.CONTRACT" }
                    title="Fold all"
                    disabled={unfoldAll === false}
                    size={14}
                    onClick={() => setUnfoldAll(false)}
                  />
                </div>
              }
            </div>
            <Tree
              unfoldAll={unfoldAll}
              setUnfoldAll={setUnfoldAll}
              selected={selected}
              onClick={setSelected}
              foldButtonPosition={foldIconPosition}
              >
                { allNodes }
            </Tree>
          </>
        </Panel>
      </div>
    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Tree                                                               --- */
/* -------------------------------------------------------------------------- */

const tree: SboxTree = [
  { id: '1', label: 'Node 1', subTree: [
      { id: '1.1', label: 'Node 1.1', subTree: [
          { id: '1.1.1', label: 'Node 1.1.1', subTree: [], },
          { id: '1.1.2', label: 'Node 1.1.2', subTree: [], },
        ],
      },
      { id: '1.2', label: 'Node 1.2', subTree: [
          { id: '1.2.1', label: 'Node 1.2.1', subTree: [], },
        ],
      },
    ],
  },
  { id: '2', label: 'Node 2', subTree: [
      { id: '2.1', label: 'Node 2.1', subTree: [
          { id: '2.1.1', label: 'Node 2.1.1', subTree: [], },
        ],
      },
      { id: '2.2', label: 'Node 2.2', subTree: [
          { id: '2.2.1', label: 'Node 2.2.1', subTree: [], },
          { id: '2.2.2', label: 'Node 2.2.2', subTree: [
              { id: '2.2.2.1', label: 'Node 2.2.2.1', subTree: [], },
            ],
          },
        ],
      },
    ],
  },
  { id: '3', label: 'Node 3', subTree: [], },
  { id: '4', label: 'Node 4', subTree: [
      { id: '4.1', label: 'Node 4.1', subTree: [
          { id: '4.1.1', label: 'Node 4.1.1', subTree: [], },
        ],
      },
      { id: '4.2', label: 'Node 4.2', subTree: [
          { id: '4.2.1', label: 'Node 4.2.1', subTree: [], },
          { id: '4.2.2', label: 'Node 4.2.2 - with a long label', subTree: [
              { id: '4.2.2.1', label: 'Node 4.2.2.1', subTree: [], },
            ],
          },
        ],
      },
    ],
  },
  { id: '5', label: 'Node 5', subTree: [
      { id: '5.1', label: 'Node 5.1', subTree: [
          { id: '5.1.1', label: 'Node 5.1.1', subTree: [], },
        ],
      },
      { id: '5.2', label: 'Node 5.2', subTree: [
          { id: '5.2.1', label: 'Node 5.2.1', subTree: [
            { id: '5.2.1.1', label: 'Node 5.2.1.1', subTree: [
                { id: '5.2.1.1.1', label: 'Node 5.2.1.1.1', subTree: [], },
              ],
            },
          ],
        },
        ],
      },
    ],
  },
];

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.tree',
  label: 'Tree',
  children: <SandboxTree />,
});

/* -------------------------------------------------------------------------- */
