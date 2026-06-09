/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Sandbox Testing of Panel                                           --- */
/* --- Only appears in DEVEL mode.                                        --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import * as Dome from 'dome';
import { IconButton } from 'dome/controls/buttons';
import { registerSandbox, TitleBar } from 'ivette';
import {
  Panel, ListElement, Element, PanelPosition
} from 'dome/frame/panel';
import { Button, ButtonGroup } from 'dome/frame/toolbars';
import { Icon } from 'dome/controls/icons';
import './style.css';
import { Label } from 'dome/controls/labels';
import { Modal, showModal } from 'dome/dialogs';

/* -------------------------------------------------------------------------- */
/* --- Use Panel                                                          --- */
/* -------------------------------------------------------------------------- */
function UsePanel(): JSX.Element {
  const [position, setPosition] = React.useState<PanelPosition>('right');
  const [visible, flipVisible] = Dome.useFlipState(true);
  const [selected, setSelected] = React.useState(1);
  const [list, setList] = React.useState({
      flip: true,
      list: [
        { id: 1, children: <div>item 1</div> },
        { id: 2, children: <div>item 2</div> },
        { id: 3, children: <div>item 3</div> },
        { id: 4, children: <div>item 4</div> },
      ]
    }
  );
  const [modalTestOpen, setModalTestOpen] = React.useState(false);
  // eslint-disable-next-line max-len
  const modalText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi eros ligula, fermentum vitae turpis sed, semper rutrum nisl. Morbi non lacus scelerisque, facilisis urna sit amet, dapibus lorem. Sed ultrices pulvinar molestie. Duis semper condimentum lectus sed egestas. Quisque faucibus nisl vel nisi mattis volutpat. Curabitur aliquet arcu a diam semper, commodo efficitur purus placerat. Integer dapibus non urna eget pretium. Pellentesque in feugiat magna, ut condimentum lectus.";


  return (
    <>
      <TitleBar help='sandbox-panel'>
        <IconButton
          icon='ANGLE.RIGHT'
          title="Change position of Panel."
          className={'sandbox-panel-button-position-'+position}
          onClick={() => setPosition((val) => {
              return val === 'right' ? 'bottom' :
                     val === 'bottom' ? 'left' :
                     val === 'left' ? 'top' :
                     'right';
            })
          }
        />
        <IconButton
          icon="SIDEBAR"
          title={"show or hide the panel"}
          onClick={flipVisible}
        />
      </TitleBar>
      <div style={{ position: 'relative', height: '100%' }}>
        <Panel label='Panel title' visible={visible} position={position}
          actions={
            <IconButton
              icon='CROSS'
              size={14}
              title="Close panel."
              onClick={() => flipVisible()} />
            }
        >
          <>
            <Label label="Open test modal">
              <Button
                label='Open'
                kind={modalTestOpen ? 'positive' : 'default'}
                onClick={() => {
                  setModalTestOpen(true);
                  showModal(<Modal
                    label="Test modal title"
                    className='sandbox-modal'
                    onClose={() => setModalTestOpen(false)}
                  >
                    <div>{ modalText }</div>
                  </Modal>); }
                }
              />
            </Label>

            <Label
              label="label"
              title="Text Component"
              icon="CODE"
            >
              Content of the Text component.
            </Label>

            <Label>
              <ButtonGroup>
                <Button
                  label='left'
                  selected={position === 'left'}
                  onClick={() => setPosition('left')} />
                <Button
                  label='top'
                  selected={position === 'top'}
                  onClick={() => setPosition('top')} />
                <Button
                  label='right'
                  selected={position === 'right'}
                  onClick={() => setPosition('right')} />
                <Button
                  label='bottom'
                  selected={position === 'bottom'}
                  onClick={() => setPosition('bottom')} />
              </ButtonGroup>
            </Label>

            <Label
              label="List icon"
              title="Text Component"
            >
              <Icon id="WARNING"/>
              <Icon id="SETTINGS"/>
              <Icon id="RELOAD"/>
              <Icon id="DOWNLOAD"/>
              <Icon id="LOCK"/>
            </Label>

            <Label
              label="Add/remove element"
              title="Text Component"
            >
              <Button
                icon='PLUS'
                title="This button add an element t the end of the list."
                onClick={() => setList((elt) => {
                    const newId = elt.list.length + 1;
                    const newChild = "item "+newId;
                    elt.list.push({
                      id: newId, children: <div>{newChild}</div>
                    });
                    return { flip: !elt.flip, list: elt.list };
                  }
                )}
              />
              <Button
                icon='MINUS'
                title="This button remove the last element of the list."
                onClick={() => setList((elt) => {
                    elt.list.pop();
                    return { flip: !elt.flip, list: elt.list };
                  }
                )}
              />
            </Label>
            <ListElement>
              { list.list.map((elt, k) =>
                <Element
                  key={k}
                  selected={selected === elt.id}
                  onSelection={() => setSelected(elt.id)}
                >
                  {elt.children}
                </Element>
                )
              }
            </ListElement>
          </>
        </Panel>
      </div>
    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.panel',
  label: 'Panel + Modal',
  children: <UsePanel />,
});

/* -------------------------------------------------------------------------- */
