/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { Button, ButtonGroup } from 'dome/frame/toolbars';
import { useWindowSettings, useWindowSettingsData } from 'dome/data/settings';
import { Decoder, Encoder, json, JsonTypeError } from 'dome/data/json';
import { ModeDisplay } from '../definitions';

/* -------------------------------------------------------------------------- */
/* --- ThreeStateButton component                                         --- */
/* -------------------------------------------------------------------------- */
export interface IThreeStateButton {
  active: boolean,
  max: boolean,
  value: number,
}

export type TThreesButtonState = [
  IThreeStateButton,
  (newValue: IThreeStateButton) => void
];

interface ThreeStateButtonProps {
  label?: string;
  icon?: string;
  title?: string;
  buttonState: TThreesButtonState;
}

const encodeButton: Encoder<IThreeStateButton> = (js: IThreeStateButton) => {
  return JSON.stringify(js);
};
const decodeButton: Decoder<IThreeStateButton> = (js: json) => {
  if (typeof js === 'string') return JSON.parse(js);
  else throw new JsonTypeError("string", js);
};

export function useTSButton(id: string): TThreesButtonState {
  return useWindowSettingsData<IThreeStateButton>(
    `ivette.callgraph.${id}`,
    decodeButton, encodeButton,
    { active: true, max: true, value: 1 }
  );
}

export function ThreeStateButton(
  props: ThreeStateButtonProps
): JSX.Element {
  const { label, icon, title, buttonState } = props;
  const [ button, setButton ] = buttonState;

  const onClickAll = (): void =>
    setButton({ ...button, active: !button.max, max: !button.max });
  const onClickVal = (): void => {
    const newVal = button.max ? true : !button.active;
    setButton({ ...button, active: newVal, max: false }
  ); };
  const onUpVal = (): void =>
    setButton({ ...button, value: button.value ? button.value + 1 : 1 });
  const onDownVal = (): void =>
    setButton({ ...button, value: button.value ? button.value - 1 : 0 });

  return (
    <div className='cg-three-states'>
      <ButtonGroup className='cg-number-button'>
        <Button
          className="three-button-label"
          label={label} icon={icon} title={title}
        />
        <Button label="All" selected={button.max} onClick={onClickAll} />
        <Button
          label={button.value.toString()}
          selected={button.active && !button.max}
          onClick={onClickVal}
        />
        <Button icon='MINUS' className='cg-plus-minus' onClick={onDownVal} />
        <Button icon='PLUS'  className='cg-plus-minus' onClick={onUpVal} />
      </ButtonGroup>
    </div>
  );
}

/** Other buttons */
export const decodeMode: Decoder<ModeDisplay> = (js: json) => {
  if (js === 'all' || js === "linked" || js === "selected" ) return js;
  else throw new JsonTypeError("ModeDisplay", js);
};

export function useDMButton(): [ModeDisplay, (newValue: ModeDisplay) => void] {
  return useWindowSettings<ModeDisplay>(
    "ivette.callgraph.displaymode", decodeMode, "all"
  );
}
