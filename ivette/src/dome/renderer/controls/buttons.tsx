/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Buttons, Check Boxes and Radio Groups
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/controls/buttons
*/

import React from 'react';
import { classes } from 'dome/misc/utils';
import { Vbox } from 'dome/layout/boxes';
import { Icon } from './icons';
import * as ToolBar from 'dome/frame/toolbars';

interface EVENT {
  stopPropagation: () => void;
}

const DISABLED = ({ disabled = false, enabled = true }): boolean => (
  !!disabled || !enabled
);

const TRIGGER = (onClick?: () => void) => (evt?: EVENT) => {
  evt?.stopPropagation();
  if (onClick) onClick();
};

// --------------------------------------------------------------------------
// --- Button
// --------------------------------------------------------------------------

interface LABELprops {
  disabled: boolean;
  label: string;
}

const LABEL = ({ disabled, label }: LABELprops): JSX.Element => {
  const className = classes(
    'dome-xButton-label',
    disabled ? 'dome-control-disabled' : 'dome-control-enabled'
  );
  return <div className={className}>{label}</div>;
};

export type ButtonKind =
  'default' | 'primary' | 'warning' | 'positive' | 'negative';

export interface ButtonProps {
  /** Text of the label. Prepend to other children elements. */
  label?: string;
  /** Icon identifier. Displayed on the left side of the label. */
  icon?: string;
  /** Tool-tip description. */
  title?: string;
  /** Additional class. */
  className?: string;
  /** Additional style. */
  style?: React.CSSProperties;
  /** Defaults to `false`. */
  selected?: boolean;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Defaults to `true`. */
  visible?: boolean;
  /** Defaults to `true`. */
  display?: boolean;
  /**
     May gain focus.
     Focused button can be clicked with the `ENTER` key.
     Defaults to `false`.
   */
  focusable?: boolean;
  /** Styled bytton:
     - `'default'`: normal button;
     - `'primary'`: primary button, in blue;
     - `'warning'`: warning button, in orange;
     - `'positive'`: positive button, in green;
     - `'negative'`: negative button, in red.
   */
  kind?: ButtonKind;
  /** Blinking button. Defaults to `false`. */
  blink?: boolean;
  /**
     Button callback.
     An undefined callback automatically disables the button.
   */
  onClick?: () => void;
}

/** Standard button. */
export function Button(props: ButtonProps): JSX.Element {
  const disabled = props.onClick ? DISABLED(props) : true;
  const {
    focusable = false, kind = 'default',
    visible = true, display = true, blink = false,
    selected, icon, label, className = '',
  } = props;
  const theClass = classes(
    'dome-xButton dome-xBoxButton',
    `dome-xButton-${selected ? 'selected' : kind}`,
    blink && 'dome-xButton-blink',
    !visible && 'dome-control-hidden',
    !display && 'dome-control-erased',
    className,
  );
  const nofocus = focusable ? undefined : true;
  return (
    <button
      type="button"
      className={theClass}
      disabled={disabled}
      onClick={TRIGGER(props.onClick)}
      title={props.title}
      style={props.style}
      tabIndex={nofocus && -1}
      onMouseDown={nofocus && ((evt) => evt.preventDefault())}
    >
      {icon && <Icon id={icon} />}
      {label && <LABEL disabled={disabled} label={label} />}
    </button>
  );
}

// --------------------------------------------------------------------------
// --- Icon Button
// --------------------------------------------------------------------------

/** Circled Icon Button. The label property is ignored. */
export const CircButton = (props: ButtonProps): JSX.Element => {
  const disabled = props.onClick ? DISABLED(props) : true;
  const {
    focusable = false, kind = 'default',
    visible = true, display = true,
    selected, icon, blink, className = '',
  } = props;
  const theClass = classes(
    'dome-xButton dome-xCircButton',
    `dome-xButton-${selected ? 'selected' : kind}`,
    blink && 'dome-xButton-blink',
    !visible && 'dome-control-hidden',
    !display && 'dome-control-erased',
    className,
  );
  const nofocus = focusable ? undefined : true;
  return (
    <button
      type="button"
      className={theClass}
      disabled={disabled}
      onClick={TRIGGER(props.onClick)}
      title={props.title}
      style={props.style}
      tabIndex={nofocus && -1}
      onMouseDown={nofocus && ((evt) => evt.preventDefault())}
    >
      {icon && <Icon offset={-1} id={icon} />}
    </button>
  );
};

// --------------------------------------------------------------------------
// --- Icon Button
// --------------------------------------------------------------------------

export type IconButtonKind =
  'default' | 'positive' | 'negative' | 'warning' | 'selected';

export interface IconButtonProps {
  /** Icon identifier. Displayed on the left side of the label. */
  icon: string;
  /** Tool-tip description. */
  title?: string;
  /** Icon size, in pixels (default: `12`). */
  size?: number;
  /** Vertical offset, in pixels. */
  offset?: number;
  /** Additional class. */
  className?: string;
  /** Additional style. */
  style?: React.CSSProperties;
  /** Defaults to `false`. */
  selected?: boolean;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Defaults to `true`. */
  visible?: boolean;
  /** Defaults to `true`. */
  display?: boolean;
  /** Styled bytton:
     - `'default'`: normal button;
     - `'selected'`: selection button, in blue;
     - `'warning'`: warning button, in orange;
     - `'positive'`: positive button, in green;
     - `'negative'`: negative button, in red.
   */
  kind?: IconButtonKind;
  /**
     Button callback.
     An undefined callback automatically disables the button.
   */
  onClick?: () => void;
}

/** Borderless Icon Button. Label property is ignored. */
export function IconButton(props: IconButtonProps): JSX.Element | null {
  const disabled = props.onClick ? DISABLED(props) : true;
  const {
    icon, title, className,
    visible = true, display = true, selected,
    kind = 'default',
  } = props;
  if (!icon) return null;
  const theClass = classes(
    'dome-xIconButton',
    `dome-xIconButton-${selected ? 'selected' : kind}`,
    (disabled ? 'dome-control-disabled' : 'dome-control-enabled'),
    !visible && 'dome-control-hidden',
    !display && 'dome-control-erased',
    className,
  );
  return (
    <Icon
      id={icon}
      title={title}
      size={props.size}
      offset={props.offset}
      style={props.style}
      className={theClass}
      onClick={TRIGGER(disabled ? undefined : props.onClick)}
    />
  );
}

// --------------------------------------------------------------------------
// --- CheckBox
// --------------------------------------------------------------------------

const CHECKBOX_ENABLED = 'dome-control-enabled dome-xCheckbox ';
const CHECKBOX_DISABLED = 'dome-control-disabled dome-xCheckbox ';

export interface CheckProps {
  /** Button label. */
  label: string;
  /** Button tooltip. */
  title?: string;
  /** Additional class. */
  className?: string;
  /** Additional style. */
  style?: React.CSSProperties;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Defaults to `false`. */
  value?: boolean;
  /** Callback to changes. */
  onChange?: (newValue: boolean) => void;
}

interface GenericCheckProps extends CheckProps {
  icon: string;
}

/** Generic Checkbox button. */
export const GenericCheckbox = (props: GenericCheckProps): JSX.Element => {
  const { onChange, value = false, icon } = props;
  const disabled = onChange ? DISABLED(props) : true;
  const callback = onChange && (() => onChange(!value));
  const baseClass = disabled ? CHECKBOX_DISABLED : CHECKBOX_ENABLED;
  const labelClass = props.className || '';
  return (
    <label
      title={props.title}
      style={props.style}
      className={baseClass + labelClass}
      onClick={TRIGGER(callback)}
    >
      <Icon
        id={icon + (value ? '.ON' : '.OFF')}
        kind={disabled ? "disabled" : "default"}
        offset={0} // Alignment handled by CSS flex centering.
      />
      {props.label}
    </label>
  );
};

/** Checkbox button. */
export const Checkbox = (props: CheckProps): JSX.Element => {
  return <GenericCheckbox icon='CHECKBOX' {...props} />;
};

/** Switch button. */
export const Switch = (props: CheckProps): JSX.Element => {
  return <GenericCheckbox icon='SWITCH' {...props} />;
};

// --------------------------------------------------------------------------
// --- Radio Button
// --------------------------------------------------------------------------

export interface RadioProps<A> {
  /** Button label. */
  label: string;
  /** Button tooltip. */
  title?: string;
  /** Additional class. */
  className?: string;
  /** Additional style. */
  style?: React.CSSProperties;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Value associated to the Radio. */
  value: A;
  /** Currently selected value. */
  selection?: A;
  /** Callback to changes. */
  onSelection?: (newValue: A) => void;
}

/** Radio Button. See also [[RadioGroup]]. */
export function Radio<A>(props: RadioProps<A>): JSX.Element {
  const { onSelection, value, selection } = props;
  const disabled = onSelection ? DISABLED(props) : true;
  const checked = value === selection;
  const onChange = onSelection && (() => onSelection(value));
  const baseClass = disabled ? CHECKBOX_DISABLED : CHECKBOX_ENABLED;
  const labelClass = props.className || '';
  return (
    <label
      title={props.title}
      style={props.style}
      className={baseClass + labelClass}
    >
      <input
        type="radio"
        disabled={disabled}
        checked={checked}
        onChange={onChange}
      />
      {props.label}
    </label>
  );
}

// --------------------------------------------------------------------------
// --- Radio Group
// --------------------------------------------------------------------------

export interface RadioGroupProps<A> {
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Currently selected value. */
  value?: A;
  /** Callback to selected values. */
  onChange?: (newValue: A) => void;
  /** Additional CSS class. */
  className?: string;
  /** Additional style for the `<div/>` container. */
  style?: React.CSSProperties;
  /** [[Radio]] Buttons. */
  children?: React.ReactNode;
}

/**
   Selector of Radio Buttons.  Childrens of the `RadioGroup` shall be [[Radio]]
   buttons.

   The selected value of the group is broadcasted to the radio buttons. Their
   callbacks are activated _before_ the radio group one, if any.

   If the radio group is disabled, all the radio buttons are disabled also and
   their `disabled` properties are ignored. On the contrary, when the radio
   group is enabled, the `disabled` property of each radio button is taken into
   account.

   The radio buttons inside a group are laidout in a vertical box with the
   additional styling properties.
*/
export function RadioGroup<A>(props: RadioGroupProps<A>): JSX.Element {
  const {
    className = '',
    style,
    value: selection,
    onChange: onGroupSelect,
  } = props;
  const disabledGroup = onGroupSelect ? DISABLED(props) : true;
  const makeRadio = (elt: unknown): JSX.Element => {
    const typedElt = elt as React.ReactElement<RadioProps<A>>;
    const radioProps = typedElt.props;
    const disabled = disabledGroup || DISABLED(radioProps);
    const { onSelection: onRadioSelect } = radioProps;
    const onSelection = (v: A): void => {
      if (onRadioSelect) onRadioSelect(v);
      if (onGroupSelect) onGroupSelect(v);
    };
    return React.cloneElement(typedElt, {
      disabled,
      enabled: !disabled,
      selection,
      onSelection,
    });
  };
  return (
    <div className={`dome - xRadio - group ${className} `} style={style}>
      {React.Children.map(props.children, makeRadio)}
    </div>
  );
}

// --------------------------------------------------------------------------
// --- Selector
// --------------------------------------------------------------------------

export interface SelectProps {
  /** Field identifier (to make forms or labels) */
  id?: string;
  /** Button tooltip */
  title?: string;
  /** Button placeholder */
  placeholder?: string;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Currently selected value. */
  value?: string;
  /** Keep focus when selected (default to false). */
  focus?: boolean;
  /** Callback to selected values. */
  onChange?: (newValue?: string) => void;
  /** Additional CSS class. */
  className?: string;
  /** Additional style for the `<div/>` container. */
  style?: React.CSSProperties;
  /** Shall be standard `<option/>` and `<optgroup/>` elements. */
  children?: React.ReactNode;
}

/**
   Menu Button.

   The different options shall be specified with HTML
   `< option/>` and `<optgroup/>` elements.
   Options and group shall be specified as follows:

   *   <optgroup label='…'>…</optgroup>
   *   <option value='…' disabled=… >…</option>

 */
export function SelectMenu(props: SelectProps): JSX.Element {
  const { onChange, placeholder, focus = false, value = '' } = props;
  const className = classes(
    'dome-xSelect dome-xBoxButton dome-xButton-default dome-xButton-label',
    props.className,
  );
  const disabled = onChange ? DISABLED(props) : true;
  const callback = (evt: React.ChangeEvent<HTMLSelectElement>): void => {
    if (onChange) onChange(evt.target.value);
    const target = evt.target;
    if (!focus) setImmediate(() => target.blur());
  };
  return (
    <select
      id={props.id}
      disabled={disabled}
      className={className}
      style={props.style}
      title={props.title}
      value={value}
      onChange={callback}
    >
      {placeholder && <option value="">— {placeholder} —</option>}
      {props.children}
    </select >
  );
}

/** ButtonProps without properties managed by `SelectButton`,
    plus an unique id for each element. */
export type SelectButtonElement =
  Omit<ToolBar.ButtonProps<string>,
    'onClick' | 'selected' | 'selection' | 'children'>
  & { id: string }

export interface SelectButtonProps {
  /** List of buttons. */
  buttonList: SelectButtonElement[];
  /** Button selected in the group. */
  selected: string;
  /** Called when the selected button changes. */
  onSelection: (a: string) => void;
  /** Vertical separation, default to true. */
  verticalSep?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Additional CSS class. */
  className?: string;
  /** Additional style for the `<div/> ` container. */
  style?: React.CSSProperties;
}

/** Selection from a group of buttons.
    Only one button from the group can be selected at a time. */
export function SelectButton(props: SelectButtonProps)
  : React.JSX.Element | null {
  const { buttonList, selected, onSelection,
    disabled = false, verticalSep = true } = props;

  const className = classes(
    'dome-xMenu-Item',
    'dome-xMenu-Item-Button',
    disabled && 'dome-xMenu-Item-disabled',
    props.className
  );

  if (buttonList.length < 2) return null;
  return (
    <div className={className}>
      <ToolBar.ButtonGroup disabled={disabled} verticalSep={verticalSep}>
        {buttonList.map(b =>
          <ToolBar.Button {...b}
            key={b.id}
            selected={b.id === selected}
            onClick={() => onSelection(b.id)}
          />
        )}
      </ToolBar.ButtonGroup>
    </div>
  );
}


// --------------------------------------------------------------------------
// --- Multiselect
// --------------------------------------------------------------------------

export interface ItemProps {
  /** Item label. */
  label: string;
  /** Optional menu identifier. */
  id?: string;
  /** tooltip */
  title?: string
  /** Displayed item, default is `true`. */
  display?: boolean;
  /** Enabled item, default is `true`. */
  enabled?: boolean;
  /** Checked item, default is `false`. */
  checked?: boolean;
  /** Item selection callback. */
  onClick: (() => void);
}
export type MultiselectItemProps = ItemProps | 'separator'

export function MultiselectItem({ item }: { item: MultiselectItemProps })
  : React.JSX.Element | null {
  if (item === 'separator')
    return <div className='dome-xMenu-Item-separator' />;

  const className = classes(
    'dome-xMenu-Item',
    !item.enabled && 'dome-xMenu-Item-disabled'
  );

  if (item.display === false) return null;
  return <div title={item.title} className={className} id={item.id}
    onClick={() => item.enabled && item.onClick()}
  >
    <Icon id='CHECK' size={14} visible={item.checked} />
    <div>{item.label}</div>
  </div>;
}

export function Multiselect(
  { title, children }: { title?: string, children: React.ReactNode }
): React.JSX.Element {
  return (
    <Vbox>
      {title &&
        <Vbox className='dome-xMenu-Item-title dome-xMenu-Item-separator'>
          {title}
        </Vbox>
      }
      {children}
    </Vbox>
  );
}

// --------------------------------------------------------------------------
// --- Spinner
// --------------------------------------------------------------------------

export interface SpinnerProps {
  /** Field identifier (to make forms or labels) */
  id?: string;
  /** Button tooltip */
  title?: string;
  /** Button placeholder */
  placeholder?: string;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Currently selected value. */
  value?: number;
  /** Minimum value. */
  vmin?: number;
  /** Maximum value. */
  vmax?: number;
  /** Increment and Decrement step. */
  vstep?: number;
  /** Callback to selected values. */
  onChange?: (newValue: number) => void;
  /** Additional CSS class. */
  className?: string;
  /** Additional style for the `<div/> ` container. */
  style?: React.CSSProperties;
}

export function Spinner(props: SpinnerProps): JSX.Element {
  const { onChange, value = '' } = props;
  const className = classes('dome-xSpinner', props.className);
  const disabled = onChange ? DISABLED(props) : true;
  const callback = (evt: React.ChangeEvent<HTMLInputElement>): void => {
    if (onChange) {
      const newValue = Number.parseInt(evt.target.value);
      if (!Number.isNaN(newValue)) onChange(newValue);
    }
  };
  return (
    <input
      id={props.id}
      title={props.title}
      type="number"
      value={value}
      min={props.vmin}
      max={props.vmax}
      step={props.vstep}
      className={className}
      style={props.style}
      disabled={disabled}
      placeholder={props.placeholder}
      onChange={callback}
    />
  );
}

// --------------------------------------------------------------------------
// --- Text Input
// --------------------------------------------------------------------------

export interface FieldProps {
  /** Field identifier (to make forms or labels) */
  id?: string;
  /** Button tooltip */
  title?: string;
  /** Button placeholder */
  placeholder?: string;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Default fo `false`. */
  autoFocus?: boolean;
  /** Currently selected value (updated on `ENTER` key) */
  value?: string;
  /** Callback on `ENTER` key. */
  onChange?: (newValue: string) => void;
  /** Callback on every modification. */
  onEdited?: (tmpValue: string) => void;
  /** Additional CSS class. */
  className?: string;
  /** Additional style for the `<div/> ` container. */
  style?: React.CSSProperties;
}

/**
   Text Field.
*/
export const Field = (props: FieldProps): JSX.Element => {
  const { className = '', onChange, onEdited, value = '' } = props;
  const [current, setCurrent] = React.useState<string>(value);
  const disabled = (onChange || onEdited) ? DISABLED(props) : true;
  const ONCHANGE = (evt: React.ChangeEvent<HTMLInputElement>): void => {
    const text = evt.target.value || '';
    setCurrent(text);
    if (onEdited) onEdited(text);
  };
  const ONKEYPRESS = (evt: React.KeyboardEvent): void => {
    switch (evt.key) {
      case 'Enter':
        if (onChange) onChange(current);
        break;
      case 'Escape':
        setCurrent(value);
        if (onChange) onChange(value);
        break;
      default:
        break;
    }
  };
  return (
    <input
      id={props.id}
      type="text"
      autoFocus={!disabled && props.autoFocus}
      value={current}
      className={`dome - xField ${className} `}
      style={props.style}
      disabled={disabled}
      placeholder={props.placeholder}
      onKeyDown={ONKEYPRESS}
      onChange={ONCHANGE}
    />
  );
};

// --------------------------------------------------------------------------
