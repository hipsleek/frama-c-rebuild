/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- ToolBars
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/frame/toolbars
 */

import React from 'react';
import * as Dome from 'dome';
import { Icon, SVG } from 'dome/controls/icons';
import { Label } from 'dome/controls/labels';
import { classes } from 'dome/misc/utils';
import './style.css';
import { GlobalState, useGlobalState } from 'dome/data/states';
import { Tooltip } from 'dome/dialogs';
import { LED, LEDstatus } from 'dome/controls/displays';

// --------------------------------------------------------------------------
// --- ToolBar Container
// --------------------------------------------------------------------------

export interface ToolBarProps {
  className?: string;
  style?: React.CSSProperties;
  children?: React.ReactNode;
}

/**
   @class
   Container for toolbar items.
 */
export function ToolBar(props: ToolBarProps): JSX.Element | null {
  const { children } = props;
  const n = React.Children.count(children);
  if (n === 0) return null;
  const className = classes(
    'dome-xToolBar',
    'dome-color-frame',
    props.className,
  );
  return (
    <div className={className} style={props.style}>
      <div className="dome-xToolBar-inset" />
      {children}
      <div className="dome-xToolBar-inset" />
    </div>
  );
}

// --------------------------------------------------------------------------
// --- ToolBar Spaces
// --------------------------------------------------------------------------

/** Fixed (tiny) space. */
export const Inset = (): JSX.Element => (
  <div className="dome-xToolBar-inset" />
);

/** Fixed space. */
export const Space = (): JSX.Element => (
  <div className="dome-xToolBar-space" />
);

/** Auto-extensible space. */
export const Filler = (): JSX.Element => (
  <div className="dome-xToolBar-filler" />
);

/** Fixed space with vertical rule. */
export const Separator = (): JSX.Element => (
  <div className="dome-xToolBar-separator">
    <div className="dome-xToolBar-vrule" />
  </div>
);

// --------------------------------------------------------------------------
// --- ToolBar Group
// --------------------------------------------------------------------------

export interface GroupProps {
  className?: string;
  style?: React.CSSProperties;
  visible?: boolean;
  display?: boolean;
  title?: string;
  children?: React.ReactNode;
}

/** Groups other ToolBar controls together in a tied box. */
export function Group(props: GroupProps): JSX.Element {
  const { visible=true, display=true } = props;
  const allClasses = classes(
    'dome-xToolBar-group',
    props.className,
    !visible && 'dome-hidden',
    !display && 'dome-erased',
  );
  return (
    <div className={allClasses} style={props.style} title={props.title}>
      {props.children}
    </div>
  );
}

// --------------------------------------------------------------------------
// --- ToolBar pinned messages
// --------------------------------------------------------------------------

export interface PinnedMessage {
  id: string,
  message: string,
  statusMessage?: LEDstatus,
  title?: string,
  actions?: React.JSX.Element
}

const pinnedMessage = new GlobalState<PinnedMessage[]>([]);

export function addPinnedMessage(content: PinnedMessage): void {
  const messages = pinnedMessage.getValue();
  const filteredMessages = messages.filter(e => e.id !== content.id);
  filteredMessages.push(content);
  pinnedMessage.setValue(filteredMessages);
}

export function delPinnedMessage(id: string): void {
  const list = pinnedMessage.getValue();
  const filteredList = list.filter(e => e.id !== id);
  pinnedMessage.setValue(filteredList);
}

export function IconPinnedMessage(): React.ReactNode {
  const icon = <Icon id='PIN' kind='warning'
    className='dome-xIcon-pinned' size={16} />;
  const [ messages, ] = useGlobalState(pinnedMessage);

  if(messages.length <= 0) return null;
  return (
    <Tooltip control={icon}>
      <div className='dome-xIcon-pinned-content'>
        { messages.map(e =>
            <div key={e.id}>
              <div className='message' title={e.title}>
                <LED
                  status={e.statusMessage ?? 'warning'}
                  style={{ width: '10px', height: '10px' }}
                />
                <div>{e.message}</div>
              </div>
              <div className='action'>{e.actions}</div>
            </div>)
        }
      </div>
    </Tooltip>
  );
}

// --------------------------------------------------------------------------
// --- ToolBar Controls
// --------------------------------------------------------------------------

const SELECT = 'dome-xToolBar-control dome-selected';
const BUTTON = 'dome-xToolBar-control dome-color-frame';
const KIND = (kind: undefined | string): string => (
  kind ? ` dome-xToolBar-${kind}` : ''
);

export type ButtonKind =
  | 'default' | 'cancel' | 'warning' | 'positive' | 'negative';

export interface ButtonProps<A> {
  /** Button icon, Cf. [gallery](../../doc/guides/icons.md). */
  icon?: string;
  /** Button label. */
  label?: string;
  /** Button tooltip text. */
  title?: string;
  /** Button kind. */
  kind?: ButtonKind;
  /** Button is displayed (default `true`). */
  visible?: boolean;
  /** Button is hidden (default `false`). */
  hidden?: boolean;
  /** Enabled State (default `true`). */
  enabled?: boolean;
  /** Disabled State (default `false`). */
  disabled?: boolean;
  /** Selection State (defaults to `false` or `selection` equal to `value`). */
  selected?: boolean;
  /** Button's value. */
  value?: A;
  /** Currently selected value. */
  selection?: A;
  /** Selection callback. Receives the button's value. */
  onClick?: (value: A | undefined, evt:React.MouseEvent) => void;
  /** Right-Click callback. Receives the button's value. */
  onContextMenu?: (value: A | undefined, evt:React.MouseEvent) => void;
  /** Callback for other mouse button click. Receives the button's value. */
  onAuxClick?: (value: A | undefined, evt:React.MouseEvent) => void;
  /** Further Styling */
  className?: string;
  /** Button contents */
  children?: React.ReactNode;
}

/** Toolbar Button. */
export function Button<A = undefined>(
  props: ButtonProps<A>
): JSX.Element | null {
  const { visible = true, hidden = false } = props;
  if (!visible || hidden) return null;
  const { enabled = true, disabled = false } = props;
  const { selected, value, selection } = props;
  const { onClick, onContextMenu, onAuxClick } = props;
  const isSelected = selected !== undefined
    ? selected : (value !== undefined && value === selection);
  const className = classes(
    isSelected ? SELECT : (BUTTON + KIND(props.kind)),
    props.className,
  );
  return (
    <button
      type="button"
      disabled={disabled || !enabled}
      className={className}
      onClick={onClick && ((evt) => onClick(value, evt))}
      onContextMenu={onContextMenu && ((evt) => onContextMenu(value, evt))}
      onAuxClick={onAuxClick && ((evt) => onAuxClick(value, evt))}
      title={props.title}
    >
      {props.icon && <SVG offset={-1} id={props.icon} />}
      {props.label && <label>{props.label}</label>}
      {props.children}
    </button>
  );
}

export interface SwitchProps {
  /** Switch tooltip. */
  title?: string;
  /** Additional class. */
  className?: string;
  /** Additional style. */
  style?: React.CSSProperties;
  /** Defaults to `true`. */
  enabled?: boolean;
  /** Defaults to `false`. */
  disabled?: boolean;
  /** Switch position. Defaults to 'left'. */
  position?: 'left' | 'right';
  /** Click callback. */
  onChange?: (newPosition: 'left' | 'right') => void;
}

/** Toolbar Left/Right Switch. */
export function Switch(props: SwitchProps): JSX.Element | null {
  const { position, onChange } = props;
  const checked = position === 'right';
  const { title, className, style } = props;
  const { enabled = true, disabled = false } = props;
  const callback = onChange && (() => onChange(checked ? 'left' : 'right'));
  if (disabled || !enabled) return null;
  return (
    <label className={classes('dome-xSwitch', className)} style={style}>
      <input type={'checkbox'} checked={checked} onChange={callback} />
      <span className={'dome-xSwitch-slider'} title={title} />
    </label >
  );
}

// --------------------------------------------------------------------------
// --- Selection Props
// --------------------------------------------------------------------------

export interface SelectionProps<A> {
  title?: string;
  /** Enabled Group (default `true`). */
  enabled?: boolean;
  /** Disabled Group (default `false`). */
  disabled?: boolean;
  /** Currently selected button. */
  value?: A;
  /** Callback on clicked buttons. */
  onChange?: (value: undefined | A) => void;
  /** Buttons array. */
  children: React.ReactElement[];
}

// --------------------------------------------------------------------------
// --- ToolBar Button Group
// --------------------------------------------------------------------------

export interface ButtonGroupProps<A> extends SelectionProps<A> {
  /** add vertical separation between button */
  verticalSep?: boolean;
  /** Additional class. */
  className?: string;
  /** Additional style. */
  style?: React.CSSProperties;
}

/**
   Toolbar Button Group.

   Properties of the button group are passed down the buttons of the group
   as appropriate defaults.
 */
export function ButtonGroup<A>(props: ButtonGroupProps<A>): JSX.Element {
  const { children, value, onChange, enabled, disabled,
    verticalSep = false } = props;
  const baseProps: ButtonProps<A> = {
    enabled,
    disabled,
    selection: value,
    onClick: onChange,
  };
  const className = classes(
    'dome-xToolBar-buttongroup',
    verticalSep && 'dome-xToolBar-buttongroup-separation',
    props.className);
  return (
    <div className={className} style={props.style}>
      {React.Children.map(children, (elt) => React.cloneElement(
        elt,
        { ...baseProps, ...elt.props },
      ))}
    </div>
  );
}

// --------------------------------------------------------------------------
// --- ToolBar Menu
// --------------------------------------------------------------------------

export interface SelectProps extends SelectionProps<string>
{
  className?: string;
  style?: React.CSSProperties;
}

/** Toolbar Selector Menu.

   Behaves likes a standard `<select>` element, except that callback directly
   receives the select value, not the entire event.
   The list of options shall be given with standard
   `<option value={...} label={...}>` elements.
 */
export function Select(props: SelectProps): JSX.Element {
  const { enabled = true, disabled = false, onChange } = props;
  const callback =
    (evt: React.ChangeEvent<HTMLSelectElement>): void => {
      if (onChange) onChange(evt.target.value);
    };
  const className = classes(
    'dome-xToolBar-control dome-color-frame',
    props.className
  );
  return (
    <select
      className={className}
      style={props.style}
      title={props.title}
      value={props.value}
      disabled={disabled || !enabled}
      onChange={callback}
    >
      {props.children}
    </select>
  );
}

// --------------------------------------------------------------------------
// --- ModalSearchField necessary types
// --------------------------------------------------------------------------

/** Description of a hint used to populate the suggestions. */
export interface Hint {
  id: string;
  label: string;
  icon?: string;
  title?: string;
  onClick?: () => void;
}

// --------------------------------------------------------------------------
// --- ModalSearchField mode button component
// --------------------------------------------------------------------------

interface ModeButtonComponentProps {
  icon: string;
  disabled: boolean;
  className: string;
  onClick?: () => void;
}

function ModeButton(props: ModeButtonComponentProps): JSX.Element {
  const { icon, disabled, onClick } = props;
  const className = classes(
    'dome-xToolBar-modeSelection',
    disabled ? 'dome-xToolBar-disabledMode' : props.className
  );
  return (
    <div className={className} onClick={onClick} >
      <SVG
        className="dome-xToolBar-modeIcon"
        id={icon}
        offset={-1}
      />
    </div>
  );
}

// --------------------------------------------------------------------------
// --- ModalSearchField suggestions component
// --------------------------------------------------------------------------

interface SuggestionsProps {
  index: number;
  hints: Hint[];
  onClose: () => void;
  onHint?: (hint: Hint) => void;
}

function scrollToRef(r: null | HTMLLabelElement): void {
  if (r) r.scrollIntoView({ block: 'nearest' });
}

function Suggestions(props: SuggestionsProps): JSX.Element {
  const { hints, onHint, index, onClose } = props;
  const suggestions = hints.slice(0, 100).map((h, k) => {
    const selected = k === index || hints.length === 1;
    const classSelected = selected && 'dome-xToolBar-searchIndex';
    const className = classes('dome-xToolBar-searchItem', classSelected);
    const onClick = (): void => {
      onClose();
      if (h.onClick) h.onClick();
      if (onHint) onHint(h);
    };
    return (
      <Label
        ref={selected ? scrollToRef : undefined}
        key={h.id}
        icon={h.icon}
        title={h.title}
        className={className}
        onClick={onClick}
      >
        {h.label}
      </Label>
    );
  });

  // Rendering the component.
  return (
    <div
      style={{ visibility: suggestions.length > 0 ? 'visible' : 'hidden' }}
      className='dome-xToolBar-suggestions'
      onMouseDown={(event) => event.preventDefault()}
    >
      {suggestions}
      {hints.length > 100 ?
        <Label>({hints.length - 100} omitted)</Label> :
        null}
    </div>
  );
}

// --------------------------------------------------------------------------
// --- ModalSearchField input field component
// --------------------------------------------------------------------------

interface SearchInputProps {
  title?: string;
  placeholder?: string;
  disabled: boolean;
  hints: Hint[];
  onHint?: (hint: Hint) => void;
  onEnter?: (pattern: string) => void;
  onClose: () => void;
  index: number;
  setIndex: (n: number) => void;
  pattern: string;
  setPattern: (pattern: string) => void;
  inputRef: React.MutableRefObject<HTMLInputElement | null>;
}

function SearchInput(props: SearchInputProps): JSX.Element {
  const {
    title, placeholder, disabled,
    hints=[], onHint, onEnter, onClose,
    index, setIndex, pattern, setPattern, inputRef
  } = props;

  // Key Up Events
  const onKeyUp = (evt: React.KeyboardEvent): void => {
    switch (evt.key) {
      case 'Escape':
        onClose();
        break;
      case 'Enter':
        onClose();
        {
          const hint : Hint | undefined =
            hints[index] ??
            (hints.length === 1 ? hints[0] : undefined);
          if (hint && hint.onClick) hint.onClick();
          if (hint && onHint) onHint(hint);
          if (onEnter) onEnter(pattern);
        }
        break;
      case 'ArrowUp':
        if (index < 0) setIndex(hints.length - 1);
        if (index > 0) setIndex(index - 1);
        break;
      case 'ArrowDown':
        if (index < 0 && 0 < hints.length) setIndex(0);
        if (0 <= index && index < hints.length - 1) setIndex(index + 1);
        break;
    }
  };

  // Key Down Events. Disables the default behavior on ArrowUp and ArrowDown.
  const onKeyDown = (evt: React.KeyboardEvent): void => {
    switch (evt.key) {
      case 'ArrowUp':
      case 'ArrowDown':
        evt.preventDefault();
        break;
    }
  };

  // // Input Events
  const onChange = (evt: React.ChangeEvent<HTMLInputElement>): void => {
    setIndex(-1);
    setPattern(evt.target.value);
  };

  return (
    <input
      type="search"
      placeholder={placeholder}
      disabled={disabled}
      ref={inputRef}
      title={title}
      value={pattern}
      onKeyUp={onKeyUp}
      onKeyDown={onKeyDown}
      onChange={onChange}
      onBlur={onClose}
    />
  );
}

// --------------------------------------------------------------------------
// --- SearchField
// --------------------------------------------------------------------------

export interface SearchFieldProps {
  /** Mode tooltip title. */
  title?: string;
  /** Mode placeholder text. */
  placeholder?: string;
  /** Search Icon. */
  icon?: string;
  /** Search Icon class. */
  className?: string;
  /** Enabled State (default `true`). */
  enabled?: boolean;
  /** Disabled State (default `false`). */
  disabled?: boolean;
  /** Hints. */
  hints?: Hint[];
  /** Hint selection callback. */
  onHint?: (hint: Hint) => void;
  /** Current search pattern. Useful to update hints. */
  onPattern?: (pattern: string) => void;
  /** Search to perform when Enter is hit. Useful for modes without hints. */
  onEnter?: (pattern: string) => void;
  /** Signal to trigger a focus request from the search field. */
  focus?: Dome.Event<void>;
  /** Click on the Search Icon. */
  onSearch?: () => void;
  /** Focus gained by the Search Field. */
  onFocus?: () => void;
  /** Focus lost by the Search Field. */
  onBlur?: () => void;
}

export function SearchField(props: SearchFieldProps): JSX.Element {
  const inputRef = React.useRef<HTMLInputElement | null>(null);
  const [index, setIndex] = React.useState(-1);
  const [pattern, setPattern] = React.useState('');
  const { onPattern } = props;
  const onFocus = React.useCallback(() => inputRef.current?.focus(), []);
  const onClose = React.useCallback(() => {
    inputRef.current?.blur();
    setPattern('');
    setIndex(-1);
    if (onPattern) onPattern('');
  }, [onPattern]);
  const onPatternChanged = React.useCallback((p: string) => {
    setPattern(p);
    if (onPattern) onPattern(p);
  }, [onPattern]);
  Dome.useEvent(props.focus, onFocus);

  // Compute the hints for the current mode.
  const {
    hints=[], onHint, onEnter,
    icon='SEARCH',
    className='dome-xToolBar-searchMode',
    enabled=true, disabled=false,
  } = props;

  const disabledInput = disabled || !enabled;
  React.useEffect(
    () => { if (disabledInput) inputRef.current?.blur(); }
    , [disabledInput]);

  // Build the component.
  return (
    <div className="dome-xToolBar-searchComponent"
         onBlur={props.onBlur}
         onFocus={props.onFocus}>
      <div className="dome-xToolBar-searchField">
        <ModeButton
          icon={icon}
          disabled={disabledInput}
          className={className}
          onClick={props.onSearch} />
        <SearchInput
          title={props.title}
          placeholder={props.placeholder}
          disabled={disabledInput}
          hints={hints}
          onHint={onHint}
          onEnter={onEnter}
          onClose={onClose}
          index={index}
          setIndex={setIndex}
          pattern={pattern}
          setPattern={onPatternChanged}
          inputRef={inputRef}
        />
      </div>
      <Suggestions
        index={index}
        hints={hints}
        onClose={onClose}
        onHint={onHint} />
    </div>
  );
}

// --------------------------------------------------------------------------
