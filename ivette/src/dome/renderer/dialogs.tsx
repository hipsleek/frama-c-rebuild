/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
   Various kind of (modal) dialogs attached to the main application window.
   @packageDocumentation
   @module dome/dialogs
 */

import React from 'react';
import ReactDOM from "react-dom";
import * as filepath from 'path';
import { ipcRenderer } from 'electron';
import { modal } from 'dome';
import * as System from 'dome/system';
import { classes, styles } from 'dome/misc/utils';
import { Label } from './controls/labels';
import { IconButton } from './controls/buttons';
import { GlobalState, RState, useGlobalState } from './data/states';
import { Icon } from './controls/icons';

// --------------------------------------------------------------------------
// --- Message Box
// --------------------------------------------------------------------------

export interface DialogButton<A> {
  label?: string;
  value?: A;
}

const defaultItems: DialogButton<boolean>[] = [
  { value: undefined },
  { value: true, label: 'Ok' },
];

const valueLabel = (v: unknown): string => {
  switch (v) {
    case undefined: return 'Cancel';
    case true: return 'Ok';
    case false: return 'No';
    default: return `${v}`;
  }
};

const itemLabel = ({ value, label }: DialogButton<unknown>): string => (
  (label || valueLabel(value))
);

const isDefault = ({ value, label }: DialogButton<unknown>): boolean => (
  (value === true || label === 'Ok' || label === 'Yes')
);

const isCancel = ({ value, label }: DialogButton<unknown>): boolean => (
  (!value || label === 'Cancel' || label === 'No')
);

export type MessageKind = 'none' | 'info' | 'error' | 'warning';

export interface MessageProps<A> {
  /** Block the interface until the message window is closed
      (default is false) */
  block?: boolean;
  /** Dialog window icon (default is `'none'`. */
  kind?: MessageKind;
  /** Message text (short sentence). */
  message: string;
  /** Message details (short sentence). */
  details?: string;
  /** Message buttons. */
  buttons?: DialogButton<A>[];
  /** Default button's value. */
  defaultValue?: A;
  /** Cancel value. */
  cancelValue?: A;
}

/**
   Show a configurable message box.

   The returned promise object is never rejected, and is resolved into:
   - the cancel value if the cancel key is pressed,
   - the default value if the enter key is pressed,
   - or the value of the clicked button otherwised.

   The promise is asynchronously resolved by default.
   For synchronous resolution, you need to use the `block` option.

   The default buttons are `"Ok"` and `"Cancel"` associated to values `true` and
   `undefined`, which are also associated to the enter and cancel keys.
   Unless specified, the default value is associated with the first button
   with 'true' value or 'Ok' or 'Yes' label,
   and the cancel value is the first button with a falsy value or 'Cancel'
   or 'No' label.
 */
export async function showMessageBox<A>(
  props: MessageProps<A>,
): Promise<A | boolean | undefined> {
  const {
    block,
    kind,
    message,
    details,
    defaultValue,
    cancelValue,
    buttons = (defaultItems as DialogButton<A | boolean>[]),
  } = props;

  const labels = buttons.map(itemLabel);
  const defaultId =
    defaultValue === undefined
      ? buttons.findIndex(isDefault)
      : buttons.findIndex((a) => a.value === defaultValue);
  let cancelId =
    cancelValue === undefined
      ? buttons.findIndex(isCancel)
      : buttons.findIndex((a) => a.value === cancelValue);

  if (cancelId === defaultId) cancelId = -1;
  const options = {
      type: kind,
      message,
      detail: details,
      defaultId,
      cancelId,
      buttons: labels,
    };

  if(block) return ipcRenderer.invoke('dome.dialog.showMessageBoxSync', options)
    .then((result) => result ? buttons[result].value : cancelValue);

  return ipcRenderer.invoke('dome.dialog.showMessageBox', options)
    .then((result) => {
      const itemIndex = result ? result.response : -1;
      return itemIndex ? buttons[itemIndex].value : cancelValue;
    });
}

// --------------------------------------------------------------------------
// --- File Dialogs
// --------------------------------------------------------------------------

const defaultPath =
  (path: string): string =>
    (filepath.extname(path) ? filepath.dirname(path) : path);

export interface FileFilter {
  /** Filter name. */
  name: string;
  /**
     Allowed extensions, _without_ dot.
     Use `['*']` to accept all files.
   */
  extensions: string[];
}

export interface FileDialogProps {
  /** Prompt message. */
  title?: string;
  /** Open button label (default is « Open »). */
  label?: string;
  /** Initially selected path. */
  path?: string;
}

export interface SaveFileProps extends FileDialogProps {
  /** File filters (default to all). */
  filters?: FileFilter[];
}

export interface OpenFileProps extends SaveFileProps {
  /** Show hidden files (default is `false`). */
  hidden?: boolean;
}

export interface OpenDirProps extends FileDialogProps {
  /** Show hidden directories (default is `false`). */
  hidden?: boolean;
}

// --------------------------------------------------------------------------
// --- openFile dialog
// --------------------------------------------------------------------------

/**
   Show a dialog for opening file.
   A file filter with `extensions:["*"]` would accept any file extension.

   The returned promise object will be asynchronously:
   - either _resolved_ with `undefined` if no file has been selected,
   - or _resolved_ with the selected path

   The promise is never rejected.
 */
export async function showOpenFile(
  props: OpenFileProps,
): Promise<string | undefined> {
  const { title, label, path, hidden = false, filters } = props;
  return ipcRenderer.invoke('dome.dialog.showOpenDialog',
    {
      title,
      buttonLabel: label,
      defaultPath: path && defaultPath(path),
      properties: (hidden ? ['openFile', 'showHiddenFiles'] : ['openFile']),
      filters,
    },
  ).then((result) => {
    if (!result.canceled && result.filePaths && result.filePaths.length > 0)
      return result.filePaths[0];
    return undefined;
  });
}

/**
   Show a dialog for opening files multiple files.
*/
export async function showOpenFiles(
  props: OpenFileProps,
): Promise<string[] | undefined> {
  const { title, label, path, hidden, filters } = props;

  return ipcRenderer.invoke('dome.dialog.showOpenDialog',
    {
      title,
      buttonLabel: label,
      defaultPath: path && defaultPath(path),
      properties: (
        hidden
          ? ['openFile', 'multiSelections', 'showHiddenFiles']
          : ['openFile', 'multiSelections']
      ),
      filters,
    },
  ).then((result) => {
    if (!result.canceled && result.filePaths && result.filePaths.length > 0)
      return result.filePaths;
    return undefined;
  });
}

// --------------------------------------------------------------------------
// --- saveFile dialog
// --------------------------------------------------------------------------

/**
   Show a dialog for saving file.

   The returned promise object will be asynchronously:
   - either _resolved_ with `undefined` when canceled,
   - or _resolved_ with the selected (single) path.

   The promise is never rejected.
*/
export async function showSaveFile(
  props: SaveFileProps,
): Promise<string | undefined> {
  const { title, label, path, filters } = props;
  return ipcRenderer.invoke('dome.dialog.showSaveDialog',
    {
      title,
      buttonLabel: label,
      defaultPath: path,
      filters,
    },
  ).then(({ canceled, filePath }) => (canceled ? undefined : filePath));
}

// --------------------------------------------------------------------------
// --- openDir dialog
// --------------------------------------------------------------------------

type openDirProperty =
  'openDirectory' | 'showHiddenFiles' | 'createDirectory' | 'promptToCreate';

/**
   Show a dialog for selecting directories.
 */
export async function showOpenDir(
  props: OpenDirProps,
): Promise<string | undefined> {
  const { title, label, path, hidden } = props;
  const properties: openDirProperty[] = ['openDirectory'];
  if (hidden) properties.push('showHiddenFiles');

  switch (System.platform) {
    case 'macos': properties.push('createDirectory'); break;
    case 'windows': properties.push('promptToCreate'); break;
    default: break;
  }

  return ipcRenderer.invoke('dome.dialog.showOpenDialog',
    {
      title,
      buttonLabel: label,
      defaultPath: path,
      properties,
    },
  ).then((result) => {
    if (!result.canceled && result.filePaths && result.filePaths.length > 0)
      return result.filePaths[0];
    return undefined;
  });
}

// --------------------------------------------------------------------------
// --- Modal
// --------------------------------------------------------------------------

export const modalLoader = new GlobalState<boolean>(false);

/**
 * ShowModal defines the content of the modal.
 * If the current modal has an onClose() function,
 * it will be called before the update.
 * The return value of onClose() can prevents the update from happening.
 */
export async function showModal(
  content: React.ReactNode,
  onClose?: () => boolean | Promise<boolean>
): Promise<void> {
  const current = modal.getValue();
  if (current === undefined || !current.onClose || await current.onClose()) {
    modalLoader.setValue(false);
    modal.setValue({ content, onClose });
  }
}
export function closeModal(): void { showModal(undefined); }

export interface ModalProps {
  /** Text of the label. Prepend to other children elements. */
  label: string;
  /** Icon identifier. Displayed on the left side of the label. */
  icon?: string;
  /** Tool-tip description. */
  title?: string;
  /** custom Classes name */
  className?: string;
  /** Actions */
  actions?: React.JSX.Element;
  /** Function onClose */
  onClose?: () => void
  /** More style */
  style?: React.CSSProperties;
  /** Modal content */
  children: JSX.Element;
}

export function Modal(
  props: ModalProps
): JSX.Element {
  const { label, title, icon, className,
    onClose, style, actions, children } = props;
  const [ isLoader, ] = useGlobalState(modalLoader);

  const contentClasses = classes('dome-xModal-content', className);
  const onCloseModal = React.useCallback((): void => {
    closeModal();
    if(onClose) onClose();
  }, [onClose]);

  React.useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent): void => {
      if (event.key === "Escape") onCloseModal();
    };

    window.addEventListener("keydown", handleKeyDown);
    return () => {
      window.removeEventListener("keydown", handleKeyDown);
    };
  }, [onCloseModal]);

  return (
    <div className={contentClasses} style={style}>
      <div className='dome-xModal-header'>
        <Label className='dome-xModal-title'
          label={label} icon={icon} title={title}
        />
        <div className='dome-xModal-actions'>
          { actions }
          <IconButton icon='CROSS' size={18} onClick={onCloseModal} />
        </div>
      </div>
      <div className='dome-xModal-body dome-xBoxes-vbox dome-xBoxes-box'>
        { isLoader &&
          <div className='dome-xModal-loader'>
            <Icon id='SPINNER' size={30}/>
          </div>
        }
        {children}
      </div>
    </div>
  );
}

// --------------------------------------------------------------------------
// --- Popup
// --------------------------------------------------------------------------

interface PopupProps {
  /** The top-left position of the popup */
  position: { top: number, left:number } | null;
  /** Popup reference */
  popupRef: React.RefObject<HTMLDivElement>;
  /** style inline */
  style?: React.CSSProperties;
  /** On mouse enter */
  onMouseEnter?: (event: React.MouseEvent<HTMLDivElement>) => void;
  /** On mouse leave */
  onMouseLeave?: (event: React.MouseEvent<HTMLDivElement>) => void;
  /** Children */
  children?: React.ReactNode;
}

// The popup is rendered in the body via the createportal function.
function Popup(props: PopupProps): JSX.Element | null {
  const { position, popupRef, style, children } = props;

  const stylePopup = styles(
    { top: position?.top, left: position?.left },
    style && { ...style }
  );

  if(children === undefined) return null;
  return ReactDOM.createPortal(
    <div
      ref={popupRef}
      className="dome-xPopup"
      style={stylePopup}
      onClick={(e: React.MouseEvent) => e.stopPropagation()}
      onMouseEnter={props.onMouseEnter}
      onMouseLeave={props.onMouseLeave}
    >{children}</div>,
    document.body
  );
}

// --------------------------------------------------------------------------
// --- Positioned popup
// --------------------------------------------------------------------------

/**
 *  Hook to track the position of the element targeted by targetRef
 */
function useElementRect(
  targetRef: React.RefObject<HTMLElement>
): DOMRect | undefined {
  const [position, setPosition] = React.useState<DOMRect>();

  React.useEffect(() => {
    if (!targetRef.current) {
      setPosition(undefined);
      return;
    }
    const trigger = targetRef.current;

    const update = (): void => {
      const rect = trigger.getBoundingClientRect();
      setPosition(rect);
    };
    update();

    window.addEventListener("scroll", update, true);
    window.addEventListener("resize", update);

    const resizeObserver = new ResizeObserver(update);
    resizeObserver.observe(trigger);

    let node: HTMLElement | null = trigger.parentElement;
    while (node) {
      resizeObserver.observe(node);
      node = node.parentElement;
    }

    return () => {
      window.removeEventListener("scroll", update, true);
      window.removeEventListener("resize", update);
      resizeObserver.disconnect();
    };
  }, [targetRef]);

  return position;
}

function useWindowSize(): {width: number, height: number} {
  const [size, setSize] = React.useState({
    width: window.innerWidth,
    height: window.innerHeight
  });

  React.useEffect(() => {
    const handleResize = (): void => {
      setSize({
        width: window.innerWidth,
        height: window.innerHeight
      });
    };

    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  return size;
}

type Position = { top: number; left: number };
const defaultPos = { top: 0, left: 0 };
/**
 *  Hook to calc the position of the popup top-left corner
 */
function usePopupPosition(
  popupRef: React.RefObject<HTMLElement>,
  controlRef: React.RefObject<HTMLElement>,
  isOpen: boolean
): Position {
  const controlRect = useElementRect(controlRef);
  const windowSize = useWindowSize();
  const [position, setPosition] = React.useState<Position>(defaultPos);

  React.useEffect(() => {
    const controlElt = controlRef.current;
    const popupElt = popupRef.current;
    const rect = controlElt?.getBoundingClientRect();

    if(!isOpen || !rect || !controlElt) setPosition(defaultPos);
    else {
      const topElement = document.elementFromPoint(
        rect.left + rect.width / 2,
        rect.top + rect.height / 2
      );
      const isCovered = Boolean(
        controlElt && !controlElt.contains(topElement) &&
        popupElt && !popupElt.contains(topElement)
      );
      const visible = !(
        rect.bottom <= 0
        || rect.top >= windowSize.height
        || rect.right <= 0
        || rect.left >= windowSize.width
      );
      if(!visible || isCovered) setPosition(defaultPos);
      else {
        /** The offset is used to prevent the anchored popup from extending
          * beyond the right/bottom edge of the screen. */
        let offsetX = 0;
        let offsetY = 0;
        if(popupElt) {
          const popupRect = popupElt.getBoundingClientRect();
          if(rect.left + popupRect.width > windowSize.width) {
            offsetX = rect.left + popupRect.width - windowSize.width;
          }
          if(rect.bottom + popupRect.height > windowSize.height) {
            offsetY = rect.height + popupRect.height;
          }
        }
        setPosition({ top: rect.bottom-offsetY, left: rect.left-offsetX });
      }
    }
  }, [controlRect, controlRef, popupRef, windowSize, isOpen]);

  return position;
}

function useClickOutsidePopup(
  refPopup: React.RefObject<HTMLElement>,
  refControl: React.RefObject<HTMLElement>,
  handler: (event: MouseEvent | TouchEvent) => void
): void {
  React.useEffect(() => {
    function handleClickOutside(event: MouseEvent | TouchEvent): void {
      if ( refPopup.current
        && !refPopup.current.contains(event.target as Node)
        && refControl.current
        && !refControl.current.contains(event.target as Node)
      ) handler(event);
    }
    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, [refPopup, refControl, handler]);
}

interface AnchoredPopupProps {
  /** Control button */
  control: React.JSX.Element;
  /** ask to open state */
  askToOpenState: RState<boolean>,
  /** Anchored popup content */
  children: React.ReactNode;
}

/**
 * After requesting to open the anchored popup,
 * it is opened in the background, the position is calculated,
 * and then the anchored popup is brought to the foreground.
 */
function AnchoredPopup(props: AnchoredPopupProps): React.ReactNode {
  const { control, askToOpenState, children } = props;
  const controlRef = React.useRef<HTMLDivElement>(null);
  const popupRef = React.useRef<HTMLDivElement>(null);

  /** Request to open the positioned popup menu */
  const [ askToOpen, setAskToOpen] = askToOpenState;

  /** if the cursor is on the popup, it will not close */
  const [ mouseOnPopup, setMouseOnPopup ] = React.useState(false);

  /**
   * The popup should be opened if askToOpen is true or
   * if the cursor is on the popup.
   */
  const isOpen = React.useMemo(() => askToOpen || mouseOnPopup,
    [askToOpen, mouseOnPopup]);

  /**
   * Top-left corner position of the popup
   * return {top: 0, left: 0} if the popup must be hidden
   */
  const position = usePopupPosition(popupRef, controlRef, isOpen);

  /** Hiding popup if the position is the default position, only used when
   * the popup should not be visible. */
  const style = styles(position === defaultPos && { zIndex: "-10" });

  /** Close when clicked outside the anchored popup */
  useClickOutsidePopup(popupRef, controlRef, () => setAskToOpen(false));

  if(!children) return null;
  return <>
    <div ref={controlRef} style={{ display: 'flex', alignItems: 'center' }} >
      { control }
    </div>
    { isOpen &&
      <Popup
        style={style}
        popupRef={popupRef}
        position={position}
        onMouseEnter={() => setMouseOnPopup(true)}
        onMouseLeave={() => setMouseOnPopup(false)}
      >{ children }</Popup>
    }
  </>;
}


// ----------------------------------------------------------------------------
// --- Dropdown
// ----------------------------------------------------------------------------

interface DropdownProps {
  /** Control button */
  control: React.JSX.Element;
  /** Anchored popup content */
  children: React.ReactNode;
}

/**
 * This component is based on the anchored popup.
 * The dropdown will open when you click on the control button.
 */
export function Dropdown(props: DropdownProps): React.ReactNode {
  const { control, children } = props;
  const askToOpenState = React.useState(false);
  /** Request to open the dropdown menu */
  const [ askToOpen, setAskToOpen] = askToOpenState;

  const dropdownControl = React.cloneElement(control, {
      onClick: () => setAskToOpen((v) => !v),
      selected: askToOpen
    });

  return (
    <AnchoredPopup
      control={dropdownControl}
      askToOpenState={askToOpenState}
    >{ children }</AnchoredPopup>
  );
}

// ----------------------------------------------------------------------------
// --- Tooltip
// ----------------------------------------------------------------------------

interface TooltipProps {
  /** Control button */
  control: React.JSX.Element;
  /** Anchored popup content */
  children: React.ReactNode;
}

/**
 * This component is based on the anchored popup.
 * The tooltip will open when you hover over the control button.
 * The control component must be compatible with the onMouseEnter and
 * onMouseLeave events.
 */
export function Tooltip(props: TooltipProps): React.ReactNode {
  const { control, children } = props;
  const askToOpenState = React.useState(false);
  /** Request to open the tooltip menu */
  const [ askToOpen, setAskToOpen] = askToOpenState;

  const tooltipControl = React.cloneElement(control, {
      // The timeout allow to delay closing if the mouse moves over the popup.
      onMouseLeave: () => setTimeout(() => setAskToOpen(false), 50),
      onMouseEnter: () => setAskToOpen(true),
      selected: askToOpen
    });

  return (
    <AnchoredPopup
      control={tooltipControl}
      askToOpenState={askToOpenState}
    >{ children }</AnchoredPopup>
  );
}

// ----------------------------------------------------------------------------
