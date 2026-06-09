/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
   @packageDocumentation
   @module dome/dnd

   D&D Facilities
 */

import React from 'react';
import { classes, styles } from 'dome/misc/utils';
import { swap } from 'dome/data/arrays';
import {
  DraggableCore,
  DraggableEvent,
  DraggableEventHandler
} from 'react-draggable';

/**
   Current dragging information:
   - `rect` is the original DOM Rectangle of the dragged HTML node;
   - `rootX,rootY` is the position in `rect` where the drag started from;
   - `dragX,dragY` is the current dragging position, relative to `rect`.

 */
export interface Dragging {
  rect: DOMRect;
  rootX: number;
  rootY: number;
  dragX: number;
  dragY: number;
}

/**
   Drop over information:
   - `meta` indicates if a modifier key is pressed;
   - `rect` is the original DOM Rectangle of the hovered HTML node;
   - `dropX,dropY` is the position inside `rect` where the drag hovers in;
 */
export interface Dropping {
  meta: boolean;
  rect: DOMRect;
  dropX: number;
  dropY: number;
}

/** Drag Callbacks. */
export interface DragHandler {
  /** Callback when drag is initiated. */
  onStart?: () => void;
  /** Callback current dragging. */
  onDrag?: (dragging: Dragging) => void;
  /** Callback when drag is interrupted. */
  onStop?: () => void;
}

/** Drop Callbacks. */
export interface DropHandler {
  /** Callback when the drag source enters the drop target. */
  onDropIn?: (d: Dropping) => void;
  /** Callback when the drag source leaves the drop target. */
  onDropOut?: () => void;
  /** Callback when the drag source is dropped in the target. */
  onDrop?: () => void;
}

/* -------------------------------------------------------------------------- */
/* --- DnD Controller                                                     --- */
/* -------------------------------------------------------------------------- */

let nodeId = 0;
function freshId(): string {
  while (1) {
    const id = `dome-dnd-${++nodeId}`;
    if (!document.getElementById(id))
      return id;
  }
  return '<dnd-crashed>';
}

/** Extended `DropZone` with an HTML element. */
export interface DropZone extends DropHandler {
  node: HTMLElement;
}

/** D&D Controller.

   This class allows to connect Drag Sources and Drop Target with each others.
   You shall never use the methods of the `DnD` class directly.

   The preferred way for creating `DnD` classes is to use the `useDnD()`
   React Hook.
*/
export class DnD {

  constructor() {
    this.handleKey = this.handleKey.bind(this);
  }

  private registry = new Map<string, DropZone>();
  private dragging: HTMLElement | undefined;
  private dropping: Dropping | undefined;
  private hovering: DropZone | undefined;

  onDropZone(zone: DropZone): string {
    const node = zone.node;
    const id = node.id ? node.id : node.id = freshId();
    this.registry.set(id, zone);
    return id;
  }

  offDropZone(id: string): void {
    this.registry.delete(id);
  }

  handleStart(node: HTMLElement): void {
    this.dragging = node;
    const body = document.body;
    body.addEventListener('keyup', this.handleKey);
    body.addEventListener('keydown', this.handleKey);
  }

  handleEvent(e: DraggableEvent): void {
    if (this.dragging && e instanceof MouseEvent) {
      const element = document
        .elementsFromPoint(e.clientX, e.clientY)
        .find((elt) => elt !== this.dragging && this.registry.get(elt.id));
      const hover = element ? this.registry.get(element.id) : undefined;
      const curr = this.hovering;
      if (hover !== curr) {
        this.hovering = hover;
        if (curr && curr.onDropOut) {
          this.dropping = undefined;
          curr.onDropOut();
        }
      }
      if (hover && hover.onDropIn) {
        const meta = e.altKey || e.ctrlKey || e.shiftKey || e.metaKey;
        const rect = hover.node.getBoundingClientRect();
        const dropX = Math.round(e.clientX - rect.left);
        const dropY = Math.round(e.clientY - rect.top);
        const d = this.dropping = { meta, rect, dropX, dropY };
        hover.onDropIn(d);
      }
    }
  }

  handleKey(e: KeyboardEvent): void {
    const callback = this.hovering?.onDropIn;
    const drop = this.dropping;
    if (callback && drop) {
      const meta = e.altKey || e.ctrlKey || e.shiftKey || e.metaKey;
      if (meta !== drop.meta) {
        drop.meta = meta;
        callback(drop);
      }
    }
  }

  handleDrop(): void {
    const body = document.body;
    body.removeEventListener('keyup', this.handleKey);
    body.removeEventListener('keydown', this.handleKey);
    this.dragging = undefined;
    const target = this.hovering;
    if (target) {
      this.hovering = undefined;
      if (target.onDrop) target.onDrop();
    }
  }
}

/** React Hook for creating a local DnD controller. */
export function useDnD(): DnD {
  return React.useMemo(() => new DnD(), []);
}

/* -------------------------------------------------------------------------- */
/* --- Drop Targets                                                       --- */
/* -------------------------------------------------------------------------- */

/** React Hook for connecting a drop target to a DnD controller and drop event
   callbacks.

   Usage: the hook returns a Rect reference `r` that you shall pass to the HTML
   `<div ref={r}/>` element of your drop target.  Once this element is mounted
   into the DOM, the DnD controller and your handler will start receiving drag
   and drop events.

   Undefined DnD controller and handlers switch off Drag & Drop events from the
   drop target.

   Alternatively, you can also use `<DropTarget/>` and `<DragSource/>`
   components that already offers such a `<div/>` element connected to a DnD
   controller.
*/
export function useDropTarget(
  dnd: DnD | undefined, handlers?: DropHandler
): React.RefObject<HTMLDivElement> {
  const nodeRef = React.useRef<HTMLDivElement>(null);
  const onDrop = handlers?.onDrop;
  const onDropIn = handlers?.onDropIn;
  const onDropOut = handlers?.onDropOut;
  const node = nodeRef.current;
  React.useEffect(() => {
    if (dnd && node && (onDrop || onDropIn || onDropOut)) {
      const id = dnd.onDropZone({ node, onDrop, onDropIn, onDropOut });
      return () => dnd.offDropZone(id);
    }
    return;
  }, [dnd, node, onDrop, onDropIn, onDropOut]);
  return nodeRef;
}

export interface DropTargetProps extends DropHandler {
  /** The DnD controller to register in. */
  dnd?: DnD;
  /** Disable dropping. */
  disabled?: boolean;
  /** Class the `<div/>` element. */
  className?: string;
  /** Style the `<div/>` element. */
  style?: React.CSSProperties;
  /** Contents of the `<div/>` element. */
  children?: React.ReactNode;
}

/** A container `<div/>` element that can be dropped in when dragging some
   DragSource connected to the specified DnD controller.

   See also `<DragSource/>` component that can also behaves as a Drop target.
   If you need a more precise control over the underlying `<div/>` element,
   refer to the `useDropTarget()` React Hook.  */
export function DropTarget(props: DropTargetProps): JSX.Element {
  const { dnd, disabled = false, className, style, children } = props;
  const nodeRef = useDropTarget(dnd, disabled ? undefined : props);
  return (
    <div ref={nodeRef} className={className} style={style}>
      {children}
    </div>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Drag Source                                                        --- */
/* -------------------------------------------------------------------------- */

interface OverlayRendering {
  outerClass?: string;
  innerClass?: string;
  outerStyle?: React.CSSProperties;
  innerStyle?: React.CSSProperties;
}

function RenderOverlay(
  props: DragSourceProps,
  dragging: Dragging | undefined,
): OverlayRendering {
  const { className, style } = props;
  if (dragging) {
    const { dragX, dragY, rootX, rootY, rect } = dragging;
    const { left, top, width, height } = rect;
    const {
      zIndex = 1,
      offsetX = 0,
      offsetY = 0,
      classDragged = 'dome-dragged',
      classDragging = 'dome-dragging',
    } = props;
    const position: React.CSSProperties = {
      position: 'fixed',
      left: left + offsetX + dragX - rootX,
      top: top + offsetY + dragY - rootY,
      width, height, zIndex, margin: 0
    };
    const holder = { width, height };
    return {
      outerClass: classes(className, classDragged),
      innerClass: classes(className, classDragging),
      outerStyle: styles(style, props.styleDragged, holder),
      innerStyle: styles(style, props.styleDragging, position),
    };
  }
  return { outerClass: className, outerStyle: style };
}

/** Can be used to dynamically render an element with respect to current
   dragging state. The parameter `d` is `undefined` when there is no current
   dragging action. Otherwise, `d` contains the relevant dragging data.  */
export type DraggingRenderer = (d: Dragging | undefined) => JSX.Element;

export interface DragSourceProps extends DragHandler, DropHandler {
  /** The DnD controller to register in. */
  dnd?: DnD;
  /** Disabled dragging. */
  disabled?: boolean;
  /** Class of the element from where a drag can be initiated. */
  handle?: string;
  /** Class of the DragSource elements. */
  className?: string;
  /** Style of the DragSource elements. */
  style?: React.CSSProperties;
  /** Additional class for the dragged (initial) element.
     Default is `'dome-dragged'`. */
  classDragged?: string;
  /** Additional class for the dragging (moved) element.
     Default is `'dome-dragging'`. */
  classDragging?: string;
  /** Additional style for the dragged (initial) element. */
  styleDragged?: React.CSSProperties;
  /** Additional style for the dragging (moved) element. */
  styleDragging?: React.CSSProperties;
  /** X-offset when dragging (defaults to 0). */
  offsetX?: number;
  /** Y-offset when dragging (defaults to 0). */
  offsetY?: number;
  /** Z-index when dragging (defaults to 1). */
  zIndex?: number;
  /** Inner contents of the DragSource element. */
  children?: React.ReactNode | DraggingRenderer;
}

/** This container can be dragged around all over the application window. Its
   content is rendered inside a double `<div/>`, the outer one being fixed when
   dragged, and the inner one being moved around when dragging.

   The content of the inner most `<div/>` can be rendered dynamically by using a
   function of type `DraggingRenderer`.

   When a Drag Source has Drop Handler callbacks, the element is also registered
   as a Drop Target into the DnD controller.
 */
export function DragSource(props: DragSourceProps): JSX.Element {
  // --- Props
  const { dnd, disabled = false, handle, children } = props;
  const { onStart, onDrag, onStop } = props;
  // --- Dragging State
  const [dragging, setDragging] = React.useState<Dragging | undefined>();
  // --- Dropping Ref
  const nodeRef = useDropTarget(dnd, disabled ? undefined : props);
  // --- onStart
  const handleStart: DraggableEventHandler = React.useCallback(
    (_, { x, y, node }) => {
      if (dnd && nodeRef.current)
        dnd.handleStart(nodeRef.current);
      setDragging({
        rootX: x, rootY: y,
        dragX: x, dragY: y,
        rect: node.getBoundingClientRect(),
      });
      if (onStart) onStart();
    }, [dnd, nodeRef, onStart]);
  // --- onDrag
  const handleDrag: DraggableEventHandler = React.useCallback(
    (e, { x, y }) => {
      if (e && dnd) dnd.handleEvent(e);
      if (dragging) {
        const newDragging = { ...dragging, dragX: x, dragY: y };
        setDragging(newDragging);
        if (onDrag) onDrag(newDragging);
      }
    }, [dnd, dragging, onDrag]);
  // --- onStop
  const handleStop: DraggableEventHandler = React.useCallback(
    () => {
      if (dnd) dnd.handleDrop();
      setDragging(undefined);
      if (onStop) onStop();
    }, [dnd, onStop]);
  // --- Renderer
  const render = RenderOverlay(props, dragging);
  return (
    <DraggableCore
      disabled={disabled}
      handle={handle}
      onStart={handleStart}
      onDrag={handleDrag}
      onStop={handleStop}
    >
      <div
        ref={nodeRef}
        className={render.outerClass}
        style={render.outerStyle}
      >
        <div
          className={render.innerClass}
          style={render.innerStyle}
        >
          {typeof (children) === 'function' ? children(dragging) : children}
        </div>
      </div>
    </DraggableCore>
  );
}

/* -------------------------------------------------------------------------- */
/* --- List Container                                                     --- */
/* -------------------------------------------------------------------------- */

interface ListContext {
  dnd?: DnD;
  items?: string[];
  setSource?: (id: string) => void;
  setTarget?: (id: string) => void;
  onStop?: () => void;
}

// Propagates the englobing List container callbacks down to ListItem elements
const CurrentList = React.createContext<ListContext>({});

function getItem(ordered: string[] | undefined, id: string): number {
  if (ordered === undefined) return -1;
  const k = ordered.indexOf(id);
  return 0 <= k ? k : ordered.push(id);
}

/** List Item properties. */
export interface ItemProps {
  id: string; /** Shall be unique inside the same `<List/>` container. */
  className?: string; /** Additional class for the List Item contents. */
  style?: React.CSSProperties; /** Additional style for the List Item contents. */
  children?: React.ReactNode; /** List Item contents. */
}

/** List item component. Shall only be used inside a `<List/>` component.  The
   item contents is rendered inside a `<DragSource/>` component automatically
   connected to the englobing `<List/>` DnD controller.  */
export function Item(props: ItemProps): JSX.Element {
  // --- Ordering
  const { dnd, items, setSource, setTarget, onStop } =
    React.useContext(CurrentList);
  const { id, className, children } = props;
  const order = getItem(items, id);
  // --- D&D Events
  const onStart = React.useCallback(() => {
    if (setSource) setSource(id);
  }, [setSource, id]);
  const onDropIn = React.useCallback(() => {
    if (setTarget) setTarget(id);
  }, [setTarget, id]);
  // --- Styling
  const style = styles(
    props.style,
    order < 0 && { display: 'none' },
    0 <= order && { order },
  );
  // --- Rendering
  return (
    <DragSource
      className={className}
      style={style}
      dnd={dnd}
      onStart={onStart}
      onDropIn={onDropIn}
      onStop={onStop}
    >
      {children}
    </DragSource>
  );
}

export interface ListProps {
  items?: string[];
  setItems?: (items: string[]) => void;
  children?: React.ReactNode;
}

/** Sortable list wrapper.

   This component has no DOM element on its own and shall be placed inside a
   `<div/>` component with a `flex` display, typically an horizontal or vertical
   Dome box.

   The component wraps its <Item/> children within a local DnD context and
   enable reordering them by Drag & Drop.

   The behavior of the component can be _controlled_ or _uncontrolled_ whether
   `items` and `setItems` properties are set or not. In controlled mode, the
   `items` property is not required to contains all the list elements, in which
   case the missing elements would be added to the end. Notice that `setItems`
   callback is only notified after a complete drag & drop sequence of events.
*/
export function List(props: ListProps): JSX.Element {
  const dnd = useDnD();
  const [locals, setLocals] = React.useState<string[]>([]);
  const [permut, setPermut] = React.useState<string[]>([]);
  const [anchor, setAnchor] = React.useState<string | undefined>();
  const setItems = props.setItems ?? setLocals;
  const input = props.items ?? locals;
  const items = anchor !== undefined ? permut : input;
  const setSource = React.useCallback((id: string) => {
    setAnchor(id);
    setPermut(input);
  }, [input]);
  const setTarget = React.useCallback((id: string) => {
    if (anchor !== undefined) {
      const src = permut.indexOf(anchor);
      const tgt = permut.indexOf(id);
      const res = swap(permut, src, tgt);
      setPermut(res);
    }
  }, [permut, anchor]);
  const onStop = React.useCallback(() => {
    setAnchor(undefined);
    setItems(permut);
  }, [setItems, permut]);
  const context: ListContext = {
    dnd, items,
    setSource, setTarget, onStop
  };
  return (
    <CurrentList.Provider value={context}>
      {props.children}
    </CurrentList.Provider>
  );
}

/* -------------------------------------------------------------------------- */
