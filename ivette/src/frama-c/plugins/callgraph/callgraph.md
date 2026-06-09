# Callgraph {#callgraph}

The callgraph represents calling relationships between functions of the program.
Each node represents a C function, and each edge from f to g indicates that
f contains a call to g.

The graph also highlights some functions, such as functions containing unproven
properties.
The [panel](#callgraph-panel) displays more information on selected functions.
The [titlebar](#callgraph-titlebar) and [toolbar](#callgraph-toolbar) contain
various options to configure the graph and show/hide some functions.

## Titlebar {#callgraph-titlebar}

The titlebar contains the following buttons:

* [icon-tunings]: filter functions appearing in the graph.
  This filter is synchronized with the sidebar function filter.
* [icon-target]: move the camera to show each node after each render.
* [icon-pin]: automatically select in the graph the node of the function
  selected in the AST.
* [icon-help]: show this help modal.

## Toolbar {#callgraph-toolbar}

The toolbar contains display and selection parameters on the left and graph
management parameters on the right.

On the left:

* The first group of buttons can hide nodes from the graph, according to their
  relation to the currently selected nodes (which are always shown).
  + Try it yourself: [button-displaymode]
* [button-select]: select in the graph a list of functions according to some
  criteria:
  + functions containing unproven properties;
  + functions selected in the `Locations` component, if a multi-selection is
    currently active;
  + functions containing tainted variables, if an Eva taint analysis has been
    performed;
  + functions within cycle in the callgraph.

On the right:

* The button `Show particles` determines whether particles are emitted to highlight
  the direction of the links. If more than 1000 edges are visible,
  they are automatically disabled.
* Edges defines the thickness of the links.
* Horizontal and vertical distance management between graph nodes.
* [icon-sidebar]: opens or closes the side [panel](#callgraph-panel).

## Graph {#callgraph-graph}

### Nodes

Each node represents a C function and may have the following icons:

* [led-warning]: the function contains unproven properties
  (the number of which is given in a tooltip).
* [led-negative]: the function contains invalid properties
  (the number of which is given in a tooltip);
* [icon-redo-orange]: the function is recursive;
* [icon-drop.filled-#882288] or [icon-drop.filled-#73BBBB]:
  the function contains tainted properties.

### Edges

Edges are oriented from caller to callees, and may have different colors
depending on the selected nodes:

* A green edge connects two selected functions.
* A red edge links a selected function node and one of its callers.
* A blue edge links a selected function node and one of its callees.

### Cycle

The graph is in 3D but is displayed as a tree, which prevents cycles from
appearing in the graph.

If a cycle is detected:

* Nodes of recursive functions have the [icon-redo-orange] icon.
* If cycles between several functions are detected, such functions can be
  selected via the [button-select] button in the [toolbar](#callgraph-toolbar).

## Panel {#callgraph-panel}

The panel displays additional information about the graph in general,
and lists the logical properties from each function selected in the graph.

The buttons above the list can be used to filter the kind of properties
shown in this panel. They are synchronised with the filters in the `Properties`
component.

At the top right, two buttons allow changing the side of the panel,
and closing it (it can be reopen via the far-right button in the toolbar).

## Shortcuts {#callgraph-shortcuts}

* In the graph:
  * Left-click: rotate the graph
  * Right-click: move in the graph
  * Mouse-wheel: zoom
 
* On nodes:
  * Left-Click: select node in the graph
  * Ctrl+click: add node to the selected graph nodes (multi-selection)
  * Alt+click: select the function in all GUI components
