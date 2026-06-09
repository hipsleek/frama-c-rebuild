# Frama-C GUI {#ivette}

Welcome to the documentation of the Frama-C user interface. This chapter
introduces basic concepts for using the GUI. Each plug-in may provide its own
dedicated documentation chapter, consult them for more information.

## Launching the GUI {#ivette-launch}

You may use `frama-c-gui` instead of `frama-c` from the command-line,
with the same options. This will launch the GUI _and_ start a Frama-C session
with the provided options. You can then further modify your command-line options
by opening the [Console](#ivette-console) view.

## General Organization {#ivette-general}

The graphical user interface is decomposed into the following main areas:

 - The left sidebar, which can be used to navigate between the different views
   and plug-ins of the platform.

 - The top toolbar, which provides shortcuts to selected views and frequently
   used actions.

 - The bottom status bar, which provides general feedback on currently running
   tasks.

 - The "Laboratory" (central area), where the different views and Frama-C
   components are displayed.

Each registered Frama-C plug-in might extend the GUI with additional views,
laboratory components, sidebar panels, status bar items and menus. Consult
the associated documentation chapters for more details.

## The Sidebar {#ivette-sidebar}

There are actually _several_ sidebars and you can use the large icons on the
left-most side of the screen to switch from one sidebar to another. The
[icon-sidebar] button in the top toolbar allows you to hide/show the
sidebars. Among them, two sidebars are especially dedicated to the GUI:

- The [icon-display] sidebar provides access to _all_ registered components and
  views that you can use in the GUI.

- The [icon-search] sidebar provides navigation to the _source declarations_ of
  your project currently analyzed by Frama-C.

## The Laboratory {#ivette-laboratory}

The principal area consists of one to four panels that you can arrange as
you want. Each panel can be resized and configured to display available
_Components_ from the Frama-C kernel and the registered plug-ins.

The [icon-display] sidebar provides access to all registered _Components_
and also provides access to some predefined combinations of components, as
_Views_. When you select a _View_, the Laboratory layout is updated to display
the relevant predefined components. The main toolbar provides quick
access to the _Views_ you already visited. You can also duplicate views and
configure them independently.

You can add individual _Components_ from the [icon-display] sidebar into your
current laboratory view: double-click a new
component to add it with its preferred layout, or right-click to choose another
registered layout.

Each component displayed in the Laboratory can be moved or resized from one
panel to the others: simply right-click on the _Component_ title bar to show its
layout menu. Components can also be docked in the status bar for later use.
The layout of docked components can be changed by right-clicking them.

You can resize the different panels by dragging the splitters. You may resize
the two top panels independently of the two bottom panels by dragging the
upper part and the lower part of the vertical splitter. Dragging the horizontal
splitter resizes the left and right panels synchronously. Dragging the
intersection of the horizontal and vertical splitters resizes all panels
synchronously.

## The Toolbar {#ivette-toolbar}

The top-most toolbar provides you quick access to Tab Views.
Right-clicking on tabs allows you to restore initial view layout or close tabs.
When restoring a _View_ to its initial layout, the extra components you may have
added to the laboratory are docked into the status bar for quick access.

On the left of _View Tabs_ the toolbar offers the following buttons:

- [icon-sidebar] to show/hide the sidebar;
- [icon-media.play] start the Frama-C session;
- [icon-reload] re-start the Frama-C session;
- [icon-media.stop] stop the Frama-C session;
- [icon-triangle.left] navigate backward;
- [icon-triangle.right] navigate forward;

On the right of _View Tabs_, the toolbar offers the following buttons:

- [icon-zoom.out] to decrease the font-size of all components;
- [icon-zoom.in] to increase the font-size of all components;
- [icon-search] to search views, components, source declarations, _etc._

The toolbar search widget can be extended by registered plug-ins to search
specific items. The keyboard shortcut `Cmd+K` provides you with instant access
to the toolbar's search widget.

## The Status Bar {#ivette-statusbar}

This area is used to provide summarized feedback on activity and running tasks
in the current Frama-C session. In case of errors, warnings or special events,
messages are raised from the status bar and may provide you with quick links to
relevant components. Docked components are also accessible from the status bar.

## The Console {#ivette-console}

Among the many available components, the _Console Component_ is dedicated to the
configuration of the Frama-C session. Depending on the status of the current
Frama-C session, this component displays:

 - the output of the currently running Frama-C session, if any;
 - the command-line options of the Frama-C session, when stopped;
 - the command-line editor panel, when configuring the Frama-C session.

The _Console Component_ title bar provides two main buttons:

 - [icon-terminal] switch on/off the command-line configuration mode;
 - [icon-media.next] activate/deactivate auto-scrolling of Frama-C output;

When in _command-line configuration_ mode, other buttons are available to navigate
through the command-line history.

There is also a predefined _Console View_ that displays the _Console Component_
and the _Messages Component_ together.
