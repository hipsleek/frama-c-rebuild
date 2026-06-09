# Dive {#dive}

Dive is a Frama-C plugin that leverages the results from [Eva](#eva), filtered
by [Studia](#eva-studia), to visualize data dependency relationships between memory
locations. Its primary goal is to identify the source of false alarms raised
during the analysis.

## Dataflow graph {#dive-dataflow-graph}

This graph illustrates memory locations as nodes and write operations as edges.
An edge exists between two nodes if, for a given program instruction, one node
is read in order to write to the other.

Nodes vary in shape, color, and outline to enhance the visualization of the
analysis results.

[legend]

- node shape:
  - ellipse: the node represents a constant
  - rectangle with simple border: the node represents a scalar memory location
  - rectangle with double border: the node represents an aggregate type (array,
    structure)
  - rhomboid: the node represents a set of memory locations
  - octagon: the node represents an alarm
- node color:
  - blue: the memory location is constant and has a single possible value
  - green: the memory location can have several values and the node is partially
    filled with dark green depending on the cardinality of the
    inferred set of possible values
  - red: the memory location can take nearly all values of its type
- node outline:
  - no outline: the memory location is never tainted
  - purple outline: the memory location may be directly tainted
  - cyan outline: the memory location may be indirectly tainted

### Titlebar

The titlebar contains the following buttons:

- [icon-pin]: enable or disable adding new nodes to the graph when code is
  selected
- [icon-lock]: when enabled, prevents nodes from being inadvertently moved
- [icon-settings]: choose whether code selection keeps nodes already added
  to the graph or only keeps nodes which are related to the current selection
- [icon-display]: choose which layout algorithm to use
- [icon-trash]: remove all nodes from the graph
- [icon-help]: show this help modal.

## Dataflow tree {#dive-dataflow-tree}

This component displays the dependencies relations as a tree. Each tree level
shows either memory location or program instructions.

- Memory location nodes can be expanded to see the list of every program
  instruction modifying the location.
- Instruction nodes can be expanded to see the list of memory locations read.
