# Region Analysis plugin (experimental)

Region is a plugin for Frama-C that implements a new memory and alias analysis.
This is a work in progress in early design stage.

## Usage

Regions are computed on demand from the registered Server requests.

Command line option `-region` can be used to compute regions for all functions
and dump on output the result analysis. With `-region-debug 1+` typed accesses
are also reported.

An API will be provided soon.

## Annotations

Behavior annotation `\region::region` and code annotation `\region::alias`
can be used to specify supplementary aliases and to name regions.
The syntax is the same:

```
//@ region [A:] lv, … , lv ;
```

Such a specification put all the l-values in the same region. A name can be
given to the region, and reused later to put other l-values on the same region.
Several regions can be specified in a single annotation.

Unnamed regions from different annotations refer to _different_ regions unless
they are aliased by sharing common l-values.

L-values in region annotations use a similar syntax to ACSL terms.
However, array access and pointer arithmetics must be specified with unlimited range, eg. `a[..]` or `*(p+(..))`.

A syntax for specifying field aliasing and input/output array shapes will be provided soon.

## Current Limitations

Currently, ACSL contracts are not taken into account except for region
annotations. Function calls are totally incomplete, and aliases that would be
produces by function calls are not _yet_ taken into account. Use `//@alias` code-annotations as a temporary work around.

## GUI Support

The Region Analysis component can be used to compute and visualize regions.
Tooltips print access types to each regions.

Colored regions means the following:
 - _Red_ regions are those accessed with incompatible types
 - _Yellow_ regions are constant pointers
 - _Orange_ regions are written pointers
 - _Green_ regions are RW regions
 - _Pink_ regions are written-only regions (might be drop)
 - _Grey_ regions are read-only regions (constant, no memory)

Regions access and localization of currently selected marker will be
provided soon.
