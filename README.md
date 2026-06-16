# HipSleek Verification via Frama-C

This directory contains demo programs for the Frama-C HipSleek plugin, which lets you verify C programs using separation-logic specifications processed by the HipSleek engine (`hip.exe`).

## Build

From the repository root:

```bash
dune build
```

This builds both Frama-C (with the HipSleek plugin) and the `hip.exe` / `sleek.exe` binaries.

## Run

```bash
dune exec --root . frama-c -- -hipsleek <file.c>
```

Example:

```bash
dune exec --root . frama-c -- -hipsleek demo_hipsleek/test_ll.c
```

Expected output:

```
[hipsleek] [HipSleek] append: SUCCESS
[hipsleek] [HipSleek] length: SUCCESS
```

## Annotation Syntax

All HipSleek annotations are written as special C comments so the file remains valid C. Two kinds are supported:

### 1. Predicate / view definitions — `/*[SL_pred] ... */`

Defines separation-logic predicates used in specs. Written at the top of the file (before any function that uses them).

```c
/*[SL_pred]
ll<> == self = null
  or self::node_star<p> * p::node<_,q> * q::ll<>;
*/
```

The body is HipSleek's native `.ss` view syntax and is passed through verbatim to the generated `.ss` file.

### 2. Pre/post specifications — `/*[SL] ... */`

Written immediately before the function they annotate.

```c
/*[SL]
  requires x::ll<> * y::ll<>
  ensures res::ll<>;
*/
node* append(node* x, node* y) { ... }
```

## Pointer types and field access

HipSleek's native format represents C pointer types `T*` as a wrapper type `T_star` with a single field `pdata`. The plugin performs this translation automatically:

| C source | Generated `.ss` |
|----------|----------------|
| `node* x` (parameter) | `node_star x` |
| `x->next` | `x.pdata.next` |
| `struct node { node* next; }` | `data node { node_star next; }` + `data node_star { node pdata; }` |
| `(node*)0` / `NULL` | `null` |

Predicate definitions must use `node_star` directly (as shown in the `ll<>` example above), since they are passed through verbatim.

## Example — `test_ll.c`

```c
/*[SL_pred]
ll<> == self = null
  or self::node_star<p> * p::node<_,q> * q::ll<>;
*/

typedef struct node {
  int val;
  struct node* next;
} node;

/*[SL]
  requires x::ll<>
  ensures x::ll<>;
*/
int length(node* x) {
  if (x == 0) return 0;
  return 1 + length(x->next);
}

/*[SL]
  requires x::ll<> * y::ll<>
  ensures res::ll<>;
*/
node* append(node* x, node* y) {
  if (x == 0) return y;
  x->next = append(x->next, y);
  return x;
}
```

## Options

| Flag | Description |
|------|-------------|
| `-hipsleek` | Enable the HipSleek plugin |
| `-hipsleek-path <path>` | Override path to `hip.exe` (auto-detected by default) |
| `-hipsleek-output-dir <dir>` | Directory for the generated `.ss` file (default: system temp) |
