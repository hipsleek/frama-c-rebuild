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

## GUI (Ivette)

The dev tree's graphical front-end is **Ivette** (an Electron app). It shows the SL contract
clauses, the verdict markers, and — with `-hipsleek-proof-log` — the per-function proof
detail in the Properties panel.

### One-time setup

Ivette needs **Node ≥ 20** and is built with `yarn`/`make`:

```bash
nvm use 23                # or any Node >= 20
yarn install              # in the repo root
make -C ivette api
make -C ivette app        # produces the bin/frama-c-gui launcher
```

### Running

```bash
nvm use 23
unset ELECTRON_RUN_AS_NODE         # else Electron runs headless -> "ipcMain undefined"
export ELECTRON_DISABLE_SANDBOX=1  # WSL/sandbox-less environments

HIP=$(pwd)/_build/default/hipsleek/hip.exe
bin/frama-c-gui -hipsleek -hipsleek-proof-log -hipsleek-path "$HIP" demo_hipsleek/test_ll.c
```

Notes:
- Any arguments after `bin/frama-c-gui` are passed straight to `frama-c`.
- **Give it ~20 s** after the window opens to connect — the dev server is slow to bind its
  socket on the Windows-mounted `/mnt/c` filesystem, so the functions list is briefly empty.
- Do **not** prefix the launch with `pkill` in the same command; that races the spawn. Kill
  any stale instance as a separate step first.
- In the GUI, open the **Source/AST** view to see the SL clauses + green verdict markers, and
  select a function to see its `HipSleek proof (…)` property in the **Properties** panel.

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

## Verification results in Frama-C

Each function's HipSleek verdict is reported back into Frama-C as a real property,
so it shows up on the command line, in `-report`, and in the Ivette GUI:

- The SL spec appears on the function as a clean `\hipsleek::hipsleek requires…/ensures…`
  contract clause (visible with `-print` and in the GUI source view), and the SUCCESS/FAIL
  verdict becomes that clause's **property status** (green *Valid* marker on SUCCESS).
- `[SL_pred]` view definitions appear as a global `\hipsleek::hipsleek_pred …` annotation.

### Proof detail — `-hipsleek-proof-log`

With this flag, the plugin runs HipSleek with its ESL proof log enabled and attaches the
per-function proof obligations (the SLEEK entailments — `PRE` / `POST` / `BIND` / `PRE_REC`,
each marked `[proved]` / `[unproved]`) as a **separate `HipSleek proof (…)` property** on the
function. This keeps the source comment clean: the detail is shown on demand (in `-report`,
or by selecting the function in the GUI's Properties panel), not inline in the contract.

```bash
dune exec --root . frama-c -- \
  -hipsleek -hipsleek-proof-log demo_hipsleek/test_ll.c -report -report-print-properties
```

### Translation-fidelity warnings

The C→`.ss` translation supports a subset of C. When a function body uses something that is
dropped or approximated (a cast, a global variable, `sizeof`, `switch`, `goto`, a nested
lvalue, …), the plugin emits a warning such as:

```
[hipsleek] Warning: uses_global: generated .ss may differ from your C (references global 'g')
```

so a green verdict on a lossily-translated function is never silent.

## Options

| Flag | Description |
|------|-------------|
| `-hipsleek` | Enable the HipSleek plugin |
| `-hipsleek-path <path>` | Override path to `hip.exe` (auto-detected by default) |
| `-hipsleek-output-dir <dir>` | Directory for the generated `.ss` file (default: system temp) |
| `-hipsleek-proof-log` | Capture HipSleek's ESL proof log and attach per-function proof detail as a property |
