# Frama-C + HipSleek Integration Project

## Project Goal

Integrate **HipSleek** as a verification backend for **Frama-C**, so that Frama-C can dispatch C program verification tasks to HipSleek's separation-logic engine alongside (or instead of) its existing WP/Why3 pipeline.

## Repository Layout

```
frama-c-rebuild/
├── src/plugins/          # Frama-C plugins (eva, wp, e-acsl, ...)
│   └── wp/               # Weakest-precondition plugin — primary reference for prover integration
├── hipsleek/             # HipSleek sub-project (hip + sleek binaries)
│   ├── src/              # Core OCaml source (solver.ml, astsimp.ml, ...)
│   ├── hip.ml            # HIP entry point — verifies .ss files
│   ├── sleek.ml          # SLEEK entry point — checks .slk entailments
│   ├── CLAUDE.md         # HipSleek stabilisation log (detailed change history)
│   └── ...
├── dune-workspace        # Multi-project workspace (enables `dune build` across both)
├── dune-project          # Frama-C project (lang 3.14, frama-c package)
├── Makefile              # `make hipsleek` builds hip.exe + sleek.exe
└── CLAUDE.md             # ← this file
```

## Architecture

### Frama-C's existing prover pipeline (WP plugin)

```
C source + ACSL specs
    → Frama-C IR (Cil AST)
    → WP plugin: weakest-precondition VCs
    → Why3 dispatch → Alt-Ergo / CVC5 / Coq / ...
```

Key files in `src/plugins/wp/`:
- `prover.ml` / `prover.mli` — prover type (`Why3 | Qed | Tactical | CFG`)
- `ProverWhy3.ml` — Why3 subprocess integration
- `ProverTask.ml` — task scheduling / result collection
- `wp_parameters.ml` — CLI options

### HipSleek's verification model

- **Input format**: `.ss` files (C-like with separation-logic specs), `.slk` (entailments)
- **Verifier (hip)**: takes annotated `.ss` programs, produces SUCCESS / FAIL per procedure
- **Checker (sleek)**: standalone entailment checker
- **Binaries** (after `make hipsleek`): `hipsleek/_build/default/hip.exe`, `hipsleek/_build/default/sleek.exe`

### Integration plan (NOT YET STARTED)

To call HipSleek from Frama-C, a new plugin is needed:

1. **`src/plugins/hipsleek/`** — new Frama-C plugin
2. **Frama-C AST → `.ss` translation** — translate Cil AST + ACSL specs into HipSleek's `.ss` format
3. **Subprocess call** — invoke `hip.exe` on generated `.ss` file, capture stdout
4. **Result parsing** — map HipSleek's SUCCESS/FAIL lines back to Frama-C function-level statuses
5. **CLI hook** — register plugin options (`-hipsleek`, `-hipsleek-path`, etc.)

The WP plugin (`src/plugins/wp/`) is the primary design reference for steps 3–5.

## Build Commands

```bash
# Build everything (Frama-C + HipSleek)
dune build

# Build only HipSleek binaries
make hipsleek
# equivalent: dune build hipsleek/hip.exe hipsleek/sleek.exe

# Run HipSleek manually
hipsleek/_build/default/hip.exe   <file>.ss
hipsleek/_build/default/sleek.exe <file>.slk

# Run with Z3 backend
hipsleek/_build/default/sleek.exe --smt-z3 <file>.slk
```

## HipSleek Stabilisation Status (as of 2026-06-11)

HipSleek has been stabilised as a standalone verifier before the Frama-C integration begins.

**Test baseline** (`python3 hipsleek/check_expected.py`):
- 20 files with comment-mismatch remain — all genuine behavioral issues (categories D/E/F/A/H)
- 72 files resolved and in `fixed/`

**Open known issues in HipSleek** (see `hipsleek/CLAUDE.md` for full detail):

| ID | Area | Status |
|----|------|--------|
| 4 | `flow __Error` semantics | Open |
| 5 | TempAnn / variable annotation binding | Open |
| 8 | Aliasing contradiction (different predicates at same address) | Open |
| 9 | Over-instantiation in solver | Open |
| 10 | Annotation variable escaping | Open |
| 11 | Backwards lemma application | Open |

**What is working**: `hip.exe` and `sleek.exe` build cleanly and pass the representative test suite. Core separation-logic reasoning, Z3/Omega backends, and standard heap predicates all work.

## Demos (`demo_hipsleek/`)

C/Frama-C demo programs exercised end-to-end through the plugin with
`./bin/frama-c -hipsleek demo_hipsleek/<file>.c`:

| File | Functions | Demonstrates |
|------|-----------|--------------|
| `hipexm.c` | `get_val`, `set_val` | the basic READ / UPDATE heap effects |
| `ll.c` | `get_next`, `set_next`, `set_null`, `append` | the `ll<n>` length predicate (`[SL_pred]`); `append` is recursive |
| `alias.c` | `alias_write`, `aliased_inputs`, `set_two`, `set_two_aliased` | aliasing vs. separation; `set_two_aliased` is an **expected FAIL** — `*` rejects unintended aliasing at the call site |
| `loop.c` | `count_to_ten` | a `while` loop with an inline `/*[SL_loop]*/` requires/ensures spec (primed post-state vars); plugin recovers the guard from Cil's normalized `if/break` head |
| `loop_arith.c` | `count_up`, `add_ones`, `countdown` | the basic loop-spec shapes: variable bound, accumulator invariant (`s = i`), loop variable as parameter |
| `loop_control.c` | `and_guard`, `or_guard`, `stop_at_five`, `skip_three` | short-circuit `&&`/`||` guards (Cil nests these as if/break trees; the guards are deliberately redundant so the spec pins down which operand binds), plus `break` and `continue` |
| `loop_heap.c` | `bump`, `length` | loops over the heap: `bump` hands back the cells it borrows; `length` is a **consuming** list traversal (the loop takes `x::ll<k>` and does not return it, which is why the spec is `res = n` and not `x::ll<n>`) |
| `loop_bad.c` | `off_by_one`, `wrong_post`, `not_invariant`, `guard_ignored` | **all four are intentional FAILs** — negative twins of the above; see the note on modular verdicts below |

All functions verify **SUCCESS** except `set_two_aliased` and all of `loop_bad.c`
(intentional FAILs). `test_ll.c` and `demo.c` are older scratch files, not part
of the demo set.

**A loop spec must be an INVARIANT, not a precondition.** HipSleek compiles a
`while` into a recursive procedure and re-checks the spec's `requires` at the
recursive call, so `requires i = 0` on a loop that increments `i` fails on the
second iteration. Write the spec over an unconstrained entry state and case-split
on the guard in the `ensures` (`loop_bad.c`'s `not_invariant` demonstrates the
mistake).

**Verdicts are modular, and a failed loop taints its function.** HipSleek checks
a loop against its own spec, then checks the enclosing function while *assuming*
that spec. A function that comes out green while depending on a loop spec that
did not verify has a real proof resting on an undischarged lemma, so the plugin
reports it as **NOT VERIFIED** (naming the loops it leaned on) rather than
SUCCESS — a don't-know property status, amber in the Ivette panel. A function
that fails on its own account still reports a plain FAIL. `loop_bad.c` covers
both shapes.

**Encoding rule the specs use**: a C `node* x` is encoded by the plugin as the wrapper
type `node_star` (a cell whose `pdata` field is the `node`), so `node* x` owns *two*
cells — `x::node_star<p> * p::node<...>` — and `x->val` becomes `x.pdata.val` in the
generated `.ss`. Specs are written in that generated-`.ss` vocabulary.

Running requires the z3/`prelude.ss`/PATH setup recorded in the project memory
(`build-run-env`): hip.exe needs a `z3-4.3.2` executable on a `WindowsApps`-free PATH
and `_build/default/hipsleek/prelude.ss` present.

## Collaboration Notes

- **`hipsleek/CLAUDE.md`** — detailed change log for the HipSleek stabilisation work (newest-first). All code changes to HipSleek must be logged there.
- **This file** — high-level project overview and integration roadmap.
- The integration plugin (`src/plugins/hipsleek/`) does **not exist yet** — that is the next major milestone.
- When adding the plugin, follow the WP plugin structure (`src/plugins/wp/`) as the reference for how Frama-C plugins invoke external tools and report results.

## Key Design Decisions Made

1. **Monorepo**: HipSleek lives under `hipsleek/` as a dune sub-project, sharing one `dune-workspace`. This allows `dune build` to build both together.
2. **HipSleek is stabilised first** before the integration plugin is written — ensures the backend is reliable.
3. **`.ss` file bridge**: The integration will generate `.ss` files from Frama-C's Cil AST, not try to link HipSleek as a library (too much OCaml module conflict risk). This keeps the boundary clean.
