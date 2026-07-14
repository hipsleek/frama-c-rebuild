# Using Frama-C with the HipSleek Plugin

A hands-on guide to verifying C programs with the **HipSleek** separation-logic
backend from inside Frama-C, and to inspecting *how* the proof was found —
the generated `.ss` program, the SLEEK entailment log, per-obligation results,
and the raw solver (Z3/Omega) traffic that the original `hip`/`sleek` tools expose.

> This guide focuses on the command line and on **logging/diagnostics**. For the
> Ivette GUI walkthrough and the annotation reference, see [`README.md`](README.md).

---

## Contents

1. [Build](#1-build)
2. [Quick start](#2-quick-start)
3. [The demo programs](#3-the-demo-programs)
4. [Annotations in one page](#4-annotations-in-one-page)
5. [Verdicts as Frama-C properties](#5-verdicts-as-frama-c-properties)
6. [Logging & diagnostics](#6-logging--diagnostics) ← the main event
   - [6.1 Plugin verbosity/debug](#61-plugin-verbositydebug)
   - [6.2 See the generated `.ss`](#62-see-the-generated-ss)
   - [6.3 The SLEEK entailment log (`-hipsleek-proof-log`)](#63-the-sleek-entailment-log--hipsleek-proof-log)
   - [6.4 Reading the raw log files hip writes](#64-reading-the-raw-log-files-hip-writes)
   - [6.5 Drive `hip.exe` directly for deeper traces](#65-drive-hipexe-directly-for-deeper-traces)
   - [6.6 Standalone entailment checking with `sleek.exe`](#66-standalone-entailment-checking-with-sleekexe)
7. [Anatomy of a SLEEK log entry](#7-anatomy-of-a-sleek-log-entry)
8. [Flag cheat-sheet](#8-flag-cheat-sheet)
9. [Troubleshooting](#9-troubleshooting)

---

## 1. Build

From the repository root:

```bash
dune build            # builds Frama-C + the plugin + hip.exe / sleek.exe
```

`hip.exe` and `sleek.exe` end up at `_build/default/hipsleek/`. The plugin
auto-detects `hip.exe`, so no path flag is normally needed (see the "Options"
table in the README).

> If you edit the plugin sources under `src/plugins/hipsleek/`, rebuild with
> `dune build @install` — the plugin has its own nested `dune-project`, so a plain
> `dune build` can leave it stale.

---

## 2. Quick start

Run the plugin on any demo file:

```bash
dune exec --root . frama-c -- -hipsleek demo_hipsleek/test_ll.c
```

(You can also use the wrapper `bin/frama-c -hipsleek …`.) Expected output:

```
[hipsleek] Running HipSleek plugin...
[hipsleek] Generated .ss file: /tmp/hipsleek_out.ss
[hipsleek] Invoking: …/hipsleek/hip.exe /tmp/hipsleek_out.ss
[hipsleek] [HipSleek] append: SUCCESS
[hipsleek] [HipSleek] length: SUCCESS
```

Each function annotated with a `/*[SL]*/` spec gets a **SUCCESS** / **FAIL** line.

---

## 3. The demo programs

All live in `demo_hipsleek/`. Every function verifies **SUCCESS** except the one
intentional FAIL in `alias.c`.

| File | Functions | Demonstrates |
|------|-----------|--------------|
| `hipexm.c` | `get_val`, `set_val` | the two basic heap effects — READ and UPDATE of a field |
| `demo.c` | all of the above + `ll<n>` functions | a single-file tour: heap R/W, aliasing, and the length predicate |
| `ll.c` | `get_next`, `set_next`, `set_null`, `append` | the `ll<n>` length view; `append` is recursive |
| `test_ll.c` | `length`, `append` | the plain `ll<>` (no length) view; smallest end-to-end example |
| `alias.c` | `alias_write`, `aliased_inputs`, `set_two`, `set_two_aliased` | aliasing vs. separation; `set_two_aliased` is an **expected FAIL** |
| `loop.c` | `count_to_ten` | a `while` loop with a `/*[SL_loop]*/` contract |

Try them:

```bash
dune exec --root . frama-c -- -hipsleek demo_hipsleek/ll.c
dune exec --root . frama-c -- -hipsleek demo_hipsleek/alias.c    # note the one FAIL
dune exec --root . frama-c -- -hipsleek demo_hipsleek/loop.c
```

`loop.c` reports the loop itself and the enclosing function separately:

```
[hipsleek] [HipSleek] while loop at line 10 (in count_to_ten): SUCCESS
[hipsleek] [HipSleek] count_to_ten: SUCCESS
```

`alias.c` shows how `*` (separating conjunction) rejects unintended aliasing at a
call site — `set_two_aliased` calls `set_two(x, x, …)` but `set_two` requires two
**disjoint** cells, so its precondition is unsatisfiable:

```
[hipsleek] [HipSleek] set_two: SUCCESS
[hipsleek] Warning: [HipSleek] set_two_aliased: FAIL
```

---

## 4. Annotations in one page

HipSleek specs are plain C comments, so the file stays valid C and coexists with
ACSL `/*@ … */`. Three kinds (full reference in the README):

```c
/*[SL_pred]                                  // predicate / view definitions
ll<n> == self = null & n = 0
  or self::node_star<p> * p::node<_,q> * q::ll<n-1>
  inv n >= 0;
*/

/*[SL]                                        // a function's pre/post contract
  requires x::ll<n> * y::ll<n2> & x != null
  ensures  x::ll<n+n2>;
*/
void append(node* x, node* y) { … }

/*[SL_loop]                                   // a while-loop contract (primed = post-state)
  requires true
  ensures  i < 10 & i' = 10 or i >= 10 & i' = i;
*/
while (i < 10) { i = i + 1; }
```

**Encoding reminder:** a C `node* x` becomes the wrapper type `node_star`
(a cell whose `pdata` field holds the `node`), so `node* x` owns *two* cells —
`x::node_star<p> * p::node<…>` — and `x->val` becomes `x.pdata.val` in the
generated `.ss`. Write specs in that generated-`.ss` vocabulary.

---

## 5. Verdicts as Frama-C properties

The verdict is not just printed — it becomes a real Frama-C **property status**, so
it shows up in `-report` and in the GUI:

```bash
dune exec --root . frama-c -- \
  -hipsleek -hipsleek-proof-log demo_hipsleek/test_ll.c \
  -report -report-print-properties
```

```
[  Valid  ] Default behavior
[  Valid  ] HipSleek proof (12 obligation(s)):
  PRE  (line 16): x::ll<>@M & x'=x & x'=null |- htrue           [proved]
  BIND (line 17): x::ll<>@M & … |- x'::node_star<…>@L           [proved]
  PRE_REC (line 17): … |- v_node_star_…::ll<>@M                 [proved]
  POST (line 13): x::ll<>@M & … & res=… |- x::ll<>@M            [proved]
  …
```

- The SL contract appears on the function as a `\hipsleek::hipsleek requires…/ensures…`
  clause, and SUCCESS makes it **Valid** (green).
- `-hipsleek-proof-log` additionally attaches the per-function proof obligations
  (the SLEEK entailments, each `[proved]`/`[unproved]`) as a separate
  `HipSleek proof (…)` property.

---

## 6. Logging & diagnostics

There are two layers you can turn up:

- **Plugin layer** (Frama-C flags, prefix `-hipsleek-…`) — controls what the plugin
  itself prints and where it keeps artifacts.
- **Engine layer** (`hip.exe` / `sleek.exe` flags) — the original HipSleek
  diagnostics: entailment logs, proof steps, solver input/output. You reach these
  either through `-hipsleek-proof-log` or by running the binaries directly on the
  generated `.ss`.

### 6.1 Plugin verbosity/debug

Standard Frama-C plug-in knobs work on `hipsleek`:

```bash
# more feedback from the plugin (default 1)
dune exec --root . frama-c -- -hipsleek -hipsleek-verbose 2 demo_hipsleek/test_ll.c

# internal debug output (default 0); prints the raw hip.exe stdout among other things
dune exec --root . frama-c -- -hipsleek -hipsleek-debug 1 demo_hipsleek/test_ll.c

# copy this plugin's messages to a file, by category key
dune exec --root . frama-c -- -hipsleek -hipsleek-log a:hipsleek.log demo_hipsleek/test_ll.c
```

Other generic keys that exist on every plug-in: `-hipsleek-msg-key <k>` (enable a
message category; `-hipsleek-msg-key help` lists them), `-hipsleek-warn-key`
(promote/demote warnings). `-hipsleek-debug 1` is the quickest way to see the exact
`hip.exe` stdout the verdicts were parsed from.

### 6.2 See the generated `.ss`

The plugin translates the Cil AST into a `.ss` program and feeds *that* to `hip`.
To keep and read it, pick an output directory:

```bash
dune exec --root . frama-c -- \
  -hipsleek -hipsleek-output-dir /tmp/hsdemo demo_hipsleek/test_ll.c
cat /tmp/hsdemo/hipsleek_out.ss
```

For `test_ll.c` you get, e.g.:

```c
ll<> == self = null
  or self::node_star<p> * p::node<_,q> * q::ll<>;

data node      { int val; node_star next; }
data node_star { node pdata; }

int length(node_star x)
  requires x::ll<>
  ensures  x::ll<>;
{
  int tmp;
  if ((x == null)) { return 0; }
  tmp = length(x.pdata.next);
  return (1 + tmp);
}
…
```

This is the exact program the verdict and every obligation are about. When a spec
"mysteriously" fails, reading the `.ss` is the first thing to do — it shows the
`node_star`/`.pdata.` encoding and any translation approximations.

### 6.3 The SLEEK entailment log (`-hipsleek-proof-log`)

With `-hipsleek-proof-log` the plugin runs hip as `hip.exe --esl --dump-slk-proof …`
and parses the SLEEK log it emits. Combine it with `-hipsleek-output-dir` to keep the
raw files:

```bash
dune exec --root . frama-c -- \
  -hipsleek -hipsleek-proof-log -hipsleek-output-dir /tmp/hsdemo \
  demo_hipsleek/test_ll.c
```

Two ways to read the result:

- **Decluttered, per function** — via the property (see §5) or the GUI HipSleek
  Proof panel. Kinds shown: `PRE` (precondition), `BIND` (a field
  access/dereference is safe), `PRE_REC` (precondition at a recursive call),
  `POST` (postcondition), `ASSERT`.
- **Raw** — the file itself:

```bash
less /tmp/hsdemo/logs/sleek_log_hipsleek_out_ss.txt
```

The log file name is `sleek_log_<ss-basename-with-nonalnum-as-underscore>.txt`.

### 6.4 Reading the raw log files hip writes

`hip` (with `--esl --dump-slk-proof`, i.e. what `-hipsleek-proof-log` passes) drops a
`logs/` directory next to the `.ss` file. For `-hipsleek-output-dir /tmp/hsdemo`:

```
/tmp/hsdemo/hipsleek_out.ss
/tmp/hsdemo/logs/sleek_log_hipsleek_out_ss.txt   # the SLEEK entailments (see §7)
/tmp/hsdemo/logs/proof_log_hipsleek_out_ss       # full proof log
/tmp/hsdemo/logs/no_eps_proof_log_…_ss.txt       # proof log without entailment-state pruning
/tmp/hsdemo/logs/allinput.z3                      # every formula sent to Z3
/tmp/hsdemo/logs/allinput.oc                       # every formula sent to Omega
/tmp/hsdemo/logs/allinput.{mona,rl,thy,v,math,…}   # other back-ends (empty unless used)
/tmp/hsdemo/oc.out                                 # Omega Calculator raw output
```

`allinput.z3` and `allinput.oc` are gold when you suspect the *solver* rather than
the separation logic: they contain the literal SMT/Omega queries and answers.

### 6.5 Drive `hip.exe` directly for deeper traces

Anything the original HipSleek offers is available by running the binary on the
generated `.ss`. Keep the `.ss` (§6.2), then:

```bash
HIP=_build/default/hipsleek/hip.exe

# clean per-procedure result
$HIP --print-tidy /tmp/hsdemo/hipsleek_out.ss
#   Checking procedure append$node_star~node_star...
#   Procedure append$node_star~node_star SUCCESS.

# full entailment + proof logging to logs/ (same as -hipsleek-proof-log)
$HIP --esl --dump-slk-proof /tmp/hsdemo/hipsleek_out.ss

# step-by-step entailment proving trace on stdout
$HIP -dd-steps /tmp/hsdemo/hipsleek_out.ss        # entailment proving steps
$HIP --trace  /tmp/hsdemo/hipsleek_out.ss         # brief tracing
$HIP -dd      /tmp/hsdemo/hipsleek_out.ss         # developer debug (verbose)

# show what goes to / comes from the SMT solver
$HIP --smtinp /tmp/hsdemo/hipsleek_out.ss         # generated SMT input
$HIP --smtout /tmp/hsdemo/hipsleek_out.ss         # raw SMT solver output
$HIP --smtimply /tmp/hsdemo/hipsleek_out.ss       # antecedent |- consequent per check
$HIP -wpf /tmp/hsdemo/hipsleek_out.ss             # all VCs + prover in/out

# persist every solver query to a file
$HIP --log-z3 /tmp/hsdemo/hipsleek_out.ss         # -> logs/allinput.z3
$HIP --log-omega /tmp/hsdemo/hipsleek_out.ss      # -> logs/allinput.oc
$HIP --log-proof /tmp/hsdemo/hipsleek_out.ss      # log (failed) proofs to file

# timing / counting statistics
$HIP --en-stat /tmp/hsdemo/hipsleek_out.ss        # all statistics
$HIP --en-pstat /tmp/hsdemo/hipsleek_out.ss       # profiling only

# choose the arithmetic back-end
$HIP --smt-z3 /tmp/hsdemo/hipsleek_out.ss         # force Z3
```

`$HIP --help` lists the full flag set (there are hundreds). The ones above cover the
common "why did this (not) prove?" questions.

### 6.6 Standalone entailment checking with `sleek.exe`

For a single entailment you don't need Frama-C or a `.ss` program at all — write a
`.slk` file and check it with `sleek`. This is the fastest way to experiment with a
predicate or debug one obligation copied from the SLEEK log.

```slk
// entail.slk
data node { int val; node next; }.

pred ll<n> == self = null & n = 0
  or self::node<_, q> * q::ll<n-1>
  inv n >= 0.

checkentail x::ll<n> & n > 0 |- (exists q: x::node<_, q> * q::ll<n-1>).
```

```bash
_build/default/hipsleek/sleek.exe entail.slk
#   Entail 1: Valid.
```

**One-time setup:** `sleek.exe` auto-loads `prelude.slk` from its working directory.
The build only symlinks `prelude.ss` next to the binaries, so copy the `.slk` prelude
once:

```bash
cp hipsleek/prelude_src/prelude.slk _build/default/hipsleek/prelude.slk
```

(Run `sleek.exe` from `_build/default/hipsleek/`, or keep a `prelude.slk` in whatever
directory you launch it from.) `sleek.exe` accepts the same logging flags as `hip`
(`--esl`, `-dd-steps`, `--log-z3`, `--smt-z3`, …). `checkentail A |- B.` prints
`Valid.` / `Fail.`; add `print residue.` to see the leftover heap.

---

## 7. Anatomy of a SLEEK log entry

Each entailment in `sleek_log_*.txt` is one block. Example (a predicate invariant check):

```
id: 0; caller: []; line: 567; classic: false; kind: Pred_Check_Inv; hec_num: 1; …
 checkentail emp & ((self=p & n=0) | (self!=null & 1<=n)) & {FLOW,(1,28)=__flow#E}[]
 |-  emp & 0<=n & {FLOW,(1,28)=__flow#E}[].
ho_vars: nothing?
res:  1[
    emp & ((self=p & n=0) | (self!=null & 1<=n)) & {FLOW,…}[]
   es_gen_impl_vars(E): []
   es_heap(consumed): emp
   ]
```

How to read it:

| Field | Meaning |
|-------|---------|
| `id:` | sequential entailment number |
| `line:` | line in the **generated `.ss`** the obligation came from (the plugin maps this back to your C line for the property/GUI) |
| `kind:` | `PRE` / `POST` / `BIND` / `PRE_REC` / `ASSERT` are real obligations; `Pred_Check_Inv` is a prelude/predicate-invariant check the plugin filters out |
| `checkentail A \|- B.` | the entailment being proved: antecedent `A` entails consequent `B` |
| `res:` | the **residual** context list. `res: 1[ … ]` (a non-empty list, count ≥ 1) means **proved**; `res: 0[]` means **failed** |
| `{FLOW,…}` / `MayLoop[]` / `Term[]` | control-flow and termination bookkeeping — the plugin strips these when it "decluttes" the entailment for display |

So in the property/GUI you see the tidied `A |- B  [proved]`; in the raw file you see
the full state including flow, existentials (`es_gen_impl_vars`), and consumed heap.

---

## 8. Flag cheat-sheet

**Plugin (Frama-C) flags** — prefix everything after `-hipsleek`:

| Flag | Effect |
|------|--------|
| `-hipsleek` | enable the plugin |
| `-hipsleek-proof-log` | run with `--esl --dump-slk-proof`; attach per-obligation proof detail |
| `-hipsleek-output-dir <dir>` | keep the generated `.ss` and hip's `logs/` here (default: system temp) |
| `-hipsleek-path <path>` | override the `hip.exe` location (auto-detected otherwise) |
| `-hipsleek-verbose <n>` | plugin feedback level (default 1) |
| `-hipsleek-debug <n>` | plugin debug level (default 0); prints raw hip stdout |
| `-hipsleek-log <k:file>` | copy plugin messages of category `k` to `file` |
| `-report -report-print-properties` | show verdicts + obligations as Frama-C properties |

**Engine (`hip.exe` / `sleek.exe`) flags** — run on the generated `.ss` (or a `.slk`):

| Flag | Effect |
|------|--------|
| `--esl` / `--dump-slk-proof` | enable + dump the SLEEK entailment log to `logs/` |
| `--print-tidy` | concise per-procedure result with shortened names |
| `-dd-steps` | trace entailment-proving steps |
| `--trace` | brief tracing |
| `-dd`, `-dd-long`, `-dd-esl` | developer debug (increasing detail) |
| `--smtinp` / `--smtout` / `--smtimply` | SMT input / raw output / per-check implication |
| `-wpf` | print all VCs and the prover's input/output |
| `--log-z3` / `--log-omega` / `--log-proof` | persist solver queries / proofs to `logs/` |
| `--en-stat` / `--en-pstat` | timing & counting statistics |
| `--smt-z3` | force the Z3 back-end |
| `print residue.` (in a `.slk`) | print the residual heap after an entailment |

---

## 9. Troubleshooting

- **`hip.exe not found`** — pass `-hipsleek-path _build/default/hipsleek/hip.exe`, or
  run from inside the checkout so auto-detection finds it. Build first with `dune build`.
- **`sleek.exe` → `prelude.slk: No such file or directory` / `SLEEK FAILURE`** — copy
  `hipsleek/prelude_src/prelude.slk` next to `sleek.exe` and run from that directory
  (see §6.6). Only `prelude.ss` is symlinked by the build.
- **A verdict is FAIL and you don't see why** — keep the `.ss`
  (`-hipsleek-output-dir`), read it (§6.2), then re-run `hip.exe -dd-steps` on it or
  read `logs/sleek_log_*.txt` (§7). If the separation logic looks right, check
  `logs/allinput.z3` for a solver issue.
- **A green verdict but the C looks lossy** — the plugin emits a
  `Warning: <feature>: generated .ss may differ from your C …` when the C→`.ss`
  translation drops or approximates something (cast, global, `sizeof`, `switch`,
  `goto`, nested lvalue). Read the `.ss` to confirm what was actually verified.
- **No obligations in the property/GUI** — you must pass `-hipsleek-proof-log`;
  without it only the SUCCESS/FAIL verdict is produced.
```
