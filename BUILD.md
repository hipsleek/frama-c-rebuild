# Build Guide — Frama-C + HipSleek Integration Project

## Prerequisites

Both Frama-C and HipSleek are OCaml projects built with **dune**. You need a single opam switch that satisfies the combined dependency set of both.

### System packages (Debian/Ubuntu)

```bash
sudo apt-get install -y \
  build-essential pkg-config \
  libgmp-dev libmpfr-dev \
  zlib1g-dev \
  opam
```

### OCaml / opam switch

The reference configuration is **OCaml 4.14.x**. Create a fresh switch:

```bash
opam init                   # if not already done
opam switch create 4.14.2
eval $(opam env)
```

### Install all dependencies (one command)

From the repo root, install Frama-C's dependencies and HipSleek's dependencies together:

```bash
# Frama-C dependencies
opam install --deps-only \
  dune dune-configurator dune-site \
  menhir ocamlfind ocamlgraph \
  ppx_deriving_yaml ppx_deriving_yojson ppx_inline_test \
  unionFind zarith yojson \
  why3 alt-ergo

# HipSleek additional dependencies
opam install \
  fileutils batteries camlp4 xml-light \
  ppx_expect ppx_deriving visitors \
  cppo
```

Or install them all in one shot:

```bash
opam install \
  dune dune-configurator dune-site \
  menhir ocamlfind ocamlgraph \
  ppx_deriving_yaml ppx_deriving_yojson ppx_inline_test \
  unionFind zarith yojson why3 alt-ergo \
  fileutils batteries camlp4 xml-light \
  ppx_expect ppx_deriving visitors cppo
```

### External provers

HipSleek dispatches arithmetic and set constraints to external provers. Only two
matter for this project; the rest are optional. See the upstream HIP/SLEEK install
guide for the canonical instructions: <https://hipsleek.github.io/hipsleek/install.html>.

#### Omega Calculator (`oc`) — default backend

**Omega is HipSleek's default arithmetic (Presburger) back-end** — hip/sleek call a
binary named `oc` for quantifier elimination unless you force `--smt-z3`. It must be
on `PATH`. Verify with:

```bash
oc            # prints "Omega Calculator v2.1.6 ..." then waits for input (Ctrl-D to exit)
```

If it is missing, build it from the upstream HipSleek tree (this trimmed monorepo
does **not** ship the `omega_modified/` sources) and put the result on `PATH`:

```bash
# in a full hipsleek checkout
(cd omega_modified; make oc)
# then add omega_modified/omega_calc/obj to PATH (see "PATH setup" below)
```

#### Z3 — SMT back-end

Used for the nonlinear/SMT obligations (and forced by `--smt-z3`). Needs `z3` on `PATH`:

```bash
# Ubuntu/Debian
sudo apt-get install -y z3

# or via opam
opam install z3
```

> Note: hip.exe also probes for a binary literally named **`z3-4.3.2`** while scanning
> `PATH`. In WSL, symlink it (`ln -s "$(which z3)" ~/bin/z3-4.3.2`) and strip
> `WindowsApps`/`/mnt/c` entries from `PATH` — see `HIPSLEEK_USAGE.md` and the project
> `build-run-env` notes.

#### Mona / Fixcalc — optional

Not required for the demos. Install only if you use set-based predicates (Mona) or
fixpoint inference (Fixcalc); their sources also live in the full upstream tree, not
here. Following the upstream guide:

```bash
# Mona 1.4 (set reasoning; enable with -tp mona)
tar -xvf mona-1.4-modif.tar.gz && cd mona-1.4
./configure --prefix=$(pwd) && make install && cp mona_predicates.mona ..

# Fixcalc (fixpoint calculator; needs GHC 9.4.8)
cabal install --lib regex-compat old-time && cabal install happy
git clone https://github.com/hipsleek/omega_stub.git && (cd omega_stub; make)
git clone https://github.com/hipsleek/fixcalc.git fixcalc_src && (cd fixcalc_src; make fixcalc)
```

The warning `ERROR : fixcalc cannot be found` when Fixcalc is absent is **harmless** and
can be ignored.

#### PATH setup

Every prover above is found via `PATH`. Add the relevant directories once — either
export them in your shell profile, or, as upstream does, with a `direnv` `.envrc`:

```bash
# .envrc (upstream layout)
eval "$(opam env --switch=4.14.2 --set-switch)"
PATH_add omega_modified/omega_calc/obj
PATH_add mona-1.4/bin
PATH_add fixcalc_src
```

---

## Building

### Build everything (Frama-C + HipSleek)

```bash
cd /path/to/frama-c-rebuild
dune build
```

This uses `dune-workspace` at the repo root to build both sub-projects in one pass.
Output artefacts land under `_build/default/`.

### Build only HipSleek binaries

```bash
make hipsleek
# equivalent:
dune build hipsleek/hip.exe hipsleek/sleek.exe
```

Binaries are at:
- `hipsleek/_build/default/hip.exe`
- `hipsleek/_build/default/sleek.exe`

### Build only Frama-C

```bash
dune build @frama-c
# or the classic route:
make RELEASE=yes
```

---

## Running

### HipSleek standalone

```bash
# Verify a .ss program (HIP)
hipsleek/_build/default/hip.exe   path/to/program.ss

# Check a .slk entailment (SLEEK)
hipsleek/_build/default/sleek.exe path/to/query.slk

# Use Z3 backend instead of Omega
hipsleek/_build/default/sleek.exe --smt-z3 path/to/query.slk
```

### Frama-C standalone

```bash
_build/default/bin/frama-c.exe [options] file.c
```

---

## Testing

### HipSleek test suite

```bash
cd hipsleek

# Run representative .slk / .ss examples and check expected outputs
python3 check_expected.py
# Results written to failure_reports/expected_mismatch/

# Run full examples/ directory and group failures
bash run_examples.sh
# Results written to failure_reports/raw/ and failure_reports/group_N_*.md

# Copy failing source files into per-group directories for easier debugging
bash collect_cases.sh
```

**Expected baseline** (as of 2026-06-11):
- `check_expected.py`: 20 files with open behavioral mismatches (categories D/E/F/A/H — see `hipsleek/CLAUDE.md`)
- `run_examples.sh`: 33 pass, 68 fail, 37 groups

### Frama-C tests

```bash
dune runtest
```

---

## Troubleshooting

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| `cppo: command not found` | cppo not installed | `opam install cppo` |
| `camlp4: not found` | camlp4 missing | `opam install camlp4` |
| `ERROR : fixcalc cannot be found` | fixcalc optional binary absent | Harmless — ignore |
| `dune: Error: No implementation found for ...` | Missing opam package | Check the dependency list above |
| `Error: Library "batteries" not found` | batteries not installed | `opam install batteries` |
| `z3: command not found` when using `--smt-z3` | Z3 not on PATH | `sudo apt install z3` or `opam install z3` |
| Arithmetic checks hang or error with no `--smt-z3` | Omega Calculator (`oc`) not on PATH | Build `oc` (`(cd omega_modified; make oc)`) and add it to PATH — see "External provers" |
| `Unix.EACCES lstat .../WindowsApps/z3-4.3.2` | hip.exe scanning Windows PATH for `z3-4.3.2` | Symlink `z3-4.3.2 -> $(which z3)` and drop `WindowsApps`/`/mnt/c` from PATH |
| Frama-C `dune build` fails on `why3` | why3 not installed | `opam install why3 alt-ergo` |

---

## Reference Versions

Tested working configuration:

| Package | Version |
|---------|---------|
| OCaml | 4.14.2 |
| dune | 3.14+ |
| menhir | 20240715 |
| ocamlgraph | 2.1.0+ |
| why3 | 1.8.2 |
| alt-ergo | 2.6.2 |
| batteries | 3.8.0+ |
| camlp4 | 4.14+1 |
| visitors | 20210608+ |
| cppo | any recent |
| z3 | 4.x (system) |

See `reference-configuration.md` for the full Frama-C reference set.
See `hipsleek/hipsleek.opam` for the HipSleek opam dependency list.
