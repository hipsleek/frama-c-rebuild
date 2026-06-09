# May-Alias Analysis plugin

Alias is a plugin for Frama-C that implements:

- a points-to analysis, i.e. an over-approximation of the possible variables a
  pointer may point to at run-time.
- a may-alias analysis, i.e. an over-approximation of the possible aliases
  between pointer variables (and, more generally, memory regions) of the
  program.

Two pointers are called aliases of each other if at runtime they point to the
same memory location. In that case changing the value of one pointer also
changes the value of the other pointer and vice versa.
We call "may-aliases" pointers that are currently not aliases of each other
but may become aliases at some point at run-time.

This plugin implements in a static analysis a variant of "Steensgaard's
algorithm", which determines at compile-time conservative approximations of
the may-alias sets of a program and its variables.

This analysis aims at correctness and efficiency rather than precision. The
results of the analysis are therefore always (i.e. in the absence of warnings)
correct, in the sense that every alias-pair that may form at run-time is
contained in the alias-pairs identified by the static analysis.

A may-alias analysis usually contains also a points-to analysis, which
is also the case for this plugin. This analysis is correct as well, in the
sense that (in the absence of warnings) all pointer relations (variable a
points to b) that may occur at run-time will be in the points-to set determined
by this analysis.

Note that the Eva plugin can also be used to implement a points-to analysis,
which is much more precise but also much less efficient than this plugin.

## Usage

To run the may-alias analysis either:

- call the function `Alias.Analysis.compute`
- run `frama-c` with the `-alias` flag

Some of the most important command-line flags are:

- `-alias-show-function-table` displays for each function of the analyzed
  source code a summary, which includes the alias sets for the function's
  parameters and return value.
- `-alias-show-stmt-table` displays for each statement the alias sets for all
  the variables (lvals) in scope.

Please run `frama-c -alias-h` for more information on command-line flags.

## API

`Alias.Analysis` provides functions to run the analysis and clear the analysis
results.
The module `Alias.API` provides function to access the analysis results.

## Example

The following example prints all the alias sets at the end of all global
functions.

```ocaml
Alias.Analysis.compute ();
let print_kf_alias_sets kf =
  let alias_sets = Alias.API.Function.alias_sets_lvals ~kf in
  List.iter (Alias.API.LSet.pretty Format.std_formatter) alias_sets
in
Globals.Functions.iter print_kf_alias_sets
```

## Limitations

This plugin implements a path-insensitive analysis based on purely syntactic
reasoning, without numerical domains/values computations. When some branch
condition appears in the program, any alias in any branch is considered.
Therefore, the analysis is efficient, whereas the results are not very precise.

### Unsupported constructs
- recursive functions,
- user-defined variadic functions,
- function declared and used without being defined (i.e., no function body)
- assembly code,
- instructions `longjmp` and `setjmp`,
- complex instruction `goto` that breaks the natural control-flow of the program
- heterogeneous casts (e.g., casts from integers to pointers or conversely)
- union type,
- dynamic memory allocation, except if done once at the beginning of the
  program, whichever the execution path is.

### Imprecisely-supported constructs
- non-complex instruction `goto`,
- homogeneous casts,
- recursive datatype, e.g., multiple levels of pointer dereferencing,
- pointer arithmetic, and array and structure accesses,
- variable-length arrays,
- volatile attributes.

## Building and Installation

The plug-in is included by default when installing Frama-C.

Note that this plugin uses assertions extensively, which has considerable
performance cost. Building and installing using the `--release` flag disables
these assertions.

## Project Members

- Allan Blanchard
- Loïc Correnson
- Tristan Le Gall
- Jan Rochel
- Julien Signoles
