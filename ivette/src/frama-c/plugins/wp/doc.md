# WP {#wp}

The WP plug-in is meant to verify that functions fulfil their ACSL contract by
deductive verification. From the source code and code annotations, it generates
formulas called verification conditions (or goals) that must be validated by
external automatic or interactive solvers.

To run WP on a code, either add the option `-wp` to the command line or
right-click on a function and "Prove function using WP". One can also run WP on
a given property by right-clicking the property and running "Prove property
using WP".

## Goals view {#wp-goals}

The main WP view is the "WP - Goals" view. It lists the different goals together
with the result of the proof. This list can be filtered [icon-FILTER] to display
only the goals related to the current scope or only the goals that are not proved
yet.

In order, the columns are:
- the scope of the property,
- the name of the property to prove,
- whether an interactive script exists and its status:
    - editing [icon-CODE]
    - updated [icon-FOLDER.OPEN]
    - saved [icon-FOLDER]
- the status of the proof.

The status of the proof comprises the result of the proof and the computation
time of the different solvers. The different results that a proof can have
depends on the kind of property, either a property that we want to prove
(typically code annotations) or properties that must fail (smoke-tests).

The result for a property to prove can be:
- [icon-CHECK-positive] "Valid": the proof succeeded,
- [icon-ATTENTION-warning] "Unknown": the prover did not find a solution,
- [icon-HELP-warning] "Timeout": the prover reached the time limit,
- [icon-HELP-warning] "Stepout": the prover reached the step limit,

The result for a smoke-test can be:
- [icon-CHECK-positive] "Passed": the smoke test did not find an inconsistency,
- [icon-CROSS-negative] "Doomed": a problem has been detected.

For any property the following results can appear:
- [icon-EXECUTE] "Running": the prover is running,
- [icon-WARNING-negative] "Failed": something wrong happened with the solver.

In the second situation, you can probably file a bug report on
[our GitLab](https://git.frama-c.com/pub/frama-c/-/issues).

By double-clicking on a goal (or selecting it and then clicking [icon-MEDIA.PLAY]),
you can open the TIP view for this goal.

## TIP view {#wp-tip}

The TIP view shows the goal (which is basically the formula to prove) and
provides tactics to solve it interactively. The top bar indicates the name
of the property, together with the status of the proof (see
[WP - Goals](#wp-goals)). In this top bar, we also find buttons for:
- replaying [icon-RELOAD] and saving [icon-SAVE] the proof script,
- canceling the last tactic [icon-MEDIA.PREV] or the entire script [icon-CROSS],
- navigating to the previous [icon-ANGLE.LEFT] or [icon-ANGLE.RIGHT] goal,
  or returning to the list [icon-EJECT]
- configuring solvers to enable [icon-SETTINGS],
- stopping all running provers [icon-MEDIA.HALT].

The displayed way the goal is displayed can be customized with:
- Counter examples displayed (CE) (only available with `-wp-counter-examples`),
- Autofocus mode (AF),
- Memory model information (MEM),
- integer format,
- float format.

The right column displays solvers and available tactics. On each tactic/prover,
if it can be run, the [icon-MEDIA.PLAY] button appears green and can be clicked.
Clicking the tactic button itself opens the configuration menu at the bottom of
the panel. The content of this panel depends on the selected tactic. Note that
the list of available tactics depends on the terms that are selected in the goal.
One can select by clicking on it.

Finally, the right column contains the proof steps, one can navigate through the
different steps of the script by clicking on the elements of the list.

## Configuration panel {#wp-config}

### Provers configuration {#wp-config-provers}

#### General configuration {#wp-config-provers-general}

- [icon-CLOCK] Timeout: stop provers after N seconds
- [icon-SETTINGS] Processes: number of parallel prover processes
- [icon-SERVER] Cache: whether and how the cache should be used:
    - None: no cache, run provers,
    - Update: use cache or run provers and update cache,
    - Cleanup: update mode with garbage collection,
    - Replay: update mode with no cache update,
    - Rebuild: always run provers and update cache,
    - Offline: use cache but never run provers.

#### Automatic provers {#wp-config-provers-auto}

Enabled automatic provers, if this list is empty it means that Why3 cannot
detect any installed solvers. The complete list of supported solvers is
available on Why3's website, we recommend Alt-Ergo, Colibri2, CVC5 and Z3.

One can also configure solvers in "counter-examples" mode, in this case the
TIP view can be configured to display counter-examples. This parameter is
not available when goals have already been generated since WP has to perform
the WP calculus again with this parameter enabled so that formulas contain all
required information to generate counter examples.

#### Interactive provers {#wp-config-provers-inter}

Enabled interactive provers, it also provides an option for the selected mode
for interactive proofs:
- Batch: check current proof
- Update: check updated proof
- Edit: edit current proof
- Fix: check current proof and edit if needed
- FixUpdate: update proof, check it and edit if needed

#### Strategies {#wp-config-provers-strats}

Customize WP strategies and scripts usage. If "use scripts" is set, proofs
scripts will be used, if "use strategies" is set, strategies can generate new
scripts. Disabling scripts also disables strategies, enabling strategies also
enables scripts.

The mode allows configuring how proofs scripts are updated (or not):
- Batch: proof scripts are reused but not updated (default for script prover)
- Update: proof scripts are reused and updated (default for tip prover)
- Init: proof scripts are generated from scratch and saved
- Dry: proof scripts are explored from scratch and not saved

### Properties {#wp-config-properties}

#### RTE Guards {#wp-config-properties-rte}

By default, WP does not generate assertions related to *runtime errors*. This
is controlled by the command line option `-wp-rte` and via the GUI, by the
"Generate" checkbox of this section. This block can be unfolded to customize
the exact runtime errors that one wants to check. These items are connected
to the corresponding kernel and RTE plug-in options.

Note that the available *runtime errors* selectors only include what is
supported currently by WP.

#### Smoke tests {#wp-config-properties-smoke}

Smoke tests are meant to detect inconsistencies in user-specifications and/or
dead code. By default, WP does not generate such checks since they can be costly
to execute. This is controlled by the command line option `-wp-smoke-tests` and
via the GUI, by the "Generate" checkbox of this section. This block can be
unfolded to disable some available smoke tests, namely:

- "Assumes": dead assumes clauses because of an invalid requires,
- "Code": unreachable code,
- "Call": non-terminating calls,
- "Local initialization": unreachable local initialization,
- "Loop": inconsistent loop invariants

#### Filters {#wp-config-properties-filters}

By default, WP tries to prove all properties related to the selected proof
target. The filters allow selecting properties more precisely. It corresponds
to the option `-wp-prop`. In the graphical user interface, one can build the
content of this option interactively. One can select and add/remove:
- categories of properties,
- custom properties based on their name.
Each property can be added in the list positively (using the [icon-PLUS] button)
or negatively (using the [icon-MINUS] button) if it is not already present in
the list, in such a case this occurrence can be removed with the opposite button.
One can also remove all filters using the [icon-CROSS] button.

### Sequent simplification {#wp-config-simpl}

Configure QED simplification for proofs. It is mostly useful for debugging
purposes (for example to understand performance drops or proofs failures because
of missing properties that should be available). Please refer to the WP manual
to configure these elements. Note that these parameters cannot be changed if the
WP calculus has been executed, results must be dropped first.

## Strategy debugger {#wp-strat-debug}

**This component is not enabled by default**, it is available in the "Views &
Components" panel, in "Other Plugins", "WP Strategy Debugger".

The component has two parts:
- an editor where one can write a strategy,
- a feedback zone that gives information about the strategy.

The editor waits for a strategy (without the `strategy` keyword), where the name
of the strategy *can* be omitted, thus just a list alternative, for example:
```
name: (optional)
  \tactic(
    "Wp.range",
    \pattern(i <= 31),
    \param("inf", 0),
    \param("sup", 31)),
  \tactic(
    "Wp.split",
    \pattern(A && B)),
  existing_strategy
```

If the strategy is syntactically incorrect, the error is displayed. Else, the
debugger goes further by *separately* analyzing each alternative. If several
alternatives are available, buttons ([icon-ANGLE.LEFT]/[icon-ANGLE.RIGHT])
appear to navigate between alternatives in the title bar and the currently
selected alternative is highlighted in the editor. All errors and warnings are
underlined in the editor, but the displayed messages are related to the
currently selected alternative.

For sub-strategies and provers, the debugger just checked for their existence.
For tactics, the debugger can go further, in particular when a proof node is
selected. It will:
- apply the patterns and raise a warning when a pattern cannot be applied,
- check parameters typing,
- check selection typing,
- display the terms matched and built via the tactic.

For each selection, parameter and pattern, it can be selected in the current
node by clicking on it when the icon is [icon-TARGET], for non-selectable
values the icon is [icon-TERMINAL].
