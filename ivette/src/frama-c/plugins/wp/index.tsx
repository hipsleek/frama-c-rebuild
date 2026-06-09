/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Eva Values
// --------------------------------------------------------------------------

import React from 'react';
import * as Dome from 'dome';
import { Label } from 'dome/controls/labels';
import { IconButton } from 'dome/controls/buttons';
import { Meter } from 'dome/controls/displays';
import { Group, Inset } from 'dome/frame/toolbars';
import { GlobalState, useGlobalState } from 'dome/data/states';
import * as Ivette from 'ivette';
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';
import * as ASTview from 'frama-c/kernel/ASTview';
import { GoalTable } from './goals';
import { TIPView } from './tip';
import { SideBar } from './sidebar';
import * as TIP from './tip';
import * as WP from 'frama-c/plugins/wp/api';
import doc from './doc.md?raw';
import './style.css';

// --------------------------------------------------------------------------
// --- help
// --------------------------------------------------------------------------

Ivette.registerDocChapter({ id: "wp", content: doc });

/* -------------------------------------------------------------------------- */
/* --- Context Menus                                                      --- */
/* -------------------------------------------------------------------------- */

function addStartProofMenus(
  menu: Dome.PopupMenuItem[],
  attr: Ast.markerAttributesData,
): void {
  const { marker, kind } = attr;
  switch (kind) {
    case 'LFUN':
    case 'DFUN':
      menu.push({
        label: `Prove function using WP`,
        onClick: () => Server.send(WP.startProofs, marker)
      });
      return;
    case 'STMT':
      menu.push({
        label: `Prove statement annotations using WP`,
        onClick: () => Server.send(WP.startProofs, marker)
      });
      return;
    case 'PROPERTY':
      menu.push({
        label: `Prove property using WP`,
        onClick: () => Server.send(WP.startProofs, marker)
      });
      return;
  }
}

ASTview.registerMarkerMenuExtender(addStartProofMenus);

function addGenerateRTEGuardsMenu(
  menu: Dome.PopupMenuItem[],
  attr: Ast.markerAttributesData,
): void {
  const { marker, kind } = attr;
  switch (kind) {
    case 'LFUN':
    case 'DFUN':
      menu.push({
        label: `Populate WP RTE guards`,
        onClick: () => Server.send(WP.generateRTEGuards, marker)
      });
      return;
  }
}

ASTview.registerMarkerMenuExtender(addGenerateRTEGuardsMenu);


/* -------------------------------------------------------------------------- */
/* --- Current Goal                                                       --- */
/* -------------------------------------------------------------------------- */

type Goal = WP.goal | undefined;

const globalGoalSelection = new GlobalState(undefined);

/* -------------------------------------------------------------------------- */
/* --- Goal Component                                                     --- */
/* -------------------------------------------------------------------------- */

type setting = [boolean, () => void]
function menuItem(label: string, [b, flip]: setting, enabled?: boolean)
  : Dome.PopupMenuItem {
  return {
    label: label,
    enabled: enabled !== undefined ? enabled : true,
    checked: b,
    onClick: flip,
  };
}

function WPGoals(): JSX.Element {
  const [current, setCurrent] = useGlobalState<Goal>(globalGoalSelection);

  const scopedState = Dome.useFlipSettings('frama-c.wp.goals.scoped');
  const [scoped] = scopedState;
  const failedState = Dome.useFlipSettings('frama-c.wp.goals.failed');
  const [failed] = failedState;


  const filterItems: Dome.PopupMenuItem[] = [
    menuItem('Current Scope Only', scopedState),
    menuItem('Unresolved Goals Only', failedState)
  ];

  const [tip, setTip] = React.useState(false);
  Server.useShutdown(() => { setTip(false); setCurrent(undefined); });
  const scope = States.useCurrentScope();
  const [goals, setGoals] = React.useState(0);
  const [total, setTotal] = React.useState(0);
  const hasGoals = total > 0;
  return (
    <>
      <Ivette.TitleBar
        label={tip ? 'WP — TIP' : 'WP — Goals'}
        title={tip ? 'Interactive Proof Transformer' : 'Generated Goals'}
        help={tip ? 'wp-tip' : 'wp-goals'}
      >
        <Label display={goals < total}>
          {goals} / {total}
        </Label>
        <Inset />
        <IconButton
          icon='FILTER' title='Filters'
          enabled={hasGoals}
          onClick={() => Dome.popupMenu(filterItems)} />
        <IconButton
          icon='MEDIA.PLAY'
          title={tip ? 'Back to Goals' : 'Interactive Proof Transformer'}
          enabled={!!current}
          selected={tip}
          onClick={() => setTip(!tip)} />
      </Ivette.TitleBar>
      <GoalTable
        display={!tip}
        failed={failed}
        scoped={scoped}
        scope={scope}
        current={current}
        setCurrent={setCurrent}
        setTIP={() => setTip(true)}
        setGoals={setGoals}
        setTotal={setTotal}
      />
      <TIPView
        display={tip}
        goal={current}
        onClose={() => setTip(false)}
      />
    </>
  );
}

Ivette.registerComponent({
  id: 'fc.wp.goals',
  label: 'WP Goals',
  title: 'WP Generated Verification Conditions',
  children: <WPGoals />,
});

/* -------------------------------------------------------------------------- */
/* --- Strategy Debugger Component                                        --- */
/* -------------------------------------------------------------------------- */

function StrategyDebugger(): JSX.Element {
  const [current, _] = useGlobalState<Goal>(globalGoalSelection);
  return <TIP.StrategyDebugger goal={current} />;
}

Ivette.registerComponent({
  id: 'fc.wp.strategy-debugger',
  label: 'WP Strategy Debugger',
  title: 'WP Strategy Debugger',
  help: 'wp-strat-debug',
  children: <StrategyDebugger />,
});

/* -------------------------------------------------------------------------- */
/* --- WP Server Activity                                                 --- */
/* -------------------------------------------------------------------------- */

function ServerActivity(): JSX.Element {
  const { done, todo, active, procs, running } = TIP.useServerActivity();
  const total = done + todo;
  const progress = done + active;
  const objective = done + todo + procs;
  const title = `${done} / ${todo} (${active} running, ${procs} procs)`;
  return (
    <Group display={total > 0} title={title}>
      <Label>WP</Label>
      <Meter min={0} value={progress} max={objective} />
      <Inset />
      <IconButton
        icon="MEDIA.HALT" kind="negative" enabled={running}
        onClick={TIP.cancelProofTasks} />
      <Inset />
    </Group>
  );
}

Ivette.registerStatusbar({
  id: 'fc.wp.server',
  children: <ServerActivity />,
});

/* -------------------------------------------------------------------------- */
/* --- WP SideBar                                                         --- */
/* -------------------------------------------------------------------------- */

Ivette.registerSidebar({
  id: 'frama-c.plugins.wp-sidebar',
  label: 'WP',
  icon: 'WP',
  title: 'WP',
  children: <SideBar />
});

/* -------------------------------------------------------------------------- */
/* --- WP View                                                            --- */
/* -------------------------------------------------------------------------- */

Ivette.registerView({
  id: 'fc.wp.main',
  label: 'WP View',
  layout: {
    'A': 'fc.kernel.astview',
    'B': 'fc.kernel.astinfo',
    'CD': 'fc.wp.goals',
  }
});

// --------------------------------------------------------------------------
