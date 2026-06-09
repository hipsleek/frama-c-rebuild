/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// React & Dome
import React from 'react';
import * as Ivette from 'ivette';
import * as States from 'frama-c/states';
import { GlobalState, useGlobalState } from 'dome/data/states';
import * as Eva from 'frama-c/plugins/eva/api/analysis';
import * as Boxes from 'dome/layout/boxes';
import { HSplit } from 'dome/layout/splitters';
import { Text } from 'frama-c/richtext';
import { Checkbox, SelectMenu } from 'dome/controls/buttons';
import { Label } from 'dome/controls/labels';
import { EvaStatus } from './components/AnalysisStatus';

const globalSelectedDomain = new GlobalState("");
const globalFilter = new GlobalState(true);

export function EvaStates(): JSX.Element {
  const marker = States.useSelected();
  const [domains, setDomains] = React.useState<string[]>([]);
  const [selected, setSelected] = useGlobalState(globalSelectedDomain);
  const [stateBefore, setStateBefore] = React.useState("");
  const [stateAfter, setStateAfter] = React.useState("");
  const [filter, setFilter] = useGlobalState(globalFilter);

  const requestArg = marker ? [marker, filter] : undefined;
  const states = States.useRequestResponse(Eva.getStates, requestArg);

  React.useEffect(() => {
    if (states && states.length > 0) {
      const names = states.map((d) => d[0]);
      setDomains(names);
      if (!names.includes(selected))
        setSelected(names[0]);
      const selectedDomain = states.find((d) => d[0] === selected);
      if (selectedDomain) {
        setStateBefore(selectedDomain[1]);
        setStateAfter(selectedDomain[2]);
      }
    } else
      setDomains([]);
  }, [states, selected, setSelected]);

  if (domains.length === 0)
    return (<></>);

  function makeOption(name: string): React.ReactNode {
    return <option value={name}>{name}</option>;
  }
  const list = React.Children.toArray(domains.map(makeOption));

  return (
    <>
      <Boxes.Hbox className="domain-state-box">
        <Label>Domain: </Label>
        <SelectMenu
          title="Select the analysis domain to be shown"
          value={selected}
          onChange={(domain) => setSelected(domain ?? "")}
        >
          {list}
        </SelectMenu>
        <Boxes.Filler/>
        <Checkbox
          label="Filtered state"
          title="If enabled, only the part of the states relevant to the
        selected marker are shown, for domains supporting this feature.
        For other domains or if disabled, entire domain states are printed.
        Beware that entire domain states can be very large."
          value={filter}
          onChange={setFilter}
        />
      </Boxes.Hbox>
      <Boxes.Scroll>
        <HSplit
          settings="ivette.eva.domainStates.beforeAfterSplit"
        >
          <div className="domain-state-box">
            State before the selected statement:
            <Text className="domain-state" text={stateBefore} />
          </div>
          <div className="domain-state-box">
            State after the selected statement:
            <Text className="domain-state" text={stateAfter} />
          </div>
        </HSplit>
      </Boxes.Scroll>
    </>);
}

function EvaStatesComponent(): JSX.Element {
  return (
    <>
      <Ivette.TitleBar help="eva-states">
        <EvaStatus />
      </Ivette.TitleBar>
      <EvaStates />
    </>
  );
}

Ivette.registerComponent({
  id: 'fc.eva.states',
  label: 'Eva States',
  title: 'States of the Eva analysis',
  children: <EvaStatesComponent />,
});
