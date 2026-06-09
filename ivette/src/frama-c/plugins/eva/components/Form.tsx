/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Sidebar Selector
// --------------------------------------------------------------------------

import React from 'react';
import * as Forms from 'dome/layout/forms';
import { classes } from 'dome/misc/utils';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';
import * as EvaDef from 'frama-c/plugins/eva/EvaDefinitions';
import { Icon } from 'dome/controls/icons';

/* -------------------------------------------------------------------------- */
/* --- Eva form section                                                   --- */
/* -------------------------------------------------------------------------- */
function Section(p: Forms.SectionProps): JSX.Element {
  const settings = `ivette.eva.option.${p.label}`;
  return (
    <Forms.Section
      label={p.label}
      unfold
      error={p.error}
      settings={settings}
    >
      {p.children}
    </Forms.Section>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Button/Radio list                                                  --- */
/* -------------------------------------------------------------------------- */
function OptionsButton(props: EvaDef.OptionsButtonProps): JSX.Element {
  const { name, label, fieldState } = props;
  const state = Forms.useProperty(fieldState, name);
  return <Forms.ButtonField label={label} state={state} />;
}

function ButtonFieldList(
  props: EvaDef.ButtonFieldListProps
): JSX.Element {
  const { classeName, state, fieldProps } = props;
  const { value } = state;

  const htmlButtonList = Object.keys(value)
    .map((p) => <OptionsButton
      key={p}
      name={p}
      label={p}
      fieldState={state}
    />);
  return (
    <Forms.Field  {...fieldProps}>
      <div className={classeName}></div>
      {htmlButtonList}
    </Forms.Field>
  );
}

export function RadioFieldList(
  props: EvaDef.RadioFieldListProps
): JSX.Element {
  const { classeName, state, values, fieldProps } = props;
  const htmlRadioList = Object.entries(values)
    .map(([k, v]) => <Forms.RadioField
      key={k}
      label={v}
      value={k}
      state={state}
    />);
  return (
    <Forms.Field  {...fieldProps}>
      <div className={classeName}></div>
      {htmlRadioList}
    </Forms.Field>
  );
}


/* -------------------------------------------------------------------------- */
/* ---Eva Form                                                            --- */
/* -------------------------------------------------------------------------- */

interface EvaFormOptionsProps {
  fields: EvaDef.EvaFormProps;
}

function getActions<A>(
  state: Forms.FieldState<A>,
  name: EvaDef.fieldsName,
  equal?: (a: A, b: A) => boolean,
): JSX.Element | undefined {
  if(!state) return undefined;
  return (
    <Forms.Actions>
      <Icon
        id = "HELP"
        title = {EvaDef.fieldHelp[name]}
        size = {12}
        className = "eva-field-help"
      />
      <Forms.ResetButton
        state={state}
        title="Reset"
        equal={equal}
      />
      <Forms.CommitButton
        state={state}
        title="Apply"
        equal={equal}
      />
    </Forms.Actions>
  );
}

export function EvaFormOptions(
  props: EvaFormOptionsProps
): JSX.Element {
  const { fields } = props;

  const showAllFields = Forms.useState(false);

  const fcts = States.useSyncArrayData(Ast.functions);
  const fctsList = React.useMemo(() =>
    fcts.filter((fct) => !fct.extern && !fct.stdlib && !fct.builtin)
        .sort((a, b) => a.name.localeCompare(b.name)),
    [fcts]
  );

  function isNoAlwaysVisibleFieldsStable(): boolean {
    let disable = false;
    for (const [name, evaField] of Object.entries(fields)) {
      if (EvaDef.fieldsAlwaysVisible.includes(name as EvaDef.fieldsName)) {
        continue;
      }
      if (!Forms.isStable(evaField.state)) {
        disable = true;
        break;
      }
    }
    return disable;
  }

  function getClasses<A>(
    state: Forms.FieldState<A>,
    name: EvaDef.fieldsName
  ): string | undefined {
    const notVisible =
      !showAllFields.value &&
      !EvaDef.fieldsAlwaysVisible.includes(name);
    return classes(
      "field"+name,
      !Forms.isStable(state) && "modified",
      notVisible ? "hidden-field" : "visible-field"
    );
  }

  function getSpinnerField(name: EvaDef.fieldsName): JSX.Element {
    const field = fields[name];
    const state = field.state;
    return (
    <Forms.SpinnerField
      label={field.label}
      step={field.step}
      min={field.min as number}
      max={field.max as number}
      state={field.state as Forms.FieldState<number | undefined>}
      className={getClasses(state, name)}
      actions={getActions(state, name)}
      />
    );
  }

  function getRadioField(name: EvaDef.fieldsName): JSX.Element {
    const field = fields[name];
    const state = field.state;
    return (
      <RadioFieldList
        fieldProps={{
          label: field.label,
          actions: getActions(state, name)
        }}
        classeName={getClasses(state, name)}
        values={field.optionRadio as EvaDef.RadioList}
        state={field.state}
        />
    );
  }

  function getBooleanField(name: EvaDef.fieldsName): JSX.Element {
    const field = fields[name];
    const state = field.state;
    return (
      <Forms.Field
        label={field.label}
        actions={getActions(state, name)}
      >
        <div className={getClasses(state, name)} />
        <Forms.ButtonField
          label={state.value ? "Enabled" : "disabled"}
          state={state}
        />
      </Forms.Field>
    );
  }

  const errorMain = Boolean(
    !fctsList.some(elt => elt.name === fields["-main"].state.value)
    || fields["-main"].state.value === ""
  );
  if(errorMain)
    fields["-main"].state.error = "Choose a function from the list.";

  const mainField =
    <Forms.SelectField
      label="Main"
      state={fields["-main"].state as Forms.FieldState<string | undefined>}
      actions={getActions(fields["-main"].state, "-main")}
    >
      { errorMain &&
        <option
          key={"empty-main"} id={"empty-main"} value={""}
          className={getClasses(fields["-main"].state, "-main")}
        >-</option>
      }
      {
        fctsList.map((f) => <option
            key={f.key} id={f.key} value={f.name}
            className={getClasses(fields["-main"].state, "-main")}
          >{f.name}</option>
        )
      }
    </Forms.SelectField>;

  const domainsField =
  <ButtonFieldList
    fieldProps={{
      label: "Domains",
      actions: getActions(
        fields["-eva-domains"].state,
        "-eva-domains",
        EvaDef.buttonListEquality)
    }}
    classeName={getClasses(fields["-eva-domains"].state, "-eva-domains")}
    state={fields["-eva-domains"].state}
    />;

  return (
    <Forms.SidebarForm className="eva-form">
      <Forms.CheckboxField
        label={"show all fields"}
        title=
          "disabled if it checked and if the fields to be hidden are not stable"
        state={showAllFields}
        disabled={isNoAlwaysVisibleFieldsStable()}
      />

      {mainField}
      {getBooleanField("-lib-entry")}
      {getSpinnerField("-eva-precision")}

      <Section label="Analysis Domains" >
        {domainsField}
        {getRadioField("-eva-equality-through-calls")}
        {getBooleanField("-eva-octagon-through-calls")}
        {getBooleanField("-eva-multidim-disjunctive-invariants")}
      </Section>

      <Section label="Analysis of Loops" >
        {getSpinnerField("-eva-auto-loop-unroll")}
        {getSpinnerField("-eva-min-loop-unroll")}
        {getSpinnerField("-eva-widening-delay")}
        {getSpinnerField("-eva-widening-period")}
      </Section>

      <Section label="Partitioning and Disjunctions" >
        {getSpinnerField("-eva-slevel")}
        {getRadioField("-eva-split-return")}
        {getSpinnerField("-eva-partition-history")}
        {getSpinnerField("-eva-ilevel")}
        {getSpinnerField("-eva-plevel")}
        {getSpinnerField("-eva-subdivide-non-linear")}
      </Section>

      {showAllFields.value &&
      <Section label="Dynamic Allocation" >
        {getRadioField("-eva-alloc-builtin")}
        {getBooleanField("-eva-alloc-returns-null")}
        {getSpinnerField("-eva-mlevel")}
      </Section>
      }

      {showAllFields.value &&
      <Section label="Initial Context" >
        {getSpinnerField("-eva-context-depth")}
        {getSpinnerField("-eva-context-width")}
        {getBooleanField("-eva-context-valid-pointers")}
      </Section>
      }

      {showAllFields.value &&
      <Section label="Alarms" >
        {getBooleanField("-warn-signed-overflow")}
        {getBooleanField("-warn-unsigned-overflow")}
        {getBooleanField("-warn-signed-downcast")}
        {getBooleanField("-warn-unsigned-downcast")}
        {getBooleanField("-warn-pointer-downcast")}
        {getRadioField("-warn-special-float")}
        {getBooleanField("-warn-invalid-pointer")}
        {getBooleanField("-warn-invalid-bool")}
        {getBooleanField("-warn-left-shift-negative")}
        {getBooleanField("-warn-right-shift-negative")}
      </Section>
      }

    </Forms.SidebarForm>
  );
}
