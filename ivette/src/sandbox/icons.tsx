/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Sandbox Icons Gallery                                              --- */
/* --- Only appears in DEVEL mode.                                        --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import { Label, Code } from 'dome/controls/labels';
import { IconData, forEach } from 'dome/controls/icons';
import { Section } from 'dome/frame/sidebars';
import { Scroll, Grid } from 'dome/layout/boxes';
import { registerSandbox } from 'ivette';

/* -------------------------------------------------------------------------- */
/* --- Use Text                                                           --- */
/* -------------------------------------------------------------------------- */

function Gallery(): JSX.Element {
  const gallery : Map<string, JSX.Element[]> = new Map();
  forEach((icon: IconData) => {
    const { id, title, section='Custom Icons' } = icon;
    let icons = gallery.get(section);
    if (icons === undefined) {
      icons = [];
      gallery.set(section, icons);
    }
    icons.push(<Code key={'C'+id} icon={id} label={id} />);
    icons.push(<Label key={'L'+id} label={title} />);
  });
  const sections : JSX.Element[] = [];
  gallery.forEach((icons, section) => {
    sections.push(
      <Section key={section} defaultUnfold label={section}>
        <Grid style={{ paddingLeft: 24 }} columns="auto auto">{icons}</Grid>
      </Section>
    );
  });
  return <Scroll>{sections}</Scroll>;
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.icons',
  label: 'Icons Gallery',
  children: <Gallery />,
});

/* -------------------------------------------------------------------------- */
