/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { shell } from 'electron';

import { Modal, showModal } from 'dome/dialogs';
import { Hbox } from 'dome/layout/boxes';
import { Button } from 'dome/controls/buttons';
import { Markdown } from 'dome/text/markdown';

import { registerDocChapter } from 'ivette';
import { getConfig } from 'frama-c/kernel/api/services';
import * as Server from 'frama-c/server';

import './style.css';
import framacImage from './frama-c.png';

/** Import doc */
import docFramaC from './doc.md?raw';

// --------------------------------------------------------------------------
// --- help
// --------------------------------------------------------------------------

registerDocChapter({ id: "framac", content: docFramaC, rank: 1 });

/* -------------------------------------------------------------------------- */
/* --- Frama-C infos                                                      --- */
/* -------------------------------------------------------------------------- */

const synopsis =
'Frama-C is a platform dedicated to the analysis of source code written in C.';
const description =
'Frama-C gathers several analysis techniques in a single collaborative \
framework, based on analyzers, called "plug-ins", that can build upon the \
results computed by other analyzers of the platform.\n\
Thanks to this approach, Frama-C provides sophisticated tools, including:\n\
- Eva, an analyzer based on abstract interpretation;\n\
- WP, a program proof framework based on weakest precondition calculus;\n\
- E-ACSL, a runtime verification tool;\n\
- Aoraï, a tool for the verification of automata-based properties;\n\
- several tools for code base exploration and dependency analysis: \
plug-ins From, Impact, Metrics, Occurrence, Scope, etc.';

const authors = [
    'Michele Alberti',
    'Thibaud Antignac',
    'Gergö Barany',
    'Patrick Baudin',
    'Nicolas Bellec',
    'Thibaut Benjamin',
    'Allan Blanchard',
    'Lionel Blatter',
    'François Bobot',
    'Richard Bonichon',
    'Vincent Botbol',
    'Quentin Bouillaguet',
    'David Bühler',
    'Zakaria Chihani',
    'Loïc Correnson',
    'Julien Crétin',
    'Pascal Cuoq',
    'Zaynah Dargaye',
    'Basile Desloges',
    'Jean-Christophe Filliâtre',
    'Philippe Herrmann',
    'Maxime Jacquemin',
    'Florent Kirchner',
    'Alexander Kogtenkov',
    'Remi Lazarini',
    'Tristan Le Gall',
    'Kilyan Le Gallic',
    'Jean-Christophe Léchenet',
    'Matthieu Lemerre',
    'Dara Ly',
    'David Maison',
    'Claude Marché',
    'André Maroneze',
    'Thibault Martin',
    'Fonenantsoa Maurica',
    'Melody Méaulle',
    'Benjamin Monate',
    'Yannick Moy',
    'Pierre Nigron',
    'Anne Pacalet',
    'Valentin Perrelle',
    'Guillaume Petiot',
    'Dario Pinto',
    'Virgile Prevosto',
    'Armand Puccetti',
    'Félix Ridoux',
    'Virgile Robles',
    'Jan Rochel',
    'Muriel Roger',
    'Cécile Ruet-Cros',
    'Julien Signoles',
    'Nicolas Stouls',
    'Kostyantyn Vorobyov',
    'Boris Yakobowski'
  ];
const homepage = 'https://frama-c.com/';
const doc = 'https://frama-c.com/html/documentation.html';
const bugReports = 'https://git.frama-c.com/pub/frama-c/issues';
const devRepo = 'https://git.frama-c.com/pub/frama-c/';

const license =
'Most sources are LGPLv2.1,\n with some isolated exceptions for \
external libraries modified for Frama-C.\n\
See the particular header of each source file for details.';
const copyright =
'© CEA and INRIA for the Frama-C kernel\n\
© CEA for the GUI and plug-ins';

/* -------------------------------------------------------------------------- */
/* --- Frama-C About                                                      --- */
/* -------------------------------------------------------------------------- */

function FramaCLogo(): JSX.Element {
  return (
    <Hbox>
      <img src={framacImage} alt="Frama-C: Software analyzers"/>
    </Hbox>
  );
}

interface AboutProps {
  version: string;
}

function AboutModal(props: AboutProps): JSX.Element {
  const scrollableArea = React.useRef<HTMLDivElement>(null);
  return (
    <Modal className='modal-framac-infos' label='About Frama-C'>
      <div ref={scrollableArea} className="globals-scrollable-area">
        <FramaCLogo />
        <Hbox className='modal-framac-about'>
          <p>version: {props.version}</p>
          <p>{synopsis}</p>
          <Hbox>
            <Button
              onClick={() => shell.openExternal(homepage)}
              label='Website' />
            <Button
              onClick={() => shell.openExternal(doc)}
              label='Documentation' />
            <Button
              onClick={() => shell.openExternal(bugReports)}
              label='Bug reports' />
            <Button
              onClick={() => shell.openExternal(devRepo)}
              label='Git repository' />
          </Hbox>
          <Markdown className='framac-about-description'>
            {description}
          </Markdown>
          <p>{copyright}</p>
          <p>{license}</p>
        </Hbox>
      </div>
    </Modal>
  );
}

export async function showAboutModal(): Promise<void> {
  const config = await Server.send(getConfig, {});
  const version = config.version_codename;
  const modal = <AboutModal version = {version}/>;
  showModal(modal);
}

function CreditsModal(): JSX.Element {
  const scrollableArea = React.useRef<HTMLDivElement>(null);
  return (
    <Modal className='modal-framac-infos' label='Credits'>
      <div ref={scrollableArea} className="globals-scrollable-area">
        <FramaCLogo />
        <Hbox>
          <p style={{ fontSize: '1.2em', textAlign: "center" }}>
            Created by:
          </p>
        </Hbox>
        <div className='modal-framac-credits'>
          {authors.map((author, i) => <div key={i} >{author}</div>)}
        </div>
      </div>
    </Modal>
  );
}

export function showCreditsModal(): void {
  const modal = <CreditsModal/>;
  showModal(modal);
}
