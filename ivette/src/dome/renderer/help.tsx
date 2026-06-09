/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
  @packageDocumentation
  @module dome/help
 */

import React from 'react';
import { GlobalState, useGlobalState } from 'dome/data/states';

import { IconButton } from './controls/buttons';
import { Modal, showModal } from './dialogs';
import { Markdown, Pattern } from './text/markdown';
import { SideBar, SidebarTitle } from './frame/sidebars';
import { Tree, Node } from './frame/tree';
import { LSplit } from './layout/splitters';

import { Icon } from './controls/icons';
import { LED } from './controls/displays';
import { Button, ButtonGroup } from './frame/toolbars';
import { useStyle } from './themes';

/* --------------------------------------------------------------------------*/
/* --- Help                                                                  */
/* --------------------------------------------------------------------------*/

interface HelpButtonProps {
  /** id */
  id: string;
  /** icon size */
  size?: number;
}

export function HelpButton(props: HelpButtonProps): JSX.Element {
  const { id, size } = props;

  return (
    <IconButton
      icon='HELP'
      size={size}
      className='dome-xDoc-icon'
      title={'Help'}
      onClick={() => showHelp(id)}
    />
  );
}

/* --------------------------------------------------------------------------*/
/* --- General doc                                                           */
/* --------------------------------------------------------------------------*/

export const docChapters = new GlobalState<ChapterProps[]>([]);
export const docHistory = new GlobalState<string[]>(['']);
const posHistory = new GlobalState(0);

export interface ChapterProps {
  id: string;
  content: string;
  rank?: number;
  patterns?: Pattern[];
}

interface Index {
  id: string;
  label: string;
  level: number;
  errors: string[];
}

type IndexTab = [chapterId: string, indexes: Index[]];

interface HNode {
  id: string;
  label: string;
  errors?: string[];
  subTree: HNode[];
}

type HTree = HNode[];

/* --------------------------------------------------------------------------*/
/* --- Check errors                                                          */
/* --------------------------------------------------------------------------*/

// Check title list
function checkTitles(chapterId: string, titles: Index[]): string[] {
  const errors: string[] = [];
  const ids: string[] = [];
  const indexToDelete: number[] = [];
  let h1Error = false;
  const duplicateIds: string[] = [];
  const badStartingIds: string[] = [];

  titles.forEach((title, index) => {
    const { level, id } = title;
    // Check H1
    if(!h1Error && (
      (level === 1 && index > 0) || (level !== 1 && index === 0))
    ) { h1Error = true; }
    // Check duplicate ID
    if(ids.find(e => e === id) !== undefined) {
      indexToDelete.push(index);
      duplicateIds.push(id);
    } else ids.push(id);
    // Check if ID stating with chapter.id
    if(id.split('-')[0] !== chapterId) badStartingIds.push(id);
  });
  // Add errors
  if(h1Error) errors.push(
    'The chapter must have one H1 and it must be placed at the beginning\n');
  if(duplicateIds.length > 0) {
    errors.push('Duplicate Ids are removed from table of content:');
    duplicateIds.forEach(id => errors.push(`- ${id}`));
  }
  if(badStartingIds.length > 0) {
    errors.push(`Id must start with "${chapterId}":`);
    badStartingIds.forEach(id => errors.push(`- ${id}`));
  }

  indexToDelete.forEach(i => titles.splice(i, 1));
  return errors;
}

function checkLinks(chapter: ChapterProps, indexes: IndexTab[]): string[] {
  const errors: string[] = [];
  const linkRegex = /\[(.*?)\]\(#([^\s)]+)\)/gm;
  let matches;
  while ((matches = linkRegex.exec(chapter.content)) !== null) {
    const id = matches[2];
    if(!indexes.find(elt => elt[1].find(title => title.id === id)))
      errors.push(id);
  }

  if(errors.length > 0) { errors.unshift('Errors in following links:'); }
  return errors;
}

function getErrors(chapters: ChapterProps[], indexes: IndexTab[]): IndexTab[] {
  const news = indexes.slice();
  news.forEach(([ chapterId, chapterIndexes ]) => {
    // check titles
    const titlesErrors = checkTitles(chapterId, chapterIndexes);
    if(titlesErrors.length > 0) {
      chapterIndexes[0].errors = chapterIndexes[0].errors.concat(titlesErrors);
    }
    // check links
    const chapter = chapters.find(elt => elt.id === chapterId);
    if(chapter) {
      const linkErrors = checkLinks(chapter, indexes);
      if(linkErrors.length > 0) {
        chapterIndexes[0].errors = chapterIndexes[0].errors.concat(linkErrors);
      }
    }
  });
  return news;
}

/* --------------------------------------------------------------------------*/
/* --- Table of content                                                      */
/* --------------------------------------------------------------------------*/

export function getIndexes(chapters: ChapterProps[]): IndexTab[] {
  const regex = /^(#{1,6})\s(.+)\s((\{#([\w-]+)\})|(\|\|#([\w-]+)\|\|))/gm;
  // Retrieving H1, H2, H3, H4 titles with an id
  let matches;
  const titles: [string, Index[]][] = [];
  chapters.forEach(chapter => {
    const tmp = [];
    while ((matches = regex.exec(chapter.content)) !== null) {
      const level = matches[1].length;
      const label = matches[2];
      const id = matches[5] || matches[7];
      tmp.push({ level, label, id, errors: [] });
    }
    if(tmp.length > 0) titles.push([chapter.id, tmp]);
  });
  return titles;
}

function getTableOfContents(indexes: IndexTab[]): HTree[] {
  let i = 0;
  const tree: HTree[] = [];

  function toTree(titles: Index[]): HTree {
    const t: HTree = [];
    while(i < titles.length && titles[i].level < 5) {
      const elt = titles[i];
      const newNode: HNode = {
        id: elt.id, label: elt.label, subTree: []
      };
      if(elt.level === 1) newNode.errors = elt.errors;
      t.push(newNode);
      if(i+1 < titles.length) {
        const nextLevel = titles[i+1].level;
        if(nextLevel > elt.level) {
          i++;
          newNode.subTree = toTree(titles);
        } else if(nextLevel < elt.level) break;
      }
      i++;
    }
    return t;
  }

  indexes.forEach(([, index]) => {
    i = 0;
    tree.push(toTree(index));
  });

  return tree;
}

/* --------------------------------------------------------------------------*/
/* --- Nodes                                                                 */
/* --------------------------------------------------------------------------*/

function getSubTree(tree: HTree): React.ReactNode {
  return tree.length > 0 ? <Nodes tree={tree} /> : null;
}

function Nodes(props: { tree: HTree, path?: string }): React.ReactNode {
  return props.tree.map(({ id, label, errors, subTree }) => {
    const actions = errors && errors.length > 0 ? (
      <>
        <LED status="negative" blink={true} />
        <Icon id='WARNING' kind="negative" title={errors.join('\n')}></Icon>
        <LED status="negative" blink={true} />
      </>) :
      undefined;
    const path = props.path ? props.path+" - "+label : label;
    return <Node key={id} id={id} label={label} title={path} actions={actions}
      >{ getSubTree(subTree) }</Node>;
  }
  );
}

/* --------------------------------------------------------------------------*/
/* --- History                                                               */
/* --------------------------------------------------------------------------*/

interface History {
  current: string;
  addElement: (id: string) => void;
  previous: () => void;
  next: () => void;
  isFirstpos: () => boolean;
  isLastpos: () => boolean;
}

/**
 * Adds an ID to the current position + 1 in the history.
 * The end of the history is deleted.
 * The ID is deleted from the history before being added to avoid duplicates.
 */
function addInHistory(id: string): void {
  const history = docHistory.getValue();
  const position = posHistory.getValue();
  if (id === history[position]) return;
  const newHistory = history.toSpliced(position+1).filter(v => v !== id);
  if(newHistory.push(id) > 20) newHistory.shift();
  docHistory.setValue(newHistory);
  posHistory.setValue(newHistory.length-1);
}

function useHelpHistory(): History {
  const [ history, ] = useGlobalState(docHistory);
  const [ pos, setPos ] = useGlobalState(posHistory);
  const current = React.useMemo(() => history[pos], [history, pos]);

  return {
    current: current,
    addElement: (id: string): void => addInHistory(id),
    previous: (): void => setPos(pos > 0 ? pos-1 : 0 ),
    next: (): void => setPos(pos < history.length-1 ? pos+1 : pos ),
    isFirstpos: (): boolean => pos === 0,
    isLastpos: (): boolean => pos === history.length-1
  };
}

/**
  * Each chapter must have a unique identifier.
  * If a chapter is saved with an existing identifier, the identifier will be
  * changed and errors will appear on the last chapter saved.
  * Each *.md file must declare a unique H1 key with
  * # <title> <{#<id>} | ||#<id>||> with alphanumeric <id> .
  * Each *.md file can then declare H2, H3 or H4 keys with
  * #+ <title> <{#<id>-<subid>} | ||#<id>-<subid>||> with alphanumeric <subid>
  * which can optionally be compounded with - (unrelated to depth level).
*/
function GeneralDocModal(): JSX.Element {
  const [ chapters, ] = useGlobalState(docChapters);

  const [ unfoldAll, setUnfoldAll ] = React.useState<boolean|undefined>(true);
  const history = useHelpHistory();
  const selectedId = React.useMemo(() => history.current, [history]);

  const indexes = React.useMemo(() => {
    const news = getIndexes(chapters.sort((a, b) => {
      const A = a.rank ?? 50;
      const B = b.rank ?? 50;
      return A - B;
    }));
    return getErrors(chapters, news);
  }, [chapters]);

  const style = useStyle();
  const [ fontSize, setFontSize ] = React.useState(
    parseInt(style.getPropertyValue('--help-modal-fontsize'))
  );

  const setSizes = React.useCallback((size: number) => {
    document.documentElement.style.setProperty(
      "--help-modal-fontsize", size.toString()+"px");
    setFontSize(size);
  }, [setFontSize]);

  const tableOfContent = React.useMemo(() => {
    return getTableOfContents(indexes);
  }, [indexes]);

  const currentDoc = React.useMemo(() => {
    const docId = selectedId.split('-')[0];
    return chapters.find(elt => elt.id === docId);
  }, [selectedId, chapters]);

  const title = React.useMemo(() => {
    const ids = selectedId.split('-');
    const chapter = ids[0].charAt(0).toUpperCase() + ids[0].slice(1);
    const section = ids.slice(1).join(' ');
    return `Documentation ${chapter} ${section ? "- "+section: ""}`;
  }, [selectedId]);

  function onLinkClick(id: string): void { history.addElement(id); }
  function checkLink(id: string): boolean {
    return Boolean(indexes.find(elt => elt[1].find(title => title.id === id)));
  }

  const actionsHeader = <>
    <ButtonGroup>
      <Button icon='ANGLE.LEFT'
        disabled={history.isFirstpos()}
        onClick={ history.previous } />
      <Button icon='ANGLE.RIGHT'
        disabled={history.isLastpos()}
        onClick={ history.next } />
    </ButtonGroup>
    <ButtonGroup>
      <Button
        icon='ZOOM.OUT'
        disabled={fontSize < 10}
        onClick={() => setSizes(fontSize-2)} />
      <Button
        icon='ZOOM.IN'
        disabled={fontSize > 25}
        onClick={() => setSizes(fontSize+2)} />
    </ButtonGroup>
  </>;

  return (
    <Modal label={title}
      className='modal-framac-doc'
      actions={actionsHeader}
      style={{ fontSize: fontSize+'px' }}
    >
      <LSplit settings="frama-c.modal-doc.split">
        <SideBar>
          <SidebarTitle label='Table of contents' >
            <div className='dome-xTree-actions'>
              <IconButton
                icon={ "CHEVRON.CONTRACT" }
                title="Fold all"
                disabled={unfoldAll === false}
                onClick={() => setUnfoldAll(false)}
              />
              <IconButton
                icon={ "CHEVRON.EXPAND" }
                title="Unfold all"
                disabled={unfoldAll}
                onClick={() => setUnfoldAll(true)}
              />
            </div>
          </SidebarTitle>
          <div className="globals-scrollable-area">
            <Tree
              unfoldAll={unfoldAll}
              setUnfoldAll={setUnfoldAll}
              selected={selectedId}
              onClick={(id) => history.addElement(id) }
            >
              { tableOfContent.map((tree, i) => <Nodes
                  key={i}
                  tree={tree}
                ></Nodes> ) }
            </Tree>
          </div>
        </SideBar>
        <Markdown
          patterns={currentDoc?.patterns}
          scrollTo={selectedId}
          onLinkClick={onLinkClick}
          checkLink={checkLink}
        >
          { currentDoc?.content ?? `No documentation for \`${selectedId}\`` }
        </Markdown>
      </LSplit>
    </Modal>
  );
}

export function showHelp(id?: string): void {
  if(id) addInHistory(id);
  showModal(<GeneralDocModal/>);
}
