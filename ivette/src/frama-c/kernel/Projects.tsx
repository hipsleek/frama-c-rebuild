/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import * as Dome from 'dome';
import { alpha } from 'dome/data/compare';
import { Item, SidebarTitle } from 'dome/frame/sidebars';
import { Button, IconButton } from 'dome/controls/buttons';
import { Hbox } from 'dome/layout/boxes';
import { useModel } from 'dome/table/models';
import * as Dialogs from 'dome/dialogs';
import { FieldState, TextField, useState } from 'dome/layout/forms';
import { Icon } from 'dome/controls/icons';
import { HelpButton } from 'dome/help';

import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import { addProjectSubMenu } from 'frama-c/menu';
import * as Project from './api/project';


// --------------------------------------------------------------------------
// --- Projects
// --------------------------------------------------------------------------
interface ProjectFieldProps {
  project?: string,
  fieldName?: string,
  placeholder?: string,
  onValidate: (name: string) => void
}

function ProjectField(props: ProjectFieldProps): React.JSX.Element {
  const {
    project = '', fieldName, placeholder = 'New name', onValidate
  } = props;
  const state = useState(project);

  const onKeyDown = (e: React.KeyboardEvent<HTMLInputElement>): void => {
    if (e.key === 'Enter') {
      e.preventDefault();
      onValidate(state.value);
    }
  };

  return <div className='project-field' >
    <TextField
      label={fieldName || ''}
      placeholder={placeholder}
      state={state as FieldState<string | undefined>}
      latency={0}
      autoFocus={true}
      onKeyDown={onKeyDown}
    />
    <Button
      label='Ok'
      focusable={false}
      onClick={() => onValidate(state.value)}
    />
  </div>;
}

function showError(title: string, error: string): void {
  Dialogs.showModal(<Dialogs.Modal label={title}>
    <div className='project-error'>
      <Icon id='WARNING' kind='warning' size={18}/>
      {error}
    </div>
  </Dialogs.Modal>);
}

function showModalProject(
  title: string,
  onValidate: (v: string) => void,
  project?: string
): void {
  Dialogs.showModal(<Dialogs.Modal label={title}>
    <ProjectField project={project || ''} onValidate={onValidate} />
  </Dialogs.Modal>);
}

/** Create a new project */
export function newProject(): void {
  const onValidate = (name: string): void => {
    Server.send(Project.create, name).then(() => Dialogs.closeModal());
  };
  showModalProject('Create project', onValidate);
}

function getProject(id?: number): Project.listData | undefined {
  const projects = States.getSyncArrayData(Project.list);
  return projects.find(e => e.id === id);
}

/** Rename a project */
export function renameProject(id: number, project?: string): void {
  const name = project || getProject(id)?.name;
  const title = `Rename project "${name}" (id:${id})`;
  const onValidate = (name: string): void => {
    Server.send(Project.rename, [id, name]).then((error) => {
      if(!error) Dialogs.closeModal();
      else showError('Error while renaming project', error);
    });
  };
  showModalProject(title, onValidate, name);
}

/** Remove a project */
export async function removeProject(id: number): Promise<void> {
  function deletionError(content: string): void {
    showError('Error while deleting project', content);
  }

  const projects = States.getSyncArrayData(Project.list);
  if(projects.length === 1) {
    return deletionError('The last project cannot be removed');
  }

  const project = projects.find(e => e.id === id);
  if(!project) {
    return deletionError(`The project with id '${id}' doesn't exist`);
  }

  const confirm = await Dialogs.showMessageBox({
    block: true,
    buttons: [
      { label: 'Cancel' },
      { label: 'Ok', value: true }
    ],
    details: `Confirm to delete project "${project.name}" (id:${id}).`,
    message: `Delete project "${project.name}"`,
  });

  if(confirm === true) {
    const error = await Server.send(Project.remove, id);
    if(error) deletionError(error);
  }
}

/** Duplicate a project */
export function duplicateProject(id: number, project?: string): void {
  const name = project || getProject(id)?.name;
  const title = `Duplicate project "${name}" (id:${id})`;
  const onValidate = (name: string): void => {
    Server.send(Project.copy, [id, name]).then((error) => {
      if(!error) Dialogs.closeModal();
      else showError('Error while duplicating project', error);
    });
    Dialogs.modalLoader.setValue(true);
  };
  showModalProject(title, onValidate, name);
}

/** Save a project */
export async function saveProject(id: number): Promise<void> {
  const file = await Dialogs.showSaveFile({});
  const error = await Server.send(Project.save, [id, file]);
  if(error) showError('Error while saving project', error);
}

/** Load a project */
export async function loadProject(): Promise<void> {
  const file = await Dialogs.showOpenFile({});
  const error = await Server.send(Project.load, file);
  if(error) showError('Error while loading project', error);
}

/** ************************************************************************ */

function getActions(id: number, name: string): React.JSX.Element {
  return (
    <>
      <IconButton
        icon='EDIT'
        size={14}
        title='Rename'
        onClick={() => renameProject(id, name)}
      />
      <IconButton
        icon='DUPLICATE'
        size={14}
        title='Duplicate'
        onClick={() =>
          duplicateProject(id, name)
        }
      />
      <IconButton
        icon='SAVE'
        size={14}
        title='Save'
        onClick={() => saveProject(id)}
      />
      <IconButton
        icon='TRASH'
        size={14}
        title='Delete'
        onClick={() => removeProject(id)}
      />
    </>
  );
}

export function Projects(): JSX.Element {
  const scrollableArea = React.useRef<HTMLDivElement>(null);
  const [ current, setCurrent ] = States.useSyncState(Project.current);
  const modelProjects = States.useSyncArrayModel(Project.list);
  const model = useModel(modelProjects);

  const projectsListSorted = React.useMemo(() => {
    return modelProjects.getArray().sort((a, b) => alpha(a.name, b.name));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [modelProjects, model]);

  /** Re-Build the project menu */
  React.useEffect(() => {
    /** The timeout is used for optimisation, * avoiding unnecessary
     * calculations when projects change very frequently. */
    const timeout = setTimeout(() => {
      Dome.delSubMenu('Project');
      const others: Dome.MenuItemProps[] = projectsListSorted.map(elt => {
        return {
          menu: 'Project',
          label: elt.name,
          id: `Project_${elt.id}`,
          kind: 'checkbox',
          checked: current === elt.id,
          enabled: current !== elt.id,
          onClick: () => setCurrent(elt.id)
        };
      });
      addProjectSubMenu(others);
    }, 100);
    return () => clearTimeout(timeout);
  }, [projectsListSorted, current, setCurrent]);

  /** Build item components for project sidebar */
  const projectsList = React.useMemo(() => {
    return projectsListSorted.map(elt => {
      return <Item
          key={elt.id}
          label={elt.name}
          title={`${elt.name} (id: ${elt.id})`}
          selected={elt.id === current}
          onSelection={() => setCurrent(elt.id) }
        >{getActions(elt.id, elt.name)}</Item>;
    });
  }, [projectsListSorted, current, setCurrent]);

  return (<>
    <SidebarTitle
      className='projects'
      label='Projects'
    >
      <Hbox className='projects-title-actions'>
        <IconButton
          icon='DOWNLOAD'
          title='Load a project'
          size={18}
          onClick={loadProject}
        />
        <IconButton
          icon='CIRC.PLUS'
          title='Create a new empty project'
          size={18}
          onClick={newProject}
        />
        <HelpButton
          id={'framac-project'}
          size={18}
        />
      </Hbox>
    </SidebarTitle>
      <div ref={scrollableArea} className='globals-scrollable-area'>
        { projectsList }
      </div>
    </>
  );
}

// --------------------------------------------------------------------------
