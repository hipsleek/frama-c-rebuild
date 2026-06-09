/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import * as Controller from './Controller';
import * as Messages from './Messages';
import * as Ivette from 'ivette';

Ivette.registerComponent({
    id: 'fc.kernel.console',
    label: 'Console',
    title: 'Frama-C Command Line',
    help: "ivette-console",
    preferredPosition: 'AB',
    children: <Controller.RenderConsole />,
});

Ivette.registerComponent({
    id: 'fc.kernel.messages',
    label: 'Messages',
    title: 'Frama-C Messages',
    preferredPosition: 'CD',
    children: <Messages.RenderMessages />,
});

Ivette.registerView({
    id: 'ivette.console',
    label: 'Console',
    title: 'Frama-C Console & Messages',
    layout: { AB: 'fc.kernel.console', CD: 'fc.kernel.messages' },
});
