/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// Declare markdown files
declare module '*.md?raw' {
  const content: string;
  export default content;
}

declare module '*.png' {
  const value: string;
  export default value;
}

declare module 'd3-flame-graph';
declare module 'react-pivottable/PivotTableUI';
declare module 'react-pivottable/PivotTable';
declare module 'react-pivottable/Utilities';

declare module 'react-pivottable/TableRenderers';
declare module 'three/examples/jsm/renderers/CSS2DRenderer';
declare module 'react-cytoscapejs';
