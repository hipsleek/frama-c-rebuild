/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import { defineConfig } from "electron-vite";
import react from "@vitejs/plugin-react";
import path from "path";
import commonjsExternals from 'vite-plugin-commonjs-externals';

const DOME = process.env.DOME || path.resolve("src", "dome");
const ENV = process.env.DOME_ENV;

// Do not use electron-devtools-installer in production mode

function domeDevtools(): string {
  switch (ENV) {
    case "dev":
      return "electron-devtools-installer";
    default:
      return path.resolve(DOME, "misc/devtools.js");
  }
}

export default defineConfig({
  main: {
    build: {
      externalizeDeps: true
    },
    resolve: {
      extensions: [".ts", ".tsx", ".js", "jsx", ".json"],
      alias: {
        "dome/main": path.resolve(DOME, "main", "dome.ts"),
        "dome/misc": path.resolve(DOME, "misc"),
        "dome/misc/devtools": domeDevtools(),
        "dome/system": path.resolve(DOME, "misc", "system.ts"),
      },
    },
  },
  preload: {
    build: {
      externalizeDeps: true
    },
  },
  renderer: {
    resolve: {
      extensions: [".ts", ".tsx", ".js", "jsx", ".json"],
      alias: {
        "dome/controls": path.resolve(DOME, "renderer", "controls"),
        "dome/data": path.resolve(DOME, "renderer", "data"),
        "dome/dialogs": path.resolve(DOME, "renderer", "dialogs"),
        "dome/help": path.resolve(DOME, "renderer", "help"),
        "dome/dnd": path.resolve(DOME, "renderer", "dnd"),
        "dome/errors": path.resolve(DOME, "renderer", "errors"),
        "dome/frame": path.resolve(DOME, "renderer", "frame"),
        "dome/layout": path.resolve(DOME, "renderer", "layout"),
        "dome/graph": path.resolve(DOME, "renderer", "graph"),
        "dome/misc": path.resolve(DOME, "misc"),
        "dome/system": path.resolve(DOME, "misc", "system.ts"),
        "dome/table": path.resolve(DOME, "renderer", "table"),
        "dome/text": path.resolve(DOME, "renderer", "text"),
        "dome/themes": path.resolve(DOME, "renderer", "themes"),
        // Must be put at the end for priority reasons
        "dome": path.resolve(DOME, "renderer", "dome.tsx"),
        "frama-c": path.resolve(__dirname, "src", "frama-c"),
        "ivette": path.resolve(__dirname, "src", "ivette"),
      },
    },
    plugins: [react(), commonjsExternals({
      externals: ['path', 'fs', 'events', 'net', 'child_process', 'electron']
    })],
  },
});
