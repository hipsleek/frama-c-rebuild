/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import { Locator, Page } from "@playwright/test";

/**
 * Locator to select "Console" in the right menu
 */
export function getConsoleView(window: Page): Locator {
  window
    .getByText("Other Plugins")
    .click();
  return window.getByText("Console").first();
}

/**
 * Locator to select the Start button in the top button bar
 */
export function getStartServerButton(window: Page): Locator {
  return window
    .locator(".dome-xToolBar")
    .getByRole("button", { name: "Start the server", exact: true });
}

/**
 * Locator to select the Shut Down button in the top button bar
 */
export function getShutDownServerButton(window: Page): Locator {
  return window.locator(".dome-xToolBar").getByTitle("Shut down the server");
}

/**
 * Locator to select the Console View
 */
export function getConsoleComponent(window: Page): Locator {
  return window.locator(".cm-global-box");
}

export function getMainFunction(window: Page): Locator {
  return window.getByText("main", { exact: true });
}

export function getServerStatusLabel(window: Page): Locator {
  return window.getByTitle("Server is running");
}
