/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import { test } from "@playwright/test";
import * as e2eService from "../libs/e2eService";

test("check server connection", async () => {
  const launchAppResult = await e2eService.launchIvette();
  const electronApp = launchAppResult.app;
  const window = launchAppResult.page;
  await e2eService.testServerIsStarted(window);
  await electronApp.close();
});
