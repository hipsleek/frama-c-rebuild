/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import { test } from "@playwright/test";
import * as e2eService from "../libs/e2eService";

test("server connection with a C file to analyze", async () => {
  const launchAppResult =
    await e2eService.launchIvette("../tests/test/adpcm.c");
  const electronApp = launchAppResult.app;
  const window = launchAppResult.page;
  await e2eService.testFileIsLoaded(window, "tests/test/adpcm.c");
  await electronApp.close();
});
