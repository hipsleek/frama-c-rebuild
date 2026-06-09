This document presents the framework for testing Ivette by describing its
overall architecture, and how to launch current tests and write new ones.

## Overall Architecture

The framework is based on [Playwright](https://playwright.dev) library.

`Playwright` is meant for end-to-end testing, i.e. for testing specific
behaviors. Here is how the framework is organized:

- `src/` contains the end-to-end tests,
- `lib/` contains utilities for writing tests using `Playwright`

## Executing tests

**Requirement:** In order to execute the tests, Frama-C has to be built
beforehand.

Testing Ivette amounts to the execution of the following commands from the
Frama-C root directory:

```sh
$ make                         // builds Frama-C, if not already
$ cd ivette
$ make tests                   // builds Ivette and execute all tests
```

## Executing tests (Advanced)

Whenever the execution of the tests does not need (re-)building Ivette, one can
directly do the following from the Ivette root directory:

```sh
$ dune exec -- yarn playwright test
```

This is also useful for executing just a (list of) test(s). In such a case, do
the following from the Ivette root directory:

```sh
$ dune exec -- yarn playwright test /path/to/test1 /path/to/test2
```

For more information, refer to the `Playwright`
[documentation](https://playwright.dev/docs/running-tests).

## Accessing tests report

Upon a test failure, `Playwright` informs about the failing step, or assertion,
with a message on the console. This should be sufficient to debug the failure.
However, `Playwright` also writes down a report of the failure that can be
accessed as follows from the Ivette root directory:

```sh
$ yarn playwright show-report
```

The previous command should open a browser tab showing the last recorded report;
if not, such a report should be available at `http://localhost:9323`.

## Writing end-to-end tests

End-to-end testing is meant for testing specific Ivette behaviors. This is done
by using the `Playwright` library, which is based on [expect
assertionts](https://playwright.dev/docs/test-assertions) on elements of a web
page at a given moment, named
[locators](https://playwright.dev/docs/api/class-locator).

A test for Ivette looks like this:

```ts
import { test } from "@playwright/test";
import * as e2eService from "../libs/e2eService";

test("<test description>", async () => {
  const launchAppResult = await e2eService.launchApp(
    <arguments>,
  );

  const electronApp = launchAppResult.app;
  const window = launchAppResult.page;

  // find/select HTML element via Playwright's
  // [locators](https://playwright.dev/docs/locators].
  const locator = await window.<locator>;

  // perform a Playwright's [action](https://playwright.dev/docs/input)
  // on the locator.
  await locator.<action>;

  // [test assertion](https://playwright.dev/docs/test-assertions) on a
  // Playwright's value (e.g. locator) with respect to a Playwright's matcher.
  await expect(<value>).<matcher>;

  // exit app.
  await electronApp.close();
});
```

The Ivette testing framework abstracts the `Playwright`'s API by providing
common utilities in the sub-directory `/tests/libs`. In particular,
`/tests/libs/e2eService.ts` provides, and should be enriched with, common
testing services for Ivette, while `/tests/libs/locatorsUtil.ts` provides, and
should be enriched with, common locators for Ivette.

Note that `/tests/libs/e2eService.ts` also provides with some examples of
`<arguments>` to fed `e2eService.launchApp()` function with. In particular, use
the `Dome`'s `--settings CLEAN` option for launching Ivette with default
settings, and the `--settings </path/to/json-file-with-settings>`
option for launching Ivette with particular settings.

`Playwright` also provides an interactive way for identifying new locators.
Adding `await window.pause()` inside a test will open an additional `Playwright`
window with a `Pick locator` button at the bottom, which provides the necessary
locator any selected element (i.e., it works similarly to the inspect
functionality of most browser developer tools).

Complete test scenarios are provided in the sub-directory `/tests/src/`.
