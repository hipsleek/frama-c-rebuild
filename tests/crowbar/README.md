# Crowbar tests

This folder groups some tests using the OCaml library Crowbar.
To run these tests the environment variable `CROWBAR` must be defined :

```bash
CROWBAR= dune test tests/crowbar
```

If `CROWBAR` is defined, these tests can also be run using `dune build
@runtest`.

# Adding a test

tests are seen as separate dune projects, so to create a new test, you must
create a directory `my_test.t` containing the following files:

- `dune-project`
- `dune`
- `run.t`
- `my_test.ml`

Except for `my_test.ml`, which contains your actual test sources, the other three can
basically be copied from existing directories (just make sure that you change the
`name` of the `test` in `dune` to match `my_test`, and, if needed, update the list of
`libraries`, especially in terms of tested plug-ins

`Crowbar` runs in qcheck mode, generating by default 5000 random inputs. If you feel like
your particular test is taking to much time, you can add an `action` field in the `test` directive
of your `dune` file, e.g. `(action (run %{test} -r 2000))` for generating 2000 random test cases
