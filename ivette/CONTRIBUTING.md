# Documentation

The following guides document the `Dome` framework, used to build and extend
`Ivette`.

- [Dome Framework](src/dome/doc/guides/dome.md)
- [Quick Start](src/dome/doc/guides/quickstart.md)
- [Live Editing](src/dome/doc/guides/hotreload.md)
- [Application Design](src/dome/doc/guides/application.md)
- [Application Development](src/dome/doc/guides/development.md)
- [Styling Components](src/dome/doc/guides/styling.md)
- [Custom Hooks](src/dome/doc/guides/hooks.md)
- [Glossary](src/dome/doc/guides/glossary.md)

# Makefile Targets

The most useful development targets are:

```
$ make tsc   # run linter
$ make dev   # run development version
$ make app   # build production version
$ make dist  # package the application
$ make doc   # generate the documentation
$ make api   # update the frama-c server API
```

Additional targets and environment variables are listed by:

```
$ make dome-help
```

# Editor Configuration

`Ivette` development mainly relies on a modern web stack based on `TypeScript`,
`React`, and related tooling. Suggested editors are those providing support for
`TypeScript` language services, `JSX/TSX` editing, code navigation, linting
integration, and automatic formatting.

The following editor configurations are known to work well with the project.

## Emacs

`Emacs` mode configuration can be setup with `Typescript`, `Web-mode` and `Tide`
packages. You can install them with `M-x package-install`:

```
M-x package-refresh-contents ;; updates your index
M-x package-install web-mode
M-x package-install typescript-mode
M-x package-install tide
```

For configuring your `.emacs` accordingly, please look at the
`src/dome/template/typescript.el` file, which setups the `Tide` package to work
with `typescript-mode` for `*.ts` files (see also `tsfmt.json` config file) and
`Web-mode` for `*.tsx` files.

Useful commands are:

```
M-. goto definition
M-, back to previous point
M-x tide-documentation-at-point
M-x tide-error-at-point
```

## VS Code

`VS Code` has native support for `Typescript` (and `JavaScript`, of course), in
terms of code navigation, syntax highlighting and formatting, compiler errors
and warnings. The same holds for `React` development.

Useful extensions:
- `ESlint` provides support for lint errors and warnings
- `ES7 React/Redux/GraphQL/React-Native snippets` provides boilerplate snippets

# Coding Rules

Coding rules are mostly enforced by `Eslint`. It is mostly composed of all
recommended rules for `Typescript` and `React`. A few of them are slightly
weakened. Additionally, a small set of rules have been added, divided in two
categories.

1. *Safety rules*. These rules aim at avoiding errors by detecting common
   mistakes early. Most of these rules have been added after finding a bug that
   could have been avoided with this particular rule.
2. *Style rules*. These rules focus on code readability, including making things
   explicit or ensuring uniformity. They should not be very constraining.

In some cases, trying to apply rules strictly might be counterproductive. It is
possible to deactivate rules locally with one of the following syntaxes.

```javascript
/* eslint-disable no-console */
console.log('test');
/* eslint-enable no-console */
```

```javascript
console.log('test'); // eslint-disable-line no-console
```

```javascript
// eslint-disable-next-line no-console
console.log('test');
```

## Safety Rules

- [semi](https://eslint.org/docs/rules/semi)
  requires semicolon at the end of statements to avoid unexpected automatic
  semicolon insertion
- [react-hooks/exhaustive-deps](https://fr.reactjs.org/docs/hooks-rules.html#eslint-plugin)
  detects missing dependencies in hooks
- [@typescript-eslint/restrict-plus-operands](https://github.com/typescript-eslint/typescript-eslint/blob/main/packages/eslint-plugin/docs/rules/restrict-plus-operands.md)
  forbids additions other than the addition of two numbers or the addition of
  two strings. In other cases, implicit conversion might have unexpected
  results. To conform to this rule, it might be necessary to use explicit
  conversions or *template literals* (e.g. `` `N°${i}` ``)
- [eqeqeq](https://eslint.org/docs/rules/eqeqeq) forces the use of `===` instead
  of `==`. The latter often leads to unexpected comparisons in presence of
  `undefined`, `null`, `false`, etc
- [@typescript-eslint/no-unused-vars](https://github.com/typescript-eslint/typescript-eslint/blob/main/packages/eslint-plugin/docs/rules/no-unused-vars.md)
  forbids local variables and parameters which are declared but unused. It often
  hides an error. Variables starting with `_` are ignored
- [no-var](https://eslint.org/docs/rules/no-var)
  forces the usage of `let` or `const` instead of `var`, potentially avoiding
  scoping errors
- [@typescript-eslint/no-explicit-any](https://github.com/typescript-eslint/typescript-eslint/blob/main/packages/eslint-plugin/docs/rules/no-explicit-any.md)
  makes typing mandatory. `any` allows conversions from and into any type,
  effectively disabling the type checker. `unknown` type can often be used
  instead, with the obligation to verify the actual type at runtime or to use
  explicit unchecked conversions with `as`

## Style Rules

- [max-len](https://eslint.org/docs/rules/max-len) enforces a maximum line
  length of 80 columns, not including comments
- [camelcase](https://eslint.org/docs/rules/camelcase) enforces camelcase for
  variable names uniformly
- [@typescript-eslint/explicit-function-return-type](https://github.com/typescript-eslint/typescript-eslint/blob/main/packages/eslint-plugin/docs/rules/explicit-function-return-type.md)
  enforces the function return type to be always given. This often helps code
  understanding. The rule is disabled for function expressions which can stay
  concise
- [lines-between-class-members](https://eslint.org/docs/rules/lines-between-class-members)
  enforces empty lines between class members, except for single line
  declarations (e.g. properties)
- [no-constant-condition](https://eslint.org/docs/rules/no-constant-condition)
  forbids constant conditions which are often used during the debug process, but
  should not remain in release versions. While recommended in `Eslint`, this
  rule is weakened to allow infinite loops which make sense in `Ivette`
- [prefer-const](https://eslint.org/docs/rules/prefer-const) enforces the use of
  `const`, whenever possible. Systematic use of `const` makes mutations easier
  to spot and may help to identify problems early
- [no-console](https://eslint.org/docs/rules/no-console) forbids the use of
  `console.*` to prevent remaining debug outputs in code releases. For error
  printing in the console, the use of `Dome.Debug` is encouraged

# Sandboxing

It is possible to add visual tests and playgrounds inside `src/sandbox`
directory. Please read the associated
[src/sandbox/README.md](src/sandbox/README.md) instructions.
