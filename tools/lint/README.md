## frama-c-lint tool

## Usage


```
> frama-c-lint -help
Usage: git ls-files -z | git check-attr --stdin -z -a | _build/install/default/bin/frama-c-lint [options]

Checks or updates files in relation to lint constraints specified by these git attributes:
  check-eoleof, check-syntax, check-utf8 and check-indent.

Options:
  --version        Prints tool version
  -c <json-config-file> Reads the JSON configuration file (allows to overload the default configuration)
  -e               Prints default JSON configuration
  -s               Considers warnings as errors for the exit value
  -u               Update ill-formed files (does not handle UTF8 update)
  -v               Verbose mode
  -help            Display this list of options
  --help           Display this list of options
```

## Managed Git Attributes

The tool manage the following `git` attributes:
`check-eoleof`, `check-syntax`, `check-utf8`, `check-indent`.

All of them can be `set` or `unset`, but a tool name can also be assignable to the `check-indent` attribute.

The check/update commands related to `check-eoleof`, `check-syntax` and `check-utf8` attributes are not overloadable and cannot be parametrized.
Only the commands related to `check-indent` attribute can be parametrized.

## The Parametrizable Configuration

The command `frama-c-lint -e` pretty prints the JSON description equivalent to the default parametrizable configuration related to `check-indent` attribute.

```
> frama-c-lint -e
Default JSON configuration:
[
  {
    "kind": "C",
    "extensions": [ ".c", ".h" ],
    "name": "clang-format",
    "available_cmd": "clang-format --version > /dev/null 2> /dev/null",
    "check_cmd": "clang-format --dry-run -Werror",
    "update_cmd": "clang-format -i"
  },
  {
    "kind": "Python",
    "extensions": [ ".py" ],
    "name": "black",
    "available_cmd": "black --version > /dev/null 2> /dev/null",
    "check_cmd": "black --quiet --line-length 100 --check",
    "update_cmd": "black --quiet --line-length 100"
  }
]
```

This description defines `black` and `clang-format` as the assignable values of the `git` attribute `check-indent`. It also specifies the system command to use for checking the availability of the tool to use for checking/updating the indentation. The check and update commands are also specified.

When the `check-indent` attribute is set without a value, the description specifies the tool to use from the extension of the file to check/update.

There is also a built-in configuration for `.ml` and `.mli` files based of `ocp-indent` tool (from a directly use of `ocp-indent` library to improve the efficiency of the tool) that can be overloaded if necessary.
That means there is an implicit overloadable JSON description:
```
[
  {
    "kind": "OCaml",
    "extensions": [ ".ml", ".mli" ],
    "name": "ocp-indent",
    "available_cmd": "...",
    "check_cmd": "...",
    "update_cmd": "..."
  }
]
```

The option  `-c <json-confi g-file>` allows to extend and/or overload the default configuration.

An empty string can also be set to the field `check_cmd` (resp. `update_cmd`) when the related tool does not offer check (resp. update) command.
When the `available_cmd` is set to and empty string, the tool is considered available except if the fields `check_cmd` `update_cmd` are both set to an empty string.
