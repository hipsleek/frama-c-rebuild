# Ivette

`Ivette` is the graphical user interface of `Frama-C`.

## Dependencies

Required packages:

- [node](https://nodejs.org/en) version **24.x** or later
- [yarn](https://yarnpkg.com/) for `node` package management
- [pandoc](https://pandoc.org/) for generating the documentation

### Linux

```sh
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
$ nvm install 24
$ nvm use 24
$ npm install --global yarn
```

Under `Arch Linux` you can rely on the `yarn` package and its standard `node`
dependency:

```sh
$ pacman -S yarn
```

### macOS

```sh
$ brew install yarn
$ brew install nvm # follow instructions
$ nvm install 24
$ nvm use 24
```

## Installation

You must configure `Frama-C` before installing `Ivette`. By default, the
installed `ivette` command looks for an installed `frama-c` to run the server.

From the `Frama-C` main directory, type the following command. The first time,
this might take some time to download all the necessary packages and
[Electron](https://www.electronjs.org/) binaries from the web.
```sh
$ make -C ivette dist
$ [sudo] make -C ivette install
```

The first command builds a binary distribution of `Ivette` for your architecture
in `ivette/dist/<arch>`. The second command installs it on your system.

The installed executable `<prefix>/bin/ivette` is a wrapper that launches the
`Ivette` application. The `Ivette` application itself is installed in:

- **Linux:** `<prefix>/lib/ivette/*`
- **macOS:** `/Applications/Ivette.app`

### Build Modes

`Ivette` supports several build modes:

- `make dev` builds and starts the development version with live code editing
  enabled. It uses local `Electron` binaries and is _not_ a packaged application
                                                              
- `make app` pre-builds the production application. It is not packaged _yet_,
  and still uses the local `Electron` binaries

- `make dist` packages the pre-built application into a new application for the
  host operating system (see `Installation` section)

Development and production versions can be launched with:
```sh
path/to/frama-c/bin/ivette
```

This is a wrapper that uses the local `path/to/frama-c/bin/frama-c` binary by
default, but this can be changed via command-line options.

## Command-Line Options

```sh
ivette [ivette options] [frama-c command line]

  -R, --reload           Re-run the last command from history
  -C, --working <dir>    Change working directory used by ivette and frama-c
  -B, --command <bin>    Set the frama-c server binary
  -U, --socket <socket>  Set the Linux socket name for the frama-c server
  --settings <file|DEFAULT>
                         Use the specified user settings
```

## Troubleshooting

When launching `Ivette`, you may encounter the following error:
```
The SUID sandbox helper binary was found, but is not configured correctly.
Rather than run without sandboxing I'm aborting now.
You need to make sure that path/to/chrome-sandbox is owned by root and has mode 4755.
```

This comes from `Electron`’s sandbox configuration. Two solutions exist:

#### 1. Launch Ivette without the sandbox (recommended workaround)

Start `Ivette` with `--no-sandbox` option.

This is currently the preferred workaround until the sandbox issue is resolved
at its source.

#### 2. Fix sandbox permissions (recommended for local builds)

Inside the directory produced by `make dist`, adjust permissions as indicated:
```sh
sudo chown root chrome-sandbox
sudo chmod 4755 chrome-sandbox
```

**Warning:** This requires root privileges and does not work with `AppImage`
builds.

## Contributing

See the [CONTRIBUTING.md](CONTRIBUTING.md) guide for development recommendations
and editor configuration if you want to work on `Ivette`’s source code.
