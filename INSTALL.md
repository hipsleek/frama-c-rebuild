# Installing Frama-C

- [Installing Frama-C](#installing-frama-c)
    - [Table of Contents](#table-of-contents)
    - [Installing Frama-C via opam](#installing-frama-c-via-opam)
        - [Installing opam](#installing-opam)
        - [Installing Frama-C from opam repository](#installing-frama-c-from-opam-repository)
        - [Installing Custom Versions of Frama-C](#installing-custom-versions-of-frama-c)
        - [Installing Frama-C on Windows via WSL](#installing-frama-c-on-windows-via-wsl)
        - [Installing Frama-C on macOS](#installing-frama-c-on-macos)
    - [Installing the GUI via the online packages](#installing-the-gui-via-the-online-packages)
        - [On Linux](#installing-the-gui-via-the-online-packages-on-linux)
        - [On macOS](#installing-the-gui-via-the-online-packages-on-macos)
    - [Installing Frama-C via your Linux distribution (Debian/Ubuntu/Fedora)](#installing-frama-c-via-your-linux-distribution-debianubuntufedora)
    - [Compiling from source](#compiling-from-source)
        - [Quick Start](#quick-start)
        - [Full Compilation Guide](#full-compilation-guide)
- [Testing the Installation](#testing-the-installation)
    - [Available resources](#available-resources)
        - [Executables: (in `/INSTALL_DIR/bin`)](#executables-in-install_dirbin)
        - [Shared files: (in `/INSTALL_DIR/share/frama-c` and subdirectories)](#shared-files-in-install_dirshareframa-c-and-subdirectories)
        - [Documentation files: (in `/INSTALL_DIR/share/frama-c/doc`)](#documentation-files-in-install_dirshareframa-cdoc)
        - [Object files: (in `/INSTALL_DIR/lib/frama-c`)](#object-files-in-install_dirlibframa-c)
        - [Plugin files: (in `/INSTALL_DIR/lib/frama-c/plugins`)](#plugin-files-in-install_dirlibframa-cplugins)
        - [Man files: (in `/INSTALL_DIR/share/man/man1`)](#man-files-in-install_dirsharemanman1)
- [Installing Additional Frama-C Plugins](#installing-additional-frama-c-plugins)
- [Frama-C additional tools](#frama-c-additional-tools)
    - [HAVE FUN WITH FRAMA-C!](#have-fun-with-frama-c)

## Installing Frama-C via opam

[opam](http://opam.ocaml.org/) is the OCaml package manager. Every Frama-C
release is made available via an opam package.

First you need to install opam, then you may install Frama-C using opam.

### Installing opam

Several Linux distributions already include an `opam` package.

macOS has opam through Homebrew.

Windows users can install opam via WSL (Windows Subsystem for Linux).

If your system does not have an opam package, you can use the provided
opam binaries available at:

http://opam.ocaml.org/doc/Install.html

Note: the `opam` binary itself is very small, but the initialization of an
*opam switch* usually takes time and disk space, since it downloads and builds
an OCaml compiler. Please refer to the opam documentation for details.

### Installing Frama-C (including dependencies) via opam

The Frama-C package in opam is called `frama-c`. It includes:
- the command-line `frama-c` executable;
- the graphical interface, `frama-c-gui`.

Note: GUI's dependencies are _not_ included in the opam package,
but downloaded from `npm` when the user runs `frama-c-gui` for the first time.

`frama-c` has some non-OCaml dependencies, such as GMP. opam includes
a mechanism (`depext`) to handle such dependencies. It may require
administrative rights to install system packages (e.g. `libgmp`).

If your `opam` version is >= 2.1, such dependencies are installed
automatically when installing `frama-c` itself:

```shell
# install Frama-C and dependencies with opam >= 2.1
opam install frama-c
```

If your opam version is < 2.1, you need to install `depext` first, then
use it to install Frama-C's dependencies:

```shell
# install Frama-C's dependencies with pre-2.1 opam
opam install depext
opam depext frama-c
# then install Frama-C itself
opam install frama-c
```

If there are errors due to missing external dependencies, opam usually emits a
message indicating which packages to install. If this is not sufficient,
there may be missing dependencies in opam's packages for your system. In this
case, you may [create a Gitlab issue](https://git.frama-c.com/pub/frama-c/issues/new)
indicating your distribution and error message.

### Configuring provers for Frama-C/WP

Frama-C/WP uses the [Why3](http://why3.lri.fr/) platform to run external provers
for proving ACSL annotations.
The Why3 platform and the Alt-Ergo prover are automatically installed _via_ opam
when installing Frama-C.

Other recommended, efficient provers are CVC4 and Z3.
They can be used as replacement or combined with Alt-Ergo.
Actually, you can use any prover supported by Why3 in combination with Frama-C/WP.

Most provers are available on all platforms. After their installation,
they will be automatically detected from `$PATH` and used by Frama-C/WP.

### Reference configuration

See file [reference-configuration.md](reference-configuration.md)
for a set of packages that is known to work with this version of Frama-C.

### Installing Custom Versions of Frama-C

If you have a **non-standard** version of Frama-C available
(with proprietary extensions, custom plugins, etc.),
you can still use opam to install Frama-C's dependencies and compile your
own sources directly:

```shell
# optional: remove the standard frama-c package if it was installed
opam remove --force frama-c

# install Frama-C's dependencies
opam install depext # only for opam < 2.1.0
opam depext frama-c # only for opam < 2.1.0
opam install --deps-only frama-c [--with-test]

# install custom version of frama-c
opam pin add --kind=path frama-c <dir>
```

where `<dir>` is the root of your unpacked Frama-C archive.
See `opam pin` for more details. The option `--with-test` is optional and
is necessary only if you want to be able to execute the tests available in
the frama-c repository.

If your extensions require other libraries than the ones already used
by Frama-C, they must of course be installed as well.

### Installing Frama-C on Windows via WSL

Frama-C is developed on Linux, but it can be installed on Windows using the
Windows Subsystem for Linux (WSL).

**Note**: if you have WSL2 (Windows 11), you can run the graphical interface
          directly, thanks to WSLg. If you are using WSL 1, you need to install
          an X server for Windows, such as VcXsrv
          (see section "Running the Frama-C GUI on WSL").

#### Prerequisites: WSL + a Linux distribution

To enable WSL on Windows, you may follow these instructions
(we tested with Ubuntu 20.04 LTS;
other distributions/versions should also work,
but the instructions below may require some modifications).

https://docs.microsoft.com/en-us/windows/wsl/install

Notes:

- older builds of Windows 10 and systems without access to the
  Microsoft Store may have no compatible Linux packages.
- in WSL 1, Frama-C/WP cannot use Why3 because of some missing features in WSL
  support, thus using WSL 2 is **highly recommended**.

#### Installing opam and Frama-C on WSL

To install opam, some packages are required. The following commands can be
run to update the system and install those packages:

```shell
sudo apt update
sudo apt upgrade
sudo apt install make m4 gcc opam gnome-icon-theme
```

Then opam can be set up using these commands:

```shell
opam init --disable-sandboxing --shell-setup
eval $(opam env)
opam install -y depext
```

You can force a particular Ocaml version during `opam init` by using the option
`-c <version>` if needed. For instance, you can try installing the OCaml version
mentioned in the [reference configuration](reference-configuration.md).

Now, to install Frama-C, run the following commands, which will use `apt` to
install the dependencies of the opam packages and then install them:

```shell
opam depext --install -y lablgtk3 lablgtk3-sourceview3
opam depext --install -y frama-c
```

#### Running the Frama-C GUI on WSL

If you have WSL2: a known issue with some versions of Frama-C, lablgtk3 and
Wayland requires prefixing the command running the Frama-C GUI with
`GDK_BACKEND=x11`, as in:

```shell
GDK_BACKEND=x11 frama-c-gui <options>
```

If you have WSL 1: WSL 1 does not support graphical user interfaces directly.
If you want to run Frama-C's GUI, you need to install an X server,
such as VcXsrv or Cygwin/X. We present below how to install VcXsrv.

First, install VcXsrv from:

https://sourceforge.net/projects/vcxsrv/

The default installation settings should work.

Now run VcXsrv from the Windows menu (it is named XLaunch), the firewall
must authorize both "Public" and "Private" domains. On the first configuration
screen, select "Multiple Windows". On the second:

- keep "Start no client" selected,
- keep "Native opengl" selected,
- select "Disable access control".

Now specific settings must be provided in WSL. you can put the export commands
in your `~/.bashrc` file, so this is done automatically when starting WSL.

##### WSL 1

The Xserver is ready. From WSL, run:

```shell
export LIBGL_ALWAYS_INDIRECT=1
export DISPLAY=:0
frama-c-gui
```

##### WSL 2

```shell
export LIBGL_ALWAYS_INDIRECT=1
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
frama-c-gui
```

### Installing Frama-C on macOS

[opam](https://opam.ocaml.org) works perfectly on macOS via
[Homebrew](https://brew.sh).
We highly recommend to rely on it for the installation of Frama-C.

1. Install *required* general macOS tools for OCaml:

    ```shell
    brew install opam
    ```

   Do not forget to `opam init` and `eval $(opam env)` for a proper
   opam installation (if not already done before).

2. Set up a compatible OCaml version (replace `<version>` with the version
   indicated in the [reference configuration](#reference-configuration)):

    ```shell
    opam switch create <version>
    ```

3. Install Frama-C:

    ```shell
    opam install frama-c
    ```

   Opam may ask for the administrator password to handle required system
   dependencies.

   **It is likely that, at this point, opam will fail due to some Homebrew
   packages and pkg-config.** The error messages should indicate what you need
   to do, e.g.:

    ```shell
    For pkg-config to find zlib you may need to set:
      export PKG_CONFIG_PATH="/usr/local/opt/zlib/lib/pkgconfig"
    ```

    After setting such environment variables, re-running `opam install frama-c`
    should work. If you still have issues, try manually installing the required
    packages (via `brew install <package>`) and then re-installing Frama-C.
    **Note**: opam packages prefixed with `conf-` only check if the
    corresponding system package is findable. If you cannot install some
    `conf-` package, the solution will likely involve Homebrew and/or setting
    environment variables, not opam.

4. Install the Frama-C graphical interface.

   Instructions on installing and running it are presented by opam when
   the `frama-c` package is installed. Follow them to get it running.

## Installing the GUI via the online packages

**Warning:** if you already have a `frama-c-gui` script along with `frama-c`, that
script is used for bootstrapping the installation of the GUI from source through
your internet connection.  The instructions provided here are intended to
_replace_ the installation procedure from source. Hence, it is highly
recommended for you to remove the bootstrapping `frama-c-gui` script if you want to
use the binary distribution of the GUI.

Only stable distributions are available online for now.
Download the GUI distribution that corresponds to your version of Frama-C,
following the appropriate link from this page: https://frama-c.com/html/framac-versions.html

### Installing the GUI via the online packages on Linux

Requirement: libfuse2 must be installed.

Download the binary distribution (for now, only x86-64 and ARM64 are supported).
Install it wherever you want:

```sh
cp frama-c-gui-linux-<arch>-<version>.AppImage <GUI-INSTALL-PATH>/frama-c-gui.AppImage
```

Then add an alias `frama-c-gui` that just runs the AppImage:

```sh
alias frama-c-gui=<ABSOLUTE-GUI-INSTALL-PATH>/frama-c-gui.AppImage
```

### Installing the GUI via the online packages on macOS

Download the universal binary distribution and install it, typically
in `/Applications/frama-c-gui.app`. To launch the GUI from the command line, you will
need your own `frama-c-gui` script, like the following one:

```sh
#! /usr/bin/env sh
exec open -na <GUI-INSTALL>/frama-c-gui.app --args\
  --command <FRAMAC-INSTALL>/frama-c\
  --working $PWD $*
```

Simply replace `<GUI-INSTALL>` and `<FRAMAC-INSTALL>` in the code above with
the (absolute) paths to your `frama-c-gui.app` and `frama-c` binaries,
respectively. Then, make your `frama-c-gui` script executable and simply use it like
the `frama-c` command-line binary!

## Installing Frama-C via your Linux distribution (Debian/Ubuntu/Fedora)

**NOTE**: Distribution packages are updated later than opam packages,
          so if you want access to the most recent versions of Frama-C,
          opam is currently the recommended approach.

Also note that it is **not** recommended to mix OCaml packages installed by
your distribution with packages installed via opam. When using opam,
we recommend uninstalling all `ocaml-*` packages from your distribution, and
then installing, exclusively via opam, an OCaml compiler and all the OCaml
packages you need. This ensures that only those versions will be in the PATH.

The advantage of using distribution packages is that dependencies are almost
always handled by the distribution's package manager. The disadvantage is that,
if you need some optional OCaml package that has not been packaged in your
distribution (e.g. `landmarks`, which is distributed via opam), it may be very
hard to install it, since mixing opam and non-opam packages often fails
(and is **strongly** discouraged).

Debian/Ubuntu: `apt-get install frama-c`

Fedora: `dnf install frama-c`

Arch Linux: `pikaur -S frama-c`

## Compiling from source

**Note**: These instructions are no longer required in the vast majority
          of cases. They are kept here mostly for historical reference.

### Quick Start

1. Install [opam](http://opam.ocaml.org/) and use it to get all of Frama-C's
   dependencies (including some external ones):

   ```shell
   opam install depext # only for opam < 2.1.0
   opam depext frama-c # only for opam < 2.1.0
   opam install --deps-only frama-c
   ```

   If not using [opam](http://opam.ocaml.org/), you will need to install
   the Frama-C dependencies by yourself. The `opam` file in the Frama-C
   .tar.gz lists the required dependencies (e.g. `ocamlfind`, `ocamlgraph`,
   `zarith`, etc.). A few of these dependencies are optional, only required
   for the graphical interface: `lablgtk`, `conf-gnomecanvas` and
   `conf-gtksourceview` (or the equivalent Gtk+3 packages).

2. On Linux-like distributions:

   ```shell
   make RELEASE=yes && make install
   ```

   See section *Installation* below for options.

3. On Windows+Cygwin:

   ```shell
   make RELEASE=yes && make install
   ```

### Full Compilation Guide

#### Frama-C Requirements

See the `opam` file, section `depends`, for compatible OCaml versions and
required dependencies (except for those related to `lablgtk`, which are
required for the GUI but otherwise optional).

To install the required dependencies, you can use opam v2.1
or higher to do the following (assuming you are in frama-c
root source folder):

1. `opam switch create frama-c ocaml-base-compiler.4.14.2`
   to create a compatible opam switch
2. `opam pin . -n` to pin to the latest development version
3. `opam install --deps-only .` will fetch and build all
   relevant dependencies

#### Compilation

There are basically two compilation modes: development and release.

Typing `make` builds in development mode, this is a shortcut for:
```shell
dune build @install
```

Typing `make RELEASE=yes` builds in release mode, this is a shortcut for:
```shell
dune build @install --release --promote-install-files=false
```

For more precise build configurations, use directly the `dune` command.

#### Testing

Basic tests can be executed using:

```shell
make run-ptests
make default-tests
```

#### Installation

Type `make install` (depending on the installation directory, this may require
superuser privileges. The installation directory is chosen through the variable
`PREFIX`). This is a shortcut for:

```shell
dune install
```

Makefile (and dune) support the `DESTDIR` variable, which can be used to
configure the location of the installation.

#### API Documentation

For plugin developers, the API documentation of the Frama-C kernel and
distributed plugins is available in the `_build/default/_doc/_html` directory
after running:

```shell
make doc
```

or:

```shell
dune build @doc
```

#### Uninstallation

Type `make uninstall` to remove Frama-C and all the installed plugins.
(Depending on the installation directory, this may require superuser
privileges.)


# Testing the Installation

This step is optional.

Download some test files:

```shell
export PREFIX_URL="https://git.frama-c.com/pub/frama-c/raw/master/tests/value"
wget -P test ${PREFIX_URL}/CruiseControl.c
wget -P test ${PREFIX_URL}/CruiseControl_const.c
wget -P test ${PREFIX_URL}/CruiseControl.h
wget -P test ${PREFIX_URL}/CruiseControl_extern.h
wget -P test ${PREFIX_URL}/scade_types.h
wget -P test ${PREFIX_URL}/config_types.h
wget -P test ${PREFIX_URL}/definitions.h
```

Then test your installation by running:

```shell
frama-c -eva test/CruiseControl*.c
# or (if the GUI is available)
frama-c-gui -eva test/CruiseControl*.c
```

# Available resources

Once Frama-C is installed, the following resources should be installed and
available:

## Executables: (in `/INSTALL_DIR/bin`)

- `frama-c`
- `frama-c-gui`       if available
- `frama-c-ptests`    testing tool for Frama-c
- `frama-c-wtests`    testing tool for Frama-c
- `frama-c-script`    utilities related to e.g. analysis parametrization

## Shared files: (in `/INSTALL_DIR/share/frama-c` and subdirectories)

- some `Makefiles` used to compile dynamic plugins
- some image files used by the Frama-C GUI
- some files for Frama-C/plug-in development (autocomplete scripts,
  Emacs settings, scripts for running Eva, ...)
- an annotated C standard library (with ACSL annotations) in `libc`
- plugin-specific files (in directories `wp`, `e-acsl`, etc.)

## Object files: (in `/INSTALL_DIR/lib/frama-c`)

- object files used to compile dynamic plugins

## Plugin files: (in `/INSTALL_DIR/lib/frama-c/plugins`)

- object files of available dynamic plugins

## Man files: (in `/INSTALL_DIR/share/man/man1`)

- `man` files for `frama-c` (and `frama-c-gui` if available)


# Installing Additional Frama-C Plugins

Plugins may be released independently of Frama-C.

The standard way for installing them should be:

```shell
dune build @install && dune install
```

Plugins may have their own custom installation procedures.
Consult their specific documentation for details.

# Frama-C additional tools

A few additional tools that are unnecessary for most users might be useful for
plugin developers. These tools are:
- `frama-c-lint`, used for checking coding conventions
- `frama-c-hdrck`, used for checking file license headers

They can be installed either by pinning them via Opam:

```shell
opam pin tools/lint
opam pin tools/hdrck
```

Or by compiling/installing Frama-C manually in developer mode:
```shell
FRAMAC_DEVELOPER=yes make
FRAMAC_DEVELOPER=yes make install

# Or:
make -C tools/<the-tool>
make -C tools/<the-tool> install
```

For convenience, `FRAMAC_DEVELOPER` can be exported globally.


# HAVE FUN WITH FRAMA-C!
