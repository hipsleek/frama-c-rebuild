In all these tests please set HOME before executing commands so that it does not
touch the actual user HOME.
In addition, make sure that you use dune with option `--cache=disabled`,
in order not to pollute `home/.cache` in dune >= 3.16

Unset these variables to make sure we use HOME and HOME/.config unless stated
otherwise
  $ export XDG_CONFIG_HOME= XDG_STATE_HOME= XDG_CACHE_HOME=

  $ dune build --cache=disabled --root . @install

Basic case
  $ HOME=home dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories
  [dirs] Not created:
  [dirs] home/.cache/frama-c/dirs/not_created
  [dirs] home/.config/frama-c/dirs/not_created
  [dirs] home/.local/state/frama-c/dirs/not_created
  [dirs] .frama-c/dirs/not_created
  [dirs] .frama-c/dirs/not_created_filepath/file
  $ find home | sort
  home
  home/.cache
  home/.cache/frama-c
  home/.cache/frama-c/dirs
  home/.cache/frama-c/dirs/created
  home/.config
  home/.config/frama-c
  home/.config/frama-c/dirs
  home/.config/frama-c/dirs/created
  home/.local
  home/.local/state
  home/.local/state/frama-c
  home/.local/state/frama-c/dirs
  home/.local/state/frama-c/dirs/created
  $ rm -rf home

Customized via variables: XDG level
  $ HOME=home XDG_CACHE_HOME=cache XDG_CONFIG_HOME=config XDG_STATE_HOME=state dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories
  [dirs] Not created:
  [dirs] cache/frama-c/dirs/not_created
  [dirs] config/frama-c/dirs/not_created
  [dirs] state/frama-c/dirs/not_created
  [dirs] .frama-c/dirs/not_created
  [dirs] .frama-c/dirs/not_created_filepath/file
  $ find home | sort
  find: 'home': No such file or directory
  $ find cache | sort
  cache
  cache/frama-c
  cache/frama-c/dirs
  cache/frama-c/dirs/created
  $ find config | sort
  config
  config/frama-c
  config/frama-c/dirs
  config/frama-c/dirs/created
  $ find state | sort
  state
  state/frama-c
  state/frama-c/dirs
  state/frama-c/dirs/created
  $ rm -rf home cache config state

Customized via variables: Kernel level
  $ HOME=home FRAMAC_CACHE=cache FRAMAC_CONFIG=config FRAMAC_STATE=state FRAMAC_SESSION=session dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories
  [dirs] Not created:
  [dirs] cache/dirs/not_created
  [dirs] config/dirs/not_created
  [dirs] state/dirs/not_created
  [dirs] session/dirs/not_created
  [dirs] session/dirs/not_created_filepath/file
  $ find home | sort
  find: 'home': No such file or directory
  $ find cache | sort
  cache
  cache/dirs
  cache/dirs/created
  $ find config | sort
  config
  config/dirs
  config/dirs/created
  $ find state | sort
  state
  state/dirs
  state/dirs/created
  $ rm -rf home cache config state

Customized via variables: Plugin level
  $ HOME=home FRAMAC_DIRS_CACHE=cache FRAMAC_DIRS_CONFIG=config FRAMAC_DIRS_STATE=state FRAMAC_DIRS_SESSION=session dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories
  [dirs] Not created:
  [dirs] cache/not_created
  [dirs] config/not_created
  [dirs] state/not_created
  [dirs] session/not_created
  [dirs] session/not_created_filepath/file
  $ find home | sort
  find: 'home': No such file or directory
  $ find cache | sort
  cache
  cache/created
  $ find config | sort
  config
  config/created
  $ find state | sort
  state
  state/created
  $ rm -rf home cache config state

Customized via options kernel level
  $ HOME=home dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -cache cache -config config -state state -session session
  [dirs] Not created:
  [dirs] cache/dirs/not_created
  [dirs] config/dirs/not_created
  [dirs] state/dirs/not_created
  [dirs] session/dirs/not_created
  [dirs] session/dirs/not_created_filepath/file
  $ find home | sort
  find: 'home': No such file or directory
  $ find cache | sort
  cache
  cache/dirs
  cache/dirs/created
  $ find config | sort
  config
  config/dirs
  config/dirs/created
  $ find state | sort
  state
  state/dirs
  state/dirs/created
  $ rm -rf home cache config state

Customized via options plug-in level
  $ HOME=home dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache cache -dirs-config config -dirs-state state -dirs-session session
  [dirs] Not created:
  [dirs] cache/not_created
  [dirs] config/not_created
  [dirs] state/not_created
  [dirs] session/not_created
  [dirs] session/not_created_filepath/file
  $ find home | sort
  find: 'home': No such file or directory
  $ find cache | sort
  cache
  cache/created
  $ find config | sort
  config
  config/created
  $ find state | sort
  state
  state/created
  $ rm -rf home cache config state

Customized plug-in subdir option > plug-in subdir variable
  $ HOME=home FRAMAC_DIRS_VAR=subdir_bad dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache-only -dirs-optvar subdir
  [dirs] User Error: home/.cache/frama-c/dirs/created is expected to be a file
  [dirs] User Error: subdir is expected to be a file
  $ rm -rf home cache subdir

Customized plug-in subdir variable > plug-in option
  $ HOME=home FRAMAC_DIRS_VAR=subdir dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache-only -dirs-cache cache
  [dirs] User Error: cache/created is expected to be a file
  [dirs] User Error: subdir is expected to be a file
  $ rm -rf home cache subdir

Customized plug-in option > plug-in var
  $ HOME=home FRAMAC_DIRS_CACHE=cache_bad dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache-only -dirs-cache cache
  [dirs] User Error: cache/created is expected to be a file
  [dirs] User Error: cache/optvar is expected to be a file
  $ rm -rf home cache

Customized plug-in var > kernel option
  $ HOME=home FRAMAC_DIRS_CACHE=cache dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache-only -cache cache_bad
  [dirs] User Error: cache/created is expected to be a file
  [dirs] User Error: cache/optvar is expected to be a file
  $ rm -rf home cache

Customized kernel option > kernel var
  $ HOME=home FRAMAC_CACHE=cache_bad dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache-only -cache cache
  [dirs] User Error: cache/dirs/created is expected to be a file
  [dirs] User Error: cache/dirs/optvar is expected to be a file
  $ rm -rf home cache

Customized kernel var > xdg var
  $ HOME=home XDG_CACHE_HOME=cache_bad FRAMAC_CACHE=cache dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache-only
  [dirs] User Error: cache/dirs/created is expected to be a file
  [dirs] User Error: cache/dirs/optvar is expected to be a file
  $ rm -rf home cache

Bad home value
  $ HOME= dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories
  [dirs] User Error: Failure when creating directories
  [dirs] Deferred error message was emitted during execution:
    Failure when creating directories
  [kernel] Plug-in dirs aborted: invalid user input.
  [1]

Bad home permission
  $ mkdir home
  $ chmod -w home
  $ HOME=home dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories
  [dirs] User Error: cannot create cache directory `home/.cache/frama-c/dirs/created'
  [kernel] Plug-in dirs aborted: invalid user input.
  [1]
  $ rm -rf home

File already exists were a directory is expected
  $ mkdir cache
  $ touch cache/created
  $ HOME=home dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-cache cache
  [dirs] User Error: cache/created is expected to be a directory
  [kernel] Plug-in dirs aborted: invalid user input.
  [1]
  $ rm -rf cache
