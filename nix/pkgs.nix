let
  sources = import ./sources.nix {};
  # we use clang-format for linting. Be sure to keep
  # that in sync with tools/lint/lint.ml
  clang = pkgs.clang_19;
  llvmPackages = pkgs.llvmPackages_19;
  ocamlOverlay = oself: osuper: {
    # External Packages
    alt-ergo = oself.callPackage ./alt-ergo.nix {};
    bisect_ppx = oself.callPackage ./bisect_ppx.nix {};
    combinetura = oself.callPackage ./combinetura.nix {};
    dolmen = oself.callPackage ./dolmen.nix {};
    ppxlib = oself.callPackage ./ppxlib.nix {};
    why3 = oself.callPackage ./why3.nix {
      coqPackages = pkgs.coqPackages_8_19 ;
    };
    # Helpers
    mk_tests = oself.callPackage ./mk_tests.nix {};
    mk_plugin = oself.callPackage ./mk_plugin.nix {};

    # Shells containing checkers (hdrck, ocp-indent, Frama-C for plugins)
    frama-c-checkers-shell = oself.callPackage ./frama-c-checkers-shell.nix {
      git = pkgs.git;
      clang = clang;
    };
    plugin-checkers-shell = oself.callPackage ./plugin-checkers-shell.nix {
      git = pkgs.git ;
    };

    # Builds
    frama-c = oself.callPackage ./frama-c.nix {};
    frama-c-hdrck = oself.callPackage ./frama-c-hdrck.nix {};
    frama-c-lint = oself.callPackage ./frama-c-lint.nix {};

    # Tests
    default-config-tests = oself.callPackage ./default-config-tests.nix {
      frama-c-no-cover = oself.frama-c.override {
        cover = false ;
      } ;
    };
    e-acsl-tests = oself.callPackage ./e-acsl-tests.nix {};
    eva-default-tests = oself.callPackage ./eva-tests.nix { config = ""; };
    eva-apron-tests = oself.callPackage ./eva-tests.nix { config = "apron" ; };
    eva-bitwise-tests = oself.callPackage ./eva-tests.nix { config = "bitwise" ; };
    eva-equality-tests = oself.callPackage ./eva-tests.nix { config = "equality" ; };
    eva-gauges-tests = oself.callPackage ./eva-tests.nix { config = "gauges" ; };
    eva-multidim-tests = oself.callPackage ./eva-tests.nix { config = "multidim" ; };
    eva-octagon-tests = oself.callPackage ./eva-tests.nix { config = "octagon" ; };
    eva-symblocs-tests = oself.callPackage ./eva-tests.nix { config = "symblocs" ; };
    full-tests = oself.callPackage ./full-tests.nix {};
    kernel-tests = oself.callPackage ./kernel-tests.nix {};
    crowbar-tests = oself.callPackage ./crowbar-tests.nix {};
    plugins-tests = oself.callPackage ./plugins-tests.nix {};
    wp-cache = oself.callPackage ./wp-cache.nix {};
    wp-tests = oself.callPackage ./wp-tests.nix {};

    # Internal tests
    internal-tests = oself.callPackage ./internal-tests.nix {
      clang = clang;
      llvmPackages = llvmPackages;
    };

    # Release
    api-doc = oself.callPackage ./api-doc.nix {};
    api-json-doc = oself.callPackage ./api-json-doc.nix {};
    manuals = oself.callPackage ./manuals.nix {};
    src-distrib-tests = oself.callPackage ./src-distrib-tests.nix {
      frama-c-release = oself.frama-c.override {
        release_mode = true ;
        cover = false ;
      } ;
    };
  };
  overlay = self: super: {
    niv = (import sources.niv {}).niv;
    ocaml-ng = super.lib.mapAttrs (
      name: value:
        if builtins.hasAttr "overrideScope" value
        then value.overrideScope ocamlOverlay
        else value
    ) super.ocaml-ng;
    inherit (super.callPackage sources."gitignore.nix" {}) gitignoreSource;
    why3 = throw "don't use pkgs.why3 but ocaml-ng.ocamlPackages_4_XX.why3";
    framac = throw "don't use pkgs.framac but ocaml-ng.ocamlPackages_4_XX.frama-c";
    frama-c = throw "don't use pkgs.framac but ocaml-ng.ocamlPackages_4_XX.frama-c";
  };
  pkgs = import sources.nixpkgs {
    # alt-ergo
    config.allowUnfree = true;
    overlays = [ overlay ];
  };
in
pkgs
