# Frama-C Docker images

Frama-C Docker images are currently available on three flavors:
- A Debian-based image (default);
- A Fedora-based image;
- An Alpine-based image.

The exact version of the base Docker image is defined in the Makefile.
These images are based on opam's Docker images.

The user is `opam` and it has sudo rights.

Frama-C dependencies are installed according to the versions mentioned in
`reference-configuration.md`.

There are 2 main images, each with 3 variants:

- `dev` image: based on the public Frama-C git repository;
- `custom` image: based on a custom .tar.gz archive put in this directory.
  Note: it _must_ be named `frama-c-<something>.tar.gz`, where
  `<something>` may be a version number, codename, etc.
  It _must_ be put in this directory.

Note that only _some_ usages of Frama-C have been tested; notify us if your
intended plug-in or usage scenario does not work (and is not listed in the
*Known issues* below).

## Known issues

- E-ACSL is disabled on Alpine: its `musl` libc does not contain some debugging
  information required by E-ACSL.

## Built images

The `dev` images are tagged with their default names in the Docker Hub,
suffixed by the distribution, e.g. `framac/frama-c:dev.fedora`.
`framac/frama-c:dev` is an alias to the Debian image,
`framac/frama-c:dev.debian`.

The `custom` images are tagged with prefix `frama-c-custom`, since they are
intended for local usage.
