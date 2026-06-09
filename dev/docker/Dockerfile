# Inspired by https://github.com/Frederic-Boulanger-UPS/docker-framac and
# https://gitlab.inria.fr/why3/why3/-/blob/master/misc/Dockerfile.deploy

# DISTRO must be passed via --build-arg (see Makefile)
ARG DISTRO
ARG OCAML_VERSION_SHORT

# Stage 1: install dependencies
FROM ocaml/opam:$DISTRO-ocaml-$OCAML_VERSION_SHORT AS base

ARG OPAM

RUN sudo ln -f /usr/bin/opam-${OPAM} /usr/bin/opam && opam init --reinit -ni
COPY reference-configuration.md /init/
RUN sudo chmod a+r /init/reference-configuration.md
COPY install.sh /init/
RUN sudo chmod a+rx /init/install.sh
# grep needs to be explicitly installed in Alpine to replace busybox with
# GNU grep, for option '-z'
RUN /init/install.sh \
  grep \
  time \
  wget

# Install Frama-C depexts and dependencies
COPY opam-install-fc-deps-from-ref-config.sh /init/
RUN sudo chmod a+rx /init/opam-install-fc-deps-from-ref-config.sh
RUN /init/opam-install-fc-deps-from-ref-config.sh

RUN eval $(opam env)

# Stage 2: install Frama-C itself

FROM base AS frama-c

## from_archive: if "git", clone from public Git; if prefixed with http,
## wget it; otherwise, use the given archive name.
## Note: the archive must contain a root directory
## starting with 'frama-c'
ARG from_archive=git

## The following line copies a frama-c archive if it exists, else nothing
COPY --chown=opam:opam README.md frama-c-*.tar.gz /frama-c/

RUN \
if [ "${from_archive}" != "git" ]; then \
  (cd /frama-c && \
    case "${from_archive}" in \
    "http"*) wget "${from_archive}" ;; \
          *) ;; \
    esac && \
    tar xvf "${from_archive##*/}" --strip 1 && \
    rm -f frama-c-*.tar.gz) \
; else \
  (cd /frama-c && rm -f * && git clone --depth 1 https://git.frama-c.com/pub/frama-c.git .) \
; fi

# E-ACSL (incompatible with musl-libc) is disabled on Alpine
RUN eval $(opam env) && cd /frama-c $([ -e /etc/alpine-release ] && echo "&& ./dev/disable-plugins.sh e-acsl") && make clean && make && make install

## Run quick sanity checks
## Note: E-ACSL tests are disabled on Alpine
RUN \
eval $(opam env) && \
printf "int g;\n//@assigns g;\nvoid f(){g = 1;}\nint main() {f(); return g;}" > /tmp/simple.c && \
frama-c -eva -wp -report /tmp/simple.c && \
rm -f /tmp/simple.c && \
echo "int main() { /*@ assert 1 + 2 == 2; */ return 0; }" > /tmp/test-eacsl.c && \
[ -e /etc/alpine-release ] || \
 (cd /tmp && e-acsl-gcc -c test-eacsl.c && \
 ./a.out.e-acsl 2>&1 | grep -q "Assertion failed" && \
 rm -f ./a.out* test-eacsl.c)

## Cleanup
RUN \
  eval $(opam env) && \
  opam clean --yes --switch-cleanup && \
  rm -rf /home/opam/opam-repository

# Stage 5 (optional): tests

FROM frama-c AS tests

## Run standard Frama-C tests
RUN eval $(opam env) && cd /frama-c && make run-ptests && dune build @ptests_config

## Run an extra test for WP with provers
RUN eval $(opam env) && cd /frama-c/ && cd src/plugins/wp/tests/ && frama-c -wp wp_gallery/binary-multiplication-without-overflow.c -wp-prover alt-ergo
