#!/bin/bash

dir="_build/default"

find $dir/failed_cases \
  -name 'const*.i' \
  -not -empty \
  -exec echo 'Running Frama-C with mutable_const_fail on {}' ';' \
  -exec cat '{}' ';' \
  -exec frama-c -no-autoload-plugins -load-module="$dir/mutable_const_fail.cmxs" '{}' ';'

find $dir/failed_cases \
  -name 'mutable*.i' \
  -not -empty \
  -exec echo 'Running Frama-C with mutable_mutable_fail on {}' ';' \
  -exec cat '{}' ';' \
  -exec frama-c -no-autoload-plugins -load-module="$dir/mutable_mutable_fail.cmxs" '{}' ';'
