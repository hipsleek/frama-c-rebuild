/* run.config
  COMMENT: only the first value make sense, the other two tests are there to
  COMMENT: check that the preprocessor passes the value to _POSIX_C_SOURCE as expected.
  OPT: -cpp-extra-args="-D__FC_FORCE_POSIX_C_SOURCE=199309L" -print
  OPT: -cpp-extra-args="-D__FC_FORCE_POSIX_C_SOURCE=0" -print

  COMMENT: Old versions of GCC do not display the line number for #error, filter
  COMMENT: it out from the oracle.
  FILTER: sed -e 's/force_posix.c:.*$/force_posix.c:/g' -e 's/   [0-9][0-9] | / /g' -e 's/      | / /g'
  OPT: -cpp-extra-args="-D__FC_FORCE_POSIX_C_SOURCE=-2" -print
EXIT:1
FILTER: sed -e "s|$TMPDIR/[^ ]*|/tmp/TEMPNAME|g" -e "s|$(realpath $(pwd)/../../../..)|FC_HOME|g" -e "/^cc1/d"
ENABLED_IF: %{read:../../../gcc-is-genuine}
  CMD: LC_ALL=C @frama-c-exe@
  OPT: -cpp-command="gcc -C -E -I." -cpp-frama-c-compliant -no-autoload-plugins -cpp-extra-args="-D__FC_FORCE_POSIX_C_SOURCE="
*/

/* should be an error in the last case because _POSIX_C_SOURCE has been forcibly undefined */
#include <unistd.h>

long f() { return _POSIX_C_SOURCE; }

long g() { return _POSIX_VERSION; }
