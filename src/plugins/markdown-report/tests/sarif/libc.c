/* run.config
 CMD: @frama-c@ -eva -eva-no-results -mdr-gen sarif -mdr-sarif-deterministic
 BIN: with-libc-unchecked.sarif
   OPT: -mdr-out ./with-libc-unchecked
   COMMENT: the log with libc is too large, so we do not use it as textual oracle; we also test -mdr-out without a file extension
 BIN: without-libc.sarif.unfiltered
   OPT: -mdr-no-print-libc -mdr-out ./without-libc.sarif.unfiltered
   EXECNOW: LOG without-libc.sarif sed -e "s:@PTEST_SESSION@:PTEST_SESSION:" %{dep:without-libc.sarif.unfiltered} > without-libc.sarif 2> @DEV_NULL@

 ENABLED_IF: (and %{bin-available:check-jsonschema} %{bin-available:jq})
 EXECNOW: LOG with-libc.sarif.checked check-jsonschema --no-cache --schemafile $(jq '."$schema"' ./with-libc-unchecked.sarif -r) %{dep:with-libc-unchecked.sarif} > ./with-libc.sarif.checked
*/
#include <string.h>
int main() {
  char *s = "hello world";
  int n = strlen(s);
  return n;
}
