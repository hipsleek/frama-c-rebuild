/* run.config
 PLUGIN: @EVA_PLUGINS@
   EXECNOW: BIN audit.json cat %{dep:./audit-in.json} | sed -e 's:PTEST_DIR:.:' > ./audit.json 2> @DEV_NULL@
 DEPS: audit_included.h, audit_included_but_not_listed.h
 LOG: audit-out.json
   STDOPT: #"-audit-check %{dep:./audit.json} -audit-prepare ./audit-out.json -kernel-warn-key audit=warning"
*/
#include "audit_included.h"
#include "audit_included_but_not_listed.h"

void main() {
  float f = 2.1; // to trigger a syntactic warning
}
