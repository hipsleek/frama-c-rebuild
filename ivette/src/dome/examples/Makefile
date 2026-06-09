##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

# --------------------------------------------------------------------------
help:
	@echo "*****************************************"
	@echo "***      Dome Framework Makefile      ***"
	@echo "*****************************************"
	@echo
	@echo "   make doc: alias for dome-doc"
	@echo "   make dev: alias for dome-dev"
	@echo "   make app: alias for dome-app"
	@echo "   make dist: alias for dome-dist"
	@echo "   make dome-help: detailed help for dome"
	@echo "   make DEMO=<xxx> demo: switch to 'src/renderer/demo/xxx.js' "
	@echo
	@echo "*****************************************"

# --------------------------------------------------------------------------

APP=Dome
DOME=.
DEMO=Hello

include $(DOME)/template/makefile

.PHONY: doc demo dev app dist

doc: dome-doc
dev: dome-dev
app: dome-app
dist: dome-dist

demo: src/renderer/demo/$(DEMO).js
	@echo "Switch to src/renderer/demo/$(DEMO).js"
	@rm -f src/renderer/Application.js
	@echo "// Dome Demonstrator" >> src/renderer/Application.js
	@echo "import React from 'react' ;"  >> src/renderer/Application.js
	@echo "import Main from './demo/$(DEMO).js' ;" >> src/renderer/Application.js
	@echo "export default (() => <Main/>);" >> src/renderer/Application.js

src/renderer/demo/%.js:
	@echo "Generate  $@"
	@echo "// Dome Demonstrator" > $@
	@echo "import React from 'react';" >> $@
	@echo "export default (() => <h1>$* Demo</h1>);" >> $@

# --------------------------------------------------------------------------
