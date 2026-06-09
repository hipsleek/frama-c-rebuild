Create GNUmakefile and test a successful run
  $ cp $(frama-c -print-lib-path)/analysis-scripts/template.mk GNUmakefile.tmp
  $ awk '/^FCFLAGS/ {print;print "  -no-autoload-plugins -load-module eva,inout,metrics,nonterm,report,scope\\";next};1' GNUmakefile.tmp > GNUmakefile
  $ make 2>&1 | grep -o "\[nonterm" # check that Nonterm ran (and so did Eva)
  [nonterm
  $ grep warnings main.eva/stats.txt
  warnings=1
  $ rm -f GNUmakefile # clean up for next test

Create a GNUmakefile with -eva-stop-at-nth-alarm to test 'crashing'
  $ cp $(frama-c -print-lib-path)/analysis-scripts/template.mk GNUmakefile.tmp1
  $ awk '/^FCFLAGS/ {print;print "  -no-autoload-plugins -load-module eva,inout,metrics,nonterm,report,scope\\";next};1' GNUmakefile.tmp1 > GNUmakefile.tmp2
  $ awk '/^EVAFLAGS/ {print;print "  -eva-stop-at-nth-alarm 0 \\";next};1' GNUmakefile.tmp2 > GNUmakefile.tmp3
  $ awk -v apostrophe="'" '/^CPPFLAGS/ {print;print "  -D" apostrophe "__P(args)=args" apostrophe " \\";next};1' GNUmakefile.tmp3 > GNUmakefile
  $ make 2>&1 | grep "Clean up"
  [eva] Clean up partial results.
  $ if [ -f main.eva/framac.sav.error ]; then
  > echo "OK! Partial save file exists"; fi
  OK! Partial save file exists
