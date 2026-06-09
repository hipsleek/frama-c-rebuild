/* run.config
   NOFRAMAC:
   EXECNOW: LOG @PTEST_FILE@.reparse.c LOG @PTEST_FILE@.1.res.log @frama-c-cmd@ -load-plugin wp @PTEST_FILE@ -print -ocode @PTEST_FILE@.reparse.c > @PTEST_FILE@.1.res.log
   EXECNOW: LOG @PTEST_FILE@.out.c LOG @PTEST_FILE@.2.res.log @frama-c-cmd@ -load-plugin wp %{dep:@PTEST_FILE@.reparse.c} -print -ocode @PTEST_FILE@.out.c > @PTEST_FILE@.2.res.log
*/
/* run.config_qualif
   DONTRUN:
*/

/*@
  \wp::strategy Prover: \prover("Alt-Ergo",0.1);
  \wp::strategy Lazy:
    Prover,
    \tactic("Wp.overflow"
      ,\pattern(\any(P(_, (..)),Q((..))))
    );
*/
