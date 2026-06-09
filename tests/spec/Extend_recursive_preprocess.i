/* run.config
  MODULE: @PTEST_NAME@
  OPT: -kernel-warn-key=annot-error=warning -print
*/

/*@ \test::gl_foo foo1 {
    \test::gl_fooo must_replace(x);
    \test::gl_fooo must_not_replace(x);
    \test::gl_fooo must_replace(x);
}*/


/*@ \test::gl_foo foo1 {
    \test::gl_foo foo2 {
      \test::gl_fooo must_replace(x);
      \test::gl_fooo must_not_replace(x);
    }
}*/

/*@ \test::gl_foo foo1 {
    \test::gl_fooo must_replace(x);
    \test::gl_foo foo2 {
      \test::gl_fooo must_replace(x);
      \test::gl_foo foo3 {
         \test::gl_fooo must_replace(x);
	 \test::gl_fooo must_not_replace(x);
      }
      \test::gl_fooo must_replace(x);
    }
    \test::gl_fooo must_not_replace(x);

}*/

//frama-c -no-autoload-plugins -kernel-warn-key=annot-error=warning -print -load-script Extend_recursive_preprocess.ml Extend_recursive_preprocess.i
